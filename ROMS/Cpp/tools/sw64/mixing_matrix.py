#!/usr/bin/env python3
"""Validate and measure iteration-time KPP ports, not initialization.

Use an isolated ROMS source snapshot while another matrix is building.
Kokkos controls include KK_STAGE_REMAINDER; only candidates add KK_STAGE_LMD.
All five builds use the same full official BL_TEST LMD physics and
PERFECT_RESTART, allowing nonlocal transport to be checked in restart output.
"""
import argparse
import json
import os
from pathlib import Path
import re
import shlex
import subprocess

import remainder_matrix as base

ROOT=base.ROOT
STAGES=base.STAGES.replace(';KK_STAGE_PRE3D','')+';KK_STAGE_REMAINDER'
EXTRA='UV_VIS2;LMD_MIXING;PERFECT_RESTART;ROMS_WALL_PROFILE'
KERNELS=('extra lmd_vmix','extra lmd_skpp','extra lmd_bkpp','extra lmd_finish')
REQUIRED_FIELDS={'roms_his.nc':('AKv','AKt','AKs','Hsbl','Hbbl'),
                 'roms_rst.nc':('ghat_temp','ghat_salt')}
for name,ny,dt in (('kpp16',16,150),('kpp64',64,30)):
    base.CASES[name]=dict(base.CASES['bl_test'],large=(256,ny),dt=dt,extra=EXTRA)


def cache_values(path):
    result={}
    for line in path.read_text().splitlines():
        if line.startswith(('#','//')) or '=' not in line or ':' not in line:continue
        key,value=line.split('=',1)
        result[key.split(':',1)[0]]=value
    return result


def verify_build(directory,backend,mpi,control):
    c=cache_values(directory/'CMakeCache.txt')
    flags=set(c.get('MY_CPP_FLAGS','').split(';'))
    required=set(EXTRA.split(';'))|{'OUT_DOUBLE','DIAGNOSTICS_UV','DIAGNOSTICS_TS'}
    if not required<=flags or c.get('ROMS_FAST_MATH')!='OFF':
        raise RuntimeError('physics or conservative-math mismatch: '+str(directory))
    if c.get('MPI')!=('ON' if mpi else 'OFF'):
        raise RuntimeError('MPI mismatch: '+str(directory))
    if backend!='reference':
        if c.get('ROMS_KOKKOS_BACKEND')!=backend or 'KK_STAGE_REMAINDER' not in flags:
            raise RuntimeError('Kokkos control configuration mismatch: '+str(directory))
        if ('KK_STAGE_LMD' in flags)==control or 'KK_STAGE_PRE3D' in flags:
            raise RuntimeError('incorrect KPP stage selection: '+str(directory))
    elif c.get('ROMS_KOKKOS')!='OFF':
        raise RuntimeError('reference is not pure Fortran: '+str(directory))
    if backend=='sw64':
        commands=directory/'compile_commands.json'
        units=json.loads(commands.read_text()) if commands.is_file() else []
        units=[x for x in units if 'Objects.dir/' in x.get('command','')
               and x.get('file','').endswith('.cpp')]
        if not units:
            raise RuntimeError('missing SW64 production compile commands: '+str(directory))
        for unit in units:
            options=shlex.split(unit['command'])
            if ('-fno-tree-vectorize' not in options or
                any(x in options for x in ('-ftree-vectorize','-ftree-loop-vectorize',
                                          '-ftree-slp-vectorize','-Ofast','-ffast-math',
                                          '-fassociative-math','-freciprocal-math',
                                          '-funsafe-math-optimizations'))):
                raise RuntimeError('SW64 strict-arithmetic compile policy mismatch: '+unit['file'])


class MixingMatrix(base.Matrix):
    def __init__(self,args):
        os.environ.setdefault('RUN_ROOT','runs/kpp-'+base.time.strftime('%Y%m%dT%H%M%S'))
        super().__init__(args)
        self.built={}
        self.report.update(scope='iteration-time KPP only; no initialization changes',
                           physics=EXTRA,run_manifest=[],
                           sw64_math_policy='scalar IEEE evaluation within CPEs; Kokkos parallel offload retained',
                           required_kpp_entries=list(KERNELS),
                           restart_validation='all records, including KPP nonlocal transport',
                           performance_grids={'kpp16':[256,16,30,150],
                                              'kpp64':[256,64,30,30]},
                           grid_description='Lm, Mm, N, DT; each comparison uses identical input')
        self.status()

    def build(self,name,backends,mpi=False,control=False):
        tags={}
        stages=STAGES+('' if control else ';KK_STAGE_LMD')
        for backend in backends.split():
            # The pure-Fortran denominator has no Kokkos stage distinction.
            is_control=control or backend=='reference'
            suffix='-kpp-r5'+('-control' if is_control else '')
            tag='BL_TEST-'+backend+('-mpi' if mpi else '')+suffix
            tags[backend]=tag
            if tag in self.built:continue
            self.status('build '+name+' '+tag)
            directory=ROOT/'build'/tag
            exe=directory/('romsM' if mpi else 'romsS')
            if exe.is_symlink():raise RuntimeError('refusing symlink executable: '+str(exe))
            if not (self.args.reuse_builds and exe.exists()):
                if exe.exists():exe.unlink()  # force relinking this task's candidate
                env=dict(os.environ,BACKENDS=backend,TAG_SUFFIX=suffix,KK_STAGES=stages,
                         MPI='ON' if mpi else 'OFF',BUILD_JOBS=os.environ.get('BUILD_JOBS','2'))
                subprocess.run(['bash',str(base.TOOLS/'build_app.sh'),'BL_TEST',EXTRA,
                                str(ROOT/'official/bl_test')],cwd=str(ROOT),env=env,check=True)
            verify_build(directory,backend,mpi,is_control)
            self.built[tag]=base.sha(exe)
        return tags

    def prepare_input(self,inp,perf):
        if perf:return
        text=inp.read_text()
        for key,value in (('NRST','1'),('LcycleRST','F')):
            text,n=re.subn(r'(?m)^(\s*'+key+r'\s*==\s*)\S+',
                           lambda m:m[1]+value,text)
            if n!=1:raise RuntimeError('missing validation restart setting '+key)
        inp.write_text(text)

    def run(self,name,tag,label,perf=False,rep=0,ranks=1):
        directory,m=super().run(name,tag,label,perf,rep,ranks)
        text=(directory/'run.log').read_text(errors='replace')
        for flag in ('LMD_MIXING','LMD_SKPP','LMD_BKPP','LMD_NONLOCAL','PERFECT_RESTART'):
            if not re.search(r'^\s*'+flag+r'\s+',text,re.M):
                raise RuntimeError('inactive KPP physics '+flag+': '+str(directory))
        if label in ('serial','sw64'):
            if any(m['kernels'].get(k,{}).get('calls',0)==0 for k in KERNELS):
                raise RuntimeError('a KPP compute phase did not execute: '+str(directory))
        elif any(k in m['kernels'] for k in KERNELS):
            raise RuntimeError('KPP offload executed in a control: '+str(directory))
        self.report['run_manifest'].append(dict(case=name,label=label,rep=rep,ranks=ranks,
                                                performance=perf,directory=str(directory),**m))
        self.status()
        return directory,m

    def compare(self,name,ref,test,label,exact=False,enforce=True):
        # Unlike the earlier four-case matrix, restart fields are required.
        if not (ref/'roms_rst.nc').is_file():
            raise RuntimeError('missing full-state KPP restart reference: '+str(ref))
        results=super().compare(name,ref,test,label,exact,enforce,include_restart=True)
        by_file={f['file']:f for f in results}
        for filename,fields in REQUIRED_FIELDS.items():
            if filename not in by_file:
                raise RuntimeError('missing KPP validation output '+filename)
            log=Path(by_file[filename]['log']).read_text()
            for field in fields:
                if not re.search(r'^'+re.escape(field)+r'\s+maxabs=',log,re.M):
                    raise RuntimeError('required KPP field was not compared: '+field)
        return results


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--apps',nargs='+',choices=('kpp16','kpp64'),default=['kpp16','kpp64'])
    p.add_argument('--repeats',type=int,default=3)
    p.add_argument('--validation-steps',type=int,default=4)
    p.add_argument('--mpi',action='store_true')
    p.add_argument('--reuse-builds',action='store_true',help='verify and reuse existing matching executables')
    args=p.parse_args()
    if args.repeats<1 or args.validation_steps<4:p.error('need positive repeats and at least four validation steps')
    m=MixingMatrix(args)
    try:m.execute()
    except BaseException as e:
        m.report.update(state='failed',error=str(e));m.status();raise


if __name__=='__main__':main()
