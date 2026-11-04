#!/usr/bin/env python3
"""Build, validate and benchmark the opt-in remainder ports on Sunway.

Run in ~/voyage/roms. All artifacts use a fresh runs/remainder-* directory;
existing controls, outputs and unrelated jobs are never overwritten.
Serial must match the matched pre-port Serial control exactly. Original
Fortran comparisons retain exact and normalized results, including legacy
third-step roundoff. SW64 strict diagnostic differences remain explicit.
"""
import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import statistics
import subprocess
import time

ROOT=Path(__file__).resolve().parents[4]
TOOLS=ROOT/'ROMS/Cpp/tools/sw64'
STAGES=('KK_STAGE_FUSED;KK_STAGE_TAVG;KK_STAGE_COUP;KK_STAGE_MOMT;'
        'KK_STAGE_DIAG;KK_STAGE_STOR;KK_STAGE_UV3DMIX;KK_STAGE_T3DMIX;'
        'KK_STAGE_PRSGRD;KK_STAGE_OMEGA;KK_STAGE_SETZETA;'
        'KK_STAGE_PRE3D;KK_STAGE_STEP3DUV')
CASES={
    'upwelling': dict(app='UPWELLING',source='ROMS/External/roms_upwelling.in',
                     header='ROMS/Include',small=(41,80),large=(256,256),extra='',control='sw64'),
    'kelvin': dict(app='KELVIN',source='official/kelvin/roms_kelvin.in',
                  header='official/kelvin',small=(50,30),large=(256,256),extra='',control='KELVIN-sw64-r2'),
    'canyon': dict(app='CANYON',source='ROMS/External/roms_canyon3d.in',
                  header='ROMS/Include',small=(65,48),large=(256,192),extra='SOLVE3D',control='CANYON-sw64-r2'),
    'bl_test': dict(app='BL_TEST',source='official/bl_test/roms_bl_test.in',
                   header='official/bl_test',small=(39,4),large=(256,16),extra='UV_VIS2',control='BL_TEST-sw64-r2'),
}


def sha(path):
    h=hashlib.sha256()
    with path.open('rb') as stream:
        for chunk in iter(lambda:stream.read(1024*1024),b''): h.update(chunk)
    return h.hexdigest()


def metrics(log):
    text=log.read_text(errors='replace')
    wall=re.search(r'ROMS time stepping wall \[rank=0\]:\s*([\d.]+)',text)
    if not wall: raise RuntimeError('missing stepping wall in '+str(log))
    regions={m[1].strip():float(m[2]) for m in re.finditer(r'^  ([\w][^\n]*?)\s+\.{3,}\s+([\d.]+)\s+\(',text,re.M)}
    kernels={m[1].strip():dict(calls=int(m[2]),seconds=float(m[3])) for m in re.finditer(r'^  ((?:step|rhs|pre_|extra )[^\n]*?)\s+(\d+)\s+([\d.]+)\s+[\d.]+\s*$',text,re.M)}
    return dict(wall=float(wall[1]),jobs=re.findall(r'Job <(\d+)>',text),regions=regions,kernels=kernels)


class Matrix:
    def __init__(self,args):
        self.args=args
        name=os.environ.get('RUN_ROOT','runs/remainder-'+time.strftime('%Y%m%dT%H%M%S'))
        self.out=(ROOT/name).resolve()
        if ROOT.resolve() not in self.out.parents:
            raise RuntimeError('RUN_ROOT must stay under the ROMS workspace')
        self.out.mkdir(parents=True,exist_ok=False)
        self.report={'state':'running','stage':'starting','cases':{},'jobs':[],
                     'validation_steps':args.validation_steps,'performance_steps':2,
                     'repeats':args.repeats,'queue':'q_share',
                     'profile_clock':'SYSTEM_CLOCK (ROMS_WALL_PROFILE)',
                     'profile_scope':'ROMS regions can include initialization; region 34 has a known nested timer collision',
                     'serial_acceptance':'exact against matched prior-kernel Serial; original-Fortran exact and normalized comparisons retained',
                     'diagnostic_absolute_crosscheck':1e-15}
        self.status()

    def status(self,stage=None):
        if stage: self.report['stage']=stage
        self.report['updated']=time.strftime('%Y-%m-%dT%H:%M:%S%z')
        temp=self.out/'STATUS.json.tmp'
        temp.write_text(json.dumps(self.report,indent=2,sort_keys=True)+'\n')
        temp.replace(self.out/'STATUS.json')
        print(self.report['state']+': '+self.report['stage'],flush=True)

    def build(self,name,backends,mpi=False,control=False):
        c=CASES[name]
        self.status('build '+name+' '+('MPI ' if mpi else '')+('control' if control else 'candidate'))
        stages=STAGES
        if name=='bl_test': stages=stages.replace(';KK_STAGE_PRE3D','')
        if not control: stages+=';KK_STAGE_REMAINDER'
        suffix='-control-r4' if control else '-r4'
        for backend in backends.split():
            tag=c['app']+'-'+backend+('-mpi' if mpi else '')+suffix
            exe=ROOT/'build'/tag/('romsM' if mpi else 'romsS')
            # Force a real relink of this task's candidates only.
            if exe.exists():
                if exe.is_symlink(): raise RuntimeError('refusing to replace a symlink: '+str(exe))
                exe.unlink()
        env=dict(os.environ,BACKENDS=backends,TAG_SUFFIX=suffix,KK_STAGES=stages,
                 MPI='ON' if mpi else 'OFF',BUILD_JOBS=os.environ.get('BUILD_JOBS','6'))
        extra=';'.join(x for x in (c['extra'],'ROMS_WALL_PROFILE') if x)
        subprocess.run(['bash',str(TOOLS/'build_app.sh'),c['app'],extra,str(ROOT/c['header'])],
                       cwd=str(ROOT),env=env,check=True)
        return {b:c['app']+'-'+b+('-mpi' if mpi else '')+suffix for b in backends.split()}

    def prepare_input(self,inp,perf):
        """Optional validation-only output settings; default physics is unchanged."""

    def run(self,name,tag,label,perf=False,rep=0,ranks=1):
        c=CASES[name]
        d=self.out/name/('perf' if perf else 'mpi-val' if ranks>1 else 'val')/(label+'-'+str(rep))
        cmd=['python3',str(TOOLS/'make_app_case.py'),str(ROOT/c['source']),str(d),
             '--size',*[str(n) for n in c['large' if perf else 'small']],
             '--steps',str(2 if perf else self.args.validation_steps)]
        if perf:
            cmd+=['--no-output']
            if name=='canyon': cmd+=['--dt','30']
        if 'dt' in c: cmd+=['--dt',str(c['dt'])]
        subprocess.run(cmd,cwd=str(ROOT),check=True)
        inp=d/Path(c['source']).name
        if ranks>1:
            text=inp.read_text()
            text,n=re.subn(r'(?m)^(\s*NtileI\s*==\s*)\S+',lambda m:m[1]+str(ranks),text)
            if n!=1: raise RuntimeError('MPI NtileI not found')
            inp.write_text(text)
        self.prepare_input(inp,perf)
        exe=ROOT/'build'/tag/('romsM' if ranks>1 else 'romsS')
        digest=sha(exe)
        self.status('run '+name+' '+label+' '+('MPI' if ranks>1 else 'perf' if perf else 'validation')+' '+str(rep))
        env=dict(os.environ,ROMS_KK_TIMING='1',OMP_NUM_THREADS='1')
        if ranks==1:
            result=subprocess.run(['bash',str(TOOLS/'run_app.sh'),tag,str(d)],env=env,cwd=str(ROOT))
            rc=result.returncode
        else:
            args=['bsub','-I','-b','-q','q_share','-n',str(ranks),'-np',str(ranks),'-cgsp','64',
                  '-share_size','15000','-cross_size','1024','-host_stack','1024','-cache_size','128',
                  '-timelimit','00:10:00','-J','roms-remainder-'+name+'-'+label,'-i',str(inp),str(exe),str(inp)]
            with (d/'run.log').open('w') as log:
                rc=subprocess.run(args,env=env,cwd=str(d),stdout=log,stderr=subprocess.STDOUT).returncode
        text=(d/'run.log').read_text(errors='replace')
        jobs=re.findall(r'Job <(\d+)>',text)
        self.report['jobs'] += [dict(id=j,case=name,label=label,directory=str(d)) for j in jobs]
        self.status()
        if rc or 'ROMS: DONE' not in text or re.search(r'ERROR STOP|Abnormal termination|BLOWUP|Kokkos row fallback:',text):
            raise RuntimeError('model run failed or fell back: '+str(d))
        expected='SW64' if label in ('sw64','control') else 'Serial' if label in ('serial','control_serial') else None
        space=re.search(r'ROMS Kokkos execution space:\s*(\w+)',text)
        if (expected and (not space or space[1]!=expected)) or (expected is None and space):
            raise RuntimeError('unexpected execution space: '+str(d))
        if label=='sw64' and ranks==1 and 'extra rhs3d' not in text:
            raise RuntimeError('new RHS kernels did not execute: '+str(d))
        if perf and list(d.glob('roms_*.nc')):
            raise RuntimeError('NetCDF output was written during a no-output run: '+str(d))
        if sha(exe)!=digest: raise RuntimeError('executable changed during run: '+str(exe))
        return d,dict(metrics(d/'run.log'),executable=str(exe),sha256=digest)

    def compare(self,name,ref,test,label,exact=False,enforce=True,include_restart=False):
        env=dict(os.environ)
        x86=Path('/usr/sw/yyzlib/local_x86')
        env['PATH']=str(x86/'bin')+os.pathsep+env.get('PATH','')
        env['LD_LIBRARY_PATH']=str(x86/'lib')+os.pathsep+env.get('LD_LIBRARY_PATH','')
        results=[]
        for f in sorted(ref.glob('roms_*.nc')):
            if f.name=='roms_rst.nc' and not include_restart: continue
            cmd=['python3',str(ROOT/'ROMS/Cpp/tools/nccmp.py'),str(f),str(test/f.name),'--all-time']
            if not exact: cmd+=['--rtol','1e-10','--zero-tol','1e-20']
            result=subprocess.run(cmd,env=env,stdout=subprocess.PIPE,stderr=subprocess.STDOUT,universal_newlines=True)
            path=self.out/name/(label+'-'+f.stem+'.log')
            path.write_text(result.stdout)
            item={'file':f.name,'strict_pass':result.returncode==0,'log':str(path)}
            if result.returncode:
                item['strict_failures']=[s for s in result.stdout.splitlines() if s.endswith('FAIL')]
                if not enforce:
                    results.append(item)
                    continue
                if exact or f.name!='roms_dia.nc': raise RuntimeError('strict field comparison failed: '+str(path))
                # Do not turn this secondary check into a strict PASS.
                cross=subprocess.run(cmd+['--tol','1e-15'],env=env,stdout=subprocess.PIPE,
                                     stderr=subprocess.STDOUT,universal_newlines=True)
                crosspath=path.with_suffix('.absolute.log');crosspath.write_text(cross.stdout)
                item.update(absolute_crosscheck_pass=cross.returncode==0,absolute_log=str(crosspath),
                            strict_failures=[s for s in result.stdout.splitlines() if s.endswith('FAIL')])
                if cross.returncode: raise RuntimeError('diagnostic mismatch exceeds absolute cross-check: '+str(crosspath))
            results.append(item)
        if not results: raise RuntimeError('no reference NetCDF outputs')
        return results

    def execute(self):
        for name in self.args.apps:
            c=CASES[name]
            tags=self.build(name,'reference serial sw64')
            controls=self.build(name,'serial sw64',control=True)
            control=controls['sw64']
            entry=self.report['cases'][name]={'tags':dict(tags,control=control,control_serial=controls['serial']),
                                             'validation':{},'performance':{}}
            runs={}
            for label,tag in [('fortran',tags['reference']),('serial',tags['serial']),('sw64',tags['sw64']),
                              ('control',control),('control_serial',controls['serial'])]:
                runs[label],_=self.run(name,tag,label)
            entry['validation']['serial_vs_control_exact']=self.compare(name,runs['control_serial'],runs['serial'],'serial-vs-control-exact',True)
            entry['validation']['serial_vs_fortran_exact']=self.compare(name,runs['fortran'],runs['serial'],'serial-vs-fortran-exact',True,False)
            entry['validation']['serial_vs_fortran_tolerance']=self.compare(name,runs['fortran'],runs['serial'],'serial-vs-fortran-tolerance')
            entry['validation']['sw64_vs_fortran']=self.compare(name,runs['fortran'],runs['sw64'],'sw64-vs-fortran')
            entry['validation']['control_vs_fortran']=self.compare(name,runs['fortran'],runs['control'],'control-vs-fortran')
            entry['validation']['sw64_vs_control']=self.compare(name,runs['control'],runs['sw64'],'sw64-vs-control')
            for label,tag in [('fortran',tags['reference']),('control',control),('serial',tags['serial']),('sw64',tags['sw64'])]:
                samples=[]
                for rep in range(1 if label=='serial' else self.args.repeats):
                    _,m=self.run(name,tag,label,perf=True,rep=rep);samples.append(m)
                entry['performance'][label]={'samples':samples,'median_wall':statistics.median(x['wall'] for x in samples)}
            p=entry['performance']
            p['vs_original_fortran']=p['fortran']['median_wall']/p['sw64']['median_wall']
            p['incremental_vs_control']=p['control']['median_wall']/p['sw64']['median_wall']
            self.status('single-rank finished '+name)
        if self.args.mpi:
            for name in self.args.apps:
                entry=self.report['cases'][name]
                mtags=self.build(name,'reference serial sw64',mpi=True)
                mcontrol=self.build(name,'serial',mpi=True,control=True)['serial']
                mruns={}
                for label,tag in mtags.items():mruns[label],_=self.run(name,tag,label,ranks=2)
                mruns['control_serial'],_=self.run(name,mcontrol,'control_serial',ranks=2)
                entry['mpi_validation']={
                    'serial_vs_control_exact':self.compare(name,mruns['control_serial'],mruns['serial'],'mpi-serial-vs-control-exact',True),
                    'serial_vs_fortran_exact':self.compare(name,mruns['reference'],mruns['serial'],'mpi-serial-vs-fortran-exact',True,False),
                    'serial_vs_fortran_tolerance':self.compare(name,mruns['reference'],mruns['serial'],'mpi-serial-vs-fortran-tolerance'),
                    'sw64_vs_fortran':self.compare(name,mruns['reference'],mruns['sw64'],'mpi-sw64-vs-fortran')}
                self.status('MPI finished '+name)
        differences=any(not f['strict_pass'] for c in self.report['cases'].values()
                        for group in [c['validation'],c.get('mpi_validation',{})]
                        for fields in group.values() for f in fields)
        self.report['state']='complete_with_reference_roundoff' if differences else 'complete'
        self.status('matrix finished')


def main():
    p=argparse.ArgumentParser(description=__doc__)
    p.add_argument('--apps',nargs='+',choices=CASES,default=list(CASES))
    p.add_argument('--repeats',type=int,default=3)
    p.add_argument('--validation-steps',type=int,default=4,
                   help='four small-grid steps cover first, second and later time-step branches')
    p.add_argument('--mpi',action='store_true')
    args=p.parse_args()
    if args.repeats<1: p.error('--repeats must be positive')
    if args.validation_steps<3: p.error('--validation-steps must cover the third-step branch')
    m=Matrix(args)
    try:m.execute()
    except BaseException as e:
        m.report.update(state='failed',error=str(e));m.status();raise

if __name__=='__main__':main()
