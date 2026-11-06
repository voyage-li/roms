#!/usr/bin/env python3
"""Host-only regression tests for the restricted Fortran row lowering.

The numerical tests compile the actual generated C++ and array ABI, using
an intentionally tiny synchronous execution-space adapter, not a second
copy of the translator's expressions as an expected-value oracle.
Run: python3 -m unittest discover -s ROMS/Cpp/tools -p test_row_codegen.py -v
Target-model Serial/Fortran and SW64 comparisons are still required.
"""
import os
import re
import sys
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

import row_codegen as gen

ROOT = Path(__file__).resolve().parents[1]
REPO = ROOT.parents[1]
# Minimal copy of the BL_TEST bulk-flux helper shape, with enough locals
# for their emission order to show whether it depends on hashing.
BULK_HELPER = '''
      FUNCTION bulk_psiu (ZoL, pi)
      USE mod_kinds
      real(r8) :: bulk_psiu
      real(dp), intent(in) :: pi
      real(r8), intent(in) :: ZoL
      real(r8), parameter :: r3 = 1.0_r8/3.0_r8
      real(r8) :: Fw, cff, psic, psik, x, y
      IF (ZoL.lt.0.0_r8) THEN
        x=(1.0_r8-15.0_r8*ZoL)**0.25_r8
        psik=2.0_r8*LOG(0.5_r8*(1.0_r8+x))+0.5_r8*pi
        y=(1.0_r8-10.15_r8*ZoL)**r3
        psic=1.5_r8*LOG(r3*(y*y+y+1.0_r8))
        cff=ZoL*ZoL
        Fw=cff/(1.0_r8+cff)
        bulk_psiu=(1.0_r8-Fw)*psik+Fw*psic
      ELSE
        bulk_psiu=-4.0_r8*ZoL
      END IF
      RETURN
      END FUNCTION bulk_psiu
'''
STUB = r'''
#pragma once
#include <cstddef>
#include <thread>
#include <vector>
#define ROMS_KOKKOS 1
#define KOKKOS_INLINE_FUNCTION inline
#define KOKKOS_LAMBDA [=]
#define ROMS_KK_TIMER(s)
namespace roms {struct exec_space {}; inline void initialize() {}}
namespace Kokkos {
template<class T>struct IndexType {};
template<class... T>struct RangePolicy {
 int a,b; RangePolicy(int aa,int bb):a(aa),b(bb) {}
};
template<class P,class F> void parallel_for(const char*,P p,F f) {
#ifdef PARALLEL_ROWS
 std::vector<std::thread> workers;
 for(int j=p.a;j<p.b;++j)workers.emplace_back([=](){f(j);});
 for(auto& worker:workers)worker.join();
#elif defined REVERSE_ROWS
 for(int j=p.b-1;j>=p.a;--j) f(j);
#else
 for(int j=p.a;j<p.b;++j) f(j);
#endif
}}
'''


def kernel(body, declarations, name='kk_test_0'):
    k=gen.Kernel(name,gen.statements(body),gen.declarations(gen.statements(declarations)))
    code=k.translate()
    f,cpp=gen.emit_kernel(k,code)
    return k,f,cpp


class LoweringTests(unittest.TestCase):
    def test_unknown_statement_fails(self):
        with self.assertRaisesRegex(ValueError,'unsupported statement'):
            kernel('DO j=1,3\n CALL exchange(a)\nEND DO', '')

    def test_unknown_dynamic_call_fails(self):
        with self.assertRaisesRegex(ValueError,'unsupported call'):
            kernel('DO j=1,3\n cff=surprise(j)\nEND DO','real(r8) :: cff')

    def test_loop_carried_scalar_fails(self):
        with self.assertRaisesRegex(ValueError,'loop-carried scalar'):
            kernel('DO j=1,3\n cff=cff+1.0_r8\nEND DO','real(r8) :: cff')

    def test_branch_only_definition_does_not_hide_dependency(self):
        with self.assertRaisesRegex(ValueError,'loop-carried scalar'):
            kernel('DO j=1,3\n IF (j.eq.1) THEN\n cff=2.0_r8\n'
                   'ELSE\n cff=cff+1.0_r8\n END IF\nEND DO','real(r8) :: cff')

    def test_parameter_array_fails_closed(self):
        with self.assertRaisesRegex(ValueError,'parameter arrays'):
            kernel('DO j=1,3\n a(1,j)=weights(j)\nEND DO',
                   'real(r8),intent(out) :: a(:,:)\n'
                   'real(r8),parameter :: weights(3)=(/1.0_r8,2.0_r8,3.0_r8/)')

    def test_scalar_liveout_fails(self):
        block=gen.statements('DO j=1,3\n cff=2.0_r8\nEND DO\n')
        k=gen.Kernel('kk_test_0',block,gen.declarations(gen.statements('real(r8) :: cff')))
        k.translate()
        st=gen.statements('DO j=1,3\n cff=2.0_r8\nEND DO\n answer=cff\n')
        with self.assertRaisesRegex(ValueError,'live-out into Fortran'):
            gen.check_scalar_liveout(st,[(0,2,k)])

    def test_power_precedence(self):
        self.assertEqual(gen.power_expr('-a**2'),'-roms_loops::power(a,2)')
        self.assertEqual(gen.power_expr('a**b**2'),
                         'roms_loops::power(a,roms_loops::power(b,2))')
        self.assertEqual(gen.power_expr('a(i+1,j)**(-0.5)'),
                         'roms_loops::power(a(i+1,j),(-0.5))')

    def test_integer_exponents_must_be_reproduced_exactly(self):
        # gfortran multiplies out INTEGER-constant powers; roms_loops::power
        # does the same for 2, 3 and 4 only and would call pow() otherwise.
        for ok in ('a**2', 'a**3', 'a**4', 'a**(3)', 'a**2.5', 'a**(-0.5)', 'a**n'):
            gen.power_expr(ok)
        for bad in ('a**5', 'a**(-2)', 'x(i,j)**10'):
            with self.assertRaisesRegex(ValueError, 'integer exponent'):
                gen.power_expr(bad)
        with self.assertRaisesRegex(ValueError, 'integer exponent'):
            kernel('DO j=1,3\n a(1,j)=b(1,j)**5\nEND DO',
                   'real(r8),intent(out) :: a(:,:)\nreal(r8),intent(in) :: b(:,:)')

    def test_not_operand_must_not_hide_a_comparison(self):
        # Fortran: .not. a == b is .not.(a == b); C++ would read (!a) == b.
        for ok in ('.not.(a.eq.b)', '.not.flag', '.not. ewperiodic(ng) .and. x',
                   '.not.domain(ng) % western_edge(tile)', '(.not.f(i,j))',
                   'a.or..not.b', '.not. .not. flag', 'g(.not.p, q)'):
            gen.check_not_operands(ok)
        for bad in ('.not. a == b', '.not. a .eq. b', '.not. j > 2',
                    '.not.(a).eq.b', '.not. a + b', '.not.'):
            with self.assertRaises(ValueError, msg=bad):
                gen.check_not_operands(bad)
        with self.assertRaisesRegex(ValueError, 'parenthesised'):
            kernel('DO j=1,3\n IF (.not. j.eq.2) THEN\n a(1,j)=1.0_r8\n END IF\nEND DO',
                   'real(r8),intent(out) :: a(1:1,1:3)')

    def test_fortran_inequality_and_negation_are_comparisons(self):
        # '/=' used to pass through and become a C++ divide-assign.
        main='''int main(){ KkLoopArgs args{}; double a[3]{};
          args.p[0]=a; args.b[0]=args.b[1]=args.b[2]=1; args.b[3]=3;
          kk_test_0(&args); std::printf("%.0f %.0f %.0f",a[0],a[1],a[2]); }'''
        for condition in ('j /= 2', 'j.ne.2', '.not.(j.eq.2)'):
            _,_,cpp=kernel('DO j=1,3\n IF ('+condition+') THEN\n a(1,j)=1.0_r8\n'
                           ' ELSE\n a(1,j)=2.0_r8\n END IF\nEND DO',
                           'integer :: j\nreal(r8),intent(out) :: a(1:1,1:3)')
            self.assertNotIn('/=',cpp)
            self.assertEqual(self.compile_run(cpp,main),'1 2 1',condition)

    def test_do_steps_fail_closed(self):
        decl='integer :: i,j\nreal(r8),intent(out) :: a(1:4,1:4)'
        with self.assertRaisesRegex(ValueError,'unit stride'):
            kernel('DO j=1,4,2\n a(1,j)=1.0_r8\nEND DO',decl)
        with self.assertRaisesRegex(ValueError,'integer literal'):
            kernel('DO j=1,4\n DO i=1,4,istep\n a(i,j)=1.0_r8\n END DO\nEND DO',decl)
        with self.assertRaisesRegex(ValueError,'unsupported DO'):
            kernel('DO j=1,4\n DO WHILE (a(1,j).lt.2.0_r8)\n a(1,j)=a(1,j)+1.0_r8\n'
                   ' END DO\nEND DO',decl)
        _,_,cpp=kernel('DO j=1,4\n DO i=4,1,-1\n a(i,j)=1.0_r8\n END DO\nEND DO',decl)
        self.assertIn('for (i=4; i>=1; i+=-1)',cpp)

    def test_output_does_not_depend_on_hash_seed(self):
        # The helper's locals once came out in set order, so two builds of
        # the same source produced different generated files.
        code='import sys,row_codegen as g; sys.stdout.write(g.process(sys.stdin.read(),"bulk_flux")[1])'
        outputs=set()
        for seed in ('1','2','3','4'):
            env=dict(os.environ,PYTHONHASHSEED=seed,PYTHONDONTWRITEBYTECODE='1')
            outputs.add(subprocess.run([sys.executable,'-c',code],input=BULK_HELPER,cwd=Path(__file__).parent,
                                       env=env,check=True,capture_output=True,text=True).stdout)
        self.assertEqual(len(outputs),1)
        self.assertIn('double result;\ndouble cff;\ndouble fw;\ndouble psic;',outputs.pop())

    def test_cmake_stage_file_lists_match_the_generator_targets(self):
        text=(REPO/'Compilers'/'roms_functions.cmake').read_text()
        lists={}
        for name in ('row_files','lmd_files','gls_files'):
            m=re.search(r'set\(\s*'+name+r'\s+([^)]*)\)',text)
            self.assertIsNotNone(m,name+' not found')
            lists[name]=m[1].split()
        names=[n for v in lists.values() for n in v]
        self.assertEqual(len(names),len(set(names)),'a file is listed under two stages')
        self.assertEqual(set(names),set(gen.TARGETS))

    def test_tracer_indexed_parameter_array_is_materialized(self):
        k,f,cpp=kernel('DO j=1,3\n DO itrc=1,2\n a(1,j,itrc)='
                       'akt_bak(itrc,ng)+1.0_r8\n END DO\nEND DO',
                       'real(r8), intent(out) :: a(:,:,:)')
        self.assertIn('kk_tracer_akt_bak',k.arrays)
        self.assertIn('kk_tracer_akt_bak(kk_it)=REAL(akt_bak(kk_it,ng),C_DOUBLE)',f)
        self.assertIn('kk_tracer_akt_bak(itrc)',cpp)

    def test_constant_tracer_index_remains_a_host_scalar(self):
        k,f,cpp=kernel('DO j=1,3\n a(1,j)=akt_bak(itemp,ng)\nEND DO',
                       'real(r8), intent(out) :: a(:,:)')
        self.assertNotIn('kk_tracer_akt_bak',k.arrays)
        self.assertIn('akt_bak(itemp,ng)',f)

    def test_optional_array_is_not_private(self):
        k,f,_=kernel('DO j=1,3\n IF (vadvection(itrc,ng)%mpdata) THEN\n'
                     ' a(1,j)=t(1,j)\n END IF\nEND DO',
                     'real(r8), intent(inout) :: a(:,:)\n'
                     'real(r8), allocatable :: t(:,:)')
        self.assertFalse(k.arrays['t']['private'])
        self.assertIn('IF (ALLOCATED(t)) THEN',f)
        self.assertLess(f.index('IF (ALLOCATED(t))'),f.index('LBOUND(t'))

    def test_source_metadata_remains_in_fallback(self):
        block=gen.statements('DO j=1,3\n a(1,j)=0.0_r8\n'
                             'IF (LwSrc(ng)) THEN\n'
                             'DO is=1,Nsrc(ng)\n'
                             'a(1,j)=SOURCES(ng)%Qsrc(is,1)\n'
                             'END DO\nEND IF\nEND DO')
        clean,guards=gen.prune_sources(block)
        self.assertEqual(guards,['.not.lwsrc(ng)'])
        self.assertNotIn('sources',str(clean))

    def test_tracer_flags_are_evaluated_per_tracer(self):
        k,f,cpp=kernel('DO j=1,3\n DO itrc=1,2\n'
                       ' IF (vadvection(itrc,ng)%splines) THEN\n'
                       'a(1,j,itrc)=1.0_r8\n END IF\n END DO\nEND DO',
                       'integer :: j,itrc\nreal(r8), intent(out) :: a(:,:,:)')
        self.assertIn('kk_flag_vadvection_splines(itrc)',cpp)
        self.assertIn('DO kk_it=1,NT(ng)',f)
        self.assertIn('vadvection(kk_it,ng)%splines',f)
        self.assertFalse(k.arrays['kk_flag_vadvection_splines']['private'])

    def test_automatic_flag_bounds_follow_dummy_declarations(self):
        source='''SUBROUTINE pre_step3d_tile(ng,nnew,a)
          USE mod_param
          integer, intent(in) :: ng,nnew
          real(r8), intent(inout) :: a(2,2,NT(ng))
          integer :: i,j,itrc
          DO j=1,2
            DO itrc=1,NT(ng)
              IF (Vadvection(itrc,ng)%SPLINES) THEN
                a(1,j,itrc)=1.0_r8
              END IF
            END DO
          END DO
          END SUBROUTINE pre_step3d_tile'''
        f,_=gen.process(source,'pre_step3d')
        self.assertLess(f.index('integer, intent(in) :: ng'),
                        f.index('real(C_DOUBLE), TARGET :: kk_flag_'))

    def test_grid_extent_capture_is_integer_not_logical(self):
        k,f,_=kernel('DO j=MAX(1,jstr-1),MIN(jend+1,mm(ng))\n'
                     ' DO i=1,lm(ng)\n a(i,j)=1.0_r8\n END DO\nEND DO',
                     'integer :: i,j,ng,jstr,jend\nreal(r8),intent(out) :: a(:,:)')
        self.assertFalse(k.scalars['lm(ng)'][1])
        self.assertFalse(k.scalars['mm(ng)'][1])
        self.assertEqual(k.scalar_types['lm(ng)'],'int')
        self.assertNotIn('MERGE(1,0,lm(ng))',f)

    def test_integer_layer_arrays_preserve_storage_and_division(self):
        k,_,cpp=kernel('DO j=-1,2\n DO i=-2,3\n'
                       ' layers(i,j)=i+7*j\n a(i,j)=layers(i,j)/2\n'
                       ' END DO\nEND DO',
                       'integer :: i,j\ninteger,intent(out) :: layers(-2:3,-1:2)\n'
                       'real(r8),intent(out) :: a(-2:3,-1:2)')
        self.assertIn('Array<2,int> layers',cpp)
        main=['int main(){ KkLoopArgs args{}; int layers[26]{}; double a[24]{};',
              'layers[0]=1234567;layers[25]=-1234567;']
        for slot,name in enumerate(k.arrays):
            ptr='layers+1' if name=='layers' else 'a'
            main += [f'args.p[{slot}]={ptr};',
                     f'args.b[{10*slot}]=-2;args.b[{10*slot+1}]=3;',
                     f'args.b[{10*slot+2}]=-1;args.b[{10*slot+3}]=2;']
        main += ['kk_test_0(&args);',
                 'std::printf("%d %d\\n",layers[0],layers[25]);',
                 'for(int n=0;n<24;++n)std::printf("%d %.0f\\n",layers[n+1],a[n]);}']
        expected=[(i+7*j,int((i+7*j)/2)) for j in range(-1,3) for i in range(-2,4)]
        for reverse,parallel in ((False,False),(True,False),(False,True)):
            got=self.compile_run(cpp,'\n'.join(main),reverse,parallel).splitlines()
            self.assertEqual(got[0],'1234567 -1234567')
            self.assertEqual([tuple(map(int,x.split())) for x in got[1:]],expected)

    def test_integer_layer_dummy_gets_target(self):
        source='''SUBROUTINE lmd_skpp_tile(layers)
          integer,intent(out) :: layers(2,3)
          integer :: i,j
          DO j=1,3
            DO i=1,2
              layers(i,j)=i+j
            END DO
          END DO
          END SUBROUTINE lmd_skpp_tile'''
        f,_=gen.process(source,'lmd_skpp')
        self.assertIn('integer,intent(out) , TARGET :: layers',f)
        self.assertIn('kk_row_contiguous(layers)',f)

    def test_logical_arrays_do_not_use_integer_abi(self):
        with self.assertRaisesRegex(ValueError,'logical arrays'):
            kernel('DO j=1,3\n flags(1,j)=.true.\nEND DO',
                   'logical,intent(out) :: flags(1,3)')

    def test_unguarded_integer_ranks_fail_closed(self):
        with self.assertRaisesRegex(ValueError,'integer arrays currently require rank two'):
            kernel('DO j=1,3\n indices(j)=j\nEND DO',
                   'integer,intent(out) :: indices(3)')

    def test_lmd_finish_retains_lateral_boundary_loops(self):
        source='''SUBROUTINE lmd_finish_tile(a)
          real(r8),intent(inout) :: a(0:2,3)
          integer :: i,j
          DO j=1,3
            DO i=1,2
              a(i,j)=2.0_r8*a(i,j)
            END DO
          END DO
          DO j=1,3
            a(0,j)=a(1,j)
          END DO
          END SUBROUTINE lmd_finish_tile'''
        f,c=gen.process(source,'lmd_vmix')
        self.assertEqual(c.count('extern "C" void'),1)
        self.assertNotIn('a(0,j)',c)
        self.assertIn('a(0,j)=a(1,j)',f)

    def test_integer_intrinsics_preserve_integer_results(self):
        output=self.compile_run('', '''int main(){
          std::printf("%d %d %d",roms_loops::fmax(1,3,2)/2,
             roms_loops::power(3,2)/2,roms_loops::sign(3,-1)/2); }''')
        self.assertEqual(output,'1 4 -1')

    def test_scratch_ownership(self):
        k,_,_=kernel('DO j=1,3\n DO i=1,4\n cf(i,0)=a(i,j)\n'
                     ' fx(i,j)=cf(i,0)\n END DO\nEND DO',
                     'integer :: i,j\nreal(r8),intent(in) :: a(:,:)\n'
                     'real(r8),dimension(1:4,0:3) :: cf\n'
                     'real(r8),dimension(1:4,JminS:JmaxS) :: fx')
        self.assertTrue(k.arrays['cf']['private'])
        self.assertFalse(k.arrays['fx']['private'])

    def compile_run(self, cpp, main, reverse=False, parallel=False):
        compiler=shutil.which('clang++') or shutil.which('c++')
        if not compiler: self.skipTest('host C++ compiler unavailable')
        with tempfile.TemporaryDirectory() as directory:
            d=Path(directory)
            (d/'kokkos_config.hpp').write_text(STUB)
            (d/'cppdefs.h').write_text('')
            shutil.copyfile(ROOT/'kokkos_loops.hpp',d/'kokkos_loops.hpp')
            (d/'test.cpp').write_text('#include "kokkos_loops.hpp"\n'
                '#include <cstdio>\n#include <vector>\n'+cpp+'\n'+main)
            cmd=[compiler,'-std=c++14','-O2','-DROMS_KOKKOS',str(d/'test.cpp'),'-o',str(d/'test')]
            if reverse: cmd.insert(1,'-DREVERSE_ROWS')
            if parallel: cmd[1:1]=['-DPARALLEL_ROWS','-pthread']
            subprocess.run(cmd,check=True,capture_output=True,text=True)
            return subprocess.check_output([str(d/'test')],text=True)

    def test_runtime_bounds_scratch_and_five_dimensional_layout(self):
        body='''DO j=-1,2
          DO k=0,4
            DO i=-2,4
              cf(i,k)=a(i,j,k)*factor
            END DO
          END DO
          DO k=0,4
            DO i=-2,4
              t(i,j,k,nn,itrc)=t(i,j,k,nn,itrc)+cf(i,k)
            END DO
          END DO
        END DO'''
        declarations='''integer :: i,j,k,nn,itrc
          real(r8) :: factor
          real(r8),intent(in) :: a(-2:4,-1:2,0:4)
          real(r8),intent(inout) :: t(-2:4,-1:2,0:4,1:3,1:2)
          real(r8),dimension(-2:4,0:4) :: cf'''
        k,_,cpp=kernel(body,declarations)
        sizes={'a':[(-2,4),(-1,2),(0,4)],'cf':[(-2,4),(0,4)],
               't':[(-2,4),(-1,2),(0,4),(1,3),(1,2)]}
        main=['int main() { KkLoopArgs args{};',
              'std::vector<double> a(140),t(840);',
              'for(int i=0;i<140;++i)a[i]=(i-73)*0.125;',
              'for(int i=0;i<840;++i)t[i]=i*0.03125;']
        for slot,(n,dec) in enumerate(k.arrays.items()):
            if not dec['private']: main += [f'args.p[{slot}]={n}.data();']
            for axis,(lo,hi) in enumerate(sizes[n]):
                main += [f'args.b[{10*slot+2*axis}]={lo};args.b[{10*slot+2*axis+1}]={hi};']
        values={'factor':'0.375','nn':'2','itrc':'2'}
        for i,(expr,_) in enumerate(k.scalars.values()):
            main += [f'args.s[{i}]={values[expr]};']
        main += ['kk_test_0(&args);','for(double v:t)std::printf("%.17g\\n",v);','}']
        expected=[i*0.03125 for i in range(840)]
        for idx in range(140): expected[idx+140+420]+=(idx-73)*0.125*0.375
        for reverse,parallel in ((False,False),(True,False),(False,True)):
            got=list(map(float,self.compile_run(cpp,'\n'.join(main),reverse,parallel).splitlines()))
            self.assertEqual(got,expected)

    def test_c2a_boundary_fills_have_single_row_ownership(self):
        body='''DO j=Jstr,Jend
          DO i=1,2
            Vrho(i,j)=0.5_r8*(v(i,j)+v(i,j+1))
            IF (.not.NSperiodic(ng)) THEN
              IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
                Vrho(i,Jstr-1)=Vrho(i,Jstr)
              END IF
              IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
                Vrho(i,Jend+1)=Vrho(i,Jend)
              END IF
            END IF
          END DO
        END DO'''
        dec='''integer :: i,j,Jstr,Jend
          real(r8), intent(in) :: v(1:2,0:4)
          real(r8), dimension(1:2,JminS:JmaxS) :: Vrho'''
        k,_,cpp=kernel(body,dec,'kk_uv_var_change_0')
        self.assertEqual(cpp.count('if (j=='),2)
        main=['int main(){KkLoopArgs args{}; double v[10],vrho[10]{};',
              'for(int j=0;j<5;++j)for(int i=1;i<=2;++i)v[2*j+i-1]=10*j+i;']
        for slot,name in enumerate(k.arrays):
            main += [f'args.p[{slot}]={name};',
                     f'args.b[{10*slot}]=1;args.b[{10*slot+1}]=2;',
                     f'args.b[{10*slot+2}]=0;args.b[{10*slot+3}]=4;']
        for slot,(expr,logical) in enumerate(k.scalars.values()):
            value=0 if expr=='nsperiodic(ng)' else 3 if expr=='jend' else 1
            main += [f'args.s[{slot}]={value};']
        main += ['kk_uv_var_change_0(&args);for(double x:vrho)std::printf("%.1f ",x); }']
        expected=[16,17,16,17,26,27,36,37,36,37]
        for reverse,parallel in ((False,False),(True,False),(False,True)):
            got=list(map(float,self.compile_run(cpp,'\n'.join(main),reverse,parallel).split()))
            self.assertEqual(got,expected)

    def test_integer_scalar_division_is_not_promoted(self):
        k,_,cpp=kernel('DO j=1,1\n a(1,j)=ncol/2\nEND DO',
                       'integer :: ncol\nreal(r8),intent(out) :: a(:,:)')
        self.assertEqual(k.scalar_types['ncol'],'int')
        main='''int main(){ KkLoopArgs args{}; double value=0;
          args.p[0]=&value;
          args.b[0]=args.b[1]=args.b[2]=args.b[3]=1;
          args.s[0]=5; kk_test_0(&args); std::printf("%.1f",value); }'''
        self.assertEqual(self.compile_run(cpp,main),'2.0')

    def test_signed_zero_sign(self):
        out=self.compile_run('', 'int main(){std::printf("%d",std::signbit(roms_loops::sign(1.0,-0.0)));}')
        self.assertEqual(out,'1')

    def test_sediment_bed_component_arrays_are_registered(self):
        # SEDBED's components come from an include file, not from the module.
        for name in ('avgbedldu','bedldv','bottom'):
            k,_,cpp=kernel('DO j=1,3\n a(1,j)=sedbed(ng)%'+name+'(1,j,1)\nEND DO',
                           'real(r8), intent(out) :: a(:,:)')
            self.assertIn('field_sedbed_'+name,k.arrays)

    def test_diag_reduction_follows_the_configuration(self):
        # The scalars reduced across rows are those the configuration
        # initialises: a 2-D build has no my_max_Cw/my_maxrho.
        head=('      SUBROUTINE diag_tile (ng, tile)\n'
              '      USE mod_kinds\n'
              '      integer :: i, j, ng, tile\n'
              '      real(r8) :: my_max_C, my_max_Cu, my_max_Cv, my_max_Ci\n'
              '      real(r8) :: my_max_Cj, my_max_Ck, my_maxspeed\n')
        decl='      real(r8), dimension(1:3,1:3) :: ke2d\n'
        tail=('        DO j=Jstr,Jend\n'
              '          ke2d(1,j)=0.0_r8\n'
              '        END DO\n'
              '      END IF\n'
              '      END SUBROUTINE diag_tile\n')
        two=(head+decl+'      IF (MOD(iic(ng)-1,ninfo(ng)).eq.0) THEN\n'
             '        my_max_C =0.0_r8\n'
             '        my_max_Cu=0.0_r8\n'
             '        my_max_Cv=0.0_r8\n'
             '        my_max_Ci=0\n'
             '        my_max_Cj=0\n'
             '        my_max_Ck=0\n'
             '        my_maxspeed=0.0_r8\n'+tail)
        out=gen.prepare_diag(two)
        self.assertIn('kk_diag(7,JminS:JmaxS)',out)
        self.assertNotIn('my_max_cw',out)
        self.assertNotIn('my_maxrho',out)
        three=two.replace('        my_maxspeed=0.0_r8\n',
                          '        my_maxspeed=0.0_r8\n'
                          '        my_maxrho=-spval\n')
        three=three.replace('        my_max_Cv=0.0_r8\n',
                            '        my_max_Cv=0.0_r8\n'
                            '        my_max_Cw=0.0_r8\n')
        out=gen.prepare_diag(three)
        self.assertIn('kk_diag(9,JminS:JmaxS)',out)
        self.assertIn('my_max_cw',out)
        self.assertIn('my_maxrho',out)

    def test_untranslated_row_stays_in_fortran_only_when_asked(self):
        # Applications outside the supported set can ask the generator to
        # keep a row it cannot lower.  The strict default is unchanged.
        source=('      SUBROUTINE ana_srflux_tile (ng, tile, i, j)\n'
                '      USE mod_kinds\n'
                '      integer :: i, j, ng, tile\n'
                '      real(r8) :: cff\n'
                '      DO j=1,3\n'
                '        cff=surprise(j)\n'
                '      END DO\n'
                '      END SUBROUTINE ana_srflux_tile\n')
        with self.assertRaisesRegex(ValueError,'unsupported call'):
            gen.process(source,'analytical')
        old=gen.SKIP_UNTRANSLATED
        gen.SKIP_UNTRANSLATED=True
        try:
            text,cpp=gen.process(source,'analytical')
        finally:
            gen.SKIP_UNTRANSLATED=old
        self.assertEqual(text,source)
        self.assertNotIn('kk_ana',cpp)


if __name__=='__main__': unittest.main()
