#!/usr/bin/env python3
"""KPP matrix must compare the same physics and never time restart output."""
from pathlib import Path
import sys
import json
import tempfile
import unittest
from unittest.mock import patch

sys.path.insert(0,str(Path(__file__).resolve().parent/'sw64'))
try:
    import mixing_matrix as module
finally:
    sys.path.pop(0)


class MixingMatrixTests(unittest.TestCase):
    def cache(self,directory,backend='sw64',control=False,mpi=False):
        flags=module.EXTRA+';OUT_DOUBLE;DIAGNOSTICS_UV;DIAGNOSTICS_TS;KK_STAGE_REMAINDER'
        if not control:flags+=';KK_STAGE_LMD'
        text=('MY_CPP_FLAGS:STRING='+flags+'\nROMS_FAST_MATH:BOOL=OFF\n'
              'MPI:BOOL='+('ON' if mpi else 'OFF')+'\n'
              'ROMS_KOKKOS_BACKEND:STRING='+backend+'\nROMS_KOKKOS:BOOL=ON\n')
        (directory/'CMakeCache.txt').write_text(text)
        (directory/'compile_commands.json').write_text(json.dumps([
            dict(file='/source/ROMS/Cpp/kokkos_step2d.cpp',
                 command='swucxx -mieee -O3 -fno-tree-vectorize -o CMakeFiles/Objects.dir/step2d.cpp.o -c /source/ROMS/Cpp/kokkos_step2d.cpp')]))

    def test_candidate_and_control_stage_are_not_interchangeable(self):
        with tempfile.TemporaryDirectory() as folder:
            d=Path(folder);self.cache(d)
            module.verify_build(d,'sw64',False,False)
            with self.assertRaisesRegex(RuntimeError,'stage selection'):
                module.verify_build(d,'sw64',False,True)
            self.cache(d,control=True)
            module.verify_build(d,'sw64',False,True)
            with self.assertRaisesRegex(RuntimeError,'stage selection'):
                module.verify_build(d,'sw64',False,False)

    def test_missing_restart_physics_is_not_accepted(self):
        with tempfile.TemporaryDirectory() as folder:
            d=Path(folder);self.cache(d)
            p=d/'CMakeCache.txt';p.write_text(p.read_text().replace('PERFECT_RESTART;',''))
            with self.assertRaisesRegex(RuntimeError,'physics'):
                module.verify_build(d,'sw64',False,False)

    def test_rank_count_configuration_must_match(self):
        with tempfile.TemporaryDirectory() as folder:
            d=Path(folder);self.cache(d)
            with self.assertRaisesRegex(RuntimeError,'MPI mismatch'):
                module.verify_build(d,'sw64',True,False)

    def test_old_or_overridden_vectorized_build_is_rejected(self):
        with tempfile.TemporaryDirectory() as folder:
            d=Path(folder);self.cache(d)
            p=d/'compile_commands.json';original=p.read_text()
            p.write_text(original.replace('-fno-tree-vectorize',''))
            with self.assertRaisesRegex(RuntimeError,'strict-arithmetic'):
                module.verify_build(d,'sw64',False,False)
            for flag in ('-ftree-vectorize','-ftree-loop-vectorize','-ftree-slp-vectorize',
                         '-Ofast','-ffast-math','-fassociative-math',
                         '-freciprocal-math','-funsafe-math-optimizations'):
                p.write_text(original.replace('-O3','-O3 '+flag))
                with self.assertRaisesRegex(RuntimeError,'strict-arithmetic'):
                    module.verify_build(d,'sw64',False,False)
            p.unlink()
            with self.assertRaisesRegex(RuntimeError,'missing SW64'):
                module.verify_build(d,'sw64',False,False)

    def test_full_restart_is_validation_only(self):
        matrix=object.__new__(module.MixingMatrix)
        with tempfile.TemporaryDirectory() as folder:
            p=Path(folder)/'input.in';original='NRST == 0\nLcycleRST == T\n'
            p.write_text(original);matrix.prepare_input(p,True)
            self.assertEqual(p.read_text(),original)
            matrix.prepare_input(p,False)
            self.assertEqual(p.read_text(),'NRST == 1\nLcycleRST == F\n')

    def test_all_mixing_fields_must_really_be_compared(self):
        matrix=object.__new__(module.MixingMatrix)
        with tempfile.TemporaryDirectory() as folder:
            d=Path(folder);(d/'roms_rst.nc').touch();results=[]
            for filename,fields in module.REQUIRED_FIELDS.items():
                log=d/(filename+'.log')
                log.write_text(''.join(f'{f} maxabs=0.00000000e+00 PASS\n' for f in fields))
                results.append(dict(file=filename,log=str(log),strict_pass=True))
            with patch.object(module.base.Matrix,'compare',return_value=results) as compare:
                matrix.compare('kpp16',d,d,'test')
                self.assertTrue(compare.call_args.kwargs['include_restart'])
                (d/'roms_rst.nc.log').write_text('ghat_temp maxabs=0 PASS\n')
                with self.assertRaisesRegex(RuntimeError,'ghat_salt'):
                    matrix.compare('kpp16',d,d,'test')


if __name__=='__main__':unittest.main()
