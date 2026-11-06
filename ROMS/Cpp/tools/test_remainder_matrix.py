#!/usr/bin/env python3
"""Acceptance-policy tests: baseline roundoff must never hide a regression."""
import importlib.util
from pathlib import Path
import subprocess
import tempfile
import unittest
from unittest.mock import patch

spec=importlib.util.spec_from_file_location('remainder_matrix',
    Path(__file__).resolve().parent/'sw64'/'remainder_matrix.py')
module=importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class AcceptanceTests(unittest.TestCase):
    def compare_sets(self,reference,test_files,results,exact=False,enforce=True,
                     include_restart=False):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory)
            ref=root/'ref'; test=root/'test'
            ref.mkdir(); test.mkdir(); (root/'canyon').mkdir()
            for filename in reference: (ref/filename).touch()
            for filename in test_files: (test/filename).touch()
            matrix=object.__new__(module.Matrix);matrix.out=root
            with patch.object(module.subprocess,'run',side_effect=results):
                return matrix.compare('canyon',ref,test,'check',exact,enforce,include_restart)

    def compare(self,filename,results,exact=False,enforce=True,include_restart=False):
        return self.compare_sets((filename,),(filename,),results,exact,enforce,include_restart)

    def failed(self):
        return subprocess.CompletedProcess([],1,
            'temp_vdiff maxabs=1.00000000e-18 normrel=1.00000000e-03 idx=1 FAIL\n')

    def passed(self):
        return subprocess.CompletedProcess([],0,'1/1 variables within tolerance\n')

    def test_new_serial_must_match_control_exactly(self):
        with self.assertRaisesRegex(RuntimeError,'strict field comparison failed'):
            self.compare('roms_dia.nc',[self.failed()],exact=True)

    def test_original_fortran_exact_failure_is_retained(self):
        result=self.compare('roms_his.nc',[self.failed()],exact=True,enforce=False)
        self.assertFalse(result[0]['strict_pass'])
        self.assertEqual(len(result[0]['strict_failures']),1)
        self.assertNotIn('absolute_crosscheck_pass',result[0])

    def test_physical_fields_do_not_get_diagnostic_absolute_floor(self):
        with self.assertRaisesRegex(RuntimeError,'strict field comparison failed'):
            self.compare('roms_his.nc',[self.failed()])

    def test_extra_test_output_fails_before_content_comparison(self):
        with self.assertRaisesRegex(RuntimeError,'extra in test: roms_dia.nc'):
            self.compare_sets(('roms_his.nc',),
                              ('roms_his.nc','roms_dia.nc'),[])

    def test_missing_test_output_fails_before_content_comparison(self):
        with self.assertRaisesRegex(RuntimeError,'missing from test: roms_dia.nc'):
            self.compare_sets(('roms_his.nc','roms_dia.nc'),
                              ('roms_his.nc',),[])

    def test_matching_output_sets_are_compared(self):
        result=self.compare_sets(('roms_dia.nc','roms_his.nc'),
                                 ('roms_dia.nc','roms_his.nc'),
                                 [self.passed(),self.passed()])
        self.assertEqual([item['file'] for item in result],
                         ['roms_dia.nc','roms_his.nc'])

    def test_restart_is_excluded_from_both_output_sets_by_default(self):
        result=self.compare_sets(('roms_his.nc','roms_rst.nc'),
                                 ('roms_his.nc','roms_rst.nc'),[self.passed()])
        self.assertEqual([item['file'] for item in result],['roms_his.nc'])

    def test_absolute_crosscheck_does_not_become_strict_pass(self):
        result=self.compare('roms_dia.nc',[self.failed(),self.passed()])
        self.assertFalse(result[0]['strict_pass'])
        self.assertTrue(result[0]['absolute_crosscheck_pass'])
        self.assertEqual(len(result[0]['strict_failures']),1)

    def test_large_diagnostic_error_stops_matrix(self):
        with self.assertRaisesRegex(RuntimeError,'exceeds absolute cross-check'):
            self.compare('roms_dia.nc',[self.failed(),self.failed()])

    def test_restart_is_compared_when_requested(self):
        result=self.compare('roms_rst.nc',[self.passed()],exact=True,include_restart=True)
        self.assertTrue(result[0]['strict_pass'])
        self.assertEqual(result[0]['file'],'roms_rst.nc')

    def test_restart_physical_fields_keep_strict_tolerance(self):
        with self.assertRaisesRegex(RuntimeError,'strict field comparison failed'):
            self.compare('roms_rst.nc',[self.failed()],include_restart=True)

    def test_any_fallback_to_fortran_invalidates_a_run(self):
        # A fallback would validate and time the wrong code.  The wording must
        # stay the one the Fortran sources print.
        root=Path(__file__).resolve().parents[2]/'Nonlinear'
        for name in ('step3d_t.F','pre_step3d.F'):
            self.assertIn('whole-tile Fortran fallback',(root/name).read_text(),name)
        for line in ('step3d_t: whole-tile Fortran fallback (tracer scheme)',
                     'pre_step3d: whole-tile Fortran fallback (point sources)',
                     'Kokkos row fallback: rhs3d','forrtl: ERROR STOP','BLOWUP'):
            self.assertTrue(module.FAILED_RUN.search(line),line)
        self.assertFalse(module.FAILED_RUN.search(
            'ROMS time stepping wall [rank=0]: 3.541 s\n ROMS: DONE'))

    def test_no_reference_outputs_is_failure(self):
        with tempfile.TemporaryDirectory() as directory:
            root=Path(directory); ref=root/'ref'; ref.mkdir()
            matrix=object.__new__(module.Matrix);matrix.out=root
            with self.assertRaisesRegex(RuntimeError,'no reference NetCDF'):
                matrix.compare('canyon',ref,root,'check')

if __name__=='__main__':unittest.main()
