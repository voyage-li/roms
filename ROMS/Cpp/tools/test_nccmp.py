#!/usr/bin/env python3
"""Parser and failure-mode regressions; no NetCDF Python package needed."""
import contextlib
import io
import sys
import unittest
from unittest.mock import patch
import nccmp

HEADER = '''netcdf test {
dimensions:
  ocean_time = UNLIMITED ; // (2 currently)
  eta_rho = 1 ;
  xi_rho = 2 ;
variables:
  double ocean_time(ocean_time) ;
  double zeta(ocean_time, eta_rho, xi_rho) ;
}'''


class ComparisonTests(unittest.TestCase):
    def datasets(self, values='1, 2, 3, 4', test_header=HEADER, ref_values='1, 2, 3, 4'):
        def dump(path, *args):
            if args == ('-h',):
                return HEADER if path == 'ref' else test_header
            name = args[-1]
            data = '0, 300' if name == 'ocean_time' else (
                ref_values if path == 'ref' else values)
            return 'netcdf test {\ndata:\n ' + name + ' = ' + data + ';\n}'
        self.patch = patch.object(nccmp, 'dump', side_effect=dump)
        self.patch.start()
        self.addCleanup(self.patch.stop)
        return nccmp.Dataset('ref'), nccmp.Dataset('test')

    def compare(self, a, b, atol=0, rtol=0, zero_tol=0):
        with contextlib.redirect_stdout(io.StringIO()):
            return nccmp.compare(a, b, 'zeta', atol, rtol, True, zero_tol)

    def test_exact(self):
        self.assertTrue(self.compare(*self.datasets()))

    def test_difference(self):
        self.assertFalse(self.compare(*self.datasets('1, 2, 3, 5')))

    def test_scaled_tolerance(self):
        self.assertTrue(self.compare(*self.datasets('1, 2, 3, 4.001'), rtol=0.001))

    def test_zero_reference_tolerance(self):
        a, b = self.datasets('0, 0, 0, 1e-24', ref_values='0, 0, 0, 0')
        self.assertFalse(self.compare(a, b, rtol=1e-10))
        self.assertTrue(self.compare(a, b, rtol=1e-10, zero_tol=1e-20))

    def test_nonfinite_and_unwritten(self):
        for token in ('nan', 'NaN', 'Infinity', '-inf', '_'):
            a, b = self.datasets('1, 2, 3, ' + token)
            with self.assertRaises(ValueError):
                self.compare(a, b)
            self.patch.stop()

    def test_incomplete(self):
        with self.assertRaises(ValueError):
            self.compare(*self.datasets('1, 2, 3'))

    def test_missing_required_variable(self):
        a, b = self.datasets(test_header=HEADER.replace('double zeta(', 'double other('))
        with self.assertRaises(ValueError):
            self.compare(a, b)

    def test_dimension_names(self):
        changed = HEADER.replace('zeta(ocean_time, eta_rho, xi_rho)',
                                 'zeta(ocean_time, xi_rho, eta_rho)')
        with self.assertRaises(ValueError):
            self.compare(*self.datasets(test_header=changed))

    def run_main(self, *argv):
        with patch.object(sys, 'argv', ['nccmp.py', 'ref', 'test', *argv]), \
                contextlib.redirect_stdout(io.StringIO()) as out, \
                contextlib.redirect_stderr(io.StringIO()):
            return nccmp.main(), out.getvalue()

    def test_all_time_covers_every_time_dependent_field(self):
        header = HEADER.replace('}', '  double temp(ocean_time, eta_rho, xi_rho) ;\n}')
        values = {'ref': header, 'test': header}
        def dump(path, *args):
            if args == ('-h',):
                return values[path]
            name = args[-1]
            data = '0, 300' if name == 'ocean_time' else '1, 2, 3, 4'
            return 'netcdf test {\ndata:\n ' + name + ' = ' + data + ';\n}'
        with patch.object(nccmp, 'dump', side_effect=dump):
            status, out = self.run_main('--all-time')
        self.assertEqual(status, 0)
        self.assertIn('2/2 variables', out)

    def test_vars_and_all_time_are_exclusive(self):
        # --vars used to be dropped silently when --all-time was also given.
        with self.assertRaises(SystemExit) as raised:
            self.run_main('--all-time', '--vars', 'zeta')
        self.assertEqual(raised.exception.code, 2)


if __name__ == '__main__':
    unittest.main()
