#!/usr/bin/env python3
"""Parser and failure-mode regressions; no NetCDF Python package needed."""
import contextlib
import io
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


if __name__ == '__main__':
    unittest.main()
