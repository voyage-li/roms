#!/usr/bin/env python3
"""Host-only regression for scratch ownership, independent of Kokkos."""
from pathlib import Path
import shutil
import subprocess
import tempfile
import unittest

CPP = Path(__file__).resolve().parents[1]
CXX = shutil.which('c++')


@unittest.skipUnless(CXX, 'a host C++ compiler is required')
class WorkspaceTests(unittest.TestCase):
    def test_reuse_growth_and_independent_ownership(self):
        source = r'''
#include "kokkos_workspace.hpp"
#include <cassert>

int main() {
  roms::HostWorkspace a, b;
  assert(a.reserve(0) == nullptr);
  double* first = a.reserve(8);
  assert(first != nullptr);
  for (int i = 0; i < 8; ++i) first[i] = i + 0.25;
  assert(a.reserve(4) == first);
  assert(a.reserve(8) == first);
  assert(a.reserve(0) == first);
  for (int i = 0; i < 8; ++i) assert(first[i] == i + 0.25);

  double* other = b.reserve(8);
  assert(other != first);
  other[0] = -7.0;
  assert(first[0] == 0.25);

  double* grown = a.reserve(32);
  assert(grown != nullptr);
  for (int i = 0; i < 32; ++i) grown[i] = 2.0 * i;
  assert(a.reserve(16) == grown);
  assert(a.reserve(32) == grown);
  assert(grown[31] == 62.0);
  assert(other[0] == -7.0);
}
'''
        with tempfile.TemporaryDirectory() as directory:
            directory = Path(directory)
            src = directory / 'workspace.cpp'
            src.write_text(source)
            executable = directory / 'workspace'
            subprocess.run([CXX, '-std=c++11', '-Wall', '-Wextra', '-pedantic',
                            '-I', str(CPP), str(src), '-o', str(executable)],
                           check=True, capture_output=True, text=True, timeout=60)
            subprocess.run([str(executable)], check=True, capture_output=True,
                           text=True, timeout=10)


if __name__ == '__main__':
    unittest.main()
