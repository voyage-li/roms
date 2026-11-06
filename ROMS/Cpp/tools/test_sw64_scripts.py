#!/usr/bin/env python3
"""Host-side checks of the Sunway build/run/case scripts in tools/sw64.

The scripts run for real only on the Sunway login node.  Here each one is
copied into a scratch checkout and driven with stand-ins for module, cmake
and bsub that record their arguments, so the options the scripts hand to the
real tools are verified before anything is queued.
Run: python3 -m unittest discover -s ROMS/Cpp/tools -p 'test_*.py'
"""
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import unittest

SW64 = Path(__file__).resolve().parent / 'sw64'
EXTERNAL = Path(__file__).resolve().parents[2] / 'External'
BASH = shutil.which('bash')


def case(*extra):
    """Run make_app_case.py on the BL_TEST input; return the written text."""
    with tempfile.TemporaryDirectory() as directory:
        source = EXTERNAL / 'roms_bl_test.in'
        subprocess.run([sys.executable, str(SW64 / 'make_app_case.py'), str(source),
                        directory, '--steps', '2', *extra],
                       check=True, capture_output=True, text=True)
        return (Path(directory) / source.name).read_text()


def setting(text, key):
    return re.search(r'(?m)^\s*' + key + r'\s*==\s*(\S+)', text)[1]


class CaseTests(unittest.TestCase):
    def test_no_output_also_disables_station_and_float_intervals(self):
        source = EXTERNAL / 'roms_bl_test.in'
        script = SW64 / 'make_app_case.py'
        with tempfile.TemporaryDirectory() as directory:
            cmd = [sys.executable, str(script), str(source), directory,
                   '--steps', '2', '--no-output']
            subprocess.run(cmd, check=True, capture_output=True, text=True)
            output = Path(directory) / source.name
            text = output.read_text()
            for key in ('NHIS', 'NAVG', 'NDIA', 'NRST', 'NSTA', 'NFLT'):
                self.assertEqual(setting(text, key), '0', key)
            before = text
            again = subprocess.run(cmd, capture_output=True, text=True)
            self.assertNotEqual(again.returncode, 0)
            self.assertIn('refusing to replace', again.stderr)
            self.assertEqual(output.read_text(), before)

    def test_tiles_default_to_one_and_can_match_the_rank_count(self):
        text = case()
        self.assertEqual((setting(text, 'NtileI'), setting(text, 'NtileJ')), ('1', '1'))
        text = case('--tiles', '12', '2')
        self.assertEqual((setting(text, 'NtileI'), setting(text, 'NtileJ')), ('12', '2'))

    def test_output_interval_sets_the_three_write_intervals(self):
        text = case('--steps', '1000', '--output-interval', '100')
        for key in ('NHIS', 'NAVG', 'NDIA'):
            self.assertEqual(setting(text, key), '100', key)
        self.assertEqual(setting(text, 'NRST'), '0')

    def test_output_interval_and_no_output_are_exclusive(self):
        with tempfile.TemporaryDirectory() as directory:
            result = subprocess.run(
                [sys.executable, str(SW64 / 'make_app_case.py'),
                 str(EXTERNAL / 'roms_bl_test.in'), directory,
                 '--no-output', '--output-interval', '10'],
                capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('exclusive', result.stderr)

    def test_tiles_must_be_positive(self):
        with tempfile.TemporaryDirectory() as directory:
            result = subprocess.run(
                [sys.executable, str(SW64 / 'make_app_case.py'),
                 str(EXTERNAL / 'roms_bl_test.in'), directory, '--tiles', '0', '1'],
                capture_output=True, text=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn('positive', result.stderr)


@unittest.skipUnless(BASH, 'bash is required')
class ScriptTests(unittest.TestCase):
    def setUp(self):
        self.root = Path(tempfile.mkdtemp())
        self.addCleanup(shutil.rmtree, self.root)
        tools = self.root / 'ROMS' / 'Cpp' / 'tools' / 'sw64'
        tools.mkdir(parents=True)
        for script in SW64.glob('*.sh'):
            shutil.copy(script, tools)
        (self.root / 'ROMS' / 'Include').mkdir()
        # Scripts are started from a directory of another checkout: the default
        # header directory must still be the one beside the script.
        self.elsewhere = self.root / 'elsewhere'
        (self.elsewhere / 'ROMS' / 'Include').mkdir(parents=True)
        self.bin = self.root / 'bin'
        self.bin.mkdir()
        self.calls = self.root / 'calls.txt'
        self.tools = tools

    def tool(self, name, body):
        path = self.bin / name
        path.write_text('#!/bin/sh\n' + body)
        path.chmod(0o755)

    def env(self, **extra):
        env = dict(os.environ, PATH=str(self.bin) + os.pathsep + os.environ['PATH'],
                   CALLS=str(self.calls))
        for key in ('BACKENDS', 'MPI', 'TAG_SUFFIX', 'KK_STAGES', 'STEP3D_T', 'TESTS',
                    'RANKS', 'RANKS_PER_NODE', 'QUEUE', 'SHARE_SIZE', 'TIMELIMIT'):
            env.pop(key, None)
        env.update(extra)
        return env

    def test_every_script_parses(self):
        for script in SW64.glob('*.sh'):
            subprocess.run([BASH, '-n', str(script)], check=True)

    def compare_app(self, ref_names, test_names):
        ref = self.root / 'ref'
        test = self.root / 'test'
        ref.mkdir()
        test.mkdir()
        for name in ref_names:
            (ref / name).touch()
        for name in test_names:
            (test / name).touch()
        stub = self.tools.parent / 'nccmp.py'
        stub.write_text("import os, sys\n"
                        "with open(os.environ['CMP_CALLS'], 'a') as stream:\n"
                        "    stream.write('\\t'.join(sys.argv[1:]) + '\\n')\n")
        calls = self.root / 'nccmp-calls.txt'
        result = subprocess.run(
            [BASH, str(self.tools / 'cmp_app.sh'), str(ref), str(test)],
            env=self.env(CMP_CALLS=str(calls)), capture_output=True, text=True,
            cwd=str(self.elsewhere))
        return result, calls

    def test_cmp_app_rejects_extra_test_output_before_content_comparison(self):
        result, calls = self.compare_app(('roms_his.nc',),
                                         ('roms_his.nc', 'roms_dia.nc'))
        self.assertEqual(result.returncode, 1)
        self.assertIn('extra test output: roms_dia.nc', result.stderr)
        self.assertFalse(calls.exists())

    def test_cmp_app_rejects_missing_test_output_before_content_comparison(self):
        result, calls = self.compare_app(('roms_his.nc', 'roms_dia.nc'),
                                         ('roms_his.nc',))
        self.assertEqual(result.returncode, 1)
        self.assertIn('missing test output: roms_dia.nc', result.stderr)
        self.assertFalse(calls.exists())

    def test_cmp_app_rejects_empty_output_sets(self):
        result, calls = self.compare_app((), ())
        self.assertEqual(result.returncode, 1)
        self.assertIn('no roms_*.nc in', result.stderr)
        self.assertFalse(calls.exists())

    def test_cmp_app_compares_matching_sets_and_excludes_restart(self):
        outputs = ('roms_his.nc', 'roms_dia.nc', 'roms_rst.nc')
        result, calls = self.compare_app(outputs, outputs)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn('roms_rst.nc', result.stdout)
        args = [line.split('\t') for line in calls.read_text().splitlines()]
        self.assertEqual({Path(line[0]).name for line in args},
                         {'roms_his.nc', 'roms_dia.nc'})
        for line in args:
            self.assertEqual(Path(line[0]).name, Path(line[1]).name)
            self.assertIn('--all-time', line)

    def build(self, *args, **env):
        self.tool('module', 'exit 0\n')
        self.tool('cmake', 'printf "%s\\n" "$*" >> "$CALLS"\n')
        result = subprocess.run([BASH, str(self.tools / 'build_app.sh'), *args],
                                env=self.env(**env), capture_output=True, text=True,
                                cwd=str(self.elsewhere))
        self.assertEqual(result.returncode, 0, result.stderr)
        configures = [line for line in self.calls.read_text().splitlines()
                      if line.startswith('-S ')]
        return {re.search(r'-B \S*/build/(\S+)', line)[1]: line for line in configures}

    @staticmethod
    def define(line, name):
        return re.search(r'-D' + name + r'=(\S*)', line)[1]

    def test_build_defaults(self):
        configures = self.build('UPWELLING', 'SOLVE3D;ROMS_WALL_PROFILE')
        self.assertEqual(set(configures),
                         {'UPWELLING-reference', 'UPWELLING-serial', 'UPWELLING-sw64'})
        reference, sw64 = configures['UPWELLING-reference'], configures['UPWELLING-sw64']
        self.assertEqual(self.define(reference, 'MY_CPP_FLAGS'),
                         'NLM_DRIVER;OUT_DOUBLE;VAR_RHO_2D;DIAGNOSTICS_UV;'
                         'DIAGNOSTICS_TS;SOLVE3D;ROMS_WALL_PROFILE')
        self.assertEqual(self.define(reference, 'ROMS_KOKKOS'), 'OFF')
        flags = self.define(sw64, 'MY_CPP_FLAGS')
        self.assertEqual(flags.count('ROMS_WALL_PROFILE'), 1)
        self.assertTrue(flags.endswith(';KK_STAGE_STEP3DUV;KK_STAGE_GLS;'
                                       'KK_STAGE_PRE3D;KK_STAGE_REMAINDER'), flags)
        self.assertEqual(self.define(sw64, 'ROMS_KOKKOS'), 'ON')
        self.assertEqual(self.define(sw64, 'ROMS_KOKKOS_BACKEND'), 'sw64')
        self.assertEqual(self.define(sw64, 'ROMS_KOKKOS_STEP3D_T'), 'ON')
        self.assertEqual(self.define(sw64, 'ROMS_KOKKOS_TESTS'), 'OFF')
        self.assertEqual(self.define(sw64, 'MPI'), 'OFF')
        # The toolchain file picks the compilers; the script must not.
        self.assertNotIn('COMPILER', sw64)
        self.assertEqual(self.define(sw64, 'MY_HEADER_DIR'),
                         str(self.root / 'ROMS' / 'Include'))

    def test_wall_profile_is_added_when_the_caller_omits_it(self):
        flags = self.define(self.build('KELVIN', BACKENDS='serial')['KELVIN-serial'],
                            'MY_CPP_FLAGS')
        self.assertTrue(flags.startswith('NLM_DRIVER;OUT_DOUBLE;VAR_RHO_2D;'
                                         'DIAGNOSTICS_UV;DIAGNOSTICS_TS;'
                                         'ROMS_WALL_PROFILE;KK_STAGE_FUSED'),
                        flags)

    def test_build_options(self):
        configures = self.build('BL_TEST', 'UV_VIS2', BACKENDS='sw64', MPI='ON',
                                TAG_SUFFIX='-r9', KK_STAGES='KK_STAGE_FUSED;KK_STAGE_TAVG',
                                STEP3D_T='OFF', TESTS='ON')
        line = configures['BL_TEST-sw64-mpi-r9']
        self.assertTrue(self.define(line, 'MY_CPP_FLAGS').endswith(
            'UV_VIS2;ROMS_WALL_PROFILE;KK_STAGE_FUSED;KK_STAGE_TAVG'))
        self.assertEqual(self.define(line, 'MPI'), 'ON')
        self.assertEqual(self.define(line, 'ROMS_KOKKOS_STEP3D_T'), 'OFF')
        self.assertEqual(self.define(line, 'ROMS_KOKKOS_TESTS'), 'ON')

    def test_missing_header_directory_is_reported(self):
        self.tool('module', 'exit 0\n')
        result = subprocess.run([BASH, str(self.tools / 'build_app.sh'), 'X', '', 'nowhere'],
                                env=self.env(), capture_output=True, text=True,
                                cwd=str(self.elsewhere))
        self.assertEqual(result.returncode, 2)
        self.assertIn('No such header directory: nowhere', result.stderr)

    # run_app.sh ---------------------------------------------------------
    def run_dir(self, executables=('romsM',), log=False):
        for name in executables:
            exe = self.root / 'build' / 'TAG' / name
            exe.parent.mkdir(parents=True, exist_ok=True)
            exe.write_text('#!/bin/sh\n')
            exe.chmod(0o755)
        run = self.root / 'run'
        run.mkdir()
        (run / 'roms_case.in').write_text('NTIMES == 2\n')
        if log:
            (run / 'run.log').write_text('old\n')
        self.tool('bsub', 'printf "%s\\n" "$*" > "$CALLS"\n'
                          'echo "Job <4242> is submitted to queue <$4>."\n'
                          'echo " ROMS: DONE"\n')
        return run

    def launch(self, run, **env):
        return subprocess.run([BASH, str(self.tools / 'run_app.sh'), 'TAG', str(run)],
                              env=self.env(**env), capture_output=True, text=True,
                              cwd=str(self.elsewhere))

    @unittest.skipUnless(os.path.exists('/usr/bin/time'), '/usr/bin/time is required')
    def test_run_spreads_ranks_over_nodes(self):
        for ranks, per_node in (('1', '1'), ('2', '2'), ('6', '6'), ('12', '6')):
            with self.subTest(ranks=ranks):
                run = self.run_dir()
                result = self.launch(run, RANKS=ranks)
                self.assertEqual(result.returncode, 0, result.stderr)
                args = self.calls.read_text()
                self.assertIn('-n ' + ranks + ' -np ' + per_node + ' ', args)
                self.assertIn('/build/TAG/romsM ', args)
                self.assertIn('-q q_share', args)
                self.assertIn('-i ' + str(run / 'roms_case.in'), args)
                self.assertIn('Job <4242>', result.stdout)
                shutil.rmtree(run)

    @unittest.skipUnless(os.path.exists('/usr/bin/time'), '/usr/bin/time is required')
    def test_run_defaults_to_one_serial_rank(self):
        run = self.run_dir(executables=('romsS',))
        result = self.launch(run, QUEUE='q_test', RANKS_PER_NODE='3')
        self.assertEqual(result.returncode, 0, result.stderr)
        args = self.calls.read_text()
        self.assertIn('-q q_test -n 1 -np 1 ', args)
        self.assertIn('/build/TAG/romsS ', args)

    def test_run_refuses_unusable_requests(self):
        run = self.run_dir(executables=('romsS',))
        result = self.launch(run, RANKS='2')
        self.assertEqual(result.returncode, 2)
        self.assertIn('needs an MPI build', result.stderr)
        for bad in ('7', '0'):
            result = self.launch(run, RANKS='12', RANKS_PER_NODE=bad)
            self.assertEqual(result.returncode, 2, bad)
        self.assertFalse(self.calls.exists())
        shutil.rmtree(run)
        run = self.run_dir(executables=('romsM',), log=True)
        result = self.launch(run)
        self.assertEqual(result.returncode, 2)
        self.assertIn('already has a log', result.stderr)
        self.assertEqual((run / 'run.log').read_text(), 'old\n')


if __name__ == '__main__':
    unittest.main()
