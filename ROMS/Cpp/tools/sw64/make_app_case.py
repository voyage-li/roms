#!/usr/bin/env python3
"""Create a short run of any ROMS application from its standard input file.

  make_app_case.py SOURCE_IN DIRECTORY [--size LM MM] [--steps N]
                   [--dt DT] [--tiles NI NJ] [--no-output]

SOURCE_IN is the application's roms_*.in (for example official/kelvin/
roms_kelvin.in or ROMS/External/roms_canyon3d.in).  The copy keeps every
physics choice and only changes grid size, step count, time step, tiling
and output frequency.  NI*NJ must equal the number of MPI ranks (ROMS
runs one tile per rank).  It refuses to replace an existing input.
"""
import argparse
from pathlib import Path
import re

p = argparse.ArgumentParser(description=__doc__,
                            formatter_class=argparse.RawDescriptionHelpFormatter)
p.add_argument("source", type=Path)
p.add_argument("directory", type=Path)
p.add_argument("--size", type=int, nargs=2, metavar=("LM", "MM"))
p.add_argument("--steps", type=int, default=2)
p.add_argument("--dt", type=str)
p.add_argument("--tiles", type=int, nargs=2, default=(1, 1), metavar=("NI", "NJ"))
p.add_argument("--no-output", action="store_true")
a = p.parse_args()
if min(a.tiles) < 1:
    p.error("--tiles must be positive")
root = Path(__file__).resolve().parents[4]
text = a.source.read_text()
out = 0 if a.no_output else 1
values = dict(NTIMES=a.steps, NtileI=a.tiles[0], NtileJ=a.tiles[1], NHIS=out,
              NAVG=out, NDIA=out, NRST=0)
if a.size:
    values.update(Lm=a.size[0], Mm=a.size[1])
if a.dt:
    values.update(DT=a.dt)
for key, value in values.items():
    text, n = re.subn(r'(?m)^(\s*' + key + r'\s*==\s*)\S+',
                      lambda m: m[1] + str(value), text)
    if n != 1:
        raise SystemExit(f"expected one {key} setting, found {n}")
# Stations/floats have independent output intervals. In particular BL_TEST
# enables STATIONS, so disabling only history/averages/diagnostics still
# writes NetCDF during a purported no-output timing run.
if a.no_output:
    for key in ("NSTA", "NFLT"):
        text = re.sub(r'(?m)^(\s*' + key + r'\s*==\s*)\S+',
                      lambda m: m[1] + "0", text)
# Optional NTSAVG/NTSDIA start the averages at the first step.
for key in ("NTSAVG", "NTSDIA"):
    text = re.sub(r'(?m)^(\s*' + key + r'\s*==\s*)\S+',
                  lambda m: m[1] + "1", text)
yaml = root / 'ROMS/External/varinfo.yaml'
text = re.sub(r"(?m)^(\s*VARNAME\s*=\s*)\S+", lambda m: m[1] + str(yaml), text)
# Station/float inputs referenced by name are taken from beside the source.
for m in set(re.findall(r'(?m)^\s*(?:SPOSNAM|FPOSNAM)\s*=\s*(\S+)', text)):
    for cand in (a.source.parent / m, root / 'ROMS/External' / m):
        if cand.exists():
            text = text.replace(m, str(cand.resolve()))
            break
a.directory.mkdir(parents=True, exist_ok=True)
output = a.directory / a.source.name
if output.exists():
    raise SystemExit(f"refusing to replace {output}")
output.write_text(text)
print(output)
