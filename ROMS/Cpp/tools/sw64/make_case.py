#!/usr/bin/env python3
"""Create a short upwelling run without changing the canonical input."""
import argparse
from pathlib import Path
import re

p = argparse.ArgumentParser(description=__doc__)
p.add_argument("directory", type=Path)
p.add_argument("--size", type=int, default=0)
p.add_argument("--steps", type=int, default=2)
p.add_argument("--tiles-i", type=int, default=1)
p.add_argument("--tiles-j", type=int, default=1)
p.add_argument("--no-output", action="store_true")
a = p.parse_args()
root = Path(__file__).resolve().parents[4]
text = (root / 'ROMS/External/roms_upwelling.in').read_text()
values = dict(NTIMES=a.steps, NtileI=a.tiles_i, NtileJ=a.tiles_j,
              NHIS=0 if a.no_output else 1, NAVG=0 if a.no_output else 1,
              NDIA=0 if a.no_output else 1, NTSAVG=1, NTSDIA=1, NRST=0)
if a.size:
    values.update(Lm=a.size, Mm=a.size)
for key, value in values.items():
    text, n = re.subn(r'(?m)^(\s*' + key + r'\s*==\s*)\S+',
                       lambda m: m[1] + str(value), text)
    if n != 1:
        raise SystemExit(f"expected one {key} setting, found {n}")
# ROMS opens this table and optional float/station input relative to the run.
for filename in ('varinfo.yaml', 'floats_upwelling.in', 'stations_upwelling.in'):
    source = root / 'ROMS/External' / filename
    if source.exists():
        text = re.sub(r"(?m)^(\s*VARNAME\s*=\s*)\S+",
                      lambda m: m[1] + str(source), text) if filename == "varinfo.yaml" else text.replace(filename, str(source))
a.directory.mkdir(parents=True, exist_ok=True)
output = a.directory / 'roms_upwelling.in'
if output.exists():
    raise SystemExit(f"refusing to replace {output}")
output.write_text(text)
print(output)
