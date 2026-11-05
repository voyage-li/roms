#!/usr/bin/env python3
"""Compare ROMS NetCDF arrays using ncdump, without Python NetCDF modules.

--tol is an absolute tolerance; --rtol scales with the reference field's
maximum magnitude. Missing fields, incomplete records and nonfinite values
are failures, not silently skipped comparisons. With neither tolerance,
values must agree exactly (this is not a bytewise comparison of the files).
"""
import argparse
import math
import re
import subprocess
import sys

DEFAULT_VARS = [
    "zeta", "ubar", "vbar", "u", "v",
    "ubar_prsgrd", "vbar_prsgrd", "ubar_hadv", "vbar_hadv",
    "ubar_xadv", "ubar_yadv", "vbar_xadv", "vbar_yadv",
    "ubar_cor", "vbar_cor", "ubar_hvisc", "vbar_hvisc",
    "ubar_xvisc", "ubar_yvisc", "vbar_xvisc", "vbar_yvisc",
    "ubar_sstr", "vbar_sstr", "ubar_bstr", "vbar_bstr",
    "ubar_accel", "vbar_accel",
]


def dump(path, *options):
    result = subprocess.run(["ncdump", *options, path], capture_output=True,
                            text=True, check=False)
    if result.returncode:
        raise ValueError(result.stderr.strip() or f"ncdump failed: {path}")
    return result.stdout


class Dataset:
    def __init__(self, path):
        self.path = path
        self.header = dump(path, "-h")
        self.dimensions = {}
        for name, size in re.findall(r"^\s*(\w+)\s*=\s*(\d+)\s*;",
                                     self.header, re.M):
            self.dimensions[name] = int(size)
        for name, size in re.findall(
                r"^\s*(\w+)\s*=\s*UNLIMITED\s*;\s*//\s*\((\d+) currently\)",
                self.header, re.M):
            self.dimensions[name] = int(size)
        self.variables = {}
        for name, dims in re.findall(
                r"^\s*\w+\s+(\w+)\(([^)]*)\)\s*;", self.header, re.M):
            self.variables[name] = tuple(d.strip() for d in dims.split(","))

    def read(self, name):
        if name not in self.variables:
            raise ValueError(f"{name} missing from {self.path}")
        dims = self.variables[name]
        shape = tuple(self.dimensions[d] for d in dims)
        expected = math.prod(shape)
        if not expected:
            raise ValueError(f"{name}: no records in {self.path}")
        output = dump(self.path, "-p", "17,17", "-v", name)
        data = output[output.index("\ndata:") + 6:]
        start = re.search(r"^\s*" + re.escape(name) + r"\s*=", data, re.M)
        if not start:
            raise ValueError(f"{name}: data absent in {self.path}")
        end = data.index(";", start.end())
        values = []
        for token in re.split(r"[,\s]+", data[start.end():end].strip()):
            if not token:
                continue
            if token == "_":
                raise ValueError(f"{name}: unwritten/fill data in {self.path}")
            value = float(token)
            if not math.isfinite(value):
                raise ValueError(f"{name}: nonfinite data in {self.path}")
            values.append(value)
        if len(values) != expected:
            raise ValueError(f"{name}: expected {expected} values, got {len(values)}")
        return dims, shape, values


def compare(a, b, name, atol, rtol, quiet, zero_tol=0.0):
    dims, shape, av = a.read(name)
    bdims, bshape, bv = b.read(name)
    if (dims, shape) != (bdims, bshape):
        raise ValueError(f"{name}: dimensions differ: {(dims, shape)} vs {(bdims, bshape)}")
    worst = max(range(len(av)), key=lambda i: abs(av[i] - bv[i]))
    delta = abs(av[worst] - bv[worst])
    peak = max(map(abs, av))
    relative = delta / peak if peak else (0.0 if delta == 0 else math.inf)
    limit = atol + rtol * peak if peak else max(atol, zero_tol)
    ok = delta <= limit
    if not quiet or not ok:
        ratio = (f"normrel={relative:.8e} pct={100 * relative:.4e}%" if peak
                 else f"zero-reference abs-limit={limit:.3e}")
        print(f"{name:15s} maxabs={delta:.8e} {ratio} "
              f"idx={worst} {'PASS' if ok else 'FAIL'}")
        if dims and dims[0] == "ocean_time":
            n = math.prod(shape[1:])
            trend = [max(abs(x - y) for x, y in zip(av[i:i+n], bv[i:i+n]))
                     for i in range(0, len(av), n)]
            print(" " * 16 + "per-record maxabs=" + " ".join(f"{d:.3e}" for d in trend))
    return ok


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("ref")
    parser.add_argument("test")
    which = parser.add_mutually_exclusive_group()
    which.add_argument("--vars", default="",
                       help="comma-separated variables (default: a momentum-budget list)")
    which.add_argument("--all-time", action="store_true",
                       help="compare every variable whose first dimension is "
                            "ocean_time, as listed in the reference file")
    parser.add_argument("--tol", type=float, default=0.0)
    parser.add_argument("--rtol", type=float, default=0.0)
    parser.add_argument("--zero-tol", type=float, default=0.0,
                        help="absolute tolerance only for identically zero reference fields")
    parser.add_argument("--quiet", action="store_true")
    args = parser.parse_args()
    if not all(math.isfinite(x) and x >= 0 for x in (args.tol, args.rtol, args.zero_tol)):
        parser.error("tolerances must be finite and nonnegative")
    try:
        a, b = Dataset(args.ref), Dataset(args.test)
        if "ocean_time" in a.variables or "ocean_time" in b.variables:
            if a.read("ocean_time") != b.read("ocean_time"):
                raise ValueError("record times differ")
        if args.all_time:
            names = [n for n, d in a.variables.items()
                     if d and d[0] == "ocean_time" and n != "ocean_time"]
            extra = [n for n, d in b.variables.items()
                     if d and d[0] == "ocean_time" and n not in a.variables]
            if extra:
                raise ValueError("only in test file: " + ",".join(extra))
        elif args.vars:
            names = [s.strip() for s in args.vars.split(",") if s.strip()]
        else:
            names = [s for s in DEFAULT_VARS if s in a.variables or s in b.variables]
        if not names:
            raise ValueError("no variables selected")
        failed = []
        for name in names:
            try:
                if not compare(a, b, name, args.tol, args.rtol, args.quiet, args.zero_tol):
                    failed.append(name)
            except ValueError as exc:
                print(f"FAIL: {exc}", file=sys.stderr)
                failed.append(name)
        print(f"{len(names) - len(failed)}/{len(names)} variables within tolerance")
        return int(bool(failed))
    except (OSError, ValueError) as exc:
        print(f"FAIL: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
