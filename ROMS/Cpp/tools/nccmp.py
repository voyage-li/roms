#!/usr/bin/env python3
"""
Field-by-field comparison of two ROMS NetCDF histories.

Written because the build machines have no netCDF4 Python module: the
data is read back through "ncdump -p <digits>,<digits>" and parsed here.
That is slower than a direct library read but needs nothing installed
and keeps full precision in the text.

Usage:
    nccmp.py <ref.nc> <test.nc> [--vars v1,v2,...] [--tol T] [--quiet]

For every variable it reports, over all time records at once:
    maxabs     largest |a - b|
    maxrel     largest |a - b| / max|a|   (guards against max|a| == 0)
    where      flat index and record of the worst point
    trend      maxabs on the first, middle and last record, so a growing
               difference can be told apart from a constant one

Exit status is 0 when every variable is within --tol (default 0, meaning
exact) and 1 otherwise, so this can gate a test script.
"""

import argparse
import re
import subprocess
import sys

# ncdump prints floats in whatever precision -p asks for.  Ask for more
# digits than double has so the round trip is exact.
PRECISION = "17,17"

DEFAULT_VARS = [
    "zeta", "ubar", "vbar",
    "u", "v",
    "ubar_prsgrd", "vbar_prsgrd",
    "ubar_hadv", "vbar_hadv",
    "ubar_xadv", "ubar_yadv",
    "vbar_xadv", "vbar_yadv",
    "ubar_cor", "vbar_cor",
    "ubar_hvisc", "vbar_hvisc",
    "ubar_xvisc", "ubar_yvisc",
    "vbar_xvisc", "vbar_yvisc",
    "ubar_sstr", "vbar_sstr",
    "ubar_bstr", "vbar_bstr",
    "ubar_accel", "vbar_accel",
]

NUM = re.compile(r"-?(?:\d+\.\d+(?:[eE][-+]?\d+)?|\d+\.?\d*[eE][-+]?\d+|\d+\.\d*|\d+|nan|NaN|inf|Infinity)")


def read_var(path, name):
    """Return (values, dims) for one variable, or (None, None)."""
    try:
        out = subprocess.run(
            ["ncdump", "-p", PRECISION, "-v", name, path],
            capture_output=True, text=True, check=False,
        ).stdout
    except OSError as exc:
        sys.exit(f"ncdump failed: {exc}")
    if not out:
        return None, None

    # Dimensions come from the header so the record count is known.
    m = re.search(r"^\s*\w+\s+" + re.escape(name) + r"\(([^)]*)\)\s*;",
                  out, re.M)
    dims = [d.strip() for d in m.group(1).split(",")] if m else []

    # The assignment lives in the data section.  Find it by looking for
    # the name followed by "=" at the start of a line's payload.
    start = re.search(r"^[ \t]*" + re.escape(name) + r"[ \t]*=", out, re.M)
    if not start:
        return None, dims
    end = out.find("\n }", start.end())
    if end < 0:
        end = out.find("\n}", start.end())
    if end < 0:
        end = len(out)
    body = out[start.end():end]

    vals = NUM.findall(body)
    return [float(v) for v in vals], dims


def fmt(x):
    return "       0" if x == 0 else f"{x:.6e}"


def compare(ref, test, name, tol):
    a, dims = read_var(ref, name)
    if a is None:
        return None
    b, _ = read_var(test, name)
    if b is None:
        print(f"  {name:14s} MISSING in test")
        return False

    n = min(len(a), len(b))
    if n == 0:
        return None
    if len(a) != len(b):
        print(f"  {name:14s} SIZE MISMATCH ref={len(a)} test={len(b)}")
        return False

    # Flat index -> per-record maxabs, for the growth check.  The
    # record count comes from the "-- (N currently)" comment ncdump
    # appends to an UNLIMITED dimension; without it a difference that
    # stays bounded cannot be told from one that is growing.
    nrec = 1
    m = re.search(r"ocean_time\s*=\s*UNLIMITED\s*;\s*//\s*\((\d+)",
                  _header(ref))
    if m:
        nrec = int(m.group(1))
    else:
        m = re.search(r"ocean_time\s*=\s*(\d+)", _header(ref))
        if m:
            nrec = int(m.group(1))

    per_rec = n // nrec if nrec > 1 else n
    recmax = []
    worst = (0.0, 0, 0.0, 0.0)
    amax = 0.0
    for k in range(n):
        d = abs(a[k] - b[k])
        if abs(a[k]) > amax:
            amax = abs(a[k])
        if d > worst[0]:
            worst = (d, k, a[k], b[k])
        if nrec > 1:
            r = k // per_rec
            if r >= len(recmax):
                recmax.append(0.0)
            if d > recmax[r]:
                recmax[r] = d

    rel = worst[0] / (amax or 1.0)
    # Per-record relative difference against that record's own peak
    # magnitude.  This is the number that matters for "is this within
    # acceptable accuracy": a constant 1e-9 relative offset everywhere
    # is round-off, a value that climbs record after record is a bug.
    trend = ""
    if len(recmax) > 1:
        peak = []
        for r in range(len(recmax)):
            lo, hi = r * per_rec, min((r + 1) * per_rec, n)
            pk = max((abs(a[k]) for k in range(lo, hi)), default=0.0)
            peak.append(pk or 1.0)
        pct = [100.0 * recmax[r] / peak[r] for r in range(len(recmax))]
        trend = "  [%] " + " ".join(f"{v:.2e}" for v in pct)
    ok = worst[0] <= tol
    mark = "ok" if ok else "DIFF"
    print(f"  {name:14s} maxabs={fmt(worst[0])} maxrel={fmt(rel)} "
          f"at idx {worst[1]}{trend}  {mark}")
    if not ok and tol == 0:
        print(f"                 ref={worst[2]!r} test={worst[3]!r}")
    return ok


_HDR_CACHE = {}


def _header(path):
    if path not in _HDR_CACHE:
        _HDR_CACHE[path] = subprocess.run(
            ["ncdump", "-h", path], capture_output=True, text=True).stdout
    return _HDR_CACHE[path]


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("ref")
    ap.add_argument("test")
    ap.add_argument("--vars", default="")
    ap.add_argument("--tol", type=float, default=0.0)
    ap.add_argument("--quiet", action="store_true")
    args = ap.parse_args()

    names = [v.strip() for v in args.vars.split(",") if v.strip()] \
        if args.vars else DEFAULT_VARS

    print(f"ref  : {args.ref}")
    print(f"test : {args.test}")
    print(f"tol  : {args.tol:g}")

    results = []
    checked = 0
    for name in names:
        r = compare(args.ref, args.test, name, args.tol)
        if r is None:
            continue
        checked += 1
        results.append(r)

    if checked == 0:
        print("\nNo variables compared.")
        return 1

    bad = [n for n, r in zip(names, results) if not r]
    print(f"\n{checked - len(bad)}/{checked} variables within tol")
    if bad:
        print("outside:", ", ".join(bad))
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
