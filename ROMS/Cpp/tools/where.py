#!/usr/bin/env python3
"""
Spatial localisation of the difference between two ROMS records.

nccmp.py answers "how big"; this answers "where".  For a 2-D field it
prints the (j, i) of the worst point and the worst point on each edge,
which is usually enough to tell a halo/edge bug from a stencil bug in
the interior.

Usage:
    where.py <ref.nc> <test.nc> <var> [record]
"""

import re
import subprocess
import sys

PRECISION = "17,17"
NUM = re.compile(r"-?(?:\d+\.\d+(?:[eE][-+]?\d+)?|\d+\.?\d*[eE][-+]?\d+|\d+\.\d*|\d+|nan|NaN)")


def header(path):
    return subprocess.run(["ncdump", "-h", path],
                          capture_output=True, text=True).stdout


def dims_of(path, name):
    out = header(path)
    m = re.search(rf"^\s*\w+\s+{re.escape(name)}\(([^)]*)\)\s*;", out, re.M)
    if not m:
        return None, None, None
    dimlen = {}
    for d in re.finditer(r"^\s*(\w+)\s*=\s*(UNLIMITED|\d+)\s*;", out, re.M):
        dimlen[d.group(1)] = d.group(2)
    m2 = re.search(rf"ocean_time = UNLIMITED ; // \((\d+) currently\)", out)
    if m2:
        dimlen["ocean_time"] = m2.group(1)
    names = [d.strip() for d in m.group(1).split(",")]
    sizes = [int(dimlen.get(n, 0)) for n in names]
    return names, sizes, dimlen


def read(path, name):
    out = subprocess.run(["ncdump", "-p", PRECISION, "-v", name, path],
                         capture_output=True, text=True).stdout
    st = re.search(rf"^[ \t]*{re.escape(name)}[ \t]*=", out, re.M)
    if not st:
        return []
    end = out.find("\n }", st.end())
    if end < 0:
        end = len(out)
    return [float(v) for v in NUM.findall(out[st.end():end])]


def main():
    ref, test, name = sys.argv[1], sys.argv[2], sys.argv[3]
    rec = int(sys.argv[4]) if len(sys.argv) > 4 else -1

    names, sizes, _ = dims_of(ref, name)
    if not names:
        sys.exit(f"{name} not found in {ref}")
    a, b = read(ref, name), read(test, name)
    n = min(len(a), len(b))

    # The slab we look at is (eta, xi): drop ocean_time, and s_rho if any
    # by taking the level with the largest difference.
    nrec = sizes[0]
    per = n // nrec
    r = nrec - 1 if rec < 0 else rec
    sl = slice(r * per, (r + 1) * per)
    d = [abs(a[k] - b[k]) for k in range(sl.start, sl.stop)]

    # Infer the eta/xi extents from the header sizes after ocean_time.
    rest = [s for s in sizes[1:]]
    if len(rest) == 2:
        eta, xi = rest
        lev = 1
    elif len(rest) == 3:
        lev, eta, xi = rest
    else:
        sys.exit(f"unexpected dims {names} {sizes}")

    print(f"{name} record {r}/{nrec-1}  shape=({lev},{eta},{xi})")
    best = (0.0, 0, 0, 0)
    for k in range(per):
        if d[k] > best[0]:
            idx = k
            kk = idx // (eta * xi)
            rem = idx % (eta * xi)
            jj, ii = rem // xi, rem % xi
            best = (d[k], kk, jj, ii)
    mx, kk, jj, ii = best
    print(f"  worst: |d|={mx:.6e} at k={kk} j={jj} i={ii}")
    idx = kk * eta * xi + jj * xi + ii
    print(f"         ref={a[sl.start+idx]:.17e}")
    print(f"         test={b[sl.start+idx]:.17e}")

    # Per-edge worst, to separate halo problems from interior ones.
    def edge_worst(pred, label):
        w = (0.0, None)
        for k in range(per):
            kk_ = k // (eta * xi)
            if kk_ != kk:
                continue
            rem = k % (eta * xi)
            j2, i2 = rem // xi, rem % xi
            if not pred(j2, i2):
                continue
            if d[k] > w[0]:
                w = (d[k], (j2, i2))
        print(f"  {label:10s} |d|={w[0]:.6e} at {w[1]}")

    edge_worst(lambda j2, i2: j2 == 0, "south")
    edge_worst(lambda j2, i2: j2 == eta - 1, "north")
    edge_worst(lambda j2, i2: i2 == 0, "west")
    edge_worst(lambda j2, i2: i2 == xi - 1, "east")
    edge_worst(lambda j2, i2: 0 < j2 < eta - 1 and 0 < i2 < xi - 1,
               "interior")


if __name__ == "__main__":
    main()
