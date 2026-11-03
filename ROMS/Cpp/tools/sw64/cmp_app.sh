#!/usr/bin/env bash
# Compare every time-dependent field of every roms_*.nc output of two runs
# of the same application (history, averages, diagnostics, stations).
#
#   cmp_app.sh REF_DIR TEST_DIR [nccmp options]
#
# The variable list is taken from each reference file, so this works for
# any application; a field present in only one of the two files fails.
set -uo pipefail
ROOT=$(cd "$(dirname "$0")/../../../.." && pwd)
X86=/usr/sw/yyzlib/local_x86
if [[ -d $X86/bin ]]; then
  export PATH=$X86/bin:$PATH
  export LD_LIBRARY_PATH=$X86/lib:${LD_LIBRARY_PATH:-}
fi
ref=${1:?reference run directory}
test=${2:?test run directory}
shift 2
status=0 n=0
for f in "$ref"/roms_*.nc; do
  [[ -e $f ]] || continue
  b=$(basename "$f")
  [[ $b == roms_rst.nc ]] && continue
  n=$((n+1))
  echo "== $b"
  python3 "$ROOT/ROMS/Cpp/tools/nccmp.py" "$f" "$test/$b" --all-time "$@" || status=1
done
(( n > 0 )) || { echo "no roms_*.nc in $ref" >&2; exit 1; }
exit $status
