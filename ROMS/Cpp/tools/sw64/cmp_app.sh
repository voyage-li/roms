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
ref_files=('') test_files=('')
n=0
for f in "$ref"/roms_*.nc; do
  [[ -e $f ]] || continue
  b=$(basename "$f")
  [[ $b == roms_rst.nc ]] && continue
  ref_files+=("$b")
  n=$((n+1))
done
for f in "$test"/roms_*.nc; do
  [[ -e $f ]] || continue
  b=$(basename "$f")
  [[ $b == roms_rst.nc ]] && continue
  test_files+=("$b")
done

contains_name() {
  local sought=$1 name
  shift
  for name in "$@"; do
    [[ $name == "$sought" ]] && return 0
  done
  return 1
}
status=0
for b in "${ref_files[@]}"; do
  [[ -n $b ]] || continue
  contains_name "$b" "${test_files[@]}" || {
    echo "missing test output: $b" >&2
    status=1
  }
done
for b in "${test_files[@]}"; do
  [[ -n $b ]] || continue
  contains_name "$b" "${ref_files[@]}" || {
    echo "extra test output: $b" >&2
    status=1
  }
done
(( status == 0 )) || exit "$status"
(( n > 0 )) || { echo "no roms_*.nc in $ref" >&2; exit 1; }

for b in "${ref_files[@]}"; do
  [[ -n $b ]] || continue
  echo "== $b"
  python3 "$ROOT/ROMS/Cpp/tools/nccmp.py" "$ref/$b" "$test/$b" --all-time "$@" || status=1
done
exit $status
