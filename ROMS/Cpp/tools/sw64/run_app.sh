#!/usr/bin/env bash
# Submit one run of a ROMS application built by build_app.sh.
#
#   run_app.sh BUILD_TAG RUN_DIR
#
# BUILD_TAG is the directory under build/ (e.g. KELVIN-sw64-r2); RUN_DIR
# holds exactly one roms_*.in made by make_app_case.py.  Same bsub
# settings as run.sh; refuses to replace an existing run.log.
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../../.." && pwd)
TAG=${1:?build tag under build/}
RUN=$(cd "${2:?run directory}" && pwd)
EXE=$ROOT/build/$TAG/romsS
shopt -s nullglob
inputs=("$RUN"/roms_*.in)
[[ -x "$EXE" && ${#inputs[@]} -eq 1 ]] || {
  printf 'Missing executable or not exactly one roms_*.in in %s\n' "$RUN" >&2
  exit 2; }
IN=${inputs[0]}
[[ ! -e "$RUN/run.log" ]] || { printf 'Run already has a log: %s\n' "$RUN" >&2; exit 2; }
cd "$RUN"
export OMP_NUM_THREADS=1
ARGS=(-I -b -q "${QUEUE:-q_share}" -n 1 -np 1 -cgsp 64
      -share_size "${SHARE_SIZE:-15000}" -cross_size 1024 -host_stack 1024
      -cache_size 128 -timelimit "${TIMELIMIT:-00:10:00}" -J "roms-$TAG"
      -i "$IN")
/usr/bin/time -p bsub "${ARGS[@]}" "$EXE" "$IN" > run.log 2>&1
if ! grep -q 'ROMS: DONE' run.log || grep -qE 'ERROR STOP|Abnormal termination|BLOWUP' run.log; then
  tail -40 run.log >&2
  exit 1
fi
grep -E 'Job <|ROMS time stepping wall|accelerated|fallback|DONE' run.log
