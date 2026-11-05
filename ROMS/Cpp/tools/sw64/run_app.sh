#!/usr/bin/env bash
# Submit one run of a ROMS application built by build_app.sh and check its log.
#
#   run_app.sh BUILD_TAG RUN_DIR
#
# BUILD_TAG is the directory under build/ (e.g. KELVIN-sw64-r2); RUN_DIR holds
# exactly one roms_*.in made by make_app_case.py.  The run waits for the job
# (bsub -I), writes RUN_DIR/run.log and refuses to replace an existing one.
#
# Environment, all optional:
#   RANKS           MPI ranks, one core group each (1).  More than one needs an
#                   MPI build (romsM) and an input whose NtileI*NtileJ equals
#                   RANKS (make_app_case.py --tiles).
#   RANKS_PER_NODE  ranks placed on each node, at most 6 (6)
#   QUEUE           bsub queue (q_share)
#   SHARE_SIZE      shared memory per rank in MB (15000)
#   TIMELIMIT       job time limit (00:10:00)
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../../.." && pwd)
TAG=${1:?build tag under build/}
RUN=$(cd "${2:?run directory}" && pwd)
RANKS=${RANKS:-1}
PER_NODE=${RANKS_PER_NODE:-6}
(( RANKS >= 1 && PER_NODE >= 1 && PER_NODE <= 6 )) || {
  printf 'Need RANKS >= 1 and 1 <= RANKS_PER_NODE <= 6\n' >&2; exit 2; }
EXE=$ROOT/build/$TAG/romsM
[[ -x "$EXE" ]] || EXE=$ROOT/build/$TAG/romsS
shopt -s nullglob
inputs=("$RUN"/roms_*.in)
[[ -x "$EXE" && ${#inputs[@]} -eq 1 ]] || {
  printf 'Missing executable or not exactly one roms_*.in in %s\n' "$RUN" >&2
  exit 2; }
if (( RANKS > 1 )) && [[ $EXE != */romsM ]]; then
  printf 'RANKS=%s needs an MPI build; %s has only romsS\n' "$RANKS" "$TAG" >&2
  exit 2
fi
IN=${inputs[0]}
[[ ! -e "$RUN/run.log" ]] || { printf 'Run already has a log: %s\n' "$RUN" >&2; exit 2; }
cd "$RUN"
export OMP_NUM_THREADS=1
# bsub -n is the total number of ranks, -np the number per node.
ARGS=(-I -b -q "${QUEUE:-q_share}" -n "$RANKS" -np "$(( RANKS < PER_NODE ? RANKS : PER_NODE ))"
      -cgsp 64 -share_size "${SHARE_SIZE:-15000}" -cross_size 1024 -host_stack 1024
      -cache_size 128 -timelimit "${TIMELIMIT:-00:10:00}" -J "roms-$TAG"
      -i "$IN")
/usr/bin/time -p bsub "${ARGS[@]}" "$EXE" "$IN" > run.log 2>&1
if ! grep -q 'ROMS: DONE' run.log || grep -qE 'ERROR STOP|Abnormal termination|BLOWUP' run.log; then
  tail -40 run.log >&2
  exit 1
fi
grep -E 'Job <|ROMS time stepping wall|accelerated|fallback|DONE' run.log
