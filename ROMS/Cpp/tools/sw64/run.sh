#!/usr/bin/env bash
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../../.." && pwd)
BACKEND=${1:?backend: reference, serial or sw64}
CASE=${2:?case directory relative to runs/}
RANKS=${RANKS:-1}
TAG=$BACKEND
BIN=romsS
if (( RANKS > 1 )); then TAG=${TAG}-mpi; BIN=romsM; fi
EXE=$ROOT/build/$TAG/$BIN
RUN=$ROOT/runs/$CASE/$BACKEND
[[ -x "$EXE" && -f "$RUN/roms_upwelling.in" ]] || { printf 'Missing executable or case\n' >&2; exit 2; }
[[ ! -e "$RUN/run.log" ]] || { printf 'Run already has a log: %s\n' "$RUN" >&2; exit 2; }
cd "$RUN"
export OMP_NUM_THREADS=1
# One MPE/core group per rank, with 64 CPEs reserved; Serial never launches them.
ARGS=(-I -b -q "${QUEUE:-q_share}" -n "$RANKS" -np "$RANKS" -cgsp 64
      -share_size "${SHARE_SIZE:-15000}" -cross_size 1024 -host_stack 1024
      -cache_size 128 -timelimit "${TIMELIMIT:-00:10:00}" -J "roms-$TAG"
      -i "$RUN/roms_upwelling.in")
/usr/bin/time -p bsub "${ARGS[@]}" "$EXE" "$RUN/roms_upwelling.in" > run.log 2>&1
if ! grep -q 'ROMS: DONE' run.log || grep -qE 'ERROR STOP|Abnormal termination|BLOWUP' run.log; then
  tail -40 run.log >&2
  exit 1
fi
# Outer time includes queueing. Use SYSTEM_CLOCK stepping timing for speedups;
# ROMS' legacy non-OpenMP timing uses cpu_time, which excludes some CPE waits.
grep -E 'ROMS Kokkos|ROMS initialization wall|ROMS time stepping wall|RMS_|^ *[0-9]+ 0001-|Total:|Average:|Maximum:|DONE|^real ' run.log
