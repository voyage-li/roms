#!/usr/bin/env bash
# Resume the Sunway remainder matrix automatically after an SSH outage.
# No dependency checkout/install is touched; only the listed project files
# are deployed. Queue jobs are submitted by remainder_matrix.py, which
# records their exact IDs and never cancels jobs.
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../../.." && pwd)
HOST=${SW_HOST:-SW}
WAIT_SECONDS=${WAIT_SECONDS:-1800}
WATCH_SECONDS=${WATCH_SECONDS:-14400}
SSH=(ssh -o BatchMode=yes -o ConnectTimeout=10 -o ConnectionAttempts=1
         -o ServerAliveInterval=30 -o ServerAliveCountMax=4)
start=$SECONDS
while ! "${SSH[@]}" "$HOST" 'test -d "$HOME/voyage/roms/ROMS/Cpp"'; do
  if (( SECONDS-start >= WAIT_SECONDS )); then
    echo "Sunway SSH unavailable; no new validation or performance result was produced." >&2
    exit 1
  fi
  sleep 45
done
python3 -m unittest discover -s "$ROOT/ROMS/Cpp/tools" -p 'test_*.py' -v
FILES=(Compilers/roms_functions.cmake ROMS/Cpp/CMakeLists.txt
       ROMS/Cpp/kokkos_loops.hpp ROMS/Cpp/kokkos_loops_mod.F
       ROMS/Cpp/tools/row_codegen.py ROMS/Cpp/tools/test_row_codegen.py
       ROMS/Cpp/tools/test_remainder_matrix.py
       ROMS/Cpp/tools/sw64/make_app_case.py
       ROMS/Cpp/tools/sw64/remainder_matrix.py
       ROMS/Cpp/tools/sw64/deploy_remainder.sh
       ROMS/Utility/mp_routines.F ROMS/Modules/mod_scalars.F)
COPYFILE_DISABLE=1 tar --no-xattrs -C "$ROOT" -cf - "${FILES[@]}" |
  "${SSH[@]}" "$HOST" 'cd "$HOME/voyage/roms" && tar -xf -'
name="remainder-$(date -u +%Y%m%dT%H%M%S)-$$"
launch=$("${SSH[@]}" "$HOST" "set -e
  cd \"\$HOME/voyage/roms\"
  module load swgcc/1473 mpi/swuc
  mkdir -p logs
  nohup env RUN_ROOT=runs/$name python3 ROMS/Cpp/tools/sw64/remainder_matrix.py --apps canyon bl_test kelvin upwelling --mpi \
    >logs/$name.log 2>&1 </dev/null &
  echo REMOTE_PID=\$!")
pid=$(printf '%s\n' "$launch" | sed -n 's/^REMOTE_PID=//p')
[[ "$pid" =~ ^[0-9]+$ ]] || { printf '%s\n' "$launch" >&2; exit 2; }
echo "Sunway matrix PID=$pid; artifacts=~/voyage/roms/runs/$name; log=~/voyage/roms/logs/$name.log"
start=$SECONDS
last=''
while (( SECONDS-start < WATCH_SECONDS )); do
  state=$("${SSH[@]}" "$HOST" "cd \"\$HOME/voyage/roms\" &&
    if test -f runs/$name/STATUS.json; then
      alive=0; if kill -0 $pid 2>/dev/null; then alive=1; fi
      python3 -c 'import json,sys; s=json.load(open(\"runs/$name/STATUS.json\")); state=s[\"state\"]; print((\"exited-with-running-status\" if not int(sys.argv[1]) and state==\"running\" else state)+\"|\"+s[\"stage\"]); print(s.get(\"error\",\"\"))' \"\$alive\"
    elif kill -0 $pid 2>/dev/null; then
      echo starting
    else
      echo exited-without-status
    fi" 2>&1) || state="connection-unavailable"
  if [[ "$state" != "$last" ]]; then printf '%s\n' "$state"; last=$state; fi
  case "$state" in
    complete*'|'*)
      "${SSH[@]}" "$HOST" "cat \"\$HOME/voyage/roms/runs/$name/STATUS.json\""
      exit 0 ;;
    failed*'|'*|exited-with-running-status*|exited-without-status)
      "${SSH[@]}" "$HOST" "tail -100 \"\$HOME/voyage/roms/logs/$name.log\"" || true
      exit 1 ;;
  esac
  sleep 45
done
echo "Monitoring deadline reached; remote state is not assumed successful. Inspect ~/voyage/roms/runs/$name/STATUS.json." >&2
exit 1
