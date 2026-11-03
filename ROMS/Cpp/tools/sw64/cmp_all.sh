#!/usr/bin/env bash
# Compare every time-dependent field of two upwelling runs with output:
# history, averages and diagnostics, including the tracers and the 3-D
# momentum and tracer budget terms.
#
#   cmp_all.sh REF_DIR TEST_DIR [nccmp options]
#
# With no tolerance option the fields must be equal value for value.
# Example: cmp_all.sh runs/smoke/reference runs/x/val --rtol 1e-10 --zero-tol 1e-20
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

HIS=zeta,ubar,vbar,u,v,w,omega,temp,salt,rho
AVG=zeta,ubar,vbar,u,v,omega,w,temp,salt,rho,zeta2,ubar2,vbar2,Huon,Hvom
AVG+=,uu,uv,vv,temp_2,salt_2,Huon_temp,Hvom_temp,Huon_salt,Hvom_salt
AVG+=,u_temp,v_temp,u_salt,v_salt
DIA=zeta,ubar_cor,vbar_cor,ubar_hadv,vbar_hadv,ubar_xadv,vbar_xadv
DIA+=,ubar_yadv,vbar_yadv,ubar_hvisc,vbar_hvisc,ubar_xvisc,vbar_xvisc
DIA+=,ubar_yvisc,vbar_yvisc,ubar_prsgrd,vbar_prsgrd,ubar_sstr,vbar_sstr
DIA+=,ubar_bstr,vbar_bstr,ubar_accel,vbar_accel
DIA+=,u_cor,v_cor,u_vadv,v_vadv,u_hadv,v_hadv,u_xadv,v_xadv,u_yadv,v_yadv
DIA+=,u_prsgrd,v_prsgrd,u_vvisc,v_vvisc,u_hvisc,v_hvisc,u_xvisc,v_xvisc
DIA+=,u_yvisc,v_yvisc,u_accel,v_accel
for t in temp salt; do
  DIA+=,${t}_hadv,${t}_xadv,${t}_yadv,${t}_vadv,${t}_hdiff,${t}_xdiff
  DIA+=,${t}_ydiff,${t}_vdiff,${t}_rate
done

status=0
for f in his:$HIS avg:$AVG dia:$DIA; do
  kind=${f%%:*} vars=${f#*:}
  echo "== roms_$kind.nc"
  python3 "$ROOT/ROMS/Cpp/tools/nccmp.py" "$ref/roms_$kind.nc" \
    "$test/roms_$kind.nc" --vars "$vars" "$@" || status=1
done
exit $status
