#!/usr/bin/env bash
# Build one ROMS application three ways with an identical physics option set:
# pure Fortran (reference), Kokkos Serial and Kokkos SW64.  Used to validate
# the kernels on applications other than upwelling.
#
#   build_app.sh APP [EXTRA_CPP_FLAGS] [HEADER_DIR]
#
# APP is the uppercase header stem (BL_TEST -> <HEADER_DIR>/bl_test.h).
# HEADER_DIR defaults to ROMS/Include; point it at a checkout of the
# official myroms/roms_test case directory to use those headers instead.
# The extra flags cover what the existing kernels require but the stock
# application header does not define; the same set goes to all three builds
# so the comparison is like for like.
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../../.." && pwd)
APP=${1:?application name, uppercase header stem}
EXTRA=${2:-}
HEADER_DIR=${3:-$ROOT/ROMS/Include}
HEADER_DIR=$(cd "$HEADER_DIR" && pwd)
cd "$ROOT"
if ! type module >/dev/null 2>&1; then source /etc/profile.d/modules.sh; fi
module load swgcc/1473 mpi/swuc
export NF_CONFIG=${NF_CONFIG:-$HOME/ftao/local-saca/bin/nf-config}
export NETCDF=${NETCDF:-$HOME/ftao/local-saca}
export NETCDF_INCDIR=${NETCDF_INCDIR:-$HOME/ftao/local-saca/include}
BASE="OUT_DOUBLE;VAR_RHO_2D;DIAGNOSTICS_UV;DIAGNOSTICS_TS"
[[ -n "$EXTRA" ]] && BASE="$BASE;$EXTRA"
STAGES=";KK_STAGE_FUSED;KK_STAGE_TAVG;KK_STAGE_COUP;KK_STAGE_MOMT;KK_STAGE_STOR"
mkdir -p logs build
for backend in reference serial sw64; do
  KOKKOS=OFF
  TRACER=OFF
  FLAGS="$BASE"
  if [[ "$backend" != reference ]]; then
    KOKKOS=ON
    TRACER=ON
    FLAGS="$BASE$STAGES"
  fi
  tag="${APP}-${backend}"
  echo "=== configuring $tag"
  cmake -S "$ROOT" -B "$ROOT/build/$tag" \
    -DCMAKE_TOOLCHAIN_FILE="$ROOT/Compilers/toolchain_SW64.cmake" \
    -DCMAKE_Fortran_COMPILER="$(command -v mpif90)" \
    -DCMAKE_CXX_COMPILER="$(command -v swucxx)" \
    -DROMS_KOKKOS_STEP3D_T="$TRACER" -DROMS_TRACER_TIMING=ON \
    -DCMAKE_BUILD_TYPE=Release -DROMS_FAST_MATH=OFF -DROMS_KOKKOS_TESTS=OFF \
    -DROMS_APP="$APP" -DMY_HEADER_DIR="$HEADER_DIR" \
    -DROMS_KOKKOS="$KOKKOS" -DROMS_KOKKOS_BACKEND="$backend" \
    -DMPI=OFF -DLIBTYPE=STATIC -DMY_CPP_FLAGS="$FLAGS" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON > "logs/configure-$tag.log" 2>&1
  echo "=== building $tag"
  cmake --build "$ROOT/build/$tag" --parallel "${BUILD_JOBS:-6}" \
    > "logs/build-$tag.log" 2>&1 || { tail -30 "logs/build-$tag.log"; exit 1; }
  echo "=== built $tag"
done
