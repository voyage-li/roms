#!/usr/bin/env bash
# Second validation case for the Kokkos kernels: ROMS' canyon application.
#
# canyon.h expects SOLVE3D from the build line, and the step2d kernel
# requires VAR_RHO_2D and DIAGNOSTICS_UV.  The tracer kernel is left OFF
# because canyon does not define SPLINES_VDIFF/DIAGNOSTICS_TS, so step3d_t
# stays Fortran there and its timer gives an independent baseline.
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../../.." && pwd)
cd "$ROOT"
if ! type module >/dev/null 2>&1; then source /etc/profile.d/modules.sh; fi
module load swgcc/1473 mpi/swuc
export NF_CONFIG=${NF_CONFIG:-$HOME/ftao/local-saca/bin/nf-config}
export NETCDF=${NETCDF:-$HOME/ftao/local-saca}
export NETCDF_INCDIR=${NETCDF_INCDIR:-$HOME/ftao/local-saca/include}
FLAGS="OUT_DOUBLE;SOLVE3D;VAR_RHO_2D;DIAGNOSTICS_UV"
FLAGS+=";KK_STAGE_FUSED;KK_STAGE_TAVG;KK_STAGE_COUP;KK_STAGE_MOMT;KK_STAGE_STOR"
for tag in serial sw64; do
  echo "=== configuring canyon-$tag"
  cmake -S "$ROOT" -B "$ROOT/build/canyon-$tag" \
    -DCMAKE_TOOLCHAIN_FILE="$ROOT/Compilers/toolchain_SW64.cmake" \
    -DCMAKE_Fortran_COMPILER="$(command -v mpif90)" \
    -DCMAKE_CXX_COMPILER="$(command -v swucxx)" \
    -DROMS_KOKKOS_STEP3D_T=OFF -DROMS_TRACER_TIMING=ON \
    -DCMAKE_BUILD_TYPE=Release -DROMS_FAST_MATH=OFF -DROMS_KOKKOS_TESTS=OFF \
    -DROMS_APP=CANYON -DMY_HEADER_DIR="$ROOT/ROMS/Include" \
    -DROMS_KOKKOS=ON -DROMS_KOKKOS_BACKEND="$tag" -DMPI=OFF -DLIBTYPE=STATIC \
    -DMY_CPP_FLAGS="$FLAGS" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
    > "logs/configure-canyon-$tag.log" 2>&1
  echo "=== building canyon-$tag"
  cmake --build "$ROOT/build/canyon-$tag" --parallel "${BUILD_JOBS:-6}" \
    > "logs/build-canyon-$tag.log" 2>&1 || { tail -30 "logs/build-canyon-$tag.log"; exit 1; }
  echo "=== built canyon-$tag"
done
