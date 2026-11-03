#!/usr/bin/env bash
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../../.." && pwd)
BACKEND=${1:-serial}
MPI=${MPI:-OFF}
case "$BACKEND" in serial|sw64|reference) ;; *) printf 'Usage: %s {serial|sw64|reference}\n' "$0" >&2; exit 2;; esac
if ! type module >/dev/null 2>&1; then source /etc/profile.d/modules.sh; fi
module load swgcc/1473 mpi/swuc
export NF_CONFIG=${NF_CONFIG:-$HOME/ftao/local-saca/bin/nf-config}
export NETCDF=${NETCDF:-$HOME/ftao/local-saca}
export NETCDF_INCDIR=${NETCDF_INCDIR:-$HOME/ftao/local-saca/include}
# BUILD_TAG names the build directory (default: the backend), e.g. sw64-opt.
TAG=${BUILD_TAG:-$BACKEND}
if [[ "$MPI" == ON ]]; then TAG=${TAG}-mpi; fi
mkdir -p "$ROOT/build/$TAG" "$ROOT/logs"
FLAGS='OUT_DOUBLE'
KOKKOS=OFF
if [[ "$BACKEND" != reference ]]; then
  KOKKOS=ON
  # KK_STAGES overrides the stage list, e.g. to build a step2d-only binary
  # whose Serial output can be compared bit for bit with build/reference.
  FLAGS+=";${KK_STAGES:-KK_STAGE_FUSED;KK_STAGE_TAVG;KK_STAGE_COUP;KK_STAGE_MOMT;KK_STAGE_DIAG;KK_STAGE_STOR;KK_STAGE_UV3DMIX;KK_STAGE_T3DMIX;KK_STAGE_PRSGRD;KK_STAGE_OMEGA;KK_STAGE_SETZETA;KK_STAGE_PRE3D;KK_STAGE_STEP3DUV}"
fi
cmake -S "$ROOT" -B "$ROOT/build/$TAG" \
  -DCMAKE_TOOLCHAIN_FILE="$ROOT/Compilers/toolchain_SW64.cmake" \
  -DCMAKE_Fortran_COMPILER="$(command -v mpif90)" \
  -DCMAKE_CXX_COMPILER="$(command -v swucxx)" \
  -DROMS_KOKKOS_STEP3D_T="${STEP3D_T:-OFF}" -DROMS_TRACER_TIMING=ON \
  -DCMAKE_BUILD_TYPE=Release -DROMS_FAST_MATH=OFF -DROMS_KOKKOS_TESTS=ON \
  -DROMS_APP=UPWELLING -DMY_HEADER_DIR="$ROOT/ROMS/Include" \
  -DROMS_KOKKOS="$KOKKOS" -DROMS_KOKKOS_BACKEND="$BACKEND" \
  -DMPI="$MPI" -DLIBTYPE=STATIC \
  -DMY_CPP_FLAGS="$FLAGS" -DCMAKE_EXPORT_COMPILE_COMMANDS=ON \
  2>&1 | tee "$ROOT/logs/configure-$TAG.log"
cmake --build "$ROOT/build/$TAG" --parallel "${BUILD_JOBS:-6}" \
  2>&1 | tee "$ROOT/logs/build-$TAG.log"
