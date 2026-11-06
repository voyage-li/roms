#!/usr/bin/env bash
# Build one ROMS application with an identical physics option set as pure
# Fortran (reference), Kokkos Serial and Kokkos SW64.
#
#   build_app.sh APP [EXTRA_CPP_FLAGS] [HEADER_DIR]
#
# APP is the uppercase header stem (BL_TEST -> <HEADER_DIR>/bl_test.h).
# HEADER_DIR defaults to ROMS/Include of the checkout this script belongs to;
# point it at a checkout of the official myroms/roms_test case directory to use
# those headers instead (a relative path is looked up from the current
# directory, then from the checkout).
# EXTRA_CPP_FLAGS cover what the kernels need but the stock application
# header does not define; every build gets the same set, so comparisons are
# like for like.
#
# Environment, all optional:
#   BACKENDS    builds to make                                (reference serial sw64)
#   MPI         ON builds romsM, OFF builds romsS             (OFF)
#   TAG_SUFFIX  appended to the build directory build/<APP>-<backend>[-mpi],
#               e.g. -r2, so earlier binaries stay in place
#   KK_STAGES   replaces the stage list of the Kokkos builds.  The default is
#               the validated set (2-D and 3-D kernels, GLS, PRE3D, REMAINDER).
#               BL_TEST has to drop KK_STAGE_PRE3D: it defines SOLAR_SOURCE,
#               which the pre_step3d kernel rejects.
#   STEP3D_T    ON enables the tracer chunk kernel in Kokkos builds (ON)
#   TESTS       ON also builds the direct kernel tests (OFF)
#   BUILD_JOBS  parallel build jobs (6)
# ROMS_WALL_PROFILE is always defined, so the region timings use the wall
# clock, as in the benchmark builds.
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../../../.." && pwd)
APP=${1:?application name, uppercase header stem}
EXTRA=${2:-}
HEADER_DIR=${3:-$ROOT/ROMS/Include}
[[ -d $HEADER_DIR ]] || HEADER_DIR=$ROOT/$HEADER_DIR
HEADER_DIR=$(cd "$HEADER_DIR" 2>/dev/null && pwd) || {
  printf 'No such header directory: %s\n' "${3:-$ROOT/ROMS/Include}" >&2; exit 2; }
cd "$ROOT"
if ! type module >/dev/null 2>&1; then source /etc/profile.d/modules.sh; fi
module load swgcc/1473 mpi/swuc
export NF_CONFIG=${NF_CONFIG:-$HOME/ftao/local-saca/bin/nf-config}
export NETCDF=${NETCDF:-$HOME/ftao/local-saca}
export NETCDF_INCDIR=${NETCDF_INCDIR:-$HOME/ftao/local-saca/include}
# NLM_DRIVER selects the nonlinear trajectory in the headers that gate their
# physics on it (DOUBLE_GYRE, CHANNEL, WC13).  Nothing else in this tree
# defines it, so without it those headers skip their whole physics block and
# every build of them is a different model.
BASE="NLM_DRIVER;OUT_DOUBLE;VAR_RHO_2D;DIAGNOSTICS_UV;DIAGNOSTICS_TS"
[[ -n "$EXTRA" ]] && BASE="$BASE;$EXTRA"
[[ ";$BASE;" == *";ROMS_WALL_PROFILE;"* ]] || BASE="$BASE;ROMS_WALL_PROFILE"
DEFAULT_STAGES="KK_STAGE_FUSED;KK_STAGE_TAVG;KK_STAGE_COUP;KK_STAGE_MOMT;KK_STAGE_DIAG;KK_STAGE_STOR;"
DEFAULT_STAGES+="KK_STAGE_UV3DMIX;KK_STAGE_T3DMIX;KK_STAGE_PRSGRD;KK_STAGE_OMEGA;KK_STAGE_SETZETA;"
DEFAULT_STAGES+="KK_STAGE_STEP3DUV;KK_STAGE_GLS;KK_STAGE_PRE3D;KK_STAGE_REMAINDER"
STAGES=${KK_STAGES:-$DEFAULT_STAGES}
MPI=${MPI:-OFF}
SUFFIX=""
[[ "$MPI" == ON ]] && SUFFIX="-mpi"
mkdir -p logs build
for backend in ${BACKENDS:-reference serial sw64}; do
  KOKKOS=OFF
  TRACER=OFF
  FLAGS="$BASE"
  if [[ "$backend" != reference ]]; then
    KOKKOS=ON
    TRACER=${STEP3D_T:-ON}
    FLAGS="$BASE;$STAGES"
  fi
  tag="${APP}-${backend}${SUFFIX}${TAG_SUFFIX:-}"
  echo "=== configuring $tag"
  # The toolchain file selects mpif90 and swucxx.
  cmake -S "$ROOT" -B "$ROOT/build/$tag" \
    -DCMAKE_TOOLCHAIN_FILE="$ROOT/Compilers/toolchain_SW64.cmake" \
    -DROMS_KOKKOS_STEP3D_T="$TRACER" \
    -DCMAKE_BUILD_TYPE=Release -DROMS_FAST_MATH=OFF \
    -DROMS_KOKKOS_TESTS="${TESTS:-OFF}" \
    -DROMS_APP="$APP" -DMY_HEADER_DIR="$HEADER_DIR" \
    -DROMS_KOKKOS="$KOKKOS" -DROMS_KOKKOS_BACKEND="$backend" \
    -DMPI="$MPI" -DLIBTYPE=STATIC -DMY_CPP_FLAGS="$FLAGS" \
    -DCMAKE_EXPORT_COMPILE_COMMANDS=ON > "logs/configure-$tag.log" 2>&1
  echo "=== building $tag"
  cmake --build "$ROOT/build/$tag" --parallel "${BUILD_JOBS:-6}" \
    > "logs/build-$tag.log" 2>&1 || { tail -30 "logs/build-$tag.log"; exit 1; }
  echo "=== built $tag"
done
