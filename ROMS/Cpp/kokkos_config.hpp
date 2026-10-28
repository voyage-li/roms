/*
** git $Id$
************************************************************************
** Copyright (c) 2002-2025 The ROMS Group                               **
**   Licensed under a MIT/X style license                               **
**   See License_ROMS.md                                                **
************************************************************************
**
** Kokkos kernel support for ROMS.
**
** This header provides the minimal glue needed to call Kokkos compute
** kernels from Fortran without copying arrays across the language
** boundary.  Fortran owns all memory; C++ only borrows raw pointers
** obtained from C_LOC.
**
** Design notes:
**
**   * Layout.  ROMS arrays are declared (LBi:UBi, LBj:UBj, ...).  The
**     first index varies fastest (column-major), so the linear offset
**     is  (i-lb_i) + (j-lb_j)*ni + (k-lb_k)*ni*nj.
**
**   * Execution space.  The kernels run on Kokkos::Serial.  ROMS is
**     already threaded over tiles (see ROMS/Drivers/nl_roms.h, the
**     "!$OMP PARALLEL" around main3d with per-thread first_tile /
**     last_tile), so a threaded Kokkos backend would either nest
**     inside that region or oversubscribe.  The Serial backend's
**     parallel_for expands to a plain loop (see
**     core/src/Serial/Kokkos_Serial_Parallel_Range.hpp) and holds no
**     shared mutable state, so it is safe to call concurrently from
**     several OpenMP threads.
**
**   * No parallel_reduce.  Kokkos::Serial::ParallelReduce touches the
**     singleton's pool_reduce_local() scratch, which is not safe under
**     concurrent entry.  step2d has no global reductions in its hot
**     path, so every kernel here is a pure parallel_for.
*/

#ifndef ROMS_KOKKOS_CONFIG_HPP
#define ROMS_KOKKOS_CONFIG_HPP

#if defined ROMS_KOKKOS

/*
** The CPP option set.  This must be the same set the Fortran side is
** compiled with, and it is: "cppdefs.h" is the very header ROMS feeds
** its own preprocessor, and ROMS_HEADER / ROOT_DIR / ANALYTICAL_DIR
** already reach the C++ compile line through add_compile_definitions().
**
** Skipping this include is not a cosmetic mistake.  The Fortran caller
** hands a stage over to Kokkos by compiling the equivalent Fortran loops
** out, so a macro that only the Fortran side sees leaves the kernel
** computing a different (or empty) expression while the timestep still
** looks plausible -- the upwelling test ran to completion with KE
** bit-identical to the reference and only failed at step 72, when
** DIAGNOSTICS_UV asked for a field nothing had written.
*/

#include "cppdefs.h"

/*
** SW64 carries its own trap, and it has to be checked before Kokkos is
** pulled in so that a misconfigured build fails with this message rather
** than something incidental further down.
**
** Kokkos guards the entire SW64 backend with
**
**   #if defined(KOKKOS_ENABLE_SW64) && defined(__sw_hybrid_coding__)
**
** so a translation unit compiled without -mhybrid-coding gets an empty
** backend.  It still compiles: Kokkos_Setup_SW64.hpp falls back to an
** ordinary "KOKKOS_LAMBDA [&]" when __sw_hybrid_coding__ is absent, so
** the failure is silent rather than a build error.  Kokkos' own CMake
** applies -mhybrid-coding only to its two internal sources and does not
** propagate it, so the kernels must assert it here.
*/

#if defined(ROMS_KOKKOS_SW64) && !defined(__sw_hybrid_coding__)
#  error "ROMS_KOKKOS_SW64 needs -mhybrid-coding on every kernel file; " \
         "__sw_hybrid_coding__ is not defined, which would silently "  \
         "compile the SW64 backend out."
#endif

#include <Kokkos_Core.hpp>

#include <cstddef>

/*
** Backend selection.  Exactly one must be defined; the build system sets
** it from ROMS_KOKKOS_BACKEND.
**
**   ROMS_KOKKOS_SERIAL  -- ROMS threads over tiles; kernels are plain
**                          loops and may be entered concurrently.
**   ROMS_KOKKOS_OPENMP  -- Kokkos threads inside each kernel; ROMS must
**                          run with OMP_NUM_THREADS=1 to avoid nesting.
**   ROMS_KOKKOS_SW64    -- Sunway manycore.  The SW64 execution space
**                          has memory_space == HostSpace, i.e. the
**                          "device" sees the same array the Fortran
**                          caller owns, so the borrowed-pointer scheme
**                          below works unchanged; there is no explicit
**                          staging copy to write.
*/

#if !defined(ROMS_KOKKOS_SERIAL) && !defined(ROMS_KOKKOS_OPENMP) && \
    !defined(ROMS_KOKKOS_SW64)
#  define ROMS_KOKKOS_SERIAL
#endif

#if (defined(ROMS_KOKKOS_SERIAL) + defined(ROMS_KOKKOS_OPENMP) + \
     defined(ROMS_KOKKOS_SW64)) > 1
#  error "Define only one of ROMS_KOKKOS_SERIAL / _OPENMP / _SW64"
#endif

/*
** SW64 carries its own trap.  Kokkos guards the entire SW64 backend with
**
**   #if defined(KOKKOS_ENABLE_SW64) && defined(__sw_hybrid_coding__)
**
** so a translation unit compiled without -mhybrid-coding gets an empty
** backend.  It still compiles: Kokkos_Setup_SW64.hpp falls back to an
** ordinary "KOKKOS_LAMBDA [&]" when __sw_hybrid_coding__ is absent, so
** the failure is silent rather than a build error.  Kokkos' own CMake
** applies -mhybrid-coding only to its two internal sources and does not
** propagate it, so the kernels must assert it here.  (The check itself
** lives above the Kokkos include, next to cppdefs.h.)
*/

namespace roms {

/*
** Borrowed Fortran array, indexed with the original Fortran bounds.
** operator() reproduces Fortran's column-major linearization so that
** kernel bodies can be written to match the Fortran source line for
** line.
*/

template <class T>
struct Array1d {
  T*  p   = nullptr;
  int lb  = 0;
  int ni  = 0;

  KOKKOS_INLINE_FUNCTION
  T& operator()(int i) const {
    return p[(i - lb)];
  }
};

template <class T>
struct Array2d {
  T*  p   = nullptr;
  int lb_i = 0, lb_j = 0;
  int ni   = 0, nj   = 0;

  KOKKOS_INLINE_FUNCTION
  T& operator()(int i, int j) const {
    return p[(i - lb_i) + (j - lb_j) * ni];
  }

  KOKKOS_INLINE_FUNCTION
  int size_i() const { return ni; }
  KOKKOS_INLINE_FUNCTION
  int size_j() const { return nj; }
};

template <class T>
struct Array3d {
  T*  p   = nullptr;
  int lb_i = 0, lb_j = 0, lb_k = 0;
  int ni   = 0, nj   = 0, nk   = 0;

  KOKKOS_INLINE_FUNCTION
  T& operator()(int i, int j, int k) const {
    return p[(i - lb_i) + (j - lb_j) * ni + (k - lb_k) * ni * nj];
  }
};

template <class T>
struct Array4d {
  T*  p   = nullptr;
  int lb_i = 0, lb_j = 0, lb_k = 0, lb_l = 0;
  int ni   = 0, nj   = 0, nk   = 0, nl   = 0;

  KOKKOS_INLINE_FUNCTION
  T& operator()(int i, int j, int k, int l) const {
    return p[(i - lb_i) + (j - lb_j) * ni + (k - lb_k) * ni * nj +
             (l - lb_l) * ni * nj * nk];
  }
};

/*
** Tile index metadata.  These are the subset of the 42 variables set by
** ROMS/Include/set_bounds.h that step2d_LF_AM3.h actually reads.  They
** are a pure function of (ng, tile) and are computed once per call on
** the Fortran side, then handed to every kernel as a single POD.
**
** Do not try to derive these from Istr: IstrU, JstrV and IstrR depend
** on the boundary-condition type of each edge and on periodic /
** composite / refined grid settings (see get_bounds.F around lines
** 1386-1412), so they must be taken as given.
*/

/*
** Domain edge flags.  Used by the advection stage, whose halo rows and
** columns are filled differently for physical, composite and periodic
** boundaries (see step2d_LF_AM3.h around lines 1343-1466).
*/

struct EdgeFlags {
  int west, east, south, north;
  int composite_w, composite_e, composite_s, composite_n;
  int periodic_ew, periodic_ns;
};

struct TileBounds {
  int Istr, Iend, Jstr, Jend;

  int IstrU, JstrV;
  int IstrR, JstrR, IendR, JendR;

  int Istrm1, Jstrm1;
  int Iendp1, Jendp1;
  int Iendp2, Jendp2;

  int IstrUm1, JstrVm1;
  int IstrUm2, JstrVm2;

  /* Trailing extents that the assumed-shape declarations do not carry. */

  int UBk;   /* vertical extent of ubar/vbar/zeta, i.e. N(ng) */
};

/*
** Pointer helpers.  Fortran passes TYPE(C_PTR); C++ receives void*.
*/

template <class T>
inline Array2d<T> as_2d(void* ptr, int lb_i, int ub_i, int lb_j, int ub_j) {
  Array2d<T> a;
  a.p    = static_cast<T*>(ptr);
  a.lb_i = lb_i;
  a.lb_j = lb_j;
  a.ni   = ub_i - lb_i + 1;
  a.nj   = ub_j - lb_j + 1;
  return a;
}

template <class T>
inline Array3d<T> as_3d(void* ptr, int lb_i, int ub_i, int lb_j, int ub_j,
                        int lb_k, int ub_k) {
  Array3d<T> a;
  a.p    = static_cast<T*>(ptr);
  a.lb_i = lb_i;
  a.lb_j = lb_j;
  a.lb_k = lb_k;
  a.ni   = ub_i - lb_i + 1;
  a.nj   = ub_j - lb_j + 1;
  a.nk   = ub_k - lb_k + 1;
  return a;
}

template <class T>
inline Array4d<T> as_4d(void* ptr, int lb_i, int ub_i, int lb_j, int ub_j,
                        int lb_k, int ub_k, int lb_l, int ub_l) {
  Array4d<T> a;
  a.p    = static_cast<T*>(ptr);
  a.lb_i = lb_i;
  a.lb_j = lb_j;
  a.lb_k = lb_k;
  a.lb_l = lb_l;
  a.ni   = ub_i - lb_i + 1;
  a.nj   = ub_j - lb_j + 1;
  a.nk   = ub_k - lb_k + 1;
  a.nl   = ub_l - lb_l + 1;
  return a;
}

/*
** The execution space the kernels run on.
*/

#if defined(ROMS_KOKKOS_OPENMP)
using exec_space = Kokkos::OpenMP;
#elif defined(ROMS_KOKKOS_SW64)
using exec_space = Kokkos::SW64;
#else
using exec_space = Kokkos::Serial;
#endif

/*
** Kokkos lifecycle.  initialize() is called once from ROMS after MPI is
** up and before the first kernel; it is guarded so that concurrent
** entry from several OpenMP threads is harmless.
**
** Scratch buffers are NOT owned here: the Fortran caller allocates them
** (it must be able to run halo exchanges on DUon/DVom/Drhs) and passes
** pointers in.  See the per-stage signatures in kokkos_step2d.hpp.
*/

void initialize();
void finalize();
bool is_initialized();

}  // namespace roms

#endif  // ROMS_KOKKOS

#endif  // ROMS_KOKKOS_CONFIG_HPP
