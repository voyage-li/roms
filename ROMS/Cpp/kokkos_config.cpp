/*
** git $Id$
************************************************************************
** Copyright (c) 2002-2025 The ROMS Group                               **
**   Licensed under a MIT/X style license                               **
**   See License_ROMS.md                                                **
************************************************************************
*/

#include "kokkos_config.hpp"

#if defined ROMS_KOKKOS

#include <mutex>

namespace roms {

namespace {
std::once_flag init_flag;
std::once_flag fini_flag;
}  // namespace

/*
** ROMS calls main3d inside an OpenMP parallel region, so several threads
** can reach this at the same time.  std::call_once makes the first one
** do the work and the rest wait.
*/

void initialize() {
  std::call_once(init_flag, []() {
    if (!Kokkos::is_initialized()) {
      Kokkos::initialize();
    }
  });
}

void finalize() {
  std::call_once(fini_flag, []() {
    if (Kokkos::is_initialized()) {
      Kokkos::finalize();
    }
  });
}

bool is_initialized() { return Kokkos::is_initialized(); }

}  // namespace roms

/*
** C entry points for the Fortran driver (ROMS/Cpp/kokkos_step2d_mod.F).
*/

extern "C" {

void roms_kokkos_initialize() { roms::initialize(); }
void roms_kokkos_finalize()   { roms::finalize(); }

}  // extern "C"

#endif  // ROMS_KOKKOS
