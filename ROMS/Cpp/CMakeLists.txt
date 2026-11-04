# git $Id$
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2025 The ROMS Group                                :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.md                                                 :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Source code list for sub-directory "ROMS/Cpp"
#
# These files are only built when ROMS_KOKKOS is ON; the top-level
# CMakeLists.txt guards the add_subdirectory() call accordingly.

list( APPEND _files
      ROMS/Cpp/kokkos_step2d_mod.F
      ROMS/Cpp/kokkos_loops_mod.F
      ROMS/Cpp/kokkos_config.cpp
      ROMS/Cpp/kokkos_step2d.cpp
      ROMS/Cpp/kokkos_step3d_t_mod.F
      ROMS/Cpp/kokkos_step3d_t.cpp
      ROMS/Cpp/kokkos_rhs3d_mod.F
      ROMS/Cpp/kokkos_rhs3d.cpp
      ROMS/Cpp/kokkos_step3d_mod.F
      ROMS/Cpp/kokkos_step3d.cpp
    )

set ( ROMS_Cpp_files
      ${_files}
      PARENT_SCOPE
    )

# SW64 kernel flags are attached to the parent Objects target, which
# consumes these sources.  Source properties set here are directory-local.
