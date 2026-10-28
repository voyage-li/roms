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
      ROMS/Cpp/kokkos_config.cpp
      ROMS/Cpp/kokkos_step2d.cpp
    )

set ( ROMS_Cpp_files
      ${_files}
      PARENT_SCOPE
    )

# Kokkos only applies "-mhybrid-coding;-fpermissive" to its own SW64
# sources (core/src/CMakeLists.txt) and does not export it, so every
# translation unit that instantiates a kernel needs it too.  Without
# __sw_hybrid_coding__ the SW64 backend is compiled out and KOKKOS_LAMBDA
# degrades to a plain lambda -- a build that succeeds and computes
# nothing.  kokkos_config.hpp has an #error guard for this; the flags
# below are what keeps that guard satisfied.

if( ROMS_KOKKOS AND ROMS_KOKKOS_BACKEND STREQUAL "sw64" )
  set_source_files_properties(
    ${CMAKE_CURRENT_SOURCE_DIR}/kokkos_config.cpp
    ${CMAKE_CURRENT_SOURCE_DIR}/kokkos_step2d.cpp
    PROPERTIES COMPILE_OPTIONS "-mhybrid-coding;-fpermissive"
  )
  message( STATUS "ROMS/Cpp: -mhybrid-coding -fpermissive applied to "
                  "the Kokkos kernel sources" )
endif()
