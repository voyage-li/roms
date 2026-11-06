# git $Id$
#:::::::::::::::::::::::::::::::::::::::::::::::::::::: David Robertson :::
# Copyright (c) 2002-2025 The ROMS Group                                :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.md                                                 :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Top-Level CMake File Definitions.

cmake_minimum_required( VERSION 3.13.0 FATAL_ERROR )

list( APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/Compilers)
Message( STATUS "module path = ${CMAKE_MODULE_PATH}" )
set( CMAKE_DIRECTORY_LABELS ${PROJECT_NAME} )

find_package( Git )
if( GIT_FOUND )
  if( EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.git )
    message( STATUS "Getting and setting GIT_URL and GIT_REV" )
    execute_process(
      COMMAND ${GIT_EXECUTABLE} config --get remote.origin.url
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      OUTPUT_VARIABLE GITURL
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    execute_process(
      COMMAND ${GIT_EXECUTABLE} rev-parse --verify HEAD
      WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
      OUTPUT_VARIABLE GITREV
      OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    add_compile_definitions( GIT_URL="${GITURL}" GIT_REV="${GITREV}" SVN_URL="https://www.myroms.org/svn/src" )
  endif()
endif()

find_package( Subversion )
if( SUBVERSION_FOUND )
  if( EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/.svn )
    message( STATUS "Getting and setting SVN_URL and SVN_REV" )
    Subversion_WC_INFO(${CMAKE_CURRENT_SOURCE_DIR} roms IGNORE_SVN_FAILURE)
    add_compile_definitions( SVN_URL="${roms_WC_URL}" SVN_REV="${roms_WC_REVISION}" )
  endif()
endif()

# Figure out if we are using ecbuild.

if( DEFINED ENV{ECBUILD_MODULE_PATH} )
  set( IN_ECBUILD TRUE )
endif()

project(roms VERSION 4.1 LANGUAGES Fortran)

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Kokkos (optional)
#
# When ROMS_KOKKOS is ON the ROMS/Cpp kernels are built and linked, and
# the ROMS_KOKKOS define reaches both Fortran and C++ so the two sides
# agree on which path is active.  C++ is enabled only for these builds.
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

option( ROMS_KOKKOS "Build ROMS with Kokkos compute kernels" OFF )
option( ROMS_KOKKOS_STEP3D_T "Enable the U3/C4 and HSIMT tracer chunk kernel" OFF )
if( ROMS_KOKKOS AND ROMS_KOKKOS_STEP3D_T )
  add_compile_definitions( KK_STAGE_STEP3D_T )
endif()
set( ROMS_KOKKOS_BACKEND "serial" CACHE STRING
     "Kokkos execution space for the ROMS kernels: serial | openmp | sw64" )
set_property( CACHE ROMS_KOKKOS_BACKEND PROPERTY STRINGS serial openmp sw64 )

if( ROMS_KOKKOS )
  enable_language( CXX )
  # Backend.  "serial" keeps ROMS as the only source of parallelism (it
  # threads over tiles) and the kernels are plain loops; "openmp" lets
  # Kokkos thread inside each kernel, which requires running ROMS itself
  # with OMP_NUM_THREADS=1 so the two do not nest; "sw64" targets the
  # Sunway manycore backend of the Kokkos fork.
  string( TOLOWER "${ROMS_KOKKOS_BACKEND}" _kk_backend )
  if( _kk_backend STREQUAL "openmp" )
    set( Kokkos_ENABLE_SERIAL ON  CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_OPENMP ON  CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_SW64   OFF CACHE BOOL "" FORCE )
    add_compile_definitions( ROMS_KOKKOS_OPENMP )
  elseif( _kk_backend STREQUAL "sw64" )
    set( Kokkos_ENABLE_SERIAL ON  CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_OPENMP OFF CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_SW64   ON  CACHE BOOL "" FORCE )
    # The SW64 backend needs C++17: Compilers/toolchain_SW64.cmake sets
    # it and Kokkos' cmake/kokkos_enable_devices.cmake enforces it.
    add_compile_definitions( ROMS_KOKKOS_SW64 )
  elseif( _kk_backend STREQUAL "serial" )
    set( Kokkos_ENABLE_SERIAL ON  CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_OPENMP OFF CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_SW64   OFF CACHE BOOL "" FORCE )
    add_compile_definitions( ROMS_KOKKOS_SERIAL )
  else()
    message( FATAL_ERROR
             "ROMS_KOKKOS_BACKEND='${ROMS_KOKKOS_BACKEND}' is not one of "
             "serial, openmp, sw64" )
  endif()
  set( Kokkos_ENABLE_TESTS    OFF CACHE BOOL "" FORCE )
  set( Kokkos_ENABLE_EXAMPLES OFF CACHE BOOL "" FORCE )
  add_subdirectory( Kokkos )
  add_compile_definitions( ROMS_KOKKOS )
  if( SW64 )
    # Static try-compile cannot discover the cross C++ implicit runtime.
    # Append it after Kokkos archives: these toolchains link statically.
    # It applies to every Fortran-linked executable, tests included.
    string( APPEND CMAKE_Fortran_STANDARD_LIBRARIES " -lstdc++" )
    if( _kk_backend STREQUAL "sw64" )
      string( APPEND CMAKE_Fortran_STANDARD_LIBRARIES
        " -lstdc++_slave -lm_slave" )
    endif()
  endif()
  message( STATUS "ROMS will be built with Kokkos kernels (ROMS/Cpp), "
                  "backend = ${_kk_backend}" )
endif()

if( IN_ECBUILD )
  set( ECBUILD_DEFAULT_BUILD_TYPE Release )
  set( ENABLE_OS_TESTS           OFF CACHE BOOL "Disable OS tests" FORCE )
  set( ENABLE_LARGE_FILE_SUPPORT OFF CACHE BOOL "Disable testing of large file support" FORCE )
  set( ENABLE_MPI ON CACHE BOOL "Compile with MPI" )

  find_package(ecbuild 3.3.2 REQUIRED)
  include( ecbuild_system NO_POLICY_SCOPE )
  ecbuild_declare_project()

  ecbuild_enable_fortran( REQUIRED )
endif()

# Include ROMS cmake files.

include( roms_functions )
include( roms_compiler_flags )
include( roms_config )

# Set MYDEFS to list of Compiling definition to check for options that require
# cmake configuration adjustments

get_directory_property( MYDEFS COMPILE_DEFINITIONS )

# Check if we need to include ESMF

if( MYDEFS MATCHES "ESMF_LIB" )
  if( NOT ESMF_FOUND )
    find_package( ESMF REQUIRED )
    if( ESMF_FOUND )
      message( STATUS "ESMF_INCLUDE_DIRECTORIES = ${ESMF_INCLUDE_DIRECTORIES}" )
      message( STATUS "ESMF_LIBRARY_LOCATION = ${ESMF_LIBRARY_LOCATION}" )
      message( STATUS "ESMF_INTERFACE_LINK_LIBRARIES = ${ESMF_INTERFACE_LINK_LIBRARIES}" )
      include_directories( ${ESMF_INCLUDE_DIRECTORIES} )
    else()
      message( FATAL "ESMF NOT FOUND" )
    endif()
  endif()
endif()

###########################################################################
# Project
###########################################################################

set( ROMS_LINKER_LANGUAGE Fortran )

if( NOT IN_ECBUILD )

  # Set Fortran modules directory when not using ecbuild. It is set
  # automatically when using ecbuild.

  set( CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/module" )
  set( CMAKE_INSTALL_RPATH_USE_LINK_PATH TRUE )
  set( CMAKE_SKIP_BUILD_RPATH FALSE )

  # MacOS requires an extra parameter for rpath to work properly. This is
  # set automatically when using ecbuild.

  if( APPLE )
    set( CMAKE_MACOSX_RPATH ON )
    set( CMAKE_INSTALL_RPATH @loader_path/../lib )
  else()
    set( CMAKE_INSTALL_RPATH \$ORIGIN/../lib )
  endif()

endif()

###########################################################################
# Dependencies
###########################################################################

# NetCDF Library.

if(DEFINED ENV{SINGULARITY_COMMAND} AND IN_ECBUILD )
  find_package( NetCDF REQUIRED COMPONENTS Fortran )
  include_directories( ${NETCDF_INCLUDE_DIRS} )
  list( APPEND netcdf_libs "netcdff" "netcdf" )

else()

  # Find NetCDF outside singularity. "target_link_libraries" is found below
  # because it has to be defined after "add_executable".  The link_netcdf()
  # function can be found in Compilers/roms_functions.cmake.

  link_netcdf()
  include_directories( ${netcdf_idir} )
  link_directories( ${netcdf_ldirs} )
endif()

# SCORPIO Include directories

if( SCORPIO )
  include_directories( ${PIO_INCDIR} )
  message( STATUS "SCORPIO included: ${PIO_INCDIR}" )
endif()

###########################################################################
# ROMS
###########################################################################

add_subdirectory( "Master" )
add_subdirectory( "ROMS/Drivers" )
add_subdirectory( "ROMS/Functionals" )
add_subdirectory( "ROMS/Modules" )
add_subdirectory( "ROMS/Nonlinear" )
add_subdirectory( "ROMS/Nonlinear/BBL" )
add_subdirectory( "ROMS/Nonlinear/Biology" )
add_subdirectory( "ROMS/Nonlinear/Sediment" )
add_subdirectory( "ROMS/Utility" )
if( ROMS_KOKKOS )
  add_subdirectory( "ROMS/Cpp" )
endif()

include_directories(
  ${CMAKE_CURRENT_BINARY_DIR}
  "/usr/local/include"
  "$ENV{NETCDF}/include"
  "Master"
  "ROMS/Drivers"
  "ROMS/Functionals"
  "ROMS/Include"
  "ROMS/Modules"
  "ROMS/Nonlinear"
  "ROMS/Nonlinear/BBL"
  "ROMS/Nonlinear/Biology"
  "ROMS/Nonlinear/Sediment"
  "ROMS/Utility"
)

link_directories(
  ${CMAKE_CURRENT_BINARY_DIR}
  "/usr/local/lib"
  "$ENV{NETCDF}/lib"
)

list(APPEND srcs
     ${Master_files}
     ${ROMS_Drivers_files}
     ${ROMS_Functionals_files}
     ${ROMS_Modules_files}
     ${ROMS_Nonlinear_files}
     ${ROMS_Nonlinear_BBL_files}
     ${ROMS_Nonlinear_Biology_files}
     ${ROMS_Nonlinear_Sediment_files}
     ${ROMS_Utility_files}
)

if( ROMS_KOKKOS )
  list( APPEND srcs ${ROMS_Cpp_files} )
  include_directories( "ROMS/Cpp" )
endif()
if( ADJOINT )
  add_subdirectory( "ROMS/Adjoint" )
  add_subdirectory( "ROMS/Adjoint/Biology" )
  include_directories( "ROMS/Adjoint" "ROMS/Adjoint/Biology" )
  list( APPEND srcs
        ${ROMS_Adjoint_files}
        ${ROMS_Adjoint_Biology_files}
  )

  # Free format flags for certain files that may exceed 132 characters per line

  set_property(
    SOURCE ${CMAKE_CURRENT_BINARY_DIR}/f90/ad_biology.f90
    APPEND_STRING PROPERTY COMPILE_FLAGS ${ROMS_FREEFLAGS}
  )
endif()

if( REPRESENTER )
  add_subdirectory( "ROMS/Representer" )
  add_subdirectory( "ROMS/Representer/Biology" )
  include_directories( "ROMS/Representer" "ROMS/Representer/Biology" )
  list( APPEND srcs
        ${ROMS_Representer_files}
        ${ROMS_Representer_Biology_files}
  )

  # Free format flags for certain files that may exceed 132 characters per line

  set_property(
    SOURCE ${CMAKE_CURRENT_BINARY_DIR}/f90/rp_biology.f90
    APPEND_STRING PROPERTY COMPILE_FLAGS ${ROMS_FREEFLAGS}
  )
endif()

if( TANGENT )
  add_subdirectory( "ROMS/Tangent" )
  add_subdirectory( "ROMS/Tangent/Biology" )
  include_directories( "ROMS/Tangent" "ROMS/Tangent/Biology" )
  list( APPEND srcs
        ${ROMS_Tangent_files}
        ${ROMS_Tangent_Biology_files}
  )

  # Free format flags for certain files that may exceed 132 characters per line

  set_property(
    SOURCE ${CMAKE_CURRENT_BINARY_DIR}/f90/tl_biology.f90
    APPEND_STRING PROPERTY COMPILE_FLAGS ${ROMS_FREEFLAGS}
  )
endif()

#--------------------------------------------------------------------------
# ROMS specific rules.
#--------------------------------------------------------------------------

# Special CPP definitions for "mod_strings.F".

set_property(
  SOURCE ROMS/Modules/mod_strings.F
  APPEND_STRING PROPERTY COMPILE_DEFINITIONS
    MY_OS='${my_os}' MY_CPU='${my_cpu}' MY_FORT='${my_fort}'
    MY_FC='${my_fc}'
    MY_FFLAGS='${my_fflags}'
)

# Free format flags for certain files that may exceed 132 characters per line

set_property(
  SOURCE
    ${CMAKE_CURRENT_BINARY_DIR}/f90/mod_ncparam.f90
    ${CMAKE_CURRENT_BINARY_DIR}/f90/mod_strings.f90
    ${CMAKE_CURRENT_BINARY_DIR}/f90/analytical.f90
    ${CMAKE_CURRENT_BINARY_DIR}/f90/biology.f90
  APPEND_STRING PROPERTY COMPILE_FLAGS ${ROMS_FREEFLAGS}
)

###########################################################################
# Clean-up the dependencies
###########################################################################


# Remove the .F files from "DependInfo.cmake" for the Objects target. If
# the "Objects" target changes names this MUST be updated.

add_custom_command(
  OUTPUT fixdep
  COMMAND ${PERL} ${CMAKE_CURRENT_SOURCE_DIR}/ROMS/Bin/FixDependInfo.pl ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/Objects.dir/DependInfo.cmake
  VERBATIM
)

# Custom target needed to make the custom command above run

add_custom_target(fix ALL
  DEPENDS fixdep
)

# The "preprocess_fortran" function tells CMake how to create .f90 files.
# It allows CMake crippled Fortran depency tracker to look for the right
# modules using the pre-processed .f90 files instead of the .F files.
#
# The dependency generator is easily confused by CPP if-directives.
# This also allows us to feed .f90 files to CMake as the sources to all
# the ROMS code and eliminate compiler assumptions related to .F files.
#
# The function "preprocess_fortran" is defined in the "roms_functions.cmake"
# file in the "Compilers" directory.

preprocess_fortran( ${srcs} )
set( All_f90s "${f90srcs}" )

# Adding all the sources as an OBJECT library prevents CMake from trying
# to link an executable or collect the object files into a library.
#
# The libXXX.(a|so) libraries will be created below.

add_library( Objects OBJECT ${All_f90s} )

# Objects is an OBJECT library, so Kokkos' usage requirements (include
# directories in particular) do not reach the ROMS/Cpp translation units
# through the executable's link line.  Attach them explicitly.  Linking
# an OBJECT library to a normal one only propagates usage requirements;
# it does not add anything to Objects' own link step.

if( ROMS_KOKKOS )
  # Options for every translation unit that includes the kernels.
  # -mhybrid-coding is required by the SW64 backend (kokkos_config.hpp).
  # The alias-check limit only matters where GCC vectorises
  # (ROMS_FAST_MATH=ON): the fused 2-D RHS and coupling loops touch more
  # arrays than the default limit of 20 versioning checks.
  #
  # swgcc-1473 changes rounding in auto-vectorized CPE arithmetic even
  # without fast-math (an identical-input momentum replay differs by one
  # ULP; its scalar CPE form is exact). KPP amplifies such seeds through
  # density cancellation and convective mixing. Conservative math must
  # therefore also retain scalar evaluation within each CPE. This does
  # not disable Kokkos/CPE parallelism or change the Fortran reference.
  function( roms_kokkos_cxx_options target )
    if( _kk_backend STREQUAL "sw64" )
      target_compile_options( ${target} PRIVATE
        "$<$<COMPILE_LANGUAGE:CXX>:-mhybrid-coding;-fpermissive>" )
      if( ROMS_FAST_MATH )
        target_compile_options( ${target} PRIVATE
          "$<$<COMPILE_LANGUAGE:CXX>:--param;vect-max-version-for-alias-checks=64>" )
      else()
        target_compile_options( ${target} PRIVATE
          "$<$<COMPILE_LANGUAGE:CXX>:-fno-tree-vectorize>" )
      endif()
    endif()
  endfunction()

  target_link_libraries( Objects Kokkos::kokkos )
  roms_kokkos_cxx_options( Objects )

  option( ROMS_KOKKOS_TESTS "Build the direct kernel regression tests" OFF )
  if( ROMS_KOKKOS_TESTS )
    function( roms_kokkos_test target )
      add_executable( ${target} ${ARGN} )
      target_link_libraries( ${target} Kokkos::kokkos )
      set_target_properties( ${target} PROPERTIES LINKER_LANGUAGE Fortran )
      roms_kokkos_cxx_options( ${target} )
    endfunction()

    roms_kokkos_test( test_step2d_kokkos ROMS/Cpp/test_step2d_kokkos.cpp
      ROMS/Cpp/test_step2d_reference.f90
      ROMS/Cpp/kokkos_config.cpp ROMS/Cpp/kokkos_step2d.cpp )

    if( ROMS_KOKKOS_STEP3D_T )
      preprocess_fortran( ROMS/Cpp/test_step3d_t_reference.F )
      roms_kokkos_test( test_step3d_t_kokkos ${f90srcs}
        ROMS/Cpp/test_step3d_t_kokkos.cpp ROMS/Cpp/kokkos_config.cpp
        ROMS/Cpp/kokkos_step3d_t.cpp )
      # It recompiles the ROMS modules; serialise their .mod writes.
      add_dependencies( test_step3d_t_kokkos Objects )
    endif()

    # Fortran only.  The two small modules are the preprocessed copies that
    # the Objects library generates (a .F source would be compiled as fixed
    # form); their own module directory keeps them from overwriting the ones
    # Objects writes.
    add_executable( test_row_guards
      "${CMAKE_CURRENT_BINARY_DIR}/f90/kokkos_step3d_t_mod.f90"
      "${CMAKE_CURRENT_BINARY_DIR}/f90/kokkos_loops_mod.f90"
      ROMS/Cpp/test_row_guards.f90 )
    set_target_properties( test_row_guards PROPERTIES
      Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/module_test_row_guards" )
    add_dependencies( test_row_guards Objects )
  endif()
endif()

if( IN_ECBUILD )
  ecbuild_add_library( TARGET   ${PROJECT_NAME}
                       SOURCES  $<TARGET_OBJECTS:Objects>
                       PUBLIC_INCLUDES
                       INSTALL_HEADERS LISTED
                       LINKER_LANGUAGE ${ROMS_LINKER_LANGUAGE}
                     )

  # The ecbuild library is consumed through $<TARGET_OBJECTS:Objects>, which
  # does not carry Objects' link interface. Keep Kokkos on this public target
  # so ecbuild downstream consumers receive the dependency.
  if( ROMS_KOKKOS )
    target_link_libraries( ${PROJECT_NAME} PUBLIC Kokkos::kokkos )
  endif()

  # Set location of modules for editing and recompiling. CMake needs the location
  # of modules in the project subdirectory (-I${CMAKE_BINARY_DIR}/roms/module)
  # for correct USE association.

  set(MODULE_DIR ${PROJECT_NAME}/module)
  set_target_properties(${PROJECT_NAME} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR})
  install(DIRECTORY ${CMAKE_BINARY_DIR}/${MODULE_DIR}/ DESTINATION ${CMAKE_INSTALL_LIBDIR}/${MODULE_DIR})
  target_include_directories(${PROJECT_NAME} INTERFACE
                              $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/${MODULE_DIR}>
                              $<INSTALL_INTERFACE:${CMAKE_INSTALL_LIBDIR}/${MODULE_DIR}>)
endif()

# Add dependency on custom target "fix" so "DependInfo.cmake" is cleaned up
# before the the dependencies are calculated.

add_dependencies( Objects fix )

# This is to set the install prefix to the build directory as long as the
# user has not set their own install prefix.

if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set(CMAKE_INSTALL_PREFIX ${CMAKE_CURRENT_BINARY_DIR} CACHE PATH "..." FORCE)
endif()

# Shared libraries need PIC. There is perfomance degradation to position
# independent code so we only set this if shared libraries are requested.
# This really has no effect when using ecbuild since Position Indpendent
# Code (PIC) is enabled by default inside ecbuild regardless of the type
# of library requested.

set( deplib )

if( IN_ECBUILD )
  message( STATUS "Enabling position independent code" )
  set_property(TARGET Objects PROPERTY POSITION_INDEPENDENT_CODE 1)
else()
  if( LIBSHARED )
    message( STATUS "Enabling position independent code" )
    set_property(TARGET Objects PROPERTY POSITION_INDEPENDENT_CODE 1)
    add_library( ROMS_shared SHARED $<TARGET_OBJECTS:Objects> )
    set_target_properties(ROMS_shared PROPERTIES OUTPUT_NAME ROMS)
    if( ROMS_KOKKOS )
      target_link_libraries( ROMS_shared PUBLIC Kokkos::kokkos )
    endif()
    install( TARGETS ROMS_shared LIBRARY )

    # The executable links the ROMS_shared target, which already orders
    # the build; the explicit dependency below is kept for clarity.

    list( APPEND deplib ROMS_shared )
  endif()

  if( LIBSTATIC )
    add_library( ROMS_static STATIC $<TARGET_OBJECTS:Objects> )
    set_target_properties(ROMS_static PROPERTIES OUTPUT_NAME ROMS)
    if( ROMS_KOKKOS )
      target_link_libraries( ROMS_static PUBLIC Kokkos::kokkos )
    endif()
    install( TARGETS ROMS_static ARCHIVE )

    # The executable links the ROMS_static target, which already orders
    # the build; the explicit dependency below is kept for clarity.

    list( APPEND deplib ROMS_static )
  endif()

  # There needs to be at least one source file for the "add_executable" line
  # so "Master/master.F" is removed from "Master/CMakeLists.txt" and added
  # manually here.
  #
  # I would have done all the files in "Master" here to simplify things
  # but "roms_kernel.F", "coupler.F", and "propagator.F" include
  # modules needed in other parts of the code outside "Master".

  if( ROMS_EXECUTABLE )

    set( CMAKE_BUILD_WITH_INSTALL_RPATH FALSE )

    message( STATUS "CMAKE_INSTALL_RPATH_USE_LINK_PATH = ${CMAKE_INSTALL_RPATH_USE_LINK_PATH}" )
    message( STATUS "CMAKE_SKIP_BUILD_RPATH = ${CMAKE_SKIP_BUILD_RPATH}" )
    message( STATUS "CMAKE_BUILD_WITH_INSTALL_RPATH = ${CMAKE_BUILD_WITH_INSTALL_RPATH}" )
    message( STATUS "CMAKE_INSTALL_RPATH = ${CMAKE_INSTALL_RPATH}" )

    if( APPLE )
      message( STATUS "CMAKE_MACOSX_RPATH = ${CMAKE_MACOSX_RPATH}" )
    endif()

    # If PARPACK/ARPACK is needed add directory for the linker. The
    # "link_directories" and "target_link_libraries" for PARPACK/ARPACK
    # are separated because "link_libraries" has to be set before
    # "add_executable" and "target_link_libraries" has to be set after.

    if( ARPACK )
      link_directories( ${PARPACK_LIBDIR} ${ARPACK_LIBDIR} )
    endif()

    # If parallel I/O using SCORPIO is requested, add directory for the
    # linker. The "link_directories" and "target_link_libraries" for
    # are separated because "link_libraries" has to be set before
    # "add_executable" and "target_link_libraries" has to be set after.

    if( SCORPIO )
      link_directories( ${PNETCDF_LIBDIR} ${PIO_LIBDIR} )
    endif()

    preprocess_fortran( Master/master.F )
    set( master_f90 "${f90srcs}" )
    add_executable( "${BIN}" ${master_f90} )
    add_dependencies( "${BIN}" Objects ${deplib} )

    if( LIBSHARED AND LIBSTATIC )
      Message( STATUS "Both LIBSHARED and LIBSTATIC are enabled, the linker will probably default to the shared ROMS library." )
      target_link_libraries( "${BIN}" ROMS_shared ${netcdf_libs} )
    elseif( LIBSHARED )
      Message( STATUS "Linking ROMS using shared ROMS library." )
      target_link_libraries( "${BIN}" ROMS_shared ${netcdf_libs} )
    elseif( LIBSTATIC )
      Message( STATUS "Linking ROMS using static ROMS library." )
      target_link_libraries( "${BIN}" ROMS_static ${netcdf_libs} )
    else()
      Message( FATAL_ERROR "Unknown configuration!" )
    endif()

    # Kokkos kernels live in ROMS/Cpp and are linked into the executable.
    # The linker language is forced back to Fortran: with C++ objects in
    # the link, CMake would otherwise pick the C++ driver, which does not
    # know about the Fortran runtime ROMS needs.

    if( ROMS_KOKKOS )
      target_link_libraries( "${BIN}" Kokkos::kokkos )
      set_target_properties( "${BIN}" PROPERTIES LINKER_LANGUAGE Fortran )
    endif()

    # If ARPACK is needed, add to linking library list.
    # The path to the library is set above.

    if( ARPACK )
      target_link_libraries( "${BIN}" parpack arpack )
    endif()

    # if ESMF is requested, add to linking library list

    if( MYDEFS MATCHES "ESMF_LIB" )
      target_link_libraries( "${BIN}" ESMF )
    endif()

    # If parallel I/O with SCORPIO is requested, add to linking library list.
    # The path to the libraries are set above.

    if( SCORPIO )
      target_link_libraries( "${BIN}" pnetcdf piof pioc )
    endif()

    install( TARGETS "${BIN}" RUNTIME )

  endif()
endif()

if( IN_ECBUILD )
  message( STATUS "CMAKE_Fortran_MODULE_DIRECTORY=${CMAKE_Fortran_MODULE_DIRECTORY}" )
  message( STATUS "CMAKE_BINARY_DIR=${CMAKE_BINARY_DIR}" )
  target_include_directories( ${PROJECT_NAME} INTERFACE
                              $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
                              $<INSTALL_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}> )

  ecbuild_install_project( NAME ${PROJECT_NAME} )
  ecbuild_print_summary()
endif()

file( WRITE ${CMAKE_CURRENT_BINARY_DIR}/fortran_flags ${my_fflags} )
