# Install script for directory: /staff/liyuanhang/work/2024OCT/roms_8/roms

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib" TYPE STATIC_LIBRARY FILES "/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/libROMS.a")
endif()

if(CMAKE_INSTALL_COMPONENT STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/romsM" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/romsM")
    file(RPATH_CHECK
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/romsM"
         RPATH "$ORIGIN/../lib:/staff/liyuanhang/work/2024OCT/roms_8/install/netcdf/lib:/staff/liyuanhang/work/2024OCT/roms_8/install/hdf5/lib:/usr/local/lib:/usr/local/cuda-11.7/targets/x86_64-linux/lib")
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE EXECUTABLE FILES "/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/romsM")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/romsM" AND
     NOT IS_SYMLINK "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/romsM")
    file(RPATH_CHANGE
         FILE "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/romsM"
         OLD_RPATH "/staff/liyuanhang/work/2024OCT/roms_8/install/netcdf/lib:/staff/liyuanhang/work/2024OCT/roms_8/install/hdf5/lib:/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM:/usr/local/lib:/usr/local/cuda-11.7/targets/x86_64-linux/lib:"
         NEW_RPATH "$ORIGIN/../lib:/staff/liyuanhang/work/2024OCT/roms_8/install/netcdf/lib:/staff/liyuanhang/work/2024OCT/roms_8/install/hdf5/lib:/usr/local/lib:/usr/local/cuda-11.7/targets/x86_64-linux/lib")
    if(CMAKE_INSTALL_DO_STRIP)
      execute_process(COMMAND "/usr/bin/strip" "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/bin/romsM")
    endif()
  endif()
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/YAKL/cmake_install.cmake")
  include("/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/Master/cmake_install.cmake")
  include("/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/ROMS/Drivers/cmake_install.cmake")
  include("/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/ROMS/Functionals/cmake_install.cmake")
  include("/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/ROMS/Modules/cmake_install.cmake")
  include("/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/ROMS/Nonlinear/cmake_install.cmake")
  include("/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/ROMS/Nonlinear/BBL/cmake_install.cmake")
  include("/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/ROMS/Nonlinear/Biology/cmake_install.cmake")
  include("/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/ROMS/Nonlinear/Sediment/cmake_install.cmake")
  include("/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/ROMS/Utility/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/staff/liyuanhang/work/2024OCT/roms_8/roms/project/upwelling/CBuild_romsM/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
