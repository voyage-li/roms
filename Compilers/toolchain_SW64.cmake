# Cross-compile on the SW login node after loading swgcc and mpi/swuc.
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR sw64)
set(SW64 TRUE CACHE BOOL "Sunway platform (also for the Serial control)")
set(SWUC TRUE CACHE BOOL "Sunway unified compiler")

set(CMAKE_CXX_COMPILER swucxx CACHE FILEPATH "Sunway C++ compiler")
# The available NetCDF/HDF5 archive depends on MPI, even when ROMS itself
# is built without distributed-memory support.  The wrapper supplies that
# link dependency; MPI=OFF still compiles a single-process, MPE-only ROMS.
set(CMAKE_Fortran_COMPILER mpif90 CACHE FILEPATH "Sunway Fortran link wrapper")
set(CMAKE_EXE_LINKER_FLAGS_INIT "-mhybrid")
set(CMAKE_CXX_FLAGS_INIT "-mieee")
set(CMAKE_Fortran_FLAGS_INIT "-mieee")
set(CMAKE_CXX_STANDARD 17 CACHE STRING "C++ standard")
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
