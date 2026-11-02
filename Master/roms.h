      PROGRAM MyROMS
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2025 The ROMS Group                              !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  Regional Ocean Model System (ROMS)                                  !
!                                                                      !
!  Master program to execute  ROMS  drivers in ocean mode only         !
!  without coupling (sequential or concurrent) to  any  atmospheric    !
!  model.                                                              !
!                                                                      !
!  This ocean model solves the free surface, hydrostatic, primitive    !
!  equations  over  variable  topography  using  stretched terrain-    !
!  following coordinates in the vertical and orthogonal curvilinear    !
!  coordinates in the horizontal.                                      !
!                                                                      !
!  Nonlinear Model Developers:                                         !
!                                                                      !
!  Dr. Hernan G. Arango                                                !
!    Institute of Marine and Coastal Sciences                          !
!    Rutgers University, New Brunswick, NJ, USA                        !
!    (arango@marine.rutgers.edu)                                       !
!                                                                      !
!  Dr. Alexander F. Shchepetkin                                        !
!    Institute of Geophysics and Planetary Physics                     !
!    UCLA, Los Angeles, CA, USA                                        !
!    (alex@atmos.ucla.edu)                                             !
!                                                                      !
!  Dr. John C. Warner                                                  !
!    U.S. Geological Survey                                            !
!    Woods Hole, MA, USA                                               !
!    (jcwarner@usgs.gov)                                               !
!                                                                      !
!  Tangent linear and Adjoint Models and Algorithms Developers:        !
!                                                                      !
!    Dr. Hernan G. Arango    (arango@marine.rutgers.edu)               !
!    Dr. Bruce Cornuelle     (bcornuelle@ucsd.edu)                     !
!    Dr. Emanuele Di Lorenzo (edl@eas.gatech.edu)                      !
!    Dr. Arthur J. Miller    (ajmiller@ucsd.edu)                       !
!    Dr. Andrew M. Moore     (ammoore@ucsc.edu)                        !
!    Dr. Brian Powell        (powellb@uscs.edu)                        !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_scalars
!
      USE mod_arrays,      ONLY : ROMS_deallocate_arrays
!
      USE roms_kernel_mod, ONLY : ROMS_initialize
      USE roms_kernel_mod, ONLY : ROMS_run
      USE roms_kernel_mod, ONLY : ROMS_finalize
#ifdef ROMS_KOKKOS
      USE kokkos_step2d_mod, ONLY : kokkos_init, kokkos_finalize
#endif
#if defined PIO_LIB && defined DISTRIBUTE
      USE set_pio_mod,     ONLY : finalize_pio
#endif
!
      implicit none
!
!  Local variable declarations.
!
      logical, save :: first

      integer :: ng, MyError, provided, required
      integer(kind=SELECTED_INT_KIND(18)) :: wall_start, wall_end
      integer(kind=SELECTED_INT_KIND(18)) :: wall_rate

#ifdef DISTRIBUTE
# ifdef MPI
!
!-----------------------------------------------------------------------
!  Initialize distributed-memory MPI configuration.
!-----------------------------------------------------------------------
!
#  ifdef MULTI_THREAD
      required=MPI_THREAD_MULTIPLE
      CALL mpi_init_thread (required, provided, MyError)
      IF (MyError.ne.0) THEN
        PRINT '(/,a)',' ROMS: Unable to initialize multi-threaded MPI'
        exit_flag=6
      END IF
#  else
      CALL mpi_init (MyError)
      IF (MyError.ne.0) THEN
        PRINT '(/,a)',' ROMS: Unable to initialize MPI'
        exit_flag=6
      END IF
#  endif
!
!  Get rank of the local process in the group associated with the
!  comunicator.
!
      CALL mpi_comm_rank (MPI_COMM_WORLD, MyRank, MyError)
      IF (MyError.ne.0) THEN
        PRINT '(/,a)',' ROMS: Unable to inquire rank of local processor'
        exit_flag=6
      END IF
# endif
#endif
!
!-----------------------------------------------------------------------
!  Initialize ocean internal and external parameters and state
!  variables for all nested grids, if applicable.
!-----------------------------------------------------------------------
!
      IF (exit_flag.eq.NoError) THEN
#ifdef ROMS_KOKKOS
        CALL kokkos_init
#endif
        first=.TRUE.
        CALL SYSTEM_CLOCK (wall_start, wall_rate)
        CALL ROMS_initialize (first)
        CALL SYSTEM_CLOCK (wall_end)
        IF (Master.and.(wall_rate.gt.0)) THEN
          WRITE (stdout,'(a,f14.6)') 'ROMS initialization wall: ', &
     &                            REAL(wall_end-wall_start,r8)/ &
     &                            REAL(wall_rate,r8)
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Time-step ocean model over all nested grids, if applicable, by the
!  specified time interval in seconds.
!-----------------------------------------------------------------------
!
      IF (exit_flag.eq.NoError) THEN
        run_time=0.0_r8
        DO ng=1,Ngrids
          run_time=MAX(run_time, dt(ng)*ntimes(ng))
        END DO
        CALL SYSTEM_CLOCK (wall_start, wall_rate)
        CALL ROMS_run (run_time)
        CALL SYSTEM_CLOCK (wall_end)
        IF (wall_rate.gt.0) THEN
          WRITE (stdout,'(a,i0,a,f14.6)') &
     &      'ROMS time stepping wall [rank=', MyRank, ']: ', &
     &      REAL(wall_end-wall_start,r8)/REAL(wall_rate,r8)
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Terminate ocean model execution: flush and close all IO files.
!-----------------------------------------------------------------------
!
      CALL ROMS_finalize
#if defined PIO_LIB && defined DISTRIBUTE
      CALL finalize_pio
#endif
#ifdef ROMS_KOKKOS
      CALL kokkos_finalize
#endif
      CALL ROMS_deallocate_arrays
#if defined DISTRIBUTE && defined MPI
      CALL mpi_finalize (MyError)
#endif
!
!-----------------------------------------------------------------------
!  If error, issue abort signal. It is expected that the Unix or POSIX
!  environment should handle this as a return errorcode from the ROMS
!  main to abort. It facilitates error handling in scripting.
!-----------------------------------------------------------------------
!
!!    IF ((exit_flag.ne.NoError).or.(blowup.ne.0)) STOP 911   ! F-2003
      IF ((exit_flag.ne.NoError).or.(blowup.ne.0)) ERROR STOP ! F-2008
!
      END PROGRAM MyROMS
