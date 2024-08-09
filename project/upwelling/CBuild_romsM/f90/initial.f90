      SUBROUTINE initial
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This routine initializes all model variables.                       !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_ocean
      USE mod_scalars
      USE mod_stepping
!
      USE analytical_mod
      USE close_io_mod,      ONLY : close_inp
      USE dateclock_mod,     ONLY : time_string
      USE distribute_mod,    ONLY : mp_bcasti
      USE get_state_mod,     ONLY : get_state
      USE ini_hmixcoef_mod,  ONLY : ini_hmixcoef
      USE set_depth_mod,     ONLY : set_depth0, set_depth
      USE omega_mod,         ONLY : omega
      USE rho_eos_mod,       ONLY : rho_eos
      USE set_massflux_mod,  ONLY : set_massflux
      USE stiffness_mod,     ONLY : stiffness
      USE strings_mod,       ONLY : FoundError
!
      implicit none
!
!  Local variable declarations.
!
      logical :: update = .FALSE.
!
      integer :: Fcount
      integer :: ng, thread, tile
      integer, dimension(Ngrids) :: IniRec, Tindex
!
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Nonlinear/initial.F"
!
!=======================================================================
!   Initialize model variables.
!=======================================================================
!
!$OMP MASTER
      IF (Master) THEN
        WRITE (stdout,20) 'INITIAL: Configuring and initializing ',     &
     &                    'forward nonlinear model ...'
 20     FORMAT (/,1x,a,a,/,1x,'*******')
      END IF
!$OMP END MASTER
!
!-----------------------------------------------------------------------
!  Initialize time stepping indices and counters.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        iif(ng)=1
        indx1(ng)=1
        next_kstp(ng)=1
        kstp(ng)=1
        krhs(ng)=1
        knew(ng)=1
        PREDICTOR_2D_STEP(ng)=.FALSE.
!
        iic(ng)=0
        nstp(ng)=1
        nrhs(ng)=1
        nnew(ng)=1
        nf(ng)=0
        nfp1(ng)=1
        nfm1(ng)=4
        nfm2(ng)=3
        nfm3(ng)=2
!
        IniRec(ng)=nrrec(ng)
        Tindex(ng)=1
!
        synchro_flag(ng)=.TRUE.
        first_time(ng)=0
        tdays(ng)=dstart
        time(ng)=tdays(ng)*day2sec
!$OMP MASTER
        ntstart(ng)=INT((time(ng)-dstart*day2sec)/dt(ng))+1
        ntend(ng)=ntstart(ng)+ntimes(ng)-1
        ntfirst(ng)=ntstart(ng)
!$OMP END MASTER
!$OMP BARRIER
        step_counter(ng)=0
      END DO
!
!  Initialize global diagnostics variables.
!
      avgke=0.0_dp
      avgpe=0.0_dp
      avgkp=0.0_dp
      volume=0.0_dp
!
!-----------------------------------------------------------------------
!  Start time wall clocks.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO thread=MyRank,MyRank
          CALL wclock_on (ng, iNLM, 2, 192, MyFile)
        END DO
      END DO
!$OMP BARRIER
!
!-----------------------------------------------------------------------
!  Set application grid, metrics, and associated variables and
!  parameters.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        IF (SetGridConfig(ng)) THEN
          CALL set_grid (ng, iNLM)
          SetGridConfig(ng)=.FALSE.
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Initialize horizontal mixing coefficients. If applicable, scale
!  mixing coefficients according to the grid size (smallest area).
!  Also increase their values in sponge areas using the "visc_factor"
!  and/or "diff_factor" read from input Grid NetCDF file.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL ini_hmixcoef (ng, tile, iNLM)
        END DO
!$OMP BARRIER
      END DO
!
!=======================================================================
!  Initialize model state variables and forcing.  This part is
!  executed for each ensemble/perturbation/iteration run.
!=======================================================================
!
!-----------------------------------------------------------------------
!  If analytical initial conditions, compute initial time-evolving
!  depths with zero free-surface.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL set_depth (ng, tile, iNLM)
        END DO
!$OMP BARRIER
      END DO
!
!-----------------------------------------------------------------------
!  Set primitive variables initial conditions.
!-----------------------------------------------------------------------
!
!  Analytical initial conditions for momentum and active tracers.
!
      DO ng=1,Ngrids
        IF (nrrec(ng).eq.0) THEN
          DO tile=first_tile(ng),last_tile(ng),+1
            CALL ana_initial (ng, tile, iNLM)
          END DO
!$OMP BARRIER
        END IF
      END DO
!
!  Analytical initial conditions for biology tracers.
!
      DO ng=1,Ngrids
        IF (nrrec(ng).eq.0) THEN
          DO tile=first_tile(ng),last_tile(ng),+1
            CALL ana_biology (ng, tile, iNLM)
          END DO
!$OMP BARRIER
        END IF
      END DO
!
!  If restart, read in initial conditions restart NetCDF file.
!
      DO ng=1,Ngrids
        IF (nrrec(ng).ne.0) THEN
!$OMP MASTER
          CALL get_state (ng, 0, 1, INI(ng), IniRec(ng), Tindex(ng))
!$OMP END MASTER
          CALL mp_bcasti (ng, iNLM, exit_flag)
!$OMP BARRIER
          IF (FoundError(exit_flag, NoError, 433, MyFile)) RETURN
          time(ng)=io_time                   ! needed for shared-memory
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Compute time independent (Zt_avg1=0) anf initial time dependent
!  depths and level thicknesses.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL set_depth0 (ng, tile, iNLM)
          CALL set_depth  (ng, tile, iNLM)
        END DO
!$OMP BARRIER
      END DO
!
!-----------------------------------------------------------------------
!  Compute initial horizontal mass fluxes, Hz*u/n and Hz*v/m.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL set_massflux (ng, tile, iNLM)
        END DO
!$OMP BARRIER
      END DO
!
!-----------------------------------------------------------------------
!  Compute initial S-coordinates vertical velocity. Compute initial
!  density anomaly from potential temperature and salinity via equation
!  of state for seawater.  Also compute other equation of state related
!  quatities.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO tile=first_tile(ng),last_tile(ng),+1
          CALL omega (ng, tile, iNLM)
          CALL rho_eos (ng, tile, iNLM)
        END DO
!$OMP BARRIER
      END DO
!
!-----------------------------------------------------------------------
!  Read in initial forcing, climatology and assimilation data from
!  input NetCDF files.  It loads the first relevant data record for
!  the time-interpolation between snapshots.
!-----------------------------------------------------------------------
!
!  If applicable, close all input boundary, climatology, and forcing
!  NetCDF files and set associated parameters to the closed state. This
!  step is essential in iterative algorithms that run the full TLM
!  repetitively. Then, Initialize several parameters in their file
!  structure, so the appropriate input single or multi-file is selected
!  during initialization/restart.
!
      DO ng=1,Ngrids
!$OMP MASTER
        CALL close_inp (ng, iNLM)
        CALL check_multifile (ng, iNLM)
!$OMP END MASTER
        CALL mp_bcasti (ng, iNLM, exit_flag)
!$OMP BARRIER
        IF (FoundError(exit_flag, NoError, 657, MyFile)) RETURN
      END DO
!
!  If applicable, read in input data.
!
      DO ng=1,Ngrids
!$OMP MASTER
        CALL get_idata (ng)
        CALL get_data (ng)
!$OMP END MASTER
        CALL mp_bcasti (ng, iNLM, exit_flag)
!$OMP BARRIER
        IF (FoundError(exit_flag, NoError, 671, MyFile)) RETURN
      END DO
!
!-----------------------------------------------------------------------
!  Set analytical spatially varying bottom friction parameter.
!-----------------------------------------------------------------------
!
      IF (Nrun.eq.ERstr) THEN
        DO ng=1,Ngrids
          DO tile=first_tile(ng),last_tile(ng),+1
            CALL ana_drag (ng, tile, iNLM)
          END DO
!$OMP BARRIER
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Compute grid stiffness.
!-----------------------------------------------------------------------
!
      IF (Lstiffness) THEN
        Lstiffness=.FALSE.
        DO ng=1,Ngrids
          DO tile=first_tile(ng),last_tile(ng),+1
            CALL stiffness (ng, tile, iNLM)
          END DO
!$OMP BARRIER
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  If applicable, convert initial locations to fractional grid
!  coordinates.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
!$OMP MASTER
        CALL grid_coords (ng, iNLM)
!$OMP END MASTER
!$OMP BARRIER
      END DO
!
!-----------------------------------------------------------------------
!  Initialize time-stepping counter and date/time string. Save NLM
!  initial conditions time.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        INItime(ng)=time(ng)
        iic(ng)=ntstart(ng)
        CALL time_string (time(ng), time_code(ng))
      END DO
!
!-----------------------------------------------------------------------
!  Turn off initialization time wall clock.
!-----------------------------------------------------------------------
!
      DO ng=1,Ngrids
        DO thread=MyRank,MyRank
          CALL wclock_off (ng, iNLM, 2, 868, MyFile)
        END DO
!$OMP BARRIER
      END DO
!
      RETURN
      END SUBROUTINE initial
