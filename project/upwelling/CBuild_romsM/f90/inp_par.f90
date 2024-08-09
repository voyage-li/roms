      MODULE inp_par_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This routine reads in input model parameters from standard input.   !
!  It also writes out these parameters to standard output.             !
!                                                                      !
!=======================================================================
!
      USE mod_kinds
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_ncparam
      USE mod_scalars
      USE mod_strings
!
      USE dateclock_mod,    ONLY : get_date
      USE distribute_mod,   ONLY : mp_bcasti, mp_bcasts
      USE lbc_mod,          ONLY : lbc_report
      USE ran_state,        ONLY : ran_seed
      USE stdinp_mod,       ONLY : stdinp_unit
      USE strings_mod,      ONLY : FoundError
      USE tadv_mod,         ONLY : tadv_report
      USE tile_indices_mod, ONLY : tile_indices, tile_obs_bounds
!
      implicit none
!
      PUBLIC  :: inp_par
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE inp_par (model)
!***********************************************************************
!
!  Imported variable declarations.
!
      integer, intent(in) :: model
!
!  Local variable declarations.
!
      logical :: GotFile, Lwrite
!
      integer :: Nghost, tile
      integer :: Imin, Imax, Jmin, Jmax
      integer :: MaxHaloLenI, MaxHaloLenJ
      integer :: ibry, inp, out, i, ic, ifield, itrc, j, ng, npts
      integer :: sequence, varid
!
      real(r8) :: cff
      real(r8), parameter :: spv = 0.0_r8
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/inp_par.F"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Read in and report input model parameters.
!-----------------------------------------------------------------------
!
!
!  Get in ROMS standard input script filename (Iname) and and open it
!  as a regular formatted file in distributed-memory configurations.
!
      inp=stdinp_unit(Master, GotFile)
      out=stdout
      Lwrite=Master
!
      IF (.not.GotFile) THEN
        IF (Master) WRITE (out,10)
 10     FORMAT (/,' INP_PAR - Unable to ROMS standard input file, ',    &

                'Iname')
        exit_flag=2
      END IF
      IF (FoundError(exit_flag, NoError, 104, MyFile)) RETURN
!
!  Get current date.
!
      IF (Master) CALL get_date (date_str)
      CALL mp_bcasts (1, model, date_str)
!
!-----------------------------------------------------------------------
!  Read in physical model input parameters.
!-----------------------------------------------------------------------
!
      IF (Master.and.Lwrite) WRITE (out,20) version, TRIM(date_str)
 20   FORMAT (80('-'),/,                                                &
              ' Model Input Parameters:  ROMS/TOMS version ',a,/,       &
     &        26x,a,/,80('-'))
!
!  Process ROMS standard input Iname script.
!
      CALL read_PhyPar (model, inp, out, Lwrite)
      CALL mp_bcasti (1, model, exit_flag)
      IF (FoundError(exit_flag, NoError, 147, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Read in biological model input parameters.
!-----------------------------------------------------------------------
!
      OPEN (25, FILE=TRIM(bparnam), FORM='formatted', STATUS='old')
      CALL read_BioPar (model, 25, out, Lwrite)
      IF (FoundError(exit_flag, NoError, 180, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Set application domain parameters and switches.
!-----------------------------------------------------------------------
!
!  Set switch for three ghost-points in the halo region.
!
      ThreeGhostPoints=ANY(Hadvection(:,:)%MPDATA).or.                  &
     &                 ANY(Hadvection(:,:)%HSIMT)
!
!  Determine the number of ghost-points in the halo region.
!
      IF (ThreeGhostPoints) THEN
        NghostPoints=3
      ELSE
        NghostPoints=2
      END IF
      IF (ANY(CompositeGrid).or.ANY(RefinedGrid)) THEN
        NghostPoints=MAX(3,NghostPoints)
      END IF
!
!  Determine the switch to process input open boundary conditions data.
!
!  In nesting applications, the lateral boundary conditions data is
!  is needed only by the main coarser grid (RefineScale(ng)=0).
!
      DO ng=1,Ngrids
        IF (.not.(RefinedGrid(ng).and.RefineScale(ng).gt.0)) THEN
          LprocessOBC(ng)=.TRUE.
        END IF
      END DO
      CALL tile_indices (model, Im, Jm, Lm, Mm,                         &
     &                   BOUNDS, DOMAIN, IOBOUNDS)
!
!-----------------------------------------------------------------------
!  Set minimum and maximum fractional coordinates for processing
!  observations.
!-----------------------------------------------------------------------
!
      CALL tile_obs_bounds (model, Im, Jm, Lm, Mm,                      &
     &                      DOMAIN)
!
!-----------------------------------------------------------------------
!  Check tile partition starting and ending (I,J) indices for illegal
!  domain decomposition parameters NtileI and NtileJ in standard input
!  file.
!-----------------------------------------------------------------------
!
      IF (Master.and.Lwrite) THEN
        DO ng=1,Ngrids
          WRITE (stdout,50) ng, Lm(ng), Mm(ng), N(ng),                  &
     &                      NtileI(ng), NtileJ(ng)
          DO tile=0,NtileI(ng)*NtileJ(ng)-1
            npts=(BOUNDS(ng)%Iend(tile)-                                &
     &            BOUNDS(ng)%Istr(tile)+1)*                             &
     &           (BOUNDS(ng)%Jend(tile)-                                &
     &            BOUNDS(ng)%Jstr(tile)+1)*N(ng)
            WRITE (stdout,70) tile,                                     &
     &                        BOUNDS(ng)%Istr(tile),                    &
     &                        BOUNDS(ng)%Iend(tile),                    &
     &                        BOUNDS(ng)%Jstr(tile),                    &
     &                        BOUNDS(ng)%Jend(tile),                    &
     &                        npts
            IF ((BOUNDS(ng)%Iend(tile)-                                 &
     &           BOUNDS(ng)%Istr(tile)+1).lt.2) THEN
              WRITE (stdout,80) ng, 'NtileI = ', NtileI(ng),            &
     &                              'Lm = ', Lm(ng),                    &
     &                              'Istr = ', BOUNDS(ng)%Istr(tile),   &
     &                              '  Iend = ', BOUNDS(ng)%Iend(tile), &
     &                              'NtileI'
              exit_flag=6
              RETURN
            END IF
            IF ((BOUNDS(ng)%Jend(tile)-                                 &
     &           BOUNDS(ng)%Jstr(tile)+1).lt.2) THEN
              WRITE (stdout,80) ng, 'NtileJ = ', NtileJ(ng),            &
     &                              'Mm = ', Mm(ng),                    &
     &                              'Jstr = ', BOUNDS(ng)%Jstr(tile),   &
     &                              '  Jend = ', BOUNDS(ng)%Jend(tile), &
     &                              'NtileJ'
              exit_flag=6
              RETURN
            END IF
          END DO
        END DO
 50     FORMAT (/,' Tile partition information for Grid ',i2.2,':',2x,  &
     &          i0,'x',i0,'x',i0,2x,'tiling: ',i0,'x',i0,/,/,           &
     &          5x,'tile',5x,'Istr',5x,'Iend',5x,'Jstr',5x,'Jend',      &
     &          5x,'Npts',/)
 70     FORMAT (5(4x,i5),1x,i8)
 80     FORMAT (/,' INP_PAR - domain decomposition error in input ',    &
     &                        'script file for grid: ',i2.2,/,          &
     &          /,11x,'The domain partition parameter, ',a,i0,          &
     &          /,11x,'is incompatible with grid size, ',a,i0,          &
     &          /,11x,'because it yields too small tile, ',a,i0,a,i0,   &
     &          /,11x,'Decrease partition parameter: ',a)
      END IF
      CALL mp_bcasti (1, model, exit_flag)
      IF (FoundError(exit_flag, NoError, 413, MyFile)) RETURN
!
!  Report tile minimum and maximum fractional grid coordinates.
!
      DO ng=1,Ngrids
        IF (Master.and.Lwrite) THEN
          WRITE (stdout,90) ng
          DO tile=0,NtileI(ng)*NtileJ(ng)-1
            WRITE (stdout,100) tile,                                    &
     &                         DOMAIN(ng)%Xmin_rho(tile),               &
     &                         DOMAIN(ng)%Xmax_rho(tile),               &
     &                         DOMAIN(ng)%Ymin_rho(tile),               &
     &                         DOMAIN(ng)%Ymax_rho(tile), 'RHO-points'
          END DO
          WRITE (stdout,'(1x)')
          DO tile=0,NtileI(ng)*NtileJ(ng)-1
            WRITE (stdout,100) tile,                                    &
     &                         DOMAIN(ng)%Xmin_u(tile),                 &
     &                         DOMAIN(ng)%Xmax_u(tile),                 &
     &                         DOMAIN(ng)%Ymin_u(tile),                 &
     &                         DOMAIN(ng)%Ymax_u(tile), '  U-points'
          END DO
          WRITE (stdout,'(1x)')
          DO tile=0,NtileI(ng)*NtileJ(ng)-1
            WRITE (stdout,100) tile,                                    &
     &                         DOMAIN(ng)%Xmin_v(tile),                 &
     &                         DOMAIN(ng)%Xmax_v(tile),                 &
     &                         DOMAIN(ng)%Ymin_v(tile),                 &
     &                         DOMAIN(ng)%Ymax_v(tile), '  V-points'
          END DO
 90       FORMAT (/,' Tile minimum and maximum fractional coordinates', &
     &            ' for Grid ',i2.2,':'/,                               &
     &            '   (interior points only)',/,/,                      &
     &            5x,'tile',5x,'Xmin',5x,'Xmax',5x,'Ymin',5x,'Ymax',    &
     &            5x,'grid',/)
 100      FORMAT (5x,i4,4f9.2,2x,a)
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Determine the maximum tile lengths in XI and ETA directions for
!  distributed-memory communications.  Notice that halo size are
!  increased by few points to allow exchanging of private arrays.
!-----------------------------------------------------------------------
!
      IF (ANY(EWperiodic).or.ANY(NSperiodic)) THEN
        Nghost=NghostPoints+1
      ELSE
        Nghost=NghostPoints
      END IF
      DO ng=1,Ngrids
        MaxHaloLenI=0
        MaxHaloLenJ=0
        HaloBry(ng)=Nghost
        DO tile=0,NtileI(ng)*NtileJ(ng)-1
          Imin=BOUNDS(ng)%LBi(tile)-1
          Imax=BOUNDS(ng)%UBi(tile)+1
          Jmin=BOUNDS(ng)%LBj(tile)-1
          Jmax=BOUNDS(ng)%UBj(tile)+1
          MaxHaloLenI=MAX(MaxHaloLenI,(Imax-Imin+1))
          MaxHaloLenJ=MAX(MaxHaloLenJ,(Jmax-Jmin+1))
        END DO
        HaloSizeI(ng)=Nghost*MaxHaloLenI+6*Nghost
        HaloSizeJ(ng)=Nghost*MaxHaloLenJ+6*Nghost
        TileSide(ng)=MAX(MaxHaloLenI,MaxHaloLenJ)
        TileSize(ng)=MaxHaloLenI*MaxHaloLenJ
        IF (Master.and.Lwrite) THEN
          WRITE (stdout,110) ng, HaloSizeI(ng), ng, HaloSizeJ(ng),      &
     &                       ng, TileSide(ng),  ng, TileSize(ng)
 110      FORMAT (/,' Maximum halo size in XI and ETA directions:',/,   &
     &            /,'               HaloSizeI(',i1,') = ',i7,           &
     &            /,'               HaloSizeJ(',i1,') = ',i7,           &
     &            /,'                TileSide(',i1,') = ',i7,           &
     &            /,'                TileSize(',i1,') = ',i7,/)
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Read in floats input parameters.
!-----------------------------------------------------------------------
!
      OPEN (45, FILE=TRIM(fposnam), FORM='formatted', STATUS='old')
      CALL read_FltPar (model, 45, out, Lwrite)
      IF (FoundError(exit_flag, NoError, 518, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Read in stations input parameters.
!-----------------------------------------------------------------------
!
      OPEN (55, FILE=TRIM(sposnam), FORM='formatted', STATUS='old')
      CALL read_StaPar (model, 55, out, Lwrite)
      IF (FoundError(exit_flag, NoError, 540, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Report tracer advection scheme.
!-----------------------------------------------------------------------
!
      IF (Master.and.Lwrite) THEN
        WRITE (out,120) 'NLM'
 120    FORMAT (/,1x,'Tracer Advection Scheme: ',a,/,1x,24('='),/,      &
     &          /,1x,'Variable',t25,'Grid',t31,'Horizontal',            &
     &          t50,'Vertical', /,1x,'---------',t25,'----',            &
     &          t31,2('------------',7x))
      END IF
      CALL tadv_report (out, iNLM, Hadvection, Vadvection, Lwrite)
      IF (FoundError(exit_flag, NoError, 556, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Report lateral boundary conditions.
!-----------------------------------------------------------------------
!
      IF (Master.and.Lwrite) THEN
        WRITE (out,130) 'NLM'
 130    FORMAT (/,1x,'Lateral Boundary Conditions: ',a,/,1x,28('='),/,  &
     &          /,1x,'Variable',t25,'Grid',t31,'West Edge',             &
     &          t44,'South Edge', t57,'East Edge',t70,'North Edge',     &
     &          /,1x,'---------',t25,'----',t31,4('----------',3x))
        DO ifield=1,nLBCvar
          IF (idBvar(ifield).gt.0) THEN
            CALL lbc_report (out, ifield, LBC)
          END IF
        END DO
      END IF
      IF (FoundError(exit_flag, NoError, 621, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Compute various constants.
!-----------------------------------------------------------------------
!
      gorho0=g/rho0
      DO ng=1,Ngrids
        dtfast(ng)=dt(ng)/REAL(ndtfast(ng),r8)
!
!  Take the square root of the biharmonic coefficients so it can
!  be applied to each harmonic operator.
!
        nl_visc4(ng)=SQRT(ABS(nl_visc4(ng)))
        tkenu4(ng)=SQRT(ABS(tkenu4(ng)))
!
!  Set internal switch for activating sponge areas.
!
        IF (LuvSponge(ng).or.                                           &
     &      ANY(LtracerSponge(:,ng))) THEN
          Lsponge(ng)=.TRUE.
        END IF
!
!  Set switch to processing nudging coefficients for passive/active
!  boundary conditions.
!
        NudgingCoeff(ng)=ANY(LBC(:,:,ng)%nudging)
!
!  Set internal switch for processing climatology data.
!
        IF (LsshCLM(ng).or.                                             &
            Lm2CLM (ng).or.LnudgeM2CLM(ng).or.                          &
            Lm3CLM (ng).or.LnudgeM3CLM(ng).or.                          &
            ANY(LtracerCLM(:,ng)).or.ANY(LnudgeTCLM(:,ng))) THEN
          Lclimatology(ng)=.TRUE.
        END IF
!
!  Set internal switch for nudging to climatology fields.
!
        IF (LnudgeM2CLM(ng).or.                                         &
     &      LnudgeM3CLM(ng).or.                                         &
     &      ANY(LnudgeTCLM(:,ng))) THEN
          Lnudging(ng)=.TRUE.
        END IF
!
!  Compute inverse nudging coefficients (1/s) used in various tasks.
!
        IF (Znudg(ng).gt.0.0_r8) THEN
          Znudg(ng)=1.0_r8/(Znudg(ng)*86400.0_r8)
        ELSE
          Znudg(ng)=0.0_r8
        END IF
!
        IF (M2nudg(ng).gt.0.0_r8) THEN
          M2nudg(ng)=1.0_r8/(M2nudg(ng)*86400.0_r8)
        ELSE
          M2nudg(ng)=0.0_r8
        END IF
!
        IF (M3nudg(ng).gt.0.0_r8) THEN
          M3nudg(ng)=1.0_r8/(M3nudg(ng)*86400.0_r8)
        ELSE
          M3nudg(ng)=0.0_r8
        END IF
!
!  Set nudging coefficients (1/s) for passive/active (outflow/inflow)
!  open boundary conditions.  Weak nudging is expected in passive
!  outflow conditions and strong nudging is expected in active inflow
!  conditions. If nudging to climatology fields, these values are
!  replaced by spatial nudging coefficients distribution in the
!  open boundary condition routines.
!
        IF (NudgingCoeff(ng)) THEN
          DO ibry=1,4
            IF (LBC(ibry,isFsur,ng)%nudging) THEN
              FSobc_out(ng,ibry)=Znudg(ng)
              FSobc_in (ng,ibry)=obcfac(ng)*Znudg(ng)
            END IF
!
            IF (LBC(ibry,isUbar,ng)%nudging.or.                         &
     &          LBC(ibry,isVbar,ng)%nudging) THEN
              M2obc_out(ng,ibry)=M2nudg(ng)
              M2obc_in (ng,ibry)=obcfac(ng)*M2nudg(ng)
            END IF
!
            IF (LBC(ibry,isUvel,ng)%nudging.or.                         &
     &          LBC(ibry,isVvel,ng)%nudging) THEN
              M3obc_out(ng,ibry)=M3nudg(ng)
              M3obc_in (ng,ibry)=obcfac(ng)*M3nudg(ng)
            END IF
!
            DO itrc=1,NT(ng)
              IF (LBC(ibry,isTvar(itrc),ng)%nudging) THEN
                Tobc_out(itrc,ng,ibry)=Tnudg(itrc,ng)
                Tobc_in (itrc,ng,ibry)=obcfac(ng)*Tnudg(itrc,ng)
              END IF
            END DO
          END DO
        END IF
!
!  Convert momentum stresses and tracer flux scales to kinematic
!  Values. Recall, that all the model fluxes are kinematic.
!
        cff=1.0_r8/rho0
        Fscale(idUsms,ng)=cff*Fscale(idUsms,ng)
        Fscale(idVsms,ng)=cff*Fscale(idVsms,ng)
        Fscale(idUbms,ng)=cff*Fscale(idUbms,ng)
        Fscale(idVbms,ng)=cff*Fscale(idVbms,ng)
        Fscale(idUbrs,ng)=cff*Fscale(idUbrs,ng)
        Fscale(idVbrs,ng)=cff*Fscale(idVbrs,ng)
        Fscale(idUbws,ng)=cff*Fscale(idUbws,ng)
        Fscale(idVbws,ng)=cff*Fscale(idVbws,ng)
        Fscale(idUbcs,ng)=cff*Fscale(idUbcs,ng)
        Fscale(idVbcs,ng)=cff*Fscale(idVbcs,ng)
        cff=1.0_r8/(rho0*Cp)
        Fscale(idTsur(itemp),ng)=cff*Fscale(idTsur(itemp),ng)
        Fscale(idTbot(itemp),ng)=cff*Fscale(idTbot(itemp),ng)
        Fscale(idSrad,ng)=cff*Fscale(idSrad,ng)
        Fscale(idLdwn,ng)=cff*Fscale(idLdwn,ng)
        Fscale(idLrad,ng)=cff*Fscale(idLrad,ng)
        Fscale(idLhea,ng)=cff*Fscale(idLhea,ng)
        Fscale(idShea,ng)=cff*Fscale(idShea,ng)
        Fscale(iddQdT,ng)=cff*Fscale(iddQdT,ng)
!
!  Determine the number of climatology tracers to process.
!
        IF (ANY(LtracerCLM(:,ng)).or.ANY(LnudgeTCLM(:,ng))) THEN
          ic=0
          DO itrc=1,NT(ng)
            IF (LtracerCLM(itrc,ng)) THEN
              ic=ic+1
            END IF
          END DO
          NTCLM(ng)=ic
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Set climatology tracers (active and passive) metadata.  It needs to
!  be done here because information is needed from all input scripts.
!  The variable name and units are the same as the basic tracers. The
!  default time-variable name is the same as the variable name but with
!  the "_time" suffix.  Recall that other time-variables names are
!  allowed provided that the input NetCDF variable has the "time"
!  attribute with the appropriate value.
!-----------------------------------------------------------------------
!
      varid=last_varid
      IF (ANY(LtracerCLM).or.ANY(LnudgeTCLM)) THEN
        DO i=1,MT
          varid=varid+1
          IF (varid.gt.MV) THEN
            WRITE (stdout,130) MV, varid
            STOP
          END IF
          idTclm(i)=varid
          DO ng=1,Ngrids
            Fscale(varid,ng)=1.0_r8
            Iinfo(1,varid,ng)=r3dvar
          END DO
          WRITE (Vname(1,varid),'(a)')                                  &
     &          TRIM(ADJUSTL(Vname(1,idTvar(i))))
          WRITE (Vname(2,varid),'(a,a)')                                &
     &          TRIM(ADJUSTL(Vname(2,idTvar(i)))), ' climatology'
          WRITE (Vname(3,varid),'(a)')                                  &
     &          TRIM(ADJUSTL(Vname(3,idTvar(i))))
          WRITE (Vname(4,varid),'(a,a)')                                &
     &          TRIM(Vname(1,varid)), ', scalar, series'
          WRITE (Vname(5,varid),'(a,a)')                                &
     &          TRIM(ADJUSTL(Vname(1,idTvar(i)))), '_time'
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Set tracers inverse nudging coeffcients metadata.  It needs to be
!  done here because information is needed from all input scripts.
!  The variable name is the same as the basic tracer but with the
!  "_NudgeCoef" suffix.
!-----------------------------------------------------------------------
!
      DO i=1,MT
        IF (ANY(LnudgeTCLM(i,:))) THEN
          varid=varid+1
          IF (varid.gt.MV) THEN
            WRITE (stdout,140) MV, varid
 140        FORMAT (/,' INP_PAR - too small dimension ',                &
     &              'parameter, MV = ',2i5,/,15x,                       &
     &              'change file  mod_ncparam.F  and recompile.')
            STOP
          END IF
          idTnud(i)=varid
          DO ng=1,Ngrids
            Fscale(varid,ng)=1.0_r8/86400        ! default units: 1/day
            Iinfo(1,varid,ng)=r3dvar
          END DO
          WRITE (Vname(1,varid),'(a,a)')                                &
     &          TRIM(ADJUSTL(Vname(1,idTvar(i)))), '_NudgeCoef'
          WRITE (Vname(2,varid),'(a,a)')                                &
     &          TRIM(ADJUSTL(Vname(2,idTvar(i)))),                      &
     &          ', inverse nudging coefficients'
          WRITE (Vname(3,varid),'(a,1x,a)')                             &
     &          TRIM(ADJUSTL(Vname(3,idTvar(i)))), 'day-1'
          WRITE (Vname(4,varid),'(a,a)')                                &
     &        TRIM(Vname(1,varid)), ', scalar'
          WRITE (Vname(5,varid),'(a)') 'nulvar'
        ELSE
          idTnud(i)=0
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Check C-preprocessing options and definitions.
!-----------------------------------------------------------------------
!
      IF (Master.and.Lwrite) THEN
        CALL checkdefs
        FLUSH (out)
      END IF
      CALL mp_bcasti (1, model, exit_flag)
      CALL mp_bcasts (1, model, Coptions)
      IF (FoundError(exit_flag, NoError, 916, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Initialize random number sequence so we can get identical results
!  everytime that we run the same solution.
!-----------------------------------------------------------------------
!
      sequence=759
      CALL ran_seed (sequence)
!
      RETURN
      END SUBROUTINE inp_par
!
      END MODULE inp_par_mod
