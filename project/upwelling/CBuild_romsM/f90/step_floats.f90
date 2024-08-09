      MODULE step_floats_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group        John M. Klinck   !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This routine time-steps  simulated  floats  trajectories using a    !
!  fourth-order Milne predictor and fourth-order Hamming corrector.    !
!                                                                      !
!  Vertical diffusion is optionally represented by a random walk,      !
!  in which case a forward scheme is used for vertical displacement.   !
!  The probability distribution for the vertical displacement is       !
!  Gaussian and includes a correction for the vertical gradient in     !
!  diffusion coefficient                                               !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: step_floats
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE step_floats (ng, Lstr, Lend)
!***********************************************************************
!
      USE mod_param
      USE mod_floats
      USE mod_stepping
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, Lstr, Lend
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Nonlinear/step_floats.F"
!
      CALL wclock_on (ng, iNLM, 10, 59, MyFile)
      CALL step_floats_tile (ng, Lstr, Lend,                            &
     &                       knew(ng), nnew(ng), nfm3(ng), nfm2(ng),    &
     &                       nfm1(ng), nf(ng), nfp1(ng),                &
     &                       DRIFTER(ng) % bounded,                     &
     &                       DRIFTER(ng) % Ftype,                       &
     &                       DRIFTER(ng) % Tinfo,                       &
     &                       DRIFTER(ng) % Fz0,                         &
     &                       DRIFTER(ng) % track)
      CALL wclock_off (ng, iNLM, 10, 73, MyFile)
!
      RETURN
      END SUBROUTINE step_floats
!
!***********************************************************************
      SUBROUTINE step_floats_tile (ng, Lstr, Lend,                      &
     &                             knew, nnew,                          &
     &                             nfm3, nfm2, nfm1, nf, nfp1,          &
     &                             bounded, Ftype, Tinfo, Fz0,          &
     &                             track)
!***********************************************************************
!
      USE mod_param
      USE mod_parallel
      USE mod_floats
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_ocean
      USE mod_scalars
!
      USE distribute_mod, ONLY : mp_collect
      USE interp_floats_mod
      USE vwalk_floats_mod, ONLY : vwalk_floats
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, Lstr, Lend
      integer, intent(in) :: knew, nnew, nfm3, nfm2, nfm1, nf, nfp1
!
      integer, intent(in) :: Ftype(:)
      real(r8), intent(in) :: Tinfo(0:,:)
      real(r8), intent(in) :: Fz0(:)
      logical, intent(inout) :: bounded(:)
      real(r8), intent(inout) :: track(:,0:,:)
!
!  Local variable declarations.
!
      logical, parameter :: Gmask = .FALSE.
      logical, parameter :: Lmask = .FALSE.
      logical, dimension(Lstr:Lend) :: my_thread
      integer :: LBi, UBi, LBj, UBj
      integer :: Ir, Jr, Npts, i, i1, i2, j, j1, j2, itrc, l, k
      real(r8), parameter :: Fspv = 0.0_r8
      real(r8) :: cff1, cff2, cff3, cff4, cff5, cff6, cff7, cff8, cff9
      real(r8) :: oHz, p1, p2, q1, q2, xrhs, yrhs, zrhs, zfloat
      real(r8) :: HalfDT
      real(r8), dimension(Lstr:Lend) :: nudg
      real(r8) :: Xstr, Xend, Ystr, Yend
      real(r8), dimension(Nfloats(ng)*NFV(ng)*(NFT+1)) :: Fwrk
!
!  Set tile array bounds.
!
      LBi=LBOUND(GRID(ng)%h,DIM=1)
      UBi=UBOUND(GRID(ng)%h,DIM=1)
      LBj=LBOUND(GRID(ng)%h,DIM=2)
      UBj=UBOUND(GRID(ng)%h,DIM=2)
!
!-----------------------------------------------------------------------
!  In distributed-memory configuration, determine which node bounds the
!  current location of the floats. Assign unbounded floats to the master
!  node.
!-----------------------------------------------------------------------
!
!  The strategy here is to build a switch that processes only the floats
!  contained within the tile node. The trajectory data for unbounded
!  floats is initialized to Fspv. These values are used during the
!  collection step at the end of the routine.  Since a SUM reduction is
!  carried-out, setting Fspv to zero means the floats only contribute in
!  their own tile.
!
      Npts=NFV(ng)*(NFT+1)*Nfloats(ng)
      Xstr=REAL(BOUNDS(ng)%Istr(MyRank),r8)-0.5_r8
      Xend=REAL(BOUNDS(ng)%Iend(MyRank),r8)+0.5_r8
      Ystr=REAL(BOUNDS(ng)%Jstr(MyRank),r8)-0.5_r8
      Yend=REAL(BOUNDS(ng)%Jend(MyRank),r8)+0.5_r8
      DO l=Lstr,Lend
        my_thread(l)=.FALSE.
        IF ((Xstr.le.track(ixgrd,nf,l)).and.                            &
     &      (track(ixgrd,nf,l).lt.Xend).and.                            &
     &      (Ystr.le.track(iygrd,nf,l)).and.                            &
     &      (track(iygrd,nf,l).lt.Yend)) THEN
          my_thread(l)=.TRUE.
        ELSE IF (Master.and.(.not.bounded(l))) THEN
          my_thread(l)=.TRUE.
        ELSE
          DO j=0,NFT
            DO i=1,NFV(ng)
              track(i,j,l)=Fspv
            END DO
          END DO
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Compute vertical positions due to vertical random walk, predictor
!  step.
!-----------------------------------------------------------------------
!
      CALL vwalk_floats (ng, Lstr, Lend, .TRUE., my_thread, nudg)
!
!-----------------------------------------------------------------------
!  Predictor step: compute first guess floats locations using a
!                  4th-order Milne time-stepping scheme.
!-----------------------------------------------------------------------
!
      cff1=8.0_r8/3.0_r8
      cff2=4.0_r8/3.0_r8
      DO l=Lstr,Lend
        IF (my_thread(l).and.bounded(l)) THEN
          track(ixgrd,nfp1,l)=track(ixgrd,nfm3,l)+                      &
     &                        dt(ng)*(cff1*track(ixrhs,nf  ,l)-         &
     &                                cff2*track(ixrhs,nfm1,l)+         &
     &                                cff1*track(ixrhs,nfm2,l))
          track(iygrd,nfp1,l)=track(iygrd,nfm3,l)+                      &
     &                        dt(ng)*(cff1*track(iyrhs,nf  ,l)-         &
     &                                cff2*track(iyrhs,nfm1,l)+         &
     &                                cff1*track(iyrhs,nfm2,l))
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Calculate slopes at new time-step.
!-----------------------------------------------------------------------
!
      CALL interp_floats (ng, LBi, UBi, LBj, UBj, 1, N(ng),             &
     &                    Lstr, Lend, nfp1, ixrhs, isUvel,              &
     &                    -u3dvar, Lmask, spval, nudg,                  &
     &                    GRID(ng) % pm,                                &
     &                    GRID(ng) % pn,                                &
     &                    GRID(ng) % Hz,                                &
     &                    OCEAN(ng) % u(:,:,:,nnew),                    &
     &                    my_thread, bounded, track)
      CALL interp_floats (ng, LBi, UBi, LBj, UBj, 1, N(ng),             &
     &                    Lstr, Lend, nfp1, iyrhs, isVvel,              &
     &                    -v3dvar, Lmask, spval, nudg,                  &
     &                    GRID(ng) % pm,                                &
     &                    GRID(ng) % pn,                                &
     &                    GRID(ng) % Hz,                                &
     &                    OCEAN(ng) % v(:,:,:,nnew),                    &
     &                    my_thread, bounded, track)
!
!-----------------------------------------------------------------------
!  Corrector step: correct floats locations using a 4th-order
!                  Hamming time-stepping scheme.
!-----------------------------------------------------------------------
!
      cff1=9.0_r8/8.0_r8
      cff2=1.0_r8/8.0_r8
      cff3=3.0_r8/8.0_r8
      cff4=6.0_r8/8.0_r8
      DO l=Lstr,Lend
        IF (my_thread(l).and.bounded(l)) THEN
          track(ixgrd,nfp1,l)=cff1*track(ixgrd,nf  ,l)-                 &
     &                        cff2*track(ixgrd,nfm2,l)+                 &
     &                        dt(ng)*(cff3*track(ixrhs,nfp1,l)+         &
     &                                cff4*track(ixrhs,nf  ,l)-         &
     &                                cff3*track(ixrhs,nfm1,l))
          track(iygrd,nfp1,l)=cff1*track(iygrd,nf  ,l)-                 &
     &                        cff2*track(iygrd,nfm2,l)+                 &
     &                        dt(ng)*(cff3*track(iyrhs,nfp1,l)+         &
     &                                cff4*track(iyrhs,nf  ,l)-         &
     &                                cff3*track(iyrhs,nfm1,l))
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Determine floats status.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng)) THEN
        cff1=REAL(Lm(ng),r8)
        DO l=Lstr,Lend
          IF (my_thread(l).and.bounded(l)) THEN
            IF (track(ixgrd,nfp1,l).ge.REAL(Lm(ng)+1,r8)-0.5_r8) THEN
              track(ixgrd,nfp1,l)=track(ixgrd,nfp1,l)-cff1
              track(ixgrd,nf  ,l)=track(ixgrd,nf  ,l)-cff1
              track(ixgrd,nfm1,l)=track(ixgrd,nfm1,l)-cff1
              track(ixgrd,nfm2,l)=track(ixgrd,nfm2,l)-cff1
              track(ixgrd,nfm3,l)=track(ixgrd,nfm3,l)-cff1
            ELSE IF (track(ixgrd,nfp1,l).lt.0.5_r8) THEN
              track(ixgrd,nfp1,l)=cff1+track(ixgrd,nfp1,l)
              track(ixgrd,nf  ,l)=cff1+track(ixgrd,nf  ,l)
              track(ixgrd,nfm1,l)=cff1+track(ixgrd,nfm1,l)
              track(ixgrd,nfm2,l)=cff1+track(ixgrd,nfm2,l)
              track(ixgrd,nfm3,l)=cff1+track(ixgrd,nfm3,l)
            END IF
          END IF
        END DO
        IF (NtileI(ng).gt.1) THEN
          Fwrk=RESHAPE(track,(/Npts/))
          CALL mp_collect (ng, iNLM, Npts, Fspv, Fwrk)
          track=RESHAPE(Fwrk,(/NFV(ng),NFT+1,Nfloats(ng)/))
          DO l=Lstr,Lend
            IF ((Xstr.le.track(ixgrd,nfp1,l)).and.                      &
     &          (track(ixgrd,nfp1,l).lt.Xend).and.                      &
     &          (Ystr.le.track(iygrd,nfp1,l)).and.                      &
     &          (track(iygrd,nfp1,l).lt.Yend)) THEN
              my_thread(l)=.TRUE.
            ELSE IF (Master.and.(.not.bounded(l))) THEN
              my_thread(l)=.TRUE.
            ELSE
              my_thread(l)=.FALSE.
              DO j=0,NFT
                DO i=1,NFV(ng)
                  track(i,j,l)=Fspv
                END DO
              END DO
            END IF
          END DO
        END IF
      ELSE
        DO l=Lstr,Lend
          IF (my_thread(l).and.bounded(l)) THEN
            IF ((track(ixgrd,nfp1,l).ge.REAL(Lm(ng)+1,r8)-0.5_r8).or.   &
     &          (track(ixgrd,nfp1,l).lt.0.5_r8)) THEN
              bounded(l)=.FALSE.
            END IF
          END IF
        END DO
      END IF
!
      IF (NSperiodic(ng)) THEN
        cff1=REAL(Mm(ng),r8)
        DO l=Lstr,Lend
          IF (my_thread(l).and.bounded(l)) THEN
            IF (track(iygrd,nfp1,l).ge.REAL(Mm(ng)+1,r8)-0.5_r8) THEN
              track(iygrd,nfp1,l)=track(iygrd,nfp1,l)-cff1
              track(iygrd,nf  ,l)=track(iygrd,nf  ,l)-cff1
              track(iygrd,nfm1,l)=track(iygrd,nfm1,l)-cff1
              track(iygrd,nfm2,l)=track(iygrd,nfm2,l)-cff1
              track(iygrd,nfm3,l)=track(iygrd,nfm3,l)-cff1
            ELSE IF (track(iygrd,nfp1,l).lt.0.5_r8) THEN
              track(iygrd,nfp1,l)=cff1+track(iygrd,nfp1,l)
              track(iygrd,nf  ,l)=cff1+track(iygrd,nf  ,l)
              track(iygrd,nfm1,l)=cff1+track(iygrd,nfm1,l)
              track(iygrd,nfm2,l)=cff1+track(iygrd,nfm2,l)
              track(iygrd,nfm3,l)=cff1+track(iygrd,nfm3,l)
            END IF
          END IF
        END DO
        IF (NtileJ(ng).gt.1) THEN
          Fwrk=RESHAPE(track,(/Npts/))
          CALL mp_collect (ng, iNLM, Npts, Fspv, Fwrk)
          track=RESHAPE(Fwrk,(/NFV(ng),NFT+1,Nfloats(ng)/))
          DO l=Lstr,Lend
            IF ((Xstr.le.track(ixgrd,nfp1,l)).and.                      &
     &          (track(ixgrd,nfp1,l).lt.Xend).and.                      &
     &          (Ystr.le.track(iygrd,nfp1,l)).and.                      &
     &          (track(iygrd,nfp1,l).lt.Yend)) THEN
              my_thread(l)=.TRUE.
            ELSE IF (Master.and.(.not.bounded(l))) THEN
              my_thread(l)=.TRUE.
            ELSE
              my_thread(l)=.FALSE.
              DO j=0,NFT
                DO i=1,NFV(ng)
                  track(i,j,l)=Fspv
                END DO
              END DO
            END IF
          END DO
        END IF
      ELSE
        DO l=Lstr,Lend
          IF (my_thread(l).and.bounded(l)) THEN
            IF ((track(iygrd,nfp1,l).ge.REAL(Mm(ng)+1,r8)-0.5_r8).or.   &
     &          (track(iygrd,nfp1,l).lt.0.5_r8)) THEN
              bounded(l)=.FALSE.
            END IF
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  If appropriate, activate the release of new floats and set initial
!  positions for all time levels.
!-----------------------------------------------------------------------
!
      HalfDT=0.5_r8*dt(ng)
      DO l=Lstr,Lend
        IF (.not.bounded(l).and.                                        &
     &      (time(ng)-HalfDT.le.Tinfo(itstr,l).and.                     &
     &       time(ng)+HalfDT.gt.Tinfo(itstr,l))) THEN
          bounded(l)=.TRUE.
          IF ((Tinfo(ixgrd,l).lt.0.5_r8).or.                            &
     &        (Tinfo(iygrd,l).lt.0.5_r8).or.                            &
     &        (Tinfo(ixgrd,l).gt.REAL(Lm(ng),r8)+0.5_r8).or.            &
     &        (Tinfo(iygrd,l).gt.REAL(Mm(ng),r8)+0.5_r8)) THEN
            bounded(l)=.FALSE.               ! outside application grid
          END IF
          IF ((Xstr.le.Tinfo(ixgrd,l)).and.                             &
     &        (Tinfo(ixgrd,l).lt.Xend).and.                             &
     &        (Ystr.le.Tinfo(iygrd,l)).and.                             &
     &        (Tinfo(iygrd,l).lt.Yend).and.bounded(l)) THEN
            DO j=0,NFT
              track(ixgrd,j,l)=Tinfo(ixgrd,l)
              track(iygrd,j,l)=Tinfo(iygrd,l)
              track(izgrd,j,l)=Tinfo(izgrd,l)
            END DO
            my_thread(l)=.TRUE.
          ELSE
            my_thread(l)=.FALSE.
            DO j=0,NFT
              DO i=1,NFV(ng)
                track(i,j,l)=Fspv
              END DO
            END DO
          END IF
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Calculate slopes with corrected locations.
!-----------------------------------------------------------------------
!
      CALL interp_floats (ng, LBi, UBi, LBj, UBj, 1, N(ng),             &
     &                    Lstr, Lend, nfp1, ixrhs, isUvel,              &
     &                    -u3dvar, Lmask, spval, nudg,                  &
     &                    GRID(ng) % pm,                                &
     &                    GRID(ng) % pn,                                &
     &                    GRID(ng) % Hz,                                &
     &                    OCEAN(ng) % u(:,:,:,nnew),                    &
     &                    my_thread, bounded, track)
      CALL interp_floats (ng, LBi, UBi, LBj, UBj, 1, N(ng),             &
     &                    Lstr, Lend, nfp1, iyrhs, isVvel,              &
     &                    -v3dvar, Lmask, spval, nudg,                  &
     &                    GRID(ng) % pm,                                &
     &                    GRID(ng) % pn,                                &
     &                    GRID(ng) % Hz,                                &
     &                    OCEAN(ng) % v(:,:,:,nnew),                    &
     &                    my_thread, bounded, track)
!
!  If newly released floats, initialize slopes at all time levels.
!
      DO l=Lstr,Lend
        IF (my_thread(l).and.bounded(l).and.                            &
     &      (time(ng)-HalfDT.le.Tinfo(itstr,l).and.                     &
     &       time(ng)+HalfDT.gt.Tinfo(itstr,l))) THEN
          xrhs=track(ixrhs,nfp1,l)
          yrhs=track(iyrhs,nfp1,l)
          zrhs=track(izrhs,nfp1,l)
          DO i=0,NFT
            track(ixrhs,i,l)=xrhs
            track(iyrhs,i,l)=yrhs
            track(izrhs,i,l)=zrhs
          END DO
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Interpolate various output variables at the corrected locations.
!-----------------------------------------------------------------------
!
      IF (spherical) THEN
        CALL interp_floats (ng, LBi, UBi, LBj, UBj, 1, 1,               &
     &                      Lstr, Lend, nfp1, iflon, isBr2d,            &
     &                      r2dvar, Gmask, spval, nudg,                 &
     &                      GRID(ng) % pm,                              &
     &                      GRID(ng) % pn,                              &
     &                      GRID(ng) % Hz,                              &
     &                      GRID(ng) % lonr,                            &
     &                      my_thread, bounded, track)
        CALL interp_floats (ng, LBi, UBi, LBj, UBj, 1, 1,               &
     &                      Lstr, Lend, nfp1, iflat, isBr2d,            &
     &                      r2dvar, Gmask, spval, nudg,                 &
     &                      GRID(ng) % pm,                              &
     &                      GRID(ng) % pn,                              &
     &                      GRID(ng) % Hz,                              &
     &                      GRID(ng) % latr,                            &
     &                      my_thread, bounded, track)
      ELSE
        CALL interp_floats (ng, LBi, UBi, LBj, UBj, 1, 1,               &
     &                      Lstr, Lend, nfp1, iflon, isBr2d,            &
     &                      r2dvar, Gmask, spval, nudg,                 &
     &                      GRID(ng) % pm,                              &
     &                      GRID(ng) % pn,                              &
     &                      GRID(ng) % Hz,                              &
     &                      GRID(ng) % xr,                              &
     &                      my_thread, bounded, track)
        CALL interp_floats (ng, LBi, UBi, LBj, UBj, 1, 1,               &
     &                      Lstr, Lend, nfp1, iflat, isBr2d,            &
     &                      r2dvar, Gmask, spval, nudg,                 &
     &                      GRID(ng) % pm,                              &
     &                      GRID(ng) % pn,                              &
     &                      GRID(ng) % Hz,                              &
     &                      GRID(ng) % yr,                              &
     &                      my_thread, bounded, track)
      END IF
      CALL interp_floats (ng, LBi, UBi, LBj, UBj, 0, N(ng),             &
     &                    Lstr, Lend, nfp1, idpth, isBw3d,              &
     &                    w3dvar, Lmask, spval, nudg,                   &
     &                    GRID(ng) % pm,                                &
     &                    GRID(ng) % pn,                                &
     &                    GRID(ng) % Hz,                                &
     &                    GRID(ng) % z_w,                               &
     &                    my_thread, bounded, track)
      CALL interp_floats (ng, LBi, UBi, LBj, UBj, 1, N(ng),             &
     &                    Lstr, Lend, nfp1, ifden,  isBr3d,             &
     &                    r3dvar, Lmask, spval, nudg,                   &
     &                    GRID(ng) % pm,                                &
     &                    GRID(ng) % pn,                                &
     &                    GRID(ng) % Hz,                                &
     &                    OCEAN(ng) % rho,                              &
     &                    my_thread, bounded, track)
      DO itrc=1,NT(ng)
        CALL interp_floats (ng, LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                      Lstr, Lend, nfp1, ifTvar(itrc),             &
     &                      isTvar(itrc), r3dvar, Lmask, spval, nudg,   &
     &                      GRID(ng) % pm,                              &
     &                      GRID(ng) % pn,                              &
     &                      GRID(ng) % Hz,                              &
     &                      OCEAN(ng) % t(:,:,:,nnew,itrc),             &
     &                      my_thread, bounded, track)
      END DO
!
!-----------------------------------------------------------------------
!  Compute vertical positions due to vertical random walk, corrector
!  step.
!-----------------------------------------------------------------------
!
      CALL vwalk_floats (ng, Lstr, Lend, .FALSE., my_thread, nudg)
!
!  Floats that hit the surface or bottom are reflected
!
      DO l=Lstr,Lend
        IF (my_thread(l).and.bounded(l)) THEN
          IF (track(izgrd,nfp1,l).gt.REAL(N(ng),r8)) THEN
            DO j=0,NFT
              track(izgrd,j,l)=2.0_r8*REAL(N(ng),r8)-track(izgrd,j,l)
            END DO
          ELSE IF (track(izgrd,nfp1,l).lt.0.0_r8) THEN
            DO j=0,NFT
              track(izgrd,:,l)=-track(izgrd,:,l)
            END DO
          END IF
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Collect floats on all nodes.
!-----------------------------------------------------------------------
!
      Fwrk=RESHAPE(track,(/Npts/))
      CALL mp_collect (ng, iNLM, Npts, Fspv, Fwrk)
      track=RESHAPE(Fwrk,(/NFV(ng),NFT+1,Nfloats(ng)/))
!
!  Collect the bounded status switch.
!
      Fwrk=Fspv
      DO l=1,Nfloats(ng)
        IF (bounded(l)) THEN
          Fwrk(l)=1.0_r8
        END IF
      END DO
      CALL mp_collect (ng, iNLM, Nfloats(ng), Fspv, Fwrk)
      DO l=1,Nfloats(ng)
        IF (Fwrk(l).ne.Fspv) THEN
          bounded(l)=.TRUE.
        ELSE
          bounded(l)=.FALSE.
        END IF
      END DO
!
      RETURN
      END SUBROUTINE step_floats_tile
      END MODULE step_floats_mod
