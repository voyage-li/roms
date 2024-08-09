      MODULE mod_floats
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  Findex     Indices of spherical coordinates entries in initial      !
!               location arrays, if any.                               !
!  Flon       Initial longitude locations, if any.                     !
!  Flat       Initial latitude locations, if any.                      !
!  Ftype      Float trajectory type:                                   !
!               Ftype(:) = 1,  neutral density 3D Lagrangian           !
!               Ftype(:) = 2,  isobaric (constant depth) float.        !
!  Tinfo      Float trajectory initial information.                    !
!  bounded    Float bounded status switch.                             !
!  rwalk      Normally distributed random deviates used in vertical    !
!               random walk.                                           !
!  track      Multivariate float trajectory data at several time       !
!               time levels.                                           !
!                                                                      !
!=======================================================================
!
        USE mod_param
!
        implicit none
!
        PUBLIC :: allocate_floats
        PUBLIC :: deallocate_floats
!
!-----------------------------------------------------------------------
!  Define T_DRIFTER structure.
!-----------------------------------------------------------------------
!
        TYPE T_DRIFTER
          logical, pointer  :: bounded(:)
          integer, pointer :: Findex(:)
          integer, pointer :: Ftype(:)
          real(r8), pointer :: Flon(:)
          real(r8), pointer :: Flat(:)
          real(r8), pointer :: Fz0(:)
          real(r8), pointer :: Tinfo(:,:)
          real(r8), pointer :: rwalk(:)
          real(r8), pointer :: track(:,:,:)
        END TYPE T_DRIFTER
!
        TYPE (T_DRIFTER), allocatable :: DRIFTER(:)
!
!-----------------------------------------------------------------------
!  Lagrangian drifters parameters.
!-----------------------------------------------------------------------
!
!  Switch to control the printing of floats positions to standard output
!  file.
!
        logical, allocatable :: Fprint(:)
!
!  Identification indices.
!
        integer, parameter :: itstr = 0          ! release time
        integer, parameter :: ixgrd = 1          ! x-grid location
        integer, parameter :: iygrd = 2          ! y-grid location
        integer, parameter :: izgrd = 3          ! z-grid location
        integer, parameter :: iflon = 4          ! longitude location
        integer, parameter :: iflat = 5          ! latitude location
        integer, parameter :: idpth = 6          ! depth
        integer, parameter :: ixrhs = 7          ! x-slope
        integer, parameter :: iyrhs = 8          ! y-slope
        integer, parameter :: izrhs = 9          ! z-slope
        integer, parameter :: ifden = 10         ! density anomaly
        integer, parameter :: ifakt = 11         ! diffusivity, Akt
        integer, parameter :: ifdak = 12         ! d(Akt)/d(s)
!
!  Tracer variables indices in the track array.
!
        integer, allocatable :: ifTvar(:)
!
!  Set float tracjectory types:
!
!    flt_Lagran:  3D Lagrangian floats
!    flt_Isobar:  Isobaric floats, p=g*(z+zeta)=constant
!    flt_Geopot:  Geopotential floats, constant depth
!
        integer, parameter :: flt_Lagran = 1
        integer, parameter :: flt_Isobar = 2
        integer, parameter :: flt_Geopot = 3
!
!  Vertical random walk, initial seed state.
!
        integer :: flt_iseed
!
!  Floats restart switch.
!
        integer, allocatable :: frrec(:)
!
      CONTAINS
!
      SUBROUTINE allocate_floats (Ldrifter)
!
!=======================================================================
!                                                                      !
!  This routine eihter  allocates and initialize all variables in      !
!  the DRIFTER structure (Ldrifter=.TRUE.) or other parameters in      !
!  the module that are independent of Nfloats (Ldrifter=.FALSE.).      !
!                                                                      !
!=======================================================================
!
      USE mod_scalars
!
!  Imported variable declarations.
!
      logical, intent(in) :: Ldrifter
!
!  Local variable declarations.
!
      integer :: ng, i, ic, iflt
      real(r8), parameter :: IniVal = 0.0_r8
!
!-----------------------------------------------------------------------
!  Allocate Langrangian drifters structure.
!-----------------------------------------------------------------------
!
      IF (Ldrifter) THEN
        allocate ( DRIFTER(Ngrids) )
!
!  Allocate variables.
!
        DO ng=1,Ngrids
          allocate ( DRIFTER(ng) % bounded(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)
          allocate ( DRIFTER(ng) % Findex(0:Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng)+1,r8)
          allocate ( DRIFTER(ng) % Ftype(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)
          allocate ( DRIFTER(ng) % Flon(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)
          allocate ( DRIFTER(ng) % Flat(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)
          allocate ( DRIFTER(ng) % Fz0(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)
          allocate ( DRIFTER(ng) % Tinfo(0:izrhs,Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL((izrhs+1)*Nfloats(ng),r8)
          allocate ( DRIFTER(ng) % rwalk(Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(Nfloats(ng),r8)
          allocate ( DRIFTER(ng) % track(NFV(ng),0:NFT,Nfloats(ng)) )
          Dmem(ng)=Dmem(ng)+REAL(NFV(ng)*(NFT+1)*Nfloats(ng),r8)
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Lagrangian drifters parameters.
!-----------------------------------------------------------------------
!
      IF (.not.Ldrifter) THEN
        allocate ( Fprint(Ngrids) )
        allocate ( frrec(Ngrids) )
        allocate ( ifTvar(MT) )
      END IF
!
!-----------------------------------------------------------------------
!  Initialize Langrangian drifters structure.
!-----------------------------------------------------------------------
!
      IF (Ldrifter) THEN
        DO ng=1,Ngrids
          DRIFTER(ng) % Findex(0) = 0
          DO iflt=1,Nfloats(ng)
            DRIFTER(ng) % bounded(iflt) = .FALSE.
            DRIFTER(ng) % Findex(iflt) = 0
            DRIFTER(ng) % Ftype(iflt) = 0
            DRIFTER(ng) % Flon(iflt) = IniVal
            DRIFTER(ng) % Flat(iflt) = IniVal
            DRIFTER(ng) % Fz0(iflt) = 0
            DRIFTER(ng) % rwalk = IniVal
            DO i=0,izrhs
              DRIFTER(ng) % Tinfo(i,iflt) = IniVal
            END DO
            DO i=1,NFV(ng)
              DRIFTER(ng) % track(i,0,iflt) = IniVal
              DRIFTER(ng) % track(i,1,iflt) = IniVal
              DRIFTER(ng) % track(i,2,iflt) = IniVal
              DRIFTER(ng) % track(i,3,iflt) = IniVal
              DRIFTER(ng) % track(i,4,iflt) = IniVal
            END DO
          END DO
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Initialize Langrangian drifters parameters.
!-----------------------------------------------------------------------
!
      IF (.not.Ldrifter) THEN
        flt_iseed=149876
        DO ng=1,Ngrids
          Fprint(ng)=.TRUE.
        END DO
!
!  Indices for tracer variables in the floats array track.
!
        ic=12
        DO i=1,MT
          ic=ic+1
          ifTvar(i)=ic
        END DO
      END IF
!
      RETURN
      END SUBROUTINE allocate_floats
!
      SUBROUTINE deallocate_floats (ng)
!
!=======================================================================
!                                                                      !
!  This routine dealocates all variables in module for all nested      !
!  grids.                                                              !
!                                                                      !
!=======================================================================
!
      USE mod_param,   ONLY : Ngrids
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Modules/mod_floats.F"//", deallocate_floats"
!
!-----------------------------------------------------------------------
!  Deallocate T_DRIFTER structure.
!-----------------------------------------------------------------------
!
      IF (ng.eq.Ngrids) THEN
        IF (allocated(DRIFTER)) deallocate ( DRIFTER )
      END IF
!
!-----------------------------------------------------------------------
!  Deallocate other variables in module.
!-----------------------------------------------------------------------
!
      IF (allocated(Fprint))  deallocate ( Fprint )
      IF (allocated(frrec))   deallocate ( frrec )
      IF (allocated(ifTvar))  deallocate ( ifTvar )
!
      RETURN
      END SUBROUTINE deallocate_floats
      END MODULE mod_floats
