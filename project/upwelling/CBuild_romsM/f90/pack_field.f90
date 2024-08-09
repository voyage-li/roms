      MODULE pack_field_mod
!
!git $Id$
!=======================================================================
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                            Hernan G. Arango   !
!=======================================================================
!                                                                      !
!  These routines pack output variables data into a 1D array. If       !
!  distributed memory, it gathers tiled data from all processors       !
!  and serializes.                                                     !
!                                                                      !
!  Then, if applicable, they extract fields at different geometry by   !
!  decimation or interpolation.                                        !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_scalars
!
      USE distribute_mod,   ONLY : mp_collect,                          &
     &                             mp_gather2d,                         &
     &                             mp_gather3d
!
      implicit none
!
      PUBLIC :: pack_field2d
      PUBLIC :: pack_field3d
      PUBLIC :: pack_field4d
!
      CONTAINS
!
      SUBROUTINE pack_field2d (ng, model, tile,                         &
     &                         gtype, ifield, tindex,                   &
     &                         LandFill, Extract_Flag,                  &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         Ascl, Adat,                              &
     &                         start, total, Npts, Awrk)
!
!=======================================================================
!                                                                      !
!  Packs 2D field data into 1D array to be written into output NetCDF  !
!  file elsewhere.                                                     !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number (integer)                        !
!     model        Calling model identifier (integer)                  !
!     tile         Domain partition (integer)                          !
!     gtype        Staggered C-grid type (integer)                     !
!     ifield       Field metadata index (integer)                      !
!     tindex       Time record index to process (integer)              !
!     LandFill     Switch to set fill value in land areas (logical)    !                                         !
!     Extract_Flag Extraction flag interpolation/decimation (integer)  ! 
!     LBi          Donor field I-dimension Lower bound (integer)       !
!     UBi          Donor field I-dimension Upper bound (integer)       !
!     LBj          Donor field J-dimension Lower bound (integer)       !
!     UBj          Donor field J-dimension Upper bound (integer)       !
!     Ascl         Factor to scale field after reading (real)          !
!     Adat         2D field to process (real)                          !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     start        Start index where the first of the data values will !
!                    be written along each dimension (integer)         !
!     total        Number of data values to be written along each      !
!                    dimension (integer)                               !
!     Npts         Number of points processed in Awrk.                 !
!     Awrk         Packed 2D field data (real)                         !
!                                                                      !
!=======================================================================
!
!  Imported variable declarations.
!
      logical, intent(in)  :: LandFill
!
      integer, intent(in)  :: ng, model, tile
      integer, intent(in)  :: gtype, ifield, tindex
      integer, intent(in)  :: Extract_Flag
      integer, intent(in)  :: LBi, UBi, LBj, UBj
      integer, intent(out) :: Npts
      integer, intent(out) :: start(:), total(:)
!
      real(dp), intent(in) :: Ascl
!
      real(r8), intent(in) :: Adat(LBi:,LBj:)
      real(r8), intent(out) :: Awrk(:)
!
!  Local variable declarations.
!
      integer :: i, ic, j
      integer :: Imin, Imax, Jmin, Jmax
      integer :: Isize, Jsize, IJsize, MyType
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/pack_field.F"//", pack_field2d"
!-----------------------------------------------------------------------
!  Set starting and ending indices to process.
!-----------------------------------------------------------------------
!
!  Set first and last grid point according to staggered C-grid
!  classification. Set loops offsets.
!
      MyType=gtype
!
      SELECT CASE (ABS(MyType))
!
!       ---------------------
        CASE (p2dvar, p3dvar)               ! PSI points field
!       ---------------------
!
          IF (Extract_Flag.ge.0) THEN
            Imin=IOBOUNDS(ng)%ILB_psi
            Imax=IOBOUNDS(ng)%IUB_psi
            Jmin=IOBOUNDS(ng)%JLB_psi
            Jmax=IOBOUNDS(ng)%JUB_psi
          END IF
!
!       ---------------------
        CASE (r2dvar, r3dvar)               ! RHO points field
!       ---------------------
!
          IF (Extract_Flag.ge.0) THEN
            Imin=IOBOUNDS(ng)%ILB_rho
            Imax=IOBOUNDS(ng)%IUB_rho
            Jmin=IOBOUNDS(ng)%JLB_rho
            Jmax=IOBOUNDS(ng)%JUB_rho
          END IF
!
!       ---------------------
        CASE (u2dvar, u3dvar)               ! U points field
!       ---------------------
!
          IF (Extract_Flag.ge.0) THEN
            Imin=IOBOUNDS(ng)%ILB_u
            Imax=IOBOUNDS(ng)%IUB_u
            Jmin=IOBOUNDS(ng)%JLB_u
            Jmax=IOBOUNDS(ng)%JUB_u
          END IF
!
!       ---------------------
        CASE (v2dvar, v3dvar)               ! V points field
!       ---------------------
!
          IF (Extract_Flag.ge.0) THEN
            Imin=IOBOUNDS(ng)%ILB_v
            Imax=IOBOUNDS(ng)%IUB_v
            Jmin=IOBOUNDS(ng)%JLB_v
            Jmax=IOBOUNDS(ng)%JUB_v
          END IF
!
!       ---------------------
        CASE DEFAULT                        ! RHO points field
!       ---------------------
!
          IF (Extract_Flag.ge.0) THEN
            Imin=IOBOUNDS(ng)%ILB_rho
            Imax=IOBOUNDS(ng)%IUB_rho
            Jmin=IOBOUNDS(ng)%JLB_rho
            Jmax=IOBOUNDS(ng)%JUB_rho
          END IF
      END SELECT
!
      Isize=Imax-Imin+1
      Jsize=Jmax-Jmin+1
      IJsize=Isize*Jsize
!
!-----------------------------------------------------------------------
!  If distributed-memory set-up, collect tile data from all spawned
!  nodes and store it into a global scratch 1D array, packed in column-
!  major order.
!-----------------------------------------------------------------------
!
      IF (Extract_Flag.ge.0) THEN
        CALL mp_gather2d (ng, model, LBi, UBi, LBj, UBj,                &
     &                    tindex, gtype, Ascl,                          &
     &                    Adat, Npts, Awrk, LandFill)
      END IF
!
!-----------------------------------------------------------------------
!  If there is no extracting data, set the start and total vectors
!  needed by the NetCDF library for writing.
!-----------------------------------------------------------------------
!
      IF (Extract_Flag.le.0) THEN
        IF (gtype.gt.0) THEN
          start(1)=1
          total(1)=Isize
          start(2)=1
          total(2)=Jsize
          start(3)=tindex
          total(3)=1
        END IF
      END IF
!
      RETURN
      END SUBROUTINE pack_field2d
!
      SUBROUTINE pack_field3d (ng, model, tile,                         &
     &                         gtype, ifield, tindex,                   &
     &                         LandFill, Extract_Flag,                  &
     &                         LBi, UBi, LBj, UBj, LBk, UBk,            &
     &                         Ascl, Adat,                              &
     &                         start, total, Npts, Awrk)
!
!=======================================================================
!                                                                      !
!  Packs 3D field data into 1D array to be written into output NetCDF  !
!  file elsewhere.                                                     !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number (integer)                        !
!     model        Calling model identifier (integer)                  !
!     tile         Domain partition (integer)                          !
!     gtype        Staggered C-grid type (integer)                     !
!     ifield       Field metadata index (integer)                      !
!     tindex       Time record index to process (integer)              !
!     LandFill     Switch to set fill value in land areas (logical)    !
!     Extract_Flag Extraction flag interpolation/decimation (integer)  ! 
!     LBi          Field I-dimension Lower bound (integer)             !
!     UBi          Field I-dimension Upper bound (integer)             !
!     LBj          Field J-dimension Lower bound (integer)             !
!     UBj          Field J-dimension Upper bound (integer)             !
!     LBk          Field K-dimension Lower bound (integer)             !
!     UBk          Field K-dimension Upper bound (integer)             !
!     Ascl         Factor to scale field after reading (real)          !
!     Adat         3D field to process (real)                          !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     start        Start index where the first of the data values will !
!                    be written along each dimension (integer)         !
!     total        Number of data values to be written along each      !
!                    dimension (integer)                               !
!     Npts         Number of points processed in Awrk.                 !
!     Awrk         Packed 3D field data (real)                         !
!                                                                      !
!=======================================================================
!
!  Imported variable declarations.
!
      logical, intent(in)  :: LandFill
!
      integer, intent(in)  :: ng, model, tile
      integer, intent(in)  :: gtype, ifield, tindex
      integer, intent(in)  :: Extract_Flag
      integer, intent(in)  :: LBi, UBi, LBj, UBj, LBk, UBk
      integer, intent(out) :: Npts
      integer, intent(out) :: start(:), total(:)
!
      real(dp), intent(in) :: Ascl
!
      real(r8), intent(in) :: Adat(LBi:,LBj:,LBk:)
      real(r8), intent(out) :: Awrk(:)
!
!  Local variable declarations.
!
      integer :: i, j, k, ic
      integer :: Imin, Imax, Jmin, Jmax, Koff
      integer :: Isize, Jsize, Ksize, IJsize, MyType
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/pack_field.F"//", pack_field3d"
!
!------------------------------------------------------------------------
!  Pack 3D field data.
!------------------------------------------------------------------------
!
!  Set first and last grid point according to staggered C-grid
!  classification. Set loops offsets.
!
      MyType=gtype
!
      SELECT CASE (ABS(MyType))
!
!       ---------------------
        CASE (p3dvar)                       ! PSI points field
!       ---------------------
!
          Imin=IOBOUNDS(ng)%ILB_psi
          Imax=IOBOUNDS(ng)%IUB_psi
          Jmin=IOBOUNDS(ng)%JLB_psi
          Jmax=IOBOUNDS(ng)%JUB_psi
!
!       ---------------------
        CASE (r3dvar)                       ! RHO points field
!       ---------------------
!
          Imin=IOBOUNDS(ng)%ILB_rho
          Imax=IOBOUNDS(ng)%IUB_rho
          Jmin=IOBOUNDS(ng)%JLB_rho
          Jmax=IOBOUNDS(ng)%JUB_rho
!
!       ---------------------
        CASE (u3dvar)                       ! U points field
!       ---------------------
!
          Imin=IOBOUNDS(ng)%ILB_u
          Imax=IOBOUNDS(ng)%IUB_u
          Jmin=IOBOUNDS(ng)%JLB_u
          Jmax=IOBOUNDS(ng)%JUB_u
!
!       ---------------------
        CASE (v3dvar)                       ! V points field
!       ---------------------
!
          Imin=IOBOUNDS(ng)%ILB_v
          Imax=IOBOUNDS(ng)%IUB_v
          Jmin=IOBOUNDS(ng)%JLB_v
          Jmax=IOBOUNDS(ng)%JUB_v
!
!       ---------------------
        CASE DEFAULT                        ! RHO points field
!       ---------------------
!
          Imin=IOBOUNDS(ng)%ILB_rho
          Imax=IOBOUNDS(ng)%IUB_rho
          Jmin=IOBOUNDS(ng)%JLB_rho
          Jmax=IOBOUNDS(ng)%JUB_rho
      END SELECT
!
      Isize=Imax-Imin+1
      Jsize=Jmax-Jmin+1
      Ksize=UBk-LBk+1
      IJsize=Isize*Jsize
!
      IF (LBk.eq.0) THEN
        Koff=0
      ELSE
        Koff=1
      END IF
!
!-----------------------------------------------------------------------
!  If distributed-memory set-up, collect tile data from all spawned
!  nodes and store it into a global scratch 1D array, packed in column-
!  major order.
!-----------------------------------------------------------------------
!
      CALL mp_gather3d (ng, model, LBi, UBi, LBj, UBj, LBk, UBk,        &
     &                  tindex, gtype, Ascl,                            &
     &                  Adat, Npts, Awrk, LandFill)
!
!-----------------------------------------------------------------------
!  If there is no extracting data, set the start and total vectors
!  needed by the NetCDF library for writing.
!-----------------------------------------------------------------------
!
      IF (Extract_Flag.le.0) THEN
        IF (gtype.gt.0) THEN
          start(1)=1
          total(1)=Isize
          start(2)=1
          total(2)=Jsize
          start(3)=1
          total(3)=Ksize
          start(4)=tindex
          total(4)=1
        END IF
      END IF
!
      RETURN
      END SUBROUTINE pack_field3d
!
      SUBROUTINE pack_field4d (ng, model, tile,                         &
     &                         gtype, ifield, tindex,                   &
     &                         LandFill, Extract_Flag,                  &
     &                         LBi, UBi, LBj, UBj, LBk, UBk, LBt, UBt,  &
     &                         fourth,                                  &
     &                         Ascl, Adat,                              &
     &                         start, total, Npts, Awrk)
!
!=======================================================================
!                                                                      !
!  Packs 4D field data into 1D array to be written into output NetCDF  !
!  file elsewhere. The field data is processed by 3D slices to reduce  !
!  memory requirements.                                                !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number (integer)                        !
!     model        Calling model identifier (integer)                  !
!     tile         Domain partition (integer)                          !
!     gtype        Staggered C-grid type (integer)                     !
!     ifield       Field metadata index (integer)                      !
!     tindex       Time record index to process (integer)              !
!     LandFill     Switch to set fill value in land areas (logical)    !
!     Extract_Flag Extraction flag interpolation/decimation (integer)  ! 
!     LBi          Field I-dimension Lower bound (integer)             !
!     UBi          Field I-dimension Upper bound (integer)             !
!     LBj          Field J-dimension Lower bound (integer)             !
!     UBj          Field J-dimension Upper bound (integer)             !
!     LBk          Field K-dimension Lower bound (integer)             !
!     UBk          Field K-dimension Upper bound (integer)             !
!     LBt          Field fourth-dimension Lower bound (integer)        !
!     UBt          Field fourth-dimension Upper bound (integer)        !
!     fourth       Fourth dimension index to process (integer)         !
!     Ascl         Factor to scale field after reading (real)          !
!     Adat         4D field to process (real)                          !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     start        Start index where the first of the data values will !
!                    be written along each dimension (integer)         !
!     total        Number of data values to be written along each      !
!                    dimension (integer)                               !
!     Npts         Number of points processed in Awrk.                 !
!     Awrk         Packed 4D field data (real)                         !
!                                                                      !
!=======================================================================
!
!  Imported variable declarations.
!
      logical, intent(in)  :: LandFill
!
      integer, intent(in)  :: ng, model, tile
      integer, intent(in)  :: gtype, ifield, tindex
      integer, intent(in)  :: Extract_Flag
      integer, intent(in)  :: LBi, UBi, LBj, UBj, LBk, UBk, LBt, UBt
      integer, intent(in)  :: fourth
      integer, intent(out) :: Npts
      integer, intent(out) :: start(:), total(:)
!
      real(dp), intent(in) :: Ascl
!
      real(r8), intent(in) :: Adat(LBi:,LBj:,LBk:,LBt:)
      real(r8), intent(out) :: Awrk(:)
!
!  Local variable declarations.
!
      integer :: i, j, k, ic
      integer :: Imin, Imax, Jmin, Jmax, Kmin, Kmax, Koff, Loff
      integer :: Isize, Jsize, Ksize, IJsize, MyType
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/pack_field.F"//", pack_field4d"
!
!------------------------------------------------------------------------
!  Pack 4D field data by 3D slices.
!------------------------------------------------------------------------
!
!  Set first and last grid point according to staggered C-grid
!  classification. Set loops offsets.
!
      MyType=gtype
!
      SELECT CASE (ABS(MyType))
!
!       ---------------------
        CASE (p2dvar, p3dvar)               ! PSI points field
!       ---------------------
!
          Imin=IOBOUNDS(ng)%ILB_psi
          Imax=IOBOUNDS(ng)%IUB_psi
          Jmin=IOBOUNDS(ng)%JLB_psi
          Jmax=IOBOUNDS(ng)%JUB_psi
!
!       ---------------------
        CASE (r2dvar, r3dvar)               ! RHO points field
!       ---------------------
!
          Imin=IOBOUNDS(ng)%ILB_rho
          Imax=IOBOUNDS(ng)%IUB_rho
          Jmin=IOBOUNDS(ng)%JLB_rho
          Jmax=IOBOUNDS(ng)%JUB_rho
!
!       ---------------------
        CASE (u2dvar, u3dvar)               ! U points field
!       ---------------------
!
          Imin=IOBOUNDS(ng)%ILB_u
          Imax=IOBOUNDS(ng)%IUB_u
          Jmin=IOBOUNDS(ng)%JLB_u
          Jmax=IOBOUNDS(ng)%JUB_u
!
!       ---------------------
        CASE (v2dvar, v3dvar)               ! V points field
!       ---------------------
!
          Imin=IOBOUNDS(ng)%ILB_v
          Imax=IOBOUNDS(ng)%IUB_v
          Jmin=IOBOUNDS(ng)%JLB_v
          Jmax=IOBOUNDS(ng)%JUB_v
!
!       ---------------------
        CASE DEFAULT                        ! RHO points field
!       ---------------------
!
          Imin=IOBOUNDS(ng)%ILB_rho
          Imax=IOBOUNDS(ng)%IUB_rho
          Jmin=IOBOUNDS(ng)%JLB_rho
          Jmax=IOBOUNDS(ng)%JUB_rho
      END SELECT
!
      Isize=Imax-Imin+1
      Jsize=Jmax-Jmin+1
      Ksize=UBk-LBk+1
      IJsize=Isize*Jsize
!
      IF (LBk.eq.0) THEN
        Koff=0
      ELSE
        Koff=1
      END IF
!
      IF (LBt.eq.0) THEN
        Loff=1
      ELSE
        Loff=0
      END IF
!
!-----------------------------------------------------------------------
!  If distributed-memory set-up, collect tile data from all spawned
!  nodes and store it into a global scratch 1D array, packed in column-
!  major order.
!-----------------------------------------------------------------------
!
!  Process the data by 3D slices.
!
      CALL mp_gather3d (ng, model, LBi, UBi, LBj, UBj, LBk, UBk,        &
     &                  tindex, gtype, Ascl,                            &
     &                  Adat(:,:,:,fourth), Npts, Awrk, LandFill)
!
!-----------------------------------------------------------------------
!  If there is no extracting data, set the start and total vectors
!  needed by the NetCDF library for writing.
!-----------------------------------------------------------------------
!
      IF (Extract_Flag.le.0) THEN
        IF (gtype.gt.0) THEN
          start(1)=1
          total(1)=Isize
          start(2)=1
          total(2)=Jsize
          start(3)=1
          total(3)=Ksize
          start(4)=fourth+Loff
          total(4)=1
          start(5)=tindex
          total(5)=1
        END IF
      END IF
!
      RETURN
      END SUBROUTINE pack_field4d
!
      END MODULE pack_field_mod
