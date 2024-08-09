      MODULE nf_fread3d_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This function reads in a generic floating point 3D array from an    !
!  input NetCDF file.                                                  !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng         Nested grid number (integer)                          !
!     model      Calling model identifier (integer)                    !
!     ncname     NetCDF file name (string)                             !
!     ncid       NetCDF file ID (integer)                              !
!     ncvname    NetCDF variable name (string)                         !
!     ncvarid    NetCDF variable ID (integer)                          !
!     tindex     NetCDF time record index to read (integer)            !
!     gtype      C-grid type (integer)                                 !
!     Vsize      Variable dimensions in NetCDF file (integer 1D array) !
!     LBi        I-dimension Lower bound (integer)                     !
!     UBi        I-dimension Upper bound (integer)                     !
!     LBj        J-dimension Lower bound (integer)                     !
!     UBj        J-dimension Upper bound (integer)                     !
!     LBk        K-dimension Lower bound (integer)                     !
!     UBk        K-dimension Upper bound (integer)                     !
!     Ascl       Factor to scale field after reading (real).           !
!     Amask      Land/Sea mask, if any (real 3D array)                 !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Amin       Field minimum value (real)                            !
!     Amax       Field maximum value (real)                            !
!     Adat       Field to read in (real 3D array)                      !
!     checksum   Field checksum value (32-bit integer; OPTIONAL)       !
!     status     Result Error flag (integer)                           !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_ncparam
      USE mod_scalars
!
      USE get_hash_mod, ONLY : get_hash
      USE strings_mod,  ONLY : FoundError
!
      implicit none
!
      INTERFACE nf_fread3d
        MODULE PROCEDURE nf90_fread3d
      END INTERFACE nf_fread3d
!
      CONTAINS
!
!***********************************************************************
      FUNCTION nf90_fread3d (ng, model, ncname, ncid,                   &
     &                       ncvname, ncvarid,                          &
     &                       tindex, gtype, Vsize,                      &
     &                       LBi, UBi, LBj, UBj, LBk, UBk,              &
     &                       Ascl, Amin, Amax,                          &
     &                       Adat,                                      &
     &                       checksum) RESULT (status)
!***********************************************************************
!
      USE mod_netcdf
!
      USE distribute_mod, ONLY : mp_bcasti
      USE distribute_mod, ONLY : mp_scatter3d
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model, ncid, ncvarid, tindex, gtype
      integer, intent(in) :: LBi, UBi, LBj, UBj, LBk, UBk
      integer, intent(in) :: Vsize(4)
!
      integer(i8b), intent(out), optional :: checksum
!
      real(dp), intent(in)  :: Ascl
      real(r8), intent(out) :: Amin
      real(r8), intent(out) :: Amax
!
      character (len=*), intent(in) :: ncname
      character (len=*), intent(in) :: ncvname
!
      real(r8), intent(out) :: Adat(LBi:,LBj:,LBk:)
!
!  Local variable declarations.
!
      logical :: Lchecksum
      logical, dimension(3) :: foundit
!
      integer :: i, j, k, ic, Npts, NWpts, status, wtype
      integer :: Is, Ie, Js, Je
      integer :: Imin, Imax, Jmin, Jmax, Koff
      integer :: Ilen, Jlen, Klen, IJlen
      integer :: Cgrid, MyType, ghost
      integer :: Nghost
      integer, dimension(4) :: start, total
!
      real(r8) :: Afactor, Aoffset, Aspval
      real(r8), dimension(3) :: AttValue
!
      real(r8), allocatable :: Cwrk(:)           ! used for checksum
      real(r8), dimension(2+(Lm(ng)+2)*(Mm(ng)+2)*(UBk-LBk+1)) :: wrk
!
      character (len=12), dimension(3) :: AttName
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/nf_fread3d.F"//", nf90_fread3d"
!
!-----------------------------------------------------------------------
!  Set starting and ending indices to process.
!-----------------------------------------------------------------------
!
!  Set global (interior plus boundary) starting and ending grid cell
!  indices in the I- and J-directions according to staggered C-grid
!  classification.
!
      MyType=gtype
      SELECT CASE (ABS(MyType))
        CASE (p2dvar, p3dvar)
          Cgrid=1                                         ! PSI-points
          Is=IOBOUNDS(ng)%ILB_psi
          Ie=IOBOUNDS(ng)%IUB_psi
          Js=IOBOUNDS(ng)%JLB_psi
          Je=IOBOUNDS(ng)%JUB_psi
        CASE (r2dvar, r3dvar, w3dvar)
          Cgrid=2                                         ! RHO-points
          Is=IOBOUNDS(ng)%ILB_rho
          Ie=IOBOUNDS(ng)%IUB_rho
          Js=IOBOUNDS(ng)%JLB_rho
          Je=IOBOUNDS(ng)%JUB_rho
        CASE (u2dvar, u3dvar)
          Cgrid=3                                         ! U-points
          Is=IOBOUNDS(ng)%ILB_u
          Ie=IOBOUNDS(ng)%IUB_u
          Js=IOBOUNDS(ng)%JLB_u
          Je=IOBOUNDS(ng)%JUB_u
        CASE (v2dvar, v3dvar)
          Cgrid=4                                         ! V-points
          Is=IOBOUNDS(ng)%ILB_v
          Ie=IOBOUNDS(ng)%IUB_v
          Js=IOBOUNDS(ng)%JLB_v
          Je=IOBOUNDS(ng)%JUB_v
        CASE DEFAULT
          Cgrid=2                                         ! RHO-points
          Is=IOBOUNDS(ng)%ILB_rho
          Ie=IOBOUNDS(ng)%IUB_rho
          Js=IOBOUNDS(ng)%JLB_rho
          Je=IOBOUNDS(ng)%JUB_rho
      END SELECT
      Ilen=Ie-Is+1
      Jlen=Je-Js+1
      Klen=UBk-LBk+1
      IJlen=Ilen*Jlen
      IF (LBk.eq.0) THEN
        Koff=0
      ELSE
        Koff=1
      END IF
!
!  Set the tile computational I- and J-bounds (no ghost points).
!
      ghost=0
      Imin=BOUNDS(ng)%Imin(Cgrid,ghost,MyRank)
      Imax=BOUNDS(ng)%Imax(Cgrid,ghost,MyRank)
      Jmin=BOUNDS(ng)%Jmin(Cgrid,ghost,MyRank)
      Jmax=BOUNDS(ng)%Jmax(Cgrid,ghost,MyRank)
!
!  Check if the following attributes: "scale_factor", "add_offset", and
!  "_FillValue" are present in the input NetCDF variable:
!
!  If the "scale_value" attribute is present, the data is multiplied by
!  this factor after reading.
!  If the "add_offset" attribute is present, this value is added to the
!  data after reading.
!  If both "scale_factor" and "add_offset" attributes are present, the
!  data are first scaled before the offset is added.
!  If the "_FillValue" attribute is present, the data having this value
!  is treated a missing and it is replaced with zero. This feature it is
!  usually related with the land/sea masking.
!
      AttName(1)='scale_factor'
      AttName(2)='add_offset  '
      AttName(3)='_FillValue  '
      CALL netcdf_get_fatt (ng, model, ncname, ncvarid, AttName,        &
     &                      AttValue, foundit,                          &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 685, MyFile)) THEN
        status=ioerror
        RETURN
      END IF
      IF (.not.foundit(1)) THEN
        Afactor=1.0_r8
      ELSE
        Afactor=AttValue(1)
      END IF
      IF (.not.foundit(2)) THEN
        Aoffset=0.0_r8
      ELSE
        Aoffset=AttValue(2)
      END IF
      IF (.not.foundit(3)) THEN
        Aspval=spval_check
      ELSE
        Aspval=AttValue(3)
      END IF
!
!  Set the number of tile ghost points, Nghost, to scatter in
!  distributed-memory applications. If Nghost=0, the ghost points
!  are not processed.  They will be processed elsewhere by the
!  appropriate call to any of the routines in "mp_exchange.F".
!
      IF (model.eq.iADM) THEN
        Nghost=0                     ! no ghost points exchange
      ELSE
        Nghost=NghostPoints          ! do ghost points exchange
      END IF
!
!  Set NetCDF dimension counters for processing requested field.
!
      IF (MyType.gt.0) THEN
        start(1)=1
        total(1)=Ilen
        start(2)=1
        total(2)=Jlen
        start(3)=1
        total(3)=Klen
        start(4)=tindex
        total(4)=1
        Npts=IJlen
        Npts=Npts*Klen
      END IF
!
!  Initialize local array to avoid denormalized numbers. This
!  facilitates processing and debugging.
!
      wrk=0.0_r8
!
!  Initialize checsum value.
!
      IF (PRESENT(checksum)) THEN
        Lchecksum=.TRUE.
        checksum=0_i8b
      ELSE
        Lchecksum=.FALSE.
      END IF
!
!-----------------------------------------------------------------------
!  Serial I/O: Read in requested field and scale it.
!-----------------------------------------------------------------------
!
      Amin=spval
      Amax=-spval
        status=nf90_noerr
        IF (InpThread) THEN
          status=nf90_get_var(ncid, ncvarid, wrk, start, total)
          IF (status.eq.nf90_noerr) THEN
            DO i=1,Npts
              IF (ABS(wrk(i)).ge.ABS(Aspval)) THEN
                wrk(i)=0.0_r8                 ! masked with _FillValue
              ELSE
                wrk(i)=Ascl*(Afactor*wrk(i)+Aoffset)
                Amin=MIN(Amin,wrk(i))
                Amax=MAX(Amax,wrk(i))
              END IF
            END DO
            IF ((ABS(Amin).ge.ABS(Aspval)).and.                         &
     &          (ABS(Amax).ge.ABS(Aspval))) THEN
              Amin=0.0_r8                     ! the entire data is all
              Amax=0.0_r8                     ! field value, _FillValue
            END IF
          END IF
        END IF
        CALL mp_bcasti (ng, model, status)
        IF (FoundError(status, nf90_noerr, 830, MyFile)) THEN
          exit_flag=2
          ioerror=status
          RETURN
        END IF
!
!-----------------------------------------------------------------------
!  Serial I/O: Unpack read field.
!-----------------------------------------------------------------------
!
!  Scatter read data over the distributed memory tiles.
!
        CALL mp_scatter3d (ng, model, LBi, UBi, LBj, UBj, LBk, UBk,     &
     &                     Nghost, MyType, Amin, Amax,                  &
     &                     Npts, wrk, Adat)
!
!-----------------------------------------------------------------------
!  If requested, compute data checksum value.
!-----------------------------------------------------------------------
!
      IF (Lchecksum) THEN
        Npts=(Imax-Imin+1)*(Jmax-Jmin+1)*(UBk-LBk+1)
        IF (.not.allocated(Cwrk)) allocate ( Cwrk(Npts) )
        Cwrk=PACK(Adat(Imin:Imax, Jmin:Jmax, LBk:UBk), .TRUE.)
        CALL get_hash (Cwrk, Npts, checksum, .TRUE.)
        IF (allocated(Cwrk)) deallocate (Cwrk)
      END IF
!
      RETURN
      END FUNCTION nf90_fread3d
      END MODULE nf_fread3d_mod
