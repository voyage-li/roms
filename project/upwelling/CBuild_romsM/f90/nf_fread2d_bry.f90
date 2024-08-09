      MODULE nf_fread2d_bry_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This module reads in a generic floating point 2D boundary array     !
!  from input file using either the standard NetCDF library or the     !
!  Parallel-IO (PIO) library.                                          !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number (integer)                        !
!     model        Calling model identifier (integer)                  !
!     ncname       NetCDF output file name (string)                    !
!     ncvname      NetCDF variable name (string)                       !
!     ncid         NetCDF file ID (integer)                            !
!     ncvarid      NetCDF variable ID (integer)                        !
!     tindex       NetCDF time record index to write (integer)         !
!     gtype        Grid type (integer)                                 !
!     LBij         IJ-dimension Lower bound (integer)                  !
!     UBij         IJ-dimension Upper bound (integer)                  !
!     Nrec         Number of boundary records (integer)                !
!     Ascl         Factor to scale field before writing (real)         !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     Amin         Field minimum value (real)                          !
!     Amax         Field maximum value (real)                          !
!     Abry         2D boundary field to read in (real array)           !
!     checksum     Field checksum value (integer*8; OPTIONAL)          !
!                                                                      !
!     status       Error flag (integer)                                !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_iounits
      USE mod_ncparam
      USE mod_scalars
!
      USE distribute_mod, ONLY : mp_bcastf
      USE get_hash_mod,   ONLY : get_hash
      USE strings_mod,    ONLY : FoundError
!
      implicit none
!
      INTERFACE nf_fread2d_bry
        MODULE PROCEDURE nf90_fread2d_bry
      END INTERFACE nf_fread2d_bry
!
      CONTAINS
!
!***********************************************************************
      FUNCTION nf90_fread2d_bry (ng, model, ncname, ncid,               &
     &                           ncvname, ncvarid,                      &
     &                           tindex, gtype,                         &
     &                           LBij, UBij, Nrec,                      &
     &                           Ascl, Amin, Amax,                      &
     &                           Abry, checksum)  RESULT(status)
!***********************************************************************
!
      USE mod_netcdf
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model, ncid, ncvarid, tindex, gtype
      integer, intent(in) :: LBij, UBij, Nrec
!
      integer(i8b), intent(out), optional :: checksum
!
      real(dp), intent(in)  :: Ascl
      real(r8), intent(out) :: Amin
      real(r8), intent(out) :: Amax
!
      character (len=*), intent(in) :: ncname
      character (len=*), intent(in) :: ncvname
      real(r8), intent(out) :: Abry(LBij:,:,:)
!
!  Local variable declarations.
!
      logical :: Lchecksum
      logical, dimension(3) :: foundit
      logical, dimension(4) :: bounded
!
      integer :: bc, ghost, i, ib, ic, ij, ir, j, tile
      integer :: Cgrid, IorJ, Imin, Imax, Jmin, Jmax, Npts
      integer :: Istr, Iend, Jstr, Jend
      integer, dimension(4) :: start, total
      integer :: status
!
      real(r8) :: Afactor, Aoffset, Aspval
!
      real(r8), allocatable :: Cwrk(:)           ! used for checksum
      real(r8), dimension(3) :: AttValue
      real(r8), dimension(3) :: rbuffer
      real(r8), dimension(LBij:UBij,4,Nrec) :: wrk
!
      character (len=12), dimension(3) :: AttName
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/nf_fread2d_bry.F"//", pio_fread2d_bry"
!
!-----------------------------------------------------------------------
!  Set starting and ending indices to process.
!-----------------------------------------------------------------------
!
      status=nf90_noerr
!
!  Set first and last grid point according to staggered C-grid
!  classification.
!
!  Notice that (Imin,Jmin) and (Imax,Jmax) are the corner of the
!  computational tile. If ghost=0, ghost points are not processed.
!  They will be processed elsewhere by the appropriate call to any
!  of the routines in "mp_exchange.F".  If ghost=1, the ghost points
!  are read.
!
      IF (model.eq.iADM) THEN
        ghost=0                      ! non-overlapping, no ghost points
      ELSE
        ghost=1                      ! overlapping, read ghost points
      END IF
!
      SELECT CASE (gtype)
        CASE (p2dvar, p3dvar)
          Cgrid=1
        CASE (r2dvar, r3dvar)
          Cgrid=2
        CASE (u2dvar, u3dvar)
          Cgrid=3
        CASE (v2dvar, v3dvar)
          Cgrid=4
        CASE DEFAULT
          Cgrid=2
      END SELECT
!
      tile=MyRank
      Imin=BOUNDS(ng)%Imin(Cgrid,ghost,tile)
      Imax=BOUNDS(ng)%Imax(Cgrid,ghost,tile)
      Jmin=BOUNDS(ng)%Jmin(Cgrid,ghost,tile)
      Jmax=BOUNDS(ng)%Jmax(Cgrid,ghost,tile)
!
      IorJ=IOBOUNDS(ng)%IorJ
      Npts=IorJ*4*Nrec
!
!  Get tile bounds.
!
      Istr=BOUNDS(ng)%Istr (tile)
      Iend=BOUNDS(ng)%Iend (tile)
      Jstr=BOUNDS(ng)%Jstr (tile)
      Jend=BOUNDS(ng)%Jend (tile)
!
!  Set switch to process boundary data by their associated tiles.
!
      bounded(iwest )=DOMAIN(ng)%Western_Edge(tile)
      bounded(ieast )=DOMAIN(ng)%Eastern_Edge(tile)
      bounded(isouth)=DOMAIN(ng)%Southern_Edge(tile)
      bounded(inorth)=DOMAIN(ng)%Northern_Edge(tile)
!
!  Set NetCDF dimension counters for processing requested field.
!
      start(1)=1
      total(1)=IorJ
      start(2)=1
      total(2)=4
      start(3)=1
      total(3)=Nrec
      start(4)=tindex
      total(4)=1
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
!  is treated as missing and it is replaced with zero. This feature it
!  is usually related with the land/sea masking.
!
      AttName(1)='scale_factor'
      AttName(2)='add_offset  '
      AttName(3)='_FillValue  '
      CALL netcdf_get_fatt (ng, model, ncname, ncvarid, AttName,        &
     &                      AttValue, foundit,                          &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 235, MyFile)) THEN
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
!  Read in requested data and scale it.
!-----------------------------------------------------------------------
!
      wrk=0.0_r8
      IF (InpThread) THEN
        status=nf90_get_var(ncid, ncvarid, wrk(LBij:,:,:), start, total)
        IF (status.eq.nf90_noerr) THEN
          Amin=spval
          Amax=-spval
          DO ir=1,Nrec
            DO ib=1,4
              DO ij=LBij,UBij
                IF (ABS(wrk(ij,ib,ir)).ge.ABS(Aspval)) THEN
                  wrk(ij,ib,ir)=0.0_r8
                ELSE
                  wrk(ij,ib,ir)=Ascl*(Afactor*wrk(ij,ib,ir)+Aoffset)
                  Amin=MIN(Amin,wrk(ij,ib,ir))
                  Amax=MAX(Amax,wrk(ij,ib,ir))
                END IF
              END DO
            END DO
          END DO
          IF ((ABS(Amin).ge.ABS(Aspval)).and.                           &
   &          (ABS(Amax).ge.ABS(Aspval))) THEN
            Amin=0.0_r8                       ! the entire data is all
            Amax=0.0_r8                       ! field value, _FillValue
          END IF
!
          IF (Lchecksum) THEN
            Npts=(UBij-LBij+1)*Nrec*4
            IF (.not.allocated(Cwrk)) allocate ( Cwrk(Npts) )
            Cwrk=PACK(wrk(LBij:UBij, 1:4, 1:Nrec), .TRUE.)
            CALL get_hash (Cwrk, Npts, checksum)
            IF (allocated(Cwrk)) deallocate (Cwrk)
          END IF
        END IF
      END IF
!
      rbuffer(1)=REAL(status,r8)
      rbuffer(2)=Amin
      rbuffer(3)=Amax
      CALL mp_bcastf (ng, model, rbuffer)
      status=INT(rbuffer(1))
      Amin=rbuffer(2)
      Amax=rbuffer(3)
!
      IF (FoundError(status, nf90_noerr, 318, MyFile)) THEN
        exit_flag=2
        ioerror=status
        RETURN
      END IF
!
!  Broadcast data to all spawned nodes.
!
      CALL mp_bcastf (ng, model, wrk)
!
!-----------------------------------------------------------------------
!  Unpack read data.
!-----------------------------------------------------------------------
!
      Abry=0.0_r8
      DO ir=1,Nrec
        DO ib=1,4
          IF (bounded(ib)) THEN
            IF ((ib.eq.iwest).or.(ib.eq.ieast)) THEN
              DO j=Jmin,Jmax
                Abry(j,ib,ir)=wrk(j,ib,ir)
              END DO
            ELSE IF ((ib.eq.isouth).or.(ib.eq.inorth)) THEN
              DO i=Imin,Imax
                Abry(i,ib,ir)=wrk(i,ib,ir)
              END DO
            END IF
          END IF
        END DO
      END DO
!
      RETURN
      END FUNCTION nf90_fread2d_bry
      END MODULE nf_fread2d_bry_mod
