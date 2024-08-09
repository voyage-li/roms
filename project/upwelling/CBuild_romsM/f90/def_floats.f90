      MODULE def_floats_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This module creates output FLOAT data file using either the         !
!  standard NetCDF library or the Parallel-IO (PIO) library.  It       !
!  defines its dimensions, attributes, and variables.                  !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_floats
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_scalars
!
      USE def_dim_mod,  ONLY : def_dim
      USE def_info_mod, ONLY : def_info
      USE def_var_mod,  ONLY : def_var
      USE strings_mod,  ONLY : FoundError
      USE wrt_info_mod, ONLY : wrt_info
!
      implicit none
!
      PUBLIC  :: def_floats
      PRIVATE :: def_floats_nf90
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE def_floats (ng, ldef)
!***********************************************************************
!
!  Imported variable declarations.
!
      logical, intent(in) :: ldef
!
      integer, intent(in) :: ng
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/def_floats.F"
!
!-----------------------------------------------------------------------
!  Create a new history file according to IO type.
!-----------------------------------------------------------------------
!
      SELECT CASE (FLT(ng)%IOtype)
        CASE (io_nf90)
          CALL def_floats_nf90 (ng, ldef)
        CASE DEFAULT
          IF (Master) WRITE (stdout,10) FLT(ng)%IOtype
          exit_flag=3
      END SELECT
      IF (FoundError(exit_flag, NoError, 82, MyFile)) RETURN
!
  10  FORMAT (' DEF_FLOATS - Illegal output file type, io_type = ',i0,  &
     &        /,14x,'Check KeyWord ''OUT_LIB'' in ''roms.in''.')
!
      RETURN
      END SUBROUTINE def_floats
!
!***********************************************************************
      SUBROUTINE def_floats_nf90 (ng, ldef)
!***********************************************************************
!
      USE mod_netcdf
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng
!
      logical, intent(in) :: ldef
!
!  Local variable declarations.
!
      logical :: got_var(-6:NV)
!
      integer, parameter :: Natt = 25
      integer :: fltdim, i, itrc, j, l
      integer :: recdim, status
      integer :: DimIDs(nDimID)
      integer :: fgrd(2), start(2), total(2)
!
      real(r8) :: Aval(6), Tinp(Nfloats(ng))
!
      character (len=256)    :: ncname
      character (len=MaxLen) :: Vinfo(Natt)
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/def_floats.F"//", def_floats_nf90"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Set and report file name.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, 128, MyFile)) RETURN
      ncname=FLT(ng)%name
!
      IF (Master) THEN
        IF (ldef) THEN
          WRITE (stdout,10) ng, TRIM(ncname)
        ELSE
          WRITE (stdout,20) ng, TRIM(ncname)
        END IF
      END IF
!
!=======================================================================
!  Create a new floats data file.
!=======================================================================
!
      DEFINE : IF (ldef) THEN
        CALL netcdf_create (ng, iNLM, TRIM(ncname), FLT(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 145, MyFile)) THEN
          IF (Master) WRITE (stdout,30) TRIM(ncname)
          RETURN
        END IF
!
!-----------------------------------------------------------------------
!  Define file dimensions.
!-----------------------------------------------------------------------
!
        DimIDs=0
!
        status=def_dim(ng, iNLM, FLT(ng)%ncid, ncname, 's_rho',         &
     &                 N(ng), DimIDs( 9))
        IF (FoundError(exit_flag, NoError, 159, MyFile)) RETURN
        status=def_dim(ng, iNLM, FLT(ng)%ncid, ncname, 's_w',           &
     &                 N(ng)+1, DimIDs(10))
        IF (FoundError(exit_flag, NoError, 163, MyFile)) RETURN
        status=def_dim(ng, iNLM, FLT(ng)%ncid, ncname, 'tracer',        &
     &                 NT(ng), DimIDs(11))
        IF (FoundError(exit_flag, NoError, 167, MyFile)) RETURN
        status=def_dim(ng, iNLM, FLT(ng)%ncid, ncname, 'drifter' ,      &
     &                 Nfloats(ng), DimIDs(15))
        IF (FoundError(exit_flag, NoError, 204, MyFile)) RETURN
        status=def_dim(ng, iNLM, FLT(ng)%ncid, ncname, 'boundary',      &
     &                 4, DimIDs(14))
        IF (FoundError(exit_flag, NoError, 208, MyFile)) RETURN
        status=def_dim(ng, iNLM, FLT(ng)%ncid, ncname,                  &
     &                 TRIM(ADJUSTL(Vname(5,idtime))),                  &
     &                 nf90_unlimited, DimIDs(12))
        IF (FoundError(exit_flag, NoError, 219, MyFile)) RETURN
        recdim=DimIDs(12)
        fltdim=DimIDs(15)
!
!  Define dimension vectors for point variables.
!
        fgrd(1)=DimIDs(15)
        fgrd(2)=DimIDs(12)
!
!  Initialize unlimited time record dimension.
!
        FLT(ng)%Rindex=0
!
!  Initialize local information variable arrays.
!
        DO i=1,Natt
          DO j=1,LEN(Vinfo(1))
            Vinfo(i)(j:j)=' '
          END DO
        END DO
        DO i=1,6
          Aval(i)=0.0_r8
        END DO
!
!-----------------------------------------------------------------------
!  Define time-recordless information variables.
!-----------------------------------------------------------------------
!
        CALL def_info (ng, iNLM, FLT(ng)%ncid, ncname, DimIDs)
        IF (FoundError(exit_flag, NoError, 249, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Define variables and their attributes.
!-----------------------------------------------------------------------
!
!  Define model time.
!
        Vinfo( 1)=Vname(1,idtime)
        Vinfo( 2)=Vname(2,idtime)
        WRITE (Vinfo( 3),'(a,a)') 'seconds since ', TRIM(Rclock%string)
        Vinfo( 4)=TRIM(Rclock%calendar)
        Vinfo(14)=Vname(4,idtime)
        status=def_var(ng, iNLM, FLT(ng)%ncid, FLT(ng)%Vid(idtime),     &
     &                 NF_TOUT, 1, (/recdim/), Aval, Vinfo, ncname,     &
     &                 SetParAccess = .TRUE.)
        IF (FoundError(exit_flag, NoError, 265, MyFile)) RETURN
!
!  Define floats X-grid locations.
!
        Vinfo( 1)='Xgrid'
        Vinfo( 2)='x-grid floats locations'
        Vinfo( 5)='valid_min'
        Vinfo( 6)='valid_max'
        Aval(2)=0.0_r8
        Aval(3)=REAL(Lm(ng)+1,r8)
        Vinfo(14)='Xgrid, scalar, series'
        Vinfo(16)=Vname(1,idtime)
        Vinfo(24)='_FillValue'
        Aval(6)=spval
        status=def_var(ng, iNLM, FLT(ng)%ncid, FLT(ng)%Vid(idXgrd),     &
     &                 NF_FOUT, 2, fgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 283, MyFile)) RETURN
!
!  Define floats Y-grid locations.
!
        Vinfo( 1)='Ygrid'
        Vinfo( 2)='Y-grid floats locations'
        Vinfo( 5)='valid_min'
        Vinfo( 6)='valid_max'
        Aval(2)=0.0_r8
        Aval(3)=REAL(Mm(ng)+1,r8)
        Vinfo(14)='Ygrid, scalar, series'
        Vinfo(16)=Vname(1,idtime)
        Vinfo(24)='_FillValue'
        Aval(6)=spval
        status=def_var(ng, iNLM, FLT(ng)%ncid, FLT(ng)%Vid(idYgrd),     &
     &                 NF_FOUT, 2, fgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 301, MyFile)) RETURN
!
!  Define floats Z-grid locations.
!
        Vinfo( 1)='Zgrid'
        Vinfo( 2)='Z-grid floats locations'
        Vinfo( 5)='valid_min'
        Vinfo( 6)='valid_max'
        Aval(2)=0.0_r8
        Aval(3)=REAL(N(ng),r8)
        Vinfo(14)='Zgrid, scalar, series'
        Vinfo(16)=Vname(1,idtime)
        Vinfo(24)='_FillValue'
        Aval(6)=spval
        status=def_var(ng, iNLM, FLT(ng)%ncid, FLT(ng)%Vid(idZgrd),     &
     &                 NF_FOUT, 2, fgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 321, MyFile)) RETURN
!
!  Define floats (lon,lat) or (x,y) locations.
!
        IF (spherical) THEN
          Vinfo( 1)='lon'
          Vinfo( 2)='longitude of floats trajectories'
          Vinfo( 3)='degree_east'
          Vinfo( 5)='valid_min'
          Vinfo( 6)='valid_max'
          Vinfo(14)='lon, scalar, series'
          Vinfo(16)=Vname(1,idtime)
          Vinfo(24)='_FillValue'
          Aval(6)=spval
          Aval(2)=-180.0_r8
          Aval(3)=180.0_r8
          status=def_var(ng, iNLM, FLT(ng)%ncid, FLT(ng)%Vid(idglon),   &
     &                   NF_FOUT, 2, fgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, 342, MyFile)) RETURN
          Vinfo( 1)='lat'
          Vinfo( 2)='latitude of floats trajectories'
          Vinfo( 3)='degree_north'
          Vinfo( 5)='valid_min'
          Vinfo( 6)='valid_max'
          Vinfo(14)='lat, scalar, series'
          Vinfo(16)=Vname(1,idtime)
          Vinfo(24)='_FillValue'
          Aval(6)=spval
          Aval(2)=-90.0_r8
          Aval(3)=90.0_r8
          status=def_var(ng, iNLM, FLT(ng)%ncid, FLT(ng)%Vid(idglat),   &
     &                   NF_FOUT, 2, fgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, 359, MyFile)) RETURN
       ELSE
          Vinfo( 1)='x'
          Vinfo( 2)='x-location of floats trajectories'
          Vinfo( 3)='meter'
          Vinfo(14)='x, scalar, series'
          Vinfo(16)=Vname(1,idtime)
          Vinfo(24)='_FillValue'
          Aval(6)=spval
          status=def_var(ng, iNLM, FLT(ng)%ncid, FLT(ng)%Vid(idglon),   &
     &                   NF_FOUT, 2, fgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, 372, MyFile)) RETURN
          Vinfo( 1)='y'
          Vinfo( 2)='y-location of floats trajectories'
          Vinfo( 3)='meter'
          Vinfo(14)='y, scalar, series'
          Vinfo(16)=Vname(1,idtime)
          Vinfo(24)='_FillValue'
          Aval(6)=spval
          status=def_var(ng, iNLM, FLT(ng)%ncid, FLT(ng)%Vid(idglat),   &
     &                   NF_FOUT, 2, fgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, 385, MyFile)) RETURN
        END IF
!
!  Define floats depths.
!
        Vinfo( 1)='depth'
        Vinfo( 2)='depth of floats trajectories'
        Vinfo( 3)='meter'
        Vinfo(14)='depth, scalar, series'
        Vinfo(16)=Vname(1,idtime)
        Vinfo(24)='_FillValue'
        Aval(6)=spval
        status=def_var(ng, iNLM, FLT(ng)%ncid, FLT(ng)%Vid(iddpth),     &
     &                 NF_FOUT, 2, fgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 403, MyFile)) RETURN
!
!  Define density anomaly.
!
        Vinfo( 1)=Vname(1,idDano)
        Vinfo( 2)=Vname(2,idDano)
        Vinfo( 3)=Vname(3,idDano)
        Vinfo(14)=Vname(4,idDano)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(24)='_FillValue'
        Aval(6)=spval
        status=def_var(ng, iNLM, FLT(ng)%ncid, FLT(ng)%Vid(idDano),     &
     &                 NF_FOUT, 2, fgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 418, MyFile)) RETURN
!
!  Define tracer type variables.
!
        DO itrc=1,NT(ng)
          Vinfo( 1)=Vname(1,idTvar(itrc))
          Vinfo( 2)=Vname(2,idTvar(itrc))
          Vinfo( 3)=Vname(3,idTvar(itrc))
          Vinfo(14)=Vname(4,idTvar(itrc))
          Vinfo(16)=Vname(1,idtime)
          Vinfo(24)='_FillValue'
          Aval(6)=spval
          status=def_var(ng, iNLM, FLT(ng)%ncid, FLT(ng)%Tid(itrc),     &
     &                   NF_FOUT, 2, fgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, 441, MyFile)) RETURN
        END DO
!
!  Initialize unlimited time record dimension.
!
        FLT(ng)%Rindex=0
!
!-----------------------------------------------------------------------
!  Leave definition mode.
!-----------------------------------------------------------------------
!
        CALL netcdf_enddef (ng, iNLM, ncname, FLT(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 517, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Write out time-recordless, information variables.
!-----------------------------------------------------------------------
!
        CALL wrt_info (ng, iNLM, FLT(ng)%ncid, ncname)
        IF (FoundError(exit_flag, NoError, 524, MyFile)) RETURN
      END IF DEFINE
!
!=======================================================================
!  Open an existing floats file, check its contents, and prepare for
!  appending data.
!=======================================================================
!
      QUERY : IF (.not.ldef) THEN
        ncname=FLT(ng)%name
!
!  Open floats file for read/write.
!
        CALL netcdf_open (ng, iNLM, ncname, 1, FLT(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 538, MyFile)) THEN
          WRITE (stdout,50) TRIM(ncname)
          RETURN
        END IF
!
!  Inquire about the dimensions and check for consistency.
!
        CALL netcdf_check_dim (ng, iNLM, ncname,                        &
     &                         ncid = FLT(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 547, MyFile)) RETURN
!
!  Get the size of the drifter dimension.
!
        DO i=1,n_dim
          IF (TRIM(dim_name(i)).eq.'drifter') THEN
            Nfloats(ng)=dim_size(i)
            EXIT
          END IF
        END DO
!
!  Inquire about the variables.
!
        CALL netcdf_inq_var (ng, iNLM, ncname,                          &
     &                       ncid = FLT(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 562, MyFile)) RETURN
!
!  Initialize logical switches.
!
        DO i=1,NV
          got_var(i)=.FALSE.
        END DO
!
!  Scan variable list from input NetCDF and activate switches for
!  float variables. Get variable IDs.
!
        DO i=1,n_var
          IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idtime))) THEN
            got_var(idtime)=.TRUE.
            FLT(ng)%Vid(idtime)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.'Xgrid') THEN
            got_var(idXgrd)=.TRUE.
            FLT(ng)%Vid(idXgrd)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.'Ygrid') THEN
            got_var(idYgrd)=.TRUE.
            FLT(ng)%Vid(idYgrd)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.'Zgrid') THEN
            got_var(idZgrd)=.TRUE.
            FLT(ng)%Vid(idZgrd)=var_id(i)
          ELSE IF (spherical.and.TRIM(var_name(i)).eq.'lon') THEN
            got_var(idglon)=.TRUE.
            FLT(ng)%Vid(idglon)=var_id(i)
          ELSE IF (spherical.and.TRIM(var_name(i)).eq.'lat') THEN
            got_var(idglat)=.TRUE.
            FLT(ng)%Vid(idglat)=var_id(i)
          ELSE IF (.not.spherical.and.TRIM(var_name(i)).eq.'x') THEN
            got_var(idglon)=.TRUE.
            FLT(ng)%Vid(idglon)=var_id(i)
          ELSE IF (.not.spherical.and.TRIM(var_name(i)).eq.'y') THEN
            got_var(idglat)=.TRUE.
            FLT(ng)%Vid(idglat)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.'depth') THEN
            got_var(iddpth)=.TRUE.
            FLT(ng)%Vid(iddpth)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idDano))) THEN
            got_var(idDano)=.TRUE.
            FLT(ng)%Vid(idDano)=var_id(i)
          END IF
          DO itrc=1,NT(ng)
            IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idTvar(itrc)))) THEN
              got_var(idTvar(itrc))=.TRUE.
              FLT(ng)%Tid(itrc)=var_id(i)
            END IF
          END DO
        END DO
!
!  Check if floats variables are available in input NetCDF file.
!
        IF (.not.got_var(idtime)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idtime)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idXgrd)) THEN
          IF (Master) WRITE (stdout,60) 'Xgrid', TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idYgrd)) THEN
          IF (Master) WRITE (stdout,60) 'Ygrid', TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idZgrd)) THEN
          IF (Master) WRITE (stdout,60) 'Zgrid', TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idglon)) THEN
          IF (spherical) THEN
            IF (Master) WRITE (stdout,60) 'lon', TRIM(ncname)
          ELSE
            IF (Master) WRITE (stdout,60) 'x', TRIM(ncname)
          END IF
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idglat)) THEN
          IF (spherical) THEN
            IF (Master) WRITE (stdout,60) 'lat', TRIM(ncname)
          ELSE
            IF (Master) WRITE (stdout,60) 'y', TRIM(ncname)
          END IF
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(iddpth)) THEN
          IF (Master) WRITE (stdout,60) 'depth', TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idDano)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idDano)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        DO itrc=1,NT(ng)
          IF (.not.got_var(idTvar(itrc))) THEN
            IF (Master) WRITE (stdout,60) TRIM(Vname(1,idTvar(itrc))),  &
     &                                    TRIM(ncname)
            exit_flag=3
            RETURN
          END IF
        END DO
!
!-----------------------------------------------------------------------
!  Initialize floats positions to the appropriate values.
!-----------------------------------------------------------------------
!
!  Set-up floats time record.
!
        IF (frrec(ng).lt.0) THEN
          FLT(ng)%Rindex=rec_size
        ELSE
          FLT(ng)%Rindex=ABS(frrec(ng))
        END IF
!
!  Read in floats nondimentional horizontal positions.  If the floats
!  have not been released yet at restart time, the values of Xgrid,
!  Ygrid, and Zgrid will be _FillValue (1.0E+37) in the 1 NetCDF
!  file. The calls to 'netcdf_get_fvar' will replace such values with
!  zero.  Therefore, we need to read Zgrid first so the bounded switch
!  is false in such cases tp trigger release.  Then, the bounded switch
!  is set correctly when reading Xgrid and/or Ygrid since the lower
!  bound is 0.5 in fractional coordinates.
!
        CALL netcdf_get_fvar (ng, iNLM, ncname, 'Zgrid',                &
     &                        Tinp,                                     &
     &                        ncid = FLT(ng)%ncid,                      &
     &                        start = (/1,FLT(ng)%Rindex/),             &
     &                        total = (/Nfloats(ng),1/))
        IF (FoundError(exit_flag, NoError, 749, MyFile)) RETURN
        DO l=1,Nfloats(ng)
          IF ((Tinp(l).gt.REAL(N(ng),r8)).or.                           &
     &        (Tinp(l).lt.0.0_r8)) THEN
            DRIFTER(ng)%bounded(l)=.FALSE.
          ELSE
            DRIFTER(ng)%bounded(l)=.TRUE.
            DO i=0,NFT
              DRIFTER(ng)%track(izgrd,i,l)=Tinp(l)
              DRIFTER(ng)%track(izrhs,i,l)=0.0_r8
            END DO
          END IF
        END DO
!
        CALL netcdf_get_fvar (ng, iNLM, ncname, 'Xgrid',                &
     &                        Tinp,                                     &
     &                        ncid = FLT(ng)%ncid,                      &
     &                        start = (/1,FLT(ng)%Rindex/),             &
     &                        total = (/Nfloats(ng),1/))
        IF (FoundError(exit_flag, NoError, 770, MyFile)) RETURN
        DO l=1,Nfloats(ng)
          IF ((Tinp(l).gt.REAL(Lm(ng)+1,r8)-0.5_r8).or.                 &
     &        (Tinp(l).lt.0.5_r8)) THEN
            DRIFTER(ng)%bounded(l)=.FALSE.
          ELSE
            DRIFTER(ng)%bounded(l)=.TRUE.
            DO i=0,NFT
              DRIFTER(ng)%track(ixgrd,i,l)=Tinp(l)
              DRIFTER(ng)%track(ixrhs,i,l)=0.0_r8
            END DO
          END IF
        END DO
!
        CALL netcdf_get_fvar (ng, iNLM, ncname, 'Ygrid',                &
     &                        Tinp,                                     &
     &                        ncid = FLT(ng)%ncid,                      &
     &                        start = (/1,FLT(ng)%Rindex/),             &
     &                        total = (/Nfloats(ng),1/))
        IF (FoundError(exit_flag, NoError, 790, MyFile)) RETURN
        DO l=1,Nfloats(ng)
          IF ((Tinp(l).gt.REAL(Mm(ng)+1,r8)-0.5_r8).or.                 &
     &        (Tinp(l).lt.0.5_r8)) THEN
            DRIFTER(ng)%bounded(l)=.FALSE.
          ELSE
            DRIFTER(ng)%bounded(l)=.TRUE.
            DO i=0,NFT
              DRIFTER(ng)%track(iygrd,i,l)=Tinp(l)
              DRIFTER(ng)%track(iyrhs,i,l)=0.0_r8
            END DO
          END IF
        END DO
      END IF QUERY
!
  10  FORMAT (2x,'DEF_FLOATS_NF90  - creating floats file,',t56,        &
     &        'Grid ',i2.2,': ',a)
  20  FORMAT (2x,'DEF_FLOATS_NF90  - inquiring floats file,',t56,       &
     &        'Grid ',i2.2,': ',a)
  30  FORMAT (/,' DEF_FLOATS_NF90 - unable to create floats NetCDF',    &
     &        ' file: ',a)
  40  FORMAT (1pe11.4,1x,'millimeter')
  50  FORMAT (/,' DEF_FLOATS_NF90 - unable to open floats NetCDF',      &
     &        ' file: ',a)
  60  FORMAT (/,' DEF_FLOATS_NF90 - unable to find variable: ',a,2x,    &
     &        ' in floats NetCDF file: ',a)
!
      RETURN
      END SUBROUTINE def_floats_nf90
      END MODULE def_floats_mod
