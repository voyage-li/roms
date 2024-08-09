      MODULE def_rst_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This module creates ouput restart file using either the standard    !
!  NetCDF library or the Parallel-IO (PIO) library.  It defines its    !
!  dimensions, attributes, and variables.                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_iounits
      USE mod_ncparam
      USE mod_scalars
!
      USE def_dim_mod,    ONLY : def_dim
      USE def_info_mod,   ONLY : def_info
      USE def_var_mod,    ONLY : def_var
      USE strings_mod,    ONLY : FoundError
      USE wrt_info_mod,   ONLY : wrt_info
!
      implicit none
!
      PUBLIC  :: def_rst
      PRIVATE :: def_rst_nf90
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE def_rst (ng)
!***********************************************************************
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/def_rst.F"
!
!-----------------------------------------------------------------------
!  Create a new history file according to IO type.
!-----------------------------------------------------------------------
!
      SELECT CASE (RST(ng)%IOtype)
        CASE (io_nf90)
          CALL def_rst_nf90 (ng, iNLM)
        CASE DEFAULT
          IF (Master) WRITE (stdout,10) RST(ng)%IOtype
          exit_flag=3
      END SELECT
      IF (FoundError(exit_flag, NoError, 88, MyFile)) RETURN
!
  10  FORMAT (' DEF_RST - Illegal output file type, io_type = ',i0,     &
     &        /,11x,'Check KeyWord ''OUT_LIB'' in ''roms.in''.')
!
      RETURN
      END SUBROUTINE def_rst
!
!***********************************************************************
      SUBROUTINE def_rst_nf90 (ng, model)
!***********************************************************************
!
      USE mod_netcdf
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model
!
!  Local variable declarations.
!
      logical :: Ldefine, got_var(NV)
!
      integer, parameter :: Natt = 25
      integer :: i, j, nvd3, nvd4, nvd5
      integer :: recdim, status, varid
      integer :: DimIDs(nDimID)
      integer :: r2dgrd(4), ru2dgrd(4), rv2dgrd(4)
      integer :: sp2dgrd(3), sr2dgrd(3), su2dgrd(3), sv2dgrd(3)
      integer :: sr3dgrd(4), su3dgrd(4), sv3dgrd(4)
      integer :: t2dgrd(4), u2dgrd(4), v2dgrd(4)
      integer :: itrc
      integer :: k3dgrd(5), t3dgrd(5)
      integer :: r3dgrd(4), ru3dgrd(5), rv3dgrd(5)
      integer :: u3dgrd(5), v3dgrd(5), w3dgrd(4)
!
      real(r8) :: Aval(6)
!
      character (len=256)    :: ncname
      character (len=MaxLen) :: Vinfo(Natt)
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/def_rst.F"//", def_rst_nf90"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Set and report file name.
!-----------------------------------------------------------------------
!
!  Activate creation of restart NetCDF file.  If a restart run, the
!  restart filename "RST(ng)%name" is different than the initial
!  filename "INI(ng)%name".
!
      IF (FoundError(exit_flag, NoError, 147, MyFile)) RETURN
      ncname=RST(ng)%name
      Ldefine=.FALSE.
      IF (((nrrec(ng).eq.0).and.(iic(ng).eq.ntstart(ng))).or.           &
     &    ((nrrec(ng).ne.0).and.                                        &
     &     (TRIM(ncname).ne.TRIM(INI(ng)%name)))) THEN
        Ldefine=.TRUE.
      END IF
!
      IF (Master) THEN
        IF (Ldefine) THEN
          WRITE (stdout,10) ng, TRIM(ncname)
        ELSE
          WRITE (stdout,20) ng, TRIM(ncname)
        END IF
      END IF
!
!=======================================================================
!  Create a new restart NetCDF file.
!=======================================================================
!
      DEFINE : IF (Ldefine) THEN
        CALL netcdf_create (ng, model, TRIM(ncname), RST(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 170, MyFile)) THEN
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
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 'xi_rho',       &
     &                 IOBOUNDS(ng)%xi_rho, DimIDs( 1))
        IF (FoundError(exit_flag, NoError, 183, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 'xi_u',         &
     &                 IOBOUNDS(ng)%xi_u, DimIDs( 2))
        IF (FoundError(exit_flag, NoError, 187, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 'xi_v',         &
     &                 IOBOUNDS(ng)%xi_v, DimIDs( 3))
        IF (FoundError(exit_flag, NoError, 191, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 'xi_psi',       &
     &                 IOBOUNDS(ng)%xi_psi, DimIDs( 4))
        IF (FoundError(exit_flag, NoError, 195, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 'eta_rho',      &
     &                 IOBOUNDS(ng)%eta_rho, DimIDs( 5))
        IF (FoundError(exit_flag, NoError, 199, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 'eta_u',        &
     &                 IOBOUNDS(ng)%eta_u, DimIDs( 6))
        IF (FoundError(exit_flag, NoError, 203, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 'eta_v',        &
     &                 IOBOUNDS(ng)%eta_v, DimIDs( 7))
        IF (FoundError(exit_flag, NoError, 207, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 'eta_psi',      &
     &                 IOBOUNDS(ng)%eta_psi, DimIDs( 8))
        IF (FoundError(exit_flag, NoError, 211, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 'N',            &
     &                 N(ng), DimIDs( 9))
        IF (FoundError(exit_flag, NoError, 250, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 's_rho',        &
     &                 N(ng), DimIDs( 9))
        IF (FoundError(exit_flag, NoError, 254, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 's_w',          &
     &                 N(ng)+1, DimIDs(10))
        IF (FoundError(exit_flag, NoError, 258, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 'tracer',       &
     &                 NT(ng), DimIDs(11))
        IF (FoundError(exit_flag, NoError, 262, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname, 'boundary',     &
     &                 4, DimIDs(14))
        IF (FoundError(exit_flag, NoError, 306, MyFile)) RETURN
        status=def_dim(ng, model, RST(ng)%ncid, ncname,                 &
     &                 TRIM(ADJUSTL(Vname(5,idtime))),                  &
     &                 nf90_unlimited, DimIDs(12))
        IF (FoundError(exit_flag, NoError, 327, MyFile)) RETURN
        recdim=DimIDs(12)
!
!  Set number of dimensions for output variables.
!
        nvd3=3
        nvd4=4
        nvd5=5
!
!  Define dimension vectors for staggered tracer type variables.
!
        t2dgrd(1)=DimIDs( 1)
        t2dgrd(2)=DimIDs( 5)
        sr2dgrd(1)=DimIDs( 1)
        sr2dgrd(2)=DimIDs( 5)
        sr2dgrd(3)=DimIDs(12)
        t2dgrd(3)=DimIDs(12)
        t3dgrd(1)=DimIDs( 1)
        t3dgrd(2)=DimIDs( 5)
        t3dgrd(3)=DimIDs( 9)
        r3dgrd(1)=DimIDs( 1)
        r3dgrd(2)=DimIDs( 5)
        r3dgrd(3)=DimIDs( 9)
        t3dgrd(4)=DimIDs(12)
        r3dgrd(4)=DimIDs(12)
!
!  Define dimension vectors for staggered type variables at PSI-points.
!
        sp2dgrd(1)=DimIDs( 4)
        sp2dgrd(2)=DimIDs( 8)
        sp2dgrd(3)=DimIDs(12)
!
!  Define dimension vectors for staggered u-momentum type variables.
!
        u2dgrd(1)=DimIDs( 2)
        u2dgrd(2)=DimIDs( 6)
        u2dgrd(3)=DimIDs(12)
        u3dgrd(1)=DimIDs( 2)
        u3dgrd(2)=DimIDs( 6)
        u3dgrd(3)=DimIDs( 9)
        u3dgrd(4)=DimIDs(12)
!
!  Define dimension vectors for staggered v-momentum type variables.
!
        v2dgrd(1)=DimIDs( 3)
        v2dgrd(2)=DimIDs( 7)
        v2dgrd(3)=DimIDs(12)
        v3dgrd(1)=DimIDs( 3)
        v3dgrd(2)=DimIDs( 7)
        v3dgrd(3)=DimIDs( 9)
        v3dgrd(4)=DimIDs(12)
!
!  Define dimension vector for staggered w-momentum type variables.
!
        w3dgrd(1)=DimIDs( 1)
        w3dgrd(2)=DimIDs( 5)
        w3dgrd(3)=DimIDs(10)
        w3dgrd(4)=DimIDs(12)
!
!  Define dimension vector for sediment, radiation stress variables.
!
        su2dgrd(1)=DimIDs( 2)
        su2dgrd(2)=DimIDs( 6)
        su2dgrd(3)=DimIDs(12)
        sv2dgrd(1)=DimIDs( 3)
        sv2dgrd(2)=DimIDs( 7)
        sv2dgrd(3)=DimIDs(12)
        sr3dgrd(1)=DimIDs( 1)
        sr3dgrd(2)=DimIDs( 5)
        sr3dgrd(3)=DimIDs(16)
        sr3dgrd(4)=DimIDs(12)
        su3dgrd(1)=DimIDs( 2)
        su3dgrd(2)=DimIDs( 6)
        su3dgrd(3)=DimIDs( 9)
        su3dgrd(4)=DimIDs(12)
        sv3dgrd(1)=DimIDs( 3)
        sv3dgrd(2)=DimIDs( 7)
        sv3dgrd(3)=DimIDs( 9)
        sv3dgrd(4)=DimIDs(12)
!
!  Initialize unlimited time record dimension.
!
        RST(ng)%Rindex=0
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
        CALL def_info (ng, model, RST(ng)%ncid, ncname, DimIDs)
        IF (FoundError(exit_flag, NoError, 607, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Define time-varying variables.
!-----------------------------------------------------------------------
!
!  Define model time.
!
        Vinfo( 1)=Vname(1,idtime)
        Vinfo( 2)=Vname(2,idtime)
        WRITE (Vinfo( 3),'(a,a)') 'seconds since ', TRIM(Rclock%string)
        Vinfo( 4)=TRIM(Rclock%calendar)
        Vinfo(14)=Vname(4,idtime)
        Vinfo(21)=Vname(6,idtime)
        status=def_var(ng, model, RST(ng)%ncid, RST(ng)%Vid(idtime),    &
     &                 NF_TOUT, 1, (/recdim/), Aval, Vinfo, ncname,     &
     &                 SetParAccess = .TRUE.)
        IF (FoundError(exit_flag, NoError, 672, MyFile)) RETURN
!
!  Define free-surface.
!
        Vinfo( 1)=Vname(1,idFsur)
        Vinfo( 2)=Vname(2,idFsur)
        Vinfo( 3)=Vname(3,idFsur)
        Vinfo(14)=Vname(4,idFsur)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(21)=Vname(6,idFsur)
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idFsur,ng),r8)
        status=def_var(ng, model, RST(ng)%ncid, RST(ng)%Vid(idFsur),    &
     &                 NF_FRST, nvd3, t2dgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 798, MyFile)) RETURN
!
!  Define 2D momentum in the XI-direction.
!
        Vinfo( 1)=Vname(1,idUbar)
        Vinfo( 2)=Vname(2,idUbar)
        Vinfo( 3)=Vname(3,idUbar)
        Vinfo(14)=Vname(4,idUbar)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(21)=Vname(6,idUbar)
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idUbar,ng),r8)
        status=def_var(ng, model, RST(ng)%ncid, RST(ng)%Vid(idUbar),    &
     &                 NF_FRST, nvd3, u2dgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 850, MyFile)) RETURN
!
!  Define 2D momentum in the ETA-direction.
!
        Vinfo( 1)=Vname(1,idVbar)
        Vinfo( 2)=Vname(2,idVbar)
        Vinfo( 3)=Vname(3,idVbar)
        Vinfo(14)=Vname(4,idVbar)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(21)=Vname(6,idVbar)
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idVbar,ng),r8)
        status=def_var(ng, model, RST(ng)%ncid, RST(ng)%Vid(idVbar),    &
     &                 NF_FRST, nvd3, v2dgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 902, MyFile)) RETURN
!
!  Define 3D momentum component in the XI-direction.
!
        Vinfo( 1)=Vname(1,idUvel)
        Vinfo( 2)=Vname(2,idUvel)
        Vinfo( 3)=Vname(3,idUvel)
        Vinfo(14)=Vname(4,idUvel)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(21)=Vname(6,idUvel)
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idUvel,ng),r8)
        status=def_var(ng, model, RST(ng)%ncid, RST(ng)%Vid(idUvel),    &
     &                 NF_FRST, nvd4, u3dgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 955, MyFile)) RETURN
!
!  Define 3D momentum component in the ETA-direction.
!
        Vinfo( 1)=Vname(1,idVvel)
        Vinfo( 2)=Vname(2,idVvel)
        Vinfo( 3)=Vname(3,idVvel)
        Vinfo(14)=Vname(4,idVvel)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(21)=Vname(6,idVvel)
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idVvel,ng),r8)
        status=def_var(ng, model, RST(ng)%ncid, RST(ng)%Vid(idVvel),    &
     &                 NF_FRST, nvd4, v3dgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 1010, MyFile)) RETURN
!
!  Define tracer type variables.
!
        DO itrc=1,NT(ng)
          Vinfo( 1)=Vname(1,idTvar(itrc))
          Vinfo( 2)=Vname(2,idTvar(itrc))
          Vinfo( 3)=Vname(3,idTvar(itrc))
          Vinfo(14)=Vname(4,idTvar(itrc))
          Vinfo(16)=Vname(1,idtime)
          Vinfo(21)=Vname(6,idTvar(itrc))
          Vinfo(22)='coordinates'
          Aval(5)=REAL(r3dvar,r8)
          status=def_var(ng, model, RST(ng)%ncid, RST(ng)%Tid(itrc),    &
     &                   NF_FRST, nvd4, t3dgrd, Aval, Vinfo, ncname)
          IF (FoundError(exit_flag, NoError, 1073, MyFile)) RETURN
        END DO
!
!  Define density anomaly.
!
        Vinfo( 1)=Vname(1,idDano)
        Vinfo( 2)=Vname(2,idDano)
        Vinfo( 3)=Vname(3,idDano)
        Vinfo(14)=Vname(4,idDano)
        Vinfo(16)=Vname(1,idtime)
        Vinfo(21)=Vname(6,idDano)
        Vinfo(22)='coordinates'
        Aval(5)=REAL(Iinfo(1,idDano,ng),r8)
        status=def_var(ng, model, RST(ng)%ncid, RST(ng)%Vid(idDano),    &
     &                 NF_FRST, nvd4, r3dgrd, Aval, Vinfo, ncname)
        IF (FoundError(exit_flag, NoError, 1096, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Leave definition mode.
!-----------------------------------------------------------------------
!
        CALL netcdf_enddef (ng, model, ncname, RST(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 1591, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Write out time-recordless, information variables.
!-----------------------------------------------------------------------
!
        CALL wrt_info (ng, model, RST(ng)%ncid, ncname)
        IF (FoundError(exit_flag, NoError, 1598, MyFile)) RETURN
      END IF DEFINE
!
!=======================================================================
!  Open an existing restart file, check its contents, and prepare for
!  appending data.
!=======================================================================
!
      QUERY : IF (.not.Ldefine) THEN
        ncname=RST(ng)%name
!
!  Open restart file for read/write.
!
        CALL netcdf_open (ng, model, ncname, 1, RST(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 1613, MyFile)) THEN
          WRITE (stdout,50) TRIM(ncname)
          RETURN
        END IF
!
!  Inquire about the dimensions and check for consistency.
!
        CALL netcdf_check_dim (ng, model, ncname,                       &
     &                         ncid = RST(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 1622, MyFile)) RETURN
!
!  Inquire about the variables.
!
        CALL netcdf_inq_var (ng, model, ncname,                         &
     &                       ncid = RST(ng)%ncid)
        IF (FoundError(exit_flag, NoError, 1628, MyFile)) RETURN
!
!  Initialize logical switches.
!
        DO i=1,NV
          got_var(i)=.FALSE.
        END DO
!
!  Scan variable list from input NetCDF and activate switches for
!  restart variables. Get variable IDs.
!
        DO i=1,n_var
          IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idtime))) THEN
            got_var(idtime)=.TRUE.
            RST(ng)%Vid(idtime)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idFsur))) THEN
            got_var(idFsur)=.TRUE.
            RST(ng)%Vid(idFsur)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUbar))) THEN
            got_var(idUbar)=.TRUE.
            RST(ng)%Vid(idUbar)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVbar))) THEN
            got_var(idVbar)=.TRUE.
            RST(ng)%Vid(idVbar)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idUvel))) THEN
            got_var(idUvel)=.TRUE.
            RST(ng)%Vid(idUvel)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVvel))) THEN
            got_var(idVvel)=.TRUE.
            RST(ng)%Vid(idVvel)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idDano))) THEN
            got_var(idDano)=.TRUE.
            RST(ng)%Vid(idDano)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idVvis))) THEN
            got_var(idVvis)=.TRUE.
            RST(ng)%Vid(idVvis)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idTdif))) THEN
            got_var(idTdif)=.TRUE.
            RST(ng)%Vid(idTdif)=var_id(i)
          ELSE IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idSdif))) THEN
            got_var(idSdif)=.TRUE.
            RST(ng)%Vid(idSdif)=var_id(i)
          END IF
          DO itrc=1,NT(ng)
            IF (TRIM(var_name(i)).eq.TRIM(Vname(1,idTvar(itrc)))) THEN
              got_var(idTvar(itrc))=.TRUE.
              RST(ng)%Tid(itrc)=var_id(i)
            END IF
          END DO
        END DO
!
!  Check if initialization variables are available in input NetCDF
!  file.
!
        IF (.not.got_var(idtime)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idtime)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idFsur)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idFsur)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idUbar)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idUbar)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idVbar)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idVbar)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idUvel)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idUvel)),          &
     &                                  TRIM(ncname)
          exit_flag=3
          RETURN
        END IF
        IF (.not.got_var(idVvel)) THEN
          IF (Master) WRITE (stdout,60) TRIM(Vname(1,idVvel)),          &
     &                                  TRIM(ncname)
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
!  Set unlimited time record dimension to current value.
!
        IF (LcycleRST(ng)) THEN
          RST(ng)%Rindex=0
        ELSE
          RST(ng)%Rindex=rec_size
        END IF
      END IF QUERY
!
  10  FORMAT (2x,'DEF_RST_NF90     - creating restart file,',t56,       &
     &        'Grid ',i2.2,': ',a)
  20  FORMAT (2x,'DEF_RST_NF90     - inquiring restart file,',t56,      &
     &        'Grid ',i2.2,': ',a)
  30  FORMAT (/,' DEF_RST_NF90 - unable to create restart NetCDF',      &
     &        ' file:',a)
  40  FORMAT (1pe11.4,1x,'millimeter')
  50  FORMAT (/,' DEF_RST_NF90 - unable to open restart NetCDF',        &
     &        ' file: ',a)
  60  FORMAT (/,' DEF_RST_NF90 - unable to find variable: ',a,2x,       &
     &        ' in restart NetCDF file: ',a)
!
      RETURN
      END SUBROUTINE def_rst_nf90
      END MODULE def_rst_mod
