      MODULE wrt_diags_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This subroutine writes model time-averaged diagnostic fields into   !
!  diagnostics file using thestandard NetCDF library or the Parallel-  !
!  IO (PIO) library.                                                   !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_diags
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
!
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
      USE nf_fwrite3d_mod, ONLY : nf_fwrite3d
      USE strings_mod,     ONLY : FoundError
!
      implicit none
!
      PUBLIC  :: wrt_diags
      PRIVATE :: wrt_diags_nf90
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE wrt_diags (ng, tile)
!***********************************************************************
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      integer :: LBi, UBi, LBj, UBj
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/wrt_diags.F"
!
!-----------------------------------------------------------------------
!  Write out time-averaged fields according to IO type.
!-----------------------------------------------------------------------
!
      LBi=BOUNDS(ng)%LBi(tile)
      UBi=BOUNDS(ng)%UBi(tile)
      LBj=BOUNDS(ng)%LBj(tile)
      UBj=BOUNDS(ng)%UBj(tile)
!
      SELECT CASE (DIA(ng)%IOtype)
        CASE (io_nf90)
          CALL wrt_diags_nf90 (ng, tile,                                &
     &                         LBi, UBi, LBj, UBj)
        CASE DEFAULT
          IF (Master) WRITE (stdout,10) DIA(ng)%IOtype
          exit_flag=3
      END SELECT
      IF (FoundError(exit_flag, NoError, 87, MyFile)) RETURN
!
  10  FORMAT (' WRT_DIAGS - Illegal output file type, io_type = ',i0,   &
     &        /,13x,'Check KeyWord ''OUT_LIB'' in ''roms.in''.')
!
      RETURN
      END SUBROUTINE wrt_diags
!
!***********************************************************************
      SUBROUTINE wrt_diags_nf90 (ng, tile,                              &
     &                           LBi, UBi, LBj, UBj)
!***********************************************************************
!
      USE mod_netcdf
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
!
!  Local variable declarations.
!
      integer :: Fcount, gfactor, gtype, ifield, itrc, ivar, status
!
      real(dp) :: scale
      real(r8) :: dtBIO
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/wrt_diags.F"//", wrt_diags_nf90"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Write out time-averaged diagnostic fields when appropriate.
!-----------------------------------------------------------------------
!
      if (FoundError(exit_flag, NoError, 125, MyFile)) RETURN
!
!  Set grid type factor to write full (gfactor=1) fields or water
!  points (gfactor=-1) fields only.
!
        gfactor=1
!
!  Set time and time-record index.
!
      DIA(ng)%Rindex=DIA(ng)%Rindex+1
      Fcount=DIA(ng)%load
      DIA(ng)%Nrec(Fcount)=DIA(ng)%Nrec(Fcount)+1
!
!  Report.
!
      IF (Master) WRITE (stdout,10) DIA(ng)%Rindex
!
!  Write out averaged time.
!
      CALL netcdf_put_fvar (ng, iNLM, DIA(ng)%name,                     &
     &                      TRIM(Vname(1,idtime)), DIAtime(ng:),        &
     &                      (/DIA(ng)%Rindex/), (/1/),                  &
     &                      ncid = DIA(ng)%ncid,                        &
     &                      varid = DIA(ng)%Vid(idtime))
      IF (FoundError(exit_flag, NoError, 157, MyFile)) RETURN
!
!  Write out time-averaged free-surface (m).
!
      scale=1.0_dp
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, iNLM, DIA(ng)%ncid, idFsur,                &
     &                   DIA(ng)%Vid(idFsur),                           &
     &                   DIA(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
     &                   DIAGS(ng) % avgzeta)
      IF (FoundError(status, nf90_noerr, 171, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idFsur)), DIA(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out 2D momentum diagnostic fields.
!
      DO ivar=1,NDM2d
        ifield=idDu2d(ivar)
        IF (Dout(ifield,ng)) THEN
          scale=1.0_dp/dt(ng)
          gtype=gfactor*u2dvar
          status=nf_fwrite2d(ng, iNLM, DIA(ng)%ncid, ifield,            &
     &                       DIA(ng)%Vid(ifield),                       &
     &                       DIA(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
     &                       DIAGS(ng) % DiaU2d(:,:,ivar),              &
     &                       SetFillVal = .FALSE.)
          IF (FoundError(status, nf90_noerr, 198, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
        ifield=idDv2d(ivar)
        IF (Dout(ifield,ng)) THEN
          scale=1.0_dp/dt(ng)
          gtype=gfactor*v2dvar
          status=nf_fwrite2d(ng, iNLM, DIA(ng)%ncid, ifield,            &
     &                       DIA(ng)%Vid(ifield),                       &
     &                       DIA(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
     &                       DIAGS(ng) % DiaV2d(:,:,ivar),              &
     &                       SetFillVal = .FALSE.)
          IF (FoundError(status, nf90_noerr, 221, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out 3D momentum diagnostic fields.
!
      DO ivar=1,NDM3d
        ifield=idDu3d(ivar)
        IF (Dout(ifield,ng)) THEN
          scale=1.0_dp/dt(ng)
          gtype=gfactor*u3dvar
          status=nf_fwrite3d(ng, iNLM, DIA(ng)%ncid, ifield,            &
     &                       DIA(ng)%Vid(ifield),                       &
     &                       DIA(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       DIAGS(ng) % DiaU3d(:,:,:,ivar),            &
     &                       SetFillVal = .FALSE.)
          IF (FoundError(status, nf90_noerr, 250, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
        ifield=idDv3d(ivar)
        IF (Dout(ifield,ng)) THEN
          scale=1.0_dp/dt(ng)
          gtype=gfactor*v3dvar
          status=nf_fwrite3d(ng, iNLM, DIA(ng)%ncid, ifield,            &
     &                       DIA(ng)%Vid(ifield),                       &
     &                       DIA(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       DIAGS(ng) % DiaV3d(:,:,:,ivar),            &
     &                       SetFillVal = .FALSE.)
          IF (FoundError(status, nf90_noerr, 273, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out tracer diagnostic fields.
!
      DO itrc=1,NT(ng)
        DO ivar=1,NDT
          ifield=idDtrc(itrc,ivar)
          IF (Dout(ifield,ng)) THEN
            scale=1.0_dp/dt(ng)
            gtype=gfactor*r3dvar
            status=nf_fwrite3d(ng, iNLM, DIA(ng)%ncid, ifield,          &
     &                         DIA(ng)%Vid(ifield),                     &
     &                         DIA(ng)%Rindex, gtype,                   &
     &                         LBi, UBi, LBj, UBj, 1, N(ng), scale,     &
     &                         DIAGS(ng) % DiaTrc(:,:,:,itrc,ivar),     &
     &                         SetFillVal = .FALSE.)
            IF (FoundError(status, nf90_noerr, 304, MyFile)) THEN
              IF (Master) THEN
                WRITE (stdout,20) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
              END IF
              exit_flag=3
              ioerror=status
              RETURN
            END IF
          END IF
        END DO
      END DO
!
!  Write out 2D biological diagnostic fields.
!
      dtBIO=dt(ng)*sec2day/REAL(BioIter(ng),r8)
      DO ivar=1,NDbio2d
        ifield=iDbio2(ivar)
        IF (Dout(ifield,ng)) THEN
          IF (ivar.eq.ipCO2) THEN
            scale=1.0_dp
          ELSE
            scale=1.0_dp/dtBIO                       ! mmole m-2 day-1
          END IF
          gtype=gfactor*r2dvar
          status=nf_fwrite2d(ng, iNLM, DIA(ng)%ncid, ifield,            &
     &                       DIA(ng)%Vid(ifield),                       &
     &                       DIA(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, scale,                 &
     &                       DIAGS(ng) % DiaBio2d(:,:,ivar),            &
     &                       SetFillVal = .FALSE.)
          IF (FoundError(status, nf90_noerr, 341, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out 3D biological diagnostic fields.
!
      DO ivar=1,NDbio3d
        ifield=iDbio3(ivar)
        IF (Dout(ifield,ng)) THEN
          scale=1.0_dp/dtBIO                         ! mmole m-3 day-1
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, iNLM, DIA(ng)%ncid, ifield,            &
     &                       DIA(ng)%Vid(ifield),                       &
     &                       DIA(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       DIAGS(ng) % DiaBio3d(:,:,:,ivar),          &
     &                       SetFillVal = .FALSE.)
          IF (FoundError(status, nf90_noerr, 370, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,ifield)), DIA(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Synchronize time-average NetCDF file to disk to allow other processes
!  to access data immediately after it is written.
!
      CALL netcdf_sync (ng, iNLM, DIA(ng)%name, DIA(ng)%ncid)
      IF (FoundError(exit_flag, NoError, 446, MyFile)) RETURN
!
  10  FORMAT (2x,'WRT_DIAGS_NF90   - writing diagnostics fields',t61,   &
     &        'in record = ',i0)
  20  FORMAT (/,' WRT_DIAGS_NF90 - error while writing variable: ',a,   &
     &        /,18x,'into diagnostics NetCDF file for time record: ',i0)
!
      RETURN
      END SUBROUTINE wrt_diags_nf90
      END MODULE wrt_diags_mod
