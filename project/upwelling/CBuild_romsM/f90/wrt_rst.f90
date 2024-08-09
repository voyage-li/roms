      MODULE wrt_rst_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This module writes requested model fields into the RESTART output   !
!  file using the standard NetCDF library or the Parallel-IO (PIO)     !
!  library.                                                            !
!                                                                      !
!  Notice that only momentum is affected by the full time-averaged     !
!  masks.  If applicable, these mask contains information about        !
!  river runoff and time-dependent wetting and drying variations.      !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_grid
      USE mod_iounits
      USE mod_mixing
      USE mod_ncparam
      USE mod_netcdf
      USE mod_ocean
      USE mod_scalars
      USE mod_stepping
!
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
      USE nf_fwrite3d_mod, ONLY : nf_fwrite3d
      USE strings_mod,     ONLY : FoundError
!
      implicit none
!
      PUBLIC  :: wrt_rst
      PRIVATE :: wrt_rst_nf90
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE wrt_rst (ng, tile)
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
     &  "ROMS/Utility/wrt_rst.F"
!
!-----------------------------------------------------------------------
!  Write out history fields according to IO type.
!-----------------------------------------------------------------------
!
      LBi=BOUNDS(ng)%LBi(tile)
      UBi=BOUNDS(ng)%UBi(tile)
      LBj=BOUNDS(ng)%LBj(tile)
      UBj=BOUNDS(ng)%UBj(tile)
!
      SELECT CASE (RST(ng)%IOtype)
        CASE (io_nf90)
          CALL wrt_rst_nf90 (ng, iNLM, tile,                            &
     &                       LBi, UBi, LBj, UBj)
        CASE DEFAULT
          IF (Master) WRITE (stdout,10) RST(ng)%IOtype
          exit_flag=3
      END SELECT
      IF (FoundError(exit_flag, NoError, 99, MyFile)) RETURN
!
  10  FORMAT (' WRT_RST - Illegal output file type, io_type = ',i0,     &
     &        /,11x,'Check KeyWord ''OUT_LIB'' in ''roms.in''.')
!
      RETURN
      END SUBROUTINE wrt_rst
!
!***********************************************************************
      SUBROUTINE wrt_rst_nf90 (ng, model, tile,                         &
     &                         LBi, UBi, LBj, UBj)
!***********************************************************************
!
      USE mod_netcdf
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
!
!  Local variable declarations.
!
      integer :: Fcount, gfactor, gtype, i, itrc, status
      integer :: ntmp(1)
!
      real(dp) :: scale
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/wrt_rst.F"//", wrt_rst_nf90"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Write out restart fields.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, 137, MyFile)) RETURN
!
!  Set grid type factor to write full (gfactor=1) fields or water
!  points (gfactor=-1) fields only.
!
      gfactor=1
!
!  Set time record index.
!
      RST(ng)%Rindex=RST(ng)%Rindex+1
      Fcount=RST(ng)%Fcount
      RST(ng)%Nrec(Fcount)=RST(ng)%Nrec(Fcount)+1
!
!  Report.
!
      IF (Master) WRITE (stdout,10) kstp(ng), nrhs(ng), RST(ng)%Rindex
!
!  If requested, set time index to recycle time records in restart
!  file.
!
      IF (LcycleRST(ng)) THEN
        RST(ng)%Rindex=MOD(RST(ng)%Rindex-1,2)+1
      END IF
!
!  Write out model time (s).
!
      CALL netcdf_put_fvar (ng, model, RST(ng)%name,                    &
     &                      TRIM(Vname(1,idtime)), time(ng:),           &
     &                      (/RST(ng)%Rindex/), (/1/),                  &
     &                      ncid = RST(ng)%ncid,                        &
     &                      varid = RST(ng)%Vid(idtime))
      IF (FoundError(exit_flag, NoError, 223, MyFile)) RETURN
!
!  Write out free-surface (m).
!
      scale=1.0_dp
      gtype=gfactor*r2dvar
      status=nf_fwrite2d(ng, model, RST(ng)%ncid, idFsur,               &
     &                   RST(ng)%Vid(idFsur),                           &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
     &                   OCEAN(ng) % zeta(:,:,kstp(ng)))
      IF (FoundError(status, nf90_noerr, 376, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idFsur)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out 2D momentum component (m/s) in the XI-direction.
!
      scale=1.0_dp
      gtype=gfactor*u2dvar
      status=nf_fwrite2d(ng, model, RST(ng)%ncid, idUbar,               &
     &                   RST(ng)%Vid(idUbar),                           &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
     &                   OCEAN(ng) % ubar(:,:,kstp(ng)))
      IF (FoundError(status, nf90_noerr, 434, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idUbar)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out 2D momentum component (m/s) in the ETA-direction.
!
      scale=1.0_dp
      gtype=gfactor*v2dvar
      status=nf_fwrite2d(ng, model, RST(ng)%ncid, idVbar,               &
     &                   RST(ng)%Vid(idVbar),                           &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, scale,                     &
     &                   OCEAN(ng) % vbar(:,:,kstp(ng)))
      IF (FoundError(status, nf90_noerr, 492, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idVbar)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out 3D momentum component (m/s) in the XI-direction.
!
      scale=1.0_dp
      gtype=gfactor*u3dvar
      status=nf_fwrite3d(ng, model, RST(ng)%ncid, idUvel,               &
     &                   RST(ng)%Vid(idUvel),                           &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, N(ng), scale,           &
     &                   OCEAN(ng) % u(:,:,:,nrhs(ng)))
      IF (FoundError(status, nf90_noerr, 551, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idUvel)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out momentum component (m/s) in the ETA-direction.
!
      scale=1.0_dp
      gtype=gfactor*v3dvar
      status=nf_fwrite3d(ng, model, RST(ng)%ncid, idVvel,               &
     &                   RST(ng)%Vid(idVvel),                           &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, N(ng), scale,           &
     &                   OCEAN(ng) % v(:,:,:,nrhs(ng)))
      IF (FoundError(status, nf90_noerr, 609, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idVvel)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!  Write out tracer type variables.
!
      DO itrc=1,NT(ng)
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, model, RST(ng)%ncid, idTvar(itrc),       &
     &                     RST(ng)%Tid(itrc),                           &
     &                     RST(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     OCEAN(ng) % t(:,:,:,nrhs(ng),itrc))
        IF (FoundError(status, nf90_noerr, 667, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idTvar(itrc))),              &
     &                        RST(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END DO
!
!  Write out density anomaly.
!
      scale=1.0_dp
      gtype=gfactor*r3dvar
      status=nf_fwrite3d(ng, model, RST(ng)%ncid, idDano,               &
     &                   RST(ng)%Vid(idDano),                           &
     &                   RST(ng)%Rindex, gtype,                         &
     &                   LBi, UBi, LBj, UBj, 1, N(ng), scale,           &
     &                   OCEAN(ng) % rho)
      IF (FoundError(status, nf90_noerr, 690, MyFile)) THEN
        IF (Master) THEN
          WRITE (stdout,20) TRIM(Vname(1,idDano)), RST(ng)%Rindex
        END IF
        exit_flag=3
        ioerror=status
        RETURN
      END IF
!
!-----------------------------------------------------------------------
!  Synchronize restart NetCDF file to disk.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, model, RST(ng)%name, RST(ng)%ncid)
      IF (FoundError(exit_flag, NoError, 1212, MyFile)) RETURN
!
  10  FORMAT (2x,'WRT_RST_NF90     - writing re-start', t42,            &
     &        'fields (Index=',i1,',',i1,') in record = ',i0)
  20  FORMAT (/,' WRT_RST_NF90 - error while writing variable: ',a,     &
     &        /,16x,'into restart NetCDF file for time record: ',i0)
!
      RETURN
      END SUBROUTINE wrt_rst_nf90
      END MODULE wrt_rst_mod
