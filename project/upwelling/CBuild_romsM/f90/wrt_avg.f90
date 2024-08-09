      MODULE wrt_avg_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This module writes model time-averaged fields into output file      !
!  using the standard NetCDF library or the Parallel-IO (PIO) library. !
!                                                                      !
!  Notice that only momentum is affected by the full time-averaged     !
!  masks.  If applicable, these mask contains information about        !
!  river runoff and time-dependent wetting and drying variations.      !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_average
      USE mod_forces
      USE mod_grid
      USE mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
!
      USE nf_fwrite2d_mod,     ONLY : nf_fwrite2d
      USE nf_fwrite3d_mod,     ONLY : nf_fwrite3d
      USE strings_mod,         ONLY : FoundError
!
      implicit none
!
      PUBLIC  :: wrt_avg
      PRIVATE :: wrt_avg_nf90
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE wrt_avg (ng, tile)
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
     &  "ROMS/Utility/wrt_avg.F"
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
      SELECT CASE (AVG(ng)%IOtype)
        CASE (io_nf90)
          CALL wrt_avg_nf90 (ng, iNLM, tile,                            &
     &                       LBi, UBi, LBj, UBj)
        CASE DEFAULT
          IF (Master) WRITE (stdout,10) AVG(ng)%IOtype
          exit_flag=3
      END SELECT
      IF (FoundError(exit_flag, NoError, 119, MyFile)) RETURN
!
  10  FORMAT (' WRT_AVG - Illegal output file type, io_type = ',i0,     &
     &        /,15x,'Check KeyWord ''OUT_LIB'' in ''roms.in''.')
!
      RETURN
      END SUBROUTINE wrt_avg
!
!***********************************************************************
      SUBROUTINE wrt_avg_nf90 (ng, model, tile,                         &
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
!
      real(dp) :: scale
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/wrt_avg.F"//", wrt_avg_nf90"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Write out time-averaged fields when appropriate.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, 154, MyFile)) RETURN
!
!  Set grid type factor to write full (gfactor=1) fields or water
!  points (gfactor=-1) fields only.
!
      gfactor=1
!
!  Set time record index.
!
      AVG(ng)%Rindex=AVG(ng)%Rindex+1
      Fcount=AVG(ng)%load
      AVG(ng)%Nrec(Fcount)=AVG(ng)%Nrec(Fcount)+1
!
!  Report.
!
      IF (Master) WRITE (stdout,10) AVG(ng)%Rindex
!
!  Write out averaged time.
!
      CALL netcdf_put_fvar (ng, model, AVG(ng)%name,                    &
     &                      TRIM(Vname(1,idtime)), AVGtime(ng:),        &
     &                      (/AVG(ng)%Rindex/), (/1/),                  &
     &                      ncid = AVG(ng)%ncid,                        &
     &                      varid = AVG(ng)%Vid(idtime))
      IF (FoundError(exit_flag, NoError, 186, MyFile)) RETURN
!
!  Write out free-surface (m).
!
      IF (Aout(idFsur,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idFsur,             &
     &                     AVG(ng)%Vid(idFsur),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgzeta)
        IF (FoundError(status, nf90_noerr, 201, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idFsur)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D momentum component (m/s) in the XI-direction.
!
      IF (Aout(idUbar,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idUbar,             &
     &                     AVG(ng)%Vid(idUbar),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgu2d)
        IF (FoundError(status, nf90_noerr, 250, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idUbar)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idVbar,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idVbar,             &
     &                     AVG(ng)%Vid(idVbar),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgv2d)
        IF (FoundError(status, nf90_noerr, 299, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idVbar)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D Eastward momentum component (m/s) at RHO-points.
!
      IF (Aout(idu2dE,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idu2dE,             &
     &                     AVG(ng)%Vid(idu2dE),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgu2dE)
        IF (FoundError(status, nf90_noerr, 348, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idu2dE)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D Northward momentum component (m/s) at RHO-points.
!
      IF (Aout(idv2dN,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idv2dN,             &
     &                     AVG(ng)%Vid(idv2dN),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgv2dN)
        IF (FoundError(status, nf90_noerr, 371, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idv2dN)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D momentum component (m/s) in the XI-direction.
!
      IF (Aout(idUvel,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idUvel,             &
     &                     AVG(ng)%Vid(idUvel),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgu3d)
        IF (FoundError(status, nf90_noerr, 485, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idUvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D momentum component (m/s) in the ETA-direction.
!
      IF (Aout(idVvel,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idVvel,             &
     &                     AVG(ng)%Vid(idVvel),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgv3d)
        IF (FoundError(status, nf90_noerr, 534, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idVvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D Eastward momentum component (m/s) at RHO-points.
!
      IF (Aout(idu3dE,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idu3dE,             &
     &                     AVG(ng)%Vid(idu3dE),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgu3dE)
        IF (FoundError(status, nf90_noerr, 583, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idu3dE)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D Northward momentum component (m/s) at RHO-points.
!
      IF (Aout(idv3dN,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idv3dN,             &
     &                     AVG(ng)%Vid(idv3dN),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgv3dN)
        IF (FoundError(status, nf90_noerr, 606, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idv3dN)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out S-coordinate omega vertical velocity (m/s).
!
      IF (Aout(idOvel,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idOvel,             &
     &                     AVG(ng)%Vid(idOvel),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
     &                     AVERAGE(ng) % avgw3d)
        IF (FoundError(status, nf90_noerr, 629, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idOvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out "true" vertical velocity (m/s).
!
      IF (Aout(idWvel,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*w3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idWvel,             &
     &                     AVG(ng)%Vid(idWvel),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 0, N(ng), scale,         &
     &                     AVERAGE(ng) % avgwvel)
        IF (FoundError(status, nf90_noerr, 652, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idOvel)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out tracer type variables.
!
      DO itrc=1,NT(ng)
        IF (Aout(idTvar(itrc),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idTvar(itrc),     &
     &                       AVG(ng)%Tid(itrc),                         &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       AVERAGE(ng) % avgt(:,:,:,itrc))
          IF (FoundError(status, nf90_noerr, 676, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idTvar(itrc))),            &
     &                          AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out density anomaly.
!
      IF (Aout(idDano,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idDano,             &
     &                     AVG(ng)%Vid(idDano),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgrho)
        IF (FoundError(status, nf90_noerr, 730, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idDano)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D potential vorticity.
!
      IF (Aout(id2dPV,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*p2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, id2dPV,             &
     &                     AVG(ng)%Vid(id2dPV),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgpvor2d)
        IF (FoundError(status, nf90_noerr, 805, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,id2dPV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 2D relative vorticity.
!
      IF (Aout(id2dRV,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*p2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, id2dRV,             &
     &                     AVG(ng)%Vid(id2dRV),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgrvor2d)
        IF (FoundError(status, nf90_noerr, 828, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,id2dRV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D potential vorticity.
!
      IF (Aout(id3dPV,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*p3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, id3dPV,             &
     &                     AVG(ng)%Vid(id3dPV),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgpvor3d)
        IF (FoundError(status, nf90_noerr, 853, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,id3dPV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out 3D relative vorticity.
!
      IF (Aout(id3dRV,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*p3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, id3dRV,             &
     &                     AVG(ng)%Vid(id3dRV),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgrvor3d)
        IF (FoundError(status, nf90_noerr, 876, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,id3dRV)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <zeta*zeta> term.
!
      IF (Aout(idZZav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idZZav,             &
     &                     AVG(ng)%Vid(idZZav),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgZZ)
        IF (FoundError(status, nf90_noerr, 900, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idZZav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <ubar*ubar> term.
!
      IF (Aout(idU2av,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idU2av,             &
     &                     AVG(ng)%Vid(idU2av),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgU2)
        IF (FoundError(status, nf90_noerr, 923, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idU2av)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <vbar*vbar> term.
!
      IF (Aout(idV2av,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idV2av,             &
     &                     AVG(ng)%Vid(idV2av),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgV2)
        IF (FoundError(status, nf90_noerr, 946, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idV2av)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out u-volume flux.
!
      IF (Aout(idHUav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idHUav,             &
     &                     AVG(ng)%Vid(idHUav),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgHuon)
        IF (FoundError(status, nf90_noerr, 971, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idHUav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out v-volume flux.
!
      IF (Aout(idHVav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idHVav,             &
     &                     AVG(ng)%Vid(idHVav),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgHvom)
        IF (FoundError(status, nf90_noerr, 994, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idHVav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <u*u> term.
!
      IF (Aout(idUUav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*u3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idUUav,             &
     &                     AVG(ng)%Vid(idUUav),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgUU)
        IF (FoundError(status, nf90_noerr, 1017, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idUUav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <u*v> term.
!
      IF (Aout(idUVav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idUVav,             &
     &                     AVG(ng)%Vid(idUVav),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgUV)
        IF (FoundError(status, nf90_noerr, 1040, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idUVav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <v*v> term.
!
      IF (Aout(idVVav,ng)) THEN
        scale=1.0_dp
        gtype=gfactor*v3dvar
        status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idVVav,             &
     &                     AVG(ng)%Vid(idVVav),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, 1, N(ng), scale,         &
     &                     AVERAGE(ng) % avgVV)
        IF (FoundError(status, nf90_noerr, 1063, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idVVav)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out quadratic <t*t> term.
!
      DO i=1,NT(ng)
        IF (Aout(idTTav(i),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*r3dvar
          status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idTTav(i),        &
     &                       AVG(ng)%Vid(idTTav(i)),                    &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       AVERAGE(ng) % avgTT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, 1087, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idTTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out active tracer volume fluxes.
!
      DO i=1,NT(ng)
        IF (Aout(iHUTav(i),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*u3dvar
          status=nf_fwrite3d(ng, model, AVG(ng)%ncid, iHUTav(i),        &
     &                       AVG(ng)%Vid(iHUTav(i)),                    &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       AVERAGE(ng) % avgHuonT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, 1112, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,iHUTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
        IF (Aout(iHVTav(i),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*v3dvar
          status=nf_fwrite3d(ng, model, AVG(ng)%ncid, iHVTav(i),        &
     &                       AVG(ng)%Vid(iHVTav(i)),                    &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       AVERAGE(ng) % avgHvomT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, 1133, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,iHVTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out quadratic <u*t> and <v*t> terms.
!
      DO i=1,NT(ng)
        IF (Aout(idUTav(i),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*u3dvar
          status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idUTav(i),        &
     &                       AVG(ng)%Vid(idUTav(i)),                    &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       AVERAGE(ng) % avgUT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, 1158, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idUTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
!
        IF (Aout(idVTav(i),ng)) THEN
          scale=1.0_dp
          gtype=gfactor*v3dvar
          status=nf_fwrite3d(ng, model, AVG(ng)%ncid, idVTav(i),        &
     &                       AVG(ng)%Vid(idVTav(i)),                    &
     &                       AVG(ng)%Rindex, gtype,                     &
     &                       LBi, UBi, LBj, UBj, 1, N(ng), scale,       &
     &                       AVERAGE(ng) % avgVT(:,:,:,i))
          IF (FoundError(status, nf90_noerr, 1179, MyFile)) THEN
            IF (Master) THEN
              WRITE (stdout,20) TRIM(Vname(1,idVTav(i))), AVG(ng)%Rindex
            END IF
            exit_flag=3
            ioerror=status
            RETURN
          END IF
        END IF
      END DO
!
!  Write out surface net heat flux.
!
      IF (Aout(idTsur(itemp),ng)) THEN
        scale=rho0*Cp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idTsur(itemp),      &
     &                     AVG(ng)%Vid(idTsur(itemp)),                  &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgstf)
        IF (FoundError(status, nf90_noerr, 1430, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idTsur(itemp))),             &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface salt flux  (PSU m/s = kg salt/m2/s).
!
      IF (Aout(idTsur(isalt),ng)) THEN
        scale=1.0_dp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idTsur(isalt),      &
     &                     AVG(ng)%Vid(idTsur(isalt)),                  &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgswf)
        IF (FoundError(status, nf90_noerr, 1456, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idTsur(isalt))),             &
     &                        AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out net shortwave radiation flux.
!
      IF (Aout(idSrad,ng)) THEN
        scale=rho0*Cp
        gtype=gfactor*r2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idSrad,             &
     &                     AVG(ng)%Vid(idSrad),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgsrf)
        IF (FoundError(status, nf90_noerr, 1621, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idSrad)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface u-momentum stress.
!
      IF (Aout(idUsms,ng)) THEN
        scale=rho0
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idUsms,             &
     &                     AVG(ng)%Vid(idUsms),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgsus)
        IF (FoundError(status, nf90_noerr, 1655, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idUsms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out surface v-momentum stress.
!
      IF (Aout(idVsms,ng)) THEN
        scale=rho0
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idVsms,             &
     &                     AVG(ng)%Vid(idVsms),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgsvs)
        IF (FoundError(status, nf90_noerr, 1687, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idVsms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom u-momentum stress.
!
      IF (Aout(idUbms,ng)) THEN
        scale=rho0
        gtype=gfactor*u2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idUbms,             &
     &                     AVG(ng)%Vid(idUbms),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgbus)
        IF (FoundError(status, nf90_noerr, 1719, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idUbms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!  Write out bottom v-momentum stress.
!
      IF (Aout(idVbms,ng)) THEN
        scale=rho0
        gtype=gfactor*v2dvar
        status=nf_fwrite2d(ng, model, AVG(ng)%ncid, idVbms,             &
     &                     AVG(ng)%Vid(idVbms),                         &
     &                     AVG(ng)%Rindex, gtype,                       &
     &                     LBi, UBi, LBj, UBj, scale,                   &
     &                     AVERAGE(ng) % avgbvs)
        IF (FoundError(status, nf90_noerr, 1751, MyFile)) THEN
          IF (Master) THEN
            WRITE (stdout,20) TRIM(Vname(1,idVbms)), AVG(ng)%Rindex
          END IF
          exit_flag=3
          ioerror=status
          RETURN
        END IF
      END IF
!
!-----------------------------------------------------------------------
!  Synchronize time-average NetCDF file to disk to allow other processes
!  to access data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, model, AVG(ng)%name, AVG(ng)%ncid)
      IF (FoundError(exit_flag, NoError, 1815, MyFile)) RETURN
!
  10  FORMAT (2x,'WRT_AVG_NF90     - writing averaged',t42,'fields',t61,&
     &        'in record = ',i0)
  20  FORMAT (/,' WRT_AVG_NF90 - error while writing variable: ',a,     &
     &        /,16x,'into averages NetCDF file for time record: ',i0)
!
      RETURN
      END SUBROUTINE wrt_avg_nf90
      END MODULE wrt_avg_mod
