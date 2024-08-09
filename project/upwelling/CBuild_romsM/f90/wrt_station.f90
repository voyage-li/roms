      MODULE wrt_station_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This module writes STATION data into output file using the standard !
!  NetCDF library or the Parallel-IO (PIO) library.                    !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_forces
      USE mod_grid
      USE mod_iounits
      USE mod_mixing
      USE mod_ncparam
      USE mod_ocean
      USE mod_scalars
      USE mod_stepping
!
      USE extract_sta_mod,     ONLY : extract_sta2d
      USE extract_sta_mod,     ONLY : extract_sta3d
      USE uv_rotate_mod,       ONLY : uv_rotate2d
      USE uv_rotate_mod,       ONLY : uv_rotate3d
      USE strings_mod,         ONLY : FoundError
!
      implicit none
!
      PUBLIC  :: wrt_station
      PRIVATE :: wrt_station_nf90
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE wrt_station (ng, tile)
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
     &  "ROMS/Utility/wrt_station.F"
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
      SELECT CASE (STA(ng)%IOtype)
        CASE (io_nf90)
          CALL wrt_station_nf90 (ng, iNLM, tile,                        &
     &                           LBi, UBi, LBj, UBj)
        CASE DEFAULT
          IF (Master) WRITE (stdout,10) STA(ng)%IOtype
          exit_flag=3
      END SELECT
      IF (FoundError(exit_flag, NoError, 117, MyFile)) RETURN
!
  10  FORMAT (' WRT_STATION - Illegal output file type, io_type = ',i0, &
     &        /,15x,'Check KeyWord ''OUT_LIB'' in ''roms.in''.')
!
      RETURN
      END SUBROUTINE wrt_station
!
!***********************************************************************
      SUBROUTINE wrt_station_nf90 (ng, model, tile,                     &
     &                             LBi, UBi, LBj, UBj)
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
      logical :: Cgrid
!
      integer :: NposB, NposR, NposW
      integer :: Fcount, i, ifield, k, np, status
!
      real(dp) :: scale
      real(r8), dimension(Nstation(ng)) :: Xpos, Ypos, Zpos, psta
      real(r8), dimension(Nstation(ng)*(N(ng))) :: XposR, YposR, ZposR
      real(r8), dimension(Nstation(ng)*(N(ng)+1)) :: XposW, YposW, ZposW
      real(r8), dimension(Nstation(ng)*(N(ng)+1)) :: rsta
      real(r8), allocatable :: Ur2d(:,:)
      real(r8), allocatable :: Vr2d(:,:)
      real(r8), allocatable :: Ur3d(:,:,:)
      real(r8), allocatable :: Vr3d(:,:,:)
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/wrt_station.F"//", wrt_station_nf90"
!
      SourceFile=MyFile
!
!-----------------------------------------------------------------------
!  Write out station data at RHO-points.
!-----------------------------------------------------------------------
!
      IF (FoundError(exit_flag, NoError, 173, MyFile)) RETURN
!
!  Set time record index.
!
      STA(ng)%Rindex=STA(ng)%Rindex+1
      Fcount=STA(ng)%Fcount
      STA(ng)%Nrec(Fcount)=STA(ng)%Nrec(Fcount)+1
!
!  Set switch to extract station data at native C-grid position (TRUE)
!  or at RHO-points (FALSE).
!
      Cgrid=.FALSE.
!
!  Set positions for generic extraction routine.
!
      NposB=Nstation(ng)*Nbed
      NposR=Nstation(ng)*N(ng)
      NposW=Nstation(ng)*(N(ng)+1)
      DO i=1,Nstation(ng)
        Xpos(i)=SCALARS(ng)%SposX(i)
        Ypos(i)=SCALARS(ng)%SposY(i)
        Zpos(i)=1.0_r8
        DO k=1,N(ng)
          np=k+(i-1)*N(ng)
          XposR(np)=SCALARS(ng)%SposX(i)
          YposR(np)=SCALARS(ng)%SposY(i)
          ZposR(np)=REAL(k,r8)
        END DO
        DO k=0,N(ng)
          np=k+1+(i-1)*(N(ng)+1)
          XposW(np)=SCALARS(ng)%SposX(i)
          YposW(np)=SCALARS(ng)%SposY(i)
          ZposW(np)=REAL(k,r8)
        END DO
      END DO
!
!  Write out model time (s).
!
      CALL netcdf_put_fvar (ng, model, STA(ng)%name,                    &
     &                      TRIM(Vname(1,idtime)), time(ng:),           &
     &                      (/STA(ng)%Rindex/), (/1/),                  &
     &                      ncid = STA(ng)%ncid,                        &
     &                      varid = STA(ng)%Vid(idtime))
      IF (FoundError(exit_flag, NoError, 230, MyFile)) RETURN
!
!  Write out free-surface (m).
!
      IF (Sout(idFsur,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, model, Cgrid, idFsur, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, OCEAN(ng)%zeta(:,:,kstp(ng)),        &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idFsur)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idFsur))
        IF (FoundError(exit_flag, NoError, 245, MyFile)) RETURN
      END IF
!
!  Write out 2D momentum component (m/s) in the XI-direction.
!
      IF (Sout(idUbar,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, model, Cgrid, idUbar, u2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, OCEAN(ng)%ubar(:,:,kstp(ng)),        &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idUbar)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idUbar))
        IF (FoundError(exit_flag, NoError, 261, MyFile)) RETURN
      END IF
!
!  Write out 2D momentum component (m/s) in the ETA-direction.
!
      IF (Sout(idVbar,ng)) THEN
        scale=1.0_dp
        CALL extract_sta2d (ng, model, Cgrid, idVbar, v2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, OCEAN(ng)%vbar(:,:,kstp(ng)),        &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idVbar)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idVbar))
        IF (FoundError(exit_flag, NoError, 277, MyFile)) RETURN
      END IF
!
!  Write out 2D Eastward and Northward momentum components (m/s) at
!  RHO-points
!
      IF (Sout(idu2dE,ng).and.Sout(idv2dN,ng)) THEN
        IF (.not.allocated(Ur2d)) THEN
          allocate (Ur2d(LBi:UBi,LBj:UBj))
          Ur2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        IF (.not.allocated(Vr2d)) THEN
          allocate (Vr2d(LBi:UBi,LBj:UBj))
          Vr2d(LBi:UBi,LBj:UBj)=0.0_r8
        END IF
        CALL uv_rotate2d (ng, tile, .FALSE., .TRUE.,                    &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    GRID(ng) % CosAngler,                         &
     &                    GRID(ng) % SinAngler,                         &
     &                    OCEAN(ng) % ubar(:,:,kstp(ng)),               &
     &                    OCEAN(ng) % vbar(:,:,kstp(ng)),               &
     &                    Ur2d, Vr2d)
!
        scale=1.0_dp
        CALL extract_sta2d (ng, model, Cgrid, idu2dE, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, Ur2d,                                &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idu2dE)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idu2dE))
        IF (FoundError(exit_flag, NoError, 313, MyFile)) RETURN
!
        CALL extract_sta2d (ng, model, Cgrid, idv2dN, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, Vr2d,                                &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idv2dN)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idv2dN))
        IF (FoundError(exit_flag, NoError, 324, MyFile)) RETURN
        deallocate (Ur2d)
        deallocate (Vr2d)
      END IF
!
!  Write out 3D momentum component (m/s) in the XI-direction.
!
      IF (Sout(idUvel,ng)) THEN
        scale=1.0_dp
        CALL extract_sta3d (ng, model, Cgrid, idUvel, u3dvar,           &
     &                      LBi, UBi, LBj, UBj, 1, N(ng),               &
     &                      scale, OCEAN(ng)%u(:,:,:,nrhs(ng)),         &
     &                      NposR, XposR, YposR, ZposR, rsta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idUvel)), rsta,              &
     &                        (/1,1,STA(ng)%Rindex/),                   &
     &                        (/N(ng),Nstation(ng),1/),                 &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idUvel))
        IF (FoundError(exit_flag, NoError, 346, MyFile)) RETURN
      END IF
!
!  Write out 3D momentum component (m/s) in the ETA-direction.
!
      IF (Sout(idVvel,ng)) THEN
        scale=1.0_dp
        CALL extract_sta3d (ng, model, Cgrid, idVvel, v3dvar,           &
     &                      LBi, UBi, LBj, UBj, 1, N(ng),               &
     &                      scale, OCEAN(ng)%v(:,:,:,nrhs(ng)),         &
     &                      NposR, XposR, YposR, ZposR, rsta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idVvel)), rsta,              &
     &                        (/1,1,STA(ng)%Rindex/),                   &
     &                        (/N(ng),Nstation(ng),1/),                 &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idVvel))
        IF (FoundError(exit_flag, NoError, 363, MyFile)) RETURN
      END IF
!
!  Write out 3D Eastward and Northward momentum components (m/s) at
!  RHO-points.
!
      IF (Sout(idu3dE,ng).and.Sout(idv3dN,ng)) THEN
        IF (.not.allocated(Ur3d)) THEN
          allocate (Ur3d(LBi:UBi,LBj:UBj,N(ng)))
          Ur3d(LBi:UBi,LBj:UBj,1:N(ng))=0.0_r8
        END IF
        IF (.not.allocated(Vr3d)) THEN
          allocate (Vr3d(LBi:UBi,LBj:UBj,N(ng)))
          Vr3d(LBi:UBi,LBj:UBj,1:N(ng))=0.0_r8
        END IF
        CALL uv_rotate3d (ng, tile, .FALSE., .TRUE.,                    &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    GRID(ng) % CosAngler,                         &
     &                    GRID(ng) % SinAngler,                         &
     &                    OCEAN(ng) % u(:,:,:,nrhs(ng)),                &
     &                    OCEAN(ng) % v(:,:,:,nrhs(ng)),                &
     &                    Ur3d, Vr3d)
!
        scale=1.0_dp
        CALL extract_sta3d (ng, model, Cgrid, idu3dE, r3dvar,           &
     &                      LBi, UBi, LBj, UBj, 1, N(ng),               &
     &                      scale, Ur3d,                                &
     &                      NposR, XposR, YposR, ZposR, rsta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idu3dE)), rsta,              &
     &                        (/1,1,STA(ng)%Rindex/),                   &
     &                        (/N(ng),Nstation(ng),1/),                 &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idu3dE))
        IF (FoundError(exit_flag, NoError, 400, MyFile)) RETURN
!
        CALL extract_sta3d (ng, model, Cgrid, idv3dN, r3dvar,           &
     &                      LBi, UBi, LBj, UBj, 1, N(ng),               &
     &                      scale, Vr3d,                                &
     &                      NposR, XposR, YposR, ZposR, rsta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idv3dN)), rsta,              &
     &                        (/1,1,STA(ng)%Rindex/),                   &
     &                        (/N(ng),Nstation(ng),1/),                 &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idv3dN))
        IF (FoundError(exit_flag, NoError, 412, MyFile)) RETURN
        deallocate (Ur3d)
        deallocate (Vr3d)
      END IF
!
!  Write out "true" vertical velocity (m/s).
!
      IF (Sout(idWvel,ng)) THEN
        scale=1.0_dp
        CALL extract_sta3d (ng, model, Cgrid, idWvel, w3dvar,           &
     &                      LBi, UBi, LBj, UBj, 0, N(ng),               &
     &                      scale, OCEAN(ng)%wvel,                      &
     &                      NposW, XposW, YposW, ZposW, rsta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idWvel)), rsta,              &
     &                        (/1,1,STA(ng)%Rindex/),                   &
     &                        (/N(ng)+1,Nstation(ng),1/),               &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idWvel))
        IF (FoundError(exit_flag, NoError, 432, MyFile)) RETURN
      END IF
!
!  Write S-coordinate "omega" vertical velocity (m3/s).
!
      IF (Sout(idOvel,ng)) THEN
        scale=1.0_dp
        CALL extract_sta3d (ng, model, Cgrid, idOvel, w3dvar,           &
     &                      LBi, UBi, LBj, UBj, 0, N(ng),               &
     &                      scale, OCEAN(ng)%W,                         &
     &                      NposW, XposW, YposW, ZposW, rsta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idOvel)), rsta,              &
     &                        (/1,1,STA(ng)%Rindex/),                   &
     &                        (/N(ng)+1,Nstation(ng),1/),               &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idOvel))
        IF (FoundError(exit_flag, NoError, 449, MyFile)) RETURN
      END IF
!
!  Write out tracer type variables.
!
      DO i=1,NT(ng)
        ifield=idTvar(i)
        IF (Sout(ifield,ng)) THEN
          scale=1.0_dp
          CALL extract_sta3d (ng, model, Cgrid, ifield, r3dvar,         &
     &                        LBi, UBi, LBj, UBj, 1, N(ng),             &
     &                        scale, OCEAN(ng)%t(:,:,:,nrhs(ng),i),     &
     &                        NposR, XposR, YposR, ZposR, rsta)
          CALL netcdf_put_fvar (ng, model, STA(ng)%name,                &
     &                          TRIM(Vname(1,idTvar(i))), rsta,         &
     &                          (/1,1,STA(ng)%Rindex/),                 &
     &                          (/N(ng),Nstation(ng),1/),               &
     &                          ncid = STA(ng)%ncid,                    &
     &                          varid = STA(ng)%Tid(i))
          IF (FoundError(exit_flag, NoError, 468, MyFile)) RETURN
        END IF
      END DO
!
!  Write out density anomaly.
!
      IF (Sout(idDano,ng)) THEN
        scale=1.0_dp
        CALL extract_sta3d (ng, model, Cgrid, idDano, r3dvar,           &
     &                      LBi, UBi, LBj, UBj, 1, N(ng),               &
     &                      scale, OCEAN(ng)%rho,                       &
     &                      NposR, XposR, YposR, ZposR, rsta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idDano)), rsta,              &
     &                        (/1,1,STA(ng)%Rindex/),                   &
     &                        (/N(ng),Nstation(ng),1/),                 &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idDano))
        IF (FoundError(exit_flag, NoError, 486, MyFile)) RETURN
      END IF
!
!  Write out vertical viscosity coefficient.
!
      IF (Sout(idVvis,ng)) THEN
        scale=1.0_dp
        CALL extract_sta3d (ng, model, Cgrid, idVvis, w3dvar,           &
     &                      LBi, UBi, LBj, UBj, 0, N(ng),               &
     &                      scale, MIXING(ng)%Akv,                      &
     &                      NposW, XposW, YposW, ZposW, rsta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idVvis)), rsta,              &
     &                        (/1,1,STA(ng)%Rindex/),                   &
     &                        (/N(ng)+1,Nstation(ng),1/),               &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idVvis))
        IF (FoundError(exit_flag, NoError, 540, MyFile)) RETURN
      END IF
!
!  Write out vertical diffusion coefficient for potential temperature.
!
      IF (Sout(idTdif,ng)) THEN
        scale=1.0_dp
        CALL extract_sta3d (ng, model, Cgrid, idTdif, w3dvar,           &
     &                      LBi, UBi, LBj, UBj, 0, N(ng),               &
     &                      scale, MIXING(ng)%Akt(:,:,:,itemp),         &
     &                      NposW, XposW, YposW, ZposW, rsta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idTdif)), rsta,              &
     &                        (/1,1,STA(ng)%Rindex/),                   &
     &                        (/N(ng)+1,Nstation(ng),1/),               &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idTdif))
        IF (FoundError(exit_flag, NoError, 557, MyFile)) RETURN
      END IF
!
!  Write out vertical diffusion coefficient for salinity.
!
      IF (Sout(idSdif,ng)) THEN
        scale=1.0_dp
        CALL extract_sta3d (ng, model, Cgrid, idSdif, w3dvar,           &
     &                      LBi, UBi, LBj, UBj, 0, N(ng),               &
     &                      scale, MIXING(ng)%Akt(:,:,:,isalt),         &
     &                      NposW, XposW, YposW, ZposW, rsta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idSdif)), rsta,              &
     &                        (/1,1,STA(ng)%Rindex/),                   &
     &                        (/N(ng)+1,Nstation(ng),1/),               &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idSdif))
        IF (FoundError(exit_flag, NoError, 576, MyFile)) RETURN
      END IF
!
!  Write out surface net heat flux.
!
      IF (Sout(idTsur(itemp),ng)) THEN
        ifield=idTsur(itemp)
        scale=rho0*Cp
        CALL extract_sta2d (ng, model, Cgrid, ifield, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%stflx(:,:,itemp),         &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,ifield)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(ifield))
        IF (FoundError(exit_flag, NoError, 730, MyFile)) RETURN
      END IF
!
!  Write out surface salt flux.
!
      IF (Sout(idTsur(isalt),ng)) THEN
        ifield=idTsur(isalt)
        scale=1.0_dp
        CALL extract_sta2d (ng, model, Cgrid, ifield, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%stflx(:,:,isalt),         &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,ifield)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(ifield))
        IF (FoundError(exit_flag, NoError, 749, MyFile)) RETURN
      END IF
!
!  Write out shortwave radiation flux.
!
      IF (Sout(idSrad,ng)) THEN
        scale=rho0*Cp
        CALL extract_sta2d (ng, model, Cgrid, idSrad, r2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%srflx,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idSrad)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idSrad))
        IF (FoundError(exit_flag, NoError, 819, MyFile)) RETURN
      END IF
!
!  Write out surface U-momentum stress.
!
      IF (Sout(idUsms,ng)) THEN
        scale=rho0
        CALL extract_sta2d (ng, model, Cgrid, idUsms, u2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%sustr,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idUsms)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idUsms))
        IF (FoundError(exit_flag, NoError, 887, MyFile)) RETURN
      END IF
!
!  Write out surface V-momentum stress.
!
      IF (Sout(idVsms,ng)) THEN
        scale=rho0
        CALL extract_sta2d (ng, model, Cgrid, idVsms, v2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%svstr,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idVsms)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idVsms))
        IF (FoundError(exit_flag, NoError, 903, MyFile)) RETURN
      END IF
!
!  Write out bottom U-momentum stress.
!
      IF (Sout(idUbms,ng)) THEN
        scale=-rho0
        CALL extract_sta2d (ng, model, Cgrid, idUbms, u2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%bustr,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idUbms)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idUbms))
        IF (FoundError(exit_flag, NoError, 919, MyFile)) RETURN
      END IF
!
!  Write out bottom V-momentum stress.
!
      IF (Sout(idVbms,ng)) THEN
        scale=-rho0
        CALL extract_sta2d (ng, model, Cgrid, idVbms, v2dvar,           &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      scale, FORCES(ng)%bvstr,                    &
     &                      Nstation(ng), Xpos, Ypos, psta)
        CALL netcdf_put_fvar (ng, model, STA(ng)%name,                  &
     &                        TRIM(Vname(1,idVbms)), psta,              &
     &                        (/1,STA(ng)%Rindex/), (/Nstation(ng),1/), &
     &                        ncid = STA(ng)%ncid,                      &
     &                        varid = STA(ng)%Vid(idVbms))
        IF (FoundError(exit_flag, NoError, 935, MyFile)) RETURN
      END IF
!
!-----------------------------------------------------------------------
!  Synchronize stations NetCDF file to disk.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, model, STA(ng)%name, STA(ng)%ncid)
      RETURN
      END SUBROUTINE wrt_station_nf90
      END MODULE wrt_station_mod
