      MODULE wrt_info_mod
!
!git $Id$
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2024 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                                               !
!=======================================================================
!                                                                      !
!  This routine defines information variables in requested NetCDF      !
!  file.                                                               !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_parallel
      USE mod_biology
      USE mod_grid
      Use mod_iounits
      USE mod_ncparam
      USE mod_netcdf
      USE mod_scalars
      USE mod_sources
!
      USE extract_sta_mod, ONLY : extract_sta2d
      USE nf_fwrite2d_mod, ONLY : nf_fwrite2d
      USE strings_mod,     ONLY : FoundError, find_string
!
      implicit none
!
      INTERFACE wrt_info
        MODULE PROCEDURE wrt_info_nf90
      END INTERFACE wrt_info
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE wrt_info_nf90 (ng, model, ncid, ncname)
!***********************************************************************
!                                                                      !
!  This routine writes out information variables into requested        !
!  NetCDF file using the standard NetCDF-3 or NetCDF-4 library.        !
!                                                                      !
!  On Input:                                                           !
!                                                                      !
!     ng           Nested grid number (integer)                        !
!     model        Calling model identifier (integer)                  !
!     ncid         NetCDF file ID (integer)                            !
!     ncname       NetCDF filename (string)                            !
!                                                                      !
!  On Output:                                                          !
!                                                                      !
!     exit_flag    Error flag (integer) stored in MOD_SCALARS          !
!     ioerror      NetCDF return code (integer) stored in MOD_IOUNITS  !
!                                                                      !
!***********************************************************************
!
      USE distribute_mod,  ONLY : mp_bcasti
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, model, ncid
!
      character (len=*), intent(in) :: ncname
!
!  Local variable declarations.
!
      logical :: Cgrid = .TRUE.
!
      integer :: LBi, UBi, LBj, UBj
      integer :: i, j, k, ibry, ilev, itrc, status, varid
      integer, dimension(2) :: ibuffer
      integer :: ifield = 0
!
      real(dp) :: scale
      real(r8), dimension(NT(ng)) :: nudg
      real(r8), dimension(NT(ng),4) :: Tobc
      real(r8), dimension(Nstation(ng)) :: Zpos, wrk
!
      character (len=*), parameter :: MyFile =                          &
     &  "ROMS/Utility/wrt_info.F"//", wrt_info_nf90"
!
      SourceFile=MyFile
!
      IF (ncid.ne.XTR(ng)%ncid) THEN
        LBi=LBOUND(GRID(ng)%h,DIM=1)
        UBi=UBOUND(GRID(ng)%h,DIM=1)
        LBj=LBOUND(GRID(ng)%h,DIM=2)
        UBj=UBOUND(GRID(ng)%h,DIM=2)
      END IF
!
!-----------------------------------------------------------------------
!  Write out running parameters.
!-----------------------------------------------------------------------
!
!  Inquire about the variables.
!
      CALL netcdf_inq_var (ng, model, ncname, ncid)
      IF (FoundError(exit_flag, NoError, 144, MyFile)) RETURN
!
!  Time stepping parameters.
!
      CALL netcdf_put_ivar (ng, model, ncname, 'ntimes',                &
     &                      ntimes(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 151, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ndtfast',               &
     &                      ndtfast(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 156, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'dt',                    &
     &                      dt(ng), (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 161, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'dtfast',                &
     &                      dtfast(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 166, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'dstart',                &
     &                      dstart, (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 171, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'nHIS',                  &
     &                      nHIS(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 205, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ndefHIS',               &
     &                      ndefHIS(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 210, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'nRST',                  &
     &                      nRST(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 232, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ntsAVG',                &
     &                      ntsAVG(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 241, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'nAVG',                  &
     &                      nAVG(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 246, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ndefAVG',               &
     &                      ndefAVG(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 251, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ntsDIA',                &
     &                      ntsDIA(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 333, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'nDIA',                  &
     &                      nDIA(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 338, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'ndefDIA',               &
     &                      ndefDIA(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 343, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'nSTA',                  &
     &                      nSTA(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 350, MyFile)) RETURN
!
!  Power-law shape filter parameters for time-averaging of barotropic
!  fields.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'Falpha',                &
     &                      Falpha, (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 373, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Fbeta',                 &
     &                      Fbeta, (/0/), (/0/),                        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 378, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Fgamma',                &
     &                      Fgamma, (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 383, MyFile)) RETURN
!
!  Horizontal mixing coefficients.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'nl_visc2',              &
     &                      nl_visc2(ng), (/0/), (/0/),                 &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 444, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'LuvSponge',             &
     &                      LuvSponge(ng), (/0/), (/0/),                &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 502, MyFile)) RETURN
!
!  Background vertical mixing coefficients.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'Akt_bak',               &
     &                      Akt_bak(:,ng), (/1/), (/NT(ng)/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 519, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Akv_bak',               &
     &                      Akv_bak(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 524, MyFile)) RETURN
!
!  Drag coefficients.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'rdrg',                  &
     &                      rdrg(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 577, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'rdrg2',                 &
     &                      rdrg2(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 582, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Zob',                   &
     &                      Zob(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 588, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Zos',                   &
     &                      Zos(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 593, MyFile)) RETURN
!
!  Nudging inverse time scales used in various tasks.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'Znudg',                 &
     &                      Znudg(ng)/sec2day, (/0/), (/0/),            &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 692, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'M2nudg',                &
     &                      M2nudg(ng)/sec2day, (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 697, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'M3nudg',                &
     &                      M3nudg(ng)/sec2day, (/0/), (/0/),           &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 703, MyFile)) RETURN
      DO itrc=1,NT(ng)
        nudg(itrc)=Tnudg(itrc,ng)/sec2day
      END DO
      CALL netcdf_put_fvar (ng, model, ncname, 'Tnudg',                 &
     &                      nudg, (/1/), (/NT(ng)/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 711, MyFile)) RETURN
!
!  Open boundary nudging, inverse time scales.
!
      IF (NudgingCoeff(ng)) THEN
        CALL netcdf_put_fvar (ng, model, ncname, 'FSobc_in',            &
     &                        FSobc_in(ng,:), (/1/), (/4/),             &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 722, MyFile)) RETURN
        CALL netcdf_put_fvar (ng, model, ncname, 'FSobc_out',           &
     &                        FSobc_out(ng,:), (/1/), (/4/),            &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 727, MyFile)) RETURN
        CALL netcdf_put_fvar (ng, model, ncname, 'M2obc_in',            &
     &                        M2obc_in(ng,:), (/1/), (/4/),             &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 732, MyFile)) RETURN
        CALL netcdf_put_fvar (ng, model, ncname, 'M2obc_out',           &
     &                        M2obc_out(ng,:), (/1/), (/4/),            &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 737, MyFile)) RETURN
        DO ibry=1,4
          DO itrc=1,NT(ng)
            Tobc(itrc,ibry)=Tobc_in(itrc,ng,ibry)
          END DO
        END DO
        CALL netcdf_put_fvar (ng, model, ncname, 'Tobc_in',             &
     &                        Tobc, (/1,1/), (/NT(ng),4/),              &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 748, MyFile)) RETURN
        DO ibry=1,4
          DO itrc=1,NT(ng)
            Tobc(itrc,ibry)=Tobc_out(itrc,ng,ibry)
          END DO
        END DO
        CALL netcdf_put_fvar (ng, model, ncname, 'Tobc_out',            &
     &                        Tobc, (/1,1/), (/NT(ng),4/),              &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 758, MyFile)) RETURN
        CALL netcdf_put_fvar (ng, model, ncname, 'M3obc_in',            &
     &                        M3obc_in(ng,:), (/1/), (/4/),             &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 763, MyFile)) RETURN
        CALL netcdf_put_fvar (ng, model, ncname, 'M3obc_out',           &
     &                        M3obc_out(ng,:), (/1/), (/4/),            &
     &                      ncid = ncid)
        IF (FoundError(exit_flag, NoError, 768, MyFile)) RETURN
      END IF
!
!  Equation of State parameters.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'rho0',                  &
     &                      rho0, (/0/), (/0/),                         &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 778, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'R0',                    &
     &                      R0(ng), (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 792, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Tcoef',                 &
     &                      Tcoef(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 797, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Scoef',                 &
     &                      Scoef(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 802, MyFile)) RETURN
!
!  Slipperiness parameters.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'gamma2',                &
     &                      gamma2(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 826, MyFile)) RETURN
!
! Logical switches to activate horizontal momentum transport
! point Sources/Sinks (like river runoff transport) and mass point
! Sources/Sinks (like volume vertical influx).
!
      CALL netcdf_put_lvar (ng, model, ncname, 'LuvSrc',                &
     &                      LuvSrc(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 835, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'LwSrc',                 &
     &                      LwSrc(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 840, MyFile)) RETURN
!
!  Logical switches to activate tracer point Sources/Sinks.
!
      CALL netcdf_put_lvar (ng, model, ncname, 'LtracerSrc',            &
     &                      LtracerSrc(:,ng), (/1/), (/NT(ng)/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 849, MyFile)) RETURN
!
!  Logical switches to process climatology fields.
!
      CALL netcdf_put_lvar (ng, model, ncname, 'LsshCLM',               &
     &                      LsshCLM(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 857, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'Lm2CLM',                &
     &                      Lm2CLM(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 862, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'Lm3CLM',                &
     &                      Lm3CLM(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 868, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'LtracerCLM',            &
     &                      LtracerCLM(:,ng), (/1/), (/NT(ng)/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 873, MyFile)) RETURN
!
!  Logical switches for nudging climatology fields.
!
      CALL netcdf_put_lvar (ng, model, ncname, 'LnudgeM2CLM',           &
     &                      LnudgeM2CLM(ng), (/0/), (/0/),              &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 881, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'LnudgeM3CLM',           &
     &                      LnudgeM3CLM(ng), (/0/), (/0/),              &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 887, MyFile)) RETURN
      CALL netcdf_put_lvar (ng, model, ncname, 'LnudgeTCLM',            &
     &                      LnudgeTCLM(:,ng), (/1/), (/NT(ng)/),        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 892, MyFile)) RETURN
!
!  Write out Fennel et al. (2006) ecosystem model parameters.
!
      CALL netcdf_put_ivar (ng, model, ncname, 'BioIter',               &
     &                      BioIter(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 21, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'AttSW',                 &
     &                      AttSW(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 26, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'AttChl',                &
     &                      AttChl(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 31, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'PARfrac',               &
     &                      PARfrac(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 36, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Vp0',                   &
     &                      Vp0(ng), (/0/), (/0/),                      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 41, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'I_thNH4',               &
     &                      I_thNH4(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 46, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'D_p5NH4',               &
     &                      D_p5NH4(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 51, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'NitriR',                &
     &                      NitriR(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 56, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'K_NO3',                 &
     &                      K_NO3(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 61, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'K_NH4',                 &
     &                      K_NH4(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 66, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'K_PO4',                 &
     &                      K_PO4(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 71, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'K_Phy',                 &
     &                      K_Phy(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 76, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Chl2C_m',               &
     &                      Chl2C_m(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 81, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'ChlMin',                &
     &                      ChlMin(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 86, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'PhyCN',                 &
     &                      PhyCN(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 91, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'R_P2N',                 &
     &                      R_P2N(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 96, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'PhyIP',                 &
     &                      PhyIP(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 101, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'PhyIS',                 &
     &                      PhyIS(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 106, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'PhyMin',                &
     &                      PhyMin(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 111, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'PhyMR',                 &
     &                      PhyMR(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 116, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'ZooAE_N',               &
     &                      ZooAE_N(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 121, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'ZooBM',                 &
     &                      ZooBM(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 126, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'ZooCN',                 &
     &                      ZooCN(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 131, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'ZooER',                 &
     &                      ZooER(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 136, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'ZooGR',                 &
     &                      ZooGR(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 141, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'ZooMin',                &
     &                      ZooMin(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 146, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'ZooMR',                 &
     &                      ZooMR(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 151, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'LDeRRN',                &
     &                      LDeRRN(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 156, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'LDeRRC',                &
     &                      LDeRRC(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 161, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'CoagR',                 &
     &                      CoagR(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 166, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'SDeRRN',                &
     &                      SDeRRN(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 171, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'SDeRRC',                &
     &                      SDeRRC(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 176, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'RDeRRN',                &
     &                      RDeRRN(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 181, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'RDeRRC',                &
     &                      RDeRRC(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 186, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'wPhy',                  &
     &                      wPhy(ng), (/0/), (/0/),                     &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 191, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'wLDet',                 &
     &                      wLDet(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 196, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'wSDet',                 &
     &                      wSDet(ng), (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 201, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'pCO2air',               &
     &                      pCO2air(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 206, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Write out grid variables.
!-----------------------------------------------------------------------
!
!  Grid type switch. Writing characters in parallel I/O is extremely
!  inefficient.  It is better to write this as an integer switch:
!  0=Cartesian, 1=spherical.
!
      CALL netcdf_put_lvar (ng, model, ncname, 'spherical',             &
     &                      spherical, (/0/), (/0/),                    &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1391, MyFile)) RETURN
!
!  Domain Length.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'xl',                    &
     &                      xl(ng), (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1398, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'el',                    &
     &                      el(ng), (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1403, MyFile)) RETURN
!
!  S-coordinate parameters.
!
      CALL netcdf_put_ivar (ng, model, ncname, 'Vtransform',            &
     &                      Vtransform(ng), (/0/), (/0/),               &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1412, MyFile)) RETURN
      CALL netcdf_put_ivar (ng, model, ncname, 'Vstretching',           &
     &                      Vstretching(ng), (/0/), (/0/),              &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1417, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'theta_s',               &
     &                      theta_s(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1422, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'theta_b',               &
     &                      theta_b(ng), (/0/), (/0/),                  &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1427, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Tcline',                &
     &                      Tcline(ng), (/0/), (/0/),                   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1432, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'hc',                    &
     &                      hc(ng), (/0/), (/0/),                       &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1437, MyFile)) RETURN
!
!  SGRID conventions for staggered data on structured grids. The value
!  is arbitrary but is set to unity so it can be used as logical during
!  post-processing.
!
      CALL netcdf_put_ivar (ng, model, ncname, 'grid',                  &
     &                      (/1/), (/0/), (/0/),                        &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1446, MyFile)) RETURN
!
!  S-coordinate non-dimensional independent variables.
!
      CALL netcdf_put_fvar (ng, model, ncname, 's_rho',                 &
     &                      SCALARS(ng)%sc_r(:), (/1/), (/N(ng)/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1453, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 's_w',                   &
     &                      SCALARS(ng)%sc_w(0:), (/1/), (/N(ng)+1/),   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1458, MyFile)) RETURN
!
!  S-coordinate non-dimensional stretching curves.
!
      CALL netcdf_put_fvar (ng, model, ncname, 'Cs_r',                  &
     &                      SCALARS(ng)%Cs_r(:), (/1/), (/N(ng)/),      &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1465, MyFile)) RETURN
      CALL netcdf_put_fvar (ng, model, ncname, 'Cs_w',                  &
     &                      SCALARS(ng)%Cs_w(0:), (/1/), (/N(ng)+1/),   &
     &                      ncid = ncid)
      IF (FoundError(exit_flag, NoError, 1470, MyFile)) RETURN
!
!  User generic parameters.
!
      IF (Nuser.gt.0) THEN
        CALL netcdf_put_fvar (ng, model, ncname, 'user',                &
     &                        user, (/1/), (/Nuser/),                   &
     &                        ncid = ncid)
        IF (FoundError(exit_flag, NoError, 1479, MyFile)) RETURN
      END IF
!
!  Stations positions.
!
      IF (ncid.eq.STA(ng)%ncid) THEN
        CALL netcdf_put_fvar (ng, model, ncname, 'Ipos',                &
     &                        SCALARS(ng)%SposX(:), (/1/),              &
     &                        (/Nstation(ng)/), ncid = ncid)
        IF (FoundError(exit_flag, NoError, 1490, MyFile)) RETURN
        CALL netcdf_put_fvar (ng, model, ncname, 'Jpos',                &
     &                        SCALARS(ng)%SposY(:), (/1/),              &
     &                        (/Nstation(ng)/), ncid = ncid)
        IF (FoundError(exit_flag, NoError, 1495, MyFile)) RETURN
      END IF
!
!-----------------------------------------------------------------------
!  Write out grid tiled variables.
!-----------------------------------------------------------------------
!
      GRID_VARS : IF (ncid.ne.FLT(ng)%ncid) THEN
!
!  Bathymetry.
!
        IF (exit_flag.eq.NoError) THEN
          scale=1.0_dp
          IF ((ncid.ne.STA(ng)%ncid).and.                               &
     &        (ncid.ne.XTR(ng)%ncid)) THEN
            IF (find_string(var_name, n_var, TRIM(Vname(1,idtopo)),     &
     &                      varid)) THEN
              status=nf_fwrite2d(ng, model, ncid, idtopo,               &
     &                           varid, 0, r2dvar,                      &
     &                           LBi, UBi, LBj, UBj, scale,             &
     &                           GRID(ng) % h,                          &
     &                           SetFillVal = .FALSE.)
              IF (FoundError(status, nf90_noerr, 1526, MyFile)) THEN
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idtopo)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=status
              END IF
            ELSE
              IF (Master) WRITE (stdout,20) TRIM(Vname(1,idtopo)),      &
     &                                      TRIM(ncname)
              exit_flag=3
              ioerror=nf90_enotvar
            END IF
          ELSE IF (ncid.eq.STA(ng)%ncid) THEN
            CALL extract_sta2d (ng, model, Cgrid, ifield, r2dvar,       &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          scale, GRID(ng)%h,                      &
     &                          Nstation(ng), SCALARS(ng)%SposX,        &
     &                          SCALARS(ng)%SposY, wrk)
            CALL netcdf_put_fvar (ng, model, ncname, Vname(1,idtopo),   &
     &                            wrk, (/1/), (/Nstation(ng)/),         &
     &                            ncid = ncid)
            IF (FoundError(exit_flag, NoError, 1574, MyFile)) RETURN
          END IF
        END IF
!
!  Coriolis parameter.
!
        IF (exit_flag.eq.NoError) THEN
          IF ((ncid.ne.STA(ng)%ncid).and.                               &
     &        (ncid.ne.XTR(ng)%ncid)) THEN
            scale=1.0_dp
            IF (find_string(var_name, n_var, TRIM(Vname(1,idfcor)),     &
     &                      varid)) THEN
              status=nf_fwrite2d(ng, model, ncid, idfcor,               &
     &                           varid, 0, r2dvar,                      &
     &                           LBi, UBi, LBj, UBj, scale,             &
     &                           GRID(ng) % f,                          &
     &                           SetFillVal = .FALSE.)
              IF (FoundError(status, nf90_noerr, 1596, MyFile)) THEN
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idfcor)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=status
              END IF
            ELSE
              IF (Master) WRITE (stdout,20) TRIM(Vname(1,idfcor)),      &
     &                                      TRIM(ncname)
              exit_flag=3
              ioerror=nf90_enotvar
            END IF
          END IF
        END IF
!
!  Curvilinear transformation metrics.
!
        IF (exit_flag.eq.NoError) THEN
          IF ((ncid.ne.STA(ng)%ncid).and.                               &
     &        (ncid.ne.XTR(ng)%ncid)) THEN
            scale=1.0_dp
            IF (find_string(var_name, n_var, TRIM(Vname(1,idpmdx)),     &
     &                      varid)) THEN
              status=nf_fwrite2d(ng, model, ncid, idpmdx,               &
     &                           varid, 0, r2dvar,                      &
     &                           LBi, UBi, LBj, UBj, scale,             &
     &                           GRID(ng) % pm,                         &
     &                           SetFillVal = .FALSE.)
              IF (FoundError(status, nf90_noerr, 1654, MyFile)) THEN
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idpmdx)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=status
              END IF
            ELSE
              IF (Master) WRITE (stdout,20) TRIM(Vname(1,idpmdx)),      &
     &                                      TRIM(ncname)
              exit_flag=3
              ioerror=nf90_enotvar
            END IF
          END IF
        END IF
!
        IF (exit_flag.eq.NoError) THEN
          IF ((ncid.ne.STA(ng)%ncid).and.                               &
     &        (ncid.ne.XTR(ng)%ncid)) THEN
            scale=1.0_dp
            IF (find_string(var_name, n_var, TRIM(Vname(1,idpndy)),     &
     &                      varid)) THEN
              status=nf_fwrite2d(ng, model, ncid, idpndy,               &
     &                           varid, 0, r2dvar,                      &
     &                           LBi, UBi, LBj, UBj, scale,             &
     &                           GRID(ng) % pn,                         &
     &                           SetFillVal = .FALSE.)
              IF (FoundError(status, nf90_noerr, 1710, MyFile)) THEN
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idpndy)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=status
              END IF
            ELSE
              IF (Master) WRITE (stdout,20) TRIM(Vname(1,idpndy)),      &
     &                                      TRIM(ncname)
              exit_flag=3
              ioerror=nf90_enotvar
            END IF
          END IF
        END IF
!
!  Grid coordinates of RHO-points.
!
        IF (spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            scale=1.0_dp
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLonR)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLonR,             &
     &                             varid, 0, r2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % lonr,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1770, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLonR)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLonR)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            ELSE IF (ncid.eq.STA(ng)%ncid) THEN
              CALL extract_sta2d (ng, model, Cgrid, ifield, r2dvar,     &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            scale, GRID(ng)%lonr,                 &
     &                            Nstation(ng), SCALARS(ng)%SposX,      &
     &                            SCALARS(ng)%SposY, wrk)
              CALL netcdf_put_fvar (ng, model, ncname, Vname(1,idLonR), &
     &                              wrk, (/1/), (/Nstation(ng)/),       &
     &                              ncid = ncid)
              IF (FoundError(exit_flag, NoError,                        &
     &                       1820, MyFile)) RETURN
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            scale=1.0_dp
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLatR)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLatR,             &
     &                             varid, 0, r2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % latr,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1840, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLatR)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLatR)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            ELSE IF (ncid.eq.STA(ng)%ncid) THEN
              CALL extract_sta2d (ng, model, Cgrid, ifield, r2dvar,     &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            scale, GRID(ng)%latr,                 &
     &                            Nstation(ng), SCALARS(ng)%SposX,      &
     &                            SCALARS(ng)%SposY, wrk)
              CALL netcdf_put_fvar (ng, model, ncname, Vname(1,idLatR), &
     &                              wrk, (/1/), (/Nstation(ng)/),       &
     &                              ncid = ncid)
              IF (FoundError(exit_flag, NoError,                        &
     &                       1890, MyFile)) RETURN
            END IF
          END IF
        END IF
!
        IF (.not.spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            scale=1.0_dp
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              IF (find_string(var_name, n_var, TRIM(Vname(1,idXgrR)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idXgrR,             &
     &                             varid, 0, r2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % xr,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1912, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idXgrR)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idXgrR)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            ELSE IF (ncid.eq.STA(ng)%ncid) THEN
              CALL extract_sta2d (ng, model, Cgrid, ifield, r2dvar,     &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            scale, GRID(ng)%xr,                   &
     &                            Nstation(ng), SCALARS(ng)%SposX,      &
     &                            SCALARS(ng)%SposY, wrk)
              CALL netcdf_put_fvar (ng, model, ncname, Vname(1,idXgrR), &
     &                              wrk, (/1/), (/Nstation(ng)/),       &
     &                              ncid = ncid)
              IF (FoundError(exit_flag, NoError,                        &
     &                       1962, MyFile)) RETURN
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            scale=1.0_dp
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              IF (find_string(var_name, n_var, TRIM(Vname(1,idYgrR)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idYgrR,             &
     &                             varid, 0, r2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % yr,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         1982, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idYgrR)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idYgrR)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            ELSE IF (ncid.eq.STA(ng)%ncid) THEN
              CALL extract_sta2d (ng, model, Cgrid, ifield, r2dvar,     &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            scale, GRID(ng)%yr,                   &
     &                            Nstation(ng), SCALARS(ng)%SposX,      &
     &                            SCALARS(ng)%SposY, wrk)
              CALL netcdf_put_fvar (ng, model, ncname, Vname(1,idYgrR), &
     &                              wrk, (/1/), (/Nstation(ng)/),       &
     &                              ncid = ncid)
              IF (FoundError(exit_flag, NoError,                        &
     &                       2032, MyFile)) RETURN
            END IF
          END IF
        END IF
!
!  Grid coordinates of U-points.
!
        IF (spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLonU)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLonU,             &
     &                             varid, 0, u2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % lonu,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2056, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLonU)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLonU)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLatU)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLatU,             &
     &                             varid, 0, u2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % latu,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2114, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLatU)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLatU)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
        IF (.not.spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idXgrU)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idXgrU,             &
     &                             varid, 0, u2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % xu,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2174, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idXgrU)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idXgrU)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idYgrU)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idYgrU,             &
     &                             varid, 0, u2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % yu,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2232, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idYgrU)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idYgrU)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
!  Grid coordinates of V-points.
!
        IF (spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLonV)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLonV,             &
     &                             varid, 0, v2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % lonv,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2294, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLonV)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLonV)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLatV)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLatV,             &
     &                             varid, 0, v2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % latv,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2352, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLatV)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLatV)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
        IF (.not.spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idXgrV)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idXgrV,             &
     &                             varid, 0, v2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % xv,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2412, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idXgrV)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idXgrV)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idYgrV)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idYgrV,             &
     &                             varid, 0, v2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % yv,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2470, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idYgrV)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idYgrV)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
!  Grid coordinates of PSI-points.
!
        IF (spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLonP)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLonP,             &
     &                             varid, 0, p2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % lonp,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2532, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLonP)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLonP)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idLatP)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idLatP,             &
     &                             varid, 0, p2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % latp,                     &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2590, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idLatP)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idLatP)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
        IF (.not.spherical) THEN
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idXgrP)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idXgrP,             &
     &                             varid, 0, p2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % xp,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2650, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idXgrP)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idXgrP)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
!
          IF (exit_flag.eq.NoError) THEN
            IF ((ncid.ne.STA(ng)%ncid).and.                             &
     &          (ncid.ne.XTR(ng)%ncid)) THEN
              scale=1.0_dp
              IF (find_string(var_name, n_var, TRIM(Vname(1,idYgrP)),   &
     &                        varid)) THEN
                status=nf_fwrite2d(ng, model, ncid, idYgrP,             &
     &                             varid, 0, p2dvar,                    &
     &                             LBi, UBi, LBj, UBj, scale,           &
     &                             GRID(ng) % yp,                       &
     &                             SetFillVal = .FALSE.)
                IF (FoundError(status, nf90_noerr,                      &
     &                         2708, MyFile)) THEN
                  IF (Master) WRITE (stdout,10) TRIM(Vname(1,idYgrP)),  &
     &                                          TRIM(ncname)
                  exit_flag=3
                  ioerror=status
                END IF
              ELSE
                IF (Master) WRITE (stdout,20) TRIM(Vname(1,idYgrP)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=nf90_enotvar
              END IF
            END IF
          END IF
        END IF
!
!  Spatially bottom friction parameter.
!
        IF (exit_flag.eq.NoError) THEN
          IF ((ncid.ne.STA(ng)%ncid).and.                               &
     &        (ncid.ne.XTR(ng)%ncid)) THEN
            scale=1.0_dp
          END IF
          IF ((ncid.ne.STA(ng)%ncid).and.                               &
     &        (ncid.ne.XTR(ng)%ncid)) THEN
            IF (find_string(var_name, n_var, TRIM(Vname(1,idragL)),     &
     &                      varid)) THEN
              status=nf_fwrite2d(ng, model, ncid, idragL,               &
     &                           varid, 0, r2dvar,                      &
     &                           LBi, UBi, LBj, UBj, scale,             &
     &                           GRID(ng) % rdrag,                      &
     &                           SetFillVal = .FALSE.)
              IF (FoundError(status, nf90_noerr, 3287, MyFile)) THEN
                IF (Master) WRITE (stdout,10) TRIM(Vname(1,idragL)),    &
     &                                        TRIM(ncname)
                exit_flag=3
                ioerror=status
              END IF
            ELSE
              IF (Master) WRITE (stdout,20) TRIM(Vname(1,idragL)),      &
     &                                      TRIM(ncname)
              exit_flag=3
              ioerror=nf90_enotvar
            END IF
          END IF
        END IF
      END IF GRID_VARS
!
!-----------------------------------------------------------------------
!  Synchronize NetCDF file to disk to allow other processes to access
!  data immediately after it is written.
!-----------------------------------------------------------------------
!
      CALL netcdf_sync (ng, model, ncname, ncid)
      IF (FoundError(exit_flag, NoError, 3390, MyFile)) RETURN
!
!  Broadcast error flags to all processors in the group.
!
      ibuffer(1)=exit_flag
      ibuffer(2)=ioerror
      CALL mp_bcasti (ng, model, ibuffer)
      exit_flag=ibuffer(1)
      ioerror=ibuffer(2)
!
  10  FORMAT (/,' WRT_INFO_NF90 - error while writing variable: ',a,/,  &
     &        17x,'into file: ',a)
  20  FORMAT (/,' WRT_INFO_NF90 - error while inquiring ID for',        &
     &        ' variable: ',a,/,17x,'in file: ',a)
  30  FORMAT (/,' WRT_INFO_NF90 - unable to synchronize to disk file:', &
     &        /,17x,a)
!
      RETURN
      END SUBROUTINE wrt_info_nf90
      END MODULE wrt_info_mod
