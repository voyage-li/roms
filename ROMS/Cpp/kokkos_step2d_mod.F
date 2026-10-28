      MODULE kokkos_step2d_mod
!
!git $Id$
!=======================================================================
!  Copyright (c) 2002-2025 The ROMS Group                               !
!    Licensed under a MIT/X style license                               !
!    See License_ROMS.md                                                !
!=======================================================================
!                                                                      !
!  Fortran interfaces to the Kokkos compute kernels in ROMS/Cpp.       !
!                                                                      !
!  Only the pure computation stages of step2d are exposed here; the    !
!  halo exchanges, open-boundary routines and side boundary conditions !
!  stay in Fortran and are interleaved between these calls.            !
!                                                                      !
!  Arrays cross the language boundary as TYPE(C_PTR) taken from        !
!  C_LOC(), so no data is copied.  C_LOC requires the actual argument  !
!  to have the POINTER or TARGET attribute, which is why the           !
!  step2d_tile dummies and its scratch arrays carry TARGET.            !
!                                                                      !
!  Index conventions:                                                  !
!                                                                      !
!    LBi, UBi, LBj, UBj   grid bounds used to declare the arrays       !
!    IminS, ImaxS, ...    tile-private scratch bounds                  !
!    tl                   TileBounds,   see kokkos_config.hpp           !
!    de                   EdgeFlags,    see kokkos_config.hpp           !
!=======================================================================
!
      USE mod_kinds,    ONLY : r8, i4b
      USE, INTRINSIC :: iso_c_binding, ONLY : C_PTR, C_INT, C_DOUBLE
!
      implicit none
!
      PRIVATE
      PUBLIC :: kokkos_tile_bounds
      PUBLIC :: kokkos_edge_flags
      PUBLIC :: k_step2d_depth_flux
      PUBLIC :: k_step2d_time_average
      PUBLIC :: k_step2d_zeta_step
      PUBLIC :: k_step2d_pressure_grad
      PUBLIC :: k_step2d_hadv_centered4
      PUBLIC :: k_step2d_coriolis
      PUBLIC :: k_step2d_psi_depth
      PUBLIC :: k_step2d_hvisc2
      PUBLIC :: k_step2d_coupling
      PUBLIC :: k_step2d_momentum_step
      PUBLIC :: k_step2d_store_rhs
      PUBLIC :: kokkos_init
      PUBLIC :: kokkos_finalize
!
!  Mirror of roms::TileBounds in ROMS/Cpp/kokkos_config.hpp.  The field
!  order must match exactly.
!
      TYPE, BIND(C) :: kokkos_tile_bounds
        integer(C_INT) :: Istr, Iend, Jstr, Jend
        integer(C_INT) :: IstrU, JstrV
        integer(C_INT) :: IstrR, JstrR, IendR, JendR
        integer(C_INT) :: Istrm1, Jstrm1
        integer(C_INT) :: Iendp1, Jendp1
        integer(C_INT) :: Iendp2, Jendp2
        integer(C_INT) :: IstrUm1, JstrVm1
        integer(C_INT) :: IstrUm2, JstrVm2
        integer(C_INT) :: UBk
      END TYPE kokkos_tile_bounds
!
!  Mirror of roms::EdgeFlags.
!
      TYPE, BIND(C) :: kokkos_edge_flags
        integer(C_INT) :: west, east, south, north
        integer(C_INT) :: composite_w, composite_e
        integer(C_INT) :: composite_s, composite_n
        integer(C_INT) :: periodic_ew, periodic_ns
      END TYPE kokkos_edge_flags
!
      INTERFACE
!
        SUBROUTINE k_step2d_depth_flux (kng, ktile, kkrhs, nfast,           &
     &                                  LBi, UBi, LBj, UBj,              &
     &                                  IminS, ImaxS, JminS, JmaxS,      &
     &                                  zeta_p, h_p, on_u_p, om_v_p,     &
     &                                  ubar_p, vbar_p, Drhs_p,          &
     &                                  DUon_p, DVom_p, UBk, tl)         &
     &                                  BIND(C, NAME='k_step2d_depth_flux')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, kkrhs, nfast
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: zeta_p, h_p, on_u_p, om_v_p
          type(C_PTR),    VALUE :: ubar_p, vbar_p, Drhs_p
          type(C_PTR),    VALUE :: DUon_p, DVom_p
          integer(C_INT), VALUE :: UBk
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_depth_flux
!
        SUBROUTINE k_step2d_time_average (kng, ktile, kkrhs,              &
     &                                    predictor, first_step,         &
     &                                    LBi, UBi, LBj, UBj,            &
     &                                    IminS, ImaxS, JminS, JmaxS,    &
     &                                    zeta_p, DUon_p, DVom_p,        &
     &                                    Zt_avg1_p, DU_avg1_p,          &
     &                                    DU_avg2_p, DV_avg1_p,          &
     &                                    DV_avg2_p, cff1, cff2, tl)     &
     &          BIND(C, NAME='k_step2d_time_average')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, kkrhs
          integer(C_INT), VALUE :: predictor, first_step
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: zeta_p
          type(C_PTR),    VALUE :: DUon_p, DVom_p
          type(C_PTR),    VALUE :: Zt_avg1_p, DU_avg1_p, DU_avg2_p
          type(C_PTR),    VALUE :: DV_avg1_p, DV_avg2_p
          real(C_DOUBLE), VALUE :: cff1, cff2
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_time_average
!
        SUBROUTINE k_step2d_zeta_step (kng, ktile, kkrhs, kstp, knew,       &
     &                                 iif, nfast, ntfirst, iic,         &
     &                                 predictor, first_2d_step,         &
     &                                 LBi, UBi, LBj, UBj, UBk,          &
     &                                 IminS, ImaxS, JminS, JmaxS,       &
     &                                 zeta_p, h_p, pm_p, pn_p,          &
     &                                 DUon_p, DVom_p,                   &
     &                                 gzeta_p, gzeta2_p,                &
     &                                 gzetaSA_p, rhoA_p, rhoS_p,        &
     &                                 rhs_zeta_p, zeta_new_p,           &
     &                                 Dnew_p, zwrk_p, rzeta_p,          &
     &                                 dtfast, rho0, tl)                 &
     &          BIND(C, NAME='k_step2d_zeta_step')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, kkrhs, kstp, knew
          integer(C_INT), VALUE :: iif, nfast, ntfirst, iic
          integer(C_INT), VALUE :: predictor, first_2d_step
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj, UBk
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: zeta_p, h_p, pm_p, pn_p
          type(C_PTR),    VALUE :: DUon_p, DVom_p
          type(C_PTR),    VALUE :: gzeta_p, gzeta2_p
          type(C_PTR),    VALUE :: gzetaSA_p, rhoA_p, rhoS_p
          type(C_PTR),    VALUE :: rhs_zeta_p, zeta_new_p
          type(C_PTR),    VALUE :: Dnew_p, zwrk_p, rzeta_p
          real(C_DOUBLE), VALUE :: dtfast, rho0
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_zeta_step
!
        SUBROUTINE k_step2d_pressure_grad (kng, ktile,                     &
     &                                     LBi, UBi, LBj, UBj,           &
     &                                     IminS, ImaxS, JminS, JmaxS,   &
     &                                     h_p, on_u_p, om_v_p,          &
     &                                     gzeta_p, gzeta2_p,            &
     &                                     gzetaSA_p, rhoA_p, zwrk_p,    &
     &                                     rhs_ubar_p, rhs_vbar_p,       &
     &                                     DiaU2rhs_p, DiaV2rhs_p,       &
     &                                     NDM2d, M2pgrd, g, tl)         &
     &          BIND(C, NAME='k_step2d_pressure_grad')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: h_p, on_u_p, om_v_p
          type(C_PTR),    VALUE :: gzeta_p, gzeta2_p
          type(C_PTR),    VALUE :: gzetaSA_p, rhoA_p, zwrk_p
          type(C_PTR),    VALUE :: rhs_ubar_p, rhs_vbar_p
          type(C_PTR),    VALUE :: DiaU2rhs_p, DiaV2rhs_p
          integer(C_INT), VALUE :: NDM2d, M2pgrd
          real(C_DOUBLE), VALUE :: g
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_pressure_grad
!
        SUBROUTINE k_step2d_hadv_centered4 (kng, ktile, krhs,              &
     &                                      LBi, UBi, LBj, UBj,          &
     &                                      IminS, ImaxS, JminS, JmaxS,  &
     &                                      ubar_p, vbar_p,              &
     &                                      DUon_p, DVom_p,              &
     &                                      grad_p, Dgrad_p,             &
     &                                      UFx_p, UFe_p,                &
     &                                      VFx_p, VFe_p,                &
     &                                      rhs_ubar_p, rhs_vbar_p,      &
     &                                      DiaU2rhs_p, DiaV2rhs_p,      &
     &                                      NDM2d, M2xadv, M2yadv,       &
     &                                      M2hadv, de, tl)              &
     &          BIND(C, NAME='k_step2d_hadv_centered4')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds, kokkos_edge_flags
          integer(C_INT), VALUE :: kng, ktile, krhs
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: ubar_p, vbar_p
          type(C_PTR),    VALUE :: DUon_p, DVom_p
          type(C_PTR),    VALUE :: grad_p, Dgrad_p
          type(C_PTR),    VALUE :: UFx_p, UFe_p
          type(C_PTR),    VALUE :: VFx_p, VFe_p
          type(C_PTR),    VALUE :: rhs_ubar_p, rhs_vbar_p
          type(C_PTR),    VALUE :: DiaU2rhs_p, DiaV2rhs_p
          integer(C_INT), VALUE :: NDM2d, M2xadv, M2yadv, M2hadv
          type(kokkos_edge_flags),  INTENT(IN) :: de
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_hadv_centered4
!
        SUBROUTINE k_step2d_coriolis (kng, ktile, krhs,                    &
     &                                LBi, UBi, LBj, UBj,                &
     &                                IminS, ImaxS, JminS, JmaxS,        &
     &                                Drhs_p, fomn_p,                    &
     &                                ubar_p, vbar_p,                    &
     &                                UFx_p, VFe_p,                      &
     &                                rhs_ubar_p, rhs_vbar_p,            &
     &                                DiaU2rhs_p, DiaV2rhs_p,            &
     &                                NDM2d, M2fcor, tl)                 &
     &          BIND(C, NAME='k_step2d_coriolis')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, krhs
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: Drhs_p, fomn_p
          type(C_PTR),    VALUE :: ubar_p, vbar_p
          type(C_PTR),    VALUE :: UFx_p, VFe_p
          type(C_PTR),    VALUE :: rhs_ubar_p, rhs_vbar_p
          type(C_PTR),    VALUE :: DiaU2rhs_p, DiaV2rhs_p
          integer(C_INT), VALUE :: NDM2d, M2fcor
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_coriolis
!
        SUBROUTINE k_step2d_psi_depth (kng, ktile,                         &
     &                                 LBi, UBi, LBj, UBj,               &
     &                                 IminS, ImaxS, JminS, JmaxS,       &
     &                                 Drhs_p, Drhs_psi_p, tl)           &
     &          BIND(C, NAME='k_step2d_psi_depth')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: Drhs_p, Drhs_psi_p
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_psi_depth
!
        SUBROUTINE k_step2d_hvisc2 (kng, ktile, krhs,                      &
     &                              LBi, UBi, LBj, UBj,                  &
     &                              IminS, ImaxS, JminS, JmaxS,          &
     &                              Drhs_p, Drhs_psi_p,                  &
     &                              ubar_p, vbar_p,                      &
     &                              pm_p, pn_p,                          &
     &                              pmon_r_p, pnom_r_p,                  &
     &                              pmon_p_p, pnom_p_p,                  &
     &                              om_r_p, on_r_p,                      &
     &                              om_p_p, on_p_p,                      &
     &                              visc2_r_p, visc2_p2_p,               &
     &                              UFx_p, UFe_p,                        &
     &                              VFx_p, VFe_p,                        &
     &                              rhs_ubar_p, rhs_vbar_p,              &
     &                              DiaU2rhs_p, DiaV2rhs_p,              &
     &                              NDM2d, M2hvis, M2xvis, M2yvis, tl)   &
     &          BIND(C, NAME='k_step2d_hvisc2')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, krhs
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: Drhs_p, Drhs_psi_p
          type(C_PTR),    VALUE :: ubar_p, vbar_p
          type(C_PTR),    VALUE :: pm_p, pn_p
          type(C_PTR),    VALUE :: pmon_r_p, pnom_r_p
          type(C_PTR),    VALUE :: pmon_p_p, pnom_p_p
          type(C_PTR),    VALUE :: om_r_p, on_r_p
          type(C_PTR),    VALUE :: om_p_p, on_p_p
          type(C_PTR),    VALUE :: visc2_r_p, visc2_p2_p
          type(C_PTR),    VALUE :: UFx_p, UFe_p
          type(C_PTR),    VALUE :: VFx_p, VFe_p
          type(C_PTR),    VALUE :: rhs_ubar_p, rhs_vbar_p
          type(C_PTR),    VALUE :: DiaU2rhs_p, DiaV2rhs_p
          integer(C_INT), VALUE :: NDM2d, M2hvis, M2xvis, M2yvis
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_hvisc2
!
        SUBROUTINE k_step2d_coupling (kng, ktile, istp, inew,            &
     &                                iic, ntfirst, first_2d_step,       &
     &                                predictor,                         &
     &                                LBi, UBi, LBj, UBj,                &
     &                                IminS, ImaxS, JminS, JmaxS,        &
     &                                rufrc_p, rvfrc_p,                  &
     &                                ru_p, rv_p,                        &
     &                                rhs_ubar_p, rhs_vbar_p,            &
     &                                DiaU2rhs_p, DiaV2rhs_p,            &
     &                                DiaRUfrc_p, DiaRVfrc_p,            &
     &                                NDM2d, M2pgrd, M2sstr, M2bstr, tl) &
     &          BIND(C, NAME='k_step2d_coupling')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, istp, inew
          integer(C_INT), VALUE :: iic, ntfirst, first_2d_step, predictor
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: rufrc_p, rvfrc_p
          type(C_PTR),    VALUE :: ru_p, rv_p
          type(C_PTR),    VALUE :: rhs_ubar_p, rhs_vbar_p
          type(C_PTR),    VALUE :: DiaU2rhs_p, DiaV2rhs_p
          type(C_PTR),    VALUE :: DiaRUfrc_p, DiaRVfrc_p
          integer(C_INT), VALUE :: NDM2d, M2pgrd, M2sstr, M2bstr
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_coupling
!
        SUBROUTINE k_step2d_momentum_step (kng, ktile, kstp, knew,         &
     &                                     LBi, UBi, LBj, UBj,           &
     &                                     IminS, ImaxS, JminS, JmaxS,   &
     &                                     zeta_p, h_p,                  &
     &                                     ubar_p, vbar_p,               &
     &                                     rubar_p, rvbar_p,             &
     &                                     pm_p, pn_p,                   &
     &                                     rhs_ubar_p, rhs_vbar_p,       &
     &                                     Dstp_p, Dnew_p,               &
     &                                     predictor, first_2d_step,     &
     &                                     dtfast, tl)                   &
     &          BIND(C, NAME='k_step2d_momentum_step')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, kstp, knew
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: zeta_p, h_p
          type(C_PTR),    VALUE :: ubar_p, vbar_p
          type(C_PTR),    VALUE :: rubar_p, rvbar_p
          type(C_PTR),    VALUE :: pm_p, pn_p
          type(C_PTR),    VALUE :: rhs_ubar_p, rhs_vbar_p
          type(C_PTR),    VALUE :: Dstp_p, Dnew_p
          integer(C_INT), VALUE :: predictor, first_2d_step
          real(C_DOUBLE), VALUE :: dtfast
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_momentum_step
!
        SUBROUTINE k_step2d_store_rhs (kng, ktile, kkrhs,                   &
     &                                 LBi, UBi, LBj, UBj,               &
     &                                 IminS, ImaxS, JminS, JmaxS,       &
     &                                 rubar_p, rvbar_p,                 &
     &                                 rhs_ubar_p, rhs_vbar_p,           &
     &                                 DiaRUbar_p, DiaRVbar_p,           &
     &                                 DiaU2rhs_p, DiaV2rhs_p,           &
     &                                 NDM2d, M2pgrd, tl)                &
     &          BIND(C, NAME='k_step2d_store_rhs')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, kkrhs
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: rubar_p, rvbar_p
          type(C_PTR),    VALUE :: rhs_ubar_p, rhs_vbar_p
          type(C_PTR),    VALUE :: DiaRUbar_p, DiaRVbar_p
          type(C_PTR),    VALUE :: DiaU2rhs_p, DiaV2rhs_p
          integer(C_INT), VALUE :: NDM2d, M2pgrd
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_store_rhs
!
      END INTERFACE
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE kokkos_init
!***********************************************************************
!
!  Initialize Kokkos once.  Called after MPI is up and before the first
!  kernel.  The C++ side guards this with std::call_once, so it is safe
!  even though every OpenMP thread reaches the driver.
!
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE roms_kokkos_initialize () BIND(C,                      &
     &      NAME='roms_kokkos_initialize')
        END SUBROUTINE roms_kokkos_initialize
      END INTERFACE
!
      CALL roms_kokkos_initialize
!
      RETURN
      END SUBROUTINE kokkos_init
!
!***********************************************************************
      SUBROUTINE kokkos_finalize
!***********************************************************************
!
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE roms_kokkos_finalize () BIND(C,                        &
     &      NAME='roms_kokkos_finalize')
        END SUBROUTINE roms_kokkos_finalize
      END INTERFACE
!
      CALL roms_kokkos_finalize
!
      RETURN
      END SUBROUTINE kokkos_finalize
!
      END MODULE kokkos_step2d_mod
