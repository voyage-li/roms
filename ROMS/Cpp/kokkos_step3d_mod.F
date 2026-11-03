      MODULE kokkos_step3d_mod
!
!  Fortran interfaces to the Kokkos 3-D time-stepping kernels in
!  ROMS/Cpp/kokkos_step3d.cpp.  Arrays cross as C_LOC pointers, so no
!  data is copied; the caller's dummies carry TARGET for that.
!
      USE, INTRINSIC :: iso_c_binding, ONLY : C_PTR, C_INT, C_DOUBLE
      implicit none
      PRIVATE
      PUBLIC :: kokkos_pre3d_config, kokkos_uv3d_config
      PUBLIC :: k_pre_step3d_t, k_pre_step3d_uv
      PUBLIC :: k_step3d_uv_solve, k_step3d_uv_couple
!
!  Mirror of roms::Pre3dConfig in ROMS/Cpp/kokkos_step3d.hpp; the field
!  order must match exactly.
!
      TYPE, BIND(C) :: kokkos_pre3d_config
        integer(C_INT) :: LBi, UBi, LBj, UBj
        integer(C_INT) :: Istr, Iend, Jstr, Jend, IstrU, JstrV
        integer(C_INT) :: N, NT, NAT, NDT
        integer(C_INT) :: nrhs, nstp, nnew
        integer(C_INT) :: step
        integer(C_INT) :: iTrate, iTvdif
        integer(C_INT) :: NDM3d, NDrhs, M3pgrd, M3vvis, M3rate
        integer(C_INT) :: fold_w, fold_e, fold_s, fold_n
        real(C_DOUBLE) :: dt, lambda
      END TYPE kokkos_pre3d_config
!
!  Mirror of roms::Uv3dConfig.
!
      TYPE, BIND(C) :: kokkos_uv3d_config
        integer(C_INT) :: LBi, UBi, LBj, UBj
        integer(C_INT) :: Istr, Iend, Jstr, Jend, IstrU, JstrV
        integer(C_INT) :: IstrP, IstrT, IendT, JstrT, JendT
        integer(C_INT) :: Mm, N, nrhs, nnew
        integer(C_INT) :: step
        integer(C_INT) :: NDM2d, NDM3d, NDrhs
        integer(C_INT) :: M2pgrd, M2bstr, M2sstr, M2rate
        integer(C_INT) :: M3pgrd, M3vvis, M3rate
        integer(C_INT) :: M3xvis, M3yvis, M3hvis
        integer(C_INT) :: npairs, m2(12), m3(12)
        integer(C_INT) :: rep_w, rep_e, rep_s, rep_n
        real(C_DOUBLE) :: dt, lambda
      END TYPE kokkos_uv3d_config
!
      INTERFACE
!
        SUBROUTINE k_pre_step3d_t (cfg, hadv, vadv,                     &
     &                             pm_p, pn_p, Hz_p, Huon_p, Hvom_p,    &
     &                             z_r_p, W_p, Akt_p, btflx_p,          &
     &                             stflx_p, t_p, DiaTwrk_p)             &
     &           BIND(C, NAME='k_pre_step3d_t')
          IMPORT :: C_PTR, C_INT, kokkos_pre3d_config
          TYPE(kokkos_pre3d_config), INTENT(IN) :: cfg
          integer(C_INT), INTENT(IN) :: hadv(*), vadv(*)
          TYPE(C_PTR), VALUE :: pm_p, pn_p, Hz_p, Huon_p, Hvom_p
          TYPE(C_PTR), VALUE :: z_r_p, W_p, Akt_p, btflx_p, stflx_p
          TYPE(C_PTR), VALUE :: t_p, DiaTwrk_p
        END SUBROUTINE k_pre_step3d_t
!
        SUBROUTINE k_pre_step3d_uv (cfg, pm_p, pn_p, Hz_p, z_r_p,       &
     &                              Akv_p, bustr_p, bvstr_p,            &
     &                              sustr_p, svstr_p, ru_p, rv_p,       &
     &                              u_p, v_p, DiaU3wrk_p, DiaV3wrk_p,   &
     &                              DiaRU_p, DiaRV_p)                   &
     &           BIND(C, NAME='k_pre_step3d_uv')
          IMPORT :: C_PTR, kokkos_pre3d_config
          TYPE(kokkos_pre3d_config), INTENT(IN) :: cfg
          TYPE(C_PTR), VALUE :: pm_p, pn_p, Hz_p, z_r_p, Akv_p
          TYPE(C_PTR), VALUE :: bustr_p, bvstr_p, sustr_p, svstr_p
          TYPE(C_PTR), VALUE :: ru_p, rv_p, u_p, v_p
          TYPE(C_PTR), VALUE :: DiaU3wrk_p, DiaV3wrk_p, DiaRU_p, DiaRV_p
        END SUBROUTINE k_pre_step3d_uv
!
        SUBROUTINE k_step3d_uv_solve (cfg, pm_p, pn_p, on_u_p, om_v_p,  &
     &                                Hz_p, z_r_p, Akv_p,               &
     &                                DU_avg1_p, DV_avg1_p,             &
     &                                ru_p, rv_p, u_p, v_p,             &
     &                                DiaU2wrk_p, DiaV2wrk_p,           &
     &                                DiaU3wrk_p, DiaV3wrk_p,           &
     &                                DiaRU_p, DiaRV_p)                 &
     &           BIND(C, NAME='k_step3d_uv_solve')
          IMPORT :: C_PTR, kokkos_uv3d_config
          TYPE(kokkos_uv3d_config), INTENT(IN) :: cfg
          TYPE(C_PTR), VALUE :: pm_p, pn_p, on_u_p, om_v_p
          TYPE(C_PTR), VALUE :: Hz_p, z_r_p, Akv_p
          TYPE(C_PTR), VALUE :: DU_avg1_p, DV_avg1_p
          TYPE(C_PTR), VALUE :: ru_p, rv_p, u_p, v_p
          TYPE(C_PTR), VALUE :: DiaU2wrk_p, DiaV2wrk_p
          TYPE(C_PTR), VALUE :: DiaU3wrk_p, DiaV3wrk_p
          TYPE(C_PTR), VALUE :: DiaRU_p, DiaRV_p
        END SUBROUTINE k_step3d_uv_solve
!
        SUBROUTINE k_step3d_uv_couple (cfg, on_u_p, om_v_p, Hz_p,       &
     &                                 DU_avg1_p, DU_avg2_p,            &
     &                                 DV_avg1_p, DV_avg2_p,            &
     &                                 u_p, v_p, ubar_p, vbar_p,        &
     &                                 Huon_p, Hvom_p,                  &
     &                                 DiaU2wrk_p, DiaV2wrk_p,          &
     &                                 DiaU2int_p, DiaV2int_p,          &
     &                                 DiaU3wrk_p, DiaV3wrk_p)          &
     &           BIND(C, NAME='k_step3d_uv_couple')
          IMPORT :: C_PTR, kokkos_uv3d_config
          TYPE(kokkos_uv3d_config), INTENT(IN) :: cfg
          TYPE(C_PTR), VALUE :: on_u_p, om_v_p, Hz_p
          TYPE(C_PTR), VALUE :: DU_avg1_p, DU_avg2_p
          TYPE(C_PTR), VALUE :: DV_avg1_p, DV_avg2_p
          TYPE(C_PTR), VALUE :: u_p, v_p, ubar_p, vbar_p
          TYPE(C_PTR), VALUE :: Huon_p, Hvom_p
          TYPE(C_PTR), VALUE :: DiaU2wrk_p, DiaV2wrk_p
          TYPE(C_PTR), VALUE :: DiaU2int_p, DiaV2int_p
          TYPE(C_PTR), VALUE :: DiaU3wrk_p, DiaV3wrk_p
        END SUBROUTINE k_step3d_uv_couple
!
      END INTERFACE
!
      END MODULE kokkos_step3d_mod
