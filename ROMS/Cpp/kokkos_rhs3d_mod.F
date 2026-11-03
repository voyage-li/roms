      MODULE kokkos_rhs3d_mod
!
!  Fortran interfaces to the Kokkos 3-D right-hand-side kernels in
!  ROMS/Cpp/kokkos_rhs3d.cpp.
!
!  The tile index metadata is the same BIND(C) object step2d uses, so
!  only one layout exists on the Fortran side.  kk_fill_bounds() fills it
!  from BOUNDS(ng) right after "set_bounds.h" has run.
!
      USE, INTRINSIC :: iso_c_binding, ONLY : C_PTR, C_INT, C_DOUBLE
      USE kokkos_step2d_mod, ONLY : kokkos_tile_bounds
      implicit none
      PRIVATE
      PUBLIC :: kokkos_tile_bounds, kk_fill_bounds
      PUBLIC :: k_uv3dmix2_s, k_t3dmix2_s, k_prsgrd32
      PUBLIC :: k_omega3d, k_set_zeta
!
      INTERFACE
!
        SUBROUTINE k_uv3dmix2_s (kng, ktile, krhs, knew,                  &
     &                           LBi, UBi, LBj, UBj,                      &
     &                           IminS, ImaxS, JminS, JmaxS,              &
     &                           Hz_p, om_p_p, om_r_p, on_p_p, on_r_p,    &
     &                           pm_p, pmon_p_p, pmon_r_p, pn_p,          &
     &                           pnom_p_p, pnom_r_p,                      &
     &                           visc2_p_p, visc2_r_p,                    &
     &                           DiaRUfrc_p, DiaRVfrc_p,                  &
     &                           DiaU3wrk_p, DiaV3wrk_p,                  &
     &                           rufrc_p, rvfrc_p, u_p, v_p,              &
     &                           NLev, NDM2d, NDM3d,                      &
     &                           M2hvis, M2xvis, M2yvis,                  &
     &                           M3hvis, M3xvis, M3yvis,                  &
     &                           dt, tl)                                  &
     &         BIND(C, NAME='k_uv3dmix2_s')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, krhs, knew
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: Hz_p
          type(C_PTR),    VALUE :: om_p_p, om_r_p, on_p_p, on_r_p
          type(C_PTR),    VALUE :: pm_p, pmon_p_p, pmon_r_p
          type(C_PTR),    VALUE :: pn_p, pnom_p_p, pnom_r_p
          type(C_PTR),    VALUE :: visc2_p_p, visc2_r_p
          type(C_PTR),    VALUE :: DiaRUfrc_p, DiaRVfrc_p
          type(C_PTR),    VALUE :: DiaU3wrk_p, DiaV3wrk_p
          type(C_PTR),    VALUE :: rufrc_p, rvfrc_p, u_p, v_p
          integer(C_INT), VALUE :: NLev, NDM2d, NDM3d
          integer(C_INT), VALUE :: M2hvis, M2xvis, M2yvis
          integer(C_INT), VALUE :: M3hvis, M3xvis, M3yvis
          real(C_DOUBLE), VALUE :: dt
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_uv3dmix2_s
!
        SUBROUTINE k_t3dmix2_s (kng, ktile, krhs, kstp, knew,             &
     &                          LBi, UBi, LBj, UBj,                       &
     &                          IminS, ImaxS, JminS, JmaxS,               &
     &                          diff2_p, Hz_p, pmon_u_p, pnom_v_p,       &
     &                          pm_p, pn_p, DiaTwrk_p, t_p,              &
     &                          NLev, NT, NDT,                            &
     &                          iTxdif, iTydif, iThdif, dt, tl)           &
     &         BIND(C, NAME='k_t3dmix2_s')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, krhs, kstp, knew
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: diff2_p, Hz_p, pmon_u_p, pnom_v_p
          type(C_PTR),    VALUE :: pm_p, pn_p, DiaTwrk_p, t_p
          integer(C_INT), VALUE :: NLev, NT, NDT
          integer(C_INT), VALUE :: iTxdif, iTydif, iThdif
          real(C_DOUBLE), VALUE :: dt
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_t3dmix2_s
!
        SUBROUTINE k_prsgrd32 (kng, ktile, krhs,                          &
     &                         LBi, UBi, LBj, UBj,                        &
     &                         IminS, ImaxS, JminS, JmaxS,                &
     &                         om_v_p, on_u_p, Hz_p, z_r_p, z_w_p,        &
     &                         rho_p, P_p, ru_p, rv_p,                    &
     &                         DiaRU_p, DiaRV_p,                          &
     &                         NLev, NDrhs, M3pgrd, g, rho0, tl)         &
     &         BIND(C, NAME='k_prsgrd32')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, krhs
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: om_v_p, on_u_p, Hz_p
          type(C_PTR),    VALUE :: z_r_p, z_w_p, rho_p, P_p
          type(C_PTR),    VALUE :: ru_p, rv_p, DiaRU_p, DiaRV_p
          integer(C_INT), VALUE :: NLev, NDrhs, M3pgrd
          real(C_DOUBLE), VALUE :: g, rho0
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_prsgrd32
!
        SUBROUTINE k_omega3d (kng, ktile,                                 &
     &                        LBi, UBi, LBj, UBj,                         &
     &                        IminS, ImaxS, JminS, JmaxS,                 &
     &                        Huon_p, Hvom_p, z_w_p, W_p, NLev, tl)      &
     &         BIND(C, NAME='k_omega3d')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: Huon_p, Hvom_p, z_w_p, W_p
          integer(C_INT), VALUE :: NLev
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_omega3d
!
        SUBROUTINE k_set_zeta (kng, ktile,                                &
     &                         LBi, UBi, LBj, UBj,                        &
     &                         IminS, ImaxS, JminS, JmaxS,                &
     &                         Zt_avg1_p, zeta_p, tl)                     &
     &         BIND(C, NAME='k_set_zeta')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: Zt_avg1_p, zeta_p
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_set_zeta
!
      END INTERFACE
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE kk_fill_bounds (tl, ng, tile)
!***********************************************************************
!
!  Copy the tile index metadata the kernels need out of BOUNDS(ng).
!  Must be called after "set_bounds.h" has set the local indices.
!
      USE mod_param, ONLY : BOUNDS, N
!
      TYPE(kokkos_tile_bounds), INTENT(OUT) :: tl
      integer, INTENT(IN) :: ng, tile
!
      tl%Istr   =BOUNDS(ng)%Istr   (tile)
      tl%Iend   =BOUNDS(ng)%Iend   (tile)
      tl%Jstr   =BOUNDS(ng)%Jstr   (tile)
      tl%Jend   =BOUNDS(ng)%Jend   (tile)
      tl%IstrU  =BOUNDS(ng)%IstrU  (tile)
      tl%JstrV  =BOUNDS(ng)%JstrV  (tile)
      tl%IstrR  =BOUNDS(ng)%IstrR  (tile)
      tl%JstrR  =BOUNDS(ng)%JstrR  (tile)
      tl%IendR  =BOUNDS(ng)%IendR  (tile)
      tl%JendR  =BOUNDS(ng)%JendR  (tile)
      tl%Istrm1 =BOUNDS(ng)%Istrm1 (tile)
      tl%Jstrm1 =BOUNDS(ng)%Jstrm1 (tile)
      tl%Iendp1 =BOUNDS(ng)%Iendp1 (tile)
      tl%Jendp1 =BOUNDS(ng)%Jendp1 (tile)
      tl%Iendp2 =BOUNDS(ng)%Iendp2 (tile)
      tl%Jendp2 =BOUNDS(ng)%Jendp2 (tile)
      tl%IstrUm1=BOUNDS(ng)%IstrUm1(tile)
      tl%JstrVm1=BOUNDS(ng)%JstrVm1(tile)
      tl%IstrUm2=BOUNDS(ng)%IstrUm2(tile)
      tl%JstrVm2=BOUNDS(ng)%JstrVm2(tile)
      tl%UBk    =N(ng)
!
      RETURN
      END SUBROUTINE kk_fill_bounds
!
      END MODULE kokkos_rhs3d_mod
