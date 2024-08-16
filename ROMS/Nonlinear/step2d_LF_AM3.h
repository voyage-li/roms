      MODULE step2d_mod
!
!git $Id$
!=======================================================================
!                                                                      !
!  Nonlinear shallow-water primitive equations predictor (Leap-frog)   !
!  and corrector (Adams-Moulton) time-stepping engine.                 !
!                                                                      !
!=======================================================================
!
      implicit none
      interface init
        subroutine pass_const(                                                  &
        &      Istr_f, IstrB_f, IstrP_f, IstrR_f, IstrT_f,                      &
        &      IstrM_f, IstrU_f,                                                &
        &      Iend_f, IendB_f, IendP_f, IendR_f, IendT_f,                      &
        &      Jstr_f, JstrB_f, JstrP_f, JstrR_f, JstrT_f,                      &
        &      JstrM_f, JstrV_f,                                                &
        &      Jend_f, JendB_f, JendP_f, JendR_f, JendT_f,                      &
        &      Istrm3_f, Istrm2_f, Istrm1_f, IstrUm2_f, IstrUm1_f,              &
        &      Iendp1_f, Iendp2_f, Iendp2i_f, Iendp3_f,                         &
        &      Jstrm3_f, Jstrm2_f, Jstrm1_f, JstrVm2_f, JstrVm1_f,              &
        &      Jendp1_f, Jendp2_f, Jendp2i_f, Jendp3_f,                         &
        &      ng_f, tile_f,                                                    &
        &      LBi_f, UBi_f, LBj_f, UBj_f, UBk_f,                               &
        &      IminS_f, ImaxS_f, JminS_f, JmaxS_f,                              &
        &      krhs_f, kstp_f, knew_f                                           &
#ifdef SOLVE3D
        &      ,nstp_f, nnew_f                                                  &
#endif
        &       ) bind(C,name="pass_const")
            use iso_c_binding
            integer :: Istr_f, IstrB_f, IstrP_f, IstrR_f, IstrT_f, IstrM_f, IstrU_f
            integer :: Iend_f, IendB_f, IendP_f, IendR_f, IendT_f
            integer :: Jstr_f, JstrB_f, JstrP_f, JstrR_f, JstrT_f, JstrM_f, JstrV_f
            integer :: Jend_f, JendB_f, JendP_f, JendR_f, JendT_f
            integer :: Istrm3_f, Istrm2_f, Istrm1_f, IstrUm2_f, IstrUm1_f
            integer :: Iendp1_f, Iendp2_f, Iendp2i_f, Iendp3_f
            integer :: Jstrm3_f, Jstrm2_f, Jstrm1_f, JstrVm2_f, JstrVm1_f
            integer :: Jendp1_f, Jendp2_f, Jendp2i_f, Jendp3_f
            integer :: ng_f, tile_f
            integer :: LBi_f, UBi_f, LBj_f, UBj_f, UBk_f
            integer :: IminS_f, ImaxS_f, JminS_f, JmaxS_f
            integer :: krhs_f, kstp_f, knew_f
#ifdef SOLVE3D
            integer :: nstp_f, nnew_f
#endif
        end subroutine pass_const
        subroutine step_loop1(                                                      &
        &       Drhs_f, zeta_f, zetax, zetay, zetaz,                                &
        &       h_f, hx, hy,                                                        &
        &       on_u_f, on_ux, on_uy,                                               &
        &       DUon_f, ubar_f, ubarx, ubary, ubarz,                                &
#ifdef WEC
#ifdef WET_DRY
        &       umask_wet_f, umask_wetx, umask_wety,                                &
#endif
        &       DUson_f, ubar_stokes_f,                                             &
        &       ubar_stokesx, ubar_stokesy, ubar_stokesz,                           &
#endif
        &       om_v_f, om_vx, om_vy,                                               &
        &       DVom_f, vbar_f, vbarx, vbary, vbarz                                 &
#ifdef WEC
#ifdef WET_DRY
        &       ,vmask_wet_f, vmask_wetx, vmask_wety                                &
#endif
        &       ,DVsom_f, vbar_stokes_f,                                            &
        &       vbar_stokesx, vbar_stokesy, vbar_stokesz                            &
#endif
        &       ) bind(C,name="step_loop1")
            use iso_c_binding
            real(8), dimension(*) :: Drhs_f, zeta_f
            integer :: zetax, zetay, zetaz
            real(8), dimension(*) :: h_f
            integer :: hx, hy
            real(8), dimension(*) :: on_u_f
            integer :: on_ux, on_uy
            real(8), dimension(*) :: DUon_f, ubar_f
            integer :: ubarx, ubary, ubarz
#ifdef WEC
#ifdef WET_DRY
            real(8), dimension(*) :: umask_wet_f
            integer :: umask_wetx, umask_wety
#endif
            real(8), dimension(*) :: DUson_f, ubar_stokes_f
            integer :: ubar_stokesx, ubar_stokesy, ubar_stokesz
#endif
            real(8), dimension(*) :: om_v_f
            integer :: om_vx, om_vy
            real(8), dimension(*) :: DVom_f, vbar_f
            integer :: vbarx, vbary, vbarz
#ifdef WEC
#ifdef WET_DRY
            real(8), dimension(*) :: vmask_wet_f
            integer :: vmask_wetx, vmask_wety   
#endif
            real(8), dimension(*) :: DVsom_f, vbar_stokes_f
            integer :: vbar_stokesx, vbar_stokesy, vbar_stokesz
#endif
        end subroutine step_loop1
        subroutine step_loop2(                                                      &
        &        cff2,                                                              &
        &        Zt_avg1_f, Zt_avg1x, Zt_avg1y,                                     &
        &        DU_avg1_f, DU_avg1x, DU_avg1y,                                     &
        &        DU_avg2_f, DU_avg2x, DU_avg2y,                                     &
        &        DUon_f,                                                            &
        &        DV_avg1_f, DV_avg1x, DV_avg1y,                                     &
        &        DV_avg2_f, DV_avg2x, DV_avg2y,                                     &
        &        DVom_f) bind(C, name="step_loop2")
            use iso_c_binding
            real(8) :: cff2
            real(8), dimension(*) :: Zt_avg1_f
            integer :: Zt_avg1x, Zt_avg1y
            real(8), dimension(*) :: DU_avg1_f
            integer :: DU_avg1x, DU_avg1y
            real(8), dimension(*) :: DU_avg2_f
            integer :: DU_avg2x, DU_avg2y
            real(8), dimension(*) :: DUon_f
            real(8), dimension(*) :: DV_avg1_f
            integer :: DV_avg1x, DV_avg1y
            real(8), dimension(*) :: DV_avg2_f
            integer :: DV_avg2x, DV_avg2y
            real(8), dimension(*) :: DVom_f
        end subroutine step_loop2
        subroutine step_loop3(                                                      &
        &        cff1, cff2,                                                        &
        &        Zt_avg1_f, Zt_avg1x, Zt_avg1y,                                     &
        &        zeta_f, zetax, zetay, zetaz,                                       &
        &        DU_avg1_f, DU_avg1x, DU_avg1y,                                     &
        &        DU_avg2_f, DU_avg2x, DU_avg2y,                                     &
        &        DUon_f,                                                            &
        &        DV_avg1_f, DV_avg1x, DV_avg1y,                                     &
        &        DV_avg2_f, DV_avg2x, DV_avg2y,                                     &
        &        DVom_f                                                             &
#ifdef WEC
        &        ,DUSon, DVSom                                                      &
#endif
        &       ) bind(C,name="step_loop3")
            use iso_c_binding
            real(8) :: cff1, cff2
            real(8), dimension(*) :: Zt_avg1_f
            integer :: Zt_avg1x, Zt_avg1y
            real(8), dimension(*) :: zeta_f
            integer :: zetax, zetay, zetaz
            real(8), dimension(*) :: DU_avg1_f
            integer :: DU_avg1x, DU_avg1y
            real(8), dimension(*) :: DU_avg2_f
            integer :: DU_avg2x, DU_avg2y
            real(8), dimension(*) :: DUon_f
            real(8), dimension(*) :: DV_avg1_f
            integer :: DV_avg1x, DV_avg1y
            real(8), dimension(*) :: DV_avg2_f
            integer :: DV_avg2x, DV_avg2y
            real(8), dimension(*) :: DVom_f
#ifdef WEC
            real(8), dimension(*) :: DUSon, DVSom
#endif
        end subroutine step_loop3
        subroutine step_loop4(                                                      &
        &       cff2,                                                               &
        &       DU_avg2_f, DU_avg2x, DU_avg2y,                                      &
        &       DUon_f,                                                             &
        &       DV_avg2_f, DV_avg2x, DV_avg2y,                                      &
        &       DVom_f) bind(C,name="step_loop4")
            use iso_c_binding
            real(8) :: cff2
            real(8), dimension(*) :: DU_avg2_f
            integer :: DU_avg2x, DU_avg2y
            real(8), dimension(*) :: DUon_f
            real(8), dimension(*) :: DV_avg2_f
            integer :: DV_avg2x, DV_avg2y
            real(8), dimension(*) :: DVom_f
        end subroutine step_loop4
        subroutine step_loop5(                                                      &
        &       cff1, rhs_zeta_f, DUon_f, DVom_f,                                   &
        &       zeta_new_f, zeta_f,                                                 &
        &       zetax, zetay, zetaz,                                                &
        &       pm_f, pmx, pmy,                                                     &
        &       pn_f, pnx, pny,                                                     &
#ifdef MASKING
        &       rmask_f, rmaskx, rmasky,                                            &
#endif
        &       Dnew_f, h_f, hx, hy,                                                &
        &       zwrk_f                                                              &
#if defined VAR_RHO_2D && defined SOLVE3D
        &       ,fac, gzeta_f, rhoS_f,                                              &
        &       rhoSx, rhoSy,                                                       &
        &       gzeta2_f, gzetaSA_f, rhoA_f,                                        &
        &       rhoAx, rhoAy                                                        &
#else
        &       ,gzeta_f, gzeta2_f                                                  &
#endif
        ) bind(C,name="step_loop5")
        use iso_c_binding
        real(8) :: cff1
        real(8), dimension(*) :: rhs_zeta_f, DUon_f, DVom_f
        real(8), dimension(*) :: zeta_new_f, zeta_f
        integer :: zetax, zetay, zetaz
        real(8), dimension(*) :: pm_f
        integer :: pmx, pmy
        real(8), dimension(*) :: pn_f
        integer :: pnx, pny
#ifdef MASKING
        real(8), dimension(*) :: rmask_f
        integer :: rmaskx, rmasky
#endif
        real(8), dimension(*) :: Dnew_f, h_f
        integer :: hx, hy
        real(8), dimension(*) :: zwrk_f
#if defined VAR_RHO_2D && defined SOLVE3D
        real(8) :: fac
        real(8), dimension(*) :: gzeta_f, rhoS_f
        integer :: rhoSx, rhoSy
        real(8), dimension(*) :: gzeta2_f, gzetaSA_f
        real(8), dimension(*) :: rhoA_f
        integer :: rhoAx, rhoAy
#else
        real(8), dimension(*) :: gzeta_f, gzeta2_f
#endif
        end subroutine step_loop5
        subroutine step_loop6(                                                      &
        &       cff1, cff4, cff5,                                                   &
        &       rhs_zeta_f, DUon_f, DVom_f,                                         &
        &       zeta_new_f, zeta_f,                                                 &
        &       zetax, zetay, zetaz,                                                &
        &       pm_f, pmx, pmy,                                                     &
        &       pn_f, pnx, pny,                                                     &
#ifdef MASKING
        &       rmask_f, rmaskx, rmasky,                                            &
#endif
        &       Dnew_f, h_f, hx, hy,                                                &
        &       zwrk_f                                                              &
#if defined VAR_RHO_2D && defined SOLVE3D
        &       ,fac, gzeta_f, rhoS_f,                                              &
        &       rhoSx, rhoSy,                                                       &
        &       gzeta2_f, gzetaSA_f, rhoA_f,                                        &
        &       rhoAx, rhoAy                                                        &
#else
        &       ,gzeta_f, gzeta2_f                                                  &
#endif
        ) bind(C,name="step_loop6")
        use iso_c_binding
        real(8) :: cff1, cff4, cff5
        real(8), dimension(*) :: rhs_zeta_f, DUon_f, DVom_f
        real(8), dimension(*) :: zeta_new_f, zeta_f
        integer :: zetax, zetay, zetaz
        real(8), dimension(*) :: pm_f
        integer :: pmx, pmy
        real(8), dimension(*) :: pn_f
        integer :: pnx, pny
#ifdef MASKING
        real(8), dimension(*) :: rmask_f
        integer :: rmaskx, rmasky
#endif
        real(8), dimension(*) :: Dnew_f, h_f
        integer :: hx, hy
        real(8), dimension(*) :: zwrk_f
#if defined VAR_RHO_2D && defined SOLVE3D
        real(8) :: fac
        real(8), dimension(*) :: gzeta_f, rhoS_f
        integer :: rhoSx, rhoSy
        real(8), dimension(*) :: gzeta2_f, gzetaSA_f
        real(8), dimension(*) :: rhoA_f
        integer :: rhoAx, rhoAy
#else
        real(8), dimension(*) :: gzeta_f, gzeta2_f
#endif
        end subroutine step_loop6
        subroutine step_loop7(                                                      &
        &       cff1, cff2, cff3, cff4, cff5,                               &
        &       DUon_f, DVom_f,                                             &
        &       zeta_new_f, zeta_f,                                         &
        &       zetax, zetay, zetaz,                                        &
        &       pm_f, pmx, pmy,                                             &
        &       pn_f, pnx, pny,                                             &
        &       rzeta_f, rzetax, rzetay, rzetaz,                            &
#ifdef MASKING
        &       rmask_f, rmaskx, rmasky,                                    &
#endif
        &       Dnew_f, h_f, hx, hy,                                        &
        &       zwrk_f                                                      &
#if defined VAR_RHO_2D && defined SOLVE3D
        &       ,fac, gzeta_f, rhoS_f,                                      &
        &       rhoSx, rhoSy,                                               &
        &       gzeta2_f, gzetaSA_f, rhoA_f,                                &
        &       rhoAx, rhoAy                                                &
#else
        &       ,gzeta_f, gzeta2_f                                          &       
#endif
        ) bind(C,name="step_loop7")
        use iso_c_binding
        real(8) :: cff1, cff2, cff3, cff4, cff5
        real(8), dimension(*) :: DUon_f, DVom_f
        real(8), dimension(*) :: zeta_new_f, zeta_f
        integer :: zetax, zetay, zetaz
        real(8), dimension(*) :: pm_f
        integer :: pmx, pmy
        real(8), dimension(*) :: pn_f
        integer :: pnx, pny
        real(8), dimension(*) :: rzeta_f
        integer :: rzetax, rzetay, rzetaz
#ifdef MASKING
        real(8), dimension(*) :: rmask_f
        integer :: rmaskx, rmasky
#endif
        real(8), dimension(*) :: Dnew_f, h_f
        integer :: hx, hy
        real(8), dimension(*) :: zwrk_f
#if defined VAR_RHO_2D && defined SOLVE3D
        real(8) :: fac
        real(8), dimension(*) :: gzeta_f, rhoS_f
        integer :: rhoSx, rhoSy
        real(8), dimension(*) :: gzeta2_f, gzetaSA_f
        real(8), dimension(*) :: rhoA_f
        integer :: rhoAx, rhoAy
#else
        real(8), dimension(*) :: gzeta_f, gzeta2_f
#endif
        end subroutine step_loop7
        subroutine step_loop8(                                                      &
        &      zeta_f, zetax, zetay, zetaz,                                         &
        &      zeta_new_f                                                           &
#if defined WET_DRY && defined MASKING
        &      ,Drict_ng, h_f, hx, hy,                                              &
        &      rmask_f, rmaskx, rmasky                                              &
#endif
        ) bind(C,name="step_loop8")
        use iso_c_binding
        real(8), dimension(*) :: zeta_f
        integer :: zetax, zetay, zetaz
        real(8), dimension(*) :: zeta_new_f
#if defined WET_DRY && defined MASKING
        real(8) :: Drict_ng
        real(8), dimension(*) :: h_f
        integer :: hx, hy
        real(8), dimension(*) :: rmask_f
        integer :: rmaskx, rmasky
#endif
        end subroutine step_loop8
        subroutine step_loop9(                                                      &
        &   fac, bed_thick_f,                                                       &
        &   bed_thickx, bed_thicky,  bed_thickz,                                    &
        &   h_f, hx, hy                                                             &
        ) bind(C,name="step_loop9")
        use iso_c_binding
        real(8) :: fac
        real(8), dimension(*) :: bed_thick_f
        integer :: bed_thickx, bed_thicky, bed_thickz
        real(8), dimension(*) :: h_f
        integer :: hx, hy
        end subroutine step_loop9                                               
      subroutine step_loop10(                                                       &
      &   cff1,cff2,g,                                                              &
#if !defined SOLVE3D && defined ATM_PRESS
      &   fac3,                                                                     &
#endif
      &   rhs_ubar_f, on_u_f, on_ux, on_uy,                                         &
      &   h_f, hx, hy,                                                              &
      &   gzeta_f,                                                                  &
#if defined VAR_RHO_2D && defined SOLVE3D
      &   gzetaSA_f, rhoA_f, rhoAx, rhoAy,                                          &
      &   zwrk_f,                                                                   &
#endif
      &   gzeta2_f,                                                                 &
#if defined ATM_PRESS && !defined SOLVE3D
      &   Pair_f, Pairx, Pairy,                                                     &
#endif
#if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
      &   eq_tide_f, eq_tidex, eq_tidey,                                            &
#endif
#ifdef DIAGNOSTICS_UV
      &   DiaU2rhs_f, DiaU2rhsz,                                                    &
      &   M2pgrd,                                                                   &
#if defined WEC_VF
      &   M2zeta, M2zetw,                                                           &
      &   M2zqsp, M2zebh, M2kvrf,                                                   &
#endif
#endif
#if defined WEC_VF
      &   zetaw_f, zetawx, zetawy,                                                  &
      &   qsp_f, qspx, qspy,                                                        &
      &   bh_f, bhx, bhy,                                                           &
      &   rukvf2d_f, rukvf2dx, rukvf2dy,                                            &
#endif
      &   rhs_var_f, om_v_f, om_vx, om_vy                                           &
#ifdef DIAGNOSTICS_UV
      &   ,DiaV2rhs_f, DiaV2rhsz                                                    &
#endif
#if defined WEC_VF
      &   ,rvkvf2d_f, rvkvf2dx, rvkvf2dy                                            &
#endif
        ) bind(C,name="step_loop10")
        use iso_c_binding
        real(8) :: cff1, cff2, g
#if !defined SOLVE3D && defined ATM_PRESS
        real(8) :: fac3
#endif
        real(8), dimension(*) :: rhs_ubar_f, on_u_f
        integer :: on_ux, on_uy
        real(8), dimension(*) :: h_f
        integer :: hx, hy
        real(8), dimension(*) :: gzeta_f
#if defined VAR_RHO_2D && defined SOLVE3D
        real(8), dimension(*) :: gzetaSA_f, rhoA_f
        integer :: rhoAx, rhoAy
        real(8), dimension(*) :: zwrk_f
#endif
        real(8), dimension(*) :: gzeta2_f
#if defined ATM_PRESS && !defined SOLVE3D
        real(8), dimension(*) :: Pair_f
        integer :: Pairx, Pairy
#endif
#if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
        real(8), dimension(*) :: eq_tide_f
        integer :: eq_tidex, eq_tidey
#endif
#ifdef DIAGNOSTICS_UV
        real(8), dimension(*) :: DiaU2rhs_f
        integer :: DiaU2rhsz
        integer :: M2zeta, M2pgrd, M2zetw
        integer :: M2zqsp, M2zebh, M2kvrf
#endif
#if defined WEC_VF
        real(8), dimension(*) :: zetaw_f
        integer :: zetawx, zetawy
        real(8), dimension(*) :: qsp_f
        integer :: qspx, qspy
        real(8), dimension(*) :: bh_f
        integer :: bhx, bhy
        real(8), dimension(*) :: rukvf2d_f
        integer :: rukvf2dx, rukvf2dy
#endif
        real(8), dimension(*) :: rhs_var_f, om_v_f
        integer :: om_vx, om_vy
#ifdef DIAGNOSTICS_UV
        real(8), dimension(*) :: DiaV2rhs_f
        integer :: DiaV2rhsz
#endif
#if defined WEC_VF
        real(8), dimension(*) :: rvkvf2d_f
        integer :: rvkvf2dx, rvkvf2dy
#endif
        end subroutine step_loop10
#ifdef UV_C2ADVECTION
        subroutine step_loop11(                                                     &
            &   UFx_f, DUon_f,                                                      &
            &   ubar_f, ubarx, ubary, ubarz,                                        &
            &   UFe_f, DVom_f,                                                      &
            &   VFx_f, VFe_f,                                                         &
            &   vbar_f, vbarx, vbary, vbarz                                         &
        ) bind(C,name="step_loop11")
        use iso_c_binding
        real(8), dimension(*) :: UFx_f, DUon_f
        real(8), dimension(*) :: ubar_f
        integer :: ubarx, ubary, ubarz
        real(8), dimension(*) :: UFe_f, DVom_f
        real(8), dimension(*) :: VFx_f, VFe_f
        real(8), dimension(*) :: vbar_f
        integer :: vbarx, vbary, vbarz
        end subroutine step_loop11
#else
#endif      
      end interface  

!
      PRIVATE
      PUBLIC  :: step2d
!
      CONTAINS
!
      SUBROUTINE step2d (ng, tile)

      USE mod_param
#ifdef SOLVE3D
      USE mod_coupling
#endif
#ifdef DIAGNOSTICS_UV
      USE mod_diags
#endif
      USE mod_forces
      USE mod_grid
#if defined UV_VIS2 || defined UV_VIS4
      USE mod_mixing
#endif
      USE mod_ocean
#if defined SEDIMENT && defined SED_MORPH && defined SOLVE3D
      USE mod_sedbed
#endif
      USE mod_stepping
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
#include "tile.h"
!
#ifdef PROFILE
      CALL wclock_on (ng, iNLM, 9, __LINE__, MyFile)
#endif
      CALL step2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj, N(ng),                      &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
     &                  krhs(ng), kstp(ng), knew(ng),                   &
#ifdef SOLVE3D
     &                  nstp(ng), nnew(ng),                             &
#endif
#ifdef MASKING
     &                  GRID(ng) % pmask,       GRID(ng) % rmask,       &
     &                  GRID(ng) % umask,       GRID(ng) % vmask,       &
#endif
#ifdef WET_DRY
     &                  GRID(ng) % pmask_wet,   GRID(ng) % pmask_full,  &
     &                  GRID(ng) % rmask_wet,   GRID(ng) % rmask_full,  &
     &                  GRID(ng) % umask_wet,   GRID(ng) % umask_full,  &
     &                  GRID(ng) % vmask_wet,   GRID(ng) % vmask_full,  &
# ifdef SOLVE3D
     &                  GRID(ng) % rmask_wet_avg,                       &
# endif
#endif
     &                  GRID(ng) % fomn,        GRID(ng) % h,           &
     &                  GRID(ng) % om_u,        GRID(ng) % om_v,        &
     &                  GRID(ng) % on_u,        GRID(ng) % on_v,        &
     &                  GRID(ng) % omn,                                 &
     &                  GRID(ng) % pm,          GRID(ng) % pn,          &
#if defined CURVGRID && defined UV_ADV
     &                  GRID(ng) % dndx,        GRID(ng) % dmde,        &
#endif
#if defined UV_VIS2 || defined UV_VIS4
     &                  GRID(ng) % pmon_r,      GRID(ng) % pnom_r,      &
     &                  GRID(ng) % pmon_p,      GRID(ng) % pnom_p,      &
     &                  GRID(ng) % om_r,        GRID(ng) % on_r,        &
     &                  GRID(ng) % om_p,        GRID(ng) % on_p,        &
# ifdef UV_VIS2
     &                  MIXING(ng) % visc2_p,   MIXING(ng) % visc2_r,   &
# endif
# ifdef UV_VIS4
     &                  MIXING(ng) % visc4_p,   MIXING(ng) % visc4_r,   &
# endif
#endif
#if defined SEDIMENT && defined SED_MORPH
     &                 SEDBED(ng) % bed_thick,                          &
#endif

#ifdef WEC
# ifdef WEC_VF
#  ifdef WEC_ROLLER
     &                  MIXING(ng) % rurol2d,                           &
     &                  MIXING(ng) % rvrol2d,                           &
#  endif
#  ifdef BOTTOM_STREAMING
     &                  MIXING(ng) % rubst2d,                           &
     &                  MIXING(ng) % rvbst2d,                           &
#  endif
#  ifdef SURFACE_STREAMING
     &                  MIXING(ng) % russt2d,                           &
     &                  MIXING(ng) % rvsst2d,                           &
#  endif
     &                  MIXING(ng) % rubrk2d,                           &
     &                  MIXING(ng) % rvbrk2d,                           &
     &                  MIXING(ng) % rukvf2d,                           &
     &                  MIXING(ng) % rvkvf2d,                           &
     &                  OCEAN(ng) % bh,                                 &
     &                  OCEAN(ng) % qsp,                                &
     &                  OCEAN(ng) % zetaw,                              &
# endif
     &                  OCEAN(ng) % ubar_stokes,                        &
     &                  OCEAN(ng) % vbar_stokes,                        &
#endif
#if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
     &                  OCEAN(ng) % eq_tide,                            &
#endif
#ifndef SOLVE3D
     &                  FORCES(ng) % sustr,     FORCES(ng) % svstr,     &
     &                  FORCES(ng) % bustr,     FORCES(ng) % bvstr,     &
# ifdef ATM_PRESS
     &                  FORCES(ng) % Pair,                              &
# endif
#else
# ifdef VAR_RHO_2D
     &                  COUPLING(ng) % rhoA,    COUPLING(ng) % rhoS,    &
# endif
     &                  COUPLING(ng) % DU_avg1, COUPLING(ng) % DU_avg2, &
     &                  COUPLING(ng) % DV_avg1, COUPLING(ng) % DV_avg2, &
     &                  COUPLING(ng) % Zt_avg1,                         &
     &                  COUPLING(ng) % rufrc,   COUPLING(ng) % rvfrc,   &
     &                  OCEAN(ng) % ru,         OCEAN(ng) % rv,         &
#endif
#ifdef DIAGNOSTICS_UV
     &                  DIAGS(ng) % DiaU2wrk,   DIAGS(ng) % DiaV2wrk,   &
     &                  DIAGS(ng) % DiaRUbar,   DIAGS(ng) % DiaRVbar,   &
# ifdef SOLVE3D
     &                  DIAGS(ng) % DiaU2int,   DIAGS(ng) % DiaV2int,   &
     &                  DIAGS(ng) % DiaRUfrc,   DIAGS(ng) % DiaRVfrc,   &
# endif
#endif
#if defined NESTING && !defined SOLVE3D
     &                  OCEAN(ng) % DU_flux,    OCEAN(ng) % DV_flux,    &
#endif
     &                  OCEAN(ng) % rubar,      OCEAN(ng) % rvbar,      &
     &                  OCEAN(ng) % rzeta,                              &
     &                  OCEAN(ng) % ubar,       OCEAN(ng) % vbar,       &
     &                  OCEAN(ng) % zeta)
#ifdef PROFILE
      CALL wclock_off (ng, iNLM, 9, __LINE__, MyFile)
#endif
!
      RETURN
      END SUBROUTINE step2d
!
!***********************************************************************
      SUBROUTINE step2d_tile (ng, tile,                                 &
     &                        LBi, UBi, LBj, UBj, UBk,                  &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        krhs, kstp, knew,                         &
#ifdef SOLVE3D
     &                        nstp, nnew,                               &
#endif
#ifdef MASKING
     &                        pmask, rmask, umask, vmask,               &
#endif
#ifdef WET_DRY
     &                        pmask_wet, pmask_full,                    &
     &                        rmask_wet, rmask_full,                    &
     &                        umask_wet, umask_full,                    &
     &                        vmask_wet, vmask_full,                    &
# ifdef SOLVE3D
     &                        rmask_wet_avg,                            &
# endif
#endif
     &                        fomn, h,                                  &
     &                        om_u, om_v, on_u, on_v, omn, pm, pn,      &
#if defined CURVGRID && defined UV_ADV
     &                        dndx, dmde,                               &
#endif
#if defined UV_VIS2 || defined UV_VIS4
     &                        pmon_r, pnom_r, pmon_p, pnom_p,           &
     &                        om_r, on_r, om_p, on_p,                   &
# ifdef UV_VIS2
     &                        visc2_p, visc2_r,                         &
# endif
# ifdef UV_VIS4
     &                        visc4_p, visc4_r,                         &
# endif
#endif
#if defined SEDIMENT && defined SED_MORPH
     &                        bed_thick,                                &
#endif
#ifdef WEC
# ifdef WEC_VF
#  ifdef WEC_ROLLER
     &                        rurol2d, rvrol2d,                         &
#  endif
#  ifdef BOTTOM_STREAMING
     &                        rubst2d, rvbst2d,                         &
#  endif
#  ifdef SURFACE_STREAMING
     &                        russt2d, rvsst2d,                         &
#  endif
     &                        rubrk2d, rvbrk2d,                         &
     &                        rukvf2d, rvkvf2d,                         &
     &                        bh, qsp, zetaw,                           &
# endif
     &                        ubar_stokes, vbar_stokes,                 &
#endif
#if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
     &                        eq_tide,                                  &
#endif
#ifndef SOLVE3D
     &                        sustr, svstr, bustr, bvstr,               &
# ifdef ATM_PRESS
     &                        Pair,                                     &
# endif
#else
# ifdef VAR_RHO_2D
     &                        rhoA, rhoS,                               &
# endif
     &                        DU_avg1, DU_avg2,                         &
     &                        DV_avg1, DV_avg2,                         &
     &                        Zt_avg1,                                  &
     &                        rufrc, rvfrc, ru, rv,                     &
#endif
#ifdef DIAGNOSTICS_UV
     &                        DiaU2wrk, DiaV2wrk,                       &
     &                        DiaRUbar, DiaRVbar,                       &
# ifdef SOLVE3D
     &                        DiaU2int, DiaV2int,                       &
     &                        DiaRUfrc, DiaRVfrc,                       &
# endif
#endif
#if defined NESTING && !defined SOLVE3D
     &                        DU_flux, DV_flux,                         &
#endif
     &                        rubar, rvbar, rzeta,                      &
     &                        ubar,  vbar, zeta)
!***********************************************************************
!
      USE mod_param
      USE mod_clima
      USE mod_ncparam
      USE mod_scalars
#if defined SEDIMENT && defined SED_MORPH
      USE mod_sedbed
#endif
      USE mod_sources
!
      USE exchange_2d_mod
#ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
#endif
      USE obc_volcons_mod, ONLY : obc_flux_tile, set_DUV_bc_tile
      USE u2dbc_mod,       ONLY : u2dbc_tile
      USE v2dbc_mod,       ONLY : v2dbc_tile
      USE zetabc_mod,      ONLY : zetabc_tile
#ifdef WET_DRY
      USE wetdry_mod,      ONLY : wetdry_tile
#endif
!
!  Imported variable declarations.
!
      integer, intent(in    ) :: ng, tile
      integer, intent(in    ) :: LBi, UBi, LBj, UBj, UBk
      integer, intent(in    ) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in    ) :: krhs, kstp, knew
#ifdef SOLVE3D
      integer, intent(in    ) :: nstp, nnew
#endif
!
#ifdef ASSUMED_SHAPE
# ifdef MASKING
      real(r8), intent(in   ) :: pmask(LBi:,LBj:)
      real(r8), intent(in   ) :: rmask(LBi:,LBj:)
      real(r8), intent(in   ) :: umask(LBi:,LBj:)
      real(r8), intent(in   ) :: vmask(LBi:,LBj:)
# endif
      real(r8), intent(in   ) :: fomn(LBi:,LBj:)
# if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(inout) :: h(LBi:,LBj:)
# else
      real(r8), intent(in   ) :: h(LBi:,LBj:)
# endif
      real(r8), intent(in   ) :: om_u(LBi:,LBj:)
      real(r8), intent(in   ) :: om_v(LBi:,LBj:)
      real(r8), intent(in   ) :: on_u(LBi:,LBj:)
      real(r8), intent(in   ) :: on_v(LBi:,LBj:)
      real(r8), intent(in   ) :: omn(LBi:,LBj:)
      real(r8), intent(in   ) :: pm(LBi:,LBj:)
      real(r8), intent(in   ) :: pn(LBi:,LBj:)
# if defined CURVGRID && defined UV_ADV
      real(r8), intent(in   ) :: dndx(LBi:,LBj:)
      real(r8), intent(in   ) :: dmde(LBi:,LBj:)
# endif
# if defined UV_VIS2 || defined UV_VIS4
      real(r8), intent(in   ) :: pmon_r(LBi:,LBj:)
      real(r8), intent(in   ) :: pnom_r(LBi:,LBj:)
      real(r8), intent(in   ) :: pmon_p(LBi:,LBj:)
      real(r8), intent(in   ) :: pnom_p(LBi:,LBj:)
      real(r8), intent(in   ) :: om_r(LBi:,LBj:)
      real(r8), intent(in   ) :: on_r(LBi:,LBj:)
      real(r8), intent(in   ) :: om_p(LBi:,LBj:)
      real(r8), intent(in   ) :: on_p(LBi:,LBj:)
#  ifdef UV_VIS2
      real(r8), intent(in   ) :: visc2_p(LBi:,LBj:)
      real(r8), intent(in   ) :: visc2_r(LBi:,LBj:)
#  endif
#  ifdef UV_VIS4
      real(r8), intent(in   ) :: visc4_p(LBi:,LBj:)
      real(r8), intent(in   ) :: visc4_r(LBi:,LBj:)
#  endif
# endif
# if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(in   ) :: bed_thick(LBi:,LBj:,:)
# endif
# ifdef WEC
#  ifdef WEC_VF
#   ifdef WEC_ROLLER
      real(r8), intent(in) :: rurol2d(LBi:,LBj:)
      real(r8), intent(in) :: rvrol2d(LBi:,LBj:)
#   endif
#   ifdef BOTTOM_STREAMING
      real(r8), intent(in) :: rubst2d(LBi:,LBj:)
      real(r8), intent(in) :: rvbst2d(LBi:,LBj:)
#   endif
#   ifdef SURFACE_STREAMING
      real(r8), intent(in) :: russt2d(LBi:,LBj:)
      real(r8), intent(in) :: rvsst2d(LBi:,LBj:)
#   endif
      real(r8), intent(in) :: rubrk2d(LBi:,LBj:)
      real(r8), intent(in) :: rvbrk2d(LBi:,LBj:)
      real(r8), intent(in) :: rukvf2d(LBi:,LBj:)
      real(r8), intent(in) :: rvkvf2d(LBi:,LBj:)
      real(r8), intent(in) :: bh(LBi:,LBj:)
      real(r8), intent(in) :: qsp(LBi:,LBj:)
      real(r8), intent(in) :: zetaw(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: ubar_stokes(LBi:,LBj:)
      real(r8), intent(in) :: vbar_stokes(LBi:,LBj:)
# endif
# if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
      real(r8), intent(in   ) :: eq_tide(LBi:,LBj:)
# endif
# ifndef SOLVE3D
      real(r8), intent(in   ) :: sustr(LBi:,LBj:)
      real(r8), intent(in   ) :: svstr(LBi:,LBj:)
      real(r8), intent(in   ) :: bustr(LBi:,LBj:)
      real(r8), intent(in   ) :: bvstr(LBi:,LBj:)
#  ifdef ATM_PRESS
      real(r8), intent(in   ) :: Pair(LBi:,LBj:)
#  endif
# else
#  ifdef VAR_RHO_2D
      real(r8), intent(in   ) :: rhoA(LBi:,LBj:)
      real(r8), intent(in   ) :: rhoS(LBi:,LBj:)
#  endif
      real(r8), intent(inout) :: DU_avg1(LBi:,LBj:)
      real(r8), intent(inout) :: DU_avg2(LBi:,LBj:)
      real(r8), intent(inout) :: DV_avg1(LBi:,LBj:)
      real(r8), intent(inout) :: DV_avg2(LBi:,LBj:)
      real(r8), intent(inout) :: Zt_avg1(LBi:,LBj:)
      real(r8), intent(inout) :: rufrc(LBi:,LBj:)
      real(r8), intent(inout) :: rvfrc(LBi:,LBj:)
      real(r8), intent(inout) :: ru(LBi:,LBj:,0:,:)
      real(r8), intent(inout) :: rv(LBi:,LBj:,0:,:)
# endif
# ifdef WET_DRY
      real(r8), intent(inout) :: pmask_full(LBi:,LBj:)
      real(r8), intent(inout) :: rmask_full(LBi:,LBj:)
      real(r8), intent(inout) :: umask_full(LBi:,LBj:)
      real(r8), intent(inout) :: vmask_full(LBi:,LBj:)

      real(r8), intent(inout) :: pmask_wet(LBi:,LBj:)
      real(r8), intent(inout) :: rmask_wet(LBi:,LBj:)
      real(r8), intent(inout) :: umask_wet(LBi:,LBj:)
      real(r8), intent(inout) :: vmask_wet(LBi:,LBj:)
#  ifdef SOLVE3D
      real(r8), intent(inout) :: rmask_wet_avg(LBi:,LBj:)
#  endif
# endif
# ifdef DIAGNOSTICS_UV
      real(r8), intent(inout) :: DiaU2wrk(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaV2wrk(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaRUbar(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaRVbar(LBi:,LBj:,:,:)
#  ifdef SOLVE3D
      real(r8), intent(inout) :: DiaU2int(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaV2int(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaRUfrc(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaRVfrc(LBi:,LBj:,:,:)
#  endif
# endif
      real(r8), intent(inout) :: rubar(LBi:,LBj:,:)
      real(r8), intent(inout) :: rvbar(LBi:,LBj:,:)
      real(r8), intent(inout) :: rzeta(LBi:,LBj:,:)
      real(r8), intent(inout) :: ubar(LBi:,LBj:,:)
      real(r8), intent(inout) :: vbar(LBi:,LBj:,:)
      real(r8), intent(inout) :: zeta(LBi:,LBj:,:)
# if defined NESTING && !defined SOLVE3D
      real(r8), intent(out  ) :: DU_flux(LBi:,LBj:)
      real(r8), intent(out  ) :: DV_flux(LBi:,LBj:)
# endif

#else

# ifdef MASKING
      real(r8), intent(in   ) :: pmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: rmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: vmask(LBi:UBi,LBj:UBj)
# endif
      real(r8), intent(in   ) :: fomn(LBi:UBi,LBj:UBj)
# if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(inout) :: h(LBi:UBi,LBj:UBj)
# else
      real(r8), intent(in   ) :: h(LBi:UBi,LBj:UBj)
# endif
      real(r8), intent(in   ) :: om_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: om_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: on_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: on_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: omn(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: pn(LBi:UBi,LBj:UBj)
# if defined CURVGRID && defined UV_ADV
      real(r8), intent(in   ) :: dndx(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: dmde(LBi:UBi,LBj:UBj)
# endif
# if defined UV_VIS2 || defined UV_VIS4
      real(r8), intent(in   ) :: pmon_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: pnom_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: pmon_p(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: pnom_p(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: om_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: on_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: om_p(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: on_p(LBi:UBi,LBj:UBj)
#  ifdef UV_VIS2
      real(r8), intent(in   ) :: visc2_p(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: visc2_r(LBi:UBi,LBj:UBj)
#  endif
#  ifdef UV_VIS4
      real(r8), intent(in   ) :: visc4_p(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: visc4_r(LBi:UBi,LBj:UBj)
#  endif
# endif
# if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(in   ) :: bed_thick(LBi:UBi,LBj:UBj,1:3)
# endif
# ifdef WEC
#  ifdef WEC_VF
#   ifdef WEC_ROLLER
      real(r8), intent(in) :: rurol2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rvrol2d(LBi:UBi,LBj:UBj)
#   endif
#   ifdef BOTTOM_STREAMING
      real(r8), intent(in) :: rubst2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rvbst2d(LBi:UBi,LBj:UBj)
#   endif
#   ifdef SURFACE_STREAMING
      real(r8), intent(in) :: russt2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rvsst2d(LBi:UBi,LBj:UBj)
#   endif
      real(r8), intent(in) :: rubrk2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rvbrk2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rukvf2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rvkvf2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bh(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: qsp(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: zetaw(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: ubar_stokes(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vbar_stokes(LBi:UBi,LBj:UBj)
# endif
# if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
      real(r8), intent(in   ) :: eq_tide(LBi:UBi,LBj:UBj)
# endif
# ifndef SOLVE3D
      real(r8), intent(in   ) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: svstr(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: bustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: bvstr(LBi:UBi,LBj:UBj)
#  ifdef ATM_PRESS
      real(r8), intent(in   ) :: Pair(LBi:UBi,LBj:UBj)
#  endif
# else
#  ifdef VAR_RHO_2D
      real(r8), intent(in   ) :: rhoA(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: rhoS(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(inout) :: DU_avg1(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: DU_avg2(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: DV_avg1(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: DV_avg2(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: Zt_avg1(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: rufrc(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: rvfrc(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: ru(LBi:UBi,LBj:UBj,0:UBk,2)
      real(r8), intent(inout) :: rv(LBi:UBi,LBj:UBj,0:UBk,2)
# endif
# ifdef WET_DRY
      real(r8), intent(inout) :: pmask_full(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: rmask_full(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: umask_full(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: vmask_full(LBi:UBi,LBj:UBj)

      real(r8), intent(inout) :: pmask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: rmask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: umask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: vmask_wet(LBi:UBi,LBj:UBj)
#  ifdef SOLVE3D
      real(r8), intent(inout) :: rmask_wet_avg(LBi:UBi,LBj:UBj)
#  endif
# endif
# ifdef DIAGNOSTICS_UV
      real(r8), intent(inout) :: DiaU2wrk(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaV2wrk(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaRUbar(LBi:UBi,LBj:UBj,2,NDM2d-1)
      real(r8), intent(inout) :: DiaRVbar(LBi:UBi,LBj:UBj,2,NDM2d-1)
#  ifdef SOLVE3D
      real(r8), intent(inout) :: DiaU2int(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaV2int(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaRUfrc(LBi:UBi,LBj:UBj,3,NDM2d-1)
      real(r8), intent(inout) :: DiaRVfrc(LBi:UBi,LBj:UBj,3,NDM2d-1)
#  endif
# endif
      real(r8), intent(inout) :: rubar(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: rvbar(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: rzeta(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: ubar(LBi:UBi,LBj:UBj,:)
      real(r8), intent(inout) :: vbar(LBi:UBi,LBj:UBj,:)
      real(r8), intent(inout) :: zeta(LBi:UBi,LBj:UBj,:)
# if defined NESTING && !defined SOLVE3D
      real(r8), intent(out  ) :: DU_flux(LBi:UBi,LBj:UBj)
      real(r8), intent(out  ) :: DV_flux(LBi:UBi,LBj:UBj)
# endif
#endif
!
!  Local variable declarations.
!
      logical :: CORRECTOR_2D_STEP
!
      integer :: i, is, j, ptsk
#ifdef DIAGNOSTICS_UV
      integer :: idiag
#endif
!.
      real(r8) :: cff, cff1, cff2, cff3, cff4, cff5, cff6, cff7
      real(r8) :: fac, fac1, fac2, fac3
!
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Dgrad
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Dnew
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Drhs
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Drhs_p
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Dstp
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DUon
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DVom
#ifdef WEC
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DUSon
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DVSom
#endif
#ifdef WEC_VF
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DUSom
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DVSon
#endif
#ifdef UV_VIS4
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: LapU
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: LapV
#endif
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: UFe
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: UFx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: VFe
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: VFx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: grad
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: gzeta
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: gzeta2
#if defined VAR_RHO_2D && defined SOLVE3D
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: gzetaSA
#endif
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: rhs_ubar
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: rhs_vbar
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: rhs_zeta
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: zeta_new
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: zwrk
#ifdef WET_DRY
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: wetdry
#endif
#ifdef DIAGNOSTICS_UV
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Uwrk
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Vwrk
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,NDM2d-1) :: DiaU2rhs
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,NDM2d-1) :: DiaV2rhs
#endif

#include "set_bounds.h"
!
      ptsk=3-kstp
      CORRECTOR_2D_STEP=.not.PREDICTOR_2D_STEP(ng)
!
!-----------------------------------------------------------------------
!  Compute total depth (m) and vertically integrated mass fluxes.
!-----------------------------------------------------------------------
!
      call pass_const(                                                  &
        Istr, IstrB, IstrP, IstrR, IstrT, IstrM, IstrU,                  &
        Iend, IendB, IendP, IendR, IendT,                               &
        Jstr, JstrB, JstrP, JstrR, JstrT, JstrM, JstrV,                 &
        Jend, JendB, JendP, JendR, JendT,                               &
        Istrm3, Istrm2, Istrm1, IstrUm2, IstrUm1,                       &
        Iendp1, Iendp2, Iendp2i, Iendp3,                                &
        Jstrm3, Jstrm2, Jstrm1, JstrVm2, JstrVm1,                       &
        Jendp1, Jendp2, Jendp2i, Jendp3,                                &
        ng, tile,                                                       &
        LBi, UBi, LBj, UBj, UBk,                                        &
        IminS, ImaxS, JminS, JmaxS,                                      &
        krhs, kstp, knew                                                &
#ifdef SOLVE3D
        ,nstp, nnew                                                     &
#endif
    )

        call step_loop1(                                                &
            Drhs, zeta,                                                 &
            size(zeta,1),size(zeta,2),size(zeta,3),                     &
            h,size(h,1),size(h,2),                                      &
            on_u,size(on_u,1),size(on_u,2),                             &
            DUon, ubar,                                                 &
            size(ubar,1),size(ubar,2),size(ubar,3),                     &
#ifdef WEC
#ifdef WET_DRY
            umask_wet,                                                  &
            size(umask_wet,1),size(umask_wet,2),                        &
#endif
            DUson, ubar_stokes,                                         &
            size(ubar_stokes,1),size(ubar_stokes,2),                    &
            size(ubar_stokes,3),                                        &
#endif
            om_v,size(om_v,1),size(om_v,2),                             &
            DVom, vbar,                                                 &
            size(vbar,1),size(vbar,2),size(vbar,3)                      &
#ifdef WEC
#ifdef WET_DRY
            ,vmask_wet,                                                 &
            size(vmask_wet,1),size(vmask_wet,2)                         &
#endif
            ,DVsom, vbar_stokes,                                        &
            size(vbar_stokes,1),size(vbar_stokes,2),                    &
            size(vbar_stokes,3)                                         &
#endif
    )
! #if defined DISTRIBUTE && !defined NESTING
! 
! !  In distributed-memory, the I- and J-ranges are different and a
! !  special exchange is done to avoid having three ghost points for
! !  high order numerical stencils. Notice that a private array is
! !  passed below to the exchange routine. It also applies periodic
! !  boundary conditions, if appropriate and no partitions in I- or
! !  J-directions.
! !
!       DO j=JstrV-2,Jendp2
!         DO i=IstrU-2,Iendp2
!           Drhs(i,j)=zeta(i,j,krhs)+h(i,j)
!         END DO
!       END DO
!       DO j=JstrV-2,Jendp2
!         DO i=IstrU-1,Iendp2
!           cff=0.5_r8*on_u(i,j)
!           cff1=cff*(Drhs(i,j)+Drhs(i-1,j))
!           DUon(i,j)=ubar(i,j,krhs)*cff1
! # ifdef WEC
! #  ifdef WET_DRY
!           cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
!           cff6=0.5_r8+DSIGN(0.5_r8,ubar_stokes(i,j))*umask_wet(i,j)
!           cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
!           cff1=cff1*cff7
! #  endif
!           DUSon(i,j)=ubar_stokes(i,j)*cff1
!           DUon(i,j)=DUon(i,j)+DUSon(i,j)
! # endif
!         END DO
!       END DO
!       DO j=JstrV-1,Jendp2
!         DO i=IstrU-2,Iendp2
!           cff=0.5_r8*om_v(i,j)
!           cff1=cff*(Drhs(i,j)+Drhs(i,j-1))
!           DVom(i,j)=vbar(i,j,krhs)*cff1
! # ifdef WEC
! #  ifdef WET_DRY
!           cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
!           cff6=0.5_r8+DSIGN(0.5_r8,vbar_stokes(i,j))*vmask_wet(i,j)
!           cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
!           cff1=cff1*cff7
! #  endif
!           DVSom(i,j)=vbar_stokes(i,j)*cff1
!           DVom(i,j)=DVom(i,j)+DVSom(i,j)
! # endif
!         END DO
!       END DO
! 
! #else
! 
!       DO j=JstrVm2-1,Jendp2
!         DO i=IstrUm2-1,Iendp2
!           Drhs(i,j)=zeta(i,j,krhs)+h(i,j)
!         END DO
!       END DO
!       DO j=JstrVm2-1,Jendp2
!         DO i=IstrUm2,Iendp2
!           cff=0.5_r8*on_u(i,j)
!           cff1=cff*(Drhs(i,j)+Drhs(i-1,j))
!           DUon(i,j)=ubar(i,j,krhs)*cff1
! # ifdef WEC
! #  ifdef WET_DRY
!           cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
!           cff6=0.5_r8+DSIGN(0.5_r8,ubar_stokes(i,j))*umask_wet(i,j)
!           cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
!           cff1=cff1*cff7
! #  endif
!           DUSon(i,j)=ubar_stokes(i,j)*cff1
!           DUon(i,j)=DUon(i,j)+DUSon(i,j)
! # endif
!         END DO
!       END DO
!       DO j=JstrVm2,Jendp2
!         DO i=IstrUm2-1,Iendp2
!           cff=0.5_r8*om_v(i,j)
!           cff1=cff*(Drhs(i,j)+Drhs(i,j-1))
!           DVom(i,j)=vbar(i,j,krhs)*cff1
! # ifdef WEC
! #  ifdef WET_DRY
!           cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
!           cff6=0.5_r8+DSIGN(0.5_r8,vbar_stokes(i,j))*vmask_wet(i,j)
!           cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
!           cff1=cff1*cff7
! #  endif
!           DVSom(i,j)=vbar_stokes(i,j)*cff1
!           DVom(i,j)=DVom(i,j)+DVSom(i,j)
! # endif
!         END DO
!       END DO
! #endif
#ifdef DISTRIBUTE
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          DUon)
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          DVom)
      END IF
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    IminS, ImaxS, JminS, JmaxS,                   &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    DUon, DVom)
#endif
!
!  Set vertically integrated mass fluxes DUon and DVom along the open
!  boundaries in such a way that the integral volume is conserved.
!
      IF (ANY(VolCons(:,ng))) THEN
        CALL set_DUV_bc_tile (ng, tile,                                 &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        krhs,                                     &
#ifdef MASKING
     &                        umask, vmask,                             &
#endif
     &                        om_v, on_u,                               &
     &                        ubar, vbar,                               &
     &                        Drhs, DUon, DVom)
      END IF
#ifdef SOLVE3D
!
!-----------------------------------------------------------------------
!  Compute time averaged fields over all short time-steps.
!-----------------------------------------------------------------------
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        IF (FIRST_2D_STEP) THEN
!
!  Reset arrays for 2D fields averaged within the short time-steps.
!
          cff2=(-1.0_r8/12.0_r8)*weight(2,iif(ng)+1,ng)
          call step_loop2(                                              &
            cff2,                                                       &
            Zt_avg1, size(Zt_avg1,1), size(Zt_avg1,2),                  &
            DU_avg1, size(DU_avg1,1), size(DU_avg1,2),                  &
            DU_avg2, size(DU_avg2,1), size(DU_avg2,2),                  &
            DUon,                                                       &
            DV_avg1, size(DV_avg1,1), size(DV_avg1,2),                  &
            DV_avg2, size(DV_avg2,1), size(DV_avg2,2),                  &
            DVom)
!          DO j=JstrR,JendR
!            DO i=IstrR,IendR
!              Zt_avg1(i,j)=0.0_r8
!            END DO
!            DO i=Istr,IendR
!              DU_avg1(i,j)=0.0_r8
!              DU_avg2(i,j)=cff2*DUon(i,j)
!            END DO
!          END DO
!          DO j=Jstr,JendR
!            DO i=IstrR,IendR
!              DV_avg1(i,j)=0.0_r8
!              DV_avg2(i,j)=cff2*DVom(i,j)
!            END DO
!          END DO
        ELSE
!
!  Accumulate field averages of previous time-step after they are
!  computed in the previous corrector step, updated their boundaries,
!  and synchronized.
!
          cff1=weight(1,iif(ng)-1,ng)
          cff2=(8.0_r8/12.0_r8)*weight(2,iif(ng)  ,ng)-                 &
     &         (1.0_r8/12.0_r8)*weight(2,iif(ng)+1,ng)
          call step_loop3(                                            &
            cff1, cff2,                                                 &
            Zt_avg1, size(Zt_avg1,1), size(Zt_avg1,2),                  &
            zeta, size(zeta,1), size(zeta,2), size(zeta,3),             &
            DU_avg1, size(DU_avg1,1), size(DU_avg1,2),                  &
            DU_avg2, size(DU_avg2,1), size(DU_avg2,2),                  &
            DUon,                                                       &
            DV_avg1, size(DV_avg1,1), size(DV_avg1,2),                  &
            DV_avg2, size(DV_avg2,1), size(DV_avg2,2),                  &
            DVom                                                        &
#ifdef WEC
            ,DUSon, DVSon                                               &
#endif
        )
!           DO j=JstrR,JendR
!             DO i=IstrR,IendR
!               Zt_avg1(i,j)=Zt_avg1(i,j)+cff1*zeta(i,j,krhs)
!             END DO
!             DO i=Istr,IendR
!               DU_avg1(i,j)=DU_avg1(i,j)+cff1*DUon(i,j)
! # ifdef WEC
!               DU_avg1(i,j)=DU_avg1(i,j)-cff1*DUSon(i,j)
! # endif
!               DU_avg2(i,j)=DU_avg2(i,j)+cff2*DUon(i,j)
!             END DO
!           END DO
!           DO j=Jstr,JendR
!             DO i=IstrR,IendR
!               DV_avg1(i,j)=DV_avg1(i,j)+cff1*DVom(i,j)
! # ifdef WEC
!               DV_avg1(i,j)=DV_avg1(i,j)-cff1*DVSom(i,j)
! # endif
!               DV_avg2(i,j)=DV_avg2(i,j)+cff2*DVom(i,j)
!             END DO
!           END DO
        END IF
      ELSE
        IF (FIRST_2D_STEP) THEN
          cff2=weight(2,iif(ng),ng)
        ELSE
          cff2=(5.0_r8/12.0_r8)*weight(2,iif(ng),ng)
        END IF
        call step_loop4(                                                &
            cff2,                                                       &
            DU_avg2, size(DU_avg2,1), size(DU_avg2,2),                  &
            DUon,                                                       &
            DV_avg2, size(DV_avg2,1), size(DV_avg2,2),                  &
            DVom                                                        &
        )
!         DO j=JstrR,JendR
!           DO i=Istr,IendR
!             DU_avg2(i,j)=DU_avg2(i,j)+cff2*DUon(i,j)
!           END DO
!         END DO
!         DO j=Jstr,JendR
!           DO i=IstrR,IendR
!             DV_avg2(i,j)=DV_avg2(i,j)+cff2*DVom(i,j)
!           END DO
!         END DO
      END IF
!
!  After all fast time steps are completed, apply boundary conditions
!  to time averaged fields.
# ifdef NESTING
!  In nesting applications with refinement grids, we need to exchange
!  the DU_avg2 and DV_avg2 fluxes boundary information for the case
!  that a contact point is at a tile partition. Notice that in such
!  cases, we need i+1 and j+1 values for spatial/temporal interpolation.
# endif
!
      IF ((iif(ng).eq.(nfast(ng)+1)).and.PREDICTOR_2D_STEP(ng)) THEN
        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_r2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            Zt_avg1)
          CALL exchange_u2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            DU_avg1)
          CALL exchange_v2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            DV_avg1)
# ifdef NESTING
          CALL exchange_u2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            DU_avg2)
          CALL exchange_v2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            DV_avg2)
# endif
        END IF
# ifdef DISTRIBUTE
        CALL mp_exchange2d (ng, tile, iNLM, 3,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints,                               &
     &                      EWperiodic(ng), NSperiodic(ng),             &
     &                      Zt_avg1, DU_avg1, DV_avg1)
#  ifdef NESTING
        CALL mp_exchange2d (ng, tile, iNLM, 2,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints,                               &
     &                      EWperiodic(ng), NSperiodic(ng),             &
     &                      DU_avg2, DV_avg2)
#  endif
# endif
      END IF
#endif
#ifdef WET_DRY
!
!-----------------------------------------------------------------------
!  Compute new wet/dry masks.
!-----------------------------------------------------------------------
!
      CALL wetdry_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
# ifdef MASKING
     &                  pmask, rmask, umask, vmask,                     &
# endif
     &                  h, zeta(:,:,kstp),                              &
# ifdef SOLVE3D
     &                  DU_avg1, DV_avg1,                               &
     &                  rmask_wet_avg,                                  &
# endif
     &                  pmask_wet, pmask_full,                          &
     &                  rmask_wet, rmask_full,                          &
     &                  umask_wet, umask_full,                          &
     &                  vmask_wet, vmask_full)
#endif
!
!  Do not perform the actual time stepping during the auxiliary
!  (nfast(ng)+1) time step.
!
      IF (iif(ng).gt.nfast(ng)) RETURN
!
!=======================================================================
!  Time step free-surface equation.
!=======================================================================
!
!  During the first time-step, the predictor step is Forward-Euler
!  and the corrector step is Backward-Euler. Otherwise, the predictor
!  step is Leap-frog and the corrector step is Adams-Moulton.
#if defined VAR_RHO_2D && defined SOLVE3D
!  Recall that the vertical averaged density (rhoA) and density
!  pertubation (rhoS) are nondimensional quantities.
!
      fac=1000.0_r8/rho0                                ! nondimensional
#endif
!
      IF (FIRST_2D_STEP) THEN
        cff1=dtfast(ng)
        call step_loop5(                                                &
            cff1, rhs_zeta, DUon, DVom,                                 &
            zeta_new, zeta,                                             &
            size(zeta,1),size(zeta,2),size(zeta,3),                     &
            pm, size(pm,1),size(pm,2),                                  &
            pn, size(pn,1),size(pn,2),                                  &
#ifdef MASKING
            rmask, size(rmask,1),size(rmask,2),                         &
#endif
            Dnew, h, size(h,1),size(h,2),                               &
            zwrk                                                        &
#if defined VAR_RHO_2D && defined SOLVE3D
            ,fac, gzeta, rhoS,                                          &
            size(rhoS,1),size(rhoS,2),                                  &
            gzeta2, gzetaSA, rhoA,                                      &
            size(rhoA,1),size(rhoA,2)                                   &
#else
            ,gzeta, gzeta2                                              &
#endif                                             
            )
!         DO j=JstrV-1,Jend
!           DO i=IstrU-1,Iend
!             rhs_zeta(i,j)=(DUon(i,j)-DUon(i+1,j))+                      &
!      &                    (DVom(i,j)-DVom(i,j+1))
!             zeta_new(i,j)=zeta(i,j,kstp)+                               &
!      &                    pm(i,j)*pn(i,j)*cff1*rhs_zeta(i,j)
! #ifdef MASKING
!             zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
! #endif
!             Dnew(i,j)=zeta_new(i,j)+h(i,j)
! !
!             zwrk(i,j)=0.5_r8*(zeta(i,j,kstp)+zeta_new(i,j))
! #if defined VAR_RHO_2D && defined SOLVE3D
!             gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
!             gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
!             gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
! #else
!             gzeta(i,j)=zwrk(i,j)
!             gzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
! #endif
!           END DO
!         END DO
      ELSE IF (PREDICTOR_2D_STEP(ng)) THEN
        cff1=2.0_r8*dtfast(ng)
        cff4=4.0_r8/25.0_r8
        cff5=1.0_r8-2.0_r8*cff4
        call step_loop6(                                                    &
            cff1, cff4, cff5,                                               &
            rhs_zeta, DUon, DVom,                                           &
            zeta_new, zeta,                                                 &
            size(zeta,1),size(zeta,2),size(zeta,3),                         &
            pm, size(pm,1),size(pm,2),                                      &
            pn, size(pn,1),size(pn,2),                                      &
#ifdef MASKING
            rmask, size(rmask,1),size(rmask,2),                             &
#endif
            Dnew, h, size(h,1),size(h,2),                                   &
            zwrk                                                            &
#if defined VAR_RHO_2D && defined SOLVE3D
            ,fac, gzeta, rhoS,                                              &
            size(rhoS,1),size(rhoS,2),                                      &
            gzeta2, gzetaSA, rhoA,                                          &
            size(rhoA,1),size(rhoA,2)                                       &
#else
            ,gzeta, gzeta2                                                  &
#endif
        )
!         DO j=JstrV-1,Jend
!           DO i=IstrU-1,Iend
!             rhs_zeta(i,j)=(DUon(i,j)-DUon(i+1,j))+                      &
!      &                    (DVom(i,j)-DVom(i,j+1))
!             zeta_new(i,j)=zeta(i,j,kstp)+                               &
!      &                    pm(i,j)*pn(i,j)*cff1*rhs_zeta(i,j)
! #ifdef MASKING
!             zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
! #endif
!             Dnew(i,j)=zeta_new(i,j)+h(i,j)
! !
!             zwrk(i,j)=cff5*zeta(i,j,krhs)+                              &
!      &                cff4*(zeta(i,j,kstp)+zeta_new(i,j))
! #if defined VAR_RHO_2D && defined SOLVE3D
!             gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
!             gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
!             gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
! #else
!             gzeta(i,j)=zwrk(i,j)
!             gzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
! #endif
!           END DO
!         END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=dtfast(ng)*5.0_r8/12.0_r8
        cff2=dtfast(ng)*8.0_r8/12.0_r8
        cff3=dtfast(ng)*1.0_r8/12.0_r8
        cff4=2.0_r8/5.0_r8
        cff5=1.0_r8-cff4
        call step_loop7(                                                    &
            cff1, cff2, cff3, cff4, cff5,                                   &
            DUon, DVom,                                                     &
            zeta_new, zeta,                                                 &
            size(zeta,1),size(zeta,2),size(zeta,3),                         &
            pm, size(pm,1),size(pm,2),                                      &
            pn, size(pn,1),size(pn,2),                                      &
            rzeta, size(rzeta,1),size(rzeta,2),size(rzeta,3),               &
#ifdef MASKING
            rmask, size(rmask,1),size(rmask,2),                             &
#endif
            Dnew, h, size(h,1),size(h,2),                                   &
            zwrk                                                            &
#if defined VAR_RHO_2D && defined SOLVE3D
            ,fac, gzeta, rhoS,                                              &
            size(rhoS,1),size(rhoS,2),                                      &
            gzeta2, gzetaSA, rhoA,                                          &
            size(rhoA,1),size(rhoA,2)                                       &
#else
            ,gzeta, gzeta2                                                  &
#endif
        )
!         DO j=JstrV-1,Jend
!           DO i=IstrU-1,Iend
!             cff=cff1*((DUon(i,j)-DUon(i+1,j))+                          &
!      &                (DVom(i,j)-DVom(i,j+1)))
!             zeta_new(i,j)=zeta(i,j,kstp)+                               &
!      &                    pm(i,j)*pn(i,j)*(cff+                         &
!      &                                     cff2*rzeta(i,j,kstp)-        &
!      &                                     cff3*rzeta(i,j,ptsk))
! #ifdef MASKING
!             zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
! #endif
!             Dnew(i,j)=zeta_new(i,j)+h(i,j)
! !
!             zwrk(i,j)=cff5*zeta_new(i,j)+cff4*zeta(i,j,krhs)
! #if defined VAR_RHO_2D && defined SOLVE3D
!             gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
!             gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
!             gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
! #else
!             gzeta(i,j)=zwrk(i,j)
!             gzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
! #endif
!           END DO
!         END DO
      END IF
!
!  Load new free-surface values into shared array at both predictor
!  and corrector steps.
#ifdef WET_DRY
!  Modify new free-surface to Ensure that depth is > Dcrit for masked
!  cells.
#endif
!
      call step_loop8(                                                      &
          zeta,size(zeta,1),size(zeta,2),size(zeta,3),                      &
          zeta_new                                                          &
#if defined WET_DRY && defined MASKING
          ,Dcrit, h, size(h,1),size(h,2),                                   &
          rmask, size(rmask,1),size(rmask,2)                                &
#endif                                                          
      )
!       DO j=Jstr,Jend
!         DO i=Istr,Iend
!           zeta(i,j,knew)=zeta_new(i,j)
! #if defined WET_DRY && defined MASKING
!           zeta(i,j,knew)=zeta(i,j,knew)+                                &
!      &                   (Dcrit(ng)-h(i,j))*(1.0_r8-rmask(i,j))
! #endif
!         END DO
!       END DO
!
!  If predictor step, load right-side-term into shared array.
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        DO j=Jstr,Jend
          DO i=Istr,Iend
            rzeta(i,j,krhs)=rhs_zeta(i,j)
          END DO
        END DO
        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_r2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            rzeta(:,:,krhs))
        END IF
#ifdef DISTRIBUTE
        CALL mp_exchange2d (ng, tile, iNLM, 1,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints,                               &
     &                      EWperiodic(ng), NSperiodic(ng),             &
     &                      rzeta(:,:,krhs))
#endif
      END IF
!
!  Apply mass point sources (volume vertical influx), if any.
!
!    Dsrc(is) = 2,  flow across grid cell w-face (positive or negative)
!
      IF (LwSrc(ng)) THEN
        DO is=1,Nsrc(ng)
          IF (INT(SOURCES(ng)%Dsrc(is)).eq.2) THEN
            i=SOURCES(ng)%Isrc(is)
            j=SOURCES(ng)%Jsrc(is)
            IF (((IstrR.le.i).and.(i.le.IendR)).and.                    &
     &          ((JstrR.le.j).and.(j.le.JendR))) THEN
              zeta(i,j,knew)=zeta(i,j,knew)+                            &
     &                       SOURCES(ng)%Qbar(is)*                      &
     &                       pm(i,j)*pn(i,j)*dtfast(ng)
            END IF
          END IF
        END DO
      END IF

#if defined SEDIMENT && defined SED_MORPH
!
!  Scale the bed change with the fast time stepping. The half is
!  becasue we do predictor and corrector. The "ndtfast/nfast" is
!  becasue we do "nfast" steps to here.
!
      fac=0.5_r8*dtfast(ng)*ndtfast(ng)/(nfast(ng)*dt(ng))
      call step_loop9(                                                      &
          fac, bed_thick,                                                   & 
          size(bed_thick,1),size(bed_thick,2),size(bed_thick,3),            &
          h,size(h,1),size(h,2)                                             &
      )
      ! DO j=Jstr,Jend
      !   DO i=Istr,Iend
      !     cff=fac*(bed_thick(i,j,nstp)-bed_thick(i,j,nnew))
      !     h(i,j)=h(i,j)-cff
      !   END DO
      ! END DO
#endif
!
!  Set free-surface lateral boundary conditions.
!
      CALL zetabc_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
     &                  krhs, kstp, knew,                               &
     &                  zeta)
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          zeta(:,:,knew))
      END IF
#ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    zeta(:,:,knew))
#endif
!
!=======================================================================
!  Compute right-hand-side for the 2D momentum equations.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Compute pressure gradient terms.
!-----------------------------------------------------------------------
!
      cff1=0.5_r8*g
      cff2=1.0_r8/3.0_r8
#if !defined SOLVE3D && defined ATM_PRESS
      fac3=0.5_r8*100.0_r8/rho0
#endif
      call step_loop10(                                                     &
            cff1,cff2,g,                                                    &
#if !defined SOLVE3D && defined ATM_PRESS
            fac3,                                                           &
#endif
            rhs_ubar,on_u,size(on_u,1),size(on_u,2),                        &
            h, size(h,1),size(h,2),                                         &
            gzeta,                                                          &
#if defined VAR_RHO_2D && defined SOLVE3D
            gzetaSA,rhoA,size(rhoA,1),size(rhoA,2),                         &
            zwrk,                                                           &
#endif
            gzeta2,                                                         &
#if defined ATM_PRESS && !defined SOLVE3D            
            Pair,size(Pair,1),size(Pair,2),                                 &
#endif
#if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
            eq_tide,size(eq_tide,1),size(eq_tide,2),                        &
#endif
#ifdef DIAGNOSTICS_UV 
            DiaU2rhs,size(DiaU2rhs,3),                                      &
            M2pgrd,                                                         &
#if defined WEC_VF
            M2zeta,M2zetw,                                                  &
            M2zqsp,M2zebh,M2kvrf,                                           &
#endif
#endif
#if defined WEC_VF
            zetaw,size(zetaw,1),size(zetaw,2),                              &
            qsp,size(qsp,1),size(qsp,2),                                    &
            bh,size(bh,1),size(bh,2),                                       &
            rukvf2d,size(rukvf2d,1),size(rukvf2d,2),                        &
#endif
            rhs_vbar,om_v,size(om_v,1),size(om_v,2)                         &
#ifdef DIAGNOSTICS_UV
            ,DiaV2rhs,size(DiaV2rhs,3)                                      &
#endif
#if defined WEC_VF
            ,rvkvf2d,size(rvkvf2d,1),size(rvkvf2d,2)                        &
#endif                                      
      )
!       DO j=Jstr,Jend
!         DO i=IstrU,Iend
!           rhs_ubar(i,j)=cff1*on_u(i,j)*                                 &
!      &                  ((h(i-1,j)+                                     &
!      &                    h(i ,j))*                                     &
!      &                   (gzeta(i-1,j)-                                 &
!      &                    gzeta(i  ,j))+                                &
! #if defined VAR_RHO_2D && defined SOLVE3D
!      &                   (h(i-1,j)-                                     &
!      &                    h(i  ,j))*                                    &
!      &                   (gzetaSA(i-1,j)+                               &
!      &                    gzetaSA(i  ,j)+                               &
!      &                    cff2*(rhoA(i-1,j)-                            &
!      &                          rhoA(i  ,j))*                           &
!      &                         (zwrk(i-1,j)-                            &
!      &                          zwrk(i  ,j)))+                          &
! #endif
!      &                   (gzeta2(i-1,j)-                                &
!      &                    gzeta2(i  ,j)))
! #if defined ATM_PRESS && !defined SOLVE3D
!           rhs_ubar(i,j)=rhs_ubar(i,j)-                                  &
!      &                  fac3*on_u(i,j)*                                 &
!      &                  (h(i-1,j)+h(i,j)+                               &
!      &                   gzeta(i-1,j)+gzeta(i,j))*                      &
!      &                  (Pair(i,j)-Pair(i-1,j))
! #endif
! #if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
!           rhs_ubar(i,j)=rhs_ubar(i,j)-                                  &
!      &                  cff1*on_u(i,j)*                                 &
!      &                  (h(i-1,j)+h(i,j)+                               &
!      &                   gzeta(i-1,j)+gzeta(i,j))*                      &
!      &                  (eq_tide(i,j)-eq_tide(i-1,j))
! #endif
! #ifdef DIAGNOSTICS_UV
!           DiaU2rhs(i,j,M2pgrd)=rhs_ubar(i,j)
! #endif
! #if defined WEC_VF
!           cff3=0.5_r8*on_u(i,j)*                                        &
!      &         (h(i-1,j)+h(i,j)+                                        &
!      &         gzeta(i-1,j)+gzeta(i,j))
!           cff4=cff3*g*(zetaw(i-1,j)-zetaw(i,j))
!           cff5=cff3*g*(qsp(i-1,j)-qsp(i,j))
!           cff6=cff3*(bh(i-1,j)-bh(i,j))
!           cff7=rukvf2d(i,j)
!           rhs_ubar(i,j)=rhs_ubar(i,j)-cff4-cff5+cff6+cff7
! # ifdef DIAGNOSTICS_UV
!           DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2pgrd)
!           DiaU2rhs(i,j,M2pgrd)=DiaU2rhs(i,j,M2pgrd)-cff4-cff5+cff6
!           DiaU2rhs(i,j,M2zetw)=-cff4
!           DiaU2rhs(i,j,M2zqsp)=-cff5
!           DiaU2rhs(i,j,M2zbeh)=cff6
!           DiaU2rhs(i,j,M2kvrf)=cff7
! #  ifndef UV_ADV
!           DiaU2rhs(i,j,M2hjvf)=0.0_r8
! #  endif
! # endif
! #endif
!         END DO
!         IF (j.ge.JstrV) THEN
!           DO i=Istr,Iend
!             rhs_vbar(i,j)=cff1*om_v(i,j)*                               &
!      &                    ((h(i,j-1)+                                   &
!      &                      h(i,j  ))*                                  &
!      &                     (gzeta(i,j-1)-                               &
!      &                      gzeta(i,j  ))+                              &
! #if defined VAR_RHO_2D && defined SOLVE3D
!      &                     (h(i,j-1)-                                   &
!      &                      h(i,j  ))*                                  &
!      &                     (gzetaSA(i,j-1)+                             &
!      &                      gzetaSA(i,j  )+                             &
!      &                      cff2*(rhoA(i,j-1)-                          &
!      &                            rhoA(i,j  ))*                         &
!      &                           (zwrk(i,j-1)-                          &
!      &                            zwrk(i,j  )))+                        &
! #endif
!      &                     (gzeta2(i,j-1)-                              &
!      &                      gzeta2(i,j  )))
! #if defined ATM_PRESS && !defined SOLVE3D
!             rhs_vbar(i,j)=rhs_vbar(i,j)-                                &
!      &                    fac3*om_v(i,j)*                               &
!      &                    (h(i,j-1)+h(i,j)+                             &
!      &                     gzeta(i,j-1)+gzeta(i,j))*                    &
!      &                    (Pair(i,j)-Pair(i,j-1))
! #endif
! #if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
!             rhs_vbar(i,j)=rhs_vbar(i,j)-                                &
!      &                    cff1*om_v(i,j)*                               &
!      &                    (h(i,j-1)+h(i,j)+                             &
!      &                     gzeta(i,j-1)+gzeta(i,j))*                    &
!      &                    (eq_tide(i,j)-eq_tide(i,j-1))
! #endif
! #ifdef DIAGNOSTICS_UV
!             DiaV2rhs(i,j,M2pgrd)=rhs_vbar(i,j)
! #endif
! #if defined WEC_VF
!             cff3=0.5_r8*om_v(i,j)*                                      &
!      &           (h(i,j-1)+h(i,j)+                                      &
!      &           gzeta(i,j-1)+gzeta(i,j))
!             cff4=cff3*g*(zetaw(i,j-1)-zetaw(i,j))
!             cff5=cff3*g*(qsp(i,j-1)-qsp(i,j))
!             cff6=cff3*(bh(i,j-1)-bh(i,j))
!             cff7=rvkvf2d(i,j)
!             rhs_vbar(i,j)=rhs_vbar(i,j)-cff4-cff5+cff6+cff7
! # ifdef DIAGNOSTICS_UV
!             DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2pgrd)
!             DiaV2rhs(i,j,M2pgrd)=DiaV2rhs(i,j,M2pgrd)-cff4-cff5+cff6
!             DiaV2rhs(i,j,M2zetw)=-cff4
!             DiaV2rhs(i,j,M2zqsp)=-cff5
!             DiaV2rhs(i,j,M2zbeh)=cff6
!             DiaV2rhs(i,j,M2kvrf)=cff7
! #  ifndef UV_ADV
!             DiaV2rhs(i,j,M2hjvf)=0.0_r8
! #  endif
! # endif
! #endif
!           END DO
!         END IF
!       END DO
#ifdef UV_ADV
!
!-----------------------------------------------------------------------
!  Add in horizontal advection of momentum.
!-----------------------------------------------------------------------

# ifdef UV_C2ADVECTION
!
!  Second-order, centered differences advection fluxes.
!
      call step_loop11(                                                     &
            UFx, DUon,                                                      &
            ubar,size(ubar,1),size(ubar,2),size(ubar,3),                    &
            UFe, DVom,                                                      &
            VFx, VFe,                                                       &
            vbar,size(vbar,1),size(vbar,2),size(vbar,3)                     &
      )
!       DO j=Jstr,Jend
!         DO i=IstrU-1,Iend
!           UFx(i,j)=0.25_r8*(DUon(i,j)+DUon(i+1,j))*                     &
!      &                     (ubar(i  ,j,krhs)+                           &
!      &                      ubar(i+1,j,krhs))
!         END DO
!       END DO
! !
!       DO j=Jstr,Jend+1
!         DO i=IstrU,Iend
!           UFe(i,j)=0.25_r8*(DVom(i,j)+DVom(i-1,j))*                     &
!      &                     (ubar(i,j  ,krhs)+                           &
!      &                      ubar(i,j-1,krhs))
!         END DO
!       END DO
! !
!       DO j=JstrV,Jend
!         DO i=Istr,Iend+1
!           VFx(i,j)=0.25_r8*(DUon(i,j)+DUon(i,j-1))*                     &
!      &                     (vbar(i  ,j,krhs)+                           &
!      &                      vbar(i-1,j,krhs))
!         END DO
!       END DO
! !
!       DO j=JstrV-1,Jend
!         DO i=Istr,Iend
!           VFe(i,j)=0.25_r8*(DVom(i,j)+DVom(i,j+1))*                     &
!      &                     (vbar(i,j  ,krhs)+                           &
!      &                      vbar(i,j+1,krhs))
!         END DO
!       END DO
# else
!
!  Fourth-order, centered differences advection fluxes.
!
      DO j=Jstr,Jend
        DO i=IstrUm1,Iendp1
          grad (i,j)=ubar(i-1,j,krhs)-2.0_r8*ubar(i,j,krhs)+            &
     &               ubar(i+1,j,krhs)
          Dgrad(i,j)=DUon(i-1,j)-2.0_r8*DUon(i,j)+DUon(i+1,j)
        END DO
      END DO
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=Jstr,Jend
            grad (Istr,j)=grad (Istr+1,j)
            Dgrad(Istr,j)=Dgrad(Istr+1,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=Jstr,Jend
            grad (Iend+1,j)=grad (Iend,j)
            Dgrad(Iend+1,j)=Dgrad(Iend,j)
          END DO
        END IF
      END IF

      cff=1.0_r8/6.0_r8
      DO j=Jstr,Jend
        DO i=IstrU-1,Iend
          UFx(i,j)=0.25_r8*(ubar(i  ,j,krhs)+                           &
     &                      ubar(i+1,j,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i+1,j)))*             &
     &                     (DUon(i,j)+DUon(i+1,j)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i+1,j)))
        END DO
      END DO
!
      DO j=Jstrm1,Jendp1
        DO i=IstrU,Iend
          grad(i,j)=ubar(i,j-1,krhs)-2.0_r8*ubar(i,j,krhs)+             &
     &              ubar(i,j+1,krhs)
        END DO
      END DO
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=IstrU,Iend
            grad(i,Jstr-1)=grad(i,Jstr)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=IstrU,Iend
            grad(i,Jend+1)=grad(i,Jend)
          END DO
        END IF
      END IF
      DO j=Jstr,Jend+1
        DO i=IstrU-1,Iend
          Dgrad(i,j)=DVom(i-1,j)-2.0_r8*DVom(i,j)+DVom(i+1,j)
        END DO
      END DO

      cff=1.0_r8/6.0_r8
      DO j=Jstr,Jend+1
        DO i=IstrU,Iend
          UFe(i,j)=0.25_r8*(ubar(i,j  ,krhs)+                           &
     &                      ubar(i,j-1,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i,j-1)))*             &
     &                     (DVom(i,j)+DVom(i-1,j)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i-1,j)))
        END DO
      END DO
!
      DO j=JstrV,Jend
        DO i=Istrm1,Iendp1
          grad(i,j)=vbar(i-1,j,krhs)-2.0_r8*vbar(i,j,krhs)+             &
     &              vbar(i+1,j,krhs)
        END DO
      END DO
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=JstrV,Jend
            grad(Istr-1,j)=grad(Istr,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=JstrV,Jend
            grad(Iend+1,j)=grad(Iend,j)
          END DO
        END IF
      END IF
      DO j=JstrV-1,Jend
        DO i=Istr,Iend+1
          Dgrad(i,j)=DUon(i,j-1)-2.0_r8*DUon(i,j)+DUon(i,j+1)
        END DO
      END DO

      cff=1.0_r8/6.0_r8
      DO j=JstrV,Jend
        DO i=Istr,Iend+1
          VFx(i,j)=0.25_r8*(vbar(i  ,j,krhs)+                           &
     &                      vbar(i-1,j,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i-1,j)))*             &
     &                     (DUon(i,j)+DUon(i,j-1)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i,j-1)))
        END DO
      END DO
!
      DO j=JstrVm1,Jendp1
        DO i=Istr,Iend
          grad(i,j)=vbar(i,j-1,krhs)-2.0_r8*vbar(i,j,krhs)+             &
     &              vbar(i,j+1,krhs)
          Dgrad(i,j)=DVom(i,j-1)-2.0_r8*DVom(i,j)+DVom(i,j+1)
        END DO
      END DO
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=Istr,Iend
            grad (i,Jstr)=grad (i,Jstr+1)
            Dgrad(i,Jstr)=Dgrad(i,Jstr+1)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=Istr,Iend
            grad (i,Jend+1)=grad (i,Jend)
            Dgrad(i,Jend+1)=Dgrad(i,Jend)
          END DO
        END IF
      END IF

      cff=1.0_r8/6.0_r8
      DO j=JstrV-1,Jend
        DO i=Istr,Iend
          VFe(i,j)=0.25_r8*(vbar(i,j  ,krhs)+                           &
     &                      vbar(i,j+1,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i,j+1)))*             &
     &                     (DVom(i,j)+DVom(i,j+1)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i,j+1)))
        END DO
      END DO
# endif
!
!  Add advection to RHS terms.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=UFx(i,j)-UFx(i-1,j)
          cff2=UFe(i,j+1)-UFe(i,j)
          fac=cff1+cff2
          rhs_ubar(i,j)=rhs_ubar(i,j)-fac
# if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2xadv)=-cff1
          DiaU2rhs(i,j,M2yadv)=-cff2
          DiaU2rhs(i,j,M2hadv)=-fac
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff1=VFx(i+1,j)-VFx(i,j)
          cff2=VFe(i,j)-VFe(i,j-1)
          fac=cff1+cff2
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac
# if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2xadv)=-cff1
          DiaV2rhs(i,j,M2yadv)=-cff2
          DiaV2rhs(i,j,M2hadv)=-fac
# endif
        END DO
      END DO
#endif

#ifdef UV_COR
!
!-----------------------------------------------------------------------
!  Add in Coriolis term.
!-----------------------------------------------------------------------
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=0.5_r8*Drhs(i,j)*fomn(i,j)
          UFx(i,j)=cff*(vbar(i,j  ,krhs)+                               &
     &                  vbar(i,j+1,krhs))
          VFe(i,j)=cff*(ubar(i  ,j,krhs)+                               &
     &                  ubar(i+1,j,krhs))
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac1
# if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2fcor)=fac1
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac1
# if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2fcor)=-fac1
# endif
        END DO
      END DO
!
# ifdef WEC
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=0.5_r8*Drhs(i,j)*fomn(i,j)
          UFx(i,j)=cff*(vbar_stokes(i,j  )+                             &
     &                  vbar_stokes(i,j+1))
          VFe(i,j)=cff*(ubar_stokes(i  ,j)+                             &
     &                  ubar_stokes(i+1,j))
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac1
#  if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2fsco)=fac1
#  endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac1
#  if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2fsco)=-fac1
#  endif
        END DO
      END DO
# endif
#endif

#if defined CURVGRID && defined UV_ADV
!
!-----------------------------------------------------------------------
!  Add in curvilinear transformation terms.
!-----------------------------------------------------------------------
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff1=0.5_r8*(vbar(i,j  ,krhs)+                                &
# ifdef WEC
     &                 vbar_stokes(i,j  )+                              &
     &                 vbar_stokes(i,j+1)+                              &
# endif
     &                 vbar(i,j+1,krhs))
          cff2=0.5_r8*(ubar(i  ,j,krhs)+                                &
# ifdef WEC
     &                 ubar_stokes(i  ,j)+                              &
     &                 ubar_stokes(i+1,j)+                              &
# endif
     &                 ubar(i+1,j,krhs))
          cff3=cff1*dndx(i,j)
          cff4=cff2*dmde(i,j)
# ifdef WEC_VF
          cff5=0.5_r8*(vbar_stokes(i,j  )+                             &
     &                 vbar_stokes(i,j+1))
          cff6=0.5_r8*(ubar_stokes(i  ,j)+                             &
     &                 ubar_stokes(i+1,j))
          cff7=cff5*dndx(i,j)
          cff8=cff6*dmde(i,j)
# endif
          cff=Drhs(i,j)*(cff3-cff4)
          UFx(i,j)=cff*cff1
          VFe(i,j)=cff*cff2
# ifdef WEC_VF
          UFx(i,j)=UFx(i,j)-(cff5*Drhs(i,j)*(cff7-cff8))
          VFe(i,j)=VFe(i,j)-(cff6*Drhs(i,j)*(cff7-cff8))
# endif
# if defined DIAGNOSTICS_UV
          cff=Drhs(i,j)*cff4
          Uwrk(i,j)=-cff*cff1                  ! ubar equation, ETA-term
          Vwrk(i,j)=-cff*cff2                  ! vbar equation, ETA-term
#  ifdef WEC_VF
            Uwrk(i,j)=Uwrk(i,j)+Drhs(i,j)*cff5*cff8
            Vwrk(i,j)=Vwrk(i,j)-Drhs(i,j)*cff6*cff8
#  endif
# endif
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac1
# if defined DIAGNOSTICS_UV
          fac2=0.5_r8*(Uwrk(i,j)+Uwrk(i-1,j))
          DiaU2rhs(i,j,M2xadv)=DiaU2rhs(i,j,M2xadv)+fac1-fac2
          DiaU2rhs(i,j,M2yadv)=DiaU2rhs(i,j,M2yadv)+fac2
          DiaU2rhs(i,j,M2hadv)=DiaU2rhs(i,j,M2hadv)+fac1
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac1
# if defined DIAGNOSTICS_UV
          fac2=0.5_r8*(Vwrk(i,j)+Vwrk(i,j-1))
          DiaV2rhs(i,j,M2xadv)=DiaV2rhs(i,j,M2xadv)-fac1+fac2
          DiaV2rhs(i,j,M2yadv)=DiaV2rhs(i,j,M2yadv)-fac2
          DiaV2rhs(i,j,M2hadv)=DiaV2rhs(i,j,M2hadv)-fac1
# endif
        END DO
      END DO
#endif
#if defined UV_VIS2 || defined UV_VIS4
!
!-----------------------------------------------------------------------
!  If horizontal mixing, compute total depth at PSI-points.
!-----------------------------------------------------------------------
!
# ifdef UV_VIS4
      DO j=Jstrm1,Jendp2
        DO i=Istrm1,Iendp2
# else
      DO j=Jstr,Jend+1
        DO i=Istr,Iend+1
# endif
          Drhs_p(i,j)=0.25_r8*(Drhs(i,j  )+Drhs(i-1,j  )+               &
     &                         Drhs(i,j-1)+Drhs(i-1,j-1))
        END DO
      END DO
#endif
#ifdef UV_VIS2
!
!-----------------------------------------------------------------------
!  Add in horizontal harmonic viscosity.
!-----------------------------------------------------------------------
!
!  Compute flux-components of the horizontal divergence of the stress
!  tensor (m5/s2) in XI- and ETA-directions.
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=visc2_r(i,j)*Drhs(i,j)*0.5_r8*                            &
     &        (pmon_r(i,j)*                                             &
     &         ((pn(i  ,j)+pn(i+1,j))*ubar(i+1,j,krhs)-                 &
     &          (pn(i-1,j)+pn(i  ,j))*ubar(i  ,j,krhs))-                &
     &         pnom_r(i,j)*                                             &
     &         ((pm(i,j  )+pm(i,j+1))*vbar(i,j+1,krhs)-                 &
     &          (pm(i,j-1)+pm(i,j  ))*vbar(i,j  ,krhs)))
          UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
          VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
        END DO
      END DO
      DO j=Jstr,Jend+1
        DO i=Istr,Iend+1
          cff=visc2_p(i,j)*Drhs_p(i,j)*0.5_r8*                          &
     &        (pmon_p(i,j)*                                             &
     &         ((pn(i  ,j-1)+pn(i  ,j))*vbar(i  ,j,krhs)-               &
     &          (pn(i-1,j-1)+pn(i-1,j))*vbar(i-1,j,krhs))+              &
     &         pnom_p(i,j)*                                             &
     &         ((pm(i-1,j  )+pm(i,j  ))*ubar(i,j  ,krhs)-               &
     &          (pm(i-1,j-1)+pm(i,j-1))*ubar(i,j-1,krhs)))
#  ifdef MASKING
          cff=cff*pmask(i,j)
#  endif
#  ifdef WET_DRY
          cff=cff*pmask_wet(i,j)
#  endif
          UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
          VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
        END DO
      END DO
!
!  Add in harmonic viscosity.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=0.5_r8*(pn(i-1,j)+pn(i,j))*(UFx(i,j  )-UFx(i-1,j))
          cff2=0.5_r8*(pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-UFe(i  ,j))
          fac=cff1+cff2
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac
# if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2hvis)=fac
          DiaU2rhs(i,j,M2xvis)=cff1
          DiaU2rhs(i,j,M2yvis)=cff2
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff1=0.5_r8*(pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-VFx(i,j  ))
          cff2=0.5_r8*(pm(i,j-1)+pm(i,j))*(VFe(i  ,j)-VFe(i,j-1))
          fac=cff1-cff2
          rhs_vbar(i,j)=rhs_vbar(i,j)+fac
# if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2hvis)=fac
          DiaV2rhs(i,j,M2xvis)= cff1
          DiaV2rhs(i,j,M2yvis)=-cff2
# endif
        END DO
      END DO
#endif
#ifdef UV_VIS4
!
!-----------------------------------------------------------------------
!  Add in horizontal biharmonic viscosity. The biharmonic operator
!  is computed by applying the harmonic operator twice.
!-----------------------------------------------------------------------
!
!  Compute flux-components of the horizontal divergence of the stress
!  tensor (m4 s^-3/2) in XI- and ETA-directions.  It is assumed here
!  that "visc4_r" and "visc4_p" are the squared root of the biharmonic
!  viscosity coefficient.  For momentum balance purposes, the total
!  thickness "D" appears only when computing the second harmonic
!  operator.
!
      DO j=JstrVm2,Jendp1
        DO i=IstrUm2,Iendp1
          cff=visc4_r(i,j)*0.5_r8*                                      &
     &        (pmon_r(i,j)*                                             &
     &         ((pn(i  ,j)+pn(i+1,j))*ubar(i+1,j,krhs)-                 &
     &          (pn(i-1,j)+pn(i  ,j))*ubar(i  ,j,krhs))-                &
     &         pnom_r(i,j)*                                             &
     &         ((pm(i,j  )+pm(i,j+1))*vbar(i,j+1,krhs)-                 &
     &          (pm(i,j-1)+pm(i,j  ))*vbar(i,j  ,krhs)))
          UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
          VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
        END DO
      END DO
      DO j=Jstrm1,Jendp2
        DO i=Istrm1,Iendp2
          cff=visc4_p(i,j)*0.5_r8*                                      &
     &        (pmon_p(i,j)*                                             &
     &         ((pn(i  ,j-1)+pn(i  ,j))*vbar(i  ,j,krhs)-               &
     &          (pn(i-1,j-1)+pn(i-1,j))*vbar(i-1,j,krhs))+              &
     &         pnom_p(i,j)*                                             &
     &         ((pm(i-1,j  )+pm(i,j  ))*ubar(i,j  ,krhs)-               &
     &          (pm(i-1,j-1)+pm(i,j-1))*ubar(i,j-1,krhs)))
# ifdef MASKING
          cff=cff*pmask(i,j)
# endif
# ifdef WET_DRY
          cff=cff*pmask_wet(i,j)
# endif
          UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
          VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
        END DO
      END DO
!
!  Compute first harmonic operator (m s^-3/2).
!
      DO j=Jstrm1,Jendp1
        DO i=IstrUm1,Iendp1
          LapU(i,j)=0.125_r8*                                           &
     &              (pm(i-1,j)+pm(i,j))*(pn(i-1,j)+pn(i,j))*            &
     &              ((pn(i-1,j)+pn(i,j))*                               &
     &               (UFx(i,j  )-UFx(i-1,j))+                           &
     &               (pm(i-1,j)+pm(i,j))*                               &
     &               (UFe(i,j+1)-UFe(i  ,j)))
        END DO
      END DO
      DO j=JstrVm1,Jendp1
        DO i=Istrm1,Iendp1
          LapV(i,j)=0.125_r8*                                           &
     &              (pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))*            &
     &              ((pn(i,j-1)+pn(i,j))*                               &
     &               (VFx(i+1,j)-VFx(i,j  ))-                           &
     &               (pm(i,j-1)+pm(i,j))*                               &
     &               (VFe(i  ,j)-VFe(i,j-1)))
        END DO
      END DO
!
!  Apply boundary conditions (other than periodic) to the first
!  harmonic operator. These are gradient or closed (free slip or
!  no slip) boundary conditions.
!
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          IF (LBC(iwest,isUbar,ng)%closed) THEN
            DO j=Jstrm1,Jendp1
              LapU(IstrU-1,j)=0.0_r8
            END DO
          ELSE
            DO j=Jstrm1,Jendp1
              LapU(IstrU-1,j)=LapU(IstrU,j)
            END DO
          END IF
          IF (LBC(iwest,isVbar,ng)%closed) THEN
            DO j=JstrVm1,Jendp1
              LapV(Istr-1,j)=gamma2(ng)*LapV(Istr,j)
            END DO
          ELSE
            DO j=JstrVm1,Jendp1
              LapV(Istr-1,j)=0.0_r8
            END DO
          END IF
        END IF
      END IF
!
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          IF (LBC(ieast,isUbar,ng)%closed) THEN
            DO j=Jstrm1,Jendp1
              LapU(Iend+1,j)=0.0_r8
            END DO
          ELSE
            DO j=Jstrm1,Jendp1
              LapU(Iend+1,j)=LapU(Iend,j)
            END DO
          END IF
          IF (LBC(ieast,isVbar,ng)%closed) THEN
            DO j=JstrVm1,Jendp1
              LapV(Iend+1,j)=gamma2(ng)*LapV(Iend,j)
            END DO
          ELSE
            DO j=JstrVm1,Jendp1
              LapV(Iend+1,j)=0.0_r8
            END DO
          END IF
        END IF
      END IF
!
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          IF (LBC(isouth,isUbar,ng)%closed) THEN
            DO i=IstrUm1,Iendp1
              LapU(i,Jstr-1)=gamma2(ng)*LapU(i,Jstr)
            END DO
          ELSE
            DO i=IstrUm1,Iendp1
              LapU(i,Jstr-1)=0.0_r8
            END DO
          END IF
          IF (LBC(isouth,isVbar,ng)%closed) THEN
            DO i=Istrm1,Iendp1
              LapV(i,JstrV-1)=0.0_r8
            END DO
          ELSE
            DO i=Istrm1,Iendp1
              LapV(i,JstrV-1)=LapV(i,JstrV)
            END DO
          END IF
        END IF
      END IF
!
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          IF (LBC(inorth,isUbar,ng)%closed) THEN
            DO i=IstrUm1,Iendp1
              LapU(i,Jend+1)=gamma2(ng)*LapU(i,Jend)
            END DO
          ELSE
            DO i=IstrUm1,Iendp1
              LapU(i,Jend+1)=0.0_r8
            END DO
          END IF
          IF (LBC(inorth,isVbar,ng)%closed) THEN
            DO i=Istrm1,Iendp1
              LapV(i,Jend+1)=0.0_r8
            END DO
          ELSE
            DO i=Istrm1,Iendp1
              LapV(i,Jend+1)=LapV(i,Jend)
            END DO
          END IF
        END IF
      END IF
!
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng).or.          &
     &          CompositeGrid(iwest ,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
          LapU(Istr  ,Jstr-1)=0.5_r8*(LapU(Istr+1,Jstr-1)+              &
     &                                LapU(Istr  ,Jstr  ))
          LapV(Istr-1,Jstr  )=0.5_r8*(LapV(Istr-1,Jstr+1)+              &
     &                                LapV(Istr  ,Jstr  ))
        END IF
      END IF

      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng).or.          &
     &          CompositeGrid(ieast ,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
          LapU(Iend+1,Jstr-1)=0.5_r8*(LapU(Iend  ,Jstr-1)+              &
     &                                LapU(Iend+1,Jstr  ))
          LapV(Iend+1,Jstr  )=0.5_r8*(LapV(Iend  ,Jstr  )+              &
     &                                LapV(Iend+1,Jstr+1))
        END IF
      END IF

      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng).or.          &
     &          CompositeGrid(iwest ,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
          LapU(Istr  ,Jend+1)=0.5_r8*(LapU(Istr+1,Jend+1)+              &
     &                                LapU(Istr  ,Jend  ))
          LapV(Istr-1,Jend+1)=0.5_r8*(LapV(Istr  ,Jend+1)+              &
     &                                LapV(Istr-1,Jend  ))
        END IF
      END IF

      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng).or.          &
     &          CompositeGrid(ieast ,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          LapU(Iend+1,Jend+1)=0.5_r8*(LapU(Iend  ,Jend+1)+              &
     &                                LapU(Iend+1,Jend  ))
          LapV(Iend+1,Jend+1)=0.5_r8*(LapV(Iend  ,Jend+1)+              &
     &                                LapV(Iend+1,Jend  ))
        END IF
      END IF
!
!  Compute flux-components of the horizontal divergence of the
!  biharmonic stress tensor (m4/s2) in XI- and ETA-directions.
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=visc4_r(i,j)*Drhs(i,j)*0.5_r8*                            &
     &        (pmon_r(i,j)*                                             &
     &         ((pn(i  ,j)+pn(i+1,j))*LapU(i+1,j)-                      &
     &          (pn(i-1,j)+pn(i  ,j))*LapU(i  ,j))-                     &
     &         pnom_r(i,j)*                                             &
     &         ((pm(i,j  )+pm(i,j+1))*LapV(i,j+1)-                      &
     &          (pm(i,j-1)+pm(i,j  ))*LapV(i,j  )))
          UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
          VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
        END DO
      END DO
      DO j=Jstr,Jend+1
        DO i=Istr,Iend+1
          cff=visc4_p(i,j)*Drhs_p(i,j)*0.5_r8*                          &
     &        (pmon_p(i,j)*                                             &
     &         ((pn(i  ,j-1)+pn(i  ,j))*LapV(i  ,j)-                    &
     &          (pn(i-1,j-1)+pn(i-1,j))*LapV(i-1,j))+                   &
     &         pnom_p(i,j)*                                             &
     &         ((pm(i-1,j  )+pm(i,j  ))*LapU(i,j  )-                    &
     &          (pm(i-1,j-1)+pm(i,j-1))*LapU(i,j-1)))
# ifdef MASKING
          cff=cff*pmask(i,j)
# endif
# ifdef WET_DRY
          cff=cff*pmask_wet(i,j)
# endif
          UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
          VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
        END DO
      END DO
!
!  Add in biharmonic viscosity.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=0.5_r8*(pn(i-1,j)+pn(i,j))*(UFx(i,j  )-UFx(i-1,j))
          cff2=0.5_r8*(pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-UFe(i  ,j))
          fac=cff1+cff2
          rhs_ubar(i,j)=rhs_ubar(i,j)-fac
# if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2hvis)=-fac
          DiaU2rhs(i,j,M2xvis)=-cff1
          DiaU2rhs(i,j,M2yvis)=-cff2
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff1=0.5_r8*(pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-VFx(i,j  ))
          cff2=0.5_r8*(pm(i,j-1)+pm(i,j))*(VFe(i  ,j)-VFe(i,j-1))
          fac=cff1-cff2
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac
# if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2hvis)=-fac
          DiaV2rhs(i,j,M2xvis)=-cff1
          DiaV2rhs(i,j,M2yvis)= cff2
# endif
        END DO
      END DO
#endif
#if defined WEC_VF && defined SOLVE3D
!
!-----------------------------------------------------------------------
!  Add in non-conservative roller terms.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=rubrk2d(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)+cff1
# ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2wbrk)=cff1
# endif
# ifdef WEC_ROLLER
          cff1=rurol2d(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2wrol)=cff1
#  endif
# else
#  ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2wrol)=0.0_r8
#  endif
# endif
# ifdef BOTTOM_STREAMING
          cff1=rubst2d(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2bstm)=cff1
#  endif
# endif
# ifdef SURFACE_STREAMING
          cff1=russt2d(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2sstm)=cff1
#  endif
# endif
        END DO
        IF (j.ge.JstrV) THEN
          DO i=Istr,Iend
            cff1=rvbrk2d(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)+cff1
# ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2wbrk)=cff1
# endif
# ifdef WEC_ROLLER
            cff1=rvrol2d(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2wrol)=cff1
#  endif
# else
#  ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2wrol)=0.0_r8
#  endif
# endif
# ifdef BOTTOM_STREAMING
            cff1=rvbst2d(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2bstm)=cff1
#  endif
# endif
# ifdef SURFACE_STREAMING
            cff1=rvsst2d(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2sstm)=cff1
#  endif
# endif
          END DO
        END IF
      END DO
# ifdef UV_ADV
#  ifdef DIAGNOSTICS_UV
!
!---------------------------------------------------------------------------
!  To obtain the full horizotal 'J' vortex force term:
!  Compute term for diagnostics only.  Subtract from hadv and add to vorf.
!---------------------------------------------------------------------------
!
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=0.5_r8*(Drhs(i-1,j)+Drhs(i,j))
            DVSom(i,j)=0.25_r8*cff*om_u(i,j)*                           &
     &                  (vbar_stokes(i  ,j  )+                          &
     &                   vbar_stokes(i  ,j+1)+                          &
     &                   vbar_stokes(i-1,j  )+                          &
     &                   vbar_stokes(i-1,j+1))
          END DO
        END DO
        DO j=Jstr,Jend+1
          DO i=IstrU,Iend
            UFx(i,j)=0.5_r8*(ubar(i  ,j-1,krhs)+                        &
                             ubar(i  ,j  ,krhs))
          END DO
        END DO
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff1=UFx(i,j+1)-UFx(i,j)
            cff=cff1*DVSom(i,j)
            DiaU2rhs(i,j,M2xadv)=DiaU2rhs(i,j,M2xadv)+cff
            DiaU2rhs(i,j,M2hadv)=DiaU2rhs(i,j,M2hadv)+cff
            DiaU2rhs(i,j,M2hjvf)=-cff
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=0.5_r8*(Drhs(i,j)+Drhs(i,j-1))
            DUSon(i,j)=cff*0.25_r8*on_v(i,j)*                           &
     &                  (ubar_stokes(i  ,j  )+                          &
     &                   ubar_stokes(i+1,j  )+                          &
     &                   ubar_stokes(i  ,j-1)+                          &
     &                   ubar_stokes(i+1,j-1))
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend+1
            VFe(i,j)=0.5_r8*(vbar(i-1,j  ,krhs)+                        &
     &                       vbar(i  ,j  ,krhs))
          END DO
        END DO
        DO i=Istr,Iend
          DO j=JstrV,Jend
            cff2=VFe(i+1,j)-VFe(i,j)
            cff=cff2*DUSon(i,j)
            DiaV2rhs(i,j,M2yadv)=DiaV2rhs(i,j,M2yadv)+cff
            DiaV2rhs(i,j,M2hadv)=DiaV2rhs(i,j,M2hadv)+cff
            DiaV2rhs(i,j,M2hjvf)=-cff
          END DO
        END DO
#  endif
!
!---------------------------------------------------------------------------
! Contribution of a term corresponding to product of
! Stokes and Eulerian Velocity Eqn. 26 and 27.
! This removes terms that were unneccessarily added in flux form.
!---------------------------------------------------------------------------
!
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=0.5_r8*(Drhs(i-1,j)+Drhs(i,j))
            DUSon(i,j)=cff*on_u(i,j)*ubar_stokes(i,j)
            DVSon(i,j)=0.25_r8*cff*on_u(i,j)*                           &
     &                  (vbar_stokes(i  ,j  )+                          &
     &                   vbar_stokes(i  ,j+1)+                          &
     &                   vbar_stokes(i-1,j  )+                          &
     &                   vbar_stokes(i-1,j+1))
          END DO
          DO i=IstrU-1,Iend
            UFx(i,j)=0.5_r8*(ubar(i  ,j  ,krhs)+                        &
                             ubar(i+1,j  ,krhs))
            VFx(i,j)=0.5_r8*(vbar(i  ,j  ,krhs)+                        &
     &                       vbar(i  ,j+1,krhs))
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=0.5_r8*(Drhs(i,j)+Drhs(i,j-1))
            DUSom(i,j)=cff*0.25_r8*om_v(i,j)*                           &
     &                  (ubar_stokes(i  ,j  )+                          &
     &                   ubar_stokes(i+1,j  )+                          &
     &                   ubar_stokes(i  ,j-1)+                          &
     &                   ubar_stokes(i+1,j-1))
            DVSom(i,j)=cff*om_v(i,j)*vbar_stokes(i,j)
          END DO
        END DO
        DO j=JstrV-1,Jend
          DO i=Istr,Iend
            cff=0.5_r8*(Drhs(i,j)+Drhs(i,j-1))
            UFe(i,j)=0.5_r8*(ubar(i+1,j  ,krhs)+                        &
     &                       ubar(i  ,j  ,krhs))
            VFe(i,j)=0.5_r8*(vbar(i  ,j  ,krhs)+                        &
     &                       vbar(i  ,j+1,krhs))
          END DO
        END DO
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff1=UFx(i,j)-UFx(i-1,j)
            cff2=VFx(i,j)-VFx(i-1,j)
            cff3=DUSon(i,j)*cff1
            cff4=DVSon(i,j)*cff2
            rhs_ubar(i,j)=rhs_ubar(i,j)+cff3+cff4
!           rustr2d(i,j)=rustr2d(i,j)-cff3-cff4
#  ifdef DIAGNOSTICS_UV
            DiaU2rhs(i,j,M2xadv)=DiaU2rhs(i,j,M2xadv)+cff3
            DiaU2rhs(i,j,M2hadv)=DiaU2rhs(i,j,M2hadv)+cff3
            DiaU2rhs(i,j,M2hjvf)=DiaU2rhs(i,j,M2hjvf)+cff4
#  endif
          END DO
        END DO
        DO i=Istr,Iend
          DO j=JstrV,Jend
            cff1=UFe(i,j)-UFe(i,j-1)
            cff2=VFe(i,j)-VFe(i,j-1)
            cff3=DUSom(i,j)*cff1
            cff4=DVSom(i,j)*cff2
            rhs_vbar(i,j)=rhs_vbar(i,j)+cff3+cff4
!           rvstr2d(i,j)=rvstr2d(i,j,k)-cff3-cff4
#  ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2yadv)=DiaV2rhs(i,j,M2yadv)+cff4
            DiaV2rhs(i,j,M2hadv)=DiaV2rhs(i,j,M2hadv)+cff4
            DiaV2rhs(i,j,M2hjvf)=DiaV2rhs(i,j,M2hjvf)+cff3
#  endif
          END DO
        END DO
# endif
#endif
#ifndef SOLVE3D
!
!-----------------------------------------------------------------------
!  Add in bottom stress.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac=bustr(i,j)*om_u(i,j)*on_u(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)-fac
# ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2bstr)=-fac
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac=bvstr(i,j)*om_v(i,j)*on_v(i,j)
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac
# ifdef DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2bstr)=-fac
# endif
        END DO
      END DO
#else
# ifdef DIAGNOSTICS_UV
!
!  Initialize the stress term if no bottom friction is defined.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          DiaU2rhs(i,j,M2bstr)=0.0_r8
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          DiaV2rhs(i,j,M2bstr)=0.0_r8
        END DO
      END DO
# endif
#endif
!
!-----------------------------------------------------------------------
!  Add in nudging of 2D momentum climatology.
!-----------------------------------------------------------------------
!
      IF (LnudgeM2CLM(ng)) THEN
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=0.25_r8*(CLIMA(ng)%M2nudgcof(i-1,j)+                    &
     &                   CLIMA(ng)%M2nudgcof(i  ,j))*                   &
     &          om_u(i,j)*on_u(i,j)
            rhs_ubar(i,j)=rhs_ubar(i,j)+                                &
     &                    cff*(Drhs(i-1,j)+Drhs(i,j))*                  &
     &                        (CLIMA(ng)%ubarclm(i,j)-                  &
     &                         ubar(i,j,krhs))
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=0.25_r8*(CLIMA(ng)%M2nudgcof(i,j-1)+                    &
     &                   CLIMA(ng)%M2nudgcof(i,j  ))*                   &
     &          om_v(i,j)*on_v(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)+                                &
     &                    cff*(Drhs(i,j-1)+Drhs(i,j))*                  &
     &                        (CLIMA(ng)%vbarclm(i,j)-                  &
     &                         vbar(i,j,krhs))
          END DO
        END DO
      END IF

#ifdef SOLVE3D
# ifdef WET_DRY
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,rhs_ubar(i,j))*umask_wet(i,j)
          cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          rhs_ubar(i,j)=rhs_ubar(i,j)*cff7
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,rhs_vbar(i,j))*vmask_wet(i,j)
          cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          rhs_vbar(i,j)=rhs_vbar(i,j)*cff7
        END DO
      END DO
# endif
!
!-----------------------------------------------------------------------
!  Coupling between 2D and 3D equations.
!-----------------------------------------------------------------------
!
!  Before the predictor step of the first barotropic time-step,
!  arrays "rufrc" and "rvfrc" contain the vertical integrals of
!  the 3D right-hand-side terms for momentum equations (including
!  surface and bottom stresses, if so prescribed).
!
!  Convert them into forcing terms by subtracting the fast time
!  "rhs_ubar" and "rhs_vbar" from them; Also, immediately apply
!  these forcing terms "rhs_ubar" and "rhs_vbar".
!
!  From now on, these newly computed forcing terms will remain
!  constant during the fast time stepping and will added to
!  "rhs_ubar" and "rhs_vbar" during all subsequent time steps.
!
      IF (FIRST_2D_STEP.and.PREDICTOR_2D_STEP(ng)) THEN
        IF (iic(ng).eq.ntfirst(ng)) THEN
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+rufrc(i,j)
              ru(i,j,0,nstp)=rufrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              DiaRUfrc(i,j,3,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                &
     &                             DiaRUfrc(i,j,3,M2pgrd)
#  endif
# endif
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+rvfrc(i,j)
              rv(i,j,0,nstp)=rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              DiaRVfrc(i,j,3,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                &
     &                             DiaRVfrc(i,j,3,M2pgrd)
#  endif
# endif
            END DO
          END DO
        ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+                              &
     &                      1.5_r8*rufrc(i,j)-0.5_r8*ru(i,j,0,nnew)
              ru(i,j,0,nstp)=rufrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              1.5_r8*DiaRUfrc(i,j,3,idiag)-       &
     &                              0.5_r8*DiaRUfrc(i,j,nnew,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=1.5_r8*DiaRUfrc(i,j,3,M2sstr)-       &
     &                             0.5_r8*DiaRUfrc(i,j,nnew,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=1.5_r8*DiaRUfrc(i,j,3,M2bstr)-       &
     &                             0.5_r8*DiaRUfrc(i,j,nnew,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#   ifdef WEC_VF
              DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                &
     &                             1.5_r8*DiaRUfrc(i,j,3,M2pgrd)-       &
     &                             0.5_r8*DiaRUfrc(i,j,nnew,M2pgrd)
#   endif
#  endif
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+                              &
     &                      1.5_r8*rvfrc(i,j)-0.5_r8*rv(i,j,0,nnew)
              rv(i,j,0,nstp)=rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              1.5_r8*DiaRVfrc(i,j,3,idiag)-       &
     &                              0.5_r8*DiaRVfrc(i,j,nnew,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=1.5_r8*DiaRVfrc(i,j,3,M2sstr)-       &
     &                             0.5_r8*DiaRVfrc(i,j,nnew,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=1.5_r8*DiaRVfrc(i,j,3,M2bstr)-       &
     &                             0.5_r8*DiaRVfrc(i,j,nnew,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                &
     &                             1.5_r8*DiaRVfrc(i,j,3,M2pgrd)-       &
     &                             0.5_r8*DiaRVfrc(i,j,nnew,M2pgrd)
#  endif
# endif
            END DO
          END DO
        ELSE
          cff1=23.0_r8/12.0_r8
          cff2=16.0_r8/12.0_r8
          cff3= 5.0_r8/12.0_r8
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+                              &
     &                      cff1*rufrc(i,j)-                            &
     &                      cff2*ru(i,j,0,nnew)+                        &
     &                      cff3*ru(i,j,0,nstp)
              ru(i,j,0,nstp)=rufrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              cff1*DiaRUfrc(i,j,3,idiag)-         &
     &                              cff2*DiaRUfrc(i,j,nnew,idiag)+      &
     &                              cff3*DiaRUfrc(i,j,nstp,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=cff1*DiaRUfrc(i,j,3,M2sstr)-         &
     &                             cff2*DiaRUfrc(i,j,nnew,M2sstr)+      &
     &                             cff3*DiaRUfrc(i,j,nstp,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=cff1*DiaRUfrc(i,j,3,M2bstr)-         &
     &                             cff2*DiaRUfrc(i,j,nnew,M2bstr)+      &
     &                             cff3*DiaRUfrc(i,j,nstp,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                &
     &                             cff1*DiaRUfrc(i,j,3,M2pgrd)-         &
     &                             cff2*DiaRUfrc(i,j,nnew,M2pgrd)+      &
     &                             cff3*DiaRUfrc(i,j,nstp,M2pgrd)
#  endif
# endif
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+                              &
     &                      cff1*rvfrc(i,j)-                            &
     &                      cff2*rv(i,j,0,nnew)+                        &
     &                      cff3*rv(i,j,0,nstp)
              rv(i,j,0,nstp)=rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              cff1*DiaRVfrc(i,j,3,idiag)-         &
     &                              cff2*DiaRVfrc(i,j,nnew,idiag)+      &
     &                              cff3*DiaRVfrc(i,j,nstp,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=cff1*DiaRVfrc(i,j,3,M2sstr)-         &
     &                             cff2*DiaRVfrc(i,j,nnew,M2sstr)+      &
     &                             cff3*DiaRVfrc(i,j,nstp,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=cff1*DiaRVfrc(i,j,3,M2bstr)-         &
     &                             cff2*DiaRVfrc(i,j,nnew,M2bstr)+      &
     &                             cff3*DiaRVfrc(i,j,nstp,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                &
     &                             cff1*DiaRVfrc(i,j,3,M2pgrd)-         &
     &                             cff2*DiaRVfrc(i,j,nnew,M2pgrd)+      &
     &                             cff3*DiaRVfrc(i,j,nstp,M2pgrd)
#  endif
# endif
            END DO
          END DO
        END IF
      ELSE
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            rhs_ubar(i,j)=rhs_ubar(i,j)+rufrc(i,j)
# ifdef DIAGNOSTICS_UV
            DO idiag=1,M2pgrd
              DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                  &
     &                            DiaRUfrc(i,j,3,idiag)
            END DO
            DiaU2rhs(i,j,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
            DiaU2rhs(i,j,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
            DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                  &
     &                           DiaRUfrc(i,j,3,M2pgrd)
#  endif
# endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            rhs_vbar(i,j)=rhs_vbar(i,j)+rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
            DO idiag=1,M2pgrd
              DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                  &
     &                            DiaRVfrc(i,j,3,idiag)
            END DO
            DiaV2rhs(i,j,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
            DiaV2rhs(i,j,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
            DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                  &
     &                           DiaRVfrc(i,j,3,M2pgrd)
#  endif
# endif
          END DO
        END DO
      END IF
#else
!
!-----------------------------------------------------------------------
!  Add in surface momentum stress.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac=sustr(i,j)*om_u(i,j)*on_u(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac
# ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2sstr)=fac
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac=svstr(i,j)*om_v(i,j)*on_v(i,j)
          rhs_vbar(i,j)=rhs_vbar(i,j)+fac
# ifdef DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2sstr)=fac
# endif
        END DO
      END DO
#endif
!
!=======================================================================
!  Time step 2D momentum equations.
!=======================================================================
!
!  Compute total water column depth.
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          Dstp(i,j)=zeta(i,j,kstp)+h(i,j)
        END DO
      END DO
!
!  During the first time-step, the predictor step is Forward-Euler
!  and the corrector step is Backward-Euler. Otherwise, the predictor
!  step is Leap-frog and the corrector step is Adams-Moulton.
!
      IF (FIRST_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)
#ifdef WET_DRY
        cff2=1.0_r8/cff1
#endif
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*cff1*rhs_ubar(i,j))*fac
#ifdef MASKING
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,ubar(i,j,knew))*umask_wet(i,j)
            cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            ubar(i,j,knew)=ubar(i,j,knew)*cff7
            rhs_ubar(i,j)=rhs_ubar(i,j)*cff7
# ifdef SOLVE3D
            IF (PREDICTOR_2D_STEP(ng)) THEN
              rufrc(i,j)=rufrc(i,j)*cff7
              ru(i,j,0,nstp)=rufrc(i,j)
            END IF
# endif
#endif
#if defined NESTING && !defined SOLVE3D
            DU_flux(i,j)=ubar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
#endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*cff1*rhs_vbar(i,j))*fac
#ifdef MASKING
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,vbar(i,j,knew))*vmask_wet(i,j)
            cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            vbar(i,j,knew)=vbar(i,j,knew)*cff7
            rhs_vbar(i,j)=rhs_vbar(i,j)*cff7
# ifdef SOLVE3D
            IF (PREDICTOR_2D_STEP(ng)) THEN
              rvfrc(i,j)=rvfrc(i,j)*cff7
              rv(i,j,0,nstp)=rvfrc(i,j)
            END IF
# endif
#endif
#if defined NESTING && !defined SOLVE3D
            DV_flux(i,j)=vbar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
#endif
          END DO
        END DO
      ELSE IF (PREDICTOR_2D_STEP(ng)) THEN
        cff1=dtfast(ng)
#ifdef WET_DRY
        cff2=1.0_r8/cff1
#endif
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*cff1*rhs_ubar(i,j))*fac
#ifdef MASKING
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,ubar(i,j,knew))*umask_wet(i,j)
            cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            ubar(i,j,knew)=ubar(i,j,knew)*cff7
            rhs_ubar(i,j)=rhs_ubar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DU_flux(i,j)=ubar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
#endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*cff1*rhs_vbar(i,j))*fac
#ifdef MASKING
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,vbar(i,j,knew))*vmask_wet(i,j)
            cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            vbar(i,j,knew)=vbar(i,j,knew)*cff7
            rhs_vbar(i,j)=rhs_vbar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DV_flux(i,j)=vbar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
#endif
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)*5.0_r8/12.0_r8
        cff2=0.5_r8*dtfast(ng)*8.0_r8/12.0_r8
        cff3=0.5_r8*dtfast(ng)*1.0_r8/12.0_r8
#ifdef WET_DRY
        cff4=1.0_r8/cff1
#endif
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*(cff1*rhs_ubar(i,j)+                    &
     &                           cff2*rubar(i,j,kstp)-                  &
     &                           cff3*rubar(i,j,ptsk)))*fac
#ifdef MASKING
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,ubar(i,j,knew))*umask_wet(i,j)
            cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            ubar(i,j,knew)=ubar(i,j,knew)*cff7
            rhs_ubar(i,j)=rhs_ubar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DU_flux(i,j)=ubar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
#endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*(cff1*rhs_vbar(i,j)+                    &
     &                           cff2*rvbar(i,j,kstp)-                  &
     &                           cff3*rvbar(i,j,ptsk)))*fac
#ifdef MASKING
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,vbar(i,j,knew))*vmask_wet(i,j)
            cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            vbar(i,j,knew)=vbar(i,j,knew)*cff7
            rhs_vbar(i,j)=rhs_vbar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DV_flux(i,j)=vbar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
#endif
          END DO
        END DO
      END IF

#ifdef DIAGNOSTICS_UV
!
!-----------------------------------------------------------------------
!  Time step 2D momentum diagnostic terms.
!-----------------------------------------------------------------------

# ifdef MASKING
!
!  Apply land/sea mask.
!
      DO idiag=1,NDM2d-1
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)*umask(i,j)
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)*vmask(i,j)
          END DO
        END DO
      END DO
# endif
# ifdef SOLVE3D
!
!  The arrays "DiaU2rhs" and "DiaV2rhs" contain the contributions of
!  each of the 2D right-hand-side terms for the momentum equations.
!
!  These values are integrated, time-stepped and converted to mass flux
!  units (m3 s-1) for coupling with the 3D diagnostic terms.
!
      fac=weight(1,iif(ng),ng)
      IF (FIRST_2D_STEP.and.CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaU2int(i,j,idiag)=cff1*DiaU2rhs(i,j,idiag)
              DiaU2wrk(i,j,idiag)=DiaU2int(i,j,idiag)*                  &
     &                            (pm(i-1,j)+pm(i,j))*fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaV2int(i,j,idiag)=cff1*DiaV2rhs(i,j,idiag)
              DiaV2wrk(i,j,idiag)=DiaV2int(i,j,idiag)*                  &
     &                            (pn(i,j)+pn(i,j-1))*fac
            END DO
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)*5.0_r8/12.0_r8
        cff2=0.5_r8*dtfast(ng)*8.0_r8/12.0_r8
        cff3=0.5_r8*dtfast(ng)*1.0_r8/12.0_r8
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaU2int(i,j,idiag)=DiaU2int(i,j,idiag)+                  &
     &                            (cff1*DiaU2rhs(i,j,idiag)+            &
     &                             cff2*DiaRUbar(i,j,kstp,idiag)-       &
     &                             cff3*DiaRUbar(i,j,ptsk,idiag))
              DiaU2wrk(i,j,idiag)=DiaU2wrk(i,j,idiag)+                  &
     &                            DiaU2int(i,j,idiag)*                  &
     &                            (pm(i-1,j)+pm(i,j))*fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaV2int(i,j,idiag)=DiaV2int(i,j,idiag)+                  &
     &                            (cff1*DiaV2rhs(i,j,idiag)+            &
     &                             cff2*DiaRVbar(i,j,kstp,idiag)-       &
     &                             cff3*DiaRVbar(i,j,ptsk,idiag))
              DiaV2wrk(i,j,idiag)=DiaV2wrk(i,j,idiag)+                  &
     &                            DiaV2int(i,j,idiag)*                  &
     &                            (pn(i,j)+pn(i,j-1))*fac
            END DO
          END DO
        END DO
      END IF
# else
!
!  Time-step the diagnostic terms.
!
      IF (FIRST_2D_STEP.and.CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
              fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
              DiaU2wrk(i,j,idiag)=cff*cff1*DiaU2rhs(i,j,idiag)*fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
              fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
              DiaV2wrk(i,j,idiag)=cff*cff1*DiaV2rhs(i,j,idiag)*fac
            END DO
          END DO
        END DO
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            DiaU2wrk(i,j,M2rate)=ubar(i,j,knew)-ubar(i,j,kstp)*         &
     &                           (Dstp(i,j)+Dstp(i-1,j))*fac
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            DiaV2wrk(i,j,M2rate)=vbar(i,j,knew)-vbar(i,j,kstp)*         &
     &                           (Dstp(i,j)+Dstp(i,j-1))*fac
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)*5.0_r8/12.0_r8
        cff2=0.5_r8*dtfast(ng)*8.0_r8/12.0_r8
        cff3=0.5_r8*dtfast(ng)*1.0_r8/12.0_r8
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
              fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
              DiaU2wrk(i,j,idiag)=cff*(cff1*DiaU2rhs(i,j,idiag)+        &
     &                                 cff2*DiaRUbar(i,j,kstp,idiag)-   &
     &                                 cff3*DiaRUbar(i,j,ptsk,idiag))*  &
     &                                fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
              fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
              DiaV2wrk(i,j,idiag)=cff*(cff1*DiaV2rhs(i,j,idiag)+        &
     &                                 cff2*DiaRVbar(i,j,kstp,idiag)-   &
     &                                 cff3*DiaRVbar(i,j,ptsk,idiag))*  &
     &                                fac
            END DO
          END DO
        END DO
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            DiaU2wrk(i,j,M2rate)=ubar(i,j,knew)-                        &
     &                           ubar(i,j,kstp)*                        &
     &                           (Dstp(i,j)+Dstp(i-1,j))*fac
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            DiaV2wrk(i,j,M2rate)=vbar(i,j,knew)-                        &
     &                           vbar(i,j,kstp)*                        &
     &                           (Dstp(i,j)+Dstp(i,j-1))*fac
          END DO
        END DO
      END IF
# endif
#endif
!
!  If predictor step, load right-side-term into shared arrays for
!  future use during the subsequent corrector step.
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            rubar(i,j,krhs)=rhs_ubar(i,j)
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            rvbar(i,j,krhs)=rhs_vbar(i,j)
          END DO
        END DO
#ifdef DIAGNOSTICS_UV
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaRUbar(i,j,krhs,idiag)=DiaU2rhs(i,j,idiag)
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaRVbar(i,j,krhs,idiag)=DiaV2rhs(i,j,idiag)
            END DO
          END DO
        END DO
#endif
      END IF
!
!-----------------------------------------------------------------------
!  Apply lateral boundary conditions.
!-----------------------------------------------------------------------
!
      CALL u2dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 krhs, kstp, knew,                                &
     &                 ubar, vbar, zeta)
      CALL v2dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 krhs, kstp, knew,                                &
     &                 ubar, vbar, zeta)
!
!  Compute integral mass flux across open boundaries and adjust
!  for volume conservation.
!
      IF (ANY(VolCons(:,ng))) THEN
        CALL obc_flux_tile (ng, tile,                                   &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      IminS, ImaxS, JminS, JmaxS,                 &
     &                      knew,                                       &
#ifdef MASKING
     &                      umask, vmask,                               &
#endif
     &                      h, om_v, on_u,                              &
     &                      ubar, vbar, zeta)
      END IF

#if defined NESTING && !defined SOLVE3D
!
!-----------------------------------------------------------------------
!  Set barotropic fluxes along physical boundaries.
!-----------------------------------------------------------------------
!
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=Jstr-1,JendR
            Dnew(Istr-1,j)=h(Istr-1,j)+zeta_new(Istr-1,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=Jstr-1,JendR
            Dnew(Iend+1,j)=h(Iend+1,j)+zeta_new(Iend+1,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=Istr-1,IendR
            Dnew(i,Jstr-1)=h(i,Jstr-1)+zeta_new(i,Jstr-1)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=Istr-1,IendR
            Dnew(i,Jend+1)=h(i,Jend+1)+zeta_new(i,Jend+1)
          END DO
        END IF
      END IF
!
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=JstrR,JendR
            DU_flux(IstrU-1,j)=ubar(IstrU-1,j,knew)*on_u(IstrU-1,j)*    &
     &                         0.5_r8*(Dnew(IstrU-1,j)+Dnew(IstrU-2,j))
          END DO
          DO j=JstrV,Jend
            DV_flux(Istr-1,j)=vbar(Istr-1,j,knew)*om_v(Istr-1,j)*       &
     &                        0.5_r8*(Dnew(Istr-1,j)+Dnew(Istr-1,j-1))
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=JstrR,JendR
            DU_flux(Iend+1,j)=ubar(Iend+1,j,knew)*on_u(iend+1,j)*       &
     &                        0.5_r8*(Dnew(Iend+1,j)+Dnew(Iend,j))
          END DO
          DO j=JstrV,Jend
            DV_flux(Iend+1,j)=vbar(Iend+1,j,knew)*om_v(Iend+1,j)*       &
     &                        0.5_r8*(Dnew(iend+1,j)+Dnew(iend+1,j-1))
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=IstrU,Iend
            DU_flux(i,Jstr-1)=ubar(i,Jstr-1,knew)*on_u(i,Jstr-1)*       &
     &                        0.5_r8*(Dnew(i,Jstr-1)+Dnew(i-1,Jstr-1))
          END DO
          DO i=IstrR,IendR
            DV_flux(i,JstrV-1)=vbar(i,JstrV-1,knew)*om_v(i,JstrV-1)*    &
     &                         0.5_r8*(Dnew(i,JstrV-1)+Dnew(i,JstrV-2))
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=IstrU,Iend
            DU_flux(i,Jend+1)=ubar(i,Jend+1,knew)*on_u(i,Jend+1)*       &
     &                        0.5_r8*(Dnew(i,Jend+1)+Dnew(i-1,Jend+1))
          END DO
          DO i=IstrR,IendR
            DV_flux(i,Jend+1)=vbar(i,Jend+1,knew)*om_v(i,Jend+1)*       &
     &                        0.5_r8*(Dnew(i,Jend+1)+Dnew(i,Jend))
          END DO
        END IF
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Apply momentum transport point sources (like river runoff), if any.
!
!    Dsrc(is) = 0,  flow across grid cell u-face (positive or negative)
!    Dsrc(is) = 1,  flow across grid cell v-face (positive or negative)
!-----------------------------------------------------------------------
!
      IF (LuvSrc(ng)) THEN
        DO is=1,Nsrc(ng)
          i=SOURCES(ng)%Isrc(is)
          j=SOURCES(ng)%Jsrc(is)
          IF (((IstrR.le.i).and.(i.le.IendR)).and.                      &
     &        ((JstrR.le.j).and.(j.le.JendR))) THEN
            IF (INT(SOURCES(ng)%Dsrc(is)).eq.0) THEN
              cff=1.0_r8/(on_u(i,j)*                                    &
     &                    0.5_r8*(zeta(i-1,j,knew)+h(i-1,j)+            &
     &                            zeta(i  ,j,knew)+h(i  ,j)))
              ubar(i,j,knew)=SOURCES(ng)%Qbar(is)*cff
#if defined NESTING && !defined SOLVE3D
              DU_flux(i,j)=SOURCES(ng)%Qbar(is)
#endif
            ELSE IF (INT(SOURCES(ng)%Dsrc(is)).eq.1) THEN
              cff=1.0_r8/(om_v(i,j)*                                    &
     &                    0.5_r8*(zeta(i,j-1,knew)+h(i,j-1)+            &
     &                            zeta(i,j  ,knew)+h(i,j  )))
              vbar(i,j,knew)=SOURCES(ng)%Qbar(is)*cff
#if defined NESTING && !defined SOLVE3D
              DV_flux(i,j)=SOURCES(ng)%Qbar(is)
#endif
            END IF
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Exchange boundary information.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          ubar(:,:,knew))
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          vbar(:,:,knew))

#if defined NESTING && !defined SOLVE3D
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          DU_flux)
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          DV_flux)
#endif
      END IF

#ifdef DISTRIBUTE
!
# if defined NESTING && !defined SOLVE3D
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
# else
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
# endif
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
# if defined NESTING && !defined SOLVE3D
     &                    DU_flux, DV_flux,                             &
# endif
     &                    ubar(:,:,knew),                               &
     &                    vbar(:,:,knew))
#endif
!
      RETURN
      END SUBROUTINE step2d_tile
!
      END MODULE step2d_mod
