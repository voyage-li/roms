/*
** Experimental TeamPolicy entry points for the step2d kernels.
** Only declared when KK_STEP2D_TEAM is defined (benchmark builds).
*/

#ifndef ROMS_KOKKOS_STEP2D_TEAM_HPP
#define ROMS_KOKKOS_STEP2D_TEAM_HPP

#if defined ROMS_KOKKOS && defined KK_STEP2D_TEAM

#include "kokkos_config.hpp"

extern "C" {

void k_step2d_depth_flux_team(int ng, int tile, int krhs, int nfast,
                              int LBi, int UBi, int LBj, int UBj,
                              int IminS, int ImaxS, int JminS, int JmaxS,
                              void* zeta_p, void* h_p, void* on_u_p,
                              void* om_v_p, void* ubar_p, void* vbar_p,
                              void* Drhs_p, void* DUon_p, void* DVom_p,
                              int UBk, const roms::TileBounds* t,
                              void* scratch_p);

void k_step2d_time_average_team(int ng, int tile, int krhs,
                                int predictor, int first_step,
                                int LBi, int UBi, int LBj, int UBj,
                                int IminS, int ImaxS, int JminS, int JmaxS,
                                void* zeta_p, void* DUon_p, void* DVom_p,
                                void* Zt_avg1_p, void* DU_avg1_p,
                                void* DU_avg2_p, void* DV_avg1_p,
                                void* DV_avg2_p, double cff1, double cff2,
                                const roms::TileBounds* t);

void k_step2d_zeta_step_team(int ng, int tile, int krhs, int kstp, int knew,
                             int iif, int nfast, int ntfirst, int iic,
                             int predictor, int first_2d_step,
                             int LBi, int UBi, int LBj, int UBj, int UBk,
                             int IminS, int ImaxS, int JminS, int JmaxS,
                             void* zeta_p, void* h_p, void* pm_p, void* pn_p,
                             void* DUon_p, void* DVom_p,
                             void* gzeta_p, void* gzeta2_p,
                             void* gzetaSA_p, void* rhoA_p, void* rhoS_p,
                             void* rhs_zeta_p, void* zeta_new_p,
                             void* Dnew_p, void* zwrk_p,
                             void* rzeta_p, double dtfast, double rho0,
                             const roms::TileBounds* t);

void k_step2d_pressure_grad_team(int ng, int tile,
                                 int LBi, int UBi, int LBj, int UBj,
                                 int IminS, int ImaxS, int JminS, int JmaxS,
                                 void* h_p, void* on_u_p, void* om_v_p,
                                 void* gzeta_p, void* gzeta2_p,
                                 void* gzetaSA_p, void* rhoA_p,
                                 void* zwrk_p,
                                 void* rhs_ubar_p, void* rhs_vbar_p,
                                 void* DiaU2rhs_p, void* DiaV2rhs_p,
                                 int NDM2d, int M2pgrd, double g,
                                 const roms::TileBounds* t);

void k_step2d_hadv_centered4_team(int ng, int tile, int krhs,
                                  int LBi, int UBi, int LBj, int UBj,
                                  int IminS, int ImaxS, int JminS, int JmaxS,
                                  void* ubar_p, void* vbar_p,
                                  void* DUon_p, void* DVom_p,
                                  void* grad_p, void* Dgrad_p,
                                  void* UFx_p, void* UFe_p,
                                  void* VFx_p, void* VFe_p,
                                  void* rhs_ubar_p, void* rhs_vbar_p,
                                  void* DiaU2rhs_p, void* DiaV2rhs_p,
                                  int NDM2d,
                                  int M2xadv, int M2yadv, int M2hadv,
                                  const roms::EdgeFlags* e,
                                  const roms::TileBounds* t);

void k_step2d_coriolis_team(int ng, int tile, int krhs,
                            int LBi, int UBi, int LBj, int UBj,
                            int IminS, int ImaxS, int JminS, int JmaxS,
                            void* Drhs_p, void* fomn_p,
                            void* ubar_p, void* vbar_p,
                            void* UFx_p, void* VFe_p,
                            void* rhs_ubar_p, void* rhs_vbar_p,
                            void* DiaU2rhs_p, void* DiaV2rhs_p,
                            int NDM2d, int M2fcor,
                            const roms::TileBounds* t, void* scratch_p);

void k_step2d_psi_depth_team(int ng, int tile,
                             int LBi, int UBi, int LBj, int UBj,
                             int IminS, int ImaxS, int JminS, int JmaxS,
                             void* Drhs_p, void* Drhs_psi_p,
                             const roms::TileBounds* t);

void k_step2d_hvisc2_team(int ng, int tile, int krhs,
                          int LBi, int UBi, int LBj, int UBj,
                          int IminS, int ImaxS, int JminS, int JmaxS,
                          void* Drhs_p, void* Drhs_psi_p,
                          void* ubar_p, void* vbar_p,
                          void* pm_p, void* pn_p,
                          void* pmon_r_p, void* pnom_r_p,
                          void* pmon_p_p, void* pnom_p_p,
                          void* om_r_p, void* on_r_p,
                          void* om_p_p, void* on_p_p,
                          void* visc2_r_p, void* visc2_p2_p,
                          void* UFx_p, void* UFe_p,
                          void* VFx_p, void* VFe_p,
                          void* rhs_ubar_p, void* rhs_vbar_p,
                          void* DiaU2rhs_p, void* DiaV2rhs_p,
                          int NDM2d, int M2hvis, int M2xvis, int M2yvis,
                          const roms::TileBounds* t);

void k_step2d_coupling_team(int ng, int tile, int istp, int inew,
                            int iic, int ntfirst, int first_2d_step,
                            int predictor,
                            int LBi, int UBi, int LBj, int UBj,
                            int IminS, int ImaxS, int JminS, int JmaxS,
                            void* rufrc_p, void* rvfrc_p,
                            void* ru_p, void* rv_p,
                            void* rhs_ubar_p, void* rhs_vbar_p,
                            void* DiaU2rhs_p, void* DiaV2rhs_p,
                            void* DiaRUfrc_p, void* DiaRVfrc_p,
                            int NDM2d, int M2pgrd, int M2sstr, int M2bstr,
                            const roms::TileBounds* t);

void k_step2d_momentum_step_team(int ng, int tile, int kstp, int knew,
                                 int LBi, int UBi, int LBj, int UBj,
                                 int IminS, int ImaxS, int JminS, int JmaxS,
                                 void* zeta_p, void* h_p,
                                 void* ubar_p, void* vbar_p,
                                 void* rubar_p, void* rvbar_p,
                                 void* pm_p, void* pn_p,
                                 void* rhs_ubar_p, void* rhs_vbar_p,
                                 void* Dstp_p, void* Dnew_p,
                                 int predictor, int first_2d_step,
                                 double dtfast, const roms::TileBounds* t);

void k_step2d_store_rhs_team(int ng, int tile, int krhs,
                             int LBi, int UBi, int LBj, int UBj,
                             int IminS, int ImaxS, int JminS, int JmaxS,
                             void* rubar_p, void* rvbar_p,
                             void* rhs_ubar_p, void* rhs_vbar_p,
                             void* DiaRUbar_p, void* DiaRVbar_p,
                             void* DiaU2rhs_p, void* DiaV2rhs_p,
                             int NDM2d, int M2pgrd,
                             const roms::TileBounds* t);

void k_step2d_rhs_fused_team(int ng, int tile, int krhs,
                             int LBi, int UBi, int LBj, int UBj,
                             int IminS, int ImaxS, int JminS, int JmaxS,
                             void* h_p, void* on_u_p, void* om_v_p,
                             void* gzeta_p, void* gzeta2_p, void* gzetaSA_p,
                             void* rhoA_p, void* zwrk_p,
                             void* Drhs_p, void* fomn_p,
                             void* ubar_p, void* vbar_p,
                             void* DUon_p, void* DVom_p,
                             void* pm_p, void* pn_p,
                             void* pmon_r_p, void* pnom_r_p,
                             void* pmon_p_p, void* pnom_p_p,
                             void* om_r_p, void* on_r_p,
                             void* om_p_p, void* on_p_p,
                             void* visc2_r_p, void* visc2_p2_p,
                             void* rhs_ubar_p, void* rhs_vbar_p,
                             void* DiaU2rhs_p, void* DiaV2rhs_p,
                             int NDM2d,
                             int M2pgrd, int M2xadv, int M2yadv, int M2hadv,
                             int M2fcor, int M2hvis, int M2xvis, int M2yvis,
                             double g,
                             const roms::EdgeFlags* e,
                             const roms::TileBounds* t, void* scratch_p);

}  // extern "C"

#endif  // ROMS_KOKKOS && KK_STEP2D_TEAM

#endif  // ROMS_KOKKOS_STEP2D_TEAM_HPP
