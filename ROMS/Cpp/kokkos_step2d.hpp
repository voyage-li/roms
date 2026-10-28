/*
** git $Id$
************************************************************************
** Copyright (c) 2002-2025 The ROMS Group                               **
**   Licensed under a MIT/X style license                               **
**   See License_ROMS.md                                                **
************************************************************************
*/

#ifndef ROMS_KOKKOS_STEP2D_HPP
#define ROMS_KOKKOS_STEP2D_HPP

#if defined ROMS_KOKKOS

#include "kokkos_config.hpp"

/*
** Kernel entry points for ROMS/Nonlinear/step2d_LF_AM3.h.
**
** Every entry point is declared extern "C" so the Fortran side can bind
** to it with BIND(C, NAME=...).  Scalar arguments are passed by value
** (INTEGER(C_INT), VALUE on the Fortran side); arrays arrive as void*
** taken from C_LOC, with the Fortran bounds needed to rebuild the index
** mapping.
**
** Conventions:
**
**   - `LBi..UBj` are the grid bounds step2d_tile uses to declare its
**     arguments; `IminS..JmaxS` are the tile-private scratch bounds.
**     Kernels never assume either starts at zero.
**
**   - `t` carries the tile index metadata, `e` the domain edge flags
**     (see kokkos_config.hpp).
**
**   - Scratch buffers are allocated by the Fortran caller and passed in,
**     so the caller can still run halo exchanges on the ones that need
**     it (Drhs, DUon, DVom).
**
**   - Diagnostic indices (M2pgrd etc.) are computed at run time in
**     ROMS/Modules/mod_scalars.F, so they are passed, not assumed.
**
** Not ported -- these stay in Fortran:
**   halo exchanges, set_DUV_bc_tile, obc_flux_tile, u2dbc_tile,
**   v2dbc_tile, zetabc_tile, wetdry_tile, point sources.
*/

extern "C" {

/* Stage 1: total depth and vertically integrated mass fluxes. */

void k_step2d_depth_flux(int ng, int tile, int krhs, int nfast,
                         int LBi, int UBi, int LBj, int UBj,
                         int IminS, int ImaxS, int JminS, int JmaxS,
                         void* zeta_p, void* h_p, void* on_u_p, void* om_v_p,
                         void* ubar_p, void* vbar_p, void* Drhs_p,
                         void* DUon_p, void* DVom_p, int UBk,
                         const roms::TileBounds* t);

/* Stage 2: time averaged fields over all short time steps (SOLVE3D). */

void k_step2d_time_average(int ng, int tile, int krhs, int istp, int inew,
                           int iif, int nfast, int predictor, int first_step,
                           int LBi, int UBi, int LBj, int UBj,
                           int IminS, int ImaxS, int JminS, int JmaxS,
                           void* zeta_p, void* ubar_p, void* vbar_p,
                           void* DUon_p, void* DVom_p,
                           void* Zt_avg1_p, void* DU_avg1_p, void* DU_avg2_p,
                           void* DV_avg1_p, void* DV_avg2_p,
                           const double* weight,
                           const roms::TileBounds* t);

/* Stage 4: free surface advance (FE / LF / AM3). */

void k_step2d_zeta_step(int ng, int tile, int krhs, int kstp, int knew,
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

/* Stage 6: pressure gradient. */

void k_step2d_pressure_grad(int ng, int tile,
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

/* Stage 7: fourth-order horizontal advection of 2-D momentum. */

void k_step2d_hadv_centered4(int ng, int tile, int krhs,
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

/* Stage 8: Coriolis. */

void k_step2d_coriolis(int ng, int tile, int krhs,
                       int LBi, int UBi, int LBj, int UBj,
                       int IminS, int ImaxS, int JminS, int JmaxS,
                       void* Drhs_p, void* fomn_p,
                       void* ubar_p, void* vbar_p,
                       void* UFx_p, void* VFe_p,
                       void* rhs_ubar_p, void* rhs_vbar_p,
                       void* DiaU2rhs_p, void* DiaV2rhs_p,
                       int NDM2d, int M2fcor,
                       const roms::TileBounds* t);

/* Stage 10: total depth at PSI points. */

void k_step2d_psi_depth(int ng, int tile,
                        int LBi, int UBi, int LBj, int UBj,
                        int IminS, int ImaxS, int JminS, int JmaxS,
                        void* Drhs_p, void* Drhs_psi_p,
                        const roms::TileBounds* t);

/* Stage 11: harmonic horizontal viscosity (UV_VIS2). */

void k_step2d_hvisc2(int ng, int tile, int krhs,
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

/* Stage 14: 2-D / 3-D coupling. */

void k_step2d_coupling(int ng, int tile, int istp, int inew,
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

/* Stage 17: 2-D momentum time step. */

void k_step2d_momentum_step(int ng, int tile, int kstp, int knew,
                            int LBi, int UBi, int LBj, int UBj,
                            int IminS, int ImaxS, int JminS, int JmaxS,
                            void* zeta_p, void* h_p,
                            void* ubar_p, void* vbar_p,
                            void* rubar_p, void* rvbar_p,
                            void* pm_p, void* pn_p,
                            void* rhs_ubar_p, void* rhs_vbar_p,
                            void* Dstp_p, void* Dnew_p,
                            int predictor, int first_2d_step, double dtfast,
                            const roms::TileBounds* t);

/* Stage 18: store the predictor right-hand side for the corrector. */

void k_step2d_store_rhs(int ng, int tile, int krhs,
                        int LBi, int UBi, int LBj, int UBj,
                        int IminS, int ImaxS, int JminS, int JmaxS,
                        void* rubar_p, void* rvbar_p,
                        void* rhs_ubar_p, void* rhs_vbar_p,
                        void* DiaRUbar_p, void* DiaRVbar_p,
                        void* DiaU2rhs_p, void* DiaV2rhs_p,
                        int NDM2d, int M2pgrd,
                        const roms::TileBounds* t);

}  // extern "C"

#endif  // ROMS_KOKKOS

#endif  // ROMS_KOKKOS_STEP2D_HPP
