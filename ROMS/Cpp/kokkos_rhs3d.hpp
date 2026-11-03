/*
** Kokkos kernels for the pure-computation parts of the 3-D right-hand-side
** helpers: harmonic mixing of momentum and tracers, hydrostatic pressure
** gradient, omega vertical velocity and the free-surface reload.
**
** Every entry is called only when the matching KK_STAGE_* flag is
** compiled, in which case the Fortran loops it replaces are compiled out.
** Halo exchanges, open boundaries and point sources stay in Fortran.
*/

#ifndef ROMS_KOKKOS_RHS3D_HPP
#define ROMS_KOKKOS_RHS3D_HPP

#if defined ROMS_KOKKOS

#include "kokkos_config.hpp"

extern "C" {

/* Vertical stress tensor, S-surfaces (uv3dmix2_s.h). */
void k_uv3dmix2_s(int ng, int tile, int nrhs, int nnew,
                  int LBi, int UBi, int LBj, int UBj,
                  int IminS, int ImaxS, int JminS, int JmaxS,
                  void* Hz_p,
                  void* om_p_p, void* om_r_p, void* on_p_p, void* on_r_p,
                  void* pm_p, void* pmon_p_p, void* pmon_r_p,
                  void* pn_p, void* pnom_p_p, void* pnom_r_p,
                  void* visc2_p_p, void* visc2_r_p,
                  void* DiaRUfrc_p, void* DiaRVfrc_p,
                  void* DiaU3wrk_p, void* DiaV3wrk_p,
                  void* rufrc_p, void* rvfrc_p,
                  void* u_p, void* v_p,
                  int NLev, int NDM2d, int NDM3d,
                  int M2hvis, int M2xvis, int M2yvis,
                  int M3hvis, int M3xvis, int M3yvis,
                  double dt, const roms::TileBounds* t);

/* Harmonic mixing of tracers, S-surfaces (t3dmix2_s.h). */
void k_t3dmix2_s(int ng, int tile, int nrhs, int nstp, int nnew,
                 int LBi, int UBi, int LBj, int UBj,
                 int IminS, int ImaxS, int JminS, int JmaxS,
                 void* diff2_p, void* Hz_p,
                 void* pmon_u_p, void* pnom_v_p,
                 void* pm_p, void* pn_p,
                 void* DiaTwrk_p, void* t_p,
                 int NLev, int NT, int NDT,
                 int iTxdif, int iTydif, int iThdif,
                 double dt, const roms::TileBounds* t);

/* Baroclinic pressure gradient, DJ_GRADPS (prsgrd32.h). */
void k_prsgrd32(int ng, int tile, int nrhs,
                int LBi, int UBi, int LBj, int UBj,
                int IminS, int ImaxS, int JminS, int JmaxS,
                void* om_v_p, void* on_u_p, void* Hz_p,
                void* z_r_p, void* z_w_p, void* rho_p, void* P_p,
                void* ru_p, void* rv_p,
                void* DiaRU_p, void* DiaRV_p,
                int NLev, int NDrhs, int M3pgrd,
                double g, double rho0, const roms::TileBounds* t);

/* Omega vertical velocity (omega.F), no point sources. */
void k_omega3d(int ng, int tile,
               int LBi, int UBi, int LBj, int UBj,
               int IminS, int ImaxS, int JminS, int JmaxS,
               void* Huon_p, void* Hvom_p, void* z_w_p, void* W_p,
               int NLev, const roms::TileBounds* t);

/* zeta(:,:,1:2) = Zt_avg1 (set_zeta.F). */
void k_set_zeta(int ng, int tile,
                int LBi, int UBi, int LBj, int UBj,
                int IminS, int ImaxS, int JminS, int JmaxS,
                void* Zt_avg1_p, void* zeta_p,
                const roms::TileBounds* t);

}  // extern "C"

#endif  // ROMS_KOKKOS

#endif  // ROMS_KOKKOS_RHS3D_HPP
