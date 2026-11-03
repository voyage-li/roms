/*
** Kokkos kernels for the 3-D time-stepping routines:
**
**   pre_step3d.F   tracer predictor (horizontal advection, vertical
**                  advection with the artificial continuity equation,
**                  explicit vertical diffusion) and the 3-D momentum
**                  predictor.
**   step3d_uv.F    3-D momentum corrector: implicit vertical viscosity,
**                  vertical-mean replacement, 2-D/3-D coupling and the
**                  Huon/Hvom mass fluxes.
**
** Each entry is compiled only when its KK_STAGE_* flag is set, in which
** case the Fortran caller skips the loops it replaces.  Lateral boundary
** conditions, halo exchanges and point sources stay in Fortran.
*/

#ifndef ROMS_KOKKOS_STEP3D_HPP
#define ROMS_KOKKOS_STEP3D_HPP

#if defined ROMS_KOKKOS

#include "kokkos_config.hpp"

namespace roms {

/* Mirror of TYPE(kokkos_pre3d_config) in kokkos_step3d_mod.F; the field
** order must match. */
struct Pre3dConfig {
  int LBi, UBi, LBj, UBj;
  int Istr, Iend, Jstr, Jend, IstrU, JstrV;
  int N, NT, NAT, NDT;
  int nrhs, nstp, nnew;
  int step;                 /* 0: iic==ntfirst, 1: ntfirst+1, 2: later */
  int iTrate, iTvdif;
  int NDM3d, NDrhs, M3pgrd, M3vvis, M3rate;
  int fold_w, fold_e, fold_s, fold_n;   /* closed-edge flux folds */
  double dt, lambda;
};

/* Horizontal / vertical tracer advection schemes, as resolved from
** Hadvection(itrc,ng) / Vadvection(itrc,ng) by the caller. */
enum Pre3dScheme {
  kSchemeNone = 0,
  kCentered2 = 1,
  kUpstream1 = 2,   /* MPDATA or HSIMT: first-order upstream here */
  kUpstream3 = 3,
  kAkima4 = 4,
  kCentered4 = 5,   /* CENTERED4 or SPLIT_U3 */
  kSplines = 6      /* vertical only */
};

/* Mirror of TYPE(kokkos_uv3d_config) in kokkos_step3d_mod.F. */
struct Uv3dConfig {
  int LBi, UBi, LBj, UBj;
  int Istr, Iend, Jstr, Jend, IstrU, JstrV;
  int IstrP, IstrT, IendT, JstrT, JendT;
  int Mm, N, nrhs, nnew;
  int step;                 /* 0: iic==ntfirst, 1: ntfirst+1, 2: later */
  int NDM2d, NDM3d, NDrhs;
  int M2pgrd, M2bstr, M2sstr, M2rate, M3pgrd, M3vvis, M3rate;
  int M3xvis, M3yvis, M3hvis;   /* 0 without UV_VIS2/UV_VIS4 */
  /* (M2, M3) index pairs integrated over the column for the interior
  ** mean replacement: pgrd, vvis->bstr, then Coriolis, viscosity and
  ** advection terms as compiled. */
  int npairs, m2[12], m3[12];
  /* Boundary-point replacement switches (step3d_uv.F, coupling). */
  int rep_w, rep_e, rep_s, rep_n;
  double dt, lambda;
};

}  // namespace roms

extern "C" {

/* pre_step3d: tracer predictor t(:,:,:,3,:), explicit vertical diffusion
** into t(:,:,:,nnew,:) and the DiaTwrk rate/vdif terms. */
void k_pre_step3d_t(const roms::Pre3dConfig* c, const int* hadv,
                    const int* vadv,
                    void* pm_p, void* pn_p, void* Hz_p,
                    void* Huon_p, void* Hvom_p, void* z_r_p, void* W_p,
                    void* Akt_p, void* btflx_p, void* stflx_p,
                    void* t_p, void* DiaTwrk_p);

/* pre_step3d: u/v predictor into time level nnew and DiaU3wrk/DiaV3wrk. */
void k_pre_step3d_uv(const roms::Pre3dConfig* c,
                     void* pm_p, void* pn_p, void* Hz_p, void* z_r_p,
                     void* Akv_p, void* bustr_p, void* bvstr_p,
                     void* sustr_p, void* svstr_p,
                     void* ru_p, void* rv_p, void* u_p, void* v_p,
                     void* DiaU3wrk_p, void* DiaV3wrk_p,
                     void* DiaRU_p, void* DiaRV_p);

/* step3d_uv: right-hand-side step, implicit vertical viscosity and the
** interior vertical-mean replacement, rows Jstr..Jend. */
void k_step3d_uv_solve(const roms::Uv3dConfig* c,
                       void* pm_p, void* pn_p, void* on_u_p, void* om_v_p,
                       void* Hz_p, void* z_r_p, void* Akv_p,
                       void* DU_avg1_p, void* DV_avg1_p,
                       void* ru_p, void* rv_p, void* u_p, void* v_p,
                       void* DiaU2wrk_p, void* DiaV2wrk_p,
                       void* DiaU3wrk_p, void* DiaV3wrk_p,
                       void* DiaRU_p, void* DiaRV_p);

/* step3d_uv: 2-D/3-D coupling (ubar/vbar, boundary-point mean
** replacement, Huon/Hvom), rows JstrT..JendT, after u3dbc/v3dbc. */
void k_step3d_uv_couple(const roms::Uv3dConfig* c,
                        void* on_u_p, void* om_v_p, void* Hz_p,
                        void* DU_avg1_p, void* DU_avg2_p,
                        void* DV_avg1_p, void* DV_avg2_p,
                        void* u_p, void* v_p, void* ubar_p, void* vbar_p,
                        void* Huon_p, void* Hvom_p,
                        void* DiaU2wrk_p, void* DiaV2wrk_p,
                        void* DiaU2int_p, void* DiaV2int_p,
                        void* DiaU3wrk_p, void* DiaV3wrk_p);

}  // extern "C"

#endif  // ROMS_KOKKOS

#endif  // ROMS_KOKKOS_STEP3D_HPP
