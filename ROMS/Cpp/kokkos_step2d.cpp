/*
** git $Id$
************************************************************************
** Copyright (c) 2002-2025 The ROMS Group                               **
**   Licensed under a MIT/X style license                               **
**   See License_ROMS.md                                                **
************************************************************************
**
** Kokkos ports of the compute stages of ROMS/Nonlinear/step2d_LF_AM3.h.
**
** Scope
** -----
** Only the pure computation sections live here.  Everything that talks
** to other tiles or ranks stays on the Fortran side and is interleaved
** between these calls: the halo exchanges (exchange_u2d_tile,
** exchange_v2d_tile, exchange_r2d_tile, mp_exchange2d), the open
** boundary routines (set_DUV_bc_tile, obc_flux_tile, u2dbc_tile,
** v2dbc_tile), zetabc_tile, wetdry_tile and the point sources.
**
** Each function corresponds to one contiguous block of loops in the
** Fortran source; the comment gives the source line range so the two
** can be diffed side by side.
**
** Converged macros
** ----------------
** This implementation covers the macro combination of the upwelling
** test case: SOLVE3D, VAR_RHO_2D, UV_ADV, UV_COR, UV_VIS2 and
** DIAGNOSTICS_UV.  The compile-time guard below rejects everything
** else rather than silently computing the wrong thing.
**
** Threading
** ---------
** Two backends are supported, selected at build time:
**
**   ROMS_KOKKOS_SERIAL  -- ROMS owns the parallelism and splits work by
**                          tile across OpenMP threads; each call here
**                          runs a plain loop.  Safe under concurrent
**                          entry from several ROMS threads.
**
**   ROMS_KOKKOS_OPENMP  -- Kokkos owns the parallelism.  ROMS must run
**                          with a single OpenMP thread, otherwise the
**                          Kokkos parallel region either nests or
**                          serialises and the result is slow but still
**                          correct.  This is the useful configuration
**                          for benchmarking the kernels themselves.
**
** Do NOT use parallel_reduce: it touches the execution space instance's
** reduce scratch and is not safe under concurrent entry.
*/

#include "kokkos_step2d.hpp"

#if defined ROMS_KOKKOS

#include <cmath>

namespace {

using roms::Array2d;
using roms::Array3d;
using roms::Array4d;
using roms::EdgeFlags;
using roms::TileBounds;

/*
** Execution space.  The selection is made in kokkos_config.hpp so that
** both this file and the test drivers agree on it.
*/

using exec_space  = roms::exec_space;

/*
** Tile index ranges start at negative values (JstrV-2, IstrUm2-1, ...)
** because they reach into the ghost region, so the policy must be
** indexed with a signed type.  Kokkos::RangePolicy's default index type
** is size_t, which would silently wrap a negative lower bound.
*/

using range_policy = Kokkos::RangePolicy<exec_space, Kokkos::IndexType<int>>;

/*
** Bounds needed to wrap the arrays step2d_tile receives with declared
** shape (LBi:UBi, LBj:UBj).  The trailing dimension sizes come from the
** caller because the assumed-shape declarations do not carry them.
*/

struct Shape {
  int LBi, UBi, LBj, UBj;
  int IminS, ImaxS, JminS, JmaxS;
};

/* Array declared (LBi:UBi, LBj:UBj). */
inline Array2d<double> grid2d(void* p, const Shape& s) {
  return roms::as_2d<double>(p, s.LBi, s.UBi, s.LBj, s.UBj);
}

/* Array declared (LBi:UBi, LBj:UBj, lb_k:ub_k). */
inline Array3d<double> grid3d(void* p, const Shape& s, int lb_k, int ub_k) {
  return roms::as_3d<double>(p, s.LBi, s.UBi, s.LBj, s.UBj, lb_k, ub_k);
}

/* Array declared (LBi:UBi, LBj:UBj, lb_k:ub_k, lb_l:ub_l). */
inline Array4d<double> grid4d(void* p, const Shape& s, int lb_k, int ub_k,
                              int lb_l, int ub_l) {
  return roms::as_4d<double>(p, s.LBi, s.UBi, s.LBj, s.UBj, lb_k, ub_k,
                             lb_l, ub_l);
}

/* Tile-private scratch, declared (IminS:ImaxS, JminS:JmaxS). */
inline Array2d<double> priv2d(void* p, const Shape& s) {
  return roms::as_2d<double>(p, s.IminS, s.ImaxS, s.JminS, s.JmaxS);
}

/*
** The diagnostic arrays come in both shapes and confusing them is a
** silent out-of-bounds write: the wrapper's stride comes from the
** bounds, so private bounds on a grid-bounded array shift every column
** and run off the end.  In step2d_tile, DiaU2rhs/DiaV2rhs are tile
** scratch (IminS:ImaxS) while DiaRUbar/DiaRVbar/DiaRUfrc/DiaRVfrc are
** full-field (LBi:UBi) -- see the dummy declarations in
** ROMS/Nonlinear/step2d_LF_AM3.h.
*/

inline Array3d<double> priv3d(void* p, const Shape& s, int lb_k, int ub_k) {
  return roms::as_3d<double>(p, s.IminS, s.ImaxS, s.JminS, s.JmaxS, lb_k, ub_k);
}

/* ROMS uses DSIGN from the Fortran runtime. */
inline double dsign(double a, double b) { return std::copysign(a, b); }


/*
** The 2-D state arrays carry three time levels, the rhs history two.
** Neither extent is N(ng), which is what UBk means in the step2d_tile
** signature; using UBk for these would silently overrun.
*/

constexpr int TLEV0 = 1, TLEV1 = 3;
constexpr int RLEV0 = 1, RLEV1 = 2;

}  // namespace

extern "C" {

/*======================================================================
** Stage 1 -- total depth and vertically integrated mass fluxes.
** Source: step2d_LF_AM3.h lines 609-703.
**
**   Drhs(i,j) = zeta(i,j,krhs) + h(i,j)
**   DUon(i,j) = 0.5*on_u*(Drhs(i,j)+Drhs(i-1,j)) * ubar(i,j,krhs)
**   DVom(i,j) = 0.5*om_v*(Drhs(i,j)+Drhs(i,j-1)) * vbar(i,j,krhs)
**====================================================================*/

void k_step2d_depth_flux(int ng, int tile, int krhs, int nfast,
                         int LBi, int UBi, int LBj, int UBj,
                         int IminS, int ImaxS, int JminS, int JmaxS,
                         void* zeta_p, void* h_p, void* on_u_p, void* om_v_p,
                         void* ubar_p, void* vbar_p, void* Drhs_p,
                         void* DUon_p, void* DVom_p, int UBk,
                         const TileBounds* t) {
  (void)ng;
  (void)tile;
  (void)nfast;
  (void)UBk;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

  Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array2d<double> h    = grid2d(h_p, s);
  Array2d<double> on_u = grid2d(on_u_p, s);
  Array2d<double> om_v = grid2d(om_v_p, s);
  Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);

  Array2d<double> Drhs = priv2d(Drhs_p, s);
  Array2d<double> DUon = priv2d(DUon_p, s);
  Array2d<double> DVom = priv2d(DVom_p, s);

#if defined DISTRIBUTE && !defined NESTING
  const int j_start = t->JstrV - 2;
  const int i_start = t->IstrU - 2;
#else
  const int j_start = t->JstrVm2 - 1;
  const int i_start = t->IstrUm2 - 1;
#endif

  const int i_end = t->Iendp2;
  const int j_end = t->Jendp2;

  Kokkos::parallel_for(
      range_policy(j_start, j_end + 1), KOKKOS_LAMBDA(int j) {
        for (int i = i_start; i <= i_end; ++i) {
          Drhs(i, j) = zeta(i, j, krhs) + h(i, j);
        }
      });

  Kokkos::parallel_for(
      range_policy(j_start, j_end + 1), KOKKOS_LAMBDA(int j) {
        for (int i = i_start + 1; i <= i_end; ++i) {
          const double cff  = 0.5 * on_u(i, j);
          const double cff1 = cff * (Drhs(i, j) + Drhs(i - 1, j));
          DUon(i, j) = ubar(i, j, krhs) * cff1;
        }
      });

  Kokkos::parallel_for(
      range_policy(j_start + 1, j_end + 1), KOKKOS_LAMBDA(int j) {
        for (int i = i_start; i <= i_end; ++i) {
          const double cff  = 0.5 * om_v(i, j);
          const double cff1 = cff * (Drhs(i, j) + Drhs(i, j - 1));
          DVom(i, j) = vbar(i, j, krhs) * cff1;
        }
      });
}

/*======================================================================
** Stage 2 -- time averaged fields over all short time steps (SOLVE3D).
** Source: step2d_LF_AM3.h lines 736-856.
**
** `weight` points at weight(1:2, 0:256, ng), i.e. the ng slab; the
** second index is the fast-step counter.  Element (level, lev) with
** level in {1,2} and lev 0-based sits at w[lev*2 + level-1].
**====================================================================*/

void k_step2d_time_average(int ng, int tile, int krhs, int istp, int inew,
                           int iif, int nfast, int predictor, int first_step,
                           int LBi, int UBi, int LBj, int UBj,
                           int IminS, int ImaxS, int JminS, int JmaxS,
                           void* zeta_p, void* ubar_p, void* vbar_p,
                           void* DUon_p, void* DVom_p,
                           void* Zt_avg1_p, void* DU_avg1_p, void* DU_avg2_p,
                           void* DV_avg1_p, void* DV_avg2_p,
                           const double* weight,
                           const TileBounds* t) {
  (void)ng;
  (void)tile;
  (void)istp;
  (void)inew;
  (void)nfast;
  (void)ubar_p;
  (void)vbar_p;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

  Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);

  Array2d<double> DUon = priv2d(DUon_p, s);
  Array2d<double> DVom = priv2d(DVom_p, s);

  Array2d<double> Zt_avg1 = grid2d(Zt_avg1_p, s);
  Array2d<double> DU_avg1 = grid2d(DU_avg1_p, s);
  Array2d<double> DU_avg2 = grid2d(DU_avg2_p, s);
  Array2d<double> DV_avg1 = grid2d(DV_avg1_p, s);
  Array2d<double> DV_avg2 = grid2d(DV_avg2_p, s);

  const int Istr  = t->Istr;
  const int IstrR = t->IstrR, IendR = t->IendR;
  const int Jstr  = t->Jstr;
  const int JstrR = t->JstrR, JendR = t->JendR;

  auto w = [weight](int level, int lev) {
    return weight[static_cast<size_t>(lev) * 2 + (level - 1)];
  };

  if (predictor) {
    if (first_step) {
      const double cff2 = (-1.0 / 12.0) * w(2, iif + 1);

      Kokkos::parallel_for(
          range_policy(JstrR, JendR + 1), KOKKOS_LAMBDA(int j) {
            for (int i = IstrR; i <= IendR; ++i) Zt_avg1(i, j) = 0.0;
            for (int i = Istr; i <= IendR; ++i) {
              DU_avg1(i, j) = 0.0;
              DU_avg2(i, j) = cff2 * DUon(i, j);
            }
          });

      Kokkos::parallel_for(
          range_policy(Jstr, JendR + 1), KOKKOS_LAMBDA(int j) {
            for (int i = IstrR; i <= IendR; ++i) {
              DV_avg1(i, j) = 0.0;
              DV_avg2(i, j) = cff2 * DVom(i, j);
            }
          });
    } else {
      const double cff1 = w(1, iif - 1);
      const double cff2 =
          (8.0 / 12.0) * w(2, iif) - (1.0 / 12.0) * w(2, iif + 1);

      Kokkos::parallel_for(
          range_policy(JstrR, JendR + 1), KOKKOS_LAMBDA(int j) {
            for (int i = IstrR; i <= IendR; ++i) {
              Zt_avg1(i, j) += cff1 * zeta(i, j, krhs);
            }
            for (int i = Istr; i <= IendR; ++i) {
              DU_avg1(i, j) += cff1 * DUon(i, j);
              DU_avg2(i, j) += cff2 * DUon(i, j);
            }
          });

      Kokkos::parallel_for(
          range_policy(Jstr, JendR + 1), KOKKOS_LAMBDA(int j) {
            for (int i = IstrR; i <= IendR; ++i) {
              DV_avg1(i, j) += cff1 * DVom(i, j);
              DV_avg2(i, j) += cff2 * DVom(i, j);
            }
          });
    }
  } else {
    const double cff2 = first_step ? w(2, iif) : (5.0 / 12.0) * w(2, iif);

    Kokkos::parallel_for(
        range_policy(JstrR, JendR + 1), KOKKOS_LAMBDA(int j) {
          for (int i = Istr; i <= IendR; ++i) {
            DU_avg2(i, j) += cff2 * DUon(i, j);
          }
        });

    Kokkos::parallel_for(
        range_policy(Jstr, JendR + 1), KOKKOS_LAMBDA(int j) {
          for (int i = IstrR; i <= IendR; ++i) {
            DV_avg2(i, j) += cff2 * DVom(i, j);
          }
        });
  }
}

/*======================================================================
** Stage 4 -- free-surface time stepping.
** Source: step2d_LF_AM3.h lines 885-1035.
**
** Branch selection follows FIRST_2D_STEP, which is iif==1 when SOLVE3D
** is defined and iic==ntfirst otherwise (ROMS/Include/globaldefs.h).
** The caller resolves that and passes the result -- getting this wrong
** silently runs the wrong integrator for every fast step after the
** first one.
**====================================================================*/

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
                        const TileBounds* t) {
  (void)ng;
  (void)tile;
  (void)nfast;
  (void)predictor;
  (void)UBk;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

  Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array2d<double> h    = grid2d(h_p, s);
  Array2d<double> pm   = grid2d(pm_p, s);
  Array2d<double> pn   = grid2d(pn_p, s);

  Array2d<double> DUon = priv2d(DUon_p, s);
  Array2d<double> DVom = priv2d(DVom_p, s);

  Array2d<double> gzeta   = priv2d(gzeta_p, s);
  Array2d<double> gzeta2  = priv2d(gzeta2_p, s);
  Array2d<double> gzetaSA = priv2d(gzetaSA_p, s);
  Array2d<double> rhoA    = grid2d(rhoA_p, s);
  Array2d<double> rhoS    = grid2d(rhoS_p, s);

  Array2d<double> rhs_zeta = priv2d(rhs_zeta_p, s);
  Array2d<double> zeta_new = priv2d(zeta_new_p, s);
  Array2d<double> Dnew     = priv2d(Dnew_p, s);
  Array2d<double> zwrk     = priv2d(zwrk_p, s);

  Array3d<double> rzeta = roms::as_3d<double>(rzeta_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);

  /*
  ** VAR_RHO_2D is switched on automatically whenever SOLVE3D is defined
  ** (ROMS/Include/globaldefs.h).  rhoA and rhoS are nondimensional, so
  ** fac makes the contribution dimensional.
  */

  const double fac  = 1000.0 / rho0;
  const int    ptsk = 3 - kstp;

  const int j0 = t->JstrV - 1, j1 = t->Jend;
  const int i0 = t->IstrU - 1, i1 = t->Iend;

  if (first_2d_step) {
    const double cff1 = dtfast;

    Kokkos::parallel_for(
        range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
          for (int i = i0; i <= i1; ++i) {
            const double rz = (DUon(i, j) - DUon(i + 1, j)) +
                              (DVom(i, j) - DVom(i, j + 1));
            rhs_zeta(i, j) = rz;
            const double zn = zeta(i, j, kstp) + pm(i, j) * pn(i, j) * cff1 * rz;
            zeta_new(i, j) = zn;
            Dnew(i, j)     = zn + h(i, j);
            const double zw = 0.5 * (zeta(i, j, kstp) + zn);
            zwrk(i, j)      = zw;
            gzeta(i, j)     = (fac + rhoS(i, j)) * zw;
            gzeta2(i, j)    = gzeta(i, j) * zw;
            gzetaSA(i, j)   = zw * (rhoS(i, j) - rhoA(i, j));
          }
        });
  } else if (predictor) {
    const double cff1 = 2.0 * dtfast;
    const double cff4 = 4.0 / 25.0;
    const double cff5 = 1.0 - 2.0 * cff4;

    Kokkos::parallel_for(
        range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
          for (int i = i0; i <= i1; ++i) {
            const double rz = (DUon(i, j) - DUon(i + 1, j)) +
                              (DVom(i, j) - DVom(i, j + 1));
            rhs_zeta(i, j) = rz;
            const double zn = zeta(i, j, kstp) + pm(i, j) * pn(i, j) * cff1 * rz;
            zeta_new(i, j) = zn;
            Dnew(i, j)     = zn + h(i, j);
            const double zw =
                cff5 * zeta(i, j, krhs) + cff4 * (zeta(i, j, kstp) + zn);
            zwrk(i, j)      = zw;
            gzeta(i, j)     = (fac + rhoS(i, j)) * zw;
            gzeta2(i, j)    = gzeta(i, j) * zw;
            gzetaSA(i, j)   = zw * (rhoS(i, j) - rhoA(i, j));
          }
        });
  } else {
    const double cff1 = dtfast * 5.0 / 12.0;
    const double cff2 = dtfast * 8.0 / 12.0;
    const double cff3 = dtfast * 1.0 / 12.0;
    const double cff4 = 2.0 / 5.0;
    const double cff5 = 1.0 - cff4;

    Kokkos::parallel_for(
        range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
          for (int i = i0; i <= i1; ++i) {
            const double cff = cff1 * ((DUon(i, j) - DUon(i + 1, j)) +
                                       (DVom(i, j) - DVom(i, j + 1)));
            const double zn = zeta(i, j, kstp) +
                              pm(i, j) * pn(i, j) *
                                  (cff + cff2 * rzeta(i, j, kstp) -
                                   cff3 * rzeta(i, j, ptsk));
            zeta_new(i, j) = zn;
            Dnew(i, j)     = zn + h(i, j);
            const double zw = cff5 * zn + cff4 * zeta(i, j, krhs);
            zwrk(i, j)      = zw;
            gzeta(i, j)     = (fac + rhoS(i, j)) * zw;
            gzeta2(i, j)    = gzeta(i, j) * zw;
            gzetaSA(i, j)   = zw * (rhoS(i, j) - rhoA(i, j));
          }
        });
  }

  const int ai0 = t->Istr, ai1 = t->Iend;
  const int aj0 = t->Jstr, aj1 = t->Jend;

  Kokkos::parallel_for(
      range_policy(aj0, aj1 + 1), KOKKOS_LAMBDA(int j) {
        for (int i = ai0; i <= ai1; ++i) {
          zeta(i, j, knew) = zeta_new(i, j);
        }
      });
}

/*======================================================================
** Stage 6 -- pressure gradient, first contribution to rhs_ubar/rhs_vbar.
** Source: step2d_LF_AM3.h lines 1150-1290.
**
**   rhs_ubar(i,j) = 0.5*g*on_u(i,j) *
**     [ (h(i-1)+h(i))*(gzeta(i-1)-gzeta(i))
**     + (h(i-1)-h(i))*(gzetaSA(i-1)+gzetaSA(i)
**        + (1/3)*(rhoA(i-1)-rhoA(i))*(zwrk(i-1)-zwrk(i)))
**     + (gzeta2(i-1)-gzeta2(i)) ]
**
** and the eta-direction counterpart with om_v and j-1.
**====================================================================*/

void k_step2d_pressure_grad(int ng, int tile,
                            int LBi, int UBi, int LBj, int UBj,
                            int IminS, int ImaxS, int JminS, int JmaxS,
                            void* h_p, void* on_u_p, void* om_v_p,
                            void* gzeta_p, void* gzeta2_p,
                            void* gzetaSA_p, void* rhoA_p,
                            void* zwrk_p,
                            void* rhs_ubar_p, void* rhs_vbar_p,
                            void* DiaU2rhs_p, void* DiaV2rhs_p,
                            int NDM2d, int M2pgrd,
                            double g,
                            const TileBounds* t) {
  (void)ng;
  (void)tile;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

  Array2d<double> h       = grid2d(h_p, s);
  Array2d<double> on_u    = grid2d(on_u_p, s);
  Array2d<double> om_v    = grid2d(om_v_p, s);
  Array2d<double> gzeta   = priv2d(gzeta_p, s);
  Array2d<double> gzeta2  = priv2d(gzeta2_p, s);
  Array2d<double> gzetaSA = priv2d(gzetaSA_p, s);
  Array2d<double> rhoA    = grid2d(rhoA_p, s);
  Array2d<double> zwrk    = priv2d(zwrk_p, s);

  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);

#if defined DIAGNOSTICS_UV
  Array3d<double> DiaU2rhs =
      priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs =
      priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
#else
  (void)DiaU2rhs_p;
  (void)DiaV2rhs_p;
  (void)M2pgrd;
#endif
  (void)NDM2d;

  const double cff1 = 0.5 * g;
  const double cff2 = 1.0 / 3.0;

  const int iu0 = t->IstrU, iu1 = t->Iend;
  const int iv0 = t->Istr,  iv1 = t->Iend;
  const int jv0 = t->Jstr,  jv1 = t->Jend;
  const int jstr_v = t->JstrV;

  Kokkos::parallel_for(
      range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
        for (int i = iu0; i <= iu1; ++i) {
          const double val =
              cff1 * on_u(i, j) *
              ((h(i - 1, j) + h(i, j)) * (gzeta(i - 1, j) - gzeta(i, j)) +
               (h(i - 1, j) - h(i, j)) *
                   (gzetaSA(i - 1, j) + gzetaSA(i, j) +
                    cff2 * (rhoA(i - 1, j) - rhoA(i, j)) *
                        (zwrk(i - 1, j) - zwrk(i, j))) +
               (gzeta2(i - 1, j) - gzeta2(i, j)));
          rhs_ubar(i, j) = val;
#if defined DIAGNOSTICS_UV
          DiaU2rhs(i, j, M2pgrd) = val;
#endif
        }
      });

  Kokkos::parallel_for(
      range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
        if (j >= jstr_v) {
          for (int i = iv0; i <= iv1; ++i) {
            const double val =
                cff1 * om_v(i, j) *
                ((h(i, j - 1) + h(i, j)) * (gzeta(i, j - 1) - gzeta(i, j)) +
                 (h(i, j - 1) - h(i, j)) *
                     (gzetaSA(i, j - 1) + gzetaSA(i, j) +
                      cff2 * (rhoA(i, j - 1) - rhoA(i, j)) *
                          (zwrk(i, j - 1) - zwrk(i, j))) +
                 (gzeta2(i, j - 1) - gzeta2(i, j)));
            rhs_vbar(i, j) = val;
#if defined DIAGNOSTICS_UV
            DiaV2rhs(i, j, M2pgrd) = val;
#endif
          }
        }
      });
}

/*======================================================================
** Stage 7 -- horizontal advection of 2-D momentum, fourth-order.
** Source: step2d_LF_AM3.h lines 1327-1495.
**
** Two passes: build the fluxes UFx/UFe/VFx/VFe from a biharmonic-
** corrected interpolation, then add their divergence to the rhs.
** grad and Dgrad are scratch shared between passes, so the passes stay
** in separate kernels.
**====================================================================*/

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
                             const EdgeFlags* e,
                             const TileBounds* t) {
  (void)ng;
  (void)tile;
  (void)NDM2d;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

  Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);

  Array2d<double> DUon = priv2d(DUon_p, s);
  Array2d<double> DVom = priv2d(DVom_p, s);

  Array2d<double> grad  = priv2d(grad_p, s);
  Array2d<double> Dgrad = priv2d(Dgrad_p, s);

  Array2d<double> UFx = priv2d(UFx_p, s);
  Array2d<double> UFe = priv2d(UFe_p, s);
  Array2d<double> VFx = priv2d(VFx_p, s);
  Array2d<double> VFe = priv2d(VFe_p, s);

  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);

#if defined DIAGNOSTICS_UV
  Array3d<double> DiaU2rhs =
      priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs =
      priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
#else
  (void)DiaU2rhs_p;
  (void)DiaV2rhs_p;
  (void)M2xadv;
  (void)M2yadv;
  (void)M2hadv;
#endif

  const double cff = 1.0 / 6.0;

  const int Istr = t->Istr, Iend = t->Iend;
  const int Jstr = t->Jstr, Jend = t->Jend;
  const int IstrU = t->IstrU, JstrV = t->JstrV;
  const int IstrUm1 = t->IstrUm1, Iendp1 = t->Iendp1;
  const int JstrVm1 = t->JstrVm1, Jendp1 = t->Jendp1;
  const int Jstrm1 = t->Jstrm1, Istrm1 = t->Istrm1;


  /* XI-direction gradient, then UFx. */

  Kokkos::parallel_for(
      range_policy(Jstr, Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = IstrUm1; i <= Iendp1; ++i) {
          grad(i, j)  = ubar(i - 1, j, krhs) - 2.0 * ubar(i, j, krhs) +
                        ubar(i + 1, j, krhs);
          Dgrad(i, j) = DUon(i - 1, j) - 2.0 * DUon(i, j) + DUon(i + 1, j);
        }
      });

  /* Non-periodic physical edges: copy the neighbouring interior value
  ** into the halo cell so the four-point stencil stays one-sided. */

  if (!e->composite_w && !e->periodic_ew && e->west) {
    Kokkos::parallel_for(
        range_policy(Jstr, Jend + 1), KOKKOS_LAMBDA(int j) {
          grad(Istr, j)  = grad(Istr + 1, j);
          Dgrad(Istr, j) = Dgrad(Istr + 1, j);
        });
  }

  if (!e->composite_e && !e->periodic_ew && e->east) {
    Kokkos::parallel_for(
        range_policy(Jstr, Jend + 1), KOKKOS_LAMBDA(int j) {
          grad(Iend + 1, j)  = grad(Iend, j);
          Dgrad(Iend + 1, j) = Dgrad(Iend, j);
        });
  }

  Kokkos::parallel_for(
      range_policy(Jstr, Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = IstrU - 1; i <= Iend; ++i) {
          UFx(i, j) =
              0.25 * (ubar(i, j, krhs) + ubar(i + 1, j, krhs) -
                      cff * (grad(i, j) + grad(i + 1, j))) *
              (DUon(i, j) + DUon(i + 1, j) -
               cff * (Dgrad(i, j) + Dgrad(i + 1, j)));
        }
      });

  /* ETA-direction gradient (at U points), then UFe. */

  Kokkos::parallel_for(
      range_policy(Jstrm1, Jendp1 + 1), KOKKOS_LAMBDA(int j) {
        for (int i = IstrU; i <= Iend; ++i) {
          grad(i, j) = ubar(i, j - 1, krhs) - 2.0 * ubar(i, j, krhs) +
                       ubar(i, j + 1, krhs);
        }
      });

  if (!e->composite_s && !e->periodic_ns && e->south) {
    Kokkos::parallel_for(
        range_policy(IstrU, Iend + 1), KOKKOS_LAMBDA(int i) {
          grad(i, Jstr - 1) = grad(i, Jstr);
        });
  }

  if (!e->composite_n && !e->periodic_ns && e->north) {
    Kokkos::parallel_for(
        range_policy(IstrU, Iend + 1), KOKKOS_LAMBDA(int i) {
          grad(i, Jend + 1) = grad(i, Jend);
        });
  }

  Kokkos::parallel_for(
      range_policy(Jstr, Jend + 2), KOKKOS_LAMBDA(int j) {
        for (int i = IstrU - 1; i <= Iend; ++i) {
          Dgrad(i, j) = DVom(i - 1, j) - 2.0 * DVom(i, j) + DVom(i + 1, j);
        }
      });

  /*
  ** The Fortran loop is "DO j=Jstr,Jend+1" -- a literal Jend+1, NOT
  ** Jendp1.  The two differ on a closed northern edge: get_bounds.F
  ** sets Jendp1 = MIN(Jend+1, Mm), which collapses to Jend when the
  ** tile sits on the boundary, dropping the very halo row that the
  ** rhs_ubar difference at j=Jend needs.  Jstrm1 has the matching
  ** clamp on the south side (MAX(1, Jstr-1)), which is why this read
  ** as symmetric and slipped through.
  */
  Kokkos::parallel_for(
      range_policy(Jstrm1, Jend + 2), KOKKOS_LAMBDA(int j) {
        for (int i = IstrU; i <= Iend; ++i) {
          UFe(i, j) =
              0.25 * (ubar(i, j, krhs) + ubar(i, j - 1, krhs) -
                      cff * (grad(i, j) + grad(i, j - 1))) *
              (DVom(i, j) + DVom(i - 1, j) -
               cff * (Dgrad(i, j) + Dgrad(i - 1, j)));
        }
      });

  /* XI-direction gradient at V points, then VFx. */

  Kokkos::parallel_for(
      range_policy(JstrV, Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = Istrm1; i <= Iendp1; ++i) {
          grad(i, j) = vbar(i - 1, j, krhs) - 2.0 * vbar(i, j, krhs) +
                       vbar(i + 1, j, krhs);
        }
      });

  if (!e->composite_w && !e->periodic_ew && e->west) {
    Kokkos::parallel_for(
        range_policy(JstrV, Jend + 1), KOKKOS_LAMBDA(int j) {
          grad(Istr - 1, j) = grad(Istr, j);
        });
  }

  if (!e->composite_e && !e->periodic_ew && e->east) {
    Kokkos::parallel_for(
        range_policy(JstrV, Jend + 1), KOKKOS_LAMBDA(int j) {
          grad(Iend + 1, j) = grad(Iend, j);
        });
  }

  Kokkos::parallel_for(
      range_policy(JstrVm1, Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = Istr; i <= Iend + 1; ++i) {
          Dgrad(i, j) = DUon(i, j - 1) - 2.0 * DUon(i, j) + DUon(i, j + 1);
        }
      });

  Kokkos::parallel_for(
      range_policy(JstrV, Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = Istr; i <= Iend + 1; ++i) {
          VFx(i, j) =
              0.25 * (vbar(i, j, krhs) + vbar(i - 1, j, krhs) -
                      cff * (grad(i, j) + grad(i - 1, j))) *
              (DUon(i, j) + DUon(i, j - 1) -
               cff * (Dgrad(i, j) + Dgrad(i, j - 1)));
        }
      });

  /* ETA-direction gradient at V points, then VFe. */

  Kokkos::parallel_for(
      range_policy(JstrVm1, Jendp1 + 1), KOKKOS_LAMBDA(int j) {
        for (int i = Istr; i <= Iend; ++i) {
          grad(i, j)  = vbar(i, j - 1, krhs) - 2.0 * vbar(i, j, krhs) +
                        vbar(i, j + 1, krhs);
          Dgrad(i, j) = DVom(i, j - 1) - 2.0 * DVom(i, j) + DVom(i, j + 1);
        }
      });

  if (!e->composite_s && !e->periodic_ns && e->south) {
    Kokkos::parallel_for(
        range_policy(Istr, Iend + 1), KOKKOS_LAMBDA(int i) {
          grad(i, Jstr)  = grad(i, Jstr + 1);
          Dgrad(i, Jstr) = Dgrad(i, Jstr + 1);
        });
  }

  if (!e->composite_n && !e->periodic_ns && e->north) {
    Kokkos::parallel_for(
        range_policy(Istr, Iend + 1), KOKKOS_LAMBDA(int i) {
          grad(i, Jend + 1)  = grad(i, Jend);
          Dgrad(i, Jend + 1) = Dgrad(i, Jend);
        });
  }

  Kokkos::parallel_for(
      range_policy(JstrV - 1, Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = Istr; i <= Iend; ++i) {
          VFe(i, j) =
              0.25 * (vbar(i, j, krhs) + vbar(i, j + 1, krhs) -
                      cff * (grad(i, j) + grad(i, j + 1))) *
              (DVom(i, j) + DVom(i, j + 1) -
               cff * (Dgrad(i, j) + Dgrad(i, j + 1)));
        }
      });

  /* Divergence of the fluxes into the rhs. */

  Kokkos::parallel_for(
      range_policy(Jstr, Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = IstrU; i <= Iend; ++i) {
          const double cff1 = UFx(i, j) - UFx(i - 1, j);
          const double cff2 = UFe(i, j + 1) - UFe(i, j);
          const double f    = cff1 + cff2;
          rhs_ubar(i, j) -= f;
#if defined DIAGNOSTICS_UV
          DiaU2rhs(i, j, M2xadv) = -cff1;
          DiaU2rhs(i, j, M2yadv) = -cff2;
          DiaU2rhs(i, j, M2hadv) = -f;
#endif
        }
      });

  Kokkos::parallel_for(
      range_policy(JstrV, Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = Istr; i <= Iend; ++i) {
          const double cff1 = VFx(i + 1, j) - VFx(i, j);
          const double cff2 = VFe(i, j) - VFe(i, j - 1);
          const double f    = cff1 + cff2;
          rhs_vbar(i, j) -= f;
#if defined DIAGNOSTICS_UV
          DiaV2rhs(i, j, M2xadv) = -cff1;
          DiaV2rhs(i, j, M2yadv) = -cff2;
          DiaV2rhs(i, j, M2hadv) = -f;
#endif
        }
      });
}

/*======================================================================
** Stage 8 -- Coriolis term.
** Source: step2d_LF_AM3.h lines 1498-1545.
**
**   cff       = 0.5*Drhs*fomn
**   UFx(i,j)  = cff*(vbar(i,j)+vbar(i,j+1))
**   VFe(i,j)  = cff*(ubar(i,j)+ubar(i+1,j))
**   rhs_ubar += 0.5*(UFx(i,j)+UFx(i-1,j))
**   rhs_vbar -= 0.5*(VFe(i,j)+VFe(i,j-1))
**====================================================================*/

void k_step2d_coriolis(int ng, int tile, int krhs,
                       int LBi, int UBi, int LBj, int UBj,
                       int IminS, int ImaxS, int JminS, int JmaxS,
                       void* Drhs_p, void* fomn_p,
                       void* ubar_p, void* vbar_p,
                       void* UFx_p, void* VFe_p,
                       void* rhs_ubar_p, void* rhs_vbar_p,
                       void* DiaU2rhs_p, void* DiaV2rhs_p,
                       int NDM2d, int M2fcor,
                       const TileBounds* t) {
  (void)ng;
  (void)tile;
  (void)NDM2d;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

  Array2d<double> Drhs = priv2d(Drhs_p, s);
  Array2d<double> fomn = grid2d(fomn_p, s);

  Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);

  Array2d<double> UFx      = priv2d(UFx_p, s);
  Array2d<double> VFe      = priv2d(VFe_p, s);
  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);

#if defined DIAGNOSTICS_UV
  Array3d<double> DiaU2rhs =
      priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs =
      priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
#else
  (void)DiaU2rhs_p;
  (void)DiaV2rhs_p;
  (void)M2fcor;
#endif

  const int j0 = t->JstrV - 1, j1 = t->Jend;
  const int i0 = t->IstrU - 1, i1 = t->Iend;

  Kokkos::parallel_for(
      range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
        for (int i = i0; i <= i1; ++i) {
          const double c = 0.5 * Drhs(i, j) * fomn(i, j);
          UFx(i, j) = c * (vbar(i, j, krhs) + vbar(i, j + 1, krhs));
          VFe(i, j) = c * (ubar(i, j, krhs) + ubar(i + 1, j, krhs));
        }
      });

  Kokkos::parallel_for(
      range_policy(t->Jstr, t->Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = t->IstrU; i <= i1; ++i) {
          const double f = 0.5 * (UFx(i, j) + UFx(i - 1, j));
          rhs_ubar(i, j) += f;
#if defined DIAGNOSTICS_UV
          DiaU2rhs(i, j, M2fcor) = f;
#endif
        }
      });

  Kokkos::parallel_for(
      range_policy(t->JstrV, t->Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = t->Istr; i <= t->Iend; ++i) {
          const double f = 0.5 * (VFe(i, j) + VFe(i, j - 1));
          rhs_vbar(i, j) -= f;
#if defined DIAGNOSTICS_UV
          DiaV2rhs(i, j, M2fcor) = -f;
#endif
        }
      });
}

/*======================================================================
** Stage 10 -- total depth at PSI points.
** Source: step2d_LF_AM3.h lines 1564-1581.
**
**   Drhs_p(i,j) = 0.25*(Drhs(i,j)+Drhs(i-1,j)+Drhs(i,j-1)+Drhs(i-1,j-1))
**====================================================================*/

void k_step2d_psi_depth(int ng, int tile,
                        int LBi, int UBi, int LBj, int UBj,
                        int IminS, int ImaxS, int JminS, int JmaxS,
                        void* Drhs_p, void* Drhs_psi_p,
                        const TileBounds* t) {
  (void)ng;
  (void)tile;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

  Array2d<double> Drhs     = priv2d(Drhs_p, s);
  Array2d<double> Drhs_psi = priv2d(Drhs_psi_p, s);

#if defined UV_VIS4
  const int j0 = t->Jstrm1, j1 = t->Jendp2;
  const int i0 = t->Istrm1, i1 = t->Iendp2;
#else
  const int j0 = t->Jstr, j1 = t->Jend + 1;
  const int i0 = t->Istr, i1 = t->Iend + 1;
#endif

  Kokkos::parallel_for(
      range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
        for (int i = i0; i <= i1; ++i) {
          Drhs_psi(i, j) = 0.25 * (Drhs(i, j) + Drhs(i - 1, j) +
                                   Drhs(i, j - 1) + Drhs(i - 1, j - 1));
        }
      });
}

/*======================================================================
** Stage 11 -- horizontal harmonic viscosity (UV_VIS2).
** Source: step2d_LF_AM3.h lines 1584-1652.
**====================================================================*/

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
                     const TileBounds* t) {
  (void)ng;
  (void)tile;
  (void)NDM2d;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

  Array2d<double> Drhs     = priv2d(Drhs_p, s);
  Array2d<double> Drhs_psi = priv2d(Drhs_psi_p, s);

  Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);

  Array2d<double> pm = grid2d(pm_p, s);
  Array2d<double> pn = grid2d(pn_p, s);

  Array2d<double> pmon_r = grid2d(pmon_r_p, s);
  Array2d<double> pnom_r = grid2d(pnom_r_p, s);
  Array2d<double> pmon_p = grid2d(pmon_p_p, s);
  Array2d<double> pnom_p = grid2d(pnom_p_p, s);
  Array2d<double> om_r   = grid2d(om_r_p, s);
  Array2d<double> on_r   = grid2d(on_r_p, s);
  Array2d<double> om_p   = grid2d(om_p_p, s);
  Array2d<double> on_p   = grid2d(on_p_p, s);

  Array2d<double> visc2_r = grid2d(visc2_r_p, s);
  Array2d<double> visc2_pp = grid2d(visc2_p2_p, s);

  Array2d<double> UFx = priv2d(UFx_p, s);
  Array2d<double> UFe = priv2d(UFe_p, s);
  Array2d<double> VFx = priv2d(VFx_p, s);
  Array2d<double> VFe = priv2d(VFe_p, s);

  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);

#if defined DIAGNOSTICS_UV
  Array3d<double> DiaU2rhs =
      priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs =
      priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
#else
  (void)DiaU2rhs_p;
  (void)DiaV2rhs_p;
  (void)M2hvis;
  (void)M2xvis;
  (void)M2yvis;
#endif

  const int Istr = t->Istr, Iend = t->Iend;
  const int Jstr = t->Jstr, Jend = t->Jend;
  const int IstrU = t->IstrU, JstrV = t->JstrV;

  /* Fluxes at R points. */

  Kokkos::parallel_for(
      range_policy(JstrV - 1, Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = IstrU - 1; i <= Iend; ++i) {
          const double c =
              visc2_r(i, j) * Drhs(i, j) * 0.5 *
              (pmon_r(i, j) *
                   ((pn(i, j) + pn(i + 1, j)) * ubar(i + 1, j, krhs) -
                    (pn(i - 1, j) + pn(i, j)) * ubar(i, j, krhs)) -
               pnom_r(i, j) *
                   ((pm(i, j) + pm(i, j + 1)) * vbar(i, j + 1, krhs) -
                    (pm(i, j - 1) + pm(i, j)) * vbar(i, j, krhs)));
          UFx(i, j) = on_r(i, j) * on_r(i, j) * c;
          VFe(i, j) = om_r(i, j) * om_r(i, j) * c;
        }
      });

  /* Fluxes at PSI points. */

  Kokkos::parallel_for(
      range_policy(Jstr, Jend + 2), KOKKOS_LAMBDA(int j) {
        for (int i = Istr; i <= Iend + 1; ++i) {
          const double c =
              visc2_pp(i, j) * Drhs_psi(i, j) * 0.5 *
              (pmon_p(i, j) *
                   ((pn(i, j - 1) + pn(i, j)) * vbar(i, j, krhs) -
                    (pn(i - 1, j - 1) + pn(i - 1, j)) * vbar(i - 1, j, krhs)) +
               pnom_p(i, j) *
                   ((pm(i - 1, j) + pm(i, j)) * ubar(i, j, krhs) -
                    (pm(i - 1, j - 1) + pm(i, j - 1)) * ubar(i, j - 1, krhs)));
          UFe(i, j) = om_p(i, j) * om_p(i, j) * c;
          VFx(i, j) = on_p(i, j) * on_p(i, j) * c;
        }
      });

  /* Add the divergence to the rhs. */

  Kokkos::parallel_for(
      range_policy(Jstr, Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = IstrU; i <= Iend; ++i) {
          const double cff1 =
              0.5 * (pn(i - 1, j) + pn(i, j)) * (UFx(i, j) - UFx(i - 1, j));
          const double cff2 =
              0.5 * (pm(i - 1, j) + pm(i, j)) * (UFe(i, j + 1) - UFe(i, j));
          const double f = cff1 + cff2;
          rhs_ubar(i, j) += f;
#if defined DIAGNOSTICS_UV
          DiaU2rhs(i, j, M2hvis) = f;
          DiaU2rhs(i, j, M2xvis) = cff1;
          DiaU2rhs(i, j, M2yvis) = cff2;
#endif
        }
      });

  Kokkos::parallel_for(
      range_policy(JstrV, Jend + 1), KOKKOS_LAMBDA(int j) {
        for (int i = Istr; i <= Iend; ++i) {
          const double cff1 =
              0.5 * (pn(i, j - 1) + pn(i, j)) * (VFx(i + 1, j) - VFx(i, j));
          const double cff2 =
              0.5 * (pm(i, j - 1) + pm(i, j)) * (VFe(i, j) - VFe(i, j - 1));
          const double f = cff1 - cff2;
          rhs_vbar(i, j) += f;
#if defined DIAGNOSTICS_UV
          DiaV2rhs(i, j, M2hvis) = f;
          DiaV2rhs(i, j, M2xvis) = cff1;
          DiaV2rhs(i, j, M2yvis) = -cff2;
#endif
        }
      });
}

/*======================================================================
** Stage 14 -- coupling between the 2-D and 3-D equations.
** Source: step2d_LF_AM3.h lines 2316-2545.
**
** On the first barotropic predictor step the vertically integrated 3-D
** forcing (rufrc/rvfrc) is turned into a correction: subtract the fast
** rhs, store the result, add it back.  Subsequent steps add the stored
** forcing, with 3/2-1/2 extrapolation on the second 3-D step.
**
** ru(:,:,0,:) and rv(:,:,0,:) hold the correction history; the third
** index 0 is the surface level, which is all this stage touches.
**====================================================================*/

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
                       const TileBounds* t) {
  (void)ng;
  (void)tile;
  (void)NDM2d;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

  Array2d<double> rufrc = grid2d(rufrc_p, s);
  Array2d<double> rvfrc = grid2d(rvfrc_p, s);

  /*
  ** ru/rv are (LBi:UBi, LBj:UBj, 0:UBk, 2) -- OCEAN(ng) % ru is
  ** allocated 0:N(ng).  Only level 0 is touched here, but the k extent
  ** still sets the stride of the trailing time-level dimension, so it
  ** must be the real UBk rather than 0.
  */

  Array4d<double> ru = grid4d(ru_p, s, 0, t->UBk, 1, 2);
  Array4d<double> rv = grid4d(rv_p, s, 0, t->UBk, 1, 2);

  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);

#if defined DIAGNOSTICS_UV
  Array3d<double> DiaU2rhs =
      priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs =
      priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
  Array4d<double> DiaRUfrc =
      grid4d(DiaRUfrc_p, s, 1, 3, 1, NDM2d - 1);
  Array4d<double> DiaRVfrc =
      grid4d(DiaRVfrc_p, s, 1, 3, 1, NDM2d - 1);
#else
  (void)DiaU2rhs_p;
  (void)DiaV2rhs_p;
  (void)DiaRUfrc_p;
  (void)DiaRVfrc_p;
  (void)M2pgrd;
  (void)M2sstr;
  (void)M2bstr;
#endif

  const int iu0 = t->IstrU, iu1 = t->Iend;
  const int iv0 = t->Istr,  iv1 = t->Iend;
  const int jv0 = t->Jstr,  jv1 = t->Jend;
  const int jstr_v = t->JstrV;

  const bool first = (first_2d_step != 0) && (predictor != 0);

  if (first && iic == ntfirst) {
    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          for (int i = iu0; i <= iu1; ++i) {
            rufrc(i, j) = rufrc(i, j) - rhs_ubar(i, j);
            rhs_ubar(i, j) += rufrc(i, j);
            ru(i, j, 0, istp) = rufrc(i, j);
#if defined DIAGNOSTICS_UV
            for (int id = 1; id <= M2pgrd; ++id) {
              DiaRUfrc(i, j, 3, id) -= DiaU2rhs(i, j, id);
              DiaU2rhs(i, j, id) += DiaRUfrc(i, j, 3, id);
              DiaRUfrc(i, j, istp, id) = DiaRUfrc(i, j, 3, id);
            }
            DiaU2rhs(i, j, M2sstr)       = DiaRUfrc(i, j, 3, M2sstr);
            DiaRUfrc(i, j, istp, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
            DiaU2rhs(i, j, M2bstr)       = DiaRUfrc(i, j, 3, M2bstr);
            DiaRUfrc(i, j, istp, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
#endif
          }
        });

    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          if (j >= jstr_v) {
            for (int i = iv0; i <= iv1; ++i) {
              rvfrc(i, j) = rvfrc(i, j) - rhs_vbar(i, j);
              rhs_vbar(i, j) += rvfrc(i, j);
              rv(i, j, 0, istp) = rvfrc(i, j);
#if defined DIAGNOSTICS_UV
              for (int id = 1; id <= M2pgrd; ++id) {
                DiaRVfrc(i, j, 3, id) -= DiaV2rhs(i, j, id);
                DiaV2rhs(i, j, id) += DiaRVfrc(i, j, 3, id);
                DiaRVfrc(i, j, istp, id) = DiaRVfrc(i, j, 3, id);
              }
              DiaV2rhs(i, j, M2sstr)       = DiaRVfrc(i, j, 3, M2sstr);
              DiaRVfrc(i, j, istp, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
              DiaV2rhs(i, j, M2bstr)       = DiaRVfrc(i, j, 3, M2bstr);
              DiaRVfrc(i, j, istp, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
#endif
            }
          }
        });
  } else if (first && iic == ntfirst + 1) {
    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          for (int i = iu0; i <= iu1; ++i) {
            rufrc(i, j) = rufrc(i, j) - rhs_ubar(i, j);
            rhs_ubar(i, j) += 1.5 * rufrc(i, j) - 0.5 * ru(i, j, 0, inew);
            ru(i, j, 0, istp) = rufrc(i, j);
#if defined DIAGNOSTICS_UV
            for (int id = 1; id <= M2pgrd; ++id) {
              DiaRUfrc(i, j, 3, id) -= DiaU2rhs(i, j, id);
              DiaU2rhs(i, j, id) += 1.5 * DiaRUfrc(i, j, 3, id) -
                                    0.5 * DiaRUfrc(i, j, inew, id);
              DiaRUfrc(i, j, istp, id) = DiaRUfrc(i, j, 3, id);
            }
            DiaU2rhs(i, j, M2sstr) =
                1.5 * DiaRUfrc(i, j, 3, M2sstr) -
                0.5 * DiaRUfrc(i, j, inew, M2sstr);
            DiaRUfrc(i, j, istp, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
            DiaU2rhs(i, j, M2bstr) =
                1.5 * DiaRUfrc(i, j, 3, M2bstr) -
                0.5 * DiaRUfrc(i, j, inew, M2bstr);
            DiaRUfrc(i, j, istp, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
#endif
          }
        });

    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          if (j >= jstr_v) {
            for (int i = iv0; i <= iv1; ++i) {
              rvfrc(i, j) = rvfrc(i, j) - rhs_vbar(i, j);
              rhs_vbar(i, j) += 1.5 * rvfrc(i, j) - 0.5 * rv(i, j, 0, inew);
              rv(i, j, 0, istp) = rvfrc(i, j);
#if defined DIAGNOSTICS_UV
              for (int id = 1; id <= M2pgrd; ++id) {
                DiaRVfrc(i, j, 3, id) -= DiaV2rhs(i, j, id);
                DiaV2rhs(i, j, id) += 1.5 * DiaRVfrc(i, j, 3, id) -
                                      0.5 * DiaRVfrc(i, j, inew, id);
                DiaRVfrc(i, j, istp, id) = DiaRVfrc(i, j, 3, id);
              }
              DiaV2rhs(i, j, M2sstr) =
                  1.5 * DiaRVfrc(i, j, 3, M2sstr) -
                  0.5 * DiaRVfrc(i, j, inew, M2sstr);
              DiaRVfrc(i, j, istp, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
              DiaV2rhs(i, j, M2bstr) =
                  1.5 * DiaRVfrc(i, j, 3, M2bstr) -
                  0.5 * DiaRVfrc(i, j, inew, M2bstr);
              DiaRVfrc(i, j, istp, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
#endif
            }
          }
        });
  } else if (first) {
    /*
    ** Third and later barotropic predictor steps inside the first 3-D
    ** step: the correction is extrapolated from the last three values.
    ** Dropping this branch (folding it into the plain "add rufrc" case)
    ** leaves the first two fast steps exact and then diverges.
    */

    constexpr double cff1 = 23.0 / 12.0;
    constexpr double cff2 = 16.0 / 12.0;
    constexpr double cff3 = 5.0 / 12.0;

    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          for (int i = iu0; i <= iu1; ++i) {
            rufrc(i, j) = rufrc(i, j) - rhs_ubar(i, j);
            rhs_ubar(i, j) += cff1 * rufrc(i, j) -
                              cff2 * ru(i, j, 0, inew) +
                              cff3 * ru(i, j, 0, istp);
            ru(i, j, 0, istp) = rufrc(i, j);
#if defined DIAGNOSTICS_UV
            for (int id = 1; id <= M2pgrd; ++id) {
              DiaRUfrc(i, j, 3, id) -= DiaU2rhs(i, j, id);
              DiaU2rhs(i, j, id) += cff1 * DiaRUfrc(i, j, 3, id) -
                                    cff2 * DiaRUfrc(i, j, inew, id) +
                                    cff3 * DiaRUfrc(i, j, istp, id);
              DiaRUfrc(i, j, istp, id) = DiaRUfrc(i, j, 3, id);
            }
            DiaU2rhs(i, j, M2sstr) =
                cff1 * DiaRUfrc(i, j, 3, M2sstr) -
                cff2 * DiaRUfrc(i, j, inew, M2sstr) +
                cff3 * DiaRUfrc(i, j, istp, M2sstr);
            DiaRUfrc(i, j, istp, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
            DiaU2rhs(i, j, M2bstr) =
                cff1 * DiaRUfrc(i, j, 3, M2bstr) -
                cff2 * DiaRUfrc(i, j, inew, M2bstr) +
                cff3 * DiaRUfrc(i, j, istp, M2bstr);
            DiaRUfrc(i, j, istp, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
#endif
          }
        });

    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          if (j >= jstr_v) {
            for (int i = iv0; i <= iv1; ++i) {
              rvfrc(i, j) = rvfrc(i, j) - rhs_vbar(i, j);
              rhs_vbar(i, j) += cff1 * rvfrc(i, j) -
                                cff2 * rv(i, j, 0, inew) +
                                cff3 * rv(i, j, 0, istp);
              rv(i, j, 0, istp) = rvfrc(i, j);
#if defined DIAGNOSTICS_UV
              for (int id = 1; id <= M2pgrd; ++id) {
                DiaRVfrc(i, j, 3, id) -= DiaV2rhs(i, j, id);
                DiaV2rhs(i, j, id) += cff1 * DiaRVfrc(i, j, 3, id) -
                                      cff2 * DiaRVfrc(i, j, inew, id) +
                                      cff3 * DiaRVfrc(i, j, istp, id);
                DiaRVfrc(i, j, istp, id) = DiaRVfrc(i, j, 3, id);
              }
              DiaV2rhs(i, j, M2sstr) =
                  cff1 * DiaRVfrc(i, j, 3, M2sstr) -
                  cff2 * DiaRVfrc(i, j, inew, M2sstr) +
                  cff3 * DiaRVfrc(i, j, istp, M2sstr);
              DiaRVfrc(i, j, istp, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
              DiaV2rhs(i, j, M2bstr) =
                  cff1 * DiaRVfrc(i, j, 3, M2bstr) -
                  cff2 * DiaRVfrc(i, j, inew, M2bstr) +
                  cff3 * DiaRVfrc(i, j, istp, M2bstr);
              DiaRVfrc(i, j, istp, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
#endif
            }
          }
        });
  } else {
    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          for (int i = iu0; i <= iu1; ++i) {
            rhs_ubar(i, j) += rufrc(i, j);
#if defined DIAGNOSTICS_UV
            for (int id = 1; id <= M2pgrd; ++id) {
              DiaU2rhs(i, j, id) += DiaRUfrc(i, j, 3, id);
            }
            DiaU2rhs(i, j, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
            DiaU2rhs(i, j, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
#endif
          }
        });

    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          if (j >= jstr_v) {
            for (int i = iv0; i <= iv1; ++i) {
              rhs_vbar(i, j) += rvfrc(i, j);
#if defined DIAGNOSTICS_UV
              for (int id = 1; id <= M2pgrd; ++id) {
                DiaV2rhs(i, j, id) += DiaRVfrc(i, j, 3, id);
              }
              DiaV2rhs(i, j, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
              DiaV2rhs(i, j, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
#endif
            }
          }
        });
  }
}

/*======================================================================
** Stage 17 -- time step of the 2-D momentum equations.
** Source: step2d_LF_AM3.h lines 2572-2755.
**
**   Dstp(i,j) = zeta(i,j,kstp) + h(i,j)
**   ubar(:,:,knew) = (ubar(:,:,kstp)*(Dstp(i,j)+Dstp(i-1,j))
**                     + pm*pn*dtfast*rhs_ubar) / (Dnew(i,j)+Dnew(i-1,j))
**
** Forward Euler, Leap-frog and Adams-Moulton differ only in the
** constant on rhs_ubar (and, for AM3, the extra rubar history terms).
**====================================================================*/

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
                            const TileBounds* t) {
  (void)ng;
  (void)tile;
  (void)predictor;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

  Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> rubar = roms::as_3d<double>(rubar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);
  Array3d<double> rvbar = roms::as_3d<double>(rvbar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);

  Array2d<double> h  = grid2d(h_p, s);
  Array2d<double> pm = grid2d(pm_p, s);
  Array2d<double> pn = grid2d(pn_p, s);

  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);
  Array2d<double> Dstp     = priv2d(Dstp_p, s);
  Array2d<double> Dnew     = priv2d(Dnew_p, s);

  const int ptsk = 3 - kstp;

  const int j0 = t->JstrV - 1, j1 = t->Jend;
  const int i0 = t->IstrU - 1, i1 = t->Iend;

  Kokkos::parallel_for(
      range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
        for (int i = i0; i <= i1; ++i) {
          Dstp(i, j) = zeta(i, j, kstp) + h(i, j);
        }
      });

  const int iu0 = t->IstrU, iu1 = t->Iend;
  const int iv0 = t->Istr,  iv1 = t->Iend;
  const int jv0 = t->Jstr,  jv1 = t->Jend;
  const int jstr_v = t->JstrV;

  if (first_2d_step) {
    const double cff1 = 0.5 * dtfast;

    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          for (int i = iu0; i <= iu1; ++i) {
            const double cff =
                (pm(i, j) + pm(i - 1, j)) * (pn(i, j) + pn(i - 1, j));
            const double div = 1.0 / (Dnew(i, j) + Dnew(i - 1, j));
            ubar(i, j, knew) =
                (ubar(i, j, kstp) * (Dstp(i, j) + Dstp(i - 1, j)) +
                 cff * cff1 * rhs_ubar(i, j)) *
                div;
          }
        });

    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          if (j >= jstr_v) {
            for (int i = iv0; i <= iv1; ++i) {
              const double cff =
                  (pm(i, j) + pm(i, j - 1)) * (pn(i, j) + pn(i, j - 1));
              const double div = 1.0 / (Dnew(i, j) + Dnew(i, j - 1));
              vbar(i, j, knew) =
                  (vbar(i, j, kstp) * (Dstp(i, j) + Dstp(i, j - 1)) +
                   cff * cff1 * rhs_vbar(i, j)) *
                  div;
            }
          }
        });
  } else if (predictor) {
    const double cff1 = dtfast;

    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          for (int i = iu0; i <= iu1; ++i) {
            const double cff =
                (pm(i, j) + pm(i - 1, j)) * (pn(i, j) + pn(i - 1, j));
            const double div = 1.0 / (Dnew(i, j) + Dnew(i - 1, j));
            ubar(i, j, knew) =
                (ubar(i, j, kstp) * (Dstp(i, j) + Dstp(i - 1, j)) +
                 cff * cff1 * rhs_ubar(i, j)) *
                div;
          }
        });

    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          if (j >= jstr_v) {
            for (int i = iv0; i <= iv1; ++i) {
              const double cff =
                  (pm(i, j) + pm(i, j - 1)) * (pn(i, j) + pn(i, j - 1));
              const double div = 1.0 / (Dnew(i, j) + Dnew(i, j - 1));
              vbar(i, j, knew) =
                  (vbar(i, j, kstp) * (Dstp(i, j) + Dstp(i, j - 1)) +
                   cff * cff1 * rhs_vbar(i, j)) *
                  div;
            }
          }
        });
  } else {
    const double cff1 = 0.5 * dtfast * 5.0 / 12.0;
    const double cff2 = 0.5 * dtfast * 8.0 / 12.0;
    const double cff3 = 0.5 * dtfast * 1.0 / 12.0;

    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          for (int i = iu0; i <= iu1; ++i) {
            const double cff =
                (pm(i, j) + pm(i - 1, j)) * (pn(i, j) + pn(i - 1, j));
            const double div = 1.0 / (Dnew(i, j) + Dnew(i - 1, j));
            ubar(i, j, knew) =
                (ubar(i, j, kstp) * (Dstp(i, j) + Dstp(i - 1, j)) +
                 cff * (cff1 * rhs_ubar(i, j) +
                        cff2 * rubar(i, j, kstp) -
                        cff3 * rubar(i, j, ptsk))) *
                div;
          }
        });

    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
          if (j >= jstr_v) {
            for (int i = iv0; i <= iv1; ++i) {
              const double cff =
                  (pm(i, j) + pm(i, j - 1)) * (pn(i, j) + pn(i, j - 1));
              const double div = 1.0 / (Dnew(i, j) + Dnew(i, j - 1));
              vbar(i, j, knew) =
                  (vbar(i, j, kstp) * (Dstp(i, j) + Dstp(i, j - 1)) +
                   cff * (cff1 * rhs_vbar(i, j) +
                          cff2 * rvbar(i, j, kstp) -
                          cff3 * rvbar(i, j, ptsk))) *
                  div;
            }
          }
        });
  }
}

/*======================================================================
** Stage 18 -- store the predictor right-hand side for the corrector.
** Source: step2d_LF_AM3.h lines 2920-2945.
**
**   rubar(i,j,krhs) = rhs_ubar(i,j)
**   rvbar(i,j,krhs) = rhs_vbar(i,j)
**====================================================================*/

void k_step2d_store_rhs(int ng, int tile, int krhs,
                        int LBi, int UBi, int LBj, int UBj,
                        int IminS, int ImaxS, int JminS, int JmaxS,
                        void* rubar_p, void* rvbar_p,
                        void* rhs_ubar_p, void* rhs_vbar_p,
                        void* DiaRUbar_p, void* DiaRVbar_p,
                        void* DiaU2rhs_p, void* DiaV2rhs_p,
                        int NDM2d, int M2pgrd,
                        const TileBounds* t) {
  (void)ng;
  (void)tile;
  (void)NDM2d;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

  Array3d<double> rubar = roms::as_3d<double>(rubar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);
  Array3d<double> rvbar = roms::as_3d<double>(rvbar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);

  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);

#if defined DIAGNOSTICS_UV
  Array4d<double> DiaRUbar =
      grid4d(DiaRUbar_p, s, 1, 2, 1, NDM2d - 1);
  Array4d<double> DiaRVbar =
      grid4d(DiaRVbar_p, s, 1, 2, 1, NDM2d - 1);
  Array3d<double> DiaU2rhs =
      priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs =
      priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
#else
  (void)DiaRUbar_p;
  (void)DiaRVbar_p;
  (void)DiaU2rhs_p;
  (void)DiaV2rhs_p;
  (void)M2pgrd;
#endif

  const int iu0 = t->IstrU, iu1 = t->Iend;
  const int iv0 = t->Istr,  iv1 = t->Iend;
  const int jv0 = t->Jstr,  jv1 = t->Jend;
  const int jstr_v = t->JstrV;

  Kokkos::parallel_for(
      range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
        for (int i = iu0; i <= iu1; ++i) {
          rubar(i, j, krhs) = rhs_ubar(i, j);
#if defined DIAGNOSTICS_UV
          for (int id = 1; id <= M2pgrd; ++id) {
            DiaRUbar(i, j, krhs, id) = DiaU2rhs(i, j, id);
          }
#endif
        }
      });

  Kokkos::parallel_for(
      range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
        if (j >= jstr_v) {
          for (int i = iv0; i <= iv1; ++i) {
            rvbar(i, j, krhs) = rhs_vbar(i, j);
#if defined DIAGNOSTICS_UV
            for (int id = 1; id <= M2pgrd; ++id) {
              DiaRVbar(i, j, krhs, id) = DiaV2rhs(i, j, id);
            }
#endif
          }
        }
      });
}

}  // extern "C"

#endif  // ROMS_KOKKOS
