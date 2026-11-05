/*
** Kokkos ports of the pure computation in the 3-D right-hand-side
** helpers.  Each entry replaces one self-contained block of Fortran
** loops; halo exchanges and open-boundary code stay where they are.
**
** Indexing notes
** --------------
**   * uv3dmix2_s computes the stress-tensor fluxes at rho and psi
**     points into tile scratch, then updates.  Here one work item owns a
**     row j, loops over k and carries the xi-flux along i; the flux a
**     neighbour row would have stored is recomputed from the same inputs,
**     which is the same expression (not a re-read of a race-prone value).
**   * t3dmix2_s is the same pattern per tracer.
**   * prsgrd32 has a column recursion (P) followed by two face passes.
**     The column pass keeps dR/dZ in a reusable workspace; the face
**     passes carry dZx/dRx along the face direction.
**   * omega and set_zeta are column/row local.
**
** The Fortran expressions are copied in the same order and grouping, so
** the results are bitwise identical on a serial backend.  Every stage is
** compiled only under its own flag; an unsupported macro combination is
** a compile error rather than a silent fallback.  The mixing and pressure
** gradient stages require the diagnostics their Fortran callers pass.
*/

#include "kokkos_rhs3d.hpp"

#if defined ROMS_KOKKOS

#include <cmath>

#ifndef __sw_slave__
#include <memory>
#endif

namespace {

using roms::Array2d;
using roms::Array3d;
using roms::Array4d;
using roms::Array5d;
using roms::TileBounds;
using exec_space = roms::exec_space;
using range_policy = Kokkos::RangePolicy<exec_space, Kokkos::IndexType<int>>;

#ifndef __sw_slave__
/* Per-thread scratch for the prsgrd column recursion.  Raw ownership, no
** Kokkos service at teardown; the SW64 HostSpace is the same memory. */
struct Workspace {
  std::unique_ptr<double[]> storage;
  std::size_t capacity = 0;
  double* reserve(std::size_t count) {
    if (count > capacity) {
      storage.reset(new double[count]);
      capacity = count;
    }
    return storage.get();
  }
};
thread_local Workspace workspace;
#endif

}  // namespace

/*======================================================================
** uv3dmix2_s.h -- harmonic stress tensor along S-surfaces.
**====================================================================*/

#if defined KK_STAGE_UV3DMIX

#if defined MASKING || defined WET_DRY || defined VISC_3DCOEF
#  error "Kokkos uv3dmix2 supports the constant-coefficient, unmasked case"
#endif
#if !defined DIAGNOSTICS_UV
#  error "Kokkos uv3dmix2 requires DIAGNOSTICS_UV"
#endif

namespace {

struct UvMix {
  Array3d<double> Hz;
  Array2d<double> om_p, om_r, on_p, on_r;
  Array2d<double> pm, pmon_p, pmon_r, pn, pnom_p, pnom_r;
  Array2d<double> visc2_p, visc2_r;
  Array4d<double> u, v;
  int nrhs, nnew;

  /* XI-component of the stress-tensor divergence at rho points, times
  ** on_r^2*visc2_r.  VFe is the same product with om_r^2. */
  KOKKOS_INLINE_FUNCTION
  double rfflux(int i, int j, int k) const {
    return Hz(i, j, k) * 0.5 *
           (pmon_r(i, j) *
                ((pn(i, j) + pn(i + 1, j)) * u(i + 1, j, k, nrhs) -
                 (pn(i - 1, j) + pn(i, j)) * u(i, j, k, nrhs)) -
            pnom_r(i, j) *
                ((pm(i, j) + pm(i, j + 1)) * v(i, j + 1, k, nrhs) -
                 (pm(i, j - 1) + pm(i, j)) * v(i, j, k, nrhs)));
  }

  KOKKOS_INLINE_FUNCTION
  double ufx(int i, int j, int k) const {
    return on_r(i, j) * on_r(i, j) * visc2_r(i, j) * rfflux(i, j, k);
  }

  KOKKOS_INLINE_FUNCTION
  double vfe(int i, int j, int k) const {
    return om_r(i, j) * om_r(i, j) * visc2_r(i, j) * rfflux(i, j, k);
  }

  /* Stress-tensor divergence at psi points. */
  KOKKOS_INLINE_FUNCTION
  double psiflux(int i, int j, int k) const {
    return 0.125 *
           (Hz(i - 1, j, k) + Hz(i, j, k) + Hz(i - 1, j - 1, k) +
            Hz(i, j - 1, k)) *
           (pmon_p(i, j) *
                ((pn(i, j - 1) + pn(i, j)) * v(i, j, k, nrhs) -
                 (pn(i - 1, j - 1) + pn(i - 1, j)) * v(i - 1, j, k, nrhs)) +
            pnom_p(i, j) *
                ((pm(i - 1, j) + pm(i, j)) * u(i, j, k, nrhs) -
                 (pm(i - 1, j - 1) + pm(i, j - 1)) * u(i, j - 1, k, nrhs)));
  }

  KOKKOS_INLINE_FUNCTION
  double ufe(int i, int j, int k) const {
    return om_p(i, j) * om_p(i, j) * visc2_p(i, j) * psiflux(i, j, k);
  }

  KOKKOS_INLINE_FUNCTION
  double vfx(int i, int j, int k) const {
    return on_p(i, j) * on_p(i, j) * visc2_p(i, j) * psiflux(i, j, k);
  }
};

}  // namespace

extern "C" void k_uv3dmix2_s(int ng, int tile, int nrhs, int nnew,
                             int LBi, int UBi, int LBj, int UBj,
                             int IminS, int ImaxS, int JminS, int JmaxS,
                             void* Hz_p,
                             void* om_p_p, void* om_r_p,
                             void* on_p_p, void* on_r_p,
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
                             double dt, const TileBounds* t) {
  roms::initialize();
  ROMS_KK_TIMER("rhs3d uv3dmix");
  (void)ng;
  (void)tile;

  const UvMix f{roms::as_3d<double>(Hz_p, LBi, UBi, LBj, UBj, 1, NLev),
                roms::as_2d<double>(om_p_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(om_r_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(on_p_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(on_r_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(pm_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(pmon_p_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(pmon_r_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(pn_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(pnom_p_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(pnom_r_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(visc2_p_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(visc2_r_p, LBi, UBi, LBj, UBj),
                roms::as_4d<double>(u_p, LBi, UBi, LBj, UBj, 1, NLev, 1, 2),
                roms::as_4d<double>(v_p, LBi, UBi, LBj, UBj, 1, NLev, 1, 2),
                nrhs, nnew};

  Array2d<double> rufrc = roms::as_2d<double>(rufrc_p, LBi, UBi, LBj, UBj);
  Array2d<double> rvfrc = roms::as_2d<double>(rvfrc_p, LBi, UBi, LBj, UBj);

  Array4d<double> DiaRUfrc = roms::as_4d<double>(
      DiaRUfrc_p, LBi, UBi, LBj, UBj, 1, 3, 1, NDM2d - 1);
  Array4d<double> DiaRVfrc = roms::as_4d<double>(
      DiaRVfrc_p, LBi, UBi, LBj, UBj, 1, 3, 1, NDM2d - 1);
  Array4d<double> DiaU3wrk = roms::as_4d<double>(
      DiaU3wrk_p, LBi, UBi, LBj, UBj, 1, NLev, 1, NDM3d);
  Array4d<double> DiaV3wrk = roms::as_4d<double>(
      DiaV3wrk_p, LBi, UBi, LBj, UBj, 1, NLev, 1, NDM3d);

  const int j0 = t->JstrV - 1, j1 = t->Jend;
  const int iu0 = t->IstrU, iv0 = t->Istr, i1 = t->Iend;
  const int Jstr = t->Jstr, JstrV = t->JstrV;

  /*
  ** One work item per row, all levels inside.  rufrc/rvfrc (and their
  ** barotropic diagnostics) are accumulated over k, so splitting k
  ** across work items would race on them and would also reorder the
  ** sum.  Levels are therefore visited in the Fortran order.
  */
  Kokkos::parallel_for(
      range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
       for (int k = 1; k <= NLev; ++k) {
        if (j >= Jstr) {
          double fprev = f.ufx(iu0 - 1, j, k);
          for (int i = iu0; i <= i1; ++i) {
            const double fc = f.ufx(i, j, k);
            const double cff = dt * 0.25 * (f.pm(i - 1, j) + f.pm(i, j)) *
                               (f.pn(i - 1, j) + f.pn(i, j));
            const double cff1 =
                0.5 * (f.pn(i - 1, j) + f.pn(i, j)) * (fc - fprev);
            const double cff2 =
                0.5 * (f.pm(i - 1, j) + f.pm(i, j)) *
                (f.ufe(i, j + 1, k) - f.ufe(i, j, k));
            const double cff3 = cff * (cff1 + cff2);
            rufrc(i, j) = rufrc(i, j) + cff1 + cff2;
            f.u(i, j, k, nnew) = f.u(i, j, k, nnew) + cff3;
            DiaRUfrc(i, j, 3, M2hvis) =
                DiaRUfrc(i, j, 3, M2hvis) + cff1 + cff2;
            DiaRUfrc(i, j, 3, M2xvis) =
                DiaRUfrc(i, j, 3, M2xvis) + cff1;
            DiaRUfrc(i, j, 3, M2yvis) =
                DiaRUfrc(i, j, 3, M2yvis) + cff2;
            DiaU3wrk(i, j, k, M3hvis) = cff3;
            DiaU3wrk(i, j, k, M3xvis) = cff * cff1;
            DiaU3wrk(i, j, k, M3yvis) = cff * cff2;
            fprev = fc;
          }
        }

        if (j >= JstrV) {
          double fnext = f.vfx(i1 + 1, j, k);
          for (int i = i1; i >= iv0; --i) {
            const double fc = f.vfx(i, j, k);
            const double cff = dt * 0.25 * (f.pm(i, j) + f.pm(i, j - 1)) *
                               (f.pn(i, j) + f.pn(i, j - 1));
            const double cff1 =
                0.5 * (f.pn(i, j - 1) + f.pn(i, j)) * (fnext - fc);
            const double cff2 =
                0.5 * (f.pm(i, j - 1) + f.pm(i, j)) *
                (f.vfe(i, j, k) - f.vfe(i, j - 1, k));
            const double cff3 = cff * (cff1 - cff2);
            rvfrc(i, j) = rvfrc(i, j) + cff1 - cff2;
            f.v(i, j, k, nnew) = f.v(i, j, k, nnew) + cff3;
            DiaRVfrc(i, j, 3, M2hvis) =
                DiaRVfrc(i, j, 3, M2hvis) + cff1 - cff2;
            DiaRVfrc(i, j, 3, M2xvis) =
                DiaRVfrc(i, j, 3, M2xvis) + cff1;
            DiaRVfrc(i, j, 3, M2yvis) =
                DiaRVfrc(i, j, 3, M2yvis) - cff2;
            DiaV3wrk(i, j, k, M3hvis) = cff3;
            DiaV3wrk(i, j, k, M3xvis) = cff * cff1;
            DiaV3wrk(i, j, k, M3yvis) = -cff * cff2;
            fnext = fc;
          }
        }
       }
      });
}

#endif  // KK_STAGE_UV3DMIX

/*======================================================================
** t3dmix2_s.h -- harmonic mixing of tracers along S-surfaces.
**====================================================================*/

#if defined KK_STAGE_T3DMIX

#if defined MASKING || defined WET_DRY || defined DIFF_3DCOEF || \
    defined TS_MIX_STABILITY || defined TS_MIX_CLIMA
#  error "Kokkos t3dmix2 supports the constant-coefficient, unmasked case"
#endif
#if !defined DIAGNOSTICS_TS
#  error "Kokkos t3dmix2 requires DIAGNOSTICS_TS"
#endif

namespace {

struct T3Mix {
  Array3d<double> diff2;
  Array3d<double> Hz;
  Array2d<double> pmon_u, pnom_v, pm, pn;
  Array5d<double> t;
  int nrhs;

  KOKKOS_INLINE_FUNCTION
  double fx(int i, int j, int k, int itrc) const {
    const double cff = 0.25 * (diff2(i, j, itrc) + diff2(i - 1, j, itrc)) *
                       pmon_u(i, j);
    return cff * (Hz(i, j, k) + Hz(i - 1, j, k)) *
           (t(i, j, k, nrhs, itrc) - t(i - 1, j, k, nrhs, itrc));
  }

  KOKKOS_INLINE_FUNCTION
  double fe(int i, int j, int k, int itrc) const {
    const double cff = 0.25 * (diff2(i, j, itrc) + diff2(i, j - 1, itrc)) *
                       pnom_v(i, j);
    return cff * (Hz(i, j, k) + Hz(i, j - 1, k)) *
           (t(i, j, k, nrhs, itrc) - t(i, j - 1, k, nrhs, itrc));
  }
};

}  // namespace

extern "C" void k_t3dmix2_s(int ng, int tile, int nrhs, int nstp, int nnew,
                            int LBi, int UBi, int LBj, int UBj,
                            int IminS, int ImaxS, int JminS, int JmaxS,
                            void* diff2_p, void* Hz_p,
                            void* pmon_u_p, void* pnom_v_p,
                            void* pm_p, void* pn_p,
                            void* DiaTwrk_p, void* t_p,
                            int NLev, int NT, int NDT,
                            int iTxdif, int iTydif, int iThdif,
                            double dt, const TileBounds* t) {
  roms::initialize();
  ROMS_KK_TIMER("rhs3d t3dmix");
  (void)ng;
  (void)tile;
  (void)nstp;

  const T3Mix f{roms::as_3d<double>(diff2_p, LBi, UBi, LBj, UBj, 1, NT),
                roms::as_3d<double>(Hz_p, LBi, UBi, LBj, UBj, 1, NLev),
                roms::as_2d<double>(pmon_u_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(pnom_v_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(pm_p, LBi, UBi, LBj, UBj),
                roms::as_2d<double>(pn_p, LBi, UBi, LBj, UBj),
                roms::as_5d<double>(t_p, LBi, UBi, LBj, UBj, 1, NLev, 1, 3,
                                    1, NT),
                nrhs};

  Array5d<double> DiaTwrk = roms::as_5d<double>(
      DiaTwrk_p, LBi, UBi, LBj, UBj, 1, NLev, 1, NT, 1, NDT);

  const int Jstr = t->Jstr, Jend = t->Jend;
  const int Istr = t->Istr, Iend = t->Iend;
  const int nrows = Jend - Jstr + 1;

  Kokkos::parallel_for(
      range_policy(0, nrows * NLev * NT), KOKKOS_LAMBDA(int idx) {
        const int j = Jstr + idx % nrows;
        const int rest = idx / nrows;
        const int k = 1 + rest % NLev;
        const int itrc = 1 + rest / NLev;

        double fprev = f.fx(Istr, j, k, itrc);
        for (int i = Istr; i <= Iend; ++i) {
          const double fc = f.fx(i + 1, j, k, itrc);
          const double cff = dt * f.pm(i, j) * f.pn(i, j);
          const double cff1 = cff * (fc - fprev);
          const double cff2 =
              cff * (f.fe(i, j + 1, k, itrc) - f.fe(i, j, k, itrc));
          const double cff3 = cff1 + cff2;
          f.t(i, j, k, nnew, itrc) = f.t(i, j, k, nnew, itrc) + cff3;
          DiaTwrk(i, j, k, itrc, iTxdif) = cff1;
          DiaTwrk(i, j, k, itrc, iTydif) = cff2;
          DiaTwrk(i, j, k, itrc, iThdif) = cff3;
          fprev = fc;
        }
      });
}

#endif  // KK_STAGE_T3DMIX

/*======================================================================
** prsgrd32.h -- baroclinic pressure gradient (DJ_GRADPS).
**====================================================================*/

#if defined KK_STAGE_PRSGRD

#if defined MASKING || defined WET_DRY || defined WEC_VF || \
    defined TIDE_GENERATING_FORCES || defined ATM_PRESS
#  error "Kokkos prsgrd32 supports the unmasked DJ_GRADPS case"
#endif
#if !defined DIAGNOSTICS_UV
#  error "Kokkos prsgrd32 requires DIAGNOSTICS_UV"
#endif

namespace {

constexpr double OneFifth = 0.2;
constexpr double OneTwelfth = 1.0 / 12.0;
constexpr double eps = 1.0e-10;

struct Pgrd {
  Array2d<double> on_u, om_v;
  Array3d<double> Hz, z_r, z_w, rho, P;
  Array4d<double> ru, rv;
  double g, rho0;
};

}  // namespace

extern "C" void k_prsgrd32(int ng, int tile, int nrhs,
                           int LBi, int UBi, int LBj, int UBj,
                           int IminS, int ImaxS, int JminS, int JmaxS,
                           void* om_v_p, void* on_u_p, void* Hz_p,
                           void* z_r_p, void* z_w_p, void* rho_p, void* P_p,
                           void* ru_p, void* rv_p,
                           void* DiaRU_p, void* DiaRV_p,
                           int NLev, int NDrhs, int M3pgrd,
                           double g, double rho0, const TileBounds* tb) {
  roms::initialize();
  ROMS_KK_TIMER("rhs3d prsgrd");
  (void)ng;
  (void)tile;

  const Pgrd f{roms::as_2d<double>(on_u_p, LBi, UBi, LBj, UBj),
               roms::as_2d<double>(om_v_p, LBi, UBi, LBj, UBj),
               roms::as_3d<double>(Hz_p, LBi, UBi, LBj, UBj, 1, NLev),
               roms::as_3d<double>(z_r_p, LBi, UBi, LBj, UBj, 1, NLev),
               roms::as_3d<double>(z_w_p, LBi, UBi, LBj, UBj, 0, NLev),
               roms::as_3d<double>(rho_p, LBi, UBi, LBj, UBj, 1, NLev),
               roms::as_3d<double>(P_p, IminS, ImaxS, JminS, JmaxS, 1, NLev),
               roms::as_4d<double>(ru_p, LBi, UBi, LBj, UBj, 0, NLev, 1, 2),
               roms::as_4d<double>(rv_p, LBi, UBi, LBj, UBj, 0, NLev, 1, 2),
               g,
               rho0};

  Array5d<double> DiaRU = roms::as_5d<double>(
      DiaRU_p, LBi, UBi, LBj, UBj, 1, NLev, 1, 2, 1, NDrhs);
  Array5d<double> DiaRV = roms::as_5d<double>(
      DiaRV_p, LBi, UBi, LBj, UBj, 1, NLev, 1, 2, 1, NDrhs);

  const double GRho = g / rho0;
  const double HalfGRho = 0.5 * GRho;

  /* ---- kinematic pressure, one recursion per column ---- */

  const int ip0 = tb->IstrU - 1, ip1 = tb->Iend;
  const int jp0 = tb->JstrV - 1, jp1 = tb->Jend;
  const int niP = ip1 - ip0 + 1;
  const int njP = jp1 - jp0 + 1;

  double* scratch = nullptr;
#ifndef __sw_slave__
  scratch = workspace.reserve(std::size_t(niP) * njP * 2 * (NLev + 1));
#endif

  Kokkos::parallel_for(
      range_policy(0, niP * njP), KOKKOS_LAMBDA(int idx) {
        const int j = jp0 + idx / niP;
        const int i = ip0 + idx % niP;
        double* dR = scratch + std::size_t(idx) * 2 * (NLev + 1);
        double* dZ = dR + (NLev + 1);

        for (int k = 1; k <= NLev - 1; ++k) {
          dR[k] = f.rho(i, j, k + 1) - f.rho(i, j, k);
          dZ[k] = f.z_r(i, j, k + 1) - f.z_r(i, j, k);
        }
        dR[NLev] = dR[NLev - 1];
        dZ[NLev] = dZ[NLev - 1];
        dR[0] = dR[1];
        dZ[0] = dZ[1];
        for (int k = NLev; k >= 1; --k) {
          const double cff = 2.0 * dR[k] * dR[k - 1];
          if (cff > eps) {
            dR[k] = cff / (dR[k] + dR[k - 1]);
          } else {
            dR[k] = 0.0;
          }
          dZ[k] = 2.0 * dZ[k] * dZ[k - 1] / (dZ[k] + dZ[k - 1]);
        }

        const double cff1 =
            1.0 / (f.z_r(i, j, NLev) - f.z_r(i, j, NLev - 1));
        const double cff2 = 0.5 * (f.rho(i, j, NLev) - f.rho(i, j, NLev - 1)) *
                            (f.z_w(i, j, NLev) - f.z_r(i, j, NLev)) * cff1;
        f.P(i, j, NLev) =
            g * f.z_w(i, j, NLev) +
            GRho * (f.rho(i, j, NLev) + cff2) *
                (f.z_w(i, j, NLev) - f.z_r(i, j, NLev));

        for (int k = NLev - 1; k >= 1; --k) {
          f.P(i, j, k) =
              f.P(i, j, k + 1) +
              HalfGRho *
                  ((f.rho(i, j, k + 1) + f.rho(i, j, k)) *
                       (f.z_r(i, j, k + 1) - f.z_r(i, j, k)) -
                   OneFifth *
                       ((dR[k + 1] - dR[k]) *
                            (f.z_r(i, j, k + 1) - f.z_r(i, j, k) -
                             OneTwelfth * (dZ[k + 1] + dZ[k])) -
                        (dZ[k + 1] - dZ[k]) *
                            (f.rho(i, j, k + 1) - f.rho(i, j, k) -
                             OneTwelfth * (dR[k + 1] + dR[k]))));
        }
      });

  /* ---- XI-component ---- */

  const int Jstr = tb->Jstr, Jend = tb->Jend;
  const int Istr = tb->Istr, Iend = tb->Iend;
  const int IstrU = tb->IstrU;
  const int nrowsX = Jend - Jstr + 1;

  Kokkos::parallel_for(
      range_policy(0, nrowsX * NLev), KOKKOS_LAMBDA(int idx) {
        const int k = 1 + idx / nrowsX;
        const int j = Jstr + idx % nrowsX;

        /* dZx/dRx at IstrU-1, then carried along i. */
        double aux0 = f.z_r(IstrU - 1, j, k) - f.z_r(IstrU - 2, j, k);
        double fc0 = f.rho(IstrU - 1, j, k) - f.rho(IstrU - 2, j, k);
        double aux1 = f.z_r(IstrU, j, k) - f.z_r(IstrU - 1, j, k);
        double fc1 = f.rho(IstrU, j, k) - f.rho(IstrU - 1, j, k);
        double cff = 2.0 * aux0 * aux1;
        double dzx_prev = (cff > eps) ? cff / (aux0 + aux1) : 0.0;
        double cff1 = 2.0 * fc0 * fc1;
        double drx_prev = (cff1 > eps) ? cff1 / (fc0 + fc1) : 0.0;

        for (int i = IstrU; i <= Iend; ++i) {
          aux0 = aux1;
          fc0 = fc1;
          aux1 = f.z_r(i + 1, j, k) - f.z_r(i, j, k);
          fc1 = f.rho(i + 1, j, k) - f.rho(i, j, k);
          cff = 2.0 * aux0 * aux1;
          const double dzx = (cff > eps) ? cff / (aux0 + aux1) : 0.0;
          cff1 = 2.0 * fc0 * fc1;
          const double drx = (cff1 > eps) ? cff1 / (fc0 + fc1) : 0.0;

          const double val =
              f.on_u(i, j) * 0.5 * (f.Hz(i, j, k) + f.Hz(i - 1, j, k)) *
              (f.P(i - 1, j, k) - f.P(i, j, k) -
               HalfGRho *
                   ((f.rho(i, j, k) + f.rho(i - 1, j, k)) *
                        (f.z_r(i, j, k) - f.z_r(i - 1, j, k)) -
                    OneFifth *
                        ((drx - drx_prev) *
                             (f.z_r(i, j, k) - f.z_r(i - 1, j, k) -
                              OneTwelfth * (dzx + dzx_prev)) -
                         (dzx - dzx_prev) *
                             (f.rho(i, j, k) - f.rho(i - 1, j, k) -
                              OneTwelfth * (drx + drx_prev)))));
          f.ru(i, j, k, nrhs) = val;
          DiaRU(i, j, k, nrhs, M3pgrd) = val;
          dzx_prev = dzx;
          drx_prev = drx;
        }
      });

  /* ---- ETA-component ---- */

  const int JstrV = tb->JstrV;
  const int nrowsY = Iend - Istr + 1;

  Kokkos::parallel_for(
      range_policy(0, nrowsY * NLev), KOKKOS_LAMBDA(int idx) {
        const int k = 1 + idx / nrowsY;
        const int i = Istr + idx % nrowsY;

        double aux0 = f.z_r(i, JstrV - 1, k) - f.z_r(i, JstrV - 2, k);
        double fc0 = f.rho(i, JstrV - 1, k) - f.rho(i, JstrV - 2, k);
        double aux1 = f.z_r(i, JstrV, k) - f.z_r(i, JstrV - 1, k);
        double fc1 = f.rho(i, JstrV, k) - f.rho(i, JstrV - 1, k);
        double cff = 2.0 * aux0 * aux1;
        double dzx_prev = (cff > eps) ? cff / (aux0 + aux1) : 0.0;
        double cff1 = 2.0 * fc0 * fc1;
        double drx_prev = (cff1 > eps) ? cff1 / (fc0 + fc1) : 0.0;

        for (int j = JstrV; j <= Jend; ++j) {
          aux0 = aux1;
          fc0 = fc1;
          aux1 = f.z_r(i, j + 1, k) - f.z_r(i, j, k);
          fc1 = f.rho(i, j + 1, k) - f.rho(i, j, k);
          cff = 2.0 * aux0 * aux1;
          const double dzx = (cff > eps) ? cff / (aux0 + aux1) : 0.0;
          cff1 = 2.0 * fc0 * fc1;
          const double drx = (cff1 > eps) ? cff1 / (fc0 + fc1) : 0.0;

          const double val =
              f.om_v(i, j) * 0.5 * (f.Hz(i, j, k) + f.Hz(i, j - 1, k)) *
              (f.P(i, j - 1, k) - f.P(i, j, k) -
               HalfGRho *
                   ((f.rho(i, j, k) + f.rho(i, j - 1, k)) *
                        (f.z_r(i, j, k) - f.z_r(i, j - 1, k)) -
                    OneFifth *
                        ((drx - drx_prev) *
                             (f.z_r(i, j, k) - f.z_r(i, j - 1, k) -
                              OneTwelfth * (dzx + dzx_prev)) -
                         (dzx - dzx_prev) *
                             (f.rho(i, j, k) - f.rho(i, j - 1, k) -
                              OneTwelfth * (drx + drx_prev)))));
          f.rv(i, j, k, nrhs) = val;
          DiaRV(i, j, k, nrhs, M3pgrd) = val;
          dzx_prev = dzx;
          drx_prev = drx;
        }
      });
}

#endif  // KK_STAGE_PRSGRD

/*======================================================================
** omega.F -- omega vertical velocity, no point sources.
**====================================================================*/

#if defined KK_STAGE_OMEGA

#if defined SEDIMENT || defined WEC_VF || defined OMEGA_IMPLICIT
#  error "Kokkos omega supports the plain, source-free configuration"
#endif

extern "C" void k_omega3d(int ng, int tile,
                          int LBi, int UBi, int LBj, int UBj,
                          int IminS, int ImaxS, int JminS, int JmaxS,
                          void* Huon_p, void* Hvom_p, void* z_w_p,
                          void* W_p,
                          int NLev, const TileBounds* t) {
  roms::initialize();
  ROMS_KK_TIMER("rhs3d omega");
  (void)ng;
  (void)tile;
  (void)IminS;
  (void)ImaxS;
  (void)JminS;
  (void)JmaxS;

  Array3d<double> Huon =
      roms::as_3d<double>(Huon_p, LBi, UBi, LBj, UBj, 1, NLev);
  Array3d<double> Hvom =
      roms::as_3d<double>(Hvom_p, LBi, UBi, LBj, UBj, 1, NLev);
  Array3d<double> z_w =
      roms::as_3d<double>(z_w_p, LBi, UBi, LBj, UBj, 0, NLev);
  Array3d<double> W = roms::as_3d<double>(W_p, LBi, UBi, LBj, UBj, 0, NLev);

  const int Istr = t->Istr, Iend = t->Iend;
  const int Jstr = t->Jstr, Jend = t->Jend;
  const int ncols = Iend - Istr + 1;

  Kokkos::parallel_for(
      range_policy(0, ncols * (Jend - Jstr + 1)), KOKKOS_LAMBDA(int idx) {
        const int j = Jstr + idx / ncols;
        const int i = Istr + idx % ncols;

        W(i, j, 0) = 0.0;
        for (int k = 1; k <= NLev; ++k) {
          W(i, j, k) = W(i, j, k - 1) -
                       (Huon(i + 1, j, k) - Huon(i, j, k) +
                        Hvom(i, j + 1, k) - Hvom(i, j, k));
        }
        const double wrk =
            W(i, j, NLev) / (z_w(i, j, NLev) - z_w(i, j, 0));
        for (int k = NLev - 1; k >= 1; --k) {
          W(i, j, k) = W(i, j, k) - wrk * (z_w(i, j, k) - z_w(i, j, 0));
        }
        W(i, j, NLev) = 0.0;
      });
}

#endif  // KK_STAGE_OMEGA

/*======================================================================
** set_zeta.F -- reload the free surface from the fast-time average.
**====================================================================*/

#if defined KK_STAGE_SETZETA

extern "C" void k_set_zeta(int ng, int tile,
                           int LBi, int UBi, int LBj, int UBj,
                           int IminS, int ImaxS, int JminS, int JmaxS,
                           void* Zt_avg1_p, void* zeta_p,
                           const TileBounds* t) {
  roms::initialize();
  ROMS_KK_TIMER("rhs3d set_zeta");
  (void)ng;
  (void)tile;

  Array2d<double> Zt_avg1 =
      roms::as_2d<double>(Zt_avg1_p, LBi, UBi, LBj, UBj);
  Array3d<double> zeta =
      roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, 1, 3);

  const int IstrR = t->IstrR, IendR = t->IendR;
  const int JstrR = t->JstrR, JendR = t->JendR;
  const int ncols = IendR - IstrR + 1;

  Kokkos::parallel_for(
      range_policy(0, ncols * (JendR - JstrR + 1)), KOKKOS_LAMBDA(int idx) {
        const int j = JstrR + idx / ncols;
        const int i = IstrR + idx % ncols;
        zeta(i, j, 1) = Zt_avg1(i, j);
        zeta(i, j, 2) = Zt_avg1(i, j);
      });
}

#endif  // KK_STAGE_SETZETA

#endif  // ROMS_KOKKOS
