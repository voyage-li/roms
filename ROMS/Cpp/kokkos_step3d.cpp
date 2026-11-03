/*
** Kokkos ports of the compute loops of ROMS/Nonlinear/pre_step3d.F and
** ROMS/Nonlinear/step3d_uv.F.
**
** Work decomposition
** ------------------
** Every launch gives a work item one grid row j (the tracer kernel one
** (row, tracer) pair) and replays the Fortran loops of that row with i
** innermost, in chunks of KB columns.  The i-innermost order matters on
** the CPEs: an earlier column-at-a-time version of the momentum
** predictor touched every level of ~25 arrays per column and ran 4.8x
** slower.
**
** pre_step3d tracers, per chunk: (1) horizontal advection into the
** predictor t(:,:,:,3,itrc), (2) vertical advection with the artificial
** continuity equation, (3) the explicit vertical diffusion into
** t(:,:,:,nnew,itrc) and DiaTwrk.  Doing the three Fortran passes chunk
** by chunk is safe: pass 1 reads the old t(nnew) only at its own point,
** pass 3 overwrites it only there, and every other read is of t(nstp),
** which nothing writes.
**
** pre_step3d momentum and both step3d_uv row loops are column local; the
** step3d_uv coupling loop runs after u3dbc/v3dbc, which stay in Fortran.
**
** Expressions keep the Fortran operand order and grouping, so a serial
** backend reproduces the Fortran result bit for bit.
*/

#include "kokkos_step3d.hpp"

#if defined ROMS_KOKKOS && \
    (defined KK_STAGE_PRE3D || defined KK_STAGE_STEP3DUV)

#include <cmath>
#ifndef __sw_slave__
#include <memory>
#endif

namespace {

using roms::Array2d;
using roms::Array3d;
using roms::Array4d;
using roms::Array5d;
using exec_space   = roms::exec_space;
using range_policy = Kokkos::RangePolicy<exec_space, Kokkos::IndexType<int>>;

/* Columns per chunk.  Within a chunk every Fortran loop is replayed with
** i innermost, so each array row is streamed contiguously (the order the
** Fortran uses and the one the CPE cache needs; a column-at-a-time sweep
** touches every level of every array before moving on and was 4.8x
** slower for the momentum predictor).  Chunking only bounds the scratch;
** columns are independent, so the results do not depend on KB. */
constexpr int KB = 32;

#ifndef __sw_slave__
/* Per-host-thread scratch, raw ownership (no Kokkos at teardown).  Every
** launch joins before returning, so it can be reused on the next call. */
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

#endif  // ROMS_KOKKOS && (KK_STAGE_PRE3D || KK_STAGE_STEP3DUV)

/*======================================================================
** pre_step3d.F
**====================================================================*/

#if defined ROMS_KOKKOS && defined KK_STAGE_PRE3D

#if defined MASKING || defined WET_DRY || defined WEC_VF || \
    defined SOLAR_SOURCE || defined LMD_NONLOCAL || defined BODYFORCE || \
    (defined AGE_MEAN && defined T_PASSIVE) || defined SINGLE_PRECISION
#  error "Kokkos pre_step3d does not support this physics configuration"
#endif

namespace {

using roms::Pre3dConfig;

constexpr double kEps = 1.0E-16;   /* eps in pre_step3d.F */

struct TracerFields {
  Pre3dConfig c;
  Array2d<double> pm, pn;
  Array3d<double> Hz, Huon, Hvom, zr, W, btflx, stflx;
  Array4d<double> Akt;
  Array5d<double> t;
#if defined DIAGNOSTICS_TS
  Array5d<double> dia;
#endif

  /* FX(i) = t(i)-t(i-1) with the closed-edge folds FX(Istr-1)=FX(Istr),
  ** FX(Iend+2)=FX(Iend+1). */
  KOKKOS_INLINE_FUNCTION
  double dx(int i, int j, int k, int it) const {
    if (c.fold_w && i == c.Istr - 1) i = c.Istr;
    else if (c.fold_e && i == c.Iend + 2) i = c.Iend + 1;
    return t(i, j, k, c.nstp, it) - t(i - 1, j, k, c.nstp, it);
  }

  KOKKOS_INLINE_FUNCTION
  double de(int i, int j, int k, int it) const {
    if (c.fold_s && j == c.Jstr - 1) j = c.Jstr;
    else if (c.fold_n && j == c.Jend + 2) j = c.Jend + 1;
    return t(i, j, k, c.nstp, it) - t(i, j - 1, k, c.nstp, it);
  }

  /* curv / grad at a cell from the two differences around it. */
  KOKKOS_INLINE_FUNCTION
  static double cell_slope(double a, double b, int scheme) {
    if (scheme == roms::kUpstream3) return a - b;
    if (scheme == roms::kAkima4) {
      const double cff = 2.0 * a * b;
      if (cff > kEps) return cff / (a + b);
      return 0.0;
    }
    return 0.5 * (a + b);
  }

  /* Xi-face flux FX(i,j) at level k (face between i-1 and i). */
  KOKKOS_INLINE_FUNCTION
  double flux_x(int i, int j, int k, int it, int scheme) const {
    const double hu = Huon(i, j, k);
    const double tl = t(i - 1, j, k, c.nstp, it);
    const double tr = t(i, j, k, c.nstp, it);
    if (scheme == roms::kCentered2) return hu * 0.5 * (tl + tr);
    if (scheme == roms::kUpstream1) {
      const double cff1 = Kokkos::max(hu, 0.0);
      const double cff2 = Kokkos::min(hu, 0.0);
      return cff1 * tl + cff2 * tr;
    }
    const double dm = dx(i - 1, j, k, it), d0 = dx(i, j, k, it);
    const double dp = dx(i + 1, j, k, it);
    const double sm = cell_slope(d0, dm, scheme);   /* cell i-1 */
    const double s0 = cell_slope(dp, d0, scheme);   /* cell i   */
    if (scheme == roms::kUpstream3) {
      return hu * 0.5 * (tl + tr) -
             (1.0 / 6.0) * (sm * Kokkos::max(hu, 0.0) + s0 * Kokkos::min(hu, 0.0));
    }
    return hu * 0.5 * (tl + tr - (1.0 / 3.0) * (s0 - sm));
  }

  /* Eta-face flux FE(i,j) at level k (face between j-1 and j). */
  KOKKOS_INLINE_FUNCTION
  double flux_e(int i, int j, int k, int it, int scheme) const {
    const double hv = Hvom(i, j, k);
    const double tl = t(i, j - 1, k, c.nstp, it);
    const double tr = t(i, j, k, c.nstp, it);
    if (scheme == roms::kCentered2) return hv * 0.5 * (tl + tr);
    if (scheme == roms::kUpstream1) {
      const double cff1 = Kokkos::max(hv, 0.0);
      const double cff2 = Kokkos::min(hv, 0.0);
      return cff1 * tl + cff2 * tr;
    }
    const double dm = de(i, j - 1, k, it), d0 = de(i, j, k, it);
    const double dp = de(i, j + 1, k, it);
    const double sm = cell_slope(d0, dm, scheme);   /* cell j-1 */
    const double s0 = cell_slope(dp, d0, scheme);   /* cell j   */
    if (scheme == roms::kUpstream3) {
      return hv * 0.5 * (tl + tr) -
             (1.0 / 6.0) * (sm * Kokkos::max(hv, 0.0) + s0 * Kokkos::min(hv, 0.0));
    }
    return hv * 0.5 * (tl + tr - (1.0 / 3.0) * (s0 - sm));
  }

  /* Vertical advective flux FC(0:N) of the columns ib..ib+nb-1 of row j
  ** from t(nstp), FC[k*KB+b] = FC(ib+b,k), the Fortran loops with i
  ** innermost. */
  KOKKOS_INLINE_FUNCTION
  void vertical_flux(int ib, int nb, int j, int it, int scheme, double* FC,
                     double* CF) const {
    const int N = c.N, s = c.nstp;
#define I_LOOP for (int b = 0, i = ib; b < nb; ++b, ++i)
    if (scheme == roms::kSplines) {
      /* NEUMANN is always defined in pre_step3d.F. */
      I_LOOP {
        FC[b] = 1.5 * t(i, j, 1, s, it);
        CF[KB + b] = 0.5;
      }
      for (int k = 1; k <= N - 1; ++k) {
        I_LOOP {
          const int p = k * KB + b;
          const double cff =
              1.0 / (2.0 * Hz(i, j, k) + Hz(i, j, k + 1) * (2.0 - CF[p]));
          CF[p + KB] = cff * Hz(i, j, k);
          FC[p] = cff * (3.0 * (Hz(i, j, k) * t(i, j, k + 1, s, it) +
                                Hz(i, j, k + 1) * t(i, j, k, s, it)) -
                         Hz(i, j, k + 1) * FC[p - KB]);
        }
      }
      I_LOOP {
        const int p = N * KB + b;
        FC[p] = (3.0 * t(i, j, N, s, it) - FC[p - KB]) / (2.0 - CF[p]);
      }
      for (int k = N - 1; k >= 0; --k) {
        I_LOOP {
          const int p = k * KB + b;
          FC[p] = FC[p] - CF[p + KB] * FC[p + KB];
          FC[p + KB] = W(i, j, k + 1) * FC[p + KB];
        }
      }
      I_LOOP {
        FC[N * KB + b] = 0.0;
        FC[b] = 0.0;
      }
    } else if (scheme == roms::kAkima4) {
      for (int k = 1; k <= N - 1; ++k) {
        I_LOOP { FC[k * KB + b] = t(i, j, k + 1, s, it) - t(i, j, k, s, it); }
      }
      I_LOOP {
        FC[b] = FC[KB + b];
        FC[N * KB + b] = FC[(N - 1) * KB + b];
      }
      for (int k = 1; k <= N; ++k) {
        I_LOOP {
          const int p = k * KB + b;
          const double cff = 2.0 * FC[p] * FC[p - KB];
          if (cff > kEps) {
            CF[p] = cff / (FC[p] + FC[p - KB]);
          } else {
            CF[p] = 0.0;
          }
        }
      }
      const double cff1 = 1.0 / 3.0;
      for (int k = 1; k <= N - 1; ++k) {
        I_LOOP {
          const int p = k * KB + b;
          FC[p] = W(i, j, k) * 0.5 *
                  (t(i, j, k, s, it) + t(i, j, k + 1, s, it) -
                   cff1 * (CF[p + KB] - CF[p]));
        }
      }
      I_LOOP {
        FC[b] = 0.0;
        FC[N * KB + b] = 0.0;
      }
    } else if (scheme == roms::kCentered2) {
      for (int k = 1; k <= N - 1; ++k) {
        I_LOOP {
          FC[k * KB + b] =
              W(i, j, k) * 0.5 * (t(i, j, k, s, it) + t(i, j, k + 1, s, it));
        }
      }
      I_LOOP {
        FC[b] = 0.0;
        FC[N * KB + b] = 0.0;
      }
    } else if (scheme == roms::kUpstream1) {
      for (int k = 1; k <= N - 1; ++k) {
        I_LOOP {
          const double cff1 = Kokkos::max(W(i, j, k), 0.0);
          const double cff2 = Kokkos::min(W(i, j, k), 0.0);
          FC[k * KB + b] = cff1 * t(i, j, k, s, it) + cff2 * t(i, j, k + 1, s, it);
        }
      }
      I_LOOP {
        FC[b] = 0.0;
        FC[N * KB + b] = 0.0;
      }
    } else {  /* CENTERED4 or SPLIT_U3 */
      const double cff1 = 0.5;
      const double cff2 = 7.0 / 12.0;
      const double cff3 = 1.0 / 12.0;
      for (int k = 2; k <= N - 2; ++k) {
        I_LOOP {
          FC[k * KB + b] =
              W(i, j, k) *
              (cff2 * (t(i, j, k, s, it) + t(i, j, k + 1, s, it)) -
               cff3 * (t(i, j, k - 1, s, it) + t(i, j, k + 2, s, it)));
        }
      }
      I_LOOP {
        FC[b] = 0.0;
        FC[KB + b] = W(i, j, 1) * (cff1 * t(i, j, 1, s, it) +
                                   cff2 * t(i, j, 2, s, it) -
                                   cff3 * t(i, j, 3, s, it));
        FC[(N - 1) * KB + b] = W(i, j, N - 1) * (cff1 * t(i, j, N, s, it) +
                                                 cff2 * t(i, j, N - 1, s, it) -
                                                 cff3 * t(i, j, N - 2, s, it));
        FC[N * KB + b] = 0.0;
      }
    }
#undef I_LOOP
  }
};

struct MomentumFields {
  Pre3dConfig c;
  Array2d<double> pm, pn, bustr, bvstr, sustr, svstr;
  Array3d<double> Hz, zr, Akv;
  Array4d<double> ru, rv, u, v;
#if defined DIAGNOSTICS_UV
  Array4d<double> DiaU3wrk, DiaV3wrk;
  Array5d<double> DiaRU, DiaRV;
#endif

  /* One row of the U (V=false) or V (V=true) predictor, swept level by
  ** level with i innermost, the Fortran loop order.  FC(k-1) of every
  ** column is carried in fcm[] and DC(i,0) in dc0[], both indexed i-i0.
  ** The values are the Fortran expressions; only the storage differs. */
  template <int STEP, bool V>
  KOKKOS_INLINE_FUNCTION
  void row(int j, double* fcm, double* dc0) const {
    const int N = c.N, s = c.nstp, nn = c.nnew, nr = c.nrhs;
    const int indx = 3 - nr;
    const int i0 = V ? c.Istr : c.IstrU, i1 = c.Iend;
    const int di = V ? 0 : 1, dj = V ? 1 : 0;
    const Array4d<double>& vel = V ? v : u;
    const Array4d<double>& rhs = V ? rv : ru;
    const Array2d<double>& bstr = V ? bvstr : bustr;
    const Array2d<double>& sstr = V ? svstr : sustr;
#if defined DIAGNOSTICS_UV
    const Array4d<double>& d3 = V ? DiaV3wrk : DiaU3wrk;
    const Array5d<double>& dr = V ? DiaRV : DiaRU;
#endif
    const double cffd = c.dt * 0.25;
    const double cfl  = c.dt * (1.0 - c.lambda);
    for (int i = i0; i <= i1; ++i) {
      dc0[i - i0] = cffd * (pm(i, j) + pm(i - di, j - dj)) *
                    (pn(i, j) + pn(i - di, j - dj));
      fcm[i - i0] = c.dt * bstr(i, j);
    }
    for (int k = 1; k <= N; ++k) {
      for (int i = i0; i <= i1; ++i) {
        double fck;
        if (k < N) {
          const double cff =
              1.0 / (zr(i, j, k + 1) + zr(i - di, j - dj, k + 1) -
                     zr(i, j, k) - zr(i - di, j - dj, k));
          fck = cfl * cff * (vel(i, j, k + 1, s) - vel(i, j, k, s)) *
                (Akv(i, j, k) + Akv(i - di, j - dj, k));
        } else {
          fck = c.dt * sstr(i, j);
        }
        const double DC0 = dc0[i - i0];
        if (STEP == 0) {
          const double cff1 =
              vel(i, j, k, s) * 0.5 * (Hz(i, j, k) + Hz(i - di, j - dj, k));
          const double cff2 = fck - fcm[i - i0];
          vel(i, j, k, nn) = cff1 + cff2;
#if defined DIAGNOSTICS_UV
          for (int id = 1; id <= c.M3pgrd; ++id) d3(i, j, k, id) = 0.0;
          d3(i, j, k, c.M3vvis) = cff2;
          d3(i, j, k, c.M3rate) = cff1;
#endif
        } else if (STEP == 1) {
          const double cff1 =
              vel(i, j, k, s) * 0.5 * (Hz(i, j, k) + Hz(i - di, j - dj, k));
          const double cff2 = fck - fcm[i - i0];
          const double cff3 = 0.5 * DC0;
          vel(i, j, k, nn) = cff1 - cff3 * rhs(i, j, k, indx) + cff2;
#if defined DIAGNOSTICS_UV
          for (int id = 1; id <= c.M3pgrd; ++id)
            d3(i, j, k, id) = -cff3 * dr(i, j, k, indx, id);
          d3(i, j, k, c.M3vvis) = cff2;
          d3(i, j, k, c.M3rate) = cff1;
#endif
        } else {
          const double cff1 = 5.0 / 12.0;
          const double cff2 = 16.0 / 12.0;
          const double cff3 =
              vel(i, j, k, s) * 0.5 * (Hz(i, j, k) + Hz(i - di, j - dj, k));
          const double cff4 = fck - fcm[i - i0];
          vel(i, j, k, nn) = cff3 +
                             DC0 * (cff1 * rhs(i, j, k, nr) -
                                    cff2 * rhs(i, j, k, indx)) +
                             cff4;
#if defined DIAGNOSTICS_UV
          for (int id = 1; id <= c.M3pgrd; ++id)
            d3(i, j, k, id) = DC0 * (cff1 * dr(i, j, k, nr, id) -
                                     cff2 * dr(i, j, k, indx, id));
          d3(i, j, k, c.M3vvis) = cff4;
          d3(i, j, k, c.M3rate) = cff3;
#endif
        }
        fcm[i - i0] = fck;
      }
    }
  }

  template <int STEP>
  KOKKOS_INLINE_FUNCTION
  void both(int j, double* fcm, double* dc0) const {
    row<STEP, false>(j, fcm, dc0);
    if (j >= c.JstrV) row<STEP, true>(j, fcm, dc0);
  }
};

}  // namespace

extern "C" {

void k_pre_step3d_t(const Pre3dConfig* cfg, const int* hadv, const int* vadv,
                    void* pm_p, void* pn_p, void* Hz_p,
                    void* Huon_p, void* Hvom_p, void* z_r_p, void* W_p,
                    void* Akt_p, void* btflx_p, void* stflx_p,
                    void* t_p, void* DiaTwrk_p) {
  roms::initialize();
  ROMS_KK_TIMER("pre_step3d tracers");

  TracerFields f;
  f.c = *cfg;
  const Pre3dConfig& c = f.c;
  const int L0 = c.LBi, L1 = c.UBi, M0 = c.LBj, M1 = c.UBj;
  f.pm    = roms::as_2d<double>(pm_p, L0, L1, M0, M1);
  f.pn    = roms::as_2d<double>(pn_p, L0, L1, M0, M1);
  f.Hz    = roms::as_3d<double>(Hz_p, L0, L1, M0, M1, 1, c.N);
  f.Huon  = roms::as_3d<double>(Huon_p, L0, L1, M0, M1, 1, c.N);
  f.Hvom  = roms::as_3d<double>(Hvom_p, L0, L1, M0, M1, 1, c.N);
  f.zr    = roms::as_3d<double>(z_r_p, L0, L1, M0, M1, 1, c.N);
  f.W     = roms::as_3d<double>(W_p, L0, L1, M0, M1, 0, c.N);
  f.btflx = roms::as_3d<double>(btflx_p, L0, L1, M0, M1, 1, c.NT);
  f.stflx = roms::as_3d<double>(stflx_p, L0, L1, M0, M1, 1, c.NT);
  f.Akt   = roms::as_4d<double>(Akt_p, L0, L1, M0, M1, 0, c.N, 1, c.NAT);
  f.t     = roms::as_5d<double>(t_p, L0, L1, M0, M1, 1, c.N, 1, 3, 1, c.NT);
#if defined DIAGNOSTICS_TS
  f.dia = roms::as_5d<double>(DiaTwrk_p, L0, L1, M0, M1, 1, c.N, 1, c.NT,
                              1, c.NDT);
#else
  (void)DiaTwrk_p;
#endif

  /* Scheme codes travel by value: copy them out of the Fortran array. */
  struct Codes { int h[32], v[32]; } codes{};
  if (c.NT > 32) Kokkos::abort("k_pre_step3d_t: more than 32 tracers");
  for (int it = 0; it < c.NT; ++it) {
    codes.h[it] = hadv[it];
    codes.v[it] = vadv[it];
  }

  const int ny  = c.Jend - c.Jstr + 1;
  const int N   = c.N;
  const int per = (2 * (N + 1) + 1) * KB + 1;   /* FC, CF, fx */
  double* scratch = nullptr;
#ifndef __sw_slave__
  scratch = workspace.reserve(std::size_t(ny) * c.NT * per);
#endif

  Kokkos::parallel_for(
      "ROMS pre_step3d tracers", range_policy(0, ny * c.NT),
      KOKKOS_LAMBDA(int index) {
        const int j  = c.Jstr + index % ny;
        const int it = 1 + index / ny;
        const int hs = codes.h[it - 1];
        const int vs = codes.v[it - 1];
        double* FC = scratch + std::size_t(index) * per;
        double* CF = FC + (N + 1) * KB;
        double* fx = CF + (N + 1) * KB;   /* KB+1 xi faces */

        /* Horizontal time-stepping coefficients. */
        double Gamma = (hs == roms::kUpstream1) ? 0.5 : 1.0 / 6.0;
        double cffh, cff1h, cff2h;
        if (c.step == 0) {
          cffh  = 0.5 * c.dt;
          cff1h = 1.0;
          cff2h = 0.0;
        } else {
          cffh  = (1.0 - Gamma) * c.dt;
          cff1h = 0.5 + Gamma;
          cff2h = 0.5 - Gamma;
        }
        /* Vertical advection coefficient. */
        Gamma = (vs == roms::kUpstream1) ? 0.5 : 1.0 / 6.0;
        double cffv;
        if (c.step == 0) {
          cffv = 0.5 * c.dt;
        } else {
          cffv = (1.0 - Gamma) * c.dt;
        }
        const double cff3d = c.dt * (1.0 - c.lambda);
        const int ltrc = (c.NAT < it) ? c.NAT : it;
        const int s = c.nstp, nn = c.nnew;

        for (int ib = c.Istr; ib <= c.Iend; ib += KB) {
          const int nb = (c.Iend - ib + 1 < KB) ? (c.Iend - ib + 1) : KB;
#define I_LOOP for (int b = 0, i = ib; b < nb; ++b, ++i)

          /* 1. Horizontal advection (m Tunits), K_LOOP.  The face
          ** shared with the next chunk is evaluated by both; it is the
          ** same expression on the same inputs. */
          for (int k = 1; k <= N; ++k) {
            for (int b = 0; b <= nb; ++b) fx[b] = f.flux_x(ib + b, j, k, it, hs);
            I_LOOP {
              const double fe0 = f.flux_e(i, j, k, it, hs);
              const double fe1 = f.flux_e(i, j + 1, k, it, hs);
              f.t(i, j, k, 3, it) =
                  f.Hz(i, j, k) * (cff1h * f.t(i, j, k, s, it) +
                                   cff2h * f.t(i, j, k, nn, it)) -
                  cffh * f.pm(i, j) * f.pn(i, j) *
                      (fx[b + 1] - fx[b] + fe1 - fe0);
            }
          }

          /* 2. Vertical advection, artificial continuity equation. */
          f.vertical_flux(ib, nb, j, it, vs, FC, CF);
          for (int k = 1; k <= N; ++k) {
            I_LOOP {
              const double DC =
                  1.0 / (f.Hz(i, j, k) -
                         cffv * f.pm(i, j) * f.pn(i, j) *
                             (f.Huon(i + 1, j, k) - f.Huon(i, j, k) +
                              f.Hvom(i, j + 1, k) - f.Hvom(i, j, k) +
                              (f.W(i, j, k) - f.W(i, j, k - 1))));
              const double cff1 = cffv * f.pm(i, j) * f.pn(i, j);
              f.t(i, j, k, 3, it) =
                  DC * (f.t(i, j, k, 3, it) -
                        cff1 * (FC[k * KB + b] - FC[(k - 1) * KB + b]));
            }
          }

          /* 3. Explicit vertical diffusion into t(nnew) (m Tunits). */
          for (int k = 1; k <= N - 1; ++k) {
            I_LOOP {
              const double cff = 1.0 / (f.zr(i, j, k + 1) - f.zr(i, j, k));
              FC[k * KB + b] = cff3d * cff * f.Akt(i, j, k, ltrc) *
                               (f.t(i, j, k + 1, s, it) - f.t(i, j, k, s, it));
            }
          }
          I_LOOP {
            FC[b] = c.dt * f.btflx(i, j, it);
            FC[N * KB + b] = c.dt * f.stflx(i, j, it);
          }
          for (int k = 1; k <= N; ++k) {
            I_LOOP {
              const double cff1 = f.Hz(i, j, k) * f.t(i, j, k, s, it);
              const double cff2 = FC[k * KB + b] - FC[(k - 1) * KB + b];
              f.t(i, j, k, nn, it) = cff1 + cff2;
#if defined DIAGNOSTICS_TS
              f.dia(i, j, k, it, c.iTrate) = cff1;
              f.dia(i, j, k, it, c.iTvdif) = cff2;
#endif
            }
          }
#undef I_LOOP
        }
      });
}

void k_pre_step3d_uv(const Pre3dConfig* cfg,
                     void* pm_p, void* pn_p, void* Hz_p, void* z_r_p,
                     void* Akv_p, void* bustr_p, void* bvstr_p,
                     void* sustr_p, void* svstr_p,
                     void* ru_p, void* rv_p, void* u_p, void* v_p,
                     void* DiaU3wrk_p, void* DiaV3wrk_p,
                     void* DiaRU_p, void* DiaRV_p) {
  roms::initialize();
  ROMS_KK_TIMER("pre_step3d momentum");

  MomentumFields f;
  f.c = *cfg;
  const Pre3dConfig& c = f.c;
  const int L0 = c.LBi, L1 = c.UBi, M0 = c.LBj, M1 = c.UBj;
  f.pm    = roms::as_2d<double>(pm_p, L0, L1, M0, M1);
  f.pn    = roms::as_2d<double>(pn_p, L0, L1, M0, M1);
  f.bustr = roms::as_2d<double>(bustr_p, L0, L1, M0, M1);
  f.bvstr = roms::as_2d<double>(bvstr_p, L0, L1, M0, M1);
  f.sustr = roms::as_2d<double>(sustr_p, L0, L1, M0, M1);
  f.svstr = roms::as_2d<double>(svstr_p, L0, L1, M0, M1);
  f.Hz    = roms::as_3d<double>(Hz_p, L0, L1, M0, M1, 1, c.N);
  f.zr    = roms::as_3d<double>(z_r_p, L0, L1, M0, M1, 1, c.N);
  f.Akv   = roms::as_3d<double>(Akv_p, L0, L1, M0, M1, 0, c.N);
  f.ru    = roms::as_4d<double>(ru_p, L0, L1, M0, M1, 0, c.N, 1, 2);
  f.rv    = roms::as_4d<double>(rv_p, L0, L1, M0, M1, 0, c.N, 1, 2);
  f.u     = roms::as_4d<double>(u_p, L0, L1, M0, M1, 1, c.N, 1, 2);
  f.v     = roms::as_4d<double>(v_p, L0, L1, M0, M1, 1, c.N, 1, 2);
#if defined DIAGNOSTICS_UV
  f.DiaU3wrk = roms::as_4d<double>(DiaU3wrk_p, L0, L1, M0, M1, 1, c.N, 1, c.NDM3d);
  f.DiaV3wrk = roms::as_4d<double>(DiaV3wrk_p, L0, L1, M0, M1, 1, c.N, 1, c.NDM3d);
  f.DiaRU = roms::as_5d<double>(DiaRU_p, L0, L1, M0, M1, 1, c.N, 1, 2, 1, c.NDrhs);
  f.DiaRV = roms::as_5d<double>(DiaRV_p, L0, L1, M0, M1, 1, c.N, 1, 2, 1, c.NDrhs);
#else
  (void)DiaU3wrk_p;
  (void)DiaV3wrk_p;
  (void)DiaRU_p;
  (void)DiaRV_p;
#endif

  const int ny  = c.Jend - c.Jstr + 1;
  const int per = 2 * (c.Iend - c.Istr + 2);
  double* scratch = nullptr;
#ifndef __sw_slave__
  scratch = workspace.reserve(std::size_t(ny) * per);
#endif
  const int half = per / 2;

  Kokkos::parallel_for(
      "ROMS pre_step3d momentum", range_policy(c.Jstr, c.Jend + 1),
      KOKKOS_LAMBDA(int j) {
        double* fcm = scratch + std::size_t(j - c.Jstr) * per;
        double* dc0 = fcm + half;
        if (c.step == 0) {
          f.both<0>(j, fcm, dc0);
        } else if (c.step == 1) {
          f.both<1>(j, fcm, dc0);
        } else {
          f.both<2>(j, fcm, dc0);
        }
      });
}

}  // extern "C"

#endif  // ROMS_KOKKOS && KK_STAGE_PRE3D

/*======================================================================
** step3d_uv.F
**
** k_step3d_uv_solve covers the first row loop of step3d_uv_tile (rows
** Jstr..Jend): the right-hand-side step, the implicit vertical
** viscosity and the interior vertical-mean replacement, for U then V.
** k_step3d_uv_couple covers the second row loop (rows JstrT..JendT),
** which runs after u3dbc/v3dbc and the point sources.  Both are column
** local: a column reads its own levels plus read-only neighbours (Hz,
** Akv, z_r, pm, pn of i-1 or j-1), so one work item per row is free of
** races.  U and V share one code path; the neighbour offset (di,dj) and
** the arrays select the direction, and every expression keeps the
** Fortran operand order for both.
**====================================================================*/

#if defined ROMS_KOKKOS && defined KK_STAGE_STEP3DUV

#if defined MASKING || defined WET_DRY || defined WEC || defined WEC_VF || \
    defined OMEGA_IMPLICIT || defined BODYFORCE || \
    defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3 || \
    defined SINGLE_PRECISION
#  error "Kokkos step3d_uv does not support this physics configuration"
#endif

namespace {

using roms::Uv3dConfig;

struct UvDir {
  int di = 0, dj = 0;               /* neighbour offset: U (1,0), V (0,1) */
  Array2d<double> metric;           /* on_u or om_v */
  Array2d<double> avg1, avg2;       /* DU_avg1/2 or DV_avg1/2 */
  Array4d<double> rhs;              /* ru or rv, (.., 0:N, 2) */
  Array4d<double> vel;              /* u or v, (.., N, 2) */
  Array3d<double> bar;              /* ubar or vbar, (.., 3) */
  Array3d<double> flux;             /* Huon or Hvom */
#if defined DIAGNOSTICS_UV
  Array3d<double> d2wrk, d2int;     /* DiaU2wrk/DiaU2int, (.., NDM2d) */
  Array4d<double> d3wrk;            /* DiaU3wrk, (.., N, NDM3d) */
  Array5d<double> dRU;              /* DiaRU, (.., N, 2, NDrhs) */
#endif
};

struct UvFields {
  Uv3dConfig c;
  Array2d<double> pm, pn;
  Array3d<double> Hz, zr, Akv;
  UvDir d[2];

  KOKKOS_INLINE_FUNCTION
  static int solve_scratch(int N, int NDM2d) {
    return (8 * (N + 1) + NDM2d + 2) * KB;
  }

  /* First row loop of step3d_uv_tile for one direction, columns
  ** [i0, i1] of row j, in chunks of KB columns.  X[k*KB+b] holds the
  ** Fortran X(i,k) for i = ib+b. */
  KOKKOS_INLINE_FUNCTION
  void solve_row(const UvDir& d, int i0, int i1, int j, double* s) const {
    const int N = c.N, nn = c.nnew, nr = c.nrhs;
    double* AK   = s;
    double* Hzk  = AK + (N + 1) * KB;
    double* oHz  = Hzk + (N + 1) * KB;
    double* FC   = oHz + (N + 1) * KB;
    double* CF   = FC + (N + 1) * KB;
    double* BC   = CF + (N + 1) * KB;
    double* DC   = BC + (N + 1) * KB;
    double* wrk  = DC + (N + 1) * KB;
    double* cf1  = wrk + (N + 1) * KB;
#if defined DIAGNOSTICS_UV
    double* Dwrk = cf1 + KB;          /* Dwrk[id*KB+b], id = 2-D term */
#endif
    (void)wrk;
    const int di = d.di, dj = d.dj;

    for (int ib = i0; ib <= i1; ib += KB) {
      const int nb = (i1 - ib + 1 < KB) ? (i1 - ib + 1) : KB;
#define I_LOOP for (int b = 0, i = ib; b < nb; ++b, ++i)

      I_LOOP { AK[b] = 0.5 * (Akv(i - di, j - dj, 0) + Akv(i, j, 0)); }
      for (int k = 1; k <= N; ++k) {
        I_LOOP {
          AK[k * KB + b]  = 0.5 * (Akv(i - di, j - dj, k) + Akv(i, j, k));
          Hzk[k * KB + b] = 0.5 * (Hz(i - di, j - dj, k) + Hz(i, j, k));
          oHz[k * KB + b] = 1.0 / Hzk[k * KB + b];
        }
      }

      /* Time step right-hand-side terms. */
      double cff;
      if (c.step == 0) {
        cff = 0.25 * c.dt;
      } else if (c.step == 1) {
        cff = 0.25 * c.dt * 3.0 / 2.0;
      } else {
        cff = 0.25 * c.dt * 23.0 / 12.0;
      }
      I_LOOP {
        DC[b] = cff * (pm(i, j) + pm(i - di, j - dj)) *
                (pn(i, j) + pn(i - di, j - dj));
      }
      for (int k = 1; k <= N; ++k) {
        I_LOOP {
          d.vel(i, j, k, nn) = d.vel(i, j, k, nn) + DC[b] * d.rhs(i, j, k, nr);
#if defined SPLINES_VVISC
          d.vel(i, j, k, nn) = d.vel(i, j, k, nn) * oHz[k * KB + b];
#endif
        }
#if defined DIAGNOSTICS_UV
        for (int id = 1; id <= c.M3pgrd; ++id) {
          I_LOOP {
            d.d3wrk(i, j, k, id) =
                (d.d3wrk(i, j, k, id) + DC[b] * d.dRU(i, j, k, nr, id)) *
                oHz[k * KB + b];
          }
        }
        I_LOOP {
          const double o = oHz[k * KB + b];
# if defined UV_VIS2 || defined UV_VIS4
          d.d3wrk(i, j, k, c.M3xvis) = d.d3wrk(i, j, k, c.M3xvis) * o;
          d.d3wrk(i, j, k, c.M3yvis) = d.d3wrk(i, j, k, c.M3yvis) * o;
          d.d3wrk(i, j, k, c.M3hvis) = d.d3wrk(i, j, k, c.M3hvis) * o;
# endif
          d.d3wrk(i, j, k, c.M3vvis) = d.d3wrk(i, j, k, c.M3vvis) * o;
          d.d3wrk(i, j, k, c.M3rate) = d.d3wrk(i, j, k, c.M3rate) * o;
        }
#endif
      }

#if defined SPLINES_VVISC
      /* Parabolic splines; implicit tridiagonal vertical viscosity. */
      {
        const double cff1 = 1.0 / 6.0;
        for (int k = 1; k <= N - 1; ++k) {
          I_LOOP {
            FC[k * KB + b] = cff1 * Hzk[k * KB + b] -
                             c.dt * AK[(k - 1) * KB + b] * oHz[k * KB + b];
            CF[k * KB + b] = cff1 * Hzk[(k + 1) * KB + b] -
                             c.dt * AK[(k + 1) * KB + b] * oHz[(k + 1) * KB + b];
          }
        }
      }
      I_LOOP {
        CF[b] = 0.0;
        DC[b] = 0.0;
      }
      {
        const double cff1 = 1.0 / 3.0;
        for (int k = 1; k <= N - 1; ++k) {
          I_LOOP {
            const int p = k * KB + b;
            BC[p] = cff1 * (Hzk[p] + Hzk[p + KB]) +
                    c.dt * AK[p] * (oHz[p] + oHz[p + KB]);
            const double cffk = 1.0 / (BC[p] - FC[p] * CF[p - KB]);
            CF[p] = cffk * CF[p];
            DC[p] = cffk * (d.vel(i, j, k + 1, nn) - d.vel(i, j, k, nn) -
                            FC[p] * DC[p - KB]);
          }
        }
      }
      I_LOOP { DC[N * KB + b] = 0.0; }
      for (int k = N - 1; k >= 1; --k) {
        I_LOOP {
          const int p = k * KB + b;
          DC[p] = DC[p] - CF[p] * DC[p + KB];
        }
      }
      for (int k = 1; k <= N; ++k) {
        I_LOOP {
          const int p = k * KB + b;
          DC[p] = DC[p] * AK[p];
          const double cffk = c.dt * oHz[p] * (DC[p] - DC[p - KB]);
          d.vel(i, j, k, nn) = d.vel(i, j, k, nn) + cffk;
# if defined DIAGNOSTICS_UV
          d.d3wrk(i, j, k, c.M3vvis) = d.d3wrk(i, j, k, c.M3vvis) + cffk;
# endif
        }
      }
#else
      /* Implicit vertical viscosity, [lambda*dt*Akv/Hz] off-diagonals. */
      {
        const double cffl = -c.lambda * c.dt / 0.5;
        for (int k = 1; k <= N - 1; ++k) {
          I_LOOP {
            const double cff1 =
                1.0 / (zr(i, j, k + 1) + zr(i - di, j - dj, k + 1) -
                       zr(i, j, k) - zr(i - di, j - dj, k));
            FC[k * KB + b] = cffl * cff1 * AK[k * KB + b];
          }
        }
      }
      I_LOOP {
        FC[b] = 0.0;
        FC[N * KB + b] = 0.0;
      }
      for (int k = 1; k <= N; ++k) {
        I_LOOP {
          const int p = k * KB + b;
          DC[p] = d.vel(i, j, k, nn);
          BC[p] = Hzk[p] - FC[p] - FC[p - KB];
        }
      }
      I_LOOP {
        const double cffk = 1.0 / BC[KB + b];
        CF[KB + b] = cffk * FC[KB + b];
        DC[KB + b] = cffk * DC[KB + b];
      }
      for (int k = 2; k <= N - 1; ++k) {
        I_LOOP {
          const int p = k * KB + b;
          const double cffk = 1.0 / (BC[p] - FC[p - KB] * CF[p - KB]);
          CF[p] = cffk * FC[p];
          DC[p] = cffk * (DC[p] - FC[p - KB] * DC[p - KB]);
        }
      }
      I_LOOP {
        const int p = N * KB + b;
# if defined DIAGNOSTICS_UV
        wrk[p] = d.vel(i, j, N, nn) * oHz[p];
# endif
        DC[p] = (DC[p] - FC[p - KB] * DC[p - KB]) /
                (BC[p] - FC[p - KB] * CF[p - KB]);
        d.vel(i, j, N, nn) = DC[p];
# if defined DIAGNOSTICS_UV
        d.d3wrk(i, j, N, c.M3vvis) =
            d.d3wrk(i, j, N, c.M3vvis) + d.vel(i, j, N, nn) - wrk[p];
# endif
      }
      for (int k = N - 1; k >= 1; --k) {
        I_LOOP {
          const int p = k * KB + b;
# if defined DIAGNOSTICS_UV
          wrk[p] = d.vel(i, j, k, nn) * oHz[p];
# endif
          DC[p] = DC[p] - CF[p] * DC[p + KB];
          d.vel(i, j, k, nn) = DC[p];
# if defined DIAGNOSTICS_UV
          d.d3wrk(i, j, k, c.M3vvis) =
              d.d3wrk(i, j, k, c.M3vvis) + d.vel(i, j, k, nn) - wrk[p];
# endif
        }
      }
#endif

      /* Replace the interior incorrect vertical mean with the barotropic
      ** component, ubar=DU_avg1/(D*on_u).  CF(:,0) and DC(:,0) are
      ** CF[b] and DC[b]. */
      I_LOOP {
        CF[b] = Hzk[KB + b];
        DC[b] = d.vel(i, j, 1, nn) * Hzk[KB + b];
      }
#if defined DIAGNOSTICS_UV
      for (int q = 0; q < c.npairs; ++q) {
        double* D = Dwrk + c.m2[q] * KB;
        I_LOOP { D[b] = d.d3wrk(i, j, 1, c.m3[q]) * Hzk[KB + b]; }
      }
#endif
      for (int k = 2; k <= N; ++k) {
        I_LOOP {
          CF[b] = CF[b] + Hzk[k * KB + b];
          DC[b] = DC[b] + d.vel(i, j, k, nn) * Hzk[k * KB + b];
        }
#if defined DIAGNOSTICS_UV
        for (int q = 0; q < c.npairs; ++q) {
          double* D = Dwrk + c.m2[q] * KB;
          I_LOOP { D[b] = D[b] + d.d3wrk(i, j, k, c.m3[q]) * Hzk[k * KB + b]; }
        }
#endif
      }
      I_LOOP {
        cf1[b] = 1.0 / (CF[b] * d.metric(i, j));
        DC[b] = (DC[b] * d.metric(i, j) - d.avg1(i, j)) * cf1[b];
      }
#if defined DIAGNOSTICS_UV
      for (int id = 1; id <= c.M2pgrd; ++id) {
        double* D = Dwrk + id * KB;
        I_LOOP { D[b] = (D[b] * d.metric(i, j) - d.d2wrk(i, j, id)) * cf1[b]; }
      }
      {
        double* D = Dwrk + c.M2bstr * KB;
        I_LOOP {
          D[b] = (D[b] * d.metric(i, j) - d.d2wrk(i, j, c.M2bstr) -
                  d.d2wrk(i, j, c.M2sstr)) * cf1[b];
        }
      }
#endif
      for (int k = 1; k <= N; ++k) {
        I_LOOP { d.vel(i, j, k, nn) = d.vel(i, j, k, nn) - DC[b]; }
#if defined DIAGNOSTICS_UV
        for (int q = 0; q < c.npairs; ++q) {
          const double* D = Dwrk + c.m2[q] * KB;
          I_LOOP {
            d.d3wrk(i, j, k, c.m3[q]) = d.d3wrk(i, j, k, c.m3[q]) - D[b];
          }
        }
#endif
      }
#undef I_LOOP
    }
  }

  KOKKOS_INLINE_FUNCTION
  static int couple_scratch(int N) { return (N + 5) * KB; }

  /* Second row loop for one direction.  U uses west point i=Istr and
  ** south row j=0, V uses i=Istr-1 and j=1: the first u/v boundary
  ** points.  Order of the boundary replacements: west, east, south,
  ** north, as in the Fortran. */
  KOKKOS_INLINE_FUNCTION
  void couple_row(const UvDir& d, int i0, int i1, int j, int west_i,
                  int south_j, int ilo, double* s) const {
    const int N = c.N, nn = c.nnew;
    const int di = d.di, dj = d.dj;
    double* DCk = s;                 /* DC(i,k), k = 1..N */
    double* DC0 = DCk + (N + 1) * KB;
    double* CF0 = DC0 + KB;
    double* FC0 = CF0 + KB;
    double* cf1 = FC0 + KB;
    for (int ib = i0; ib <= i1; ib += KB) {
      const int nb = (i1 - ib + 1 < KB) ? (i1 - ib + 1) : KB;
#define I_LOOP for (int b = 0, i = ib; b < nb; ++b, ++i)
      I_LOOP {
        DC0[b] = 0.0;
        CF0[b] = 0.0;
        FC0[b] = 0.0;
      }
      for (int k = 1; k <= N; ++k) {
        I_LOOP {
          const double cff = 0.5 * d.metric(i, j);
          DCk[k * KB + b] = cff * (Hz(i, j, k) + Hz(i - di, j - dj, k));
          DC0[b] = DC0[b] + DCk[k * KB + b];
          CF0[b] = CF0[b] + DCk[k * KB + b] * d.vel(i, j, k, nn);
        }
      }
      I_LOOP {
        cf1[b] = DC0[b];
        DC0[b] = 1.0 / DC0[b];
        CF0[b] = DC0[b] * (CF0[b] - d.avg1(i, j));
        d.bar(i, j, 1) = DC0[b] * d.avg1(i, j);
        d.bar(i, j, 2) = d.bar(i, j, 1);
#if defined DIAGNOSTICS_UV
        d.d2wrk(i, j, c.M2rate) = d.bar(i, j, 1) - d.d2int(i, j, c.M2rate) * DC0[b];
        d.d2int(i, j, c.M2rate) = d.bar(i, j, 1) * cf1[b];
#endif
      }
#if defined DIAGNOSTICS_UV
      for (int id = 1; id <= c.NDM2d - 1; ++id) {
        I_LOOP { d.d2wrk(i, j, id) = DC0[b] * d.d2wrk(i, j, id); }
      }
#endif
      I_LOOP {
        if (c.rep_w && i == west_i) {
          for (int k = 1; k <= N; ++k) d.vel(i, j, k, nn) = d.vel(i, j, k, nn) - CF0[b];
        }
        if (c.rep_e && i == c.Iend + 1) {
          for (int k = 1; k <= N; ++k) d.vel(i, j, k, nn) = d.vel(i, j, k, nn) - CF0[b];
        }
        if (c.rep_s && j == south_j && i >= ilo && i <= c.Iend) {
          for (int k = 1; k <= N; ++k) d.vel(i, j, k, nn) = d.vel(i, j, k, nn) - CF0[b];
        }
        if (c.rep_n && j == c.Mm + 1 && i >= ilo && i <= c.Iend) {
          for (int k = 1; k <= N; ++k) d.vel(i, j, k, nn) = d.vel(i, j, k, nn) - CF0[b];
        }
      }
      /* Correct mass flux, Hz*u/n. */
      for (int k = N; k >= 1; --k) {
        I_LOOP {
          d.flux(i, j, k) =
              0.5 * (d.flux(i, j, k) + d.vel(i, j, k, nn) * DCk[k * KB + b]);
          FC0[b] = FC0[b] + d.flux(i, j, k);
#if defined DIAGNOSTICS_UV
          d.d3wrk(i, j, k, c.M3rate) =
              d.vel(i, j, k, nn) - d.d3wrk(i, j, k, c.M3rate);
#endif
        }
      }
      I_LOOP { FC0[b] = DC0[b] * (FC0[b] - d.avg2(i, j)); }
      for (int k = 1; k <= N; ++k) {
        I_LOOP { d.flux(i, j, k) = d.flux(i, j, k) - DCk[k * KB + b] * FC0[b]; }
      }
#undef I_LOOP
    }
  }
};

}  // namespace

extern "C" {

void k_step3d_uv_solve(const Uv3dConfig* cfg,
                       void* pm_p, void* pn_p, void* on_u_p, void* om_v_p,
                       void* Hz_p, void* z_r_p, void* Akv_p,
                       void* DU_avg1_p, void* DV_avg1_p,
                       void* ru_p, void* rv_p, void* u_p, void* v_p,
                       void* DiaU2wrk_p, void* DiaV2wrk_p,
                       void* DiaU3wrk_p, void* DiaV3wrk_p,
                       void* DiaRU_p, void* DiaRV_p) {
  roms::initialize();
  ROMS_KK_TIMER("step3d_uv solve");

  UvFields f;
  f.c = *cfg;
  const Uv3dConfig& c = f.c;
  const int L0 = c.LBi, L1 = c.UBi, M0 = c.LBj, M1 = c.UBj;
  f.pm  = roms::as_2d<double>(pm_p, L0, L1, M0, M1);
  f.pn  = roms::as_2d<double>(pn_p, L0, L1, M0, M1);
  f.Hz  = roms::as_3d<double>(Hz_p, L0, L1, M0, M1, 1, c.N);
  f.zr  = roms::as_3d<double>(z_r_p, L0, L1, M0, M1, 1, c.N);
  f.Akv = roms::as_3d<double>(Akv_p, L0, L1, M0, M1, 0, c.N);
  void* metric[2] = {on_u_p, om_v_p};
  void* avg1[2]   = {DU_avg1_p, DV_avg1_p};
  void* rhs[2]    = {ru_p, rv_p};
  void* vel[2]    = {u_p, v_p};
  void* d2w[2]    = {DiaU2wrk_p, DiaV2wrk_p};
  void* d3w[2]    = {DiaU3wrk_p, DiaV3wrk_p};
  void* dru[2]    = {DiaRU_p, DiaRV_p};
  for (int n = 0; n < 2; ++n) {
    UvDir& d = f.d[n];
    d.di = (n == 0) ? 1 : 0;
    d.dj = (n == 0) ? 0 : 1;
    d.metric = roms::as_2d<double>(metric[n], L0, L1, M0, M1);
    d.avg1   = roms::as_2d<double>(avg1[n], L0, L1, M0, M1);
    d.rhs    = roms::as_4d<double>(rhs[n], L0, L1, M0, M1, 0, c.N, 1, 2);
    d.vel    = roms::as_4d<double>(vel[n], L0, L1, M0, M1, 1, c.N, 1, 2);
#if defined DIAGNOSTICS_UV
    d.d2wrk = roms::as_3d<double>(d2w[n], L0, L1, M0, M1, 1, c.NDM2d);
    d.d3wrk = roms::as_4d<double>(d3w[n], L0, L1, M0, M1, 1, c.N, 1, c.NDM3d);
    d.dRU   = roms::as_5d<double>(dru[n], L0, L1, M0, M1, 1, c.N, 1, 2, 1, c.NDrhs);
#else
    (void)d2w;
    (void)d3w;
    (void)dru;
#endif
  }

  const int ny  = c.Jend - c.Jstr + 1;
  const int per = UvFields::solve_scratch(c.N, c.NDM2d);
  double* scratch = nullptr;
#ifndef __sw_slave__
  scratch = workspace.reserve(std::size_t(ny) * per);
#endif

  Kokkos::parallel_for(
      "ROMS step3d_uv solve", range_policy(c.Jstr, c.Jend + 1),
      KOKKOS_LAMBDA(int j) {
        double* s = scratch + std::size_t(j - c.Jstr) * per;
        f.solve_row(f.d[0], c.IstrU, c.Iend, j, s);
        if (j >= c.JstrV) f.solve_row(f.d[1], c.Istr, c.Iend, j, s);
      });
}

void k_step3d_uv_couple(const Uv3dConfig* cfg,
                        void* on_u_p, void* om_v_p, void* Hz_p,
                        void* DU_avg1_p, void* DU_avg2_p,
                        void* DV_avg1_p, void* DV_avg2_p,
                        void* u_p, void* v_p, void* ubar_p, void* vbar_p,
                        void* Huon_p, void* Hvom_p,
                        void* DiaU2wrk_p, void* DiaV2wrk_p,
                        void* DiaU2int_p, void* DiaV2int_p,
                        void* DiaU3wrk_p, void* DiaV3wrk_p) {
  roms::initialize();
  ROMS_KK_TIMER("step3d_uv couple");

  UvFields f;
  f.c = *cfg;
  const Uv3dConfig& c = f.c;
  const int L0 = c.LBi, L1 = c.UBi, M0 = c.LBj, M1 = c.UBj;
  f.Hz = roms::as_3d<double>(Hz_p, L0, L1, M0, M1, 1, c.N);
  void* metric[2] = {on_u_p, om_v_p};
  void* avg1[2]   = {DU_avg1_p, DV_avg1_p};
  void* avg2[2]   = {DU_avg2_p, DV_avg2_p};
  void* vel[2]    = {u_p, v_p};
  void* bar[2]    = {ubar_p, vbar_p};
  void* flux[2]   = {Huon_p, Hvom_p};
  void* d2w[2]    = {DiaU2wrk_p, DiaV2wrk_p};
  void* d2i[2]    = {DiaU2int_p, DiaV2int_p};
  void* d3w[2]    = {DiaU3wrk_p, DiaV3wrk_p};
  for (int n = 0; n < 2; ++n) {
    UvDir& d = f.d[n];
    d.di = (n == 0) ? 1 : 0;
    d.dj = (n == 0) ? 0 : 1;
    d.metric = roms::as_2d<double>(metric[n], L0, L1, M0, M1);
    d.avg1   = roms::as_2d<double>(avg1[n], L0, L1, M0, M1);
    d.avg2   = roms::as_2d<double>(avg2[n], L0, L1, M0, M1);
    d.vel    = roms::as_4d<double>(vel[n], L0, L1, M0, M1, 1, c.N, 1, 2);
    d.bar    = roms::as_3d<double>(bar[n], L0, L1, M0, M1, 1, 3);
    d.flux   = roms::as_3d<double>(flux[n], L0, L1, M0, M1, 1, c.N);
#if defined DIAGNOSTICS_UV
    d.d2wrk = roms::as_3d<double>(d2w[n], L0, L1, M0, M1, 1, c.NDM2d);
    d.d2int = roms::as_3d<double>(d2i[n], L0, L1, M0, M1, 1, c.NDM2d);
    d.d3wrk = roms::as_4d<double>(d3w[n], L0, L1, M0, M1, 1, c.N, 1, c.NDM3d);
#else
    (void)d2w;
    (void)d2i;
    (void)d3w;
#endif
  }

  const int ny  = c.JendT - c.JstrT + 1;
  const int per = UvFields::couple_scratch(c.N);
  double* scratch = nullptr;
#ifndef __sw_slave__
  scratch = workspace.reserve(std::size_t(ny) * per);
#endif

  Kokkos::parallel_for(
      "ROMS step3d_uv couple", range_policy(c.JstrT, c.JendT + 1),
      KOKKOS_LAMBDA(int j) {
        double* s = scratch + std::size_t(j - c.JstrT) * per;
        f.couple_row(f.d[0], c.IstrP, c.IendT, j, c.Istr, 0, c.IstrU, s);
        if (j >= c.Jstr) {
          f.couple_row(f.d[1], c.IstrT, c.IendT, j, c.Istr - 1, 1, c.Istr, s);
        }
      });
}

}  // extern "C"

#endif  // ROMS_KOKKOS && KK_STAGE_STEP3DUV
