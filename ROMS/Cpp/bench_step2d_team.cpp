/*
** Policy benchmark: every step2d kernel under RangePolicy and under the
** experimental TeamPolicy + team-scratch entry, on the same tile.
**
**   bench_step2d_team_kokkos NX NY [REPS]
**
** Each kernel is run twice with an identical input state: once through
** the production range entry, once through the team entry, and the whole
** array set is compared bitwise.  The timed loops run REPS calls after a
** warm-up, with the state restored before each variant.
**
** Use a size that matches a real tile (upwelling 41 x 80, canyon 65 x 48,
** bl_test 39 x 4) and larger ones (256 x 256, 512 x 512) to separate the
** policy cost from the per-cell work.
*/

#include "kokkos_step2d.hpp"
#include "kokkos_step2d_team.hpp"

#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <vector>

namespace {

#define CASE_FIELDS(X)                                                        \
  X(h) X(on_u) X(om_v) X(gz) X(gz2) X(gza) X(rho) X(zw) X(depth) X(fomn)      \
  X(pm) X(pn) X(pmr) X(pnr) X(pmp) X(pnp) X(omr) X(onr) X(omp) X(onp)         \
  X(visr) X(visp) X(visc2pp)                                                  \
  X(zeta) X(ubar) X(vbar) X(rzeta) X(ru) X(rv) X(rubar) X(rvbar)              \
  X(du) X(dv) X(drhs) X(drhspsi) X(grad) X(dgrad) X(ufx) X(ufe) X(vfx) X(vfe) \
  X(rhs_u) X(rhs_v) X(zt_avg1) X(du_avg1) X(du_avg2) X(dv_avg1) X(dv_avg2)    \
  X(zeta_new) X(dnew) X(zwrk) X(dstp) X(rufrc) X(rvfrc)                       \
  X(diau) X(diav) X(diaru) X(diarv) X(diarufrc) X(diarvfrc)

struct Snapshot {
#define DECL_FIELD(f) std::vector<double> f;
  CASE_FIELDS(DECL_FIELD)
#undef DECL_FIELD
};

struct Case {
  int nx, ny;
  int LBi, UBi, LBj, UBj;          // grid arrays
  int IminS, ImaxS, JminS, JmaxS;  // tile-private arrays
  int n2g, n2p;
  roms::TileBounds t{};
  roms::EdgeFlags e{};

#define DECL_FIELD(f) std::vector<double> f;
  CASE_FIELDS(DECL_FIELD)
#undef DECL_FIELD

  Case(int NX, int NY) : nx(NX), ny(NY) {
    LBi = -2; UBi = nx + 2; LBj = -2; UBj = ny + 2;
    IminS = -4; ImaxS = nx + 4; JminS = -4; JmaxS = ny + 4;
    n2g = (UBi - LBi + 1) * (UBj - LBj + 1);
    n2p = (ImaxS - IminS + 1) * (JmaxS - JminS + 1);

    t.Istr = 1; t.Iend = nx; t.Jstr = 1; t.Jend = ny;
    t.IstrU = 2; t.JstrV = 2;
    t.IstrR = 0; t.IendR = nx + 1; t.JstrR = 0; t.JendR = ny + 1;
    t.Istrm1 = 1; t.Jstrm1 = 1; t.Iendp1 = nx + 1; t.Jendp1 = ny + 1;
    t.Iendp2 = nx + 2; t.Jendp2 = ny + 2;
    t.IstrUm1 = 1; t.JstrVm1 = 1; t.IstrUm2 = 0; t.JstrVm2 = 0;
    t.UBk = 1;

    // Closed, non-periodic, non-composite: the fold branches stay live.
    e.west = e.east = e.south = e.north = 1;
    e.composite_w = e.composite_e = e.composite_s = e.composite_n = 0;
    e.periodic_ew = e.periodic_ns = 0;

    int k = 0;
#define ALLOC2G(f) f.assign(n2g, 0.0); fill(f, ++k);
#define ALLOC2P(f) f.assign(n2p, 0.0); fill(f, ++k);
#define ALLOC3(f, m) f.assign(std::size_t(n2g) * (m), 0.0); fill(f, ++k);
#define ALLOC3P(f, m) f.assign(std::size_t(n2p) * (m), 0.0); fill(f, ++k);
    CASE_FIELDS(ALLOC2G)
#undef ALLOC2G
#undef ALLOC2P
#undef ALLOC3
#undef ALLOC3P
    // Re-assign the shapes that are not plain 2-D grid fields.
    k = 0;
#define FIX(f, size) f.assign(std::size_t(size), 0.0); fill(f, ++k);
    FIX(zeta, n2g * 3) FIX(ubar, n2g * 3) FIX(vbar, n2g * 3)
    FIX(rzeta, n2g * 2) FIX(rubar, n2g * 2) FIX(rvbar, n2g * 2)
    FIX(ru, n2g * 4) FIX(rv, n2g * 4)
    FIX(gz, n2p) FIX(gz2, n2p) FIX(gza, n2p) FIX(zw, n2p)
    FIX(du, n2p) FIX(dv, n2p) FIX(drhs, n2p) FIX(drhspsi, n2p)
    FIX(grad, n2p) FIX(dgrad, n2p) FIX(ufx, n2p) FIX(ufe, n2p)
    FIX(vfx, n2p) FIX(vfe, n2p) FIX(rhs_u, n2p) FIX(rhs_v, n2p)
    FIX(zt_avg1, n2p) FIX(du_avg1, n2p) FIX(du_avg2, n2p)
    FIX(dv_avg1, n2p) FIX(dv_avg2, n2p)
    FIX(zeta_new, n2p) FIX(dnew, n2p) FIX(zwrk, n2p) FIX(dstp, n2p)
    FIX(rufrc, n2p) FIX(rvfrc, n2p)
    FIX(diau, n2p * 10) FIX(diav, n2p * 10)
    FIX(diaru, n2g * 20) FIX(diarv, n2g * 20)
    FIX(diarufrc, n2g * 30) FIX(diarvfrc, n2g * 30)
#undef FIX
    save();
  }

  void save() {
#define SV(f) pristine_.f = f;
    CASE_FIELDS(SV)
#undef SV
  }

  void fill(std::vector<double>& a, int k) {
    const double base = 1.0 + 0.03 * (k % 11);
    for (std::size_t i = 0; i < a.size(); ++i)
      a[i] = base + 1.0e-4 * double(i % 997);
  }

  void restore() {
#define CP(f) f = pristine_.f;
    CASE_FIELDS(CP)
#undef CP
  }

  double compare(const Case& ref, const char** field, long* ndiff,
                 long* nnan, double* fa, double* fb, std::size_t* fi) const {
    double worst = 0.0;
    *field = "none";
    *ndiff = 0;
    *nnan = 0;
    bool first = true;
#define CMP(f)                                                                 \
  {                                                                            \
    for (std::size_t i = 0; i < f.size(); ++i) {                               \
      const double d = std::abs(f[i] - ref.f[i]);                              \
      if (std::isnan(d)) { ++*nnan; }                                          \
      else if (d != 0.0) {                                                     \
        ++*ndiff;                                                              \
        if (first) { first = false; *fa = f[i]; *fb = ref.f[i]; *fi = i; }     \
        if (*ndiff <= 6)                                                       \
          std::printf("    diff %s[%zu] team=%.17g range=%.17g\n", #f, i,      \
                      f[i], ref.f[i]);                                         \
        if (d > worst) { worst = d; *field = #f; }                             \
      }                                                                        \
    }                                                                          \
  }
    CASE_FIELDS(CMP)
#undef CMP
    return worst;
  }

  Snapshot pristine_;
};

double now() {
  Kokkos::fence();
  return std::chrono::duration<double>(
             std::chrono::steady_clock::now().time_since_epoch())
      .count();
}

int g_fail = 0;

template <class FR, class FT>
void bench(const char* name, Case& c, int reps, FR range_fn, FT team_fn) {
  c.restore();
  range_fn();
  Kokkos::fence();
  Case ref = c;

  c.restore();
  team_fn();
  Kokkos::fence();
  const char* field = "none";
  long ndiff = 0, nnan = 0;
  double fa = 0, fb = 0;
  std::size_t fi = 0;
  const double diff = c.compare(ref, &field, &ndiff, &nnan, &fa, &fb, &fi);
  const bool exact = (diff == 0.0);
  if (!exact) ++g_fail;

  c.restore();
  range_fn();  // warm-up
  double t0 = now();
  for (int r = 0; r < reps; ++r) range_fn();
  const double tr = (now() - t0) / reps;

  c.restore();
  team_fn();
  t0 = now();
  for (int r = 0; r < reps; ++r) team_fn();
  const double tt = (now() - t0) / reps;

  std::printf("%-14s %10.4f %10.4f %7.3f   %s %.3g %s n=%ld nan=%ld", name,
              tr * 1e3, tt * 1e3, tr / tt, exact ? "exact" : "DIFF", diff, field,
              ndiff, nnan);
  if (!exact) std::printf(" first[%zu] team=%.17g range=%.17g", fi, fa, fb);
  std::printf("\n");
}

}  // namespace

int main(int argc, char** argv) {
  roms::initialize();
  const int nx = argc > 1 ? std::atoi(argv[1]) : 256;
  const int ny = argc > 2 ? std::atoi(argv[2]) : 256;
  const int reps = argc > 3 ? std::atoi(argv[3]) : 10;

  std::printf("backend=%s tile=%dx%d reps=%d\n", roms::exec_space::name(), nx,
              ny, reps);
  std::printf("%-14s %10s %10s %7s   %s\n", "kernel", "range_ms", "team_ms",
              "r/t", "check");

  Case c(nx, ny);
  // Team scratch slices: one league at a time on every backend we run, but
  // size for one slice per league so a concurrent backend stays correct.
  std::vector<double> scratch(std::size_t(ny + 8) * 3 * (nx + 8), 0.0);
  const int LBi = c.LBi, UBi = c.UBi, LBj = c.LBj, UBj = c.UBj;
  const int IminS = c.IminS, ImaxS = c.ImaxS, JminS = c.JminS, JmaxS = c.JmaxS;
  const roms::TileBounds* t = &c.t;
  const roms::EdgeFlags* e = &c.e;
  const int nd = 11, pg = 8;
  const int krhs = 1;

#define A(f) c.f.data()

  bench("depth_flux", c, reps, [&] {
    k_step2d_depth_flux(1, 0, krhs, 1, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
                        JmaxS, A(zeta), A(h), A(on_u), A(om_v), A(ubar),
                        A(vbar), A(drhs), A(du), A(dv), 1, t);
  }, [&] {
    k_step2d_depth_flux_team(1, 0, krhs, 1, LBi, UBi, LBj, UBj, IminS, ImaxS,
                             JminS, JmaxS, A(zeta), A(h), A(on_u), A(om_v),
                             A(ubar), A(vbar), A(drhs), A(du), A(dv), 1, t,
                             scratch.data());
  });

  bench("time_average", c, reps, [&] {
    k_step2d_time_average(1, 0, krhs, 1, 0, LBi, UBi, LBj, UBj, IminS, ImaxS,
                          JminS, JmaxS, A(zeta), A(du), A(dv), A(zt_avg1),
                          A(du_avg1), A(du_avg2), A(dv_avg1), A(dv_avg2), 0.5,
                          0.25, t);
  }, [&] {
    k_step2d_time_average_team(1, 0, krhs, 1, 0, LBi, UBi, LBj, UBj, IminS,
                               ImaxS, JminS, JmaxS, A(zeta), A(du), A(dv),
                               A(zt_avg1), A(du_avg1), A(du_avg2), A(dv_avg1),
                               A(dv_avg2), 0.5, 0.25, t);
  });

  bench("zeta_step", c, reps, [&] {
    k_step2d_zeta_step(1, 0, krhs, 2, 1, 1, 1, 1, 2, 1, 0, LBi, UBi, LBj, UBj,
                       1, IminS, ImaxS, JminS, JmaxS, A(zeta), A(h), A(pm),
                       A(pn), A(du), A(dv), A(gz), A(gz2), A(gza), A(rho),
                       A(rvfrc), A(rhs_u), A(zeta_new), A(dnew), A(zwrk),
                       A(rzeta), 10.0, 1027.0, t);
  }, [&] {
    k_step2d_zeta_step_team(1, 0, krhs, 2, 1, 1, 1, 1, 2, 1, 0, LBi, UBi, LBj,
                            UBj, 1, IminS, ImaxS, JminS, JmaxS, A(zeta), A(h),
                            A(pm), A(pn), A(du), A(dv), A(gz), A(gz2), A(gza),
                            A(rho), A(rvfrc), A(rhs_u), A(zeta_new), A(dnew),
                            A(zwrk), A(rzeta), 10.0, 1027.0, t);
  });

  bench("pressure_grad", c, reps, [&] {
    k_step2d_pressure_grad(1, 0, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS,
                           A(h), A(on_u), A(om_v), A(gz), A(gz2), A(gza),
                           A(rho), A(zw), A(rhs_u), A(rhs_v), A(diau),
                           A(diav), nd, pg, 9.81, t);
  }, [&] {
    k_step2d_pressure_grad_team(1, 0, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
                                JmaxS, A(h), A(on_u), A(om_v), A(gz), A(gz2),
                                A(gza), A(rho), A(zw), A(rhs_u), A(rhs_v),
                                A(diau), A(diav), nd, pg, 9.81, t);
  });

  bench("hadv_c4", c, reps, [&] {
    k_step2d_hadv_centered4(1, 0, krhs, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
                            JmaxS, A(ubar), A(vbar), A(du), A(dv), A(grad),
                            A(dgrad), A(ufx), A(ufe), A(vfx), A(vfe), A(rhs_u),
                            A(rhs_v), A(diau), A(diav), nd, 1, 2, 3, e, t);
  }, [&] {
    k_step2d_hadv_centered4_team(1, 0, krhs, LBi, UBi, LBj, UBj, IminS, ImaxS,
                                 JminS, JmaxS, A(ubar), A(vbar), A(du), A(dv),
                                 A(grad), A(dgrad), A(ufx), A(ufe), A(vfx),
                                 A(vfe), A(rhs_u), A(rhs_v), A(diau),
                                 A(diav), nd, 1, 2, 3, e, t);
  });

  bench("coriolis", c, reps, [&] {
    k_step2d_coriolis(1, 0, krhs, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
                      JmaxS, A(drhs), A(fomn), A(ubar), A(vbar), A(ufx),
                      A(vfe), A(rhs_u), A(rhs_v), A(diau), A(diav), nd, 4, t);
  }, [&] {
    k_step2d_coriolis_team(1, 0, krhs, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
                           JmaxS, A(drhs), A(fomn), A(ubar), A(vbar), A(ufx),
                           A(vfe), A(rhs_u), A(rhs_v), A(diau), A(diav), nd,
                           4, t, scratch.data());
  });

  bench("psi_depth", c, reps, [&] {
    k_step2d_psi_depth(1, 0, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS,
                       A(drhs), A(drhspsi), t);
  }, [&] {
    k_step2d_psi_depth_team(1, 0, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
                            JmaxS, A(drhs), A(drhspsi), t);
  });

  bench("hvisc2", c, reps, [&] {
    k_step2d_hvisc2(1, 0, krhs, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS,
                    A(drhs), A(drhspsi), A(ubar), A(vbar), A(pm), A(pn),
                    A(pmr), A(pnr), A(pmp), A(pnp), A(omr), A(onr), A(omp),
                    A(onp), A(visr), A(visp), A(ufx), A(ufe), A(vfx), A(vfe),
                    A(rhs_u), A(rhs_v), A(diau), A(diav), nd, 5, 6, 7, t);
  }, [&] {
    k_step2d_hvisc2_team(1, 0, krhs, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
                         JmaxS, A(drhs), A(drhspsi), A(ubar), A(vbar), A(pm),
                         A(pn), A(pmr), A(pnr), A(pmp), A(pnp), A(omr),
                         A(onr), A(omp), A(onp), A(visr), A(visp), A(ufx),
                         A(ufe), A(vfx), A(vfe), A(rhs_u), A(rhs_v), A(diau),
                         A(diav), nd, 5, 6, 7, t);
  });

  bench("coupling", c, reps, [&] {
    k_step2d_coupling(1, 0, 1, 2, 2, 1, 0, 1, LBi, UBi, LBj, UBj, IminS,
                      ImaxS, JminS, JmaxS, A(rufrc), A(rvfrc), A(ru), A(rv),
                      A(rhs_u), A(rhs_v), A(diau), A(diav), A(diarufrc),
                      A(diarvfrc), nd, pg, 9, 10, t);
  }, [&] {
    k_step2d_coupling_team(1, 0, 1, 2, 2, 1, 0, 1, LBi, UBi, LBj, UBj, IminS,
                           ImaxS, JminS, JmaxS, A(rufrc), A(rvfrc), A(ru),
                           A(rv), A(rhs_u), A(rhs_v), A(diau), A(diav),
                           A(diarufrc), A(diarvfrc), nd, pg, 9, 10, t);
  });

  bench("momentum_step", c, reps, [&] {
    k_step2d_momentum_step(1, 0, 2, 1, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
                           JmaxS, A(zeta), A(h), A(ubar), A(vbar), A(rubar),
                           A(rvbar), A(pm), A(pn), A(rhs_u), A(rhs_v), A(dstp),
                           A(dnew), 1, 0, 10.0, t);
  }, [&] {
    k_step2d_momentum_step_team(1, 0, 2, 1, LBi, UBi, LBj, UBj, IminS, ImaxS,
                                JminS, JmaxS, A(zeta), A(h), A(ubar), A(vbar),
                                A(rubar), A(rvbar), A(pm), A(pn), A(rhs_u),
                                A(rhs_v), A(dstp), A(dnew), 1, 0, 10.0, t);
  });

  bench("store_rhs", c, reps, [&] {
    k_step2d_store_rhs(1, 0, krhs, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
                       JmaxS, A(rubar), A(rvbar), A(rhs_u), A(rhs_v), A(diaru),
                       A(diarv), A(diau), A(diav), nd, pg, t);
  }, [&] {
    k_step2d_store_rhs_team(1, 0, krhs, LBi, UBi, LBj, UBj, IminS, ImaxS,
                            JminS, JmaxS, A(rubar), A(rvbar), A(rhs_u),
                            A(rhs_v), A(diaru), A(diarv), A(diau), A(diav),
                            nd, pg, t);
  });

  bench("rhs_fused", c, reps, [&] {
    k_step2d_rhs_fused(1, 0, krhs, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
                       JmaxS, A(h), A(on_u), A(om_v), A(gz), A(gz2), A(gza),
                       A(rho), A(zw), A(drhs), A(fomn), A(ubar), A(vbar),
                       A(du), A(dv), A(pm), A(pn), A(pmr), A(pnr), A(pmp),
                       A(pnp), A(omr), A(onr), A(omp), A(onp), A(visr),
                       A(visp), A(rhs_u), A(rhs_v), A(diau), A(diav), nd, pg,
                       1, 2, 3, 4, 5, 6, 7, 9.81, e, t);
  }, [&] {
    k_step2d_rhs_fused_team(1, 0, krhs, LBi, UBi, LBj, UBj, IminS, ImaxS,
                            JminS, JmaxS, A(h), A(on_u), A(om_v), A(gz),
                            A(gz2), A(gza), A(rho), A(zw), A(drhs), A(fomn),
                            A(ubar), A(vbar), A(du), A(dv), A(pm), A(pn),
                            A(pmr), A(pnr), A(pmp), A(pnp), A(omr), A(onr),
                            A(omp), A(onp), A(visr), A(visp), A(rhs_u),
                            A(rhs_v), A(diau), A(diav), nd, pg, 1, 2, 3, 4,
                            5, 6, 7, 9.81, e, t, scratch.data());
  });

#undef A
  roms::finalize();
  return g_fail ? 1 : 0;
}
