/* Kernel A/B for the row-block fused 2-D right-hand side.
**
** Runs k_step2d_rhs_fused and k_step2d_rhs_fused_rows on identical
** synthetic fields, checks that every output double is bitwise equal,
** then times both (and both block sizes) alternating, reporting the
** median of the reps.  Shapes are the campaign base tiles.
**
** Only built when ROMS_KK_RHS_ROWS is enabled (see CMakeLists.txt).
** Run on the target machine; the same job must time both variants.
*/
#include "kokkos_step2d.hpp"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <vector>

namespace {

double median(std::vector<double> v) {
  std::sort(v.begin(), v.end());
  return v[v.size() / 2];
}

double now() {
  return std::chrono::duration<double>(
      std::chrono::steady_clock::now().time_since_epoch()).count();
}

constexpr int nd = 11;              /* NDM2d */
constexpr int M2pgrd = 1, M2xadv = 2, M2yadv = 3, M2hadv = 4;
constexpr int M2fcor = 5, M2hvis = 6, M2xvis = 7, M2yvis = 8;

/*
** Index ranges.  The kernels reach two cells into the ghost region
** (u_rho_flux at IstrU-1 reads i-2 and j+1; ddy at Jstr-1 reads j-2),
** so every array carries a four-cell halo and the tile interior is
** [2, nx-1] x [2, ny-1].
*/
constexpr int HALO = 4;

struct Fixture {
  int nx = 0, ny = 0;
  int LBi = 0, UBi = 0, LBj = 0, UBj = 0;
  int IminS = 0, ImaxS = 0, JminS = 0, JmaxS = 0;
  std::vector<double> h, on_u, om_v, gzeta, gzeta2, gzetaSA, rhoA, zwrk;
  std::vector<double> Drhs, fomn, ubar, vbar, DUon, DVom, pm, pn;
  std::vector<double> pmon_r, pnom_r, pmon_p, pnom_p;
  std::vector<double> om_r, on_r, om_p, on_p, visc2_r, visc2_pp;
  std::vector<double> rhs_ubar, rhs_vbar, DiaU2rhs, DiaV2rhs;
  roms::EdgeFlags e{};
  roms::TileBounds t{};
};

void fill(std::vector<double>& a, int lb_i, int ub_i, int lb_j, int ub_j,
          int levels, int lb_k, double phase) {
  const int ni = ub_i - lb_i + 1;
  const int nj = ub_j - lb_j + 1;
  a.assign(std::size_t(ni) * nj * levels, 0.0);
  for (int k = 0; k < levels; ++k)
    for (int j = lb_j; j <= ub_j; ++j)
      for (int i = lb_i; i <= ub_i; ++i)
        a[(i - lb_i) + (j - lb_j) * ni + k * ni * nj] =
            0.5 + 0.25 * std::sin(0.11 * i + 0.07 * j +
                                  0.31 * (k + lb_k) + phase);
}

void build(Fixture& f, int nx, int ny) {
  f.nx = nx;
  f.ny = ny;
  f.LBi = -HALO;              f.UBi = nx + HALO - 1;
  f.LBj = -HALO;              f.UBj = ny + HALO - 1;
  f.IminS = -HALO;            f.ImaxS = nx + HALO - 1;
  f.JminS = -HALO;            f.JmaxS = ny + HALO - 1;

  const int gl = f.LBi, gu = f.UBi, gm = f.LBj, gv = f.UBj;
  const int pi = f.IminS, pu = f.ImaxS, pj = f.JminS, pv = f.JmaxS;

  double s = 0.3;
  auto grid = [&](std::vector<double>& a) {
    fill(a, gl, gu, gm, gv, 1, 0, s); s += 0.7;
  };
  auto priv = [&](std::vector<double>& a) {
    fill(a, pi, pu, pj, pv, 1, 0, s); s += 0.7;
  };

  grid(f.h);        grid(f.on_u);     grid(f.om_v);    grid(f.rhoA);
  grid(f.fomn);     grid(f.pm);       grid(f.pn);
  grid(f.pmon_r);   grid(f.pnom_r);   grid(f.pmon_p);  grid(f.pnom_p);
  grid(f.om_r);     grid(f.on_r);     grid(f.om_p);    grid(f.on_p);
  grid(f.visc2_r);  grid(f.visc2_pp);
  priv(f.gzeta);    priv(f.gzeta2);   priv(f.gzetaSA); priv(f.zwrk);
  priv(f.Drhs);     priv(f.DUon);     priv(f.DVom);
  priv(f.rhs_ubar); priv(f.rhs_vbar);

  fill(f.ubar, gl, gu, gm, gv, 3, 1, s); s += 0.7;
  fill(f.vbar, gl, gu, gm, gv, 3, 1, s); s += 0.7;

  fill(f.DiaU2rhs, pi, pu, pj, pv, nd - 1, 1, s); s += 0.7;
  fill(f.DiaV2rhs, pi, pu, pj, pv, nd - 1, 1, s);

  /* Closed edges, the upwelling configuration: every fold is live. */
  f.e.west = f.e.east = f.e.south = f.e.north = 1;
  f.e.composite_w = f.e.composite_e = f.e.composite_s = f.e.composite_n = 0;
  f.e.periodic_ew = f.e.periodic_ns = 0;

  /* Closed edges, as in get_bounds.F: the U rows start one cell in
  ** from Istr and the V rows one row in from Jstr. */
  f.t = roms::TileBounds{};
  f.t.Istr = 2;  f.t.Iend = nx - 1;
  f.t.Jstr = 2;  f.t.Jend = ny - 1;
  f.t.IstrU = 3; f.t.JstrV = 3;
}

void invoke(Fixture& f, int rows) {
  const int LBi = f.LBi, UBi = f.UBi, LBj = f.LBj, UBj = f.UBj;
  const int IminS = f.IminS, ImaxS = f.ImaxS, JminS = f.JminS, JmaxS = f.JmaxS;
  void* h = f.h.data();                 void* on_u = f.on_u.data();
  void* om_v = f.om_v.data();           void* gzeta = f.gzeta.data();
  void* gzeta2 = f.gzeta2.data();       void* gzetaSA = f.gzetaSA.data();
  void* rhoA = f.rhoA.data();           void* zwrk = f.zwrk.data();
  void* Drhs = f.Drhs.data();           void* fomn = f.fomn.data();
  void* ubar = f.ubar.data();           void* vbar = f.vbar.data();
  void* DUon = f.DUon.data();           void* DVom = f.DVom.data();
  void* pm = f.pm.data();               void* pn = f.pn.data();
  void* pmon_r = f.pmon_r.data();       void* pnom_r = f.pnom_r.data();
  void* pmon_p = f.pmon_p.data();       void* pnom_p = f.pnom_p.data();
  void* om_r = f.om_r.data();           void* on_r = f.on_r.data();
  void* om_p = f.om_p.data();           void* on_p = f.on_p.data();
  void* visc2_r = f.visc2_r.data();     void* visc2_pp = f.visc2_pp.data();
  void* rhs_ubar = f.rhs_ubar.data();   void* rhs_vbar = f.rhs_vbar.data();
  void* diau = f.DiaU2rhs.data();       void* diav = f.DiaV2rhs.data();
  const roms::EdgeFlags* e = &f.e;
  const roms::TileBounds* t = &f.t;

  if (rows == 0) {
    k_step2d_rhs_fused(1, 0, 1, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
        JmaxS, h, on_u, om_v, gzeta, gzeta2, gzetaSA, rhoA, zwrk, Drhs, fomn,
        ubar, vbar, DUon, DVom, pm, pn, pmon_r, pnom_r, pmon_p, pnom_p, om_r,
        on_r, om_p, on_p, visc2_r, visc2_pp, rhs_ubar, rhs_vbar, diau, diav,
        nd, M2pgrd, M2xadv, M2yadv, M2hadv, M2fcor, M2hvis, M2xvis, M2yvis,
        9.81, e, t);
  } else {
    k_step2d_rhs_fused_rows(1, 0, 1, LBi, UBi, LBj, UBj, IminS, ImaxS, JminS,
        JmaxS, h, on_u, om_v, gzeta, gzeta2, gzetaSA, rhoA, zwrk, Drhs, fomn,
        ubar, vbar, DUon, DVom, pm, pn, pmon_r, pnom_r, pmon_p, pnom_p, om_r,
        on_r, om_p, on_p, visc2_r, visc2_pp, rhs_ubar, rhs_vbar, diau, diav,
        nd, M2pgrd, M2xadv, M2yadv, M2hadv, M2fcor, M2hvis, M2xvis, M2yvis,
        9.81, e, t, rows);
  }
}

double maxdiff(const std::vector<double>& a, const std::vector<double>& b) {
  double d = 0.0;
  for (std::size_t k = 0; k < a.size(); ++k) {
    const double x = std::abs(a[k] - b[k]);
    if (!(x <= d)) d = x;                /* NaN propagates as a failure */
  }
  return d;
}

struct Outs {
  std::vector<double> u, v, du, dv;
};

Outs snapshot(const Fixture& f) {
  return Outs{f.rhs_ubar, f.rhs_vbar, f.DiaU2rhs, f.DiaV2rhs};
}

double diff(const Fixture& f, const Outs& o) {
  return std::max(std::max(maxdiff(f.rhs_ubar, o.u), maxdiff(f.rhs_vbar, o.v)),
                  std::max(maxdiff(f.DiaU2rhs, o.du),
                           maxdiff(f.DiaV2rhs, o.dv)));
}

void run_shape(int nx, int ny, int reps) {
  Fixture f;
  build(f, nx, ny);

  invoke(f, 0);
  const Outs ref = snapshot(f);

  double d2 = 0.0, d4 = 0.0;
  invoke(f, 2); d2 = diff(f, ref);
  invoke(f, 4); d4 = diff(f, ref);

  /* Alternate the order so a slow drift (frequency, node) cannot favour
  ** one variant; the medians are then comparable. */
  std::vector<double> t0, t2, t4;
  invoke(f, 0); invoke(f, 2); invoke(f, 4);          /* warmup */
  for (int rep = 0; rep < reps; ++rep) {
    const int order[3] = {(rep & 1) ? 4 : 0, 2, (rep & 1) ? 0 : 4};
    double t[3];
    for (int q = 0; q < 3; ++q) {
      const double s = now();
      invoke(f, order[q]);
      t[q] = now() - s;
    }
    /* t[1] is always rows=2; the outer two swap with the rep. */
    const bool swap = (rep & 1) != 0;
    t0.push_back(t[swap ? 2 : 0]);
    t2.push_back(t[1]);
    t4.push_back(t[swap ? 0 : 2]);
  }
  const double m0 = median(t0), m2 = median(t2), m4 = median(t4);
  std::printf("shape=%dx%d base=%.4f rows2=%.4f (%.2fx) rows4=%.4f (%.2fx)"
              " diff2=%g diff4=%g\n",
              nx, ny, m0, m2, m0 / m2, m4, m0 / m4, d2, d4);
  std::fflush(stdout);
}

}  // namespace

int main(int argc, char** argv) {
  if (argc >= 3) {
    const int reps = (argc >= 4) ? std::atoi(argv[3]) : 5;
    run_shape(std::atoi(argv[1]), std::atoi(argv[2]), reps);
    return 0;
  }
  /* Campaign base tiles. */
  run_shape(533, 1040, 5);     /* UPWELLING */
  run_shape(1599, 164, 5);     /* BL_TEST */
  run_shape(240, 352, 5);      /* 12-rank tile */
  run_shape(440, 1000, 5);     /* LAB_CANYON */
  return 0;
}
