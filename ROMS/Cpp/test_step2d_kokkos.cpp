/* Direct regression for fused stencils and complete predictor diagnostics.
** Run on the target machine; it also checks the actual execution space. */
#include "kokkos_step2d.hpp"

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <vector>

namespace {
constexpr int lo = -4, hi = 16, n = hi - lo + 1, cells = n * n;
constexpr int nd = 11, pgrd = 8;
using Field = std::vector<double>;

Field field(int levels, double offset) {
  Field a(cells * levels);
  for (int k = 0; k < levels; ++k)
    for (int j = lo; j <= hi; ++j)
      for (int i = lo; i <= hi; ++i)
        a[(i - lo) + (j - lo) * n + k * cells] =
            offset + 0.03 * i + 0.017 * j * j + 0.001 * i * i * j + 0.2 * k;
  return a;
}

bool check(const Field& a, const Field& b, const char* tag) {
  double diff = 0, peak = 0;
  for (std::size_t k = 0; k < a.size(); ++k) {
    if (!std::isfinite(a[k]) || !std::isfinite(b[k])) return false;
    diff = std::max(diff, std::abs(a[k] - b[k]));
    peak = std::max(peak, std::abs(a[k]));
  }
  const bool ok = diff <= 1.e-12 * std::max(1.0, peak);
  std::printf("%s maxabs=%.17g peak=%.17g %s\n", tag, diff, peak,
              ok ? "PASS" : "FAIL");
  return ok;
}

bool stencils(int periodic, int krhs) {
  roms::TileBounds t{};
  t.Istr = t.Jstr = 1; t.Iend = 9; t.Jend = 8;
  t.IstrU = t.JstrV = periodic ? 1 : 2;
  t.IstrR = t.JstrR = 0; t.IendR = 10; t.JendR = 9;
  t.Istrm1 = t.Jstrm1 = periodic ? 0 : 1;
  t.Iendp1 = periodic ? 10 : 9; t.Jendp1 = periodic ? 9 : 8;
  t.Iendp2 = 11; t.Jendp2 = 10;
  t.IstrUm1 = t.IstrU - 1; t.JstrVm1 = t.JstrV - 1;
  t.IstrUm2 = t.IstrU - 2; t.JstrVm2 = t.JstrV - 2; t.UBk = 3;
  roms::EdgeFlags e{1, 1, 1, 1, 0, 0, 0, 0, periodic, periodic};
  auto h = field(1, 20), on_u = field(1, 2), om_v = field(1, 3);
  auto gz = field(1, 0.1), gz2 = field(1, 0.2), gza = field(1, 0.3);
  auto rho = field(1, 0.01), zw = field(1, 0.03), depth = field(1, 25);
  auto f = field(1, 0.0001), u = field(3, 0.02), v = field(3, 0.04);
  auto du = field(1, 0.3), dv = field(1, 0.4);
  auto pm = field(1, 2), pn = field(1, 4);
  auto pmr = field(1, 3), pnr = field(1, 2);
  auto pmp = field(1, 5), pnp = field(1, 2);
  auto omr = field(1, 3), onr = field(1, 4), omp = field(1, 5), onp = field(1, 6);
  auto visr = field(1, 0.5), visp = field(1, 0.7);
  Field ru(cells, 0), rv(cells, 0), fu(cells, 0), fv(cells, 0);
  Field diau(cells * (nd - 1), 0), diav = diau, fdiau = diau, fdiav = diau;
  Field grad(cells, 0), dgrad = grad, ufx = grad, ufe = grad, vfx = grad, vfe = grad, psi = grad;
  k_step2d_pressure_grad(1, 0, lo, hi, lo, hi, lo, hi, lo, hi,
      h.data(), on_u.data(), om_v.data(), gz.data(), gz2.data(), gza.data(),
      rho.data(), zw.data(), ru.data(), rv.data(), diau.data(), diav.data(), nd, pgrd, 9.81, &t);
  k_step2d_hadv_centered4(1, 0, krhs, lo, hi, lo, hi, lo, hi, lo, hi,
      u.data(), v.data(), du.data(), dv.data(), grad.data(), dgrad.data(),
      ufx.data(), ufe.data(), vfx.data(), vfe.data(), ru.data(), rv.data(),
      diau.data(), diav.data(), nd, 1, 2, 3, &e, &t);
  k_step2d_coriolis(1, 0, krhs, lo, hi, lo, hi, lo, hi, lo, hi,
      depth.data(), f.data(), u.data(), v.data(), ufx.data(), vfe.data(),
      ru.data(), rv.data(), diau.data(), diav.data(), nd, 4, &t);
  k_step2d_psi_depth(1, 0, lo, hi, lo, hi, lo, hi, lo, hi, depth.data(), psi.data(), &t);
  k_step2d_hvisc2(1, 0, krhs, lo, hi, lo, hi, lo, hi, lo, hi,
      depth.data(), psi.data(), u.data(), v.data(), pm.data(), pn.data(),
      pmr.data(), pnr.data(), pmp.data(), pnp.data(), omr.data(), onr.data(),
      omp.data(), onp.data(), visr.data(), visp.data(), ufx.data(), ufe.data(),
      vfx.data(), vfe.data(), ru.data(), rv.data(), diau.data(), diav.data(), nd, 5, 6, 7, &t);
  k_step2d_rhs_fused(1, 0, krhs, lo, hi, lo, hi, lo, hi, lo, hi,
      h.data(), on_u.data(), om_v.data(), gz.data(), gz2.data(), gza.data(),
      rho.data(), zw.data(), depth.data(), f.data(), u.data(), v.data(),
      du.data(), dv.data(), pm.data(), pn.data(), pmr.data(), pnr.data(),
      pmp.data(), pnp.data(), omr.data(), onr.data(), omp.data(), onp.data(),
      visr.data(), visp.data(), fu.data(), fv.data(), fdiau.data(), fdiav.data(),
      nd, pgrd, 1, 2, 3, 4, 5, 6, 7, 9.81, &e, &t);
  std::printf("periodic=%d krhs=%d\n", periodic, krhs);
  bool ok = check(ru, fu, "RHS U") & check(rv, fv, "RHS V") &
            check(diau, fdiau, "diagnostics U") & check(diav, fdiav, "diagnostics V");
  // Seed stress slots, which are beyond M2pgrd and must still be stored.
  for (int id = 9; id < nd; ++id)
    for (int k = 0; k < cells; ++k) {
      diau[k + (id - 1) * cells] = id + 0.01 * k;
      diav[k + (id - 1) * cells] = id - 0.02 * k;
    }
  Field rubar(cells * 2, -99), rvbar = rubar;
  Field dur(cells * 2 * (nd - 1), -99), dvr = dur;
  k_step2d_store_rhs(1, 0, 2, lo, hi, lo, hi, lo, hi, lo, hi,
      rubar.data(), rvbar.data(), ru.data(), rv.data(), dur.data(), dvr.data(),
      diau.data(), diav.data(), nd, pgrd, &t);
  for (int id = 1; id < nd; ++id)
    for (int j = 1; j <= t.Jend; ++j)
      for (int i = 1; i <= t.Iend; ++i) {
        const int p = (i - lo) + (j - lo) * n;
        if (i >= t.IstrU) ok &= dur[p + cells + (id - 1) * cells * 2] == diau[p + (id - 1) * cells];
        if (j >= t.JstrV) ok &= dvr[p + cells + (id - 1) * cells * 2] == diav[p + (id - 1) * cells];
        ok &= dur[p + (id - 1) * cells * 2] == -99;
        ok &= dvr[p + (id - 1) * cells * 2] == -99;
      }
  std::printf("complete stored diagnostics: %s\n", ok ? "PASS" : "FAIL");
  return ok;
}

bool coupling(int step, int first, int predictor, int istp) {
  roms::TileBounds t{};
  t.Istr = t.Jstr = 1; t.Iend = 9; t.Jend = 8;
  t.IstrU = t.JstrV = 2; t.UBk = 3;
  const int inew = 3 - istp;
  auto uf = field(1, 3), vf = field(1, 4);
  auto uh = field(8, 5), vh = field(8, 6);
  auto ur = field(1, 0.1), vr = field(1, 0.2);
  auto ud = field(nd - 1, 0.3), vd = field(nd - 1, 0.4);
  auto udh = field(3 * (nd - 1), 1), vdh = field(3 * (nd - 1), 2);
  auto euf = uf, evf = vf, euh = uh, evh = vh, eur = ur, evr = vr;
  auto eud = ud, evd = vd, eudh = udh, evdh = vdh;
  const bool update = first && predictor;
  const double a = step == 1 ? 1.0 : step == 2 ? 1.5 : 23.0 / 12.0;
  const double b = step == 1 ? 0.0 : step == 2 ? -0.5 : -16.0 / 12.0;
  const double c = step <= 2 ? 0.0 : 5.0 / 12.0;
  // Scalar reference reads the old time slots before replacing istp.
  auto reference = [&](Field& force, Field& hist, Field& rhs, Field& diag,
                       Field& dhist, int ilo, int jlo) {
    for (int j = jlo; j <= t.Jend; ++j)
      for (int i = ilo; i <= t.Iend; ++i) {
        const int p = i - lo + (j - lo) * n;
        if (update) {
          force[p] -= rhs[p];
          rhs[p] += a * force[p] + b * hist[p + (inew - 1) * cells * 4] +
                    c * hist[p + (istp - 1) * cells * 4];
          hist[p + (istp - 1) * cells * 4] = force[p];
        } else rhs[p] += force[p];
        for (int id = 1; id < nd; ++id) {
          const int d = p + (id - 1) * cells;
          const int h = p + (id - 1) * cells * 3;
          if (update) {
            if (id <= pgrd) dhist[h + 2 * cells] -= diag[d];
            const double value = a * dhist[h + 2 * cells] +
                b * dhist[h + (inew - 1) * cells] +
                c * dhist[h + (istp - 1) * cells];
            if (id <= pgrd) diag[d] += value;
            else diag[d] = value;
            dhist[h + (istp - 1) * cells] = dhist[h + 2 * cells];
          } else if (id <= pgrd) diag[d] += dhist[h + 2 * cells];
          else diag[d] = dhist[h + 2 * cells];
        }
      }
  };
  reference(euf, euh, eur, eud, eudh, t.IstrU, t.Jstr);
  reference(evf, evh, evr, evd, evdh, t.Istr, t.JstrV);
  k_step2d_coupling(1, 0, istp, inew, step, 1, first, predictor,
      lo, hi, lo, hi, lo, hi, lo, hi, uf.data(), vf.data(), uh.data(), vh.data(),
      ur.data(), vr.data(), ud.data(), vd.data(), udh.data(), vdh.data(),
      nd, pgrd, 9, 10, &t);
  std::printf("coupling step=%d first=%d predictor=%d istp=%d\n", step, first, predictor, istp);
  return check(euf, uf, "forcing U") & check(evf, vf, "forcing V") &
      check(euh, uh, "history U") & check(evh, vh, "history V") &
      check(eur, ur, "coupled RHS U") & check(evr, vr, "coupled RHS V") &
      check(eud, ud, "coupled diagnostics U") & check(evd, vd, "coupled diagnostics V") &
      check(eudh, udh, "diagnostic history U") & check(evdh, vdh, "diagnostic history V");
}

}  // namespace

int main() {
  roms::initialize();
  bool ok = true;
  {
    Kokkos::View<int*, Kokkos::HostSpace> ids("CPE IDs", 64);
    Kokkos::parallel_for(Kokkos::RangePolicy<roms::exec_space>(0, 64), KOKKOS_LAMBDA(int i) {
#ifdef __sw_slave__
      ids(i) = _PEN;
#else
      ids(i) = -1;
#endif
    });
    for (int i = 0; i < 64; ++i) {
#ifdef ROMS_KOKKOS_SW64
      ok &= ids(i) == i;
#else
      ok &= ids(i) == -1;
#endif
    }
    std::printf("execution-space probe: %s\n", ok ? "PASS" : "FAIL");
    for (int periodic = 0; periodic <= 1; ++periodic)
      for (int krhs = 1; krhs <= 3; ++krhs) ok &= stencils(periodic, krhs);
    for (int istp = 1; istp <= 2; ++istp) {
      for (int step = 1; step <= 3; ++step) ok &= coupling(step, 1, 1, istp);
      ok &= coupling(3, 0, 1, istp);
      ok &= coupling(3, 1, 0, istp);
    }
  }
  roms::finalize();
  return ok ? 0 : 1;
}
