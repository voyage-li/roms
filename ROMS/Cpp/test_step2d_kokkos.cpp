/* Direct regression for the step2d store, momentum and coupling kernels.
** Run on the target machine; it also checks the actual execution space.
** rhs_fused has no direct oracle here: it is validated end to end, Serial
** output exactly equal to the original Fortran. */
#include "kokkos_step2d.hpp"

#include <algorithm>
#include <cmath>
#include <cstdio>
#include <vector>

extern "C" void reference_step2d_momentum(
    int,int,int,int,int,int,int,int,int,int,int,int,int,int,double,
    const double*,const double*,const double*,const double*,const double*,
    const double*,const double*,const double*,const double*,double*,double*);

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

// Exercise every axis and nonzero/negative lower bounds independently of
// the numerical kernels. The borrowed views store only stride extents.
bool borrowed_views() {
  constexpr int li = -2, lj = 4, lk = -1, ll = 2, lm = -3;
  constexpr int ni = 4, nj = 3, nk = 2, nl = 3, nm = 2;
  Field data(ni * nj * nk * nl * nm, -99.0);
  auto a2 = roms::as_2d<double>(data.data(), li, li+ni-1, lj, lj+nj-1);
  auto a3 = roms::as_3d<double>(data.data(), li, li+ni-1, lj, lj+nj-1,
                               lk, lk+nk-1);
  auto a4 = roms::as_4d<double>(data.data(), li, li+ni-1, lj, lj+nj-1,
                               lk, lk+nk-1, ll, ll+nl-1);
  auto a5 = roms::as_5d<double>(data.data(), li, li+ni-1, lj, lj+nj-1,
                               lk, lk+nk-1, ll, ll+nl-1, lm, lm+nm-1);
  bool ok = true;
  std::size_t q = 0;
  for (int m = lm; m < lm+nm; ++m)
    for (int l = ll; l < ll+nl; ++l)
      for (int k = lk; k < lk+nk; ++k)
        for (int j = lj; j < lj+nj; ++j)
          for (int i = li; i < li+ni; ++i, ++q) {
            const int p2 = (i-li) + (j-lj)*ni;
            const int p3 = p2 + (k-lk)*ni*nj;
            const int p4 = p3 + (l-ll)*ni*nj*nk;
            ok &= &a2(i,j) == &data[p2];
            ok &= &a3(i,j,k) == &data[p3];
            ok &= &a4(i,j,k,l) == &data[p4];
            ok &= &a5(i,j,k,l,m) == &data[q];
            a5(i,j,k,l,m) = double(q) + 0.25;
            ok &= data[q] == double(q) + 0.25;
          }
  std::printf("borrowed views rank 2-5: %s\n", ok ? "PASS" : "FAIL");
  return ok;
}

// store_rhs copies the predictor RHS and every diagnostic slot, including
// the stress slots beyond M2pgrd, into time level krhs and nowhere else.
bool store(int periodic) {
  roms::TileBounds t{};
  t.Istr = t.Jstr = 1; t.Iend = 9; t.Jend = 8;
  t.IstrU = t.JstrV = periodic ? 1 : 2;
  auto ru = field(1, 0.1), rv = field(1, 0.2);
  auto diau = field(nd - 1, 0.3), diav = field(nd - 1, 0.4);
  Field rubar(cells * 2, -99), rvbar = rubar;
  Field dur(cells * 2 * (nd - 1), -99), dvr = dur;
  k_step2d_store_rhs(1, 0, 2, lo, hi, lo, hi, lo, hi, lo, hi,
      rubar.data(), rvbar.data(), ru.data(), rv.data(), dur.data(), dvr.data(),
      diau.data(), diav.data(), nd, &t);
  bool ok = true;
  for (int j = t.Jstr; j <= t.Jend; ++j)
    for (int i = t.Istr; i <= t.Iend; ++i) {
      const int p = (i - lo) + (j - lo) * n;
      if (i >= t.IstrU) ok &= rubar[p + cells] == ru[p];
      if (j >= t.JstrV) ok &= rvbar[p + cells] == rv[p];
      ok &= rubar[p] == -99 && rvbar[p] == -99;
      for (int id = 1; id < nd; ++id) {
        if (i >= t.IstrU) ok &= dur[p + cells + (id - 1) * cells * 2] == diau[p + (id - 1) * cells];
        if (j >= t.JstrV) ok &= dvr[p + cells + (id - 1) * cells * 2] == diav[p + (id - 1) * cells];
        ok &= dur[p + (id - 1) * cells * 2] == -99;
        ok &= dvr[p + (id - 1) * cells * 2] == -99;
      }
    }
  std::printf("store_rhs periodic=%d: %s\n", periodic, ok ? "PASS" : "FAIL");
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

// Exact, not the looser stencil tolerance: these one-ULP seeds can be
// amplified by KPP. Include the actual first-step rounding counterexamples,
// nonuniform fields, all LF/AM3 branches, both old slots and untouched halos.
bool momentum(int mode,int kstp,int fixture) {
  roms::TileBounds t{};
  t.Istr=t.Jstr=1; t.Iend=13; t.Jend=8; t.IstrU=2; t.JstrV=1;
  const int knew=3;
  auto h=field(1,10),pm=field(1,0.001),pn=field(1,0.0008);
  auto z=field(3,0.001),u=field(3,0.02),v=field(3,0.03);
  auto ru=field(2,0.1),rv=field(2,0.2);
  auto rhsu=field(1,0.758),rhsv=field(1,169.5),dn=field(1,10.4);
  if(fixture<2) {
    const double p=fixture==0?0.002106722106722107:0.0024242424242424242;
    const double d=fixture==0?20.81054889196482:15.597233823054959;
    std::fill(pm.begin(),pm.end(),0.5*p);
    std::fill(pn.begin(),pn.end(),0.0008);
    std::fill(h.begin(),h.end(),0.5*d);
    std::fill(dn.begin(),dn.end(),0.5*d);
    std::fill(z.begin(),z.end(),0.0);
    std::fill(u.begin(),u.end(),0.0); std::fill(v.begin(),v.end(),0.0);
    std::fill(rhsu.begin(),rhsu.end(),0.7586775314472525);
    std::fill(rhsv.begin(),rhsv.end(),169.5796069252924);
  }
  auto eu=u,ev=v; Field ds(cells,-999);
  const int first=mode==0,predictor=mode!=2;
  reference_step2d_momentum(lo,hi,lo,hi,t.Istr,t.Iend,t.Jstr,t.Jend,
      t.IstrU,t.JstrV,kstp,knew,predictor,first,5.0,z.data(),h.data(),
      pm.data(),pn.data(),ru.data(),rv.data(),rhsu.data(),rhsv.data(),
      dn.data(),eu.data(),ev.data());
  k_step2d_momentum_step(1,0,kstp,knew,lo,hi,lo,hi,lo,hi,lo,hi,
      z.data(),h.data(),u.data(),v.data(),ru.data(),rv.data(),pm.data(),
      pn.data(),rhsu.data(),rhsv.data(),ds.data(),dn.data(),predictor,first,5.0,&t);
  int bad=0;double error=0;
  for(std::size_t k=0;k<u.size();++k) {
    if(!std::isfinite(u[k])||!std::isfinite(v[k])||u[k]!=eu[k]||v[k]!=ev[k])++bad;
    error=std::max(error,std::max(std::abs(u[k]-eu[k]),std::abs(v[k]-ev[k])));
  }
  std::printf("momentum mode=%d kstp=%d fixture=%d exact-differences=%d maxabs=%.17g %s\n",
      mode,kstp,fixture,bad,error,bad?"FAIL":"PASS");
  return bad==0;
}

}  // namespace

int main() {
  roms::initialize();
  bool ok = borrowed_views();
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
    for (int periodic = 0; periodic <= 1; ++periodic) ok &= store(periodic);
    for(int old=1;old<=2;++old)
      for(int mode=0;mode<3;++mode)
        for(int fixture=0;fixture<3;++fixture) ok &= momentum(mode,old,fixture);
    for (int istp = 1; istp <= 2; ++istp) {
      for (int step = 1; step <= 3; ++step) ok &= coupling(step, 1, 1, istp);
      ok &= coupling(3, 0, 1, istp);
      ok &= coupling(3, 1, 0, istp);
    }
  }
  roms::finalize();
  return ok ? 0 : 1;
}
