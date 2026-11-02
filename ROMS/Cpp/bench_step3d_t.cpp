/*
** Standalone timing of the migrated step3d_t column kernel at several tile
** sizes.  The fixture is the same shape as the validation case (two tracers,
** U3/C4 temperature and HSIMT/HSIMT salinity, closed edges), so the numbers
** can be compared with the module wall times printed by the model.
**
**   bench_step3d_t [reps]
**
** Each call is timed individually on the host clock.  The mutable time level
** is restored from the immutable predictor slot before every call and that
** reset is outside the timed region.  Scratch allocation happens in the
** untimed warm-up call.
*/

#include "kokkos_step3d_t.hpp"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <vector>

namespace {

double median(std::vector<double> v) {
  std::sort(v.begin(), v.end());
  return v[v.size()/2];
}

void bench(int nx, int ny, int levels, int ndt, int reps) {
  roms::TracerConfig p{};
  p.LBi = p.LBj = -3;
  p.UBi = nx+4;
  p.UBj = ny+4;
  p.Istr = p.Jstr = 1;
  p.Iend = nx;
  p.Jend = ny;
  p.N = levels;
  p.NT = p.NAT = 2;
  p.NDT = ndt;
  p.nnew = 1;
  p.xadv=1; p.yadv=2; p.hadv=3; p.vadv=4; p.vdif=5;
  p.west = p.east = p.south = p.north = 1;
  p.periodic_ew = p.periodic_ns = 0;
  p.dt = 300.0;
  p.cc1 = 0.25; p.cc2 = 0.5; p.cc3 = 1.0/12.0;

  const int ni = p.UBi-p.LBi+1, nj = p.UBj-p.LBj+1;
  const std::size_t cells = std::size_t(ni)*nj;
  const std::size_t columns = std::size_t(nx)*ny;
  std::vector<double> pm(cells), pn(cells);
  std::vector<double> hz(cells*levels), hu(cells*levels), hv(cells*levels);
  std::vector<double> zr(cells*levels);
  std::vector<double> w(cells*(levels+1)), akt(cells*(levels+1)*2);
  std::vector<double> t(cells*levels*3*2), diag(cells*levels*2*ndt);
  const double bytes = 8.0*(pm.size()+pn.size()+4.0*hz.size()+w.size()+
      akt.size()+t.size()+diag.size());

  auto a2=[&](std::vector<double>& a){return roms::as_2d<double>(a.data(),p.LBi,p.UBi,p.LBj,p.UBj);};
  auto a3=[&](std::vector<double>& a,int low){
    return roms::as_3d<double>(a.data(),p.LBi,p.UBi,p.LBj,p.UBj,low,levels);};
  auto pmv=a2(pm), pnv=a2(pn);
  auto h=a3(hz,1), z=a3(zr,1), wv=a3(w,0), u=a3(hu,1), v=a3(hv,1);
  auto av=roms::as_4d<double>(akt.data(),p.LBi,p.UBi,p.LBj,p.UBj,0,levels,1,2);
  auto tv=roms::as_5d<double>(t.data(),p.LBi,p.UBi,p.LBj,p.UBj,1,levels,1,3,1,2);
  auto dv=roms::as_5d<double>(diag.data(),p.LBi,p.UBi,p.LBj,p.UBj,1,levels,1,2,1,ndt);

  for (int j=p.LBj;j<=p.UBj;++j) for (int i=p.LBi;i<=p.UBi;++i) {
    pmv(i,j)=0.001*(1+0.01*i);
    pnv(i,j)=0.0009*(1+0.01*j);
    wv(i,j,0)=0.0;
    for (int k=1;k<=levels;++k) {
      h(i,j,k)=4.0+0.2*k+0.01*i+0.02*j;
      z(i,j,k)=5.0*k+0.01*i+0.02*j;
      u(i,j,k)=5000.0*std::sin(0.3*i+0.2*j+k);
      v(i,j,k)=4000.0*std::cos(0.2*i-0.3*j+k);
      wv(i,j,k)=100.0*std::sin(0.3*i+j+k);
      for (int it=1;it<=2;++it) {
        for (int state=1;state<=3;++state) {
          const double q=10.0*it+0.03*i*i+0.02*j*j+
              0.1*std::sin(0.5*i+j+k)+0.07*k;
          tv(i,j,k,state,it)=state==p.nnew?h(i,j,k)*q:q+0.1*(state-3);
        }
        for (int id=1;id<=ndt;++id) dv(i,j,k,it,id)=0.001*id+0.00001*(i+j+k+it);
      }
    }
    for (int k=0;k<=levels;++k) for (int it=1;it<=2;++it)
      av(i,j,k,it)=1e-4*(1+0.01*k+0.001*i+0.002*j+0.1*it);
  }
  int schemes[2]={0,1};
  auto call=[&]{
    k_step3d_t(&p,schemes,pm.data(),pn.data(),hz.data(),hu.data(),hv.data(),
        w.data(),zr.data(),akt.data(),t.data(),diag.data());
  };
  call();  // warm-up, also sizes the scratch
  std::vector<double> times;
  for (int r=0;r<reps;++r) {
    for (int it=1;it<=2;++it) for (int k=1;k<=levels;++k)
      for (int j=p.Jstr;j<=p.Jend;++j) for (int i=p.Istr;i<=p.Iend;++i)
        tv(i,j,k,p.nnew,it)=tv(i,j,k,3,it);
    const auto t0=std::chrono::steady_clock::now();
    call();
    const auto t1=std::chrono::steady_clock::now();
    times.push_back(std::chrono::duration<double>(t1-t0).count());
  }
  const double best=*std::min_element(times.begin(),times.end());
  const double updates=double(columns)*levels*2.0;
  std::printf("%-6s cols=%-7zu N=%-3d updates=%-9.0f ndt=%-3d "
      "best=%8.4f s median=%8.4f s %6.2f ns/update footprint=%5.2f GB\n",
      roms::exec_space::name(),columns,levels,updates,ndt,best,
      median(times),1e9*best/updates,bytes/1e9);
  std::fflush(stdout);
}

}  // namespace

int main(int argc, char** argv) {
  Kokkos::initialize(argc, argv);
  {
    const int reps = argc > 1 ? std::atoi(argv[1]) : 3;
    const int ndt = argc > 2 ? std::atoi(argv[2]) : 20;
    std::printf("step3d_t kernel benchmark, %d reps, %d diagnostics\n", reps, ndt);
    bench(254, 254, 16, ndt, reps);
    bench(510, 510, 16, ndt, reps);
    bench(254, 254, 32, ndt, reps);
    bench(510, 510, 32, ndt, reps);
  }
  Kokkos::finalize();
  return 0;
}
