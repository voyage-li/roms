/* Timing and exactness benchmark for the production tracer chunk kernel
** (k_step3d_t_chunks) and the TeamPolicy y-face-share variant
** (k_step3d_t_team, built with ROMS_KK_TEAM_STEP3D_T).
**
**   bench_step3d_t_kokkos --check
**   bench_step3d_t_kokkos nx ny N R reps
**   bench_step3d_t_kokkos                 (the four campaign base shapes)
**
** The fixture is the one the kernel tests use (two tracers, U3/C4 and HSIMT,
** closed edges, NDT diagnostics).  Both kernels run on separate copies of the
** same state; every element of t and diag is compared, then the two are timed
** in alternation from a restored state so platform drift hits both.  The team
** kernel needs the job submitted with -ldm_share_mode 5 and an
** -ldm_share_size that holds the patched share array; with no share it prints
** the reported size and falls back to the chunk kernel. */
#include "kokkos_step3d_t.hpp"

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <vector>

#if !defined ROMS_KOKKOS
int main() {
  std::printf("bench_step3d_t_kokkos needs a ROMS_KOKKOS build\n");
  return 1;
}
#else

#if defined(ROMS_KOKKOS_SW64)
/* Runtime LDM share configuration of the current job (athread host runtime). */
extern "C" int athread_get_ldmshare_mode();
extern "C" int athread_get_ldmshare_size();
#endif

namespace {

double median(std::vector<double> v) {
  std::sort(v.begin(), v.end());
  return v[v.size()/2];
}

double now() {
  return std::chrono::duration<double>(
      std::chrono::steady_clock::now().time_since_epoch()).count();
}

int check_share() {
#if defined(ROMS_KOKKOS_SW64)
  const int mode=athread_get_ldmshare_mode();
  const int size=athread_get_ldmshare_size();
  std::printf("LDM share: mode=%d size=%d\n",mode,size);
  using team_policy=Kokkos::TeamPolicy<roms::exec_space>;
  const int count=64*int(sizeof(int));
  Kokkos::View<int*> out("ldm_check",1);
  Kokkos::parallel_for("ldm check",
      team_policy(1,64).set_scratch_size(0,Kokkos::PerTeam(count)),
      KOKKOS_LAMBDA(const team_policy::member_type& team) {
        int* s=static_cast<int*>(team.team_scratch(0).get_shmem(count));
        s[team.team_rank()]=team.team_rank();
        team.team_barrier();
        int bad=0;
        for (int i=0;i<64;++i) bad+=s[i]!=i;
        if (team.team_rank()==0) out(0)=bad;
      });
  auto h=Kokkos::create_mirror_view_and_copy(Kokkos::HostSpace(),out);
  std::printf("team scratch cross-CPE: %s (bad=%d)\n",
      h(0)==0?"PASS":"FAIL",int(h(0)));
  return h(0)==0?0:1;
#else
  std::printf("team scratch check: skipped on this backend\n");
  return 0;
#endif
}

void bench(int nx, int ny, int levels, int R, int C, int SW, int reps) {
  roms::TracerConfig p{};
  p.LBi=p.LBj=-3; p.UBi=nx+4; p.UBj=ny+4;
  p.Istr=p.Jstr=1; p.Iend=nx; p.Jend=ny;
  p.N=levels; p.NT=p.NAT=2; p.NDT=8; p.nnew=1;
  p.xadv=1; p.yadv=2; p.hadv=3; p.vadv=4; p.vdif=5;
  p.west=p.east=p.south=p.north=1;
  p.periodic_ew=p.periodic_ns=0;
  p.dt=300.0; p.cc1=0.25; p.cc2=0.5; p.cc3=1.0/12.0;

  const int ni=p.UBi-p.LBi+1, nj=p.UBj-p.LBj+1;
  const std::size_t cells=std::size_t(ni)*nj;
  std::vector<double> pm(cells), pn(cells), hz(cells*levels);
  std::vector<double> hu(cells*levels), hv(cells*levels), zr(cells*levels);
  std::vector<double> w(cells*(levels+1)), akt(cells*(levels+1)*2);
  std::vector<double> t(cells*levels*3*2), diag(cells*levels*2*p.NDT);

  auto a2=[&](std::vector<double>& a) {
    return roms::as_2d<double>(a.data(),p.LBi,p.UBi,p.LBj,p.UBj);};
  auto a3=[&](std::vector<double>& a,int low) {
    return roms::as_3d<double>(a.data(),p.LBi,p.UBi,p.LBj,p.UBj,low,levels);};
  auto pmv=a2(pm), pnv=a2(pn);
  auto h=a3(hz,1), z=a3(zr,1), wv=a3(w,0), u=a3(hu,1), v=a3(hv,1);
  auto av=roms::as_4d<double>(akt.data(),p.LBi,p.UBi,p.LBj,p.UBj,0,levels,1,2);
  auto tv=roms::as_5d<double>(t.data(),p.LBi,p.UBi,p.LBj,p.UBj,1,levels,1,3,1,2);
  auto dv=roms::as_5d<double>(diag.data(),p.LBi,p.UBi,p.LBj,p.UBj,1,levels,1,2,1,p.NDT);

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
        for (int id=1;id<=p.NDT;++id)
          dv(i,j,k,it,id)=0.001*id+0.00001*(i+j+k+it);
      }
    }
    for (int k=0;k<=levels;++k) for (int it=1;it<=2;++it)
      av(i,j,k,it)=1e-4*(1+0.01*k+0.001*i+0.002*j+0.1*it);
  }
  int schemes[2]={0,1};
  const std::vector<double> t0=t, diag0=diag;
  auto reset=[&] {
    for (int it=1;it<=2;++it) for (int k=1;k<=levels;++k)
      for (int j=p.Jstr;j<=p.Jend;++j) for (int i=p.Istr;i<=p.Iend;++i)
        tv(i,j,k,p.nnew,it)=tv(i,j,k,3,it);
  };
  const auto range_call=[&] {
    k_step3d_t_chunks(&p,schemes,pm.data(),pn.data(),hz.data(),hu.data(),
        hv.data(),w.data(),zr.data(),akt.data(),t.data(),diag.data());
  };
  const auto team_call=[&] {
    k_step3d_t_team(&p,schemes,R,C,pm.data(),pn.data(),hz.data(),hu.data(),
        hv.data(),w.data(),zr.data(),akt.data(),t.data(),diag.data());
  };
  const auto rows_call=[&] {
    k_step3d_t_rows(&p,schemes,R,C,SW,pm.data(),pn.data(),hz.data(),hu.data(),
        hv.data(),w.data(),zr.data(),akt.data(),t.data(),diag.data());
  };
  const auto pipe_call=[&] {
    k_step3d_t_team_pipe(&p,schemes,R,C,pm.data(),pn.data(),hz.data(),
        hu.data(),hv.data(),w.data(),zr.data(),akt.data(),t.data(),
        diag.data());
  };

  /* Exactness: both kernels from the same restored state, every element. */
  t=t0; diag=diag0; reset();
  range_call();
  const std::vector<double> tr=t, dr=diag;
  t=t0; diag=diag0; reset();
  team_call();
  double et=0.0, ed=0.0, pt=0.0, pd=0.0;
  bool untouched=true;
  for (std::size_t k=0;k<t.size();++k) {
    et=std::max(et,std::abs(t[k]-tr[k]));
    pt=std::max(pt,std::abs(tr[k]));
  }
  for (std::size_t k=0;k<diag.size();++k) {
    ed=std::max(ed,std::abs(diag[k]-dr[k]));
    pd=std::max(pd,std::abs(dr[k]));
  }
  auto tv0=roms::as_5d<double>(const_cast<double*>(t0.data()),p.LBi,p.UBi,
      p.LBj,p.UBj,1,levels,1,3,1,2);
  for (int it=1;it<=2;++it) for (int state=1;state<=3;++state) {
    if (state==p.nnew) continue;
    for (int k=1;k<=levels;++k) for (int j=p.Jstr;j<=p.Jend;++j)
      for (int i=p.Istr;i<=p.Iend;++i)
        untouched &= tv(i,j,k,state,it)==tv0(i,j,k,state,it);
  }
  std::printf("team: cols=%zu N=%d R=%d C=%d max|dt|=%.3e max|d diag|=%.3e "
      "peak=%.3e/%.3e %s%s\n",std::size_t(nx)*ny,levels,R,C,et,ed,pt,pd,
      (et==0.0 && ed==0.0)?"identical":"DIFFERS",
      untouched?"":" UNTOUCHED-SLOT-CHANGED");

  /* The row-block control must match the same reference. */
  t=t0; diag=diag0; reset();
  rows_call();
  double rt=0.0, rd=0.0, rpt=0.0, rpd=0.0;
  bool r_untouched=true;
  for (std::size_t k=0;k<t.size();++k) {
    rt=std::max(rt,std::abs(t[k]-tr[k]));
    rpt=std::max(rpt,std::abs(tr[k]));
  }
  for (std::size_t k=0;k<diag.size();++k) {
    rd=std::max(rd,std::abs(diag[k]-dr[k]));
    rpd=std::max(rpd,std::abs(dr[k]));
  }
  for (int it=1;it<=2;++it) for (int state=1;state<=3;++state) {
    if (state==p.nnew) continue;
    for (int k=1;k<=levels;++k) for (int j=p.Jstr;j<=p.Jend;++j)
      for (int i=p.Istr;i<=p.Iend;++i)
        r_untouched &= tv(i,j,k,state,it)==tv0(i,j,k,state,it);
  }
  std::printf("rows: cols=%zu N=%d R=%d C=%d SW=%d max|dt|=%.3e max|d diag|=%.3e "
      "peak=%.3e/%.3e %s%s\n",std::size_t(nx)*ny,levels,R,C,SW,rt,rd,rpt,rpd,
      (rt==0.0 && rd==0.0)?"identical":"DIFFERS",
      r_untouched?"":" UNTOUCHED-SLOT-CHANGED");

  /* The pipelined team kernel must match the same reference. */
  t=t0; diag=diag0; reset();
  pipe_call();
  double qt=0.0, qd=0.0, qpt=0.0, qpd=0.0;
  for (std::size_t k=0;k<t.size();++k) {
    qt=std::max(qt,std::abs(t[k]-tr[k]));
    qpt=std::max(qpt,std::abs(tr[k]));
  }
  for (std::size_t k=0;k<diag.size();++k) {
    qd=std::max(qd,std::abs(diag[k]-dr[k]));
    qpd=std::max(qpd,std::abs(dr[k]));
  }
  std::printf("pipe: cols=%zu N=%d R=%d C=%d max|dt|=%.3e max|d diag|=%.3e "
      "peak=%.3e/%.3e %s\n",std::size_t(nx)*ny,levels,R,C,qt,qd,qpt,qpd,
      (qt==0.0 && qd==0.0)?"identical":"DIFFERS");

  /* Timing: alternate the kernels so platform drift hits all of them. */
  std::vector<double> ctimes, ttimes, rtimes, ptimes;
  for (int r=0;r<reps;++r) {
    t=t0; diag=diag0; reset();
    const double a=now();
    range_call();
    const double b=now();
    ctimes.push_back(b-a);
    t=t0; diag=diag0; reset();
    const double c=now();
    team_call();
    const double d=now();
    ttimes.push_back(d-c);
    t=t0; diag=diag0; reset();
    const double e=now();
    rows_call();
    const double f=now();
    rtimes.push_back(f-e);
    t=t0; diag=diag0; reset();
    const double g=now();
    pipe_call();
    const double h=now();
    ptimes.push_back(h-g);
  }
  const double m0=median(ctimes), m1=median(ttimes), m2=median(rtimes);
  const double m3=median(ptimes);
  std::printf("cols=%zu N=%d R=%d C=%d SW=%d chunks=%.4f team=%.4f rows=%.4f "
      "pipe=%.4f | chunks/team=%.2fx chunks/rows=%.2fx chunks/pipe=%.2fx "
      "rows/pipe=%.2fx\n",
      std::size_t(nx)*ny,levels,R,C,SW,m0,m1,m2,m3,m0/m1,m0/m2,m0/m3,m2/m3);
  std::fflush(stdout);
}

}  // namespace

int main(int argc, char** argv) {
  /* The model entry now dispatches to the row block by default; this
  ** bench compares the kernels directly, so keep the chunks entry raw. */
  setenv("ROMS_KK_T_ROWS","0",1);
  Kokkos::initialize(argc,argv);
  int rc=0;
  {
    if (argc>1 && std::string(argv[1])=="--check") {
      rc=check_share();
    } else if (argc>=8) {
      bench(std::atoi(argv[1]),std::atoi(argv[2]),std::atoi(argv[3]),
            std::atoi(argv[4]),std::atoi(argv[5]),std::atoi(argv[6]),
            std::atoi(argv[7]));
    } else {
      bench(533,1040,16,16,32,64,5);
      bench(1599,164,30,8,32,128,5);
      bench(440,1000,25,16,32,64,5);
      bench(240,352,25,16,32,64,5);
    }
  }
  Kokkos::finalize();
  return rc;
}

#endif
