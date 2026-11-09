/* U3/C4 and HSIMT tracer advection followed by spline vertical diffusion.
** Predictor slot 3 is read-only. Boundary exchange and epilogue stay in
** Fortran; this entry is called only after whole-tile eligibility checks
** in ROMS/Nonlinear/step3d_t.F. */
#include "kokkos_step3d_t.hpp"
#if defined ROMS_KOKKOS
#include <cmath>
#include <cstdio>
#include <cstdlib>
#ifndef __sw_slave__
#include "kokkos_workspace.hpp"
#endif
#if defined(ROMS_KOKKOS_SW64) && !defined(__sw_slave__)
/* Runtime LDM share configuration of the current job (atherad host runtime). */
extern "C" int athread_get_ldmshare_mode();
extern "C" int athread_get_ldmshare_size();
#endif

namespace {
using roms::TracerConfig;
struct Slope { double gradient, ka; };

#ifndef __sw_slave__
/* Raw host ownership needs no Kokkos service at thread/process teardown.
** The SW64 HostSpace and borrowed Fortran allocations share this memory.
** Each host thread has independent scratch; all supported launches join. */
thread_local roms::HostWorkspace workspace;
#endif

struct Fields {
  TracerConfig p;
  roms::Array2d<double> pm, pn;
  roms::Array3d<double> hz, huon, hvom, w, zr;
  roms::Array4d<double> akt;
  roms::Array5d<double> t, diag;

  KOKKOS_INLINE_FUNCTION
  double difference(int i, int j, int k, int it, bool xi) const {
    if (xi) {
      if (!p.periodic_ew && !p.composite_w && p.west && i == p.Istr-1) ++i;
      else if (!p.periodic_ew && !p.composite_e && p.east && i == p.Iend+2) --i;
      return t(i,j,k,3,it)-t(i-1,j,k,3,it);
    }
    if (!p.periodic_ns && !p.composite_s && p.south && j == p.Jstr-1) ++j;
    else if (!p.periodic_ns && !p.composite_n && p.north && j == p.Jend+2) --j;
    return t(i,j,k,3,it)-t(i,j-1,k,3,it);
  }

  KOKKOS_INLINE_FUNCTION
  double curvature(int i, int j, int k, int it, bool xi) const {
    return difference(i+(xi?1:0),j+(xi?0:1),k,it,xi)-difference(i,j,k,it,xi);
  }

  KOKKOS_INLINE_FUNCTION
  Slope horizontal_slope(int i, int j, int k, int it, bool xi) const {
    if (xi) {
      if (!p.periodic_ew && p.west && i == p.Istr-1 && huon(p.Istr,j,k)>=0) return {0,0};
      if (!p.periodic_ew && p.east && i == p.Iend+2 && huon(p.Iend+1,j,k)<0) return {0,0};
      const double c = 0.125*(pm(i-1,j)+pm(i,j))*(pn(i-1,j)+pn(i,j))*p.dt;
      const double c1 = c*(1.0/hz(i-1,j,k)+1.0/hz(i,j,k));
      return {t(i,j,k,3,it)-t(i-1,j,k,3,it), 1.0-std::abs(huon(i,j,k)*c1)};
    }
    if (!p.periodic_ns && p.south && j == p.Jstr-1 && hvom(i,p.Jstr,k)>=0) return {0,0};
    if (!p.periodic_ns && p.north && j == p.Jend+2 && hvom(i,p.Jend+1,k)<0) return {0,0};
    const double c = 0.125*(pn(i,j)+pn(i,j-1))*(pm(i,j)+pm(i,j-1))*p.dt;
    const double c1 = c*(1.0/hz(i,j,k)+1.0/hz(i,j-1,k));
    return {t(i,j,k,3,it)-t(i,j-1,k,3,it), 1.0-std::abs(hvom(i,j,k)*c1)};
  }

  KOKKOS_INLINE_FUNCTION
  double correction(Slope s, Slope neighbor, double inverse) const {
    double r=0, rka=0;
    if (std::abs(s.gradient)>1.0e-12) {
      r=neighbor.gradient/s.gradient;
      rka=neighbor.ka*inverse;
    }
    const double a=p.cc1*s.ka+p.cc2-p.cc3*inverse;
    const double b=-p.cc1*s.ka+p.cc2+p.cc3*inverse;
    const double beta=a+b*r;
    const double limit=Kokkos::max(0.0,Kokkos::min(2.0,Kokkos::min(2.0*r*rka,beta)));
    return 0.5*limit*s.gradient*s.ka;
  }

  KOKKOS_INLINE_FUNCTION
  double horizontal(int i, int j, int k, int it, bool xi, int scheme) const {
    const int di=xi?1:0, dj=xi?0:1;
    const double transport=xi?huon(i,j,k):hvom(i,j,k);
    if (scheme==0) {
      return transport*0.5*(t(i-di,j-dj,k,3,it)+t(i,j,k,3,it))-
          (1.0/6.0)*(curvature(i-di,j-dj,k,it,xi)*Kokkos::max(transport,0.0)+
                     curvature(i,j,k,it,xi)*Kokkos::min(transport,0.0));
    }
    const Slope s=horizontal_slope(i,j,k,it,xi);
    const double inverse=s.ka<=1.0e-12?0.0:1.0/Kokkos::max(s.ka,1.0e-12);
    const bool positive=transport>=0.0;
    const Slope neighbor=horizontal_slope(i+(positive?-di:di),j+(positive?-dj:dj),k,it,xi);
    const double delta=correction(s,neighbor,inverse);
    const double value=positive?t(i-di,j-dj,k,3,it)+delta:t(i,j,k,3,it)-delta;
    return value*transport;
  }

  KOKKOS_INLINE_FUNCTION
  Slope vertical_slope(int i, int j, int k, int it) const {
    if (k==0 || k==p.N) return {0,0};
    const double c=pm(i,j)*pn(i,j)*p.dt;
    return {t(i,j,k+1,3,it)-t(i,j,k,3,it),
            1.0-std::abs(c*w(i,j,k)/(zr(i,j,k+1)-zr(i,j,k)))};
  }

  KOKKOS_INLINE_FUNCTION
  double vertical(int i, int j, int k, int it, int scheme) const {
    if (k==0 || k==p.N) return 0.0;
    const double velocity=w(i,j,k);
    if (scheme==0) {
      if (k==1) return velocity*(0.5*t(i,j,1,3,it)+(7.0/12.0)*t(i,j,2,3,it)-
                                (1.0/12.0)*t(i,j,3,3,it));
      if (k==p.N-1) return velocity*(0.5*t(i,j,p.N,3,it)+(7.0/12.0)*t(i,j,p.N-1,3,it)-
                                    (1.0/12.0)*t(i,j,p.N-2,3,it));
      return velocity*((7.0/12.0)*(t(i,j,k,3,it)+t(i,j,k+1,3,it))-
                       (1.0/12.0)*(t(i,j,k-1,3,it)+t(i,j,k+2,3,it)));
    }
    if (k==1 && velocity>=0) return velocity*t(i,j,k,3,it);
    if (k==p.N-1 && velocity<0) return velocity*t(i,j,k+1,3,it);
    const Slope s=vertical_slope(i,j,k,it);
    const Slope neighbor=vertical_slope(i,j,k+(velocity>=0?-1:1),it);
    const double delta=correction(s,neighbor,1.0/s.ka);
    return velocity*(velocity>=0?t(i,j,k,3,it)+delta:t(i,j,k+1,3,it)-delta);
  }
};
}  // namespace

/*
** A work item owns one (j, tracer) row of the tile and walks it in chunks
** of KB columns, level by level with i innermost: the Fortran loop order,
** which keeps the working set of a chunk in CPE cache.  Each xi-face flux
** is evaluated once per chunk and shared by the two columns beside it.
** Per point the operations and their order are those of step3d_t.F, so
** the Serial result equals the Fortran exactly.
*/
#if defined(ROMS_KK_TEAM_STEP3D_T)
/* Defined below; the row-block entry has the same contract. */
extern "C" void k_step3d_t_rows(const TracerConfig* config, const int* schemes,
    int block_rows, int chunk_cols, int strip_cols, void* pm, void* pn,
    void* Hz, void* Huon, void* Hvom, void* W, void* zr, void* Akt,
    void* tracer, void* diagnostics);
#endif

extern "C" void k_step3d_t_chunks(const TracerConfig* config, const int* schemes,
    void* pm, void* pn, void* Hz, void* Huon, void* Hvom, void* W,
    void* zr, void* Akt, void* tracer, void* diagnostics) {
  roms::initialize();
#if defined(ROMS_KK_TEAM_STEP3D_T)
  /* Row-block kernel by default in builds that carry it: bitwise equal,
  ** one pass over the data, no LDM and no per-job flags.  ROMS_KK_T_ROWS
  ** overrides it (0 = production chunk kernel, 2 or 4 = block size); the
  ** value is read once per process so one binary can be timed both ways.
  ** Strip width follows the tile: the bench's best was 64 columns at
  ** nx=533 and 128 at nx=1599. */
  static const int t_rows = []() {
    const char* v = std::getenv("ROMS_KK_T_ROWS");
    return (v != nullptr) ? std::atoi(v) : 1;
  }();
  if (t_rows > 0) {
    const int nx = config->Iend - config->Istr + 1;
    const int strip = (nx >= 1024) ? 128 : 64;
    const int rows = (t_rows >= 2) ? t_rows : 8;
    k_step3d_t_rows(config, schemes, rows, 32, strip, pm, pn, Hz, Huon,
                    Hvom, W, zr, Akt, tracer, diagnostics);
    return;
  }
#endif
  ROMS_KK_TIMER("step3d_t chunks");
  const TracerConfig p=*config;
  if (p.N<3 || p.NT<1 || p.NAT<1 || p.nnew<1 || p.nnew>2)
    Kokkos::abort("Invalid Kokkos tracer dimensions or predictor alias");
  const int nx=p.Iend-p.Istr+1, ny=p.Jend-p.Jstr+1;
  if (nx<1 || ny<1) Kokkos::abort("Invalid tracer tile");
  for (int it=0;it<p.NT;++it)
    if (schemes[it]!=0 && schemes[it]!=1) Kokkos::abort("Unsupported tracer scheme");
  constexpr int KB=32;
  const int per=(KB+1)+KB+2*(p.N+1)*KB;   /* fx, oh, cf, dc */
  double* scratch=nullptr;
#ifndef __sw_slave__
  scratch=workspace.reserve(std::size_t(ny)*p.NT*per);
#endif
  const auto a2=[&](void* v) {return roms::as_2d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj);};
  const auto a3=[&](void* v,int low) {return roms::as_3d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj,low,p.N);};
  const Fields f{p,a2(pm),a2(pn),a3(Hz,1),a3(Huon,1),a3(Hvom,1),a3(W,0),a3(zr,1),
      roms::as_4d<double>(Akt,p.LBi,p.UBi,p.LBj,p.UBj,0,p.N,1,p.NAT),
      roms::as_5d<double>(tracer,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,3,1,p.NT),
      roms::as_5d<double>(diagnostics,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,p.NT,1,p.NDT)};
  Kokkos::parallel_for("ROMS step3d_t chunks",
      Kokkos::RangePolicy<roms::exec_space,Kokkos::IndexType<int>>(0,ny*p.NT),
      KOKKOS_LAMBDA(int index) {
        const int j=p.Jstr+index%ny;
        const int it=1+index/ny;
        const int scheme=schemes[it-1];
        const int N=p.N, nn=p.nnew;
        const int tracer=it<p.NAT?it:p.NAT;
        double* fx=scratch+std::size_t(index)*per;
        double* oh=fx+(KB+1);
        double* cf=oh+KB;
        double* dc=cf+(N+1)*KB;
        for (int ib=p.Istr;ib<=p.Iend;ib+=KB) {
          const int nb=(p.Iend-ib+1<KB)?(p.Iend-ib+1):KB;
          /* Advection and the unit conversion of the diagnostics. */
          for (int k=1;k<=N;++k) {
            for (int b=0;b<=nb;++b) fx[b]=f.horizontal(ib+b,j,k,it,true,scheme);
            for (int b=0;b<nb;++b) {
              const int i=ib+b;
              const double c=p.dt*f.pm(i,j)*f.pn(i,j);
              const double dx=c*(fx[b+1]-fx[b]);
              const double dy=c*(f.horizontal(i,j+1,k,it,false,scheme)-
                                 f.horizontal(i,j,k,it,false,scheme));
              const double d=dx+dy;
              f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)-d;
              const double dz=c*(f.vertical(i,j,k,it,scheme)-f.vertical(i,j,k-1,it,scheme));
              f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)-dz;
              oh[b]=1.0/f.hz(i,j,k);
              f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)*oh[b];
              f.diag(i,j,k,it,p.xadv)=-dx;
              f.diag(i,j,k,it,p.yadv)=-dy;
              f.diag(i,j,k,it,p.hadv)=-d;
              f.diag(i,j,k,it,p.vadv)=-dz;
            }
            for (int id=1;id<=p.NDT;++id)
              for (int b=0;b<nb;++b)
                f.diag(ib+b,j,k,it,id)=f.diag(ib+b,j,k,it,id)*oh[b];
          }
          /* Implicit spline vertical diffusion. */
          for (int b=0;b<nb;++b) { cf[b]=0.0; dc[b]=0.0; }
          for (int k=1;k<N;++k) {
            for (int b=0;b<nb;++b) {
              const int i=ib+b, q=k*KB+b;
              const double o=1.0/f.hz(i,j,k), o1=1.0/f.hz(i,j,k+1);
              const double lower=(1.0/6.0)*f.hz(i,j,k)-p.dt*f.akt(i,j,k-1,tracer)*o;
              const double upper=(1.0/6.0)*f.hz(i,j,k+1)-p.dt*f.akt(i,j,k+1,tracer)*o1;
              const double bb=(1.0/3.0)*(f.hz(i,j,k)+f.hz(i,j,k+1))+
                              p.dt*f.akt(i,j,k,tracer)*(o+o1);
              const double inverse=1.0/(bb-lower*cf[q-KB]);
              cf[q]=inverse*upper;
              dc[q]=inverse*(f.t(i,j,k+1,nn,it)-f.t(i,j,k,nn,it)-lower*dc[q-KB]);
            }
          }
          for (int b=0;b<nb;++b) dc[N*KB+b]=0.0;
          for (int k=N-1;k>=1;--k)
            for (int b=0;b<nb;++b) dc[k*KB+b]=dc[k*KB+b]-cf[k*KB+b]*dc[(k+1)*KB+b];
          for (int k=1;k<=N;++k) {
            for (int b=0;b<nb;++b) {
              const int i=ib+b, q=k*KB+b;
              dc[q]=dc[q]*f.akt(i,j,k,tracer);
              const double change=p.dt*(1.0/f.hz(i,j,k))*(dc[q]-dc[q-KB]);
              f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)+change;
              f.diag(i,j,k,it,p.vdif)=f.diag(i,j,k,it,p.vdif)+change;
            }
          }
        }
      });
}

#if defined(ROMS_KK_TEAM_STEP3D_T)
/*
** TeamPolicy variant of the chunk kernel.  One team of 64 CPEs owns a block of
** R rows and all its columns.  The y-face flux of each of the block's R+1
** faces is evaluated once into the team-shared scratch and read by the one or
** two rows that border it; the x-face flux keeps the chunk kernel's rolling
** 32-column buffer.  The per-cell expressions and their order are those of
** k_step3d_t_chunks, so the two kernels agree exactly.
**
** block_rows selects R (0 = 4).  The host entry asks the runtime for the LDM
** share size and falls back to the chunk kernel when the job was not submitted
** with -ldm_share_mode 5, so a missing flag degrades visibly instead of
** faulting on the first scratch access.
*/
extern "C" void k_step3d_t_team(const TracerConfig* config, const int* schemes,
    int block_rows, int chunk_cols, void* pm, void* pn, void* Hz, void* Huon,
    void* Hvom, void* W, void* zr, void* Akt, void* tracer, void* diagnostics) {
  roms::initialize();
  ROMS_KK_TIMER("step3d_t team");
  const TracerConfig p=*config;
  if (p.N<3 || p.NT<1 || p.NAT<1 || p.nnew<1 || p.nnew>2)
    Kokkos::abort("Invalid Kokkos tracer dimensions or predictor alias");
  const int nx=p.Iend-p.Istr+1, ny=p.Jend-p.Jstr+1;
  if (nx<1 || ny<1) Kokkos::abort("Invalid tracer tile");
  for (int it=0;it<p.NT;++it)
    if (schemes[it]!=0 && schemes[it]!=1) Kokkos::abort("Unsupported tracer scheme");
  constexpr int CM=32;   /* widest supported column chunk (stack buffers) */
  const int C=(chunk_cols>0 && chunk_cols<CM)?chunk_cols:CM;
  using team_policy=Kokkos::TeamPolicy<roms::exec_space>;
  using member=team_policy::member_type;
#if defined(ROMS_KOKKOS_SW64)
  constexpr int team_size=64;   /* one core group; the backend fixes this */
#else
  constexpr int team_size=1;    /* the Serial backend rejects larger teams */
#endif
  const int per_member=((2*(p.N+1)+15)/16)*16;   /* 128-byte private slices */
  const std::size_t cap=std::size_t(3)*1024*1024+768*1024;  /* patched share */
  const auto need=[&](int rows) {
    return (std::size_t(rows+1)*std::size_t(nx)*std::size_t(p.N)+
            std::size_t(team_size)*std::size_t(per_member))*sizeof(double);
  };
  int R=block_rows>0?block_rows:4;
  if (R>ny) R=ny;
  while (R>1 && need(R)>cap) R/=2;
  if (need(R)>cap) {   /* no block size fits the share: keep the production path */
    k_step3d_t_chunks(config,schemes,pm,pn,Hz,Huon,Hvom,W,zr,Akt,tracer,diagnostics);
    return;
  }
#ifndef __sw_slave__
#if defined(ROMS_KOKKOS_SW64)
  {
    static bool reported=false;
    const int mode=athread_get_ldmshare_mode();
    const int share=athread_get_ldmshare_size();
    if (!reported) {
      std::printf("step3d_t team: LDM share mode=%d size=%d, R=%d, "
                  "request %.2f MB\n", mode, share, R,
                  double(need(R))/1048576.0);
      reported=true;
    }
    if (share<=0) {
      k_step3d_t_chunks(config,schemes,pm,pn,Hz,Huon,Hvom,W,zr,Akt,tracer,diagnostics);
      return;
    }
  }
#endif
#endif
  const auto a2=[&](void* v) {return roms::as_2d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj);};
  const auto a3=[&](void* v,int low) {return roms::as_3d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj,low,p.N);};
  const Fields f{p,a2(pm),a2(pn),a3(Hz,1),a3(Huon,1),a3(Hvom,1),a3(W,0),a3(zr,1),
      roms::as_4d<double>(Akt,p.LBi,p.UBi,p.LBj,p.UBj,0,p.N,1,p.NAT),
      roms::as_5d<double>(tracer,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,3,1,p.NT),
      roms::as_5d<double>(diagnostics,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,p.NT,1,p.NDT)};
  const int blocks=(ny+R-1)/R;
  const int nchunks=(nx+C-1)/C;
  const std::size_t yf_count=std::size_t(R+1)*std::size_t(nx)*std::size_t(p.N);
  const std::size_t shmem=(yf_count+
      std::size_t(team_size)*std::size_t(per_member))*sizeof(double);
  Kokkos::parallel_for("ROMS step3d_t team",
      team_policy(blocks,team_size).set_scratch_size(0,Kokkos::PerTeam(shmem)),
      KOKKOS_LAMBDA(const member& team) {
        const int block=team.league_rank();
        const int j0=p.Jstr+block*R;
        const int rows=Kokkos::min(R,p.Jend-j0+1);
        double* base=static_cast<double*>(team.team_scratch(0).get_shmem(shmem));
        double* yf=base;
        double* cf=base+yf_count+std::size_t(team.team_rank())*std::size_t(per_member);
        double* dc=cf+p.N+1;
        for (int it=1;it<=p.NT;++it) {
          const int scheme=schemes[it-1];
          const int tracer=it<p.NAT?it:p.NAT;
          const int nn=p.nnew;
          /* y-face fluxes of the block's rows+1 faces, evaluated once. */
          Kokkos::parallel_for(Kokkos::TeamThreadRange(team,(rows+1)*nchunks),
              [&](int index) {
                const int face=index/nchunks, chunk=index%nchunks;
                const int ib=p.Istr+chunk*C;
                const int nb=(p.Iend-ib+1<C)?(p.Iend-ib+1):C;
                const int jface=j0+face;
                double* out=yf+std::size_t(face)*std::size_t(p.N)*std::size_t(nx)+
                    std::size_t(ib-p.Istr);
                for (int k=1;k<=p.N;++k)
                  for (int b=0;b<nb;++b)
                    out[std::size_t(k-1)*std::size_t(nx)+b]=
                        f.horizontal(ib+b,jface,k,it,false,scheme);
              });
          team.team_barrier();
          /* The chunk kernel's advection nest with the shared y flux. */
          Kokkos::parallel_for(Kokkos::TeamThreadRange(team,rows*nchunks),
              [&](int index) {
                const int r=index/nchunks, chunk=index%nchunks;
                const int j=j0+r;
                const int ib=p.Istr+chunk*C;
                const int nb=(p.Iend-ib+1<C)?(p.Iend-ib+1):C;
                double fx[CM+1], oh[CM];
                const double* y0=yf+std::size_t(r)*std::size_t(p.N)*std::size_t(nx)+
                    std::size_t(ib-p.Istr);
                const double* y1=y0+std::size_t(p.N)*std::size_t(nx);
                for (int k=1;k<=p.N;++k) {
                  for (int b=0;b<=nb;++b) fx[b]=f.horizontal(ib+b,j,k,it,true,scheme);
                  for (int b=0;b<nb;++b) {
                    const int i=ib+b;
                    const double c=p.dt*f.pm(i,j)*f.pn(i,j);
                    const double dx=c*(fx[b+1]-fx[b]);
                    const double dy=c*(y1[std::size_t(k-1)*std::size_t(nx)+b]-
                                       y0[std::size_t(k-1)*std::size_t(nx)+b]);
                    const double d=dx+dy;
                    f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)-d;
                    const double dz=c*(f.vertical(i,j,k,it,scheme)-f.vertical(i,j,k-1,it,scheme));
                    f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)-dz;
                    oh[b]=1.0/f.hz(i,j,k);
                    f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)*oh[b];
                    f.diag(i,j,k,it,p.xadv)=-dx;
                    f.diag(i,j,k,it,p.yadv)=-dy;
                    f.diag(i,j,k,it,p.hadv)=-d;
                    f.diag(i,j,k,it,p.vadv)=-dz;
                  }
                  for (int id=1;id<=p.NDT;++id)
                    for (int b=0;b<nb;++b)
                      f.diag(ib+b,j,k,it,id)=f.diag(ib+b,j,k,it,id)*oh[b];
                }
                /* Implicit spline vertical diffusion, one column at a time. */
                for (int b=0;b<nb;++b) {
                  const int i=ib+b;
                  cf[0]=0.0; dc[0]=0.0;
                  for (int k=1;k<p.N;++k) {
                    const double o=1.0/f.hz(i,j,k), o1=1.0/f.hz(i,j,k+1);
                    const double lower=(1.0/6.0)*f.hz(i,j,k)-p.dt*f.akt(i,j,k-1,tracer)*o;
                    const double upper=(1.0/6.0)*f.hz(i,j,k+1)-p.dt*f.akt(i,j,k+1,tracer)*o1;
                    const double bb=(1.0/3.0)*(f.hz(i,j,k)+f.hz(i,j,k+1))+
                                    p.dt*f.akt(i,j,k,tracer)*(o+o1);
                    const double inverse=1.0/(bb-lower*cf[k-1]);
                    cf[k]=inverse*upper;
                    dc[k]=inverse*(f.t(i,j,k+1,nn,it)-f.t(i,j,k,nn,it)-lower*dc[k-1]);
                  }
                  dc[p.N]=0.0;
                  for (int k=p.N-1;k>=1;--k) dc[k]=dc[k]-cf[k]*dc[k+1];
                  for (int k=1;k<=p.N;++k) {
                    dc[k]=dc[k]*f.akt(i,j,k,tracer);
                    const double change=p.dt*(1.0/f.hz(i,j,k))*(dc[k]-dc[k-1]);
                    f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)+change;
                    f.diag(i,j,k,it,p.vdif)=f.diag(i,j,k,it,p.vdif)+change;
                  }
                }
              });
          team.team_barrier();
        }
      });
}

/*
** Double-buffered TeamPolicy variant.
**
** The team owns the whole tile in one league rank and walks the (tracer,
** row block) work list as a software pipeline: one scratch buffer holds the
** y-face fluxes being consumed (block n-1), the other those being produced
** (block n).  Produce and consume of consecutive blocks share a single loop,
** so a CPE keeps both memory streams in flight between two barriers, and
** there is one barrier per work item.  Same per-cell expressions as the
** chunk kernel, so the result is bitwise identical.
*/
extern "C" void k_step3d_t_team_pipe(const TracerConfig* config,
    const int* schemes, int block_rows, int chunk_cols, void* pm, void* pn,
    void* Hz, void* Huon, void* Hvom, void* W, void* zr, void* Akt,
    void* tracer, void* diagnostics) {
  roms::initialize();
  ROMS_KK_TIMER("step3d_t team pipe");
  const TracerConfig p=*config;
  if (p.N<3 || p.NT<1 || p.NAT<1 || p.nnew<1 || p.nnew>2)
    Kokkos::abort("Invalid Kokkos tracer dimensions or predictor alias");
  const int nx=p.Iend-p.Istr+1, ny=p.Jend-p.Jstr+1;
  if (nx<1 || ny<1) Kokkos::abort("Invalid tracer tile");
  for (int it=0;it<p.NT;++it)
    if (schemes[it]!=0 && schemes[it]!=1) Kokkos::abort("Unsupported tracer scheme");
  constexpr int CM=32;
  const int C=(chunk_cols>0 && chunk_cols<CM)?chunk_cols:CM;
  using team_policy=Kokkos::TeamPolicy<roms::exec_space>;
  using member=team_policy::member_type;
#if defined(ROMS_KOKKOS_SW64)
  constexpr int team_size=64;
#else
  constexpr int team_size=1;
#endif
  const int per_member=((2*(p.N+1)+15)/16)*16;
  const std::size_t cap=std::size_t(3)*1024*1024+768*1024;
  const std::size_t face_len=std::size_t(nx)*std::size_t(p.N);
  const auto need=[&](int rows) {
    return (std::size_t(2)*std::size_t(rows+1)*face_len+
            std::size_t(team_size)*std::size_t(per_member))*sizeof(double);
  };
  int R=block_rows>0?block_rows:8;
  if (R>ny) R=ny;
  while (R>1 && need(R)>cap) R/=2;
  if (need(R)>cap) {
    k_step3d_t_chunks(config,schemes,pm,pn,Hz,Huon,Hvom,W,zr,Akt,tracer,diagnostics);
    return;
  }
#ifndef __sw_slave__
#if defined(ROMS_KOKKOS_SW64)
  {
    static bool reported=false;
    const int mode=athread_get_ldmshare_mode();
    const int share=athread_get_ldmshare_size();
    if (!reported) {
      std::printf("step3d_t team pipe: LDM share mode=%d size=%d, R=%d, "
                  "request %.2f MB\n", mode, share, R,
                  double(need(R))/1048576.0);
      reported=true;
    }
    if (share<=0) {
      k_step3d_t_chunks(config,schemes,pm,pn,Hz,Huon,Hvom,W,zr,Akt,tracer,diagnostics);
      return;
    }
  }
#endif
#endif
  const auto a2=[&](void* v) {return roms::as_2d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj);};
  const auto a3=[&](void* v,int low) {return roms::as_3d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj,low,p.N);};
  const Fields f{p,a2(pm),a2(pn),a3(Hz,1),a3(Huon,1),a3(Hvom,1),a3(W,0),a3(zr,1),
      roms::as_4d<double>(Akt,p.LBi,p.UBi,p.LBj,p.UBj,0,p.N,1,p.NAT),
      roms::as_5d<double>(tracer,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,3,1,p.NT),
      roms::as_5d<double>(diagnostics,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,p.NT,1,p.NDT)};
  const int blocks=(ny+R-1)/R;
  const int nchunks=(nx+C-1)/C;
  const int items=p.NT*blocks;
  const std::size_t buf_count=std::size_t(R+1)*face_len;
  const std::size_t shmem=(std::size_t(2)*buf_count+
      std::size_t(team_size)*std::size_t(per_member))*sizeof(double);
  Kokkos::parallel_for("ROMS step3d_t team pipe",
      team_policy(1,team_size).set_scratch_size(0,Kokkos::PerTeam(shmem)),
      KOKKOS_LAMBDA(const member& team) {
        double* base=static_cast<double*>(team.team_scratch(0).get_shmem(shmem));
        double* cf=base+std::size_t(2)*buf_count+
            std::size_t(team.team_rank())*std::size_t(per_member);
        double* dc=cf+p.N+1;
        for (int n=0;n<=items;++n) {
          /* Produce item n into buffer (n&1); consume item n-1 from the
          ** other.  The two buffers never coincide, and the barrier at the
          ** end of the item orders produce(n) before consume(n). */
          const int np=n<items?n:-1;
          const int nc=n>0?n-1:-1;
          int jp0=0,rp=0,itp=0,sp=0,nprod=0;
          if (np>=0) {
            itp=np/blocks+1;
            const int b=np%blocks;
            jp0=p.Jstr+b*R;
            rp=Kokkos::min(R,p.Jend-jp0+1);
            sp=schemes[itp-1];
            nprod=(rp+1)*nchunks;
          }
          int jc0=0,rc=0,itc=0,nnc=0,trc=0,sc=0,ncons=0;
          if (nc>=0) {
            itc=nc/blocks+1;
            const int b=nc%blocks;
            jc0=p.Jstr+b*R;
            rc=Kokkos::min(R,p.Jend-jc0+1);
            nnc=p.nnew;
            trc=itc<p.NAT?itc:p.NAT;
            sc=schemes[itc-1];
            ncons=rc*nchunks;
          }
          const int nmax=nprod>ncons?nprod:ncons;
          if (nmax<=0) break;
          double* bp=base+std::size_t(np>=0?(np&1):0)*buf_count;
          double* bc=base+std::size_t(nc>=0?(nc&1):0)*buf_count;
          Kokkos::parallel_for(Kokkos::TeamThreadRange(team,nmax),
              [&](int index) {
                if (index<nprod) {
                  const int face=index/nchunks, chunk=index%nchunks;
                  const int ib=p.Istr+chunk*C;
                  const int nb=(p.Iend-ib+1<C)?(p.Iend-ib+1):C;
                  const int jface=jp0+face;
                  double* out=bp+std::size_t(face)*face_len+
                      std::size_t(ib-p.Istr);
                  for (int k=1;k<=p.N;++k)
                    for (int b=0;b<nb;++b)
                      out[std::size_t(k-1)*std::size_t(nx)+b]=
                          f.horizontal(ib+b,jface,k,itp,false,sp);
                }
                if (index<ncons) {
                  const int r=index/nchunks, chunk=index%nchunks;
                  const int j=jc0+r;
                  const int ib=p.Istr+chunk*C;
                  const int nb=(p.Iend-ib+1<C)?(p.Iend-ib+1):C;
                  double fx[CM+1], oh[CM];
                  const double* y0=bc+std::size_t(r)*face_len+
                      std::size_t(ib-p.Istr);
                  const double* y1=y0+face_len;
                  for (int k=1;k<=p.N;++k) {
                    for (int b=0;b<=nb;++b) fx[b]=f.horizontal(ib+b,j,k,itc,true,sc);
                    for (int b=0;b<nb;++b) {
                      const int i=ib+b;
                      const double c=p.dt*f.pm(i,j)*f.pn(i,j);
                      const double dx=c*(fx[b+1]-fx[b]);
                      const double dy=c*(y1[std::size_t(k-1)*std::size_t(nx)+b]-
                                         y0[std::size_t(k-1)*std::size_t(nx)+b]);
                      const double d=dx+dy;
                      f.t(i,j,k,nnc,itc)=f.t(i,j,k,nnc,itc)-d;
                      const double dz=c*(f.vertical(i,j,k,itc,sc)-f.vertical(i,j,k-1,itc,sc));
                      f.t(i,j,k,nnc,itc)=f.t(i,j,k,nnc,itc)-dz;
                      oh[b]=1.0/f.hz(i,j,k);
                      f.t(i,j,k,nnc,itc)=f.t(i,j,k,nnc,itc)*oh[b];
                      f.diag(i,j,k,itc,p.xadv)=-dx;
                      f.diag(i,j,k,itc,p.yadv)=-dy;
                      f.diag(i,j,k,itc,p.hadv)=-d;
                      f.diag(i,j,k,itc,p.vadv)=-dz;
                    }
                    for (int id=1;id<=p.NDT;++id)
                      for (int b=0;b<nb;++b)
                        f.diag(ib+b,j,k,itc,id)=f.diag(ib+b,j,k,itc,id)*oh[b];
                  }
                  for (int b=0;b<nb;++b) {
                    const int i=ib+b;
                    cf[0]=0.0; dc[0]=0.0;
                    for (int k=1;k<p.N;++k) {
                      const double o=1.0/f.hz(i,j,k), o1=1.0/f.hz(i,j,k+1);
                      const double lower=(1.0/6.0)*f.hz(i,j,k)-p.dt*f.akt(i,j,k-1,trc)*o;
                      const double upper=(1.0/6.0)*f.hz(i,j,k+1)-p.dt*f.akt(i,j,k+1,trc)*o1;
                      const double bb=(1.0/3.0)*(f.hz(i,j,k)+f.hz(i,j,k+1))+
                                      p.dt*f.akt(i,j,k,trc)*(o+o1);
                      const double inverse=1.0/(bb-lower*cf[k-1]);
                      cf[k]=inverse*upper;
                      dc[k]=inverse*(f.t(i,j,k+1,nnc,itc)-f.t(i,j,k,nnc,itc)-lower*dc[k-1]);
                    }
                    dc[p.N]=0.0;
                    for (int k=p.N-1;k>=1;--k) dc[k]=dc[k]-cf[k]*dc[k+1];
                    for (int k=1;k<=p.N;++k) {
                      dc[k]=dc[k]*f.akt(i,j,k,trc);
                      const double change=p.dt*(1.0/f.hz(i,j,k))*(dc[k]-dc[k-1]);
                      f.t(i,j,k,nnc,itc)=f.t(i,j,k,nnc,itc)+change;
                      f.diag(i,j,k,itc,p.vdif)=f.diag(i,j,k,itc,p.vdif)+change;
                    }
                  }
                }
              });
          team.team_barrier();
        }
      });
}
#endif


#if defined(ROMS_KK_TEAM_STEP3D_T)
/*
** RangePolicy row-block control for the team kernel: one work item owns a
** block of R rows, a strip of SW columns and one tracer.  Per column chunk it
** evaluates the block's R+1 y-face fluxes once and keeps them in the item's
** private workspace while the R rows are updated from them; the x-face fluxes
** keep the chunk kernel's rolling buffer.  One pass over the data, no team
** barrier and no LDM; the per-cell expressions are those of the chunk kernel.
*/
extern "C" void k_step3d_t_rows(const TracerConfig* config, const int* schemes,
    int block_rows, int chunk_cols, int strip_cols, void* pm, void* pn,
    void* Hz, void* Huon, void* Hvom, void* W, void* zr, void* Akt,
    void* tracer, void* diagnostics) {
  roms::initialize();
  ROMS_KK_TIMER("step3d_t rows");
  const TracerConfig p=*config;
  if (p.N<3 || p.NT<1 || p.NAT<1 || p.nnew<1 || p.nnew>2)
    Kokkos::abort("Invalid Kokkos tracer dimensions or predictor alias");
  const int nx=p.Iend-p.Istr+1, ny=p.Jend-p.Jstr+1;
  if (nx<1 || ny<1) Kokkos::abort("Invalid tracer tile");
  for (int it=0;it<p.NT;++it)
    if (schemes[it]!=0 && schemes[it]!=1) Kokkos::abort("Unsupported tracer scheme");
  constexpr int CM=32;
  int R=block_rows>0?block_rows:8;
  if (R>ny) R=ny;
  const int C=(chunk_cols>0 && chunk_cols<CM)?chunk_cols:CM;
  const int SW=strip_cols>0?strip_cols:128;
  const int blocks=(ny+R-1)/R;
  const int strips=(nx+SW-1)/SW;
  const int items=blocks*strips*p.NT;
  const std::size_t per_item=std::size_t(R+1)*std::size_t(C+1)+
      std::size_t(2)*(p.N+1);
  double* scratch=nullptr;
#ifndef __sw_slave__
  scratch=workspace.reserve(std::size_t(items)*per_item);
#endif
  const auto a2=[&](void* v) {return roms::as_2d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj);};
  const auto a3=[&](void* v,int low) {return roms::as_3d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj,low,p.N);};
  const Fields f{p,a2(pm),a2(pn),a3(Hz,1),a3(Huon,1),a3(Hvom,1),a3(W,0),a3(zr,1),
      roms::as_4d<double>(Akt,p.LBi,p.UBi,p.LBj,p.UBj,0,p.N,1,p.NAT),
      roms::as_5d<double>(tracer,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,3,1,p.NT),
      roms::as_5d<double>(diagnostics,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,p.NT,1,p.NDT)};
  Kokkos::parallel_for("ROMS step3d_t rows",
      Kokkos::RangePolicy<roms::exec_space,Kokkos::IndexType<int>>(0,items),
      KOKKOS_LAMBDA(int index) {
        const int it=1+index%p.NT;
        const int t=index/p.NT;
        const int strip=t%strips;
        const int block=t/strips;
        const int scheme=schemes[it-1];
        const int tracer=it<p.NAT?it:p.NAT;
        const int nn=p.nnew;
        const int j0=p.Jstr+block*R;
        const int rows=Kokkos::min(R,p.Jend-j0+1);
        const int i0=p.Istr+strip*SW;
        const int iend=(i0+SW-1<p.Iend)?(i0+SW-1):p.Iend;
        double* base=scratch+std::size_t(index)*per_item;
        double* yf=base;
        double* cf=base+std::size_t(R+1)*std::size_t(C+1);
        double* dc=cf+p.N+1;
        for (int ib=i0;ib<=iend;ib+=C) {
          const int nb=(iend-ib+1<C)?(iend-ib+1):C;
          double fx[CM+1], oh[CM];
          for (int k=1;k<=p.N;++k) {
            /* The block's rows+1 y faces, once per level. */
            for (int face=0;face<=rows;++face) {
              const int jface=j0+face;
              double* out=yf+std::size_t(face)*std::size_t(C+1);
              for (int b=0;b<=nb;++b)
                out[b]=f.horizontal(ib+b,jface,k,it,false,scheme);
            }
            for (int r=0;r<rows;++r) {
              const int j=j0+r;
              const double* y0=yf+std::size_t(r)*std::size_t(C+1);
              const double* y1=y0+(C+1);
              for (int b=0;b<=nb;++b) fx[b]=f.horizontal(ib+b,j,k,it,true,scheme);
              for (int b=0;b<nb;++b) {
                const int i=ib+b;
                const double c=p.dt*f.pm(i,j)*f.pn(i,j);
                const double dx=c*(fx[b+1]-fx[b]);
                const double dy=c*(y1[b]-y0[b]);
                const double d=dx+dy;
                f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)-d;
                const double dz=c*(f.vertical(i,j,k,it,scheme)-f.vertical(i,j,k-1,it,scheme));
                f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)-dz;
                oh[b]=1.0/f.hz(i,j,k);
                f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)*oh[b];
                f.diag(i,j,k,it,p.xadv)=-dx;
                f.diag(i,j,k,it,p.yadv)=-dy;
                f.diag(i,j,k,it,p.hadv)=-d;
                f.diag(i,j,k,it,p.vadv)=-dz;
              }
              for (int id=1;id<=p.NDT;++id)
                for (int b=0;b<nb;++b)
                  f.diag(ib+b,j,k,it,id)=f.diag(ib+b,j,k,it,id)*oh[b];
            }
          }
          /* Implicit spline vertical diffusion for the chunk's columns. */
          for (int r=0;r<rows;++r) {
            const int j=j0+r;
            for (int b=0;b<nb;++b) {
              const int i=ib+b;
              cf[0]=0.0; dc[0]=0.0;
              for (int k=1;k<p.N;++k) {
                const double o=1.0/f.hz(i,j,k), o1=1.0/f.hz(i,j,k+1);
                const double lower=(1.0/6.0)*f.hz(i,j,k)-p.dt*f.akt(i,j,k-1,tracer)*o;
                const double upper=(1.0/6.0)*f.hz(i,j,k+1)-p.dt*f.akt(i,j,k+1,tracer)*o1;
                const double bb=(1.0/3.0)*(f.hz(i,j,k)+f.hz(i,j,k+1))+
                                p.dt*f.akt(i,j,k,tracer)*(o+o1);
                const double inverse=1.0/(bb-lower*cf[k-1]);
                cf[k]=inverse*upper;
                dc[k]=inverse*(f.t(i,j,k+1,nn,it)-f.t(i,j,k,nn,it)-lower*dc[k-1]);
              }
              dc[p.N]=0.0;
              for (int k=p.N-1;k>=1;--k) dc[k]=dc[k]-cf[k]*dc[k+1];
              for (int k=1;k<=p.N;++k) {
                dc[k]=dc[k]*f.akt(i,j,k,tracer);
                const double change=p.dt*(1.0/f.hz(i,j,k))*(dc[k]-dc[k-1]);
                f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)+change;
                f.diag(i,j,k,it,p.vdif)=f.diag(i,j,k,it,p.vdif)+change;
              }
            }
          }
        }
      });
}
#endif

#endif
