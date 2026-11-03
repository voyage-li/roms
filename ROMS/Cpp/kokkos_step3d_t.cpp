/* U3/C4 and HSIMT tracer advection followed by spline vertical diffusion.
** Each work item owns a complete water column, including both tracers.
** Predictor slot 3 is read-only. Boundary exchange and epilogue stay in
** Fortran; this entry is called only after whole-tile eligibility checks. */
#include "kokkos_step3d_t.hpp"
#if defined ROMS_KOKKOS
#include <cmath>
#include <cstdio>
#ifndef __sw_slave__
#include <memory>
#endif

namespace {
using roms::TracerConfig;
struct Slope { double gradient, ka; };

#ifndef __sw_slave__
/* Raw host ownership needs no Kokkos service at thread/process teardown.
** The SW64 HostSpace and borrowed Fortran allocations share this memory.
** Each host thread has independent scratch; all supported launches join. */
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
thread_local unsigned long calls = 0;
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

  KOKKOS_INLINE_FUNCTION
  void column_body(int i, int j, int it, int scheme, double* cf, double* dc,
                   const double* fx0, const double* fx1) const {
    const double c=p.dt*pm(i,j)*pn(i,j);
    for (int k=1;k<=p.N;++k) {
      const double dx=fx0 ? c*(fx1[k-1]-fx0[k-1])
          : c*(horizontal(i+1,j,k,it,true,scheme)-horizontal(i,j,k,it,true,scheme));
      const double dy=c*(horizontal(i,j+1,k,it,false,scheme)-horizontal(i,j,k,it,false,scheme));
      const double d=dx+dy;
      t(i,j,k,p.nnew,it)=t(i,j,k,p.nnew,it)-d;
      const double dz=c*(vertical(i,j,k,it,scheme)-vertical(i,j,k-1,it,scheme));
      t(i,j,k,p.nnew,it)=t(i,j,k,p.nnew,it)-dz;
      const double oh=1.0/hz(i,j,k);
      t(i,j,k,p.nnew,it)=t(i,j,k,p.nnew,it)*oh;
      diag(i,j,k,it,p.xadv)=-dx;
      diag(i,j,k,it,p.yadv)=-dy;
      diag(i,j,k,it,p.hadv)=-d;
      diag(i,j,k,it,p.vadv)=-dz;
      for (int id=1;id<=p.NDT;++id) diag(i,j,k,it,id)=diag(i,j,k,it,id)*oh;
    }
    const int tracer=it<p.NAT?it:p.NAT;
    cf[0]=0.0; dc[0]=0.0;
    for (int k=1;k<p.N;++k) {
      const double oh=1.0/hz(i,j,k), oh1=1.0/hz(i,j,k+1);
      const double lower=(1.0/6.0)*hz(i,j,k)-p.dt*akt(i,j,k-1,tracer)*oh;
      const double upper=(1.0/6.0)*hz(i,j,k+1)-p.dt*akt(i,j,k+1,tracer)*oh1;
      const double b=(1.0/3.0)*(hz(i,j,k)+hz(i,j,k+1))+
                     p.dt*akt(i,j,k,tracer)*(oh+oh1);
      const double inverse=1.0/(b-lower*cf[k-1]);
      cf[k]=inverse*upper;
      dc[k]=inverse*(t(i,j,k+1,p.nnew,it)-t(i,j,k,p.nnew,it)-lower*dc[k-1]);
    }
    dc[p.N]=0.0;
    for (int k=p.N-1;k>=1;--k) dc[k]=dc[k]-cf[k]*dc[k+1];
    for (int k=1;k<=p.N;++k) {
      dc[k]=dc[k]*akt(i,j,k,tracer);
      const double change=p.dt*(1.0/hz(i,j,k))*(dc[k]-dc[k-1]);
      t(i,j,k,p.nnew,it)=t(i,j,k,p.nnew,it)+change;
      diag(i,j,k,it,p.vdif)=diag(i,j,k,it,p.vdif)+change;
    }
  }

  KOKKOS_INLINE_FUNCTION
  void column(int i, int j, int it, int scheme, double* cf, double* dc) const {
    column_body(i,j,it,scheme,cf,dc,nullptr,nullptr);
  }
};
}  // namespace

extern "C" void k_step3d_t(const TracerConfig* config, const int* schemes,
    void* pm, void* pn, void* Hz, void* Huon, void* Hvom, void* W,
    void* zr, void* Akt, void* tracer, void* diagnostics) {
  roms::initialize();
  ROMS_KK_TIMER("step3d_t");
  const TracerConfig p=*config;
  if (p.N<3 || p.NT<1 || p.NAT<1 || p.nnew<1 || p.nnew>2)
    Kokkos::abort("Invalid Kokkos tracer dimensions or predictor alias");
  const int nx=p.Iend-p.Istr+1, ny=p.Jend-p.Jstr+1;
  const std::size_t columns=std::size_t(nx)*ny;
  if (nx<1 || ny<1 || columns>2147483647u) Kokkos::abort("Invalid tracer tile");
  for (int it=0;it<p.NT;++it)
    if (schemes[it]!=0 && schemes[it]!=1) Kokkos::abort("Unsupported tracer scheme");
  double* scratch=nullptr;
#ifndef __sw_slave__
  scratch=workspace.reserve(columns*2*(p.N+1));
  if (++calls==1) std::printf("Kokkos step3d_t: backend=%s one launch/call columns=%zu N=%d NT=%d\n",
      roms::exec_space::name(),columns,p.N,p.NT);
#endif
  const auto a2=[&](void* v) {return roms::as_2d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj);};
  const auto a3=[&](void* v,int low) {return roms::as_3d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj,low,p.N);};
  const Fields f{p,a2(pm),a2(pn),a3(Hz,1),a3(Huon,1),a3(Hvom,1),a3(W,0),a3(zr,1),
      roms::as_4d<double>(Akt,p.LBi,p.UBi,p.LBj,p.UBj,0,p.N,1,p.NAT),
      roms::as_5d<double>(tracer,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,3,1,p.NT),
      roms::as_5d<double>(diagnostics,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,p.NT,1,p.NDT)};
  Kokkos::parallel_for("ROMS step3d_t columns",
      Kokkos::RangePolicy<roms::exec_space,Kokkos::IndexType<int>>(0,int(columns)),
      KOKKOS_LAMBDA(int index) {
        const int i=p.Istr+index%nx, j=p.Jstr+index/nx;
        double* cf=scratch+std::size_t(index)*2*(p.N+1);
        double* dc=cf+p.N+1;
        for (int it=1;it<=p.NT;++it) f.column(i,j,it,schemes[it-1],cf,dc);
      });
}

/*
** Row variant of k_step3d_t.  The xi-face flux at (i,j,k) is used by the
** columns on both sides of that face, and the per-column kernel evaluates
** it twice.  A work item here owns one (j, tracer) row, walks i and rolls
** two face buffers, so every face is evaluated once.  No team, no
** cross-item sharing, and the arithmetic is the same expression at the
** same index, so the result is bitwise identical to k_step3d_t.
*/
extern "C" void k_step3d_t_rows(const TracerConfig* config, const int* schemes,
    void* pm, void* pn, void* Hz, void* Huon, void* Hvom, void* W,
    void* zr, void* Akt, void* tracer, void* diagnostics) {
  roms::initialize();
  ROMS_KK_TIMER("step3d_t rows");
  const TracerConfig p=*config;
  if (p.N<3 || p.NT<1 || p.NAT<1 || p.nnew<1 || p.nnew>2)
    Kokkos::abort("Invalid Kokkos tracer dimensions or predictor alias");
  const int nx=p.Iend-p.Istr+1, ny=p.Jend-p.Jstr+1;
  if (nx<1 || ny<1 || nx>2147483647 || ny>2147483647)
    Kokkos::abort("Invalid tracer tile");
  for (int it=0;it<p.NT;++it)
    if (schemes[it]!=0 && schemes[it]!=1) Kokkos::abort("Unsupported tracer scheme");
  const int per=2*p.N+2*(p.N+1);
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
  Kokkos::parallel_for("ROMS step3d_t rows",
      Kokkos::RangePolicy<roms::exec_space,Kokkos::IndexType<int>>(0,ny*p.NT),
      KOKKOS_LAMBDA(int index) {
        const int j=p.Jstr+index%ny;
        const int it=1+index/ny;
        const int scheme=schemes[it-1];
        double* base=scratch+std::size_t(index)*per;
        double* cf=base+2*p.N;
        double* dc=cf+p.N+1;
        double* fx=base;
        for (int k=1;k<=p.N;++k)
          fx[k-1]=f.horizontal(p.Istr,j,k,it,true,scheme);
        int parity=0;
        for (int i=p.Istr;i<=p.Iend;++i) {
          double* left =fx+(parity?p.N:0);
          double* right=fx+(parity?0:p.N);
          for (int k=1;k<=p.N;++k)
            right[k-1]=f.horizontal(i+1,j,k,it,true,scheme);
          f.column_body(i,j,it,scheme,cf,dc,left,right);
          parity^=1;
        }
      });
}

/*
** Chunked row kernel, the production entry.  k_step3d_t_rows walks a row
** one column at a time and, per column, touches every level of the
** tracer, the fluxes and the NDT diagnostics before moving on; on the
** CPEs that working set does not stay in cache.  Here a work item still
** owns one (j, tracer) row, but replays the column body level by level
** with i innermost over KB columns at a time, the loop order the Fortran
** uses.  Per point the operations and their order are those of
** column_body, so the result is bitwise identical to k_step3d_t_rows.
*/
extern "C" void k_step3d_t_chunks(const TracerConfig* config, const int* schemes,
    void* pm, void* pn, void* Hz, void* Huon, void* Hvom, void* W,
    void* zr, void* Akt, void* tracer, void* diagnostics) {
  roms::initialize();
  ROMS_KK_TIMER("step3d_t chunks");
  const TracerConfig p=*config;
  if (p.N<3 || p.NT<1 || p.NAT<1 || p.nnew<1 || p.nnew>2)
    Kokkos::abort("Invalid Kokkos tracer dimensions or predictor alias");
  const int nx=p.Iend-p.Istr+1, ny=p.Jend-p.Jstr+1;
  if (nx<1 || ny<1 || nx>2147483647 || ny>2147483647)
    Kokkos::abort("Invalid tracer tile");
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

#ifdef KK_STEP3D_T_TEAM
/*
** Experimental TeamPolicy entries, compiled only for the team benchmark.
** On the SW64 backend a team is fixed at 64 CPEs (one core group) and the
** leagues run one after another inside that group, so these variants trade
** launch-level parallelism for team-scoped sharing.
**
**   mode 0: one column per team member; identical work to k_step3d_t and
**           only measures the team machinery's overhead.
**   mode 1: one team per row (j), the team computes the x-face fluxes for
**           every level once into its scratch, then each member advances
**           its own columns using those fluxes. This removes the duplicate
**           x-face evaluation the per-column kernel does for neighbouring
**           columns.
*/
extern "C" void k_step3d_t_team(const roms::TracerConfig* config,
    const int* schemes, int mode, void* pm, void* pn, void* Hz, void* Huon,
    void* Hvom, void* W, void* zr, void* Akt, void* tracer, void* diagnostics,
    void* scratch_buffer) {
  roms::initialize();
  const TracerConfig p=*config;
  if (p.N<3 || p.NT<1 || p.NAT<1 || p.nnew<1 || p.nnew>2 || mode<0 || mode>1)
    Kokkos::abort("Invalid team tracer configuration");
  const int nx=p.Iend-p.Istr+1, ny=p.Jend-p.Jstr+1;
  const std::size_t columns=std::size_t(nx)*ny;
  if (nx<1 || ny<1) Kokkos::abort("Invalid tracer tile");
  for (int it=0;it<p.NT;++it)
    if (schemes[it]!=0 && schemes[it]!=1) Kokkos::abort("Unsupported tracer scheme");
  double* sh=static_cast<double*>(scratch_buffer);
  const auto a2=[&](void* v) {return roms::as_2d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj);};
  const auto a3=[&](void* v,int low) {return roms::as_3d<double>(v,p.LBi,p.UBi,p.LBj,p.UBj,low,p.N);};
  const Fields f{p,a2(pm),a2(pn),a3(Hz,1),a3(Huon,1),a3(Hvom,1),a3(W,0),a3(zr,1),
      roms::as_4d<double>(Akt,p.LBi,p.UBi,p.LBj,p.UBj,0,p.N,1,p.NAT),
      roms::as_5d<double>(tracer,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,3,1,p.NT),
      roms::as_5d<double>(diagnostics,p.LBi,p.UBi,p.LBj,p.UBj,1,p.N,1,p.NT,1,p.NDT)};
  using team_policy=Kokkos::TeamPolicy<roms::exec_space>;
  using member=team_policy::member_type;
#if defined(ROMS_KOKKOS_SW64)
  constexpr int team_size=64;   // one core group; the backend fixes this
#else
  constexpr int team_size=1;    // the Serial backend rejects larger teams
#endif

  if (mode==0) {
    const int league=(int(columns)+team_size-1)/team_size;
    Kokkos::parallel_for("ROMS step3d_t team columns",
        team_policy(league,team_size),
        KOKKOS_LAMBDA(const member& team) {
          const int index=team.league_rank()*team_size+team.team_rank();
          if (index<int(columns)) {
            const int i=p.Istr+index%nx, j=p.Jstr+index/nx;
            double* cf=sh+std::size_t(index)*2*(p.N+1);
            double* dc=cf+p.N+1;
            for (int it=1;it<=p.NT;++it) f.column(i,j,it,schemes[it-1],cf,dc);
          }
          team.team_barrier();
        });
    return;
  }

  const std::size_t face_size=std::size_t(nx+1)*p.N;
  const std::size_t team_scratch=face_size+std::size_t(team_size)*2*(p.N+1);
  Kokkos::parallel_for("ROMS step3d_t team rows",
      team_policy(ny,team_size),
      KOKKOS_LAMBDA(const member& team) {
        const int j=p.Jstr+team.league_rank();
        double* base=sh+std::size_t(team.league_rank())*team_scratch;
        double* fx=base;
        for (int it=1;it<=p.NT;++it) {
          const int scheme=schemes[it-1];
          Kokkos::parallel_for(Kokkos::TeamThreadRange(team,nx+1),
              [&](int fi) {
                const int i=p.Istr+fi;
                double* out=fx+std::size_t(fi)*p.N;
                for (int k=1;k<=p.N;++k) out[k-1]=f.horizontal(i,j,k,it,true,scheme);
              });
          team.team_barrier();
          Kokkos::parallel_for(Kokkos::TeamThreadRange(team,nx),
              [&](int ci) {
                const int i=p.Istr+ci;
                double* cf=base+face_size+
                    std::size_t(team.team_rank())*2*(p.N+1);
                double* dc=cf+p.N+1;
                f.column_body(i,j,it,scheme,cf,dc,
                    fx+std::size_t(ci)*p.N,fx+std::size_t(ci+1)*p.N);
              });
          team.team_barrier();
        }
      });
}
#endif
#endif
