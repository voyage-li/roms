/* Direct regression for the production tracer kernel k_step3d_t_chunks
** against a Fortran copy of the step3d_t loops (test_step3d_t_reference.F).
** The 37-column tile crosses the 32-column chunk boundary. */
#include "kokkos_step3d_t.hpp"
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <vector>
extern "C" void reference_step3d_t(const roms::TracerConfig*, const int*,
    void*,void*,void*,void*,void*,void*,void*,void*,void*,void*);

bool run_case(int nx, int levels, int output, int periodic, int mode) {
  roms::TracerConfig p{};
  p.LBi=p.LBj=-3; p.UBi=nx+4; p.UBj=11;
  p.Istr=p.Jstr=1; p.Iend=nx; p.Jend=7;
  p.N=levels; p.NT=p.NAT=2; p.NDT=8; p.nnew=output;
  p.xadv=1; p.yadv=2; p.hadv=3; p.vadv=4; p.vdif=5;
  p.west=p.east=p.south=p.north=1;
  p.periodic_ew=periodic&1; p.periodic_ns=(periodic>>1)&1;
  p.dt=300; p.cc1=0.25; p.cc2=0.5; p.cc3=1.0/12.0;
  const int ni=p.UBi-p.LBi+1, nj=p.UBj-p.LBj+1;
  const std::size_t cells=ni*nj;
  std::vector<double> pm(cells),pn(cells), hz(cells*levels), hu=hz,hv=hz,zr=hz;
  std::vector<double> w(cells*(levels+1)), akt(cells*(levels+1)*2);
  std::vector<double> t(cells*levels*3*2),diag(cells*levels*2*p.NDT);
  auto a2=[&](auto& a){return roms::as_2d<double>(a.data(),p.LBi,p.UBi,p.LBj,p.UBj);};
  auto a3=[&](auto& a,int low){return roms::as_3d<double>(a.data(),p.LBi,p.UBi,p.LBj,p.UBj,low,levels);};
  auto pmv=a2(pm),pnv=a2(pn); auto h=a3(hz,1),u=a3(hu,1),v=a3(hv,1),z=a3(zr,1),wv=a3(w,0);
  auto av=roms::as_4d<double>(akt.data(),p.LBi,p.UBi,p.LBj,p.UBj,0,levels,1,2);
  auto tv=roms::as_5d<double>(t.data(),p.LBi,p.UBi,p.LBj,p.UBj,1,levels,1,3,1,2);
  auto dv=roms::as_5d<double>(diag.data(),p.LBi,p.UBi,p.LBj,p.UBj,1,levels,1,2,1,p.NDT);
  for (int j=p.LBj;j<=p.UBj;++j) for (int i=p.LBi;i<=p.UBi;++i) {
    pmv(i,j)=0.001*(1+0.01*i); pnv(i,j)=0.0009*(1+0.01*j);
    wv(i,j,0)=0;
    for (int k=1;k<=levels;++k) {
      h(i,j,k)=4+0.2*k+0.01*i+0.02*j;
      z(i,j,k)=5.0*k+0.01*i+0.02*j;
      u(i,j,k)=(mode==0 || mode==3)?0:mode==2?1300:5000*std::sin(0.3*i+0.2*j+k);
      v(i,j,k)=(mode==0 || mode==3)?0:mode==2?-2000:4000*std::cos(0.2*i-0.3*j+k);
      wv(i,j,k)=mode==1?100*std::sin(0.3*i+j+k):0;
      for (int it=1;it<=2;++it) {
        for (int state=1;state<=3;++state) {
          const double q=mode==2?10.0*it:10.0*it+0.03*i*i+0.02*j*j+
                           0.1*std::sin(0.5*i+j+k)+0.07*k;
          tv(i,j,k,state,it)=state==output?h(i,j,k)*q:q+0.1*(state-3);
        }
        for (int id=1;id<=p.NDT;++id) dv(i,j,k,it,id)=0.001*id+0.00001*(i+j+k+it);
      }
    }
    for (int k=0;k<=levels;++k) for (int it=1;it<=2;++it)
      av(i,j,k,it)=mode==0?0:1e-4*(1+0.01*k+0.001*i+0.002*j+0.1*it);
  }
  int schemes[2]={0,1};
  const auto original=t;
  auto expected=t, expected_diag=diag;
  reference_step3d_t(&p,schemes,pm.data(),pn.data(),hz.data(),hu.data(),hv.data(),
      w.data(),zr.data(),akt.data(),expected.data(),expected_diag.data());
  k_step3d_t_chunks(&p,schemes,pm.data(),pn.data(),hz.data(),hu.data(),hv.data(),
      w.data(),zr.data(),akt.data(),t.data(),diag.data());
  bool ok=true; double error=0,derr=0,peak=0,dpeak=0;
  for (std::size_t k=0;k<t.size();++k) {
    ok &= std::isfinite(t[k]) && std::isfinite(expected[k]);
    error=std::max(error,std::abs(t[k]-expected[k]));peak=std::max(peak,std::abs(expected[k]));
    const int state=int(k/(cells*levels))%3+1;
    if (state!=output) ok &= t[k]==original[k];
  }
  for (std::size_t k=0;k<diag.size();++k) {
    ok &= std::isfinite(diag[k]) && std::isfinite(expected_diag[k]);
    derr=std::max(derr,std::abs(diag[k]-expected_diag[k]));dpeak=std::max(dpeak,std::abs(expected_diag[k]));
  }
  ok &= error<=1e-12*std::max(1.0,peak) && derr<=1e-12*std::max(1.0,dpeak);
  if (mode==3) {
    // Independently assemble and solve the spline system with dense pivoted
    // elimination. This is deliberately not the kernel's Thomas recurrence.
    const int i=3,j=3, m=levels-1;
    auto old=roms::as_5d<double>(const_cast<double*>(original.data()),p.LBi,p.UBi,
        p.LBj,p.UBj,1,levels,1,3,1,2);
    for (int it=1;it<=2;++it) {
      std::vector<double> matrix(m*m,0), rhs(m), concentration(levels+1), solution(levels+1,0);
      for (int k=1;k<=levels;++k) concentration[k]=old(i,j,k,output,it)/h(i,j,k);
      for (int k=1;k<levels;++k) {
        const int row=k-1;
        matrix[row*m+row]=(h(i,j,k)+h(i,j,k+1))/3.0+
            p.dt*av(i,j,k,it)*(1.0/h(i,j,k)+1.0/h(i,j,k+1));
        if (k>1) matrix[row*m+row-1]=h(i,j,k)/6.0-p.dt*av(i,j,k-1,it)/h(i,j,k);
        if (k<levels-1) matrix[row*m+row+1]=h(i,j,k+1)/6.0-p.dt*av(i,j,k+1,it)/h(i,j,k+1);
        rhs[row]=concentration[k+1]-concentration[k];
      }
      for (int col=0;col<m;++col) {
        int pivot=col;
        for (int row=col+1;row<m;++row)
          if (std::abs(matrix[row*m+col])>std::abs(matrix[pivot*m+col])) pivot=row;
        for (int q=col;q<m;++q) std::swap(matrix[col*m+q],matrix[pivot*m+q]);
        std::swap(rhs[col],rhs[pivot]);
        for (int row=col+1;row<m;++row) {
          const double ratio=matrix[row*m+col]/matrix[col*m+col];
          for (int q=col;q<m;++q) matrix[row*m+q]-=ratio*matrix[col*m+q];
          rhs[row]-=ratio*rhs[col];
        }
      }
      for (int row=m-1;row>=0;--row) {
        double value=rhs[row];
        for (int q=row+1;q<m;++q) value-=matrix[row*m+q]*solution[q+1];
        solution[row+1]=value/matrix[row*m+row];
      }
      for (int k=1;k<=levels;++k) {
        const double value=concentration[k]+p.dt/h(i,j,k)*
            (av(i,j,k,it)*solution[k]-av(i,j,k-1,it)*solution[k-1]);
        ok &= std::abs(tv(i,j,k,output,it)-value)<=1e-12*std::max(1.0,std::abs(value));
      }
    }
  }

  std::printf("nx=%d N=%d nnew=%d periodic=%d mode=%d tracer=%.4e diag=%.4e %s\n",
      nx,levels,output,periodic,mode,error,derr,ok?"PASS":"FAIL");
  return ok;
}
int main() {
  roms::initialize();
  bool ok=true;
  for (int n:{4,7,16,33}) for (int output=1;output<=2;++output)
    for (int periodic=0;periodic<4;++periodic) for (int mode=0;mode<4;++mode)
      ok &= run_case(9,n,output,periodic,mode);
  for (int n:{4,33}) for (int output=1;output<=2;++output)
    for (int periodic:{0,3}) for (int mode=1;mode<=2;++mode)
      ok &= run_case(37,n,output,periodic,mode);
  roms::finalize();
  return ok?0:1;
}
