/* Restricted, generated row kernels. See tools/row_codegen.py.
 * The ABI carries runtime Fortran bounds, never copies model fields, and
 * keeps each launch synchronous before the next dependent Fortran phase. */
#ifndef ROMS_KOKKOS_LOOPS_HPP
#define ROMS_KOKKOS_LOOPS_HPP
#include "kokkos_config.hpp"
#if defined ROMS_KOKKOS
#if defined SINGLE_PRECISION
#error "Generated ROMS row kernels require double precision"
#endif
#include <cmath>
#include <type_traits>
#ifndef __sw_slave__
#include <vector>
#endif
static_assert(sizeof(int)==4 && sizeof(double)==8,"ROMS C interoperability requires int32/float64");
struct KkLoopArgs {
  void* p[128];
  int b[1280];
  double s[256];
};
namespace roms_loops {
template<int Rank, class T=double> struct Array {
  T* p;
  int lo[Rank];
  std::ptrdiff_t stride[Rank];
  KOKKOS_INLINE_FUNCTION Array(void* ptr, const int* bounds) : p(static_cast<T*>(ptr)) {
    std::ptrdiff_t n = 1;
    for (int d=0; d<Rank; ++d) {
      lo[d]=bounds[2*d]; stride[d]=n;
      n*=bounds[2*d+1]-bounds[2*d]+1;
    }
  }
  template<class... I> KOKKOS_INLINE_FUNCTION T& operator()(I... indices) const {
    static_assert(sizeof...(I)==Rank,"Fortran array rank mismatch");
    const int ii[Rank]={static_cast<int>(indices)...};
    std::ptrdiff_t n=0;
    for (int d=0;d<Rank;++d) n+=(ii[d]-lo[d])*stride[d];
    return p[n];
  }
};
template<class A,class B> KOKKOS_INLINE_FUNCTION typename std::common_type<A,B>::type fmin(A a,B b) { return a<b?a:b; }
template<class A,class B> KOKKOS_INLINE_FUNCTION typename std::common_type<A,B>::type fmax(A a,B b) { return a>b?a:b; }
template<class A,class B,class C,class... T>
KOKKOS_INLINE_FUNCTION typename std::common_type<A,B,C,T...>::type fmin(A a,B b,C c,T... rest) { return fmin(fmin(a,b),c,rest...); }
template<class A,class B,class C,class... T>
KOKKOS_INLINE_FUNCTION typename std::common_type<A,B,C,T...>::type fmax(A a,B b,C c,T... rest) { return fmax(fmax(a,b),c,rest...); }
KOKKOS_INLINE_FUNCTION int sign(int a,int b) { return b<0?-std::abs(a):std::abs(a); }
KOKKOS_INLINE_FUNCTION double sign(double a,double b) { return std::copysign(std::abs(a),b); }
KOKKOS_INLINE_FUNCTION int power(int a,int b) {
  if(b<0) return a==1?1:a==-1?(b%2?-1:1):0;
  int value=1;
  for(unsigned n=static_cast<unsigned>(b);n;n>>=1) {
    if(n&1) value*=a;
    if(n>1) a*=a;
  }
  return value;
}
KOKKOS_INLINE_FUNCTION double power(double a,int b) {
  if(b==2) return a*a;
  if(b==3) return a*a*a;
  if(b==4) { double t=a*a; return t*t; }
  return std::pow(a,static_cast<double>(b));
}
KOKKOS_INLINE_FUNCTION double power(double a,double b) { return std::pow(a,b); }
}
#endif // ROMS_KOKKOS
#endif // ROMS_KOKKOS_LOOPS_HPP
