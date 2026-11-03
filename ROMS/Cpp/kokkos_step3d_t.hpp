#ifndef ROMS_KOKKOS_STEP3D_T_HPP
#define ROMS_KOKKOS_STEP3D_T_HPP
#include "kokkos_config.hpp"

#if defined ROMS_KOKKOS
namespace roms {
/* Mirrored exactly by kokkos_tracer_config, including C alignment. */
struct TracerConfig {
  int LBi, UBi, LBj, UBj;
  int Istr, Iend, Jstr, Jend;
  int N, NT, NAT, NDT, nnew;
  int xadv, yadv, hadv, vadv, vdif;
  int west, east, south, north, periodic_ew, periodic_ns;
  int composite_w, composite_e, composite_s, composite_n;
  double dt, cc1, cc2, cc3;
};
}  // namespace roms
extern "C" void k_step3d_t(const roms::TracerConfig* config,
    const int* schemes, void* pm, void* pn, void* Hz, void* Huon, void* Hvom,
    void* W, void* zr, void* Akt, void* tracer, void* diagnostics);
extern "C" void k_step3d_t_rows(const roms::TracerConfig* config,
    const int* schemes, void* pm, void* pn, void* Hz, void* Huon, void* Hvom,
    void* W, void* zr, void* Akt, void* tracer, void* diagnostics);
/* Production entry: the row kernel with the Fortran loop order inside
** each chunk of columns (i innermost).  Same expressions, same result. */
extern "C" void k_step3d_t_chunks(const roms::TracerConfig* config,
    const int* schemes, void* pm, void* pn, void* Hz, void* Huon, void* Hvom,
    void* W, void* zr, void* Akt, void* tracer, void* diagnostics);
#endif
#endif
