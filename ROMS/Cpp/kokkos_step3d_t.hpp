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
/* Tracer advection and spline vertical diffusion for one tile, with the
** Fortran loop order inside each chunk of columns (i innermost). */
extern "C" void k_step3d_t_chunks(const roms::TracerConfig* config,
    const int* schemes, void* pm, void* pn, void* Hz, void* Huon, void* Hvom,
    void* W, void* zr, void* Akt, void* tracer, void* diagnostics);
#if defined(ROMS_KK_TEAM_STEP3D_T)
/* TeamPolicy variant sharing the y-face flux in team scratch; block_rows is
** the number of rows per team (0 takes the default). */
extern "C" void k_step3d_t_team(const roms::TracerConfig* config,
    const int* schemes, int block_rows, int chunk_cols, void* pm, void* pn,
    void* Hz, void* Huon, void* Hvom, void* W, void* zr, void* Akt,
    void* tracer, void* diagnostics);
/* Double-buffered pipeline: league of one team, produce block n while
** consuming block n-1 in the same loop. */
extern "C" void k_step3d_t_team_pipe(const roms::TracerConfig* config,
    const int* schemes, int block_rows, int chunk_cols, void* pm, void* pn,
    void* Hz, void* Huon, void* Hvom, void* W, void* zr, void* Akt,
    void* tracer, void* diagnostics);
/* RangePolicy row-block control: same y sharing in one pass, private
** workspace, no team barrier.  strip_cols sets the columns per work item. */
extern "C" void k_step3d_t_rows(const roms::TracerConfig* config,
    const int* schemes, int block_rows, int chunk_cols, int strip_cols,
    void* pm, void* pn, void* Hz, void* Huon, void* Hvom, void* W, void* zr,
    void* Akt, void* tracer, void* diagnostics);
#endif
#endif
#endif
