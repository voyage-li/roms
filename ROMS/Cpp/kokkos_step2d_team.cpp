/*
** Experimental TeamPolicy variants of the step2d kernels.
**
** Compiled only when KK_STEP2D_TEAM is defined (benchmark builds).  The
** production range kernels in kokkos_step2d.cpp are untouched.  Every
** entry here must reproduce its range counterpart bitwise; the policy
** benchmark checks that for each kernel and size.
**
** Mapping.  A team covers `rt = ceil(TEAM/nx)` consecutive j rows so
** the members stay busy on narrow tiles, and the members split the i
** extent with TeamThreadRange.  On SW64 TEAM is the 64 CPEs of one core
** group and the leagues run one after another inside that group, the
** same execution shape the range kernels get.
**
** Scratch.  A team-scoped slice is used where two cells of the same row
** need the same intermediate: depth_flux (Drhs), coriolis (UFx) and
** rhs_fused (the xi derivatives).  The other kernels have no such value,
** so they run with zero scratch and measure the policy machinery alone.
**
** The slice comes from a caller-supplied buffer, not team_scratch().
** The pinned Kokkos fork declares an 8 MB __thread_local_share array
** (Kokkos_SW64_Parallel_Team.hpp) regardless of the requested size, and
** the runtime rejects that: with -ldm_share_mode 5 -ldm_share_size 128
** the job dies at startup with "__ldm_space: __thread_local data is too
** big (8427232 B), more than ldm space", and without the share mode any
** access faults with "accessed wrong shared LDM space".  Host memory is
** what the validated step3d_t team variant uses, so the same hand-off
** works here and the LDM question is left to the backend.
*/

#include "kokkos_step2d_team.hpp"

#if defined ROMS_KOKKOS && defined KK_STEP2D_TEAM

#include <cmath>

#if !defined SOLVE3D || !defined VAR_RHO_2D || !defined UV_ADV || \
    !defined UV_COR || !defined UV_VIS2 || !defined DIAGNOSTICS_UV
#  error "Kokkos step2d currently requires the diagnostic 3-D upwelling physics"
#endif
#if defined MASKING || defined WET_DRY || defined WEC || defined WEC_VF || \
    defined NESTING || defined CURVGRID || defined UV_C2ADVECTION || \
    defined UV_VIS4 || defined SINGLE_PRECISION
#  error "Unsupported Kokkos step2d physics or precision configuration"
#endif

namespace {

using roms::Array2d;
using roms::Array3d;
using roms::Array4d;
using roms::EdgeFlags;
using roms::TileBounds;

using exec_space = roms::exec_space;
using team_policy = Kokkos::TeamPolicy<exec_space>;
using member_type = team_policy::member_type;

#if defined ROMS_KOKKOS_SW64
constexpr int TEAM = 64;  // one core group; the backend fixes this
#else
constexpr int TEAM = 1;   // the Serial backend rejects larger teams
#endif

struct Shape {
  int LBi, UBi, LBj, UBj;
  int IminS, ImaxS, JminS, JmaxS;
};

inline Array2d<double> grid2d(void* p, const Shape& s) {
  return roms::as_2d<double>(p, s.LBi, s.UBi, s.LBj, s.UBj);
}
inline Array3d<double> grid3d(void* p, const Shape& s, int lb_k, int ub_k) {
  return roms::as_3d<double>(p, s.LBi, s.UBi, s.LBj, s.UBj, lb_k, ub_k);
}
inline Array4d<double> grid4d(void* p, const Shape& s, int lb_k, int ub_k,
                              int lb_l, int ub_l) {
  return roms::as_4d<double>(p, s.LBi, s.UBi, s.LBj, s.UBj, lb_k, ub_k,
                             lb_l, ub_l);
}
inline Array2d<double> priv2d(void* p, const Shape& s) {
  return roms::as_2d<double>(p, s.IminS, s.ImaxS, s.JminS, s.JmaxS);
}
inline Array3d<double> priv3d(void* p, const Shape& s, int lb_k, int ub_k) {
  return roms::as_3d<double>(p, s.IminS, s.ImaxS, s.JminS, s.JmaxS, lb_k, ub_k);
}

constexpr int TLEV0 = 1, TLEV1 = 3;
constexpr int RLEV0 = 1, RLEV1 = 2;

KOKKOS_INLINE_FUNCTION
double ddx(const roms::Array3d<double>& a, int k, int i, int j,
           int fold_lo, int fold_hi, int lo_idx, int hi_idx) {
  if (fold_lo && i == lo_idx) ++i;
  else if (fold_hi && i == hi_idx) --i;
  return a(i - 1, j, k) - 2.0 * a(i, j, k) + a(i + 1, j, k);
}
KOKKOS_INLINE_FUNCTION
double ddy(const roms::Array3d<double>& a, int k, int i, int j,
           int fold_lo, int fold_hi, int lo_idx, int hi_idx) {
  if (fold_lo && j == lo_idx) ++j;
  else if (fold_hi && j == hi_idx) --j;
  return a(i, j - 1, k) - 2.0 * a(i, j, k) + a(i, j + 1, k);
}
KOKKOS_INLINE_FUNCTION
double ddx2(const roms::Array2d<double>& a, int i, int j,
            int fold_lo, int fold_hi, int lo_idx, int hi_idx) {
  if (fold_lo && i == lo_idx) ++i;
  else if (fold_hi && i == hi_idx) --i;
  return a(i - 1, j) - 2.0 * a(i, j) + a(i + 1, j);
}
KOKKOS_INLINE_FUNCTION
double ddy2(const roms::Array2d<double>& a, int i, int j,
            int fold_lo, int fold_hi, int lo_idx, int hi_idx) {
  if (fold_lo && j == lo_idx) ++j;
  else if (fold_hi && j == hi_idx) --j;
  return a(i, j - 1) - 2.0 * a(i, j) + a(i, j + 1);
}

/* Rows per league.  A league is one team; keeping it to a single row makes
** the fixed cost of a league dominate anything the kernels do, so aim for
** about TARGET cells per league (TARGET/TEAM per member). */
constexpr int TARGET_CELLS = 65536;
KOKKOS_INLINE_FUNCTION
int rows_per_team(int ni) {
  return ni >= TARGET_CELLS ? 1 : (TARGET_CELLS + ni - 1) / ni;
}

}  // namespace

/*
** A 2-D pass: j over [JLO,JHI], i over [ILO,IHI], split across the team.
** The body sees `i` and `j`; `r` is the row index inside the team.
*/
/*
** Scratch source.  By default the slice comes from the caller's host
** buffer.  Compile with KK_STEP2D_TEAM_LDM to take it from the SW64
** backend's shared-LDM pool instead -- that requires
** -cache_size 32 -ldm_share_mode 5 -ldm_share_size 128 at launch, see
** the file header.  All leagues reuse offset 0 of the pool, which is
** safe only because this backend runs leagues one at a time.
*/
#if defined KK_STEP2D_TEAM_LDM
#define KK_TEAM_SCRATCH_ATTR(n)                                               \
  .set_scratch_size(0, Kokkos::PerTeam(int((n) * sizeof(double))))
#define KK_TEAM_SCRATCH_PTR(team, n)                                          \
  static_cast<double*>(team.team_scratch(0).get_shmem(int((n) * sizeof(double))))
#else
#define KK_TEAM_SCRATCH_ATTR(n)
#define KK_TEAM_SCRATCH_PTR(team, n) (sh_base + std::size_t((team).league_rank()) * (n))
#endif

#define TEAM_PASS(JLO, JHI, ILO, IHI, BODY)                                   \
  do {                                                                        \
    const int ni_ = (IHI) - (ILO) + 1;                                        \
    const int rows_ = (JHI) - (JLO) + 1;                                      \
    const int rq_ = rows_per_team(ni_);                                       \
    const int rt_ = (rq_ < rows_ ? rq_ : rows_);                              \
    const int lg_ = (rows_ + rt_ - 1) / rt_;                                  \
    Kokkos::parallel_for(                                                     \
        team_policy(lg_, TEAM), KOKKOS_LAMBDA(const member_type & team) {     \
          const int jb_ = (JLO) + team.league_rank() * rt_;                   \
          const int jn_ = (JHI) - jb_ + 1;                                    \
          const int cnt_ = (jn_ < rt_ ? jn_ : rt_) * ni_;                     \
          Kokkos::parallel_for(                                               \
              Kokkos::TeamThreadRange(team, cnt_), [&](int n_) {              \
                const int j = jb_ + n_ / ni_;                                 \
                const int i = (ILO) + n_ % ni_;                               \
                const int r = n_ / ni_;                                       \
                (void)r;                                                      \
                BODY                                                          \
              });                                                             \
          team.team_barrier();                                                \
        });                                                                   \
  } while (0)

/* Same, with a team-scoped scratch slice of PROW doubles per row.
** The buffer is supplied by the caller (see the file header for why it is
** not team_scratch()). */
#define TEAM_PASS_SH(JLO, JHI, ILO, IHI, PROW, BODY)                          \
  do {                                                                        \
    const int ni_ = (IHI) - (ILO) + 1;                                        \
    const int rows_ = (JHI) - (JLO) + 1;                                      \
    const int rq_ = rows_per_team(ni_);                                       \
    const int rt_ = (rq_ < rows_ ? rq_ : rows_);                              \
    const int lg_ = (rows_ + rt_ - 1) / rt_;                                  \
    const int prow_ = (PROW);                                                 \
    Kokkos::parallel_for(                                                     \
        team_policy(lg_, TEAM) KK_TEAM_SCRATCH_ATTR(rt_ * prow_),             \
        KOKKOS_LAMBDA(const member_type & team) {                             \
          const int jb_ = (JLO) + team.league_rank() * rt_;                   \
          double* sh = KK_TEAM_SCRATCH_PTR(team, rt_ * prow_);                \
          (void)sh;                                                           \
          const int jn_ = (JHI) - jb_ + 1;                                    \
          const int cnt_ = (jn_ < rt_ ? jn_ : rt_) * ni_;                     \
          Kokkos::parallel_for(                                               \
              Kokkos::TeamThreadRange(team, cnt_), [&](int n_) {              \
                const int j = jb_ + n_ / ni_;                                 \
                const int i = (ILO) + n_ % ni_;                               \
                const int r = n_ / ni_;                                       \
                (void)r;                                                      \
                BODY                                                          \
              });                                                             \
          team.team_barrier();                                                \
        });                                                                   \
  } while (0)

/*
** One pass with a team-scratch hand-off: FILL writes the row cache, the
** barrier makes it visible to the whole team, USE reads it.  Keeping both
** halves in one launch is required -- the scratch slice is not carried
** across launches.
*/
#define TEAM_PASS_SH2(JLO, JHI, ILO, IHI, PROW, FILL, USE)                    \
  do {                                                                        \
    const int ni_ = (IHI) - (ILO) + 1;                                        \
    const int rows_ = (JHI) - (JLO) + 1;                                      \
    const int rq_ = rows_per_team(ni_);                                       \
    const int rt_ = (rq_ < rows_ ? rq_ : rows_);                              \
    const int lg_ = (rows_ + rt_ - 1) / rt_;                                  \
    const int prow_ = (PROW);                                                 \
    Kokkos::parallel_for(                                                     \
        team_policy(lg_, TEAM) KK_TEAM_SCRATCH_ATTR(rt_ * prow_),             \
        KOKKOS_LAMBDA(const member_type & team) {                             \
          const int jb_ = (JLO) + team.league_rank() * rt_;                   \
          double* sh = KK_TEAM_SCRATCH_PTR(team, rt_ * prow_);                \
          (void)sh;                                                           \
          const int jn_ = (JHI) - jb_ + 1;                                    \
          const int cnt_ = (jn_ < rt_ ? jn_ : rt_) * ni_;                     \
          Kokkos::parallel_for(                                               \
              Kokkos::TeamThreadRange(team, cnt_), [&](int n_) {              \
                const int j = jb_ + n_ / ni_;                                 \
                const int i = (ILO) + n_ % ni_;                               \
                const int r = n_ / ni_;                                       \
                (void)r;                                                      \
                FILL                                                          \
              });                                                             \
          team.team_barrier();                                                \
          Kokkos::parallel_for(                                               \
              Kokkos::TeamThreadRange(team, cnt_), [&](int n_) {              \
                const int j = jb_ + n_ / ni_;                                 \
                const int i = (ILO) + n_ % ni_;                               \
                const int r = n_ / ni_;                                       \
                (void)r;                                                      \
                USE                                                           \
              });                                                             \
          team.team_barrier();                                                \
        });                                                                   \
  } while (0)


/* A 1-D pass over i, one cell per member. */
#define TEAM_PASS1(ILO, IHI, BODY)                                            \
  do {                                                                        \
    const int n1_ = (IHI) - (ILO) + 1;                                        \
    const int lg_ = (n1_ + TEAM - 1) / TEAM;                                  \
    Kokkos::parallel_for(                                                     \
        team_policy(lg_, TEAM), KOKKOS_LAMBDA(const member_type & team) {     \
          const int i = (ILO) + team.league_rank() * TEAM + team.team_rank(); \
          if (i <= (IHI)) {                                                   \
            BODY                                                              \
          }                                                                   \
          team.team_barrier();                                                \
        });                                                                   \
  } while (0)

extern "C" {

/*======================================================================
** Stage 1 -- total depth and mass fluxes.  The fused variant keeps Drhs
** for the row in scratch so the DUon pass reads LDM instead of the
** global scratch array.
**====================================================================*/

void k_step2d_depth_flux_team(int ng, int tile, int krhs, int nfast,
                              int LBi, int UBi, int LBj, int UBj,
                              int IminS, int ImaxS, int JminS, int JmaxS,
                              void* zeta_p, void* h_p, void* on_u_p,
                              void* om_v_p, void* ubar_p, void* vbar_p,
                              void* Drhs_p, void* DUon_p, void* DVom_p,
                              int UBk, const TileBounds* t, void* scratch_p) {
  roms::initialize();
  (void)ng; (void)tile; (void)nfast; (void)UBk;
  double* sh_base = static_cast<double*>(scratch_p);
  (void)sh_base;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array2d<double> h    = grid2d(h_p, s);
  Array2d<double> on_u = grid2d(on_u_p, s);
  Array2d<double> om_v = grid2d(om_v_p, s);
  Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array2d<double> Drhs = priv2d(Drhs_p, s);
  Array2d<double> DUon = priv2d(DUon_p, s);
  Array2d<double> DVom = priv2d(DVom_p, s);

#if defined DISTRIBUTE && !defined NESTING
  const int j_start = t->JstrV - 2;
  const int i_start = t->IstrU - 2;
#else
  const int j_start = t->JstrVm2 - 1;
  const int i_start = t->IstrUm2 - 1;
#endif
  const int i_end = t->Iendp2;
  const int j_end = t->Jendp2;

  /* Drhs row in scratch, then DUon from it, with one halo column on the
  ** left for the difference.  One launch, so the cache survives. */
  TEAM_PASS_SH2(j_start, j_end, i_start, i_end, i_end - i_start + 2, {
    Drhs(i, j) = zeta(i, j, krhs) + h(i, j);
    sh[r * prow_ + (i - i_start) + 1] = Drhs(i, j);
    if (i == i_start) sh[r * prow_ + 0] = zeta(i - 1, j, krhs) + h(i - 1, j);
  }, {
    if (i > i_start) {
      const double dr  = sh[r * prow_ + (i - i_start) + 1];
      const double drm = sh[r * prow_ + (i - i_start)];
      const double cff  = 0.5 * on_u(i, j);
      DUon(i, j) = ubar(i, j, krhs) * (cff * (dr + drm));
    }
  });

  /* DVom reads the previous row's Drhs, which is not in this team's
  ** scratch, so it stays global. */
  TEAM_PASS(j_start + 1, j_end, i_start, i_end, {
    const double cff  = 0.5 * om_v(i, j);
    const double cff1 = cff * (Drhs(i, j) + Drhs(i, j - 1));
    DVom(i, j) = vbar(i, j, krhs) * cff1;
  });
}

/*======================================================================
** Stage 2 -- time averaging.
**====================================================================*/

void k_step2d_time_average_team(int ng, int tile, int krhs,
                                int predictor, int first_step,
                                int LBi, int UBi, int LBj, int UBj,
                                int IminS, int ImaxS, int JminS, int JmaxS,
                                void* zeta_p, void* DUon_p, void* DVom_p,
                                void* Zt_avg1_p, void* DU_avg1_p,
                                void* DU_avg2_p, void* DV_avg1_p,
                                void* DV_avg2_p, double cff1, double cff2,
                                const TileBounds* t) {
  roms::initialize();
  (void)ng; (void)tile;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array2d<double> DUon = priv2d(DUon_p, s);
  Array2d<double> DVom = priv2d(DVom_p, s);
  Array2d<double> Zt_avg1 = grid2d(Zt_avg1_p, s);
  Array2d<double> DU_avg1 = grid2d(DU_avg1_p, s);
  Array2d<double> DU_avg2 = grid2d(DU_avg2_p, s);
  Array2d<double> DV_avg1 = grid2d(DV_avg1_p, s);
  Array2d<double> DV_avg2 = grid2d(DV_avg2_p, s);

  const int Istr  = t->Istr;
  const int IstrR = t->IstrR, IendR = t->IendR;
  const int Jstr  = t->Jstr;
  const int JstrR = t->JstrR, JendR = t->JendR;

  if (predictor) {
    if (first_step) {
      TEAM_PASS(JstrR, JendR, IstrR, IendR, {
        Zt_avg1(i, j) = 0.0;
      });
      TEAM_PASS(JstrR, JendR, Istr, IendR, {
        DU_avg1(i, j) = 0.0;
        DU_avg2(i, j) = cff2 * DUon(i, j);
      });
      TEAM_PASS(Jstr, JendR, IstrR, IendR, {
        DV_avg1(i, j) = 0.0;
        DV_avg2(i, j) = cff2 * DVom(i, j);
      });
    } else {
      TEAM_PASS(JstrR, JendR, IstrR, IendR, {
        Zt_avg1(i, j) += cff1 * zeta(i, j, krhs);
      });
      TEAM_PASS(JstrR, JendR, Istr, IendR, {
        DU_avg1(i, j) += cff1 * DUon(i, j);
        DU_avg2(i, j) += cff2 * DUon(i, j);
      });
      TEAM_PASS(Jstr, JendR, IstrR, IendR, {
        DV_avg1(i, j) += cff1 * DVom(i, j);
        DV_avg2(i, j) += cff2 * DVom(i, j);
      });
    }
  } else {
    TEAM_PASS(JstrR, JendR, Istr, IendR, {
      DU_avg2(i, j) += cff2 * DUon(i, j);
    });
    TEAM_PASS(Jstr, JendR, IstrR, IendR, {
      DV_avg2(i, j) += cff2 * DVom(i, j);
    });
  }
}

/*======================================================================
** Stage 4 -- free-surface advance.
**====================================================================*/

void k_step2d_zeta_step_team(int ng, int tile, int krhs, int kstp, int knew,
                             int iif, int nfast, int ntfirst, int iic,
                             int predictor, int first_2d_step,
                             int LBi, int UBi, int LBj, int UBj, int UBk,
                             int IminS, int ImaxS, int JminS, int JmaxS,
                             void* zeta_p, void* h_p, void* pm_p, void* pn_p,
                             void* DUon_p, void* DVom_p,
                             void* gzeta_p, void* gzeta2_p,
                             void* gzetaSA_p, void* rhoA_p, void* rhoS_p,
                             void* rhs_zeta_p, void* zeta_new_p,
                             void* Dnew_p, void* zwrk_p,
                             void* rzeta_p, double dtfast, double rho0,
                             const TileBounds* t) {
  roms::initialize();
  (void)ng; (void)tile; (void)nfast; (void)UBk; (void)iif;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array2d<double> h  = grid2d(h_p, s);
  Array2d<double> pm = grid2d(pm_p, s);
  Array2d<double> pn = grid2d(pn_p, s);
  Array2d<double> DUon = priv2d(DUon_p, s);
  Array2d<double> DVom = priv2d(DVom_p, s);
  Array2d<double> gzeta   = priv2d(gzeta_p, s);
  Array2d<double> gzeta2  = priv2d(gzeta2_p, s);
  Array2d<double> gzetaSA = priv2d(gzetaSA_p, s);
  Array2d<double> rhoA    = grid2d(rhoA_p, s);
  Array2d<double> rhoS    = grid2d(rhoS_p, s);
  Array2d<double> rhs_zeta = priv2d(rhs_zeta_p, s);
  Array2d<double> zeta_new = priv2d(zeta_new_p, s);
  Array2d<double> Dnew     = priv2d(Dnew_p, s);
  Array2d<double> zwrk     = priv2d(zwrk_p, s);
  Array3d<double> rzeta = roms::as_3d<double>(rzeta_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);

  const double fac  = 1000.0 / rho0;
  const int    ptsk = 3 - kstp;
  const int j0 = t->JstrV - 1, j1 = t->Jend;
  const int i0 = t->IstrU - 1, i1 = t->Iend;

  if (first_2d_step) {
    const double cff1 = dtfast;
    TEAM_PASS(j0, j1, i0, i1, {
      const double rz = (DUon(i, j) - DUon(i + 1, j)) +
                        (DVom(i, j) - DVom(i, j + 1));
      rhs_zeta(i, j) = rz;
      const double zn = zeta(i, j, kstp) + pm(i, j) * pn(i, j) * cff1 * rz;
      zeta_new(i, j) = zn;
      Dnew(i, j)     = zn + h(i, j);
      const double zw = 0.5 * (zeta(i, j, kstp) + zn);
      zwrk(i, j)      = zw;
      gzeta(i, j)     = (fac + rhoS(i, j)) * zw;
      gzeta2(i, j)    = gzeta(i, j) * zw;
      gzetaSA(i, j)   = zw * (rhoS(i, j) - rhoA(i, j));
    });
  } else if (predictor) {
    const double cff1 = 2.0 * dtfast;
    const double cff4 = 4.0 / 25.0;
    const double cff5 = 1.0 - 2.0 * cff4;
    TEAM_PASS(j0, j1, i0, i1, {
      const double rz = (DUon(i, j) - DUon(i + 1, j)) +
                        (DVom(i, j) - DVom(i, j + 1));
      rhs_zeta(i, j) = rz;
      const double zn = zeta(i, j, kstp) + pm(i, j) * pn(i, j) * cff1 * rz;
      zeta_new(i, j) = zn;
      Dnew(i, j)     = zn + h(i, j);
      const double zw =
          cff5 * zeta(i, j, krhs) + cff4 * (zeta(i, j, kstp) + zn);
      zwrk(i, j)      = zw;
      gzeta(i, j)     = (fac + rhoS(i, j)) * zw;
      gzeta2(i, j)    = gzeta(i, j) * zw;
      gzetaSA(i, j)   = zw * (rhoS(i, j) - rhoA(i, j));
    });
  } else {
    const double cff1 = dtfast * 5.0 / 12.0;
    const double cff2 = dtfast * 8.0 / 12.0;
    const double cff3 = dtfast * 1.0 / 12.0;
    const double cff4 = 2.0 / 5.0;
    const double cff5 = 1.0 - cff4;
    TEAM_PASS(j0, j1, i0, i1, {
      const double cff = cff1 * ((DUon(i, j) - DUon(i + 1, j)) +
                                 (DVom(i, j) - DVom(i, j + 1)));
      const double zn = zeta(i, j, kstp) +
                        pm(i, j) * pn(i, j) *
                            (cff + cff2 * rzeta(i, j, kstp) -
                             cff3 * rzeta(i, j, ptsk));
      zeta_new(i, j) = zn;
      Dnew(i, j)     = zn + h(i, j);
      const double zw = cff5 * zn + cff4 * zeta(i, j, krhs);
      zwrk(i, j)      = zw;
      gzeta(i, j)     = (fac + rhoS(i, j)) * zw;
      gzeta2(i, j)    = gzeta(i, j) * zw;
      gzetaSA(i, j)   = zw * (rhoS(i, j) - rhoA(i, j));
    });
  }

  /* The range entry also stores rzeta(:,:,krhs) on predictor steps. */
  TEAM_PASS(t->Jstr, t->Jend, t->Istr, t->Iend, {
    zeta(i, j, knew) = zeta_new(i, j);
    if (predictor) rzeta(i, j, krhs) = rhs_zeta(i, j);
  });
}

/*======================================================================
** Stage 6 -- pressure gradient.
**====================================================================*/

void k_step2d_pressure_grad_team(int ng, int tile,
                                 int LBi, int UBi, int LBj, int UBj,
                                 int IminS, int ImaxS, int JminS, int JmaxS,
                                 void* h_p, void* on_u_p, void* om_v_p,
                                 void* gzeta_p, void* gzeta2_p,
                                 void* gzetaSA_p, void* rhoA_p,
                                 void* zwrk_p,
                                 void* rhs_ubar_p, void* rhs_vbar_p,
                                 void* DiaU2rhs_p, void* DiaV2rhs_p,
                                 int NDM2d, int M2pgrd, double g,
                                 const TileBounds* t) {
  roms::initialize();
  (void)ng; (void)tile; (void)NDM2d;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array2d<double> h       = grid2d(h_p, s);
  Array2d<double> on_u    = grid2d(on_u_p, s);
  Array2d<double> om_v    = grid2d(om_v_p, s);
  Array2d<double> gzeta   = priv2d(gzeta_p, s);
  Array2d<double> gzeta2  = priv2d(gzeta2_p, s);
  Array2d<double> gzetaSA = priv2d(gzetaSA_p, s);
  Array2d<double> rhoA    = grid2d(rhoA_p, s);
  Array2d<double> zwrk    = priv2d(zwrk_p, s);
  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);
#if defined DIAGNOSTICS_UV
  Array3d<double> DiaU2rhs = priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs = priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
#else
  (void)DiaU2rhs_p; (void)DiaV2rhs_p; (void)M2pgrd;
#endif

  const double cff1 = 0.5 * g;
  const double cff2 = 1.0 / 3.0;
  const int iu0 = t->IstrU, iu1 = t->Iend;
  const int iv0 = t->Istr,  iv1 = t->Iend;
  const int jv0 = t->Jstr,  jv1 = t->Jend;
  const int jstr_v = t->JstrV;

  TEAM_PASS(jv0, jv1, iu0, iu1, {
    const double val =
        cff1 * on_u(i, j) *
        ((h(i - 1, j) + h(i, j)) * (gzeta(i - 1, j) - gzeta(i, j)) +
         (h(i - 1, j) - h(i, j)) *
             (gzetaSA(i - 1, j) + gzetaSA(i, j) +
              cff2 * (rhoA(i - 1, j) - rhoA(i, j)) *
                  (zwrk(i - 1, j) - zwrk(i, j))) +
         (gzeta2(i - 1, j) - gzeta2(i, j)));
    rhs_ubar(i, j) = val;
#if defined DIAGNOSTICS_UV
    DiaU2rhs(i, j, M2pgrd) = val;
#endif
  });

  TEAM_PASS(jv0, jv1, iv0, iv1, {
    if (j >= jstr_v) {
      const double val =
          cff1 * om_v(i, j) *
          ((h(i, j - 1) + h(i, j)) * (gzeta(i, j - 1) - gzeta(i, j)) +
           (h(i, j - 1) - h(i, j)) *
               (gzetaSA(i, j - 1) + gzetaSA(i, j) +
                cff2 * (rhoA(i, j - 1) - rhoA(i, j)) *
                    (zwrk(i, j - 1) - zwrk(i, j))) +
           (gzeta2(i, j - 1) - gzeta2(i, j)));
      rhs_vbar(i, j) = val;
#if defined DIAGNOSTICS_UV
      DiaV2rhs(i, j, M2pgrd) = val;
#endif
    }
  });
}

/*======================================================================
** Stage 7 -- fourth-order horizontal advection.
**====================================================================*/

void k_step2d_hadv_centered4_team(int ng, int tile, int krhs,
                                  int LBi, int UBi, int LBj, int UBj,
                                  int IminS, int ImaxS, int JminS, int JmaxS,
                                  void* ubar_p, void* vbar_p,
                                  void* DUon_p, void* DVom_p,
                                  void* grad_p, void* Dgrad_p,
                                  void* UFx_p, void* UFe_p,
                                  void* VFx_p, void* VFe_p,
                                  void* rhs_ubar_p, void* rhs_vbar_p,
                                  void* DiaU2rhs_p, void* DiaV2rhs_p,
                                  int NDM2d,
                                  int M2xadv, int M2yadv, int M2hadv,
                                  const EdgeFlags* e,
                                  const TileBounds* t) {
  roms::initialize();
  (void)ng; (void)tile; (void)NDM2d;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array2d<double> DUon = priv2d(DUon_p, s);
  Array2d<double> DVom = priv2d(DVom_p, s);
  Array2d<double> grad  = priv2d(grad_p, s);
  Array2d<double> Dgrad = priv2d(Dgrad_p, s);
  Array2d<double> UFx = priv2d(UFx_p, s);
  Array2d<double> UFe = priv2d(UFe_p, s);
  Array2d<double> VFx = priv2d(VFx_p, s);
  Array2d<double> VFe = priv2d(VFe_p, s);
  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);
#if defined DIAGNOSTICS_UV
  Array3d<double> DiaU2rhs = priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs = priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
#else
  (void)DiaU2rhs_p; (void)DiaV2rhs_p;
  (void)M2xadv; (void)M2yadv; (void)M2hadv;
#endif

  const double cff = 1.0 / 6.0;
  const int Istr = t->Istr, Iend = t->Iend;
  const int Jstr = t->Jstr, Jend = t->Jend;
  const int IstrU = t->IstrU, JstrV = t->JstrV;
  const int IstrUm1 = t->IstrUm1, Iendp1 = t->Iendp1;
  const int JstrVm1 = t->JstrVm1, Jendp1 = t->Jendp1;
  const int Jstrm1 = t->Jstrm1, Istrm1 = t->Istrm1;

  TEAM_PASS(Jstr, Jend, IstrUm1, Iendp1, {
    grad(i, j)  = ubar(i - 1, j, krhs) - 2.0 * ubar(i, j, krhs) +
                  ubar(i + 1, j, krhs);
    Dgrad(i, j) = DUon(i - 1, j) - 2.0 * DUon(i, j) + DUon(i + 1, j);
  });

  if (!e->composite_w && !e->periodic_ew && e->west) {
    TEAM_PASS(Jstr, Jend, Istr, Istr, {
      grad(Istr, j)  = grad(Istr + 1, j);
      Dgrad(Istr, j) = Dgrad(Istr + 1, j);
    });
  }
  if (!e->composite_e && !e->periodic_ew && e->east) {
    TEAM_PASS(Jstr, Jend, Iend + 1, Iend + 1, {
      grad(Iend + 1, j)  = grad(Iend, j);
      Dgrad(Iend + 1, j) = Dgrad(Iend, j);
    });
  }

  TEAM_PASS(Jstr, Jend, IstrU - 1, Iend, {
    UFx(i, j) =
        0.25 * (ubar(i, j, krhs) + ubar(i + 1, j, krhs) -
                cff * (grad(i, j) + grad(i + 1, j))) *
        (DUon(i, j) + DUon(i + 1, j) -
         cff * (Dgrad(i, j) + Dgrad(i + 1, j)));
  });

  TEAM_PASS(Jstrm1, Jendp1, IstrU, Iend, {
    grad(i, j) = ubar(i, j - 1, krhs) - 2.0 * ubar(i, j, krhs) +
                 ubar(i, j + 1, krhs);
  });

  if (!e->composite_s && !e->periodic_ns && e->south) {
    TEAM_PASS1(IstrU, Iend, {
      grad(i, Jstr - 1) = grad(i, Jstr);
    });
  }
  if (!e->composite_n && !e->periodic_ns && e->north) {
    TEAM_PASS1(IstrU, Iend, {
      grad(i, Jend + 1) = grad(i, Jend);
    });
  }

  TEAM_PASS(Jstr, Jend + 1, IstrU - 1, Iend, {
    Dgrad(i, j) = DVom(i - 1, j) - 2.0 * DVom(i, j) + DVom(i + 1, j);
  });

  TEAM_PASS(Jstrm1, Jend + 1, IstrU, Iend, {
    UFe(i, j) =
        0.25 * (ubar(i, j, krhs) + ubar(i, j - 1, krhs) -
                cff * (grad(i, j) + grad(i, j - 1))) *
        (DVom(i, j) + DVom(i - 1, j) -
         cff * (Dgrad(i, j) + Dgrad(i - 1, j)));
  });

  TEAM_PASS(JstrV, Jend, Istrm1, Iendp1, {
    grad(i, j) = vbar(i - 1, j, krhs) - 2.0 * vbar(i, j, krhs) +
                 vbar(i + 1, j, krhs);
  });

  if (!e->composite_w && !e->periodic_ew && e->west) {
    TEAM_PASS(JstrV, Jend, Istr - 1, Istr - 1, {
      grad(Istr - 1, j) = grad(Istr, j);
    });
  }
  if (!e->composite_e && !e->periodic_ew && e->east) {
    TEAM_PASS(JstrV, Jend, Iend + 1, Iend + 1, {
      grad(Iend + 1, j) = grad(Iend, j);
    });
  }

  TEAM_PASS(JstrVm1, Jend, Istr, Iend + 1, {
    Dgrad(i, j) = DUon(i, j - 1) - 2.0 * DUon(i, j) + DUon(i, j + 1);
  });

  TEAM_PASS(JstrV, Jend, Istr, Iend + 1, {
    VFx(i, j) =
        0.25 * (vbar(i, j, krhs) + vbar(i - 1, j, krhs) -
                cff * (grad(i, j) + grad(i - 1, j))) *
        (DUon(i, j) + DUon(i, j - 1) -
         cff * (Dgrad(i, j) + Dgrad(i, j - 1)));
  });

  TEAM_PASS(JstrVm1, Jendp1, Istr, Iend, {
    grad(i, j)  = vbar(i, j - 1, krhs) - 2.0 * vbar(i, j, krhs) +
                  vbar(i, j + 1, krhs);
    Dgrad(i, j) = DVom(i, j - 1) - 2.0 * DVom(i, j) + DVom(i, j + 1);
  });

  if (!e->composite_s && !e->periodic_ns && e->south) {
    TEAM_PASS1(Istr, Iend, {
      grad(i, Jstr)  = grad(i, Jstr + 1);
      Dgrad(i, Jstr) = Dgrad(i, Jstr + 1);
    });
  }
  if (!e->composite_n && !e->periodic_ns && e->north) {
    TEAM_PASS1(Istr, Iend, {
      grad(i, Jend + 1)  = grad(i, Jend);
      Dgrad(i, Jend + 1) = Dgrad(i, Jend);
    });
  }

  TEAM_PASS(JstrV - 1, Jend, Istr, Iend, {
    VFe(i, j) =
        0.25 * (vbar(i, j, krhs) + vbar(i, j + 1, krhs) -
                cff * (grad(i, j) + grad(i, j + 1))) *
        (DVom(i, j) + DVom(i, j + 1) -
         cff * (Dgrad(i, j) + Dgrad(i, j + 1)));
  });

  TEAM_PASS(Jstr, Jend, IstrU, Iend, {
    const double cff1 = UFx(i, j) - UFx(i - 1, j);
    const double cff2 = UFe(i, j + 1) - UFe(i, j);
    const double f    = cff1 + cff2;
    rhs_ubar(i, j) -= f;
#if defined DIAGNOSTICS_UV
    DiaU2rhs(i, j, M2xadv) = -cff1;
    DiaU2rhs(i, j, M2yadv) = -cff2;
    DiaU2rhs(i, j, M2hadv) = -f;
#endif
  });

  TEAM_PASS(JstrV, Jend, Istr, Iend, {
    const double cff1 = VFx(i + 1, j) - VFx(i, j);
    const double cff2 = VFe(i, j) - VFe(i, j - 1);
    const double f    = cff1 + cff2;
    rhs_vbar(i, j) -= f;
#if defined DIAGNOSTICS_UV
    DiaV2rhs(i, j, M2xadv) = -cff1;
    DiaV2rhs(i, j, M2yadv) = -cff2;
    DiaV2rhs(i, j, M2hadv) = -f;
#endif
  });
}

/*======================================================================
** Stage 8 -- Coriolis.  The fused variant keeps the UFx row in scratch;
** VFe cannot be cached because rhs_vbar needs VFe(j-1) from the
** previous row.
**====================================================================*/

void k_step2d_coriolis_team(int ng, int tile, int krhs,
                            int LBi, int UBi, int LBj, int UBj,
                            int IminS, int ImaxS, int JminS, int JmaxS,
                            void* Drhs_p, void* fomn_p,
                            void* ubar_p, void* vbar_p,
                            void* UFx_p, void* VFe_p,
                            void* rhs_ubar_p, void* rhs_vbar_p,
                            void* DiaU2rhs_p, void* DiaV2rhs_p,
                            int NDM2d, int M2fcor,
                            const TileBounds* t, void* scratch_p) {
  roms::initialize();
  (void)ng; (void)tile; (void)NDM2d;
  double* sh_base = static_cast<double*>(scratch_p);
  (void)sh_base;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array2d<double> Drhs = priv2d(Drhs_p, s);
  Array2d<double> fomn = grid2d(fomn_p, s);
  Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array2d<double> UFx = priv2d(UFx_p, s);
  Array2d<double> VFe = priv2d(VFe_p, s);
  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);
#if defined DIAGNOSTICS_UV
  Array3d<double> DiaU2rhs = priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs = priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
#else
  (void)DiaU2rhs_p; (void)DiaV2rhs_p; (void)M2fcor;
#endif

  const int j0 = t->JstrV - 1, j1 = t->Jend;
  const int i0 = t->IstrU - 1, i1 = t->Iend;
  const int jstr = t->Jstr, jstrv = t->JstrV;
  const int istr = t->Istr, istrU = t->IstrU;

  /* Fluxes into scratch (with a left halo column), then the U divergence
  ** from the cached row.  VFe still goes to its global array because
  ** rhs_vbar needs VFe(j-1), which belongs to the previous row. */
  TEAM_PASS_SH2(j0, j1, i0, i1, i1 - i0 + 2, {
    const double c = 0.5 * Drhs(i, j) * fomn(i, j);
    const double ux = c * (vbar(i, j, krhs) + vbar(i, j + 1, krhs));
    UFx(i, j) = ux;
    VFe(i, j) = c * (ubar(i, j, krhs) + ubar(i + 1, j, krhs));
    sh[r * prow_ + (i - i0) + 1] = ux;
    if (i == i0) {
      const double cm = 0.5 * Drhs(i - 1, j) * fomn(i - 1, j);
      sh[r * prow_ + 0] =
          cm * (vbar(i - 1, j, krhs) + vbar(i - 1, j + 1, krhs));
    }
  }, {
    if (i >= istrU && j >= jstr) {
      const double f = 0.5 * (sh[r * prow_ + (i - i0) + 1] +
                              sh[r * prow_ + (i - i0)]);
      rhs_ubar(i, j) += f;
#if defined DIAGNOSTICS_UV
      DiaU2rhs(i, j, M2fcor) = f;
#endif
    }
  });

  /* Pass 3: the V divergence stays on the global VFe array. */
  TEAM_PASS(jstrv, j1, istr, i1, {
    const double f = 0.5 * (VFe(i, j) + VFe(i, j - 1));
    rhs_vbar(i, j) -= f;
#if defined DIAGNOSTICS_UV
    DiaV2rhs(i, j, M2fcor) = -f;
#endif
  });
}

/*======================================================================
** Stage 10 -- total depth at PSI points.
**====================================================================*/

void k_step2d_psi_depth_team(int ng, int tile,
                             int LBi, int UBi, int LBj, int UBj,
                             int IminS, int ImaxS, int JminS, int JmaxS,
                             void* Drhs_p, void* Drhs_psi_p,
                             const TileBounds* t) {
  roms::initialize();
  (void)ng; (void)tile;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array2d<double> Drhs     = priv2d(Drhs_p, s);
  Array2d<double> Drhs_psi = priv2d(Drhs_psi_p, s);

#if defined UV_VIS4
  const int j0 = t->Jstrm1, j1 = t->Jendp2;
  const int i0 = t->Istrm1, i1 = t->Iendp2;
#else
  const int j0 = t->Jstr, j1 = t->Jend + 1;
  const int i0 = t->Istr, i1 = t->Iend + 1;
#endif

  TEAM_PASS(j0, j1, i0, i1, {
    Drhs_psi(i, j) = 0.25 * (Drhs(i, j) + Drhs(i - 1, j) +
                             Drhs(i, j - 1) + Drhs(i - 1, j - 1));
  });
}

/*======================================================================
** Stage 11 -- harmonic horizontal viscosity.
**====================================================================*/

void k_step2d_hvisc2_team(int ng, int tile, int krhs,
                          int LBi, int UBi, int LBj, int UBj,
                          int IminS, int ImaxS, int JminS, int JmaxS,
                          void* Drhs_p, void* Drhs_psi_p,
                          void* ubar_p, void* vbar_p,
                          void* pm_p, void* pn_p,
                          void* pmon_r_p, void* pnom_r_p,
                          void* pmon_p_p, void* pnom_p_p,
                          void* om_r_p, void* on_r_p,
                          void* om_p_p, void* on_p_p,
                          void* visc2_r_p, void* visc2_p2_p,
                          void* UFx_p, void* UFe_p,
                          void* VFx_p, void* VFe_p,
                          void* rhs_ubar_p, void* rhs_vbar_p,
                          void* DiaU2rhs_p, void* DiaV2rhs_p,
                          int NDM2d, int M2hvis, int M2xvis, int M2yvis,
                          const TileBounds* t) {
  roms::initialize();
  (void)ng; (void)tile; (void)NDM2d;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array2d<double> Drhs     = priv2d(Drhs_p, s);
  Array2d<double> Drhs_psi = priv2d(Drhs_psi_p, s);
  Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array2d<double> pm = grid2d(pm_p, s);
  Array2d<double> pn = grid2d(pn_p, s);
  Array2d<double> pmon_r = grid2d(pmon_r_p, s);
  Array2d<double> pnom_r = grid2d(pnom_r_p, s);
  Array2d<double> pmon_p = grid2d(pmon_p_p, s);
  Array2d<double> pnom_p = grid2d(pnom_p_p, s);
  Array2d<double> om_r   = grid2d(om_r_p, s);
  Array2d<double> on_r   = grid2d(on_r_p, s);
  Array2d<double> om_p   = grid2d(om_p_p, s);
  Array2d<double> on_p   = grid2d(on_p_p, s);
  Array2d<double> visc2_r = grid2d(visc2_r_p, s);
  Array2d<double> visc2_pp = grid2d(visc2_p2_p, s);
  Array2d<double> UFx = priv2d(UFx_p, s);
  Array2d<double> UFe = priv2d(UFe_p, s);
  Array2d<double> VFx = priv2d(VFx_p, s);
  Array2d<double> VFe = priv2d(VFe_p, s);
  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);
#if defined DIAGNOSTICS_UV
  Array3d<double> DiaU2rhs = priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs = priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
#else
  (void)DiaU2rhs_p; (void)DiaV2rhs_p;
  (void)M2hvis; (void)M2xvis; (void)M2yvis;
#endif

  const int Istr = t->Istr, Iend = t->Iend;
  const int Jstr = t->Jstr, Jend = t->Jend;
  const int IstrU = t->IstrU, JstrV = t->JstrV;

  TEAM_PASS(JstrV - 1, Jend, IstrU - 1, Iend, {
    const double c =
        visc2_r(i, j) * Drhs(i, j) * 0.5 *
        (pmon_r(i, j) *
             ((pn(i, j) + pn(i + 1, j)) * ubar(i + 1, j, krhs) -
              (pn(i - 1, j) + pn(i, j)) * ubar(i, j, krhs)) -
         pnom_r(i, j) *
             ((pm(i, j) + pm(i, j + 1)) * vbar(i, j + 1, krhs) -
              (pm(i, j - 1) + pm(i, j)) * vbar(i, j, krhs)));
    UFx(i, j) = on_r(i, j) * on_r(i, j) * c;
    VFe(i, j) = om_r(i, j) * om_r(i, j) * c;
  });

  TEAM_PASS(Jstr, Jend + 1, Istr, Iend + 1, {
    const double c =
        visc2_pp(i, j) * Drhs_psi(i, j) * 0.5 *
        (pmon_p(i, j) *
             ((pn(i, j - 1) + pn(i, j)) * vbar(i, j, krhs) -
              (pn(i - 1, j - 1) + pn(i - 1, j)) * vbar(i - 1, j, krhs)) +
         pnom_p(i, j) *
             ((pm(i - 1, j) + pm(i, j)) * ubar(i, j, krhs) -
              (pm(i - 1, j - 1) + pm(i, j - 1)) * ubar(i, j - 1, krhs)));
    UFe(i, j) = om_p(i, j) * om_p(i, j) * c;
    VFx(i, j) = on_p(i, j) * on_p(i, j) * c;
  });

  TEAM_PASS(Jstr, Jend, IstrU, Iend, {
    const double cff1 =
        0.5 * (pn(i - 1, j) + pn(i, j)) * (UFx(i, j) - UFx(i - 1, j));
    const double cff2 =
        0.5 * (pm(i - 1, j) + pm(i, j)) * (UFe(i, j + 1) - UFe(i, j));
    const double f = cff1 + cff2;
    rhs_ubar(i, j) += f;
#if defined DIAGNOSTICS_UV
    DiaU2rhs(i, j, M2hvis) = f;
    DiaU2rhs(i, j, M2xvis) = cff1;
    DiaU2rhs(i, j, M2yvis) = cff2;
#endif
  });

  TEAM_PASS(JstrV, Jend, Istr, Iend, {
    const double cff1 =
        0.5 * (pn(i, j - 1) + pn(i, j)) * (VFx(i + 1, j) - VFx(i, j));
    const double cff2 =
        0.5 * (pm(i, j - 1) + pm(i, j)) * (VFe(i, j) - VFe(i, j - 1));
    const double f = cff1 - cff2;
    rhs_vbar(i, j) += f;
#if defined DIAGNOSTICS_UV
    DiaV2rhs(i, j, M2hvis) = f;
    DiaV2rhs(i, j, M2xvis) = cff1;
    DiaV2rhs(i, j, M2yvis) = -cff2;
#endif
  });
}

/*======================================================================
** Stage 14 -- 2-D / 3-D coupling.
**====================================================================*/

void k_step2d_coupling_team(int ng, int tile, int istp, int inew,
                            int iic, int ntfirst, int first_2d_step,
                            int predictor,
                            int LBi, int UBi, int LBj, int UBj,
                            int IminS, int ImaxS, int JminS, int JmaxS,
                            void* rufrc_p, void* rvfrc_p,
                            void* ru_p, void* rv_p,
                            void* rhs_ubar_p, void* rhs_vbar_p,
                            void* DiaU2rhs_p, void* DiaV2rhs_p,
                            void* DiaRUfrc_p, void* DiaRVfrc_p,
                            int NDM2d, int M2pgrd, int M2sstr, int M2bstr,
                            const TileBounds* t) {
  roms::initialize();
  (void)ng; (void)tile; (void)NDM2d;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array2d<double> rufrc = grid2d(rufrc_p, s);
  Array2d<double> rvfrc = grid2d(rvfrc_p, s);
  Array4d<double> ru = grid4d(ru_p, s, 0, t->UBk, 1, 2);
  Array4d<double> rv = grid4d(rv_p, s, 0, t->UBk, 1, 2);
  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);
#if defined DIAGNOSTICS_UV
  Array3d<double> DiaU2rhs = priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs = priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
  Array4d<double> DiaRUfrc = grid4d(DiaRUfrc_p, s, 1, 3, 1, NDM2d - 1);
  Array4d<double> DiaRVfrc = grid4d(DiaRVfrc_p, s, 1, 3, 1, NDM2d - 1);
#else
  (void)DiaU2rhs_p; (void)DiaV2rhs_p;
  (void)DiaRUfrc_p; (void)DiaRVfrc_p;
  (void)M2pgrd; (void)M2sstr; (void)M2bstr;
#endif

  const int iu0 = t->IstrU, iu1 = t->Iend;
  const int iv0 = t->Istr,  iv1 = t->Iend;
  const int jv0 = t->Jstr,  jv1 = t->Jend;
  const int jstr_v = t->JstrV;
  const bool first = (first_2d_step != 0) && (predictor != 0);

  if (first && iic == ntfirst) {
    TEAM_PASS(jv0, jv1, iu0, iu1, {
      rufrc(i, j) = rufrc(i, j) - rhs_ubar(i, j);
      rhs_ubar(i, j) += rufrc(i, j);
      ru(i, j, 0, istp) = rufrc(i, j);
#if defined DIAGNOSTICS_UV
      for (int id = 1; id <= M2pgrd; ++id) {
        DiaRUfrc(i, j, 3, id) -= DiaU2rhs(i, j, id);
        DiaU2rhs(i, j, id) += DiaRUfrc(i, j, 3, id);
        DiaRUfrc(i, j, istp, id) = DiaRUfrc(i, j, 3, id);
      }
      DiaU2rhs(i, j, M2sstr)       = DiaRUfrc(i, j, 3, M2sstr);
      DiaRUfrc(i, j, istp, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
      DiaU2rhs(i, j, M2bstr)       = DiaRUfrc(i, j, 3, M2bstr);
      DiaRUfrc(i, j, istp, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
#endif
    });
    TEAM_PASS(jv0, jv1, iv0, iv1, {
      if (j >= jstr_v) {
        rvfrc(i, j) = rvfrc(i, j) - rhs_vbar(i, j);
        rhs_vbar(i, j) += rvfrc(i, j);
        rv(i, j, 0, istp) = rvfrc(i, j);
#if defined DIAGNOSTICS_UV
        for (int id = 1; id <= M2pgrd; ++id) {
          DiaRVfrc(i, j, 3, id) -= DiaV2rhs(i, j, id);
          DiaV2rhs(i, j, id) += DiaRVfrc(i, j, 3, id);
          DiaRVfrc(i, j, istp, id) = DiaRVfrc(i, j, 3, id);
        }
        DiaV2rhs(i, j, M2sstr)       = DiaRVfrc(i, j, 3, M2sstr);
        DiaRVfrc(i, j, istp, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
        DiaV2rhs(i, j, M2bstr)       = DiaRVfrc(i, j, 3, M2bstr);
        DiaRVfrc(i, j, istp, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
#endif
      }
    });
  } else if (first && iic == ntfirst + 1) {
    TEAM_PASS(jv0, jv1, iu0, iu1, {
      rufrc(i, j) = rufrc(i, j) - rhs_ubar(i, j);
      rhs_ubar(i, j) = rhs_ubar(i, j) + 1.5 * rufrc(i, j) - 0.5 * ru(i, j, 0, inew);
      ru(i, j, 0, istp) = rufrc(i, j);
#if defined DIAGNOSTICS_UV
      for (int id = 1; id <= M2pgrd; ++id) {
        DiaRUfrc(i, j, 3, id) -= DiaU2rhs(i, j, id);
        DiaU2rhs(i, j, id) = DiaU2rhs(i, j, id) + 1.5 * DiaRUfrc(i, j, 3, id) -
                              0.5 * DiaRUfrc(i, j, inew, id);
        DiaRUfrc(i, j, istp, id) = DiaRUfrc(i, j, 3, id);
      }
      DiaU2rhs(i, j, M2sstr) = 1.5 * DiaRUfrc(i, j, 3, M2sstr) -
                               0.5 * DiaRUfrc(i, j, inew, M2sstr);
      DiaRUfrc(i, j, istp, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
      DiaU2rhs(i, j, M2bstr) = 1.5 * DiaRUfrc(i, j, 3, M2bstr) -
                               0.5 * DiaRUfrc(i, j, inew, M2bstr);
      DiaRUfrc(i, j, istp, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
#endif
    });
    TEAM_PASS(jv0, jv1, iv0, iv1, {
      if (j >= jstr_v) {
        rvfrc(i, j) = rvfrc(i, j) - rhs_vbar(i, j);
        rhs_vbar(i, j) = rhs_vbar(i, j) + 1.5 * rvfrc(i, j) - 0.5 * rv(i, j, 0, inew);
        rv(i, j, 0, istp) = rvfrc(i, j);
#if defined DIAGNOSTICS_UV
        for (int id = 1; id <= M2pgrd; ++id) {
          DiaRVfrc(i, j, 3, id) -= DiaV2rhs(i, j, id);
          DiaV2rhs(i, j, id) = DiaV2rhs(i, j, id) + 1.5 * DiaRVfrc(i, j, 3, id) -
                                0.5 * DiaRVfrc(i, j, inew, id);
          DiaRVfrc(i, j, istp, id) = DiaRVfrc(i, j, 3, id);
        }
        DiaV2rhs(i, j, M2sstr) = 1.5 * DiaRVfrc(i, j, 3, M2sstr) -
                                 0.5 * DiaRVfrc(i, j, inew, M2sstr);
        DiaRVfrc(i, j, istp, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
        DiaV2rhs(i, j, M2bstr) = 1.5 * DiaRVfrc(i, j, 3, M2bstr) -
                                 0.5 * DiaRVfrc(i, j, inew, M2bstr);
        DiaRVfrc(i, j, istp, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
#endif
      }
    });
  } else if (first) {
    constexpr double cff1 = 23.0 / 12.0;
    constexpr double cff2 = 16.0 / 12.0;
    constexpr double cff3 = 5.0 / 12.0;
    TEAM_PASS(jv0, jv1, iu0, iu1, {
      rufrc(i, j) = rufrc(i, j) - rhs_ubar(i, j);
      rhs_ubar(i, j) = rhs_ubar(i, j) + cff1 * rufrc(i, j) -
                        cff2 * ru(i, j, 0, inew) +
                        cff3 * ru(i, j, 0, istp);
      ru(i, j, 0, istp) = rufrc(i, j);
#if defined DIAGNOSTICS_UV
      for (int id = 1; id <= M2pgrd; ++id) {
        DiaRUfrc(i, j, 3, id) -= DiaU2rhs(i, j, id);
        DiaU2rhs(i, j, id) = DiaU2rhs(i, j, id) + cff1 * DiaRUfrc(i, j, 3, id) -
                              cff2 * DiaRUfrc(i, j, inew, id) +
                              cff3 * DiaRUfrc(i, j, istp, id);
        DiaRUfrc(i, j, istp, id) = DiaRUfrc(i, j, 3, id);
      }
      DiaU2rhs(i, j, M2sstr) =
          cff1 * DiaRUfrc(i, j, 3, M2sstr) -
          cff2 * DiaRUfrc(i, j, inew, M2sstr) +
          cff3 * DiaRUfrc(i, j, istp, M2sstr);
      DiaRUfrc(i, j, istp, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
      DiaU2rhs(i, j, M2bstr) =
          cff1 * DiaRUfrc(i, j, 3, M2bstr) -
          cff2 * DiaRUfrc(i, j, inew, M2bstr) +
          cff3 * DiaRUfrc(i, j, istp, M2bstr);
      DiaRUfrc(i, j, istp, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
#endif
    });
    TEAM_PASS(jv0, jv1, iv0, iv1, {
      if (j >= jstr_v) {
        rvfrc(i, j) = rvfrc(i, j) - rhs_vbar(i, j);
        rhs_vbar(i, j) = rhs_vbar(i, j) + cff1 * rvfrc(i, j) -
                          cff2 * rv(i, j, 0, inew) +
                          cff3 * rv(i, j, 0, istp);
        rv(i, j, 0, istp) = rvfrc(i, j);
#if defined DIAGNOSTICS_UV
        for (int id = 1; id <= M2pgrd; ++id) {
          DiaRVfrc(i, j, 3, id) -= DiaV2rhs(i, j, id);
          DiaV2rhs(i, j, id) = DiaV2rhs(i, j, id) + cff1 * DiaRVfrc(i, j, 3, id) -
                                cff2 * DiaRVfrc(i, j, inew, id) +
                                cff3 * DiaRVfrc(i, j, istp, id);
          DiaRVfrc(i, j, istp, id) = DiaRVfrc(i, j, 3, id);
        }
        DiaV2rhs(i, j, M2sstr) =
            cff1 * DiaRVfrc(i, j, 3, M2sstr) -
            cff2 * DiaRVfrc(i, j, inew, M2sstr) +
            cff3 * DiaRVfrc(i, j, istp, M2sstr);
        DiaRVfrc(i, j, istp, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
        DiaV2rhs(i, j, M2bstr) =
            cff1 * DiaRVfrc(i, j, 3, M2bstr) -
            cff2 * DiaRVfrc(i, j, inew, M2bstr) +
            cff3 * DiaRVfrc(i, j, istp, M2bstr);
        DiaRVfrc(i, j, istp, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
#endif
      }
    });
  } else {
    TEAM_PASS(jv0, jv1, iu0, iu1, {
      rhs_ubar(i, j) += rufrc(i, j);
#if defined DIAGNOSTICS_UV
      for (int id = 1; id <= M2pgrd; ++id) {
        DiaU2rhs(i, j, id) += DiaRUfrc(i, j, 3, id);
      }
      DiaU2rhs(i, j, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
      DiaU2rhs(i, j, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
#endif
    });
    TEAM_PASS(jv0, jv1, iv0, iv1, {
      if (j >= jstr_v) {
        rhs_vbar(i, j) += rvfrc(i, j);
#if defined DIAGNOSTICS_UV
        for (int id = 1; id <= M2pgrd; ++id) {
          DiaV2rhs(i, j, id) += DiaRVfrc(i, j, 3, id);
        }
        DiaV2rhs(i, j, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
        DiaV2rhs(i, j, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
#endif
      }
    });
  }
}

/*======================================================================
** Stage 17 -- 2-D momentum time step.
**====================================================================*/

void k_step2d_momentum_step_team(int ng, int tile, int kstp, int knew,
                                 int LBi, int UBi, int LBj, int UBj,
                                 int IminS, int ImaxS, int JminS, int JmaxS,
                                 void* zeta_p, void* h_p,
                                 void* ubar_p, void* vbar_p,
                                 void* rubar_p, void* rvbar_p,
                                 void* pm_p, void* pn_p,
                                 void* rhs_ubar_p, void* rhs_vbar_p,
                                 void* Dstp_p, void* Dnew_p,
                                 int predictor, int first_2d_step,
                                 double dtfast, const TileBounds* t) {
  roms::initialize();
  (void)ng; (void)tile; (void)predictor;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
  Array3d<double> rubar = roms::as_3d<double>(rubar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);
  Array3d<double> rvbar = roms::as_3d<double>(rvbar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);
  Array2d<double> h  = grid2d(h_p, s);
  Array2d<double> pm = grid2d(pm_p, s);
  Array2d<double> pn = grid2d(pn_p, s);
  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);
  Array2d<double> Dstp = priv2d(Dstp_p, s);
  Array2d<double> Dnew = priv2d(Dnew_p, s);

  const int ptsk = 3 - kstp;
  const int j0 = t->JstrV - 1, j1 = t->Jend;
  const int i0 = t->IstrU - 1, i1 = t->Iend;

  TEAM_PASS(j0, j1, i0, i1, {
    Dstp(i, j) = zeta(i, j, kstp) + h(i, j);
  });

  const int iu0 = t->IstrU, iu1 = t->Iend;
  const int iv0 = t->Istr,  iv1 = t->Iend;
  const int jv0 = t->Jstr,  jv1 = t->Jend;
  const int jstr_v = t->JstrV;

  if (first_2d_step) {
    const double cff1 = 0.5 * dtfast;
    TEAM_PASS(jv0, jv1, iu0, iu1, {
      const double cff = (pm(i, j) + pm(i - 1, j)) * (pn(i, j) + pn(i - 1, j));
      const double div = 1.0 / (Dnew(i, j) + Dnew(i - 1, j));
      ubar(i, j, knew) =
          (ubar(i, j, kstp) * (Dstp(i, j) + Dstp(i - 1, j)) +
           cff * cff1 * rhs_ubar(i, j)) * div;
    });
    TEAM_PASS(jv0, jv1, iv0, iv1, {
      if (j >= jstr_v) {
        const double cff = (pm(i, j) + pm(i, j - 1)) * (pn(i, j) + pn(i, j - 1));
        const double div = 1.0 / (Dnew(i, j) + Dnew(i, j - 1));
        vbar(i, j, knew) =
            (vbar(i, j, kstp) * (Dstp(i, j) + Dstp(i, j - 1)) +
             cff * cff1 * rhs_vbar(i, j)) * div;
      }
    });
  } else if (predictor) {
    const double cff1 = dtfast;
    TEAM_PASS(jv0, jv1, iu0, iu1, {
      const double cff = (pm(i, j) + pm(i - 1, j)) * (pn(i, j) + pn(i - 1, j));
      const double div = 1.0 / (Dnew(i, j) + Dnew(i - 1, j));
      ubar(i, j, knew) =
          (ubar(i, j, kstp) * (Dstp(i, j) + Dstp(i - 1, j)) +
           cff * cff1 * rhs_ubar(i, j)) * div;
    });
    TEAM_PASS(jv0, jv1, iv0, iv1, {
      if (j >= jstr_v) {
        const double cff = (pm(i, j) + pm(i, j - 1)) * (pn(i, j) + pn(i, j - 1));
        const double div = 1.0 / (Dnew(i, j) + Dnew(i, j - 1));
        vbar(i, j, knew) =
            (vbar(i, j, kstp) * (Dstp(i, j) + Dstp(i, j - 1)) +
             cff * cff1 * rhs_vbar(i, j)) * div;
      }
    });
  } else {
    const double cff1 = 0.5 * dtfast * 5.0 / 12.0;
    const double cff2 = 0.5 * dtfast * 8.0 / 12.0;
    const double cff3 = 0.5 * dtfast * 1.0 / 12.0;
    TEAM_PASS(jv0, jv1, iu0, iu1, {
      const double cff = (pm(i, j) + pm(i - 1, j)) * (pn(i, j) + pn(i - 1, j));
      const double div = 1.0 / (Dnew(i, j) + Dnew(i - 1, j));
      ubar(i, j, knew) =
          (ubar(i, j, kstp) * (Dstp(i, j) + Dstp(i - 1, j)) +
           cff * (cff1 * rhs_ubar(i, j) + cff2 * rubar(i, j, kstp) -
                  cff3 * rubar(i, j, ptsk))) * div;
    });
    TEAM_PASS(jv0, jv1, iv0, iv1, {
      if (j >= jstr_v) {
        const double cff = (pm(i, j) + pm(i, j - 1)) * (pn(i, j) + pn(i, j - 1));
        const double div = 1.0 / (Dnew(i, j) + Dnew(i, j - 1));
        vbar(i, j, knew) =
            (vbar(i, j, kstp) * (Dstp(i, j) + Dstp(i, j - 1)) +
             cff * (cff1 * rhs_vbar(i, j) + cff2 * rvbar(i, j, kstp) -
                    cff3 * rvbar(i, j, ptsk))) * div;
      }
    });
  }
}

/*======================================================================
** Stage 18 -- store the predictor right-hand side.
**====================================================================*/

void k_step2d_store_rhs_team(int ng, int tile, int krhs,
                             int LBi, int UBi, int LBj, int UBj,
                             int IminS, int ImaxS, int JminS, int JmaxS,
                             void* rubar_p, void* rvbar_p,
                             void* rhs_ubar_p, void* rhs_vbar_p,
                             void* DiaRUbar_p, void* DiaRVbar_p,
                             void* DiaU2rhs_p, void* DiaV2rhs_p,
                             int NDM2d, int M2pgrd,
                             const TileBounds* t) {
  roms::initialize();
  (void)ng; (void)tile; (void)NDM2d;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array3d<double> rubar = roms::as_3d<double>(rubar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);
  Array3d<double> rvbar = roms::as_3d<double>(rvbar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);
  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);
#if defined DIAGNOSTICS_UV
  Array4d<double> DiaRUbar = grid4d(DiaRUbar_p, s, 1, 2, 1, NDM2d - 1);
  Array4d<double> DiaRVbar = grid4d(DiaRVbar_p, s, 1, 2, 1, NDM2d - 1);
  Array3d<double> DiaU2rhs = priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs = priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
#else
  (void)DiaRUbar_p; (void)DiaRVbar_p;
  (void)DiaU2rhs_p; (void)DiaV2rhs_p; (void)M2pgrd;
#endif

  const int iu0 = t->IstrU, iu1 = t->Iend;
  const int iv0 = t->Istr,  iv1 = t->Iend;
  const int jv0 = t->Jstr,  jv1 = t->Jend;
  const int jstr_v = t->JstrV;

  TEAM_PASS(jv0, jv1, iu0, iu1, {
    rubar(i, j, krhs) = rhs_ubar(i, j);
#if defined DIAGNOSTICS_UV
    for (int id = 1; id < NDM2d; ++id) {
      DiaRUbar(i, j, krhs, id) = DiaU2rhs(i, j, id);
    }
#endif
  });

  TEAM_PASS(jv0, jv1, iv0, iv1, {
    if (j >= jstr_v) {
      rvbar(i, j, krhs) = rhs_vbar(i, j);
#if defined DIAGNOSTICS_UV
      for (int id = 1; id < NDM2d; ++id) {
        DiaRVbar(i, j, krhs, id) = DiaV2rhs(i, j, id);
      }
#endif
    }
  });
}

/*======================================================================
** Fused pressure / advection / Coriolis / viscosity RHS.
**
** Scratch caches the xi-direction second differences that cell i+1
** would otherwise recompute for cell i:
**   U rows: ddx(ubar) and ddx2(DUon), one row each plus a left halo.
**   V rows: ddx(vbar) one row, and ddy2(DUon) for rows j-1 and j.
** The eta-direction terms are each used once per cell and stay inline.
**====================================================================*/

void k_step2d_rhs_fused_team(int ng, int tile, int krhs,
                             int LBi, int UBi, int LBj, int UBj,
                             int IminS, int ImaxS, int JminS, int JmaxS,
                             void* h_p, void* on_u_p, void* om_v_p,
                             void* gzeta_p, void* gzeta2_p, void* gzetaSA_p,
                             void* rhoA_p, void* zwrk_p,
                             void* Drhs_p, void* fomn_p,
                             void* ubar_p, void* vbar_p,
                             void* DUon_p, void* DVom_p,
                             void* pm_p, void* pn_p,
                             void* pmon_r_p, void* pnom_r_p,
                             void* pmon_p_p, void* pnom_p_p,
                             void* om_r_p, void* on_r_p,
                             void* om_p_p, void* on_p_p,
                             void* visc2_r_p, void* visc2_p2_p,
                             void* rhs_ubar_p, void* rhs_vbar_p,
                             void* DiaU2rhs_p, void* DiaV2rhs_p,
                             int NDM2d,
                             int M2pgrd, int M2xadv, int M2yadv, int M2hadv,
                             int M2fcor, int M2hvis, int M2xvis, int M2yvis,
                             double g, const EdgeFlags* e,
                             const TileBounds* t, void* scratch_p) {
  roms::initialize();
  (void)ng; (void)tile; (void)NDM2d;
  double* sh_base = static_cast<double*>(scratch_p);
  (void)sh_base;

  const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};
  Array2d<double> h       = grid2d(h_p, s);
  Array2d<double> on_u    = grid2d(on_u_p, s);
  Array2d<double> om_v    = grid2d(om_v_p, s);
  Array2d<double> gzeta   = priv2d(gzeta_p, s);
  Array2d<double> gzeta2  = priv2d(gzeta2_p, s);
  Array2d<double> gzetaSA = priv2d(gzetaSA_p, s);
  Array2d<double> rhoA    = grid2d(rhoA_p, s);
  Array2d<double> zwrk    = priv2d(zwrk_p, s);
  Array2d<double> Drhs = priv2d(Drhs_p, s);
  Array2d<double> fomn = grid2d(fomn_p, s);
  Array3d<double> ubar = grid3d(ubar_p, s, TLEV0, TLEV1);
  Array3d<double> vbar = grid3d(vbar_p, s, TLEV0, TLEV1);
  Array2d<double> DUon = priv2d(DUon_p, s);
  Array2d<double> DVom = priv2d(DVom_p, s);
  Array2d<double> pm = grid2d(pm_p, s);
  Array2d<double> pn = grid2d(pn_p, s);
  Array2d<double> pmon_r = grid2d(pmon_r_p, s);
  Array2d<double> pnom_r = grid2d(pnom_r_p, s);
  Array2d<double> pmon_p = grid2d(pmon_p_p, s);
  Array2d<double> pnom_p = grid2d(pnom_p_p, s);
  Array2d<double> om_r   = grid2d(om_r_p, s);
  Array2d<double> on_r   = grid2d(on_r_p, s);
  Array2d<double> om_p   = grid2d(om_p_p, s);
  Array2d<double> on_p   = grid2d(on_p_p, s);
  Array2d<double> visc2_r  = grid2d(visc2_r_p, s);
  Array2d<double> visc2_pp = grid2d(visc2_p2_p, s);
  Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
  Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);
#if defined DIAGNOSTICS_UV
  Array3d<double> DiaU2rhs = priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
  Array3d<double> DiaV2rhs = priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
#else
  (void)DiaU2rhs_p; (void)DiaV2rhs_p;
  (void)M2pgrd; (void)M2xadv; (void)M2yadv; (void)M2hadv;
  (void)M2fcor; (void)M2hvis; (void)M2xvis; (void)M2yvis;
#endif

  const int Istr = t->Istr, Iend = t->Iend;
  const int IstrU = t->IstrU, JstrV = t->JstrV;
  const int Jstr = t->Jstr, Jend = t->Jend;

  const int fw = (!e->composite_w && !e->periodic_ew && e->west);
  const int fe = (!e->composite_e && !e->periodic_ew && e->east);
  const int fs = (!e->composite_s && !e->periodic_ns && e->south);
  const int fn = (!e->composite_n && !e->periodic_ns && e->north);

  const double cpg  = 0.5 * g;
  const double cpg2 = 1.0 / 3.0;
  const double c6   = 1.0 / 6.0;

  /* U rows.  Cache slot x = i - (IstrU-1); the cell at i sits at x = o+1
  ** with o = i - IstrU, so neighbours are o-1, o, o+1.  Each face
  ** derivative is computed once for the whole row instead of once per
  ** cell that touches it. */
  {
    const int ni = Iend - IstrU + 1;   // cells in the row
    const int nf = ni + 2;             // cached faces
    const int prow = 2 * nf;
    TEAM_PASS_SH2(Jstr, Jend, IstrU - 1, Iend + 1, prow, {
      const int x = i - (IstrU - 1);
      sh[r * prow + x] = ddx(ubar, krhs, i, j, fw, fe, Istr, Iend + 1);
      sh[r * prow + nf + x] = ddx2(DUon, i, j, fw, fe, Istr, Iend + 1);
    }, {
      if (i >= IstrU && i <= Iend) {
      const int o = i - IstrU + 1;
      const double gx0 = sh[r * prow + o];
      const double gx1 = sh[r * prow + o + 1];
      const double gxm = sh[r * prow + o - 1];
      const double dx0 = sh[r * prow + nf + o];
      const double dx1 = sh[r * prow + nf + o + 1];
      const double dxm = sh[r * prow + nf + o - 1];
      const double ufx =
          0.25 * (ubar(i, j, krhs) + ubar(i + 1, j, krhs) -
                  c6 * (gx0 + gx1)) *
          (DUon(i, j) + DUon(i + 1, j) - c6 * (dx0 + dx1));
      const double ufxm =
          0.25 * (ubar(i - 1, j, krhs) + ubar(i, j, krhs) -
                  c6 * (gxm + gx0)) *
          (DUon(i - 1, j) + DUon(i, j) - c6 * (dxm + dx0));

      /* Remaining terms are per-cell. */
      const double pg =
          cpg * on_u(i, j) *
          ((h(i - 1, j) + h(i, j)) * (gzeta(i - 1, j) - gzeta(i, j)) +
           (h(i - 1, j) - h(i, j)) *
               (gzetaSA(i - 1, j) + gzetaSA(i, j) +
                cpg2 * (rhoA(i - 1, j) - rhoA(i, j)) *
                    (zwrk(i - 1, j) - zwrk(i, j))) +
           (gzeta2(i - 1, j) - gzeta2(i, j)));
      double acc = pg;
#if defined DIAGNOSTICS_UV
      DiaU2rhs(i, j, M2pgrd) = pg;
#endif
      const double gy0 = ddy(ubar, krhs, i, j, fs, fn, Jstr - 1, Jend + 1);
      const double gy_1 = ddy(ubar, krhs, i, j - 1, fs, fn, Jstr - 1, Jend + 1);
      const double dgy0 = ddx2(DVom, i, j, 0, 0, 0, 0);
      const double dgy_1 = ddx2(DVom, i - 1, j, 0, 0, 0, 0);
      const double ufe =
          0.25 * (ubar(i, j, krhs) + ubar(i, j - 1, krhs) -
                  c6 * (gy0 + gy_1)) *
          (DVom(i, j) + DVom(i - 1, j) - c6 * (dgy0 + dgy_1));
      const double gy1 = ddy(ubar, krhs, i, j + 1, fs, fn, Jstr - 1, Jend + 1);
      const double dgy1 = ddx2(DVom, i, j + 1, 0, 0, 0, 0);
      const double dgy1m = ddx2(DVom, i - 1, j + 1, 0, 0, 0, 0);
      const double ufe1 =
          0.25 * (ubar(i, j + 1, krhs) + ubar(i, j, krhs) -
                  c6 * (gy1 + gy0)) *
          (DVom(i, j + 1) + DVom(i - 1, j + 1) - c6 * (dgy1 + dgy1m));

      const double axadv = ufx - ufxm;
      const double ayadv = ufe1 - ufe;
      const double aadv  = axadv + ayadv;
      acc -= aadv;
#if defined DIAGNOSTICS_UV
      DiaU2rhs(i, j, M2xadv) = -axadv;
      DiaU2rhs(i, j, M2yadv) = -ayadv;
      DiaU2rhs(i, j, M2hadv) = -aadv;
#endif
      const double fcor =
          0.5 * (0.5 * Drhs(i, j) * fomn(i, j) *
                     (vbar(i, j, krhs) + vbar(i, j + 1, krhs)) +
                 0.5 * Drhs(i - 1, j) * fomn(i - 1, j) *
                     (vbar(i - 1, j, krhs) + vbar(i - 1, j + 1, krhs)));
      acc += fcor;
#if defined DIAGNOSTICS_UV
      DiaU2rhs(i, j, M2fcor) = fcor;
#endif
      const double dp00 = 0.25 * (Drhs(i, j) + Drhs(i - 1, j) +
                                  Drhs(i, j - 1) + Drhs(i - 1, j - 1));
      const double dp0p = 0.25 * (Drhs(i, j + 1) + Drhs(i - 1, j + 1) +
                                  Drhs(i, j) + Drhs(i - 1, j));
      const double uR =
          on_r(i, j) * on_r(i, j) * (visc2_r(i, j) * Drhs(i, j) * 0.5 *
          (pmon_r(i, j) *
               ((pn(i, j) + pn(i + 1, j)) * ubar(i + 1, j, krhs) -
                (pn(i - 1, j) + pn(i, j)) * ubar(i, j, krhs)) -
           pnom_r(i, j) *
               ((pm(i, j) + pm(i, j + 1)) * vbar(i, j + 1, krhs) -
                (pm(i, j - 1) + pm(i, j)) * vbar(i, j, krhs))));
      const double uRm =
          on_r(i - 1, j) * on_r(i - 1, j) *
          (visc2_r(i - 1, j) * Drhs(i - 1, j) * 0.5 *
          (pmon_r(i - 1, j) *
               ((pn(i - 1, j) + pn(i, j)) * ubar(i, j, krhs) -
                (pn(i - 2, j) + pn(i - 1, j)) * ubar(i - 1, j, krhs)) -
           pnom_r(i - 1, j) *
               ((pm(i - 1, j) + pm(i - 1, j + 1)) * vbar(i - 1, j + 1, krhs) -
                (pm(i - 1, j - 1) + pm(i - 1, j)) * vbar(i - 1, j, krhs))));
      const double uP =
          om_p(i, j) * om_p(i, j) * (visc2_pp(i, j) * dp00 * 0.5 *
          (pmon_p(i, j) *
               ((pn(i, j - 1) + pn(i, j)) * vbar(i, j, krhs) -
                (pn(i - 1, j - 1) + pn(i - 1, j)) * vbar(i - 1, j, krhs)) +
           pnom_p(i, j) *
               ((pm(i - 1, j) + pm(i, j)) * ubar(i, j, krhs) -
                (pm(i - 1, j - 1) + pm(i, j - 1)) * ubar(i, j - 1, krhs))));
      const double uPp =
          om_p(i, j + 1) * om_p(i, j + 1) * (visc2_pp(i, j + 1) * dp0p * 0.5 *
          (pmon_p(i, j + 1) *
               ((pn(i, j) + pn(i, j + 1)) * vbar(i, j + 1, krhs) -
                (pn(i - 1, j) + pn(i - 1, j + 1)) * vbar(i - 1, j + 1, krhs)) +
           pnom_p(i, j + 1) *
               ((pm(i - 1, j + 1) + pm(i, j + 1)) * ubar(i, j + 1, krhs) -
                (pm(i - 1, j) + pm(i, j)) * ubar(i, j, krhs))));
      const double vx1 = 0.5 * (pn(i - 1, j) + pn(i, j)) * (uR - uRm);
      const double vy1 = 0.5 * (pm(i - 1, j) + pm(i, j)) * (uPp - uP);
      const double hv  = vx1 + vy1;
      acc += hv;
#if defined DIAGNOSTICS_UV
      DiaU2rhs(i, j, M2hvis) = hv;
      DiaU2rhs(i, j, M2xvis) = vx1;
      DiaU2rhs(i, j, M2yvis) = vy1;
#endif
      rhs_ubar(i, j) = acc;
      }
    });
  }

  /* V rows.  Same cache scheme; ddy2(DUon) is needed for rows j and
  ** j-1, so both are cached. */
  {
    const int ni = Iend - Istr + 1;
    const int nf = ni + 2;
    const int prow = 3 * nf;
    TEAM_PASS_SH2(JstrV, Jend, Istr - 1, Iend + 1, prow, {
      const int x = i - (Istr - 1);
      sh[r * prow + x] = ddx(vbar, krhs, i, j, fw, fe, Istr - 1, Iend + 1);
      sh[r * prow + nf + x] = ddy2(DUon, i, j, 0, 0, 0, 0);
      sh[r * prow + 2 * nf + x] = ddy2(DUon, i, j - 1, 0, 0, 0, 0);
    }, {
      if (i >= Istr && i <= Iend) {
      const int o = i - Istr + 1;
      const double gx0 = sh[r * prow + o];
      const double gx1 = sh[r * prow + o + 1];
      const double gxm = sh[r * prow + o - 1];
      const double dx0 = sh[r * prow + nf + o];
      const double dx1 = sh[r * prow + nf + o + 1];
      const double dxm = sh[r * prow + 2 * nf + o];
      const double dx1m = sh[r * prow + 2 * nf + o + 1];
      const double vfx =
          0.25 * (vbar(i, j, krhs) + vbar(i - 1, j, krhs) -
                  c6 * (gx0 + gxm)) *
          (DUon(i, j) + DUon(i, j - 1) - c6 * (dx0 + dxm));
      const double vfx1 =
          0.25 * (vbar(i + 1, j, krhs) + vbar(i, j, krhs) -
                  c6 * (gx1 + gx0)) *
          (DUon(i + 1, j) + DUon(i + 1, j - 1) - c6 * (dx1 + dx1m));

      const double pg =
          cpg * om_v(i, j) *
          ((h(i, j - 1) + h(i, j)) * (gzeta(i, j - 1) - gzeta(i, j)) +
           (h(i, j - 1) - h(i, j)) *
               (gzetaSA(i, j - 1) + gzetaSA(i, j) +
                cpg2 * (rhoA(i, j - 1) - rhoA(i, j)) *
                    (zwrk(i, j - 1) - zwrk(i, j))) +
           (gzeta2(i, j - 1) - gzeta2(i, j)));
      double acc = pg;
#if defined DIAGNOSTICS_UV
      DiaV2rhs(i, j, M2pgrd) = pg;
#endif
      const double gy0 = ddy(vbar, krhs, i, j, fs, fn, Jstr, Jend + 1);
      const double dgy0 = ddy2(DVom, i, j, fs, fn, Jstr, Jend + 1);
      const double gy1 = ddy(vbar, krhs, i, j + 1, fs, fn, Jstr, Jend + 1);
      const double dgy1 = ddy2(DVom, i, j + 1, fs, fn, Jstr, Jend + 1);
      const double vfe =
          0.25 * (vbar(i, j, krhs) + vbar(i, j + 1, krhs) -
                  c6 * (gy0 + gy1)) *
          (DVom(i, j) + DVom(i, j + 1) - c6 * (dgy0 + dgy1));
      const double gym = ddy(vbar, krhs, i, j - 1, fs, fn, Jstr, Jend + 1);
      const double dgym = ddy2(DVom, i, j - 1, fs, fn, Jstr, Jend + 1);
      const double vfem =
          0.25 * (vbar(i, j - 1, krhs) + vbar(i, j, krhs) -
                  c6 * (gym + gy0)) *
          (DVom(i, j - 1) + DVom(i, j) - c6 * (dgym + dgy0));

      const double axadv = vfx1 - vfx;
      const double ayadv = vfe - vfem;
      const double aadv  = axadv + ayadv;
      acc -= aadv;
#if defined DIAGNOSTICS_UV
      DiaV2rhs(i, j, M2xadv) = -axadv;
      DiaV2rhs(i, j, M2yadv) = -ayadv;
      DiaV2rhs(i, j, M2hadv) = -aadv;
#endif
      const double fcor =
          0.5 * (0.5 * Drhs(i, j) * fomn(i, j) *
                     (ubar(i, j, krhs) + ubar(i + 1, j, krhs)) +
                 0.5 * Drhs(i, j - 1) * fomn(i, j - 1) *
                     (ubar(i, j - 1, krhs) + ubar(i + 1, j - 1, krhs)));
      acc -= fcor;
#if defined DIAGNOSTICS_UV
      DiaV2rhs(i, j, M2fcor) = -fcor;
#endif
      const double dp00 = 0.25 * (Drhs(i, j) + Drhs(i - 1, j) +
                                  Drhs(i, j - 1) + Drhs(i - 1, j - 1));
      const double dp10 = 0.25 * (Drhs(i + 1, j) + Drhs(i, j) +
                                  Drhs(i + 1, j - 1) + Drhs(i, j - 1));
      const double vP =
          on_p(i, j) * on_p(i, j) * (visc2_pp(i, j) * dp00 * 0.5 *
          (pmon_p(i, j) *
               ((pn(i, j - 1) + pn(i, j)) * vbar(i, j, krhs) -
                (pn(i - 1, j - 1) + pn(i - 1, j)) * vbar(i - 1, j, krhs)) +
           pnom_p(i, j) *
               ((pm(i - 1, j) + pm(i, j)) * ubar(i, j, krhs) -
                (pm(i - 1, j - 1) + pm(i, j - 1)) * ubar(i, j - 1, krhs))));
      const double vPp =
          on_p(i + 1, j) * on_p(i + 1, j) *
          (visc2_pp(i + 1, j) * dp10 * 0.5 *
          (pmon_p(i + 1, j) *
               ((pn(i + 1, j - 1) + pn(i + 1, j)) * vbar(i + 1, j, krhs) -
                (pn(i, j - 1) + pn(i, j)) * vbar(i, j, krhs)) +
           pnom_p(i + 1, j) *
               ((pm(i, j) + pm(i + 1, j)) * ubar(i + 1, j, krhs) -
                (pm(i, j - 1) + pm(i + 1, j - 1)) * ubar(i + 1, j - 1, krhs))));
      const double vR =
          om_r(i, j) * om_r(i, j) * (visc2_r(i, j) * Drhs(i, j) * 0.5 *
          (pmon_r(i, j) *
               ((pn(i, j) + pn(i + 1, j)) * ubar(i + 1, j, krhs) -
                (pn(i - 1, j) + pn(i, j)) * ubar(i, j, krhs)) -
           pnom_r(i, j) *
               ((pm(i, j) + pm(i, j + 1)) * vbar(i, j + 1, krhs) -
                (pm(i, j - 1) + pm(i, j)) * vbar(i, j, krhs))));
      const double vRm =
          om_r(i, j - 1) * om_r(i, j - 1) *
          (visc2_r(i, j - 1) * Drhs(i, j - 1) * 0.5 *
          (pmon_r(i, j - 1) *
               ((pn(i, j - 1) + pn(i + 1, j - 1)) * ubar(i + 1, j - 1, krhs) -
                (pn(i - 1, j - 1) + pn(i, j - 1)) * ubar(i, j - 1, krhs)) -
           pnom_r(i, j - 1) *
               ((pm(i, j - 1) + pm(i, j)) * vbar(i, j, krhs) -
                (pm(i, j - 2) + pm(i, j - 1)) * vbar(i, j - 1, krhs))));
      const double vx1 = 0.5 * (pn(i, j - 1) + pn(i, j)) * (vPp - vP);
      const double vy1 = 0.5 * (pm(i, j - 1) + pm(i, j)) * (vR - vRm);
      const double hv  = vx1 - vy1;
      acc += hv;
#if defined DIAGNOSTICS_UV
      DiaV2rhs(i, j, M2hvis) = hv;
      DiaV2rhs(i, j, M2xvis) = vx1;
      DiaV2rhs(i, j, M2yvis) = -vy1;
#endif
      rhs_vbar(i, j) = acc;
      }
    });
  }
}

}  // extern "C"

#endif  // ROMS_KOKKOS && KK_STEP2D_TEAM
