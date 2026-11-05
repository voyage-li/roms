# Remaining iteration-time hot spots after the PRE3D/REMAINDER ports

Date: 2026-11-05
Source commit: `58191be0`

This note records what is left to optimize after the coarse kernels and the
row-level remainder ports are enabled. It is a planning document based on
the 12-rank profiles; the final campaign numbers live in
`kokkos-mpi12-scale-benchmark.md`.

## Coverage status

For the tested configurations, the iteration-time compute paths are now
covered as follows.

| region | implementation |
|---|---|
| 2-D barotropic step | `kokkos_step2d.cpp` range kernels: depth flux, time average, zeta, fused RHS (pressure gradient, advection, Coriolis, viscosity), coupling, momentum, store RHS, diagnostics |
| 3-D predictor | `KK_STAGE_PRE3D` (tracers and momentum) for CENTERED2/4, UPSTREAM3, AKIMA4 (`A4`), SPLIT_U3, HSIMT and MPDATA (first-order upstream form in the predictor). Any other scheme, tracer point sources, N < 3 or more than 32 tracers run the Fortran loops for the whole tile and print a fallback line. BL_TEST (`SOLAR_SOURCE`) builds without PRE3D and runs `pre_step3d` through the `KK_STAGE_REMAINDER` rows. |
| 3-D momentum RHS | `KK_STAGE_REMAINDER` row ports (`rhs3d`, `rho_eos`, `wvelocity`, `uv_C2A_grid`, `set_vbc`) plus the hand-written `KK_STAGE_PRSGRD`, `KK_STAGE_UV3DMIX` and `KK_STAGE_T3DMIX` kernels |
| 3-D tracer corrector | `ROMS_KOKKOS_STEP3D_T` chunks (U3/C4, HSIMT) where the hook is compiled (`SPLINES_VDIFF` with `DIAGNOSTICS_TS`: UPWELLING, BL_TEST); elsewhere `KK_STAGE_REMAINDER` rows, which also cover CANYON's A4/A4 advection and the non-spline vertical diffusion of KELVIN, CANYON and CHANNEL |
| vertical mixing | `KK_STAGE_GLS` / `KK_STAGE_LMD` where the physics is active |
| diagnostics/averages | `KK_STAGE_DIAG`, `KK_STAGE_TAVG`, `KK_STAGE_REMAINDER` row ports |
| initialization | not ported; excluded from the iteration-time metric |

Boundary conditions, halo/MPI exchanges, global reductions and file I/O
remain in Fortran by design.

## Physics not exercised by the five benchmark cases

The following paths are not active in the retained applications, so they are
neither benchmarked nor newly validated here:

- tracer advection other than U3, C4, HSIMT and A4 (for example MPDATA);
- WEC, WET_DRY, MASKING, BODYFORCE, nesting and moving grids;
- open boundary conditions, nudging, tides and BBL;
- biology, sediment, sea ice and floats;
- KPP/LMD and GLS mixing (these have separate one- and two-rank validation
  and are not active in the five benchmark physics configurations).

A configuration that activates one of these paths must run its own
fallback and field checks before any speedup is claimed.

## Remainder profile

Single 12-rank UPWELLING 1440x704 smoke run (log:
`runs/pre3d-r9-20261105T0250/smoke/upwelling-s3-rem/run.log`, see Evidence in
`kokkos-mpi12-scale-benchmark.md`), two steps, `KK_STAGE_PRE3D` and
`KK_STAGE_REMAINDER` enabled, rank-0 stepping wall 3.541 s total
(1.770 s/step; the final three-sample campaign median for this grid is
1.966 s/step). Kernel entry timers sum to 2.745 s total per rank
(1.372 s/step, 77.5% of the smoke wall). The entry-timer shares of the
stepping wall are:

| item | share of stepping wall |
|---|---:|
| 2-D kernels (all step2d entries) | ~51% |
| 3-D pressure gradient | ~5% |
| 3-D momentum RHS proper (remainder rows) | ~3% |
| 3-D corrector (step3d_t) | ~4% |
| 3-D predictor (pre_step3d) | ~3% |
| other entry kernels and `step3d_uv` | ~11% |
| non-kernel remainder (MPI/exchanges/boundaries/reductions) | ~22% |

In the ROMS wall profile (`ROMS_WALL_PROFILE`; regions can include
initialization) the 2-D MPI halo exchanges alone are 7.9% of the profiled
total and all MPI communication together 13.0%; both sit inside that
non-kernel remainder.

The single largest kernels per rank over two steps are `step2d rhs_fused`
(0.636 s), `step2d coupling` (0.434 s), `step2d diag_int` (0.321 s),
`step3d_uv solve` (0.177 s), `step3d_t chunks` (0.152 s) and
`step2d store_rhs` (0.134 s).

After the remainder ports, there is no large un-migrated compute loop left
in these configurations. The next gains must come from the already-ported
2-D kernels and from communication, not from another module translation.

## Candidate optimizations, in priority order

1. **Fuse the 2-D diagnostic/store passes.**
   `store_rhs` (0.134 s) copies `rhs_ubar`/`rhs_vbar` and the diagnostic
   RHS into `rubar`/`rvbar`; `diag_int` (0.321 s) then accumulates the
   diagnostic integrals in separate launches. Both are full-tile passes
   over data already resident from `rhs_fused`/`coupling`. Fusing the
   copies and integral accumulation into the producer kernel would remove
   two launches and two array sweeps per fast step. This is the largest
   safe target that does not change arithmetic order.
2. **Reduce 2-D halo exchange cost.**
   The 2-D halo exchange is 7.9% of the profiled total (all MPI
   communication 13.0%) and scales with the number of fast steps per
   baroclinic step. Options are nonblocking
   exchange overlapped with the interior update, exchanging only the
   boundary strips needed by the next kernel, and avoiding redundant
   exchanges when a kernel does not read the halo. This is Fortran/MPI
   work, not a Kokkos kernel change.
3. **Tune `rhs_fused` and `coupling` loop order.**
   These two kernels account for about 59% of the 2-D kernel time (1.07 s
   of 1.81 s over the two steps). Under the strict-arithmetic policy the
   kernels evaluate scalar code on each CPE (`-fno-tree-vectorize`), so SIMD
   is an option only with its own exactness check; TeamPolicy was measured
   to be slower on SW64 and was removed (`ROMS/Cpp/tools/sw64/README.md`,
   Decisions). Cache blocking over j and reusing face fluxes between
   `rhs_fused` and `coupling` are the remaining structural options, but both
   need an exact-arithmetic check.
4. **Global reductions and boundary fills.**
   The CFL/global-information reductions and the lateral boundary fills are
   small individually. Porting them is only worth it after the items above.

## Validation protocol for any follow-up

Every change must keep the existing acceptance gates:

- Serial candidate exactly equal to its matched Serial control.
- SW64 candidate within the normalized 1e-10 physical gate against the
  original Fortran, including all history/average/diagnostic fields.
- No whole-tile Fortran fallback, no row fallback, all 12 ranks executing
  the expected kernels, and no output written in performance runs.
- Two-step no-output timing at 12 ranks, three samples, median; largest
  grid at least 60 s/step in the pure-Fortran reference.
