# Remaining iteration-time hot spots after the PRE3D/REMAINDER ports

Date: 2026-11-05
Source commit: `58191be0`; the kernels are unchanged through `2cc37f4c`,
where the throughput analysis and the re-ranking below were added.

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

## Where the time goes inside the 2-D kernels

Each rank owns one tile of the 6x2 decomposition, so on the 1440x704 grid a
tile is 240x352 points and one field slab is about 0.69 MB. Dividing the
entry-timer time of each kernel by the number of slabs it reads and writes
gives the sustained throughput it runs at:

| kernel | calls | ms/call | slabs touched per call | GB/s |
|---|---:|---:|---:|---:|
| `store_rhs` | 84 | 1.59 | ~44 (22 read, 22 written) | ~19 |
| `diag_int` | 84 | 3.82 | ~140 (10 terms, U and V) | ~25 |
| `coupling` | 168 | 2.59 | ~80 (8 compact terms plus stresses) | ~22 |
| `rhs_fused` | 168 | 3.79 | ~56 | ~10 |

`NDM2d` is 11 for these configurations (UV_ADV, UV_COR, UV_VIS2), so
`diag_int` integrates 10 terms over U and V and `coupling` loops over the
8 terms below `M2pgrd` plus the two stresses. `store_rhs`, `diag_int` and
`coupling` all sustain 19-25 GB/s, the streaming rate this rank reaches,
so their cost is data movement and their traffic is what the Fortran does.
`rhs_fused` runs at about half that rate because it evaluates the scalar
CPE arithmetic of the whole 2-D RHS: with `-fno-tree-vectorize` it is
ALU-bound, not memory-bound.

## Candidate optimizations, in priority order

1. **Reduce 2-D halo exchange cost**, the only remaining lever above a few
   percent. The 2-D halo exchange is 7.9% of the profiled total (all MPI
   communication 13.0%) and scales with the number of fast steps per
   baroclinic step. Options are nonblocking exchange overlapped with the
   interior update, exchanging only the boundary strips the next kernel
   reads, and skipping exchanges whose result is not read. This is
   Fortran/MPI work, not a Kokkos kernel change, and it changes the order
   of messages, so it needs the full acceptance protocol.
2. **Global reductions and boundary fills.** Small individually, and inside
   the same non-kernel remainder as item 1.
3. **SIMD in `rhs_fused`.** It is 0.636 s of the 3.541 s stepping wall and
   runs at about half the streaming rate, so it is the one large kernel
   whose cost is arithmetic rather than data movement; `coupling` is
   already at the streaming rate and would gain little. Vectorisation is
   therefore the only change that could move `rhs_fused` materially.
   swgcc-1473 changes rounding in auto-vectorised CPE arithmetic, which is
   why the strict-arithmetic policy disables it; a vectorised variant would
   need its own exactness proof before it may be used (TeamPolicy was
   measured slower and was removed, see `ROMS/Cpp/tools/sw64/README.md`,
   Decisions).

The following was listed earlier as the first candidate and is withdrawn
after checking the data flow:

- **Fusing the diagnostic/store passes into their producer.** `store_rhs`
  and `diag_int` do not consume `rhs_fused`'s output directly: `coupling`
  runs between them and rewrites `rhs_ubar`/`rhs_vbar` and `DiaU2rhs`/
  `DiaV2rhs` in place (all four coupling branches), so the copies must stay
  after it. The exact fusion is therefore into `coupling`, which saves the
  overlap between what `coupling` writes and what `store_rhs` reads: about
  15 MB per fast step for `store_rhs` and the `DiaU2rhs` re-read for
  `diag_int`, about 0.12 s or 3% of the stepping wall, in exchange for
  merging a four-branch kernel with two more modes. Not worth it. (MASKING and
  WET_DRY, the only other writers between the kernels, are rejected at
  compile time by `ROMS/Cpp/kokkos_step2d.cpp`.)

## Validation protocol for any follow-up

Every change must keep the existing acceptance gates:

- Serial candidate exactly equal to its matched Serial control.
- SW64 candidate within the normalized 1e-10 physical gate against the
  original Fortran, including all history/average/diagnostic fields.
- No whole-tile Fortran fallback, no row fallback, all 12 ranks executing
  the expected kernels, and no output written in performance runs.
- Two-step no-output timing at 12 ranks, three samples, median; largest
  grid at least 60 s/step in the pure-Fortran reference.
