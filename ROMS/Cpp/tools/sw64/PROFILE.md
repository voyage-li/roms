# Where the remaining time goes on SW64 — 2026-11-02

Two independent cases, both on `q_share`, one core group per job, no NetCDF
output. The per-routine tables are ROMS' own timer profile. On this build
they measure the same clock as `SYSTEM_CLOCK`, which can be checked
independently: `2 x 0.549 = 1.098` seconds matches the printed
`Corrector time-step for tracers` of `1.110` s, and `2 x 1.913 = 3.826`
matches the Serial `3.820` s. The listed regions are disjoint (child time
is removed from the parent), so they add up to the printed profile total.

Both cases use the step2d Kokkos kernels. `step3d_t` is Kokkos in the
upwelling case and plain Fortran in the canyon case, which is a build
difference, not a result of this profile: canyon does not define
`SPLINES_VDIFF`/`DIAGNOSTICS_TS`, so the traced kernel is compiled out
there.

## Case A — upwelling, 256 x 256 x 16, NT=2, two steps

Total 13.740 s; profiled 11.970 s (87.1%).

| region | s | % of total | Kokkos |
|---|---:|---:|---|
| Model 2D kernel | 5.550 | 40.4 | yes |
| 3D equations predictor step (pre_step3d) | 1.120 | 8.2 | no |
| Corrector time-step for tracers (step3d_t) | 1.110 | 8.1 | yes |
| 3D equations right-side terms (rhs3d) | 0.730 | 5.3 | no |
| Harmonic stress tensor, S-surfaces (uv3dmix) | 0.570 | 4.1 | no |
| Pressure gradient (prsgrd) | 0.470 | 3.4 | no |
| Harmonic mixing of tracers, S-surfaces (t3dmix) | 0.260 | 1.9 | no |
| 2D/3D coupling, vertical metrics | 0.220 | 1.6 | no |
| Corrector time-step for 3D momentum (step3d_uv) | 0.140 | 1.0 | no |
| Omega vertical velocity | 0.090 | 0.7 | no |
| Equation of state | 0.070 | 0.5 | no |
| Allocation/state init (one-off) | 1.410 | 10.3 | - |
| Global integrals, vertical BC | 0.230 | 1.7 | no |

## Case B — canyon, 256 x 192 x 15, NT=1, four steps, DT=30

Total 15.49 s; profiled 12.76 s (82.4%). Serial comparison run: 26.81 s
stepping versus 14.45 s on SW64, i.e. 1.86x whole-model.

| region | s | % of total | Serial s |
|---|---:|---:|---:|
| Model 2D kernel | 5.630 | 36.3 | 18.000 |
| 3D equations predictor step | 1.460 | 9.4 | 1.430 |
| 3D equations right-side terms | 0.990 | 6.4 | 1.000 |
| Harmonic stress tensor, S-surfaces | 0.840 | 5.4 | 0.790 |
| Harmonic mixing of tracers, geopotentials | 0.800 | 5.2 | 0.810 |
| Pressure gradient | 0.720 | 4.6 | 0.720 |
| Corrector time-step for tracers (Fortran) | 0.710 | 4.6 | 0.740 |
| 2D/3D coupling, vertical metrics | 0.240 | 1.5 | 0.230 |
| Corrector time-step for 3D momentum | 0.160 | 1.0 | 0.200 |
| Omega vertical velocity | 0.140 | 0.9 | 0.110 |
| Equation of state | 0.060 | 0.4 | 0.060 |
| Allocation/state init (one-off) | 0.700 | 4.5 | 0.700 |

The pure-Fortran rows are the same on both backends to within a timer
tick, which is the expected check that only the Kokkos kernels moved.

## The 3.2-3.5x ceiling

| kernel | Serial | SW64 | ratio |
|---|---:|---:|---:|
| step2d (upwelling) | 18.00 | 5.550 | 3.24 |
| step2d (canyon) | 18.00 | 5.630 | 3.20 |
| step3d_t (measured directly) | 1.913 | 0.549 | 3.48 |

Three independent kernels land in the same narrow range. That is not a
property of any one kernel's code; it is where the zero-copy HostSpace
memory path saturates for 64 CPEs. Further module migration should
therefore be budgeted at about 3.3x per module, not 64x, until the SW64
backend stages data closer to the CPEs.

### Correction: the ratio is not a fixed ceiling

A standalone benchmark of the real `step3d_t` kernel
(`bench_step3d_t_kokkos`, three timed calls per size after an untimed
warm-up, mutable time level restored from the predictor outside the timed
region, closed edges, U3/C4 + HSIMT/HSIMT, 20 diagnostics) shows the
ratio depends on the working set:

| columns | N | updates | footprint | Serial s | SW64 s | ratio | Serial ns/update | SW64 ns/update |
|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| 64516 | 16 | 2.06 M | 0.47 GB | 2.154 | 0.863 | 2.50 | 1043 | 418 |
| 260100 | 16 | 8.32 M | 1.83 GB | 14.077 | 3.314 | 4.25 | 1691 | 398 |
| 64516 | 32 | 4.13 M | 0.93 GB | 5.354 | 1.829 | 2.93 | 1297 | 443 |
| 260100 | 32 | 16.65 M | 3.65 GB | 34.764 | 7.431 | 4.68 | 2088 | 446 |

SW64's cost per cell-tracer is flat (398-446 ns) over an 8x increase in
work, so there is no meaningful per-launch overhead at these sizes. The
Serial cost grows from 1043 to 2088 ns/update as the working set exceeds
cache, which is what moves the ratio from 2.5x to 4.7x. The ratio is a
property of the Serial baseline as much as of the SW64 path.

This also means the earlier "3.3x" statement was an artefact of testing
only one tile size: it applies to the 65k-column tile, not to the backend.
Larger tiles per rank favour SW64 more.

One inconsistency is not explained: the model run measured 1.913 s Serial
and 0.549 s SW64 for a 65536-column, 16-level tile (3.48x), while the
standalone fixture at the same size gives 2.154 s and 0.863 s (2.50x).
Fixture values, page residency and node assignment differ; the trend
across sizes is consistent, the absolute ratio at one size is not.

## MPI domain decomposition with Kokkos

### Resource accounting first

`bsub` on this system takes `-n` as the total MPE (process) count and
`-np` as MPEs **per node**; `bsub -l` reports `one node mpes <6>` and a
node has 6 MPE cores (`bqueues` lists `NPROCS 6` per node). With
`-cgsp 64` each rank also takes one 64-CPE core group. So:

| ranks | bsub | nodes | MPE cores | CPEs |
|---:|---|---:|---:|---:|
| 1 | `-n 1 -np 1` | 1 | 1 | 64 |
| 4 | `-n 4 -np 4` | 1 | 4 | 256 |
| 6 | `-n 6 -np 6` | 1 | 6 | 384 |
| 12 | `-n 12 -np 6` | 2 | 12 | 768 |
| 24 | `-n 24 -np 6` | 4 | 24 | 1536 |

`-np 8` is rejected because it asks for 8 MPEs on a 6-MPE node. An
earlier draft of this file claimed a four-rank job used four nodes; it
does not, `bjobs -l` shows `now using <1> nodes` for `-n 4 -np 4`.

`MPI=ON` builds of both backends (`romsM`), upwelling, two steps, no
output, rank 0's `ROMS time stepping wall` (ranks agree to 3e-4 s).

Single node, 1 to 6 ranks:

| case | ranks | Serial | SW64 | SW64/Serial |
|---|---:|---:|---:|---:|
| 128^2 x 16 | 1 | 6.683 | 2.867 | 2.33 |
| | 2 | 3.493 | 1.588 | 2.20 |
| | 4 | 1.856 | 0.891 | 2.08 |
| | 6 | 1.284 | 0.639 | 2.01 |
| 256^2 x 16 | 1 | 27.949 | 11.960 | 2.34 |
| | 2 | 14.368 | 5.878 | 2.44 |
| | 4 | 7.744 | 3.213 | 2.41 |
| | 6 | 5.374 | 2.172 | 2.47 |
| 512^2 x 16 | 1 | 114.466 | 47.530 | 2.41 |
| | 2 | 56.228 | 23.174 | 2.43 |
| | 4 | 29.149 | 12.218 | 2.39 |
| | 6 | 19.811 | 8.229 | 2.41 |

Across nodes (2 and 4 nodes for 12 and 24 ranks):

| case | ranks / nodes | Serial | SW64 | SW64/Serial |
|---|---:|---:|---:|---:|
| 128^2 x 16 | 12 / 2 | 0.687 | 0.419 | 1.64 |
| 256^2 x 16 | 12 / 2 | 2.826 | 1.230 | 2.30 |
| 512^2 x 16 | 12 / 2 | 10.334 | 4.172 | 2.48 |
| 512^2 x 16 | 24 / 4 | 5.363 | 2.297 | 2.33 |

Parallel efficiency against each backend's own one-rank run, 512^2:
Serial 96% at 6 ranks, 92% at 12, 89% at 24; SW64 96%, 95%, 86%. The
128^2 case at 12 ranks is the exception in both backends, where the
per-rank tile is only ~1.4k columns and the exchange cost dominates; that
is also where the SW64 advantage falls to 1.64x.

The SW64 advantage is 2.3-2.5x at every rank count once the per-rank tile
is large enough, so MPI decomposition and the SW64 backend multiply:
512^2 x 16 goes from 114.5 s (1 rank, Kokkos Serial) to 2.30 s (24 ranks
over 4 nodes, Kokkos SW64), 49.8x, at 24x the MPE cores and 1536 CPEs.

Per-rank `step3d_t module wall` at 256^2 (single node):

| ranks | Serial | SW64 | per-rank tile columns |
|---|---:|---:|---:|
| 1 | 1.913 | 0.549 | 65536 |
| 2 | 0.9645 | 0.2014 | 32768 |
| 4 | 0.4797 | 0.1133 | 16384 |

Serial is 1.98x/3.99x, i.e. linear. SW64 is 2.73x/4.85x, better than
linear; with one sample per point, part of that is node and page-residency
variation, so it should not be read as superlinear scaling. Absolute
per-rank wall times are reproducible across backends but not across
sessions.

The four-rank profile shows what limits scaling: the 2D kernel is 5.55 s
at one rank and 6.43 s summed over four (1.61 s per rank, 86% efficiency),
while the 3D modules sum to near-exact linear. The 2D kernel is the
scaling bottleneck, not the MPI halo exchange.

Numerical check at two ranks, same 41 x 80 x 16 case with output:
Serial-MPI is bitwise identical to the one-rank Serial run in all 10
history, 10 average and 18 tracer-diagnostic fields; SW64-MPI differs from
it by at most 1.16e-15 normalized relative, the same level as one-rank
SW64. Both tiles print `step3d_t accelerated` with no fallback.

## Kernel speedup versus whole-program speedup

A pure-Fortran build (`ROMS_KOKKOS=OFF`, `build/reference` and
`build/reference-mpi`) gives the honest denominator: what the program
cost before any of this work. Same cases, two steps, no output. Kernel
rows are ROMS' own timer regions: `Model 2D kernel` for step2d and
`Corrector time-step for tracers` for step3d_t (identical to twice the
instrumented `step3d_t module wall`). Under MPI these regions are rank
sums, so the ratio is unaffected.

256 x 256 x 16, one rank, seconds:

| build | whole program | step2d | step3d_t |
|---|---:|---:|---:|
| pure Fortran | 23.803 | 17.120 | 1.520 |
| Kokkos Serial | 27.949 | 18.910 | 3.820 |
| Kokkos SW64 | 11.960 | 5.550 | 1.110 |

| ratio | step2d | step3d_t | whole program |
|---|---:|---:|---:|
| SW64 / Fortran | 3.08 | 1.37 | **1.99** |
| SW64 / Kokkos Serial | 3.41 | 3.44 | 2.34 |
| Kokkos Serial / Fortran | 0.91 | 0.40 | 0.85 |

Across sizes and rank counts:

| case | ranks / nodes | step2d SW64/Fortran | step2d SW64/Serial | step3d_t SW64/Fortran | step3d_t SW64/Serial | whole SW64/Fortran | whole SW64/Serial |
|---|---:|---:|---:|---:|---:|---:|---:|
| 256^2 | 1 / 1 | 3.08 | 3.41 | 1.37 | 3.44 | 1.99 | 2.34 |
| 256^2 | 6 / 1 | 2.79 | 3.37 | 1.93 | 4.27 | 2.01 | 2.47 |
| 512^2 | 1 / 1 | 3.10 | 3.32 | 1.30 | 4.39 | 1.99 | 2.41 |
| 512^2 | 12 / 2 | 3.08 | 3.52 | 1.64 | 4.09 | 2.07 | 2.48 |
| 512^2 | 24 / 4 | 2.60 | 3.17 | 1.38 | 3.17 | 1.89 | 2.33 |

Raw seconds for the 512^2 rows (whole program / step2d / step3d_t):

| ranks | Fortran | Kokkos Serial | Kokkos SW64 |
|---|---:|---:|---:|
| 1 | 94.644 / 67.130 / 6.380 | 114.466 / 71.850 / 21.480 | 47.530 / 21.640 / 4.890 |
| 12 | 8.651 / 76.060 / 6.180 | 10.334 / 86.940 / 15.420 | 4.172 / 24.690 / 3.770 |
| 24 | 4.333 / 75.940 / 6.660 | 5.363 / 92.400 / 15.330 | 2.297 / 29.170 / 4.830 |

Three things this makes explicit:

- The kernel speedup against the code it replaces is 2.6-3.1x for step2d
  and 1.3-1.9x for step3d_t, not 3.5x. step3d_t is lower because the
  per-column kernel redoes each shared face flux (see above); the 3.4x
  figure is against Kokkos Serial, which is itself slower than Fortran.
- The whole-program speedup against Fortran is a flat **1.9-2.1x**. The
  migrated kernels are 78% of Fortran's 256^2 one-rank time
  ((17.120 + 1.520) / 23.803); with 3.08x and 1.37x on those parts and
  1x on the rest, Amdahl predicts 1 / (0.719/3.08 + 0.064/1.37 + 0.217)
  = 2.02x. Measured 1.99x. The remaining 22% is the 3-D momentum chain,
  mixing, coupling, EOS and I/O, which is exactly the migration list
  below.
- Kokkos Serial is not a faster target than Fortran: it is 0.85x overall
  and 0.40x on step3d_t. Do not use it as a production baseline; it is a
  correctness oracle and a relative yardstick for the SW64 backend.

## What is left to migrate, by measured size

Canyon, SW64, excluding the 2D kernel already done:

1. 3-D momentum chain: pre_step3d 1.46 + rhs3d 0.99 + prsgrd 0.72 +
   step3d_uv 0.16 = **3.33 s, 21.5% of total**. Largest single block, and
   column-local like step3d_t. Highest effort (many physics switches).
2. Horizontal mixing pair: uv3dmix 0.84 + t3dmix 0.80 = **1.64 s, 10.6%**.
   Face-based, the same pattern as the horizontal diffusion already
   written for step3d_t. Lowest risk.
3. Smaller: 2D/3D coupling 0.24, omega 0.14, EOS 0.06, vertical BC 0.02.

At the measured ceiling, migrating both groups would remove about
(3.33 + 1.64) x (1 - 1/3.3) = 3.5 s of the 14.45 s stepping, i.e. roughly
14.45 -> 11 s. The upwelling profile gives the same ranking
(pre_step3d + rhs3d + prsgrd + step3d_uv = 20% of profiled time), so the
ordering is not specific to canyon's topography or forcing.

## Build deviation for the canyon case

`canyon.h` needs `SOLVE3D` from outside, and the existing step2d kernel
requires `VAR_RHO_2D` and `DIAGNOSTICS_UV`. Those three macros were added
for the canyon build only (`build_canyon.sh`); upwelling is untouched.
