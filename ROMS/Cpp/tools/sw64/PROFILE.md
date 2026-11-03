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

## Complete MPI test matrix (after the column_body refactor)

Per-application MPI builds are required: an MPI binary is tied to its
application header, so a bl_test or canyon input run through the upwelling
`romsM` is meaningless (it crashes or silently computes the wrong case).
`build_app.sh` now takes `MPI=ON` and `BACKENDS="serial sw64"` and produces
`<APP>-<backend>-mpi`.

Correctness, two ranks, output enabled, compared with the one-rank run:

| case | grid | 1-rank Fortran -> 2-rank Serial | 1-rank -> 2-rank SW64 | 2-rank Serial -> 2-rank SW64 |
|---|---|---:|---:|---:|
| upwelling | 41x80x16 | 10/10 his + 18/18 dia, exact | same, zeta 1.8e-15 | zeta ~1e-15 |
| canyon | 65x48x15 | 8/8 | 8/8, zeta 4.7e-15 | 8/8, zeta 4.7e-15 |
| bl_test | 39x4x30 | 2-rank Fortran is bitwise equal to 2-rank Serial | 2-rank SW64 within 7.7e-15 | - |

bl_test needs a note: its one-rank and two-rank solutions differ by
2.1e-07 in zeta and 4e-04 in the tracer diagnostics, and **the pure
Fortran build shows exactly the same difference** (identical `maxabs`
to the Kokkos comparison). Two-rank Fortran and two-rank Kokkos Serial
are bitwise equal. The divergence is therefore a property of decomposing
this case in i, not of the kernels; the same one-vs-two-rank check passes
for canyon and upwelling.

Performance, upwelling, two steps, no output (`ROMS time stepping wall`,
rank 0):

| case | ranks / nodes | Serial | SW64 | SW64/Serial |
|---|---:|---:|---:|---:|
| 256^2 x 16 | 1 / 1 | 27.391 | 11.346 | 2.41 |
| 256^2 x 16 | 2 / 1 | 14.386 | 5.869 | 2.45 |
| 256^2 x 16 | 4 / 1 | 7.756 | 3.155 | 2.46 |
| 512^2 x 16 | 12 / 2 | 10.363 | 4.179 | 2.48 |
| 512^2 x 16 | 24 / 4 | 5.357 | 2.235 | 2.40 |

These match the pre-refactor numbers within run-to-run noise, and the
128/128 differential test still passes on both backends, so the
`column_body` split is numerically neutral.

## TeamPolicy experiment on step3d_t

`bench_step3d_t_team_kokkos` compiles two TeamPolicy variants next to the
production kernel. On the SW64 backend a team is fixed at 64 CPEs (one
core group) and the leagues run one after another inside that group, so
TeamPolicy trades launch-level parallelism for team-scoped sharing.

- `team-columns`: one column per member, same work as the range kernel.
  This isolates the cost of the team machinery.
- `team-rows`: one team per row `j`; the team evaluates the x-face fluxes
  for every level once into a shared buffer, then each member advances
  its own columns from those fluxes. This removes the duplicate x-face
  evaluation the per-column kernel does, but not the y-face one.

Both variants reproduce the range kernel bitwise (`max|dt| = 0`,
`max|ddiag| = 0` at every size and backend), because `column_body` uses
the same expressions in the same order and only the flux source changes.

SW64, one core group, seconds per call:

| columns x N | range | team-columns | team-rows | rows vs range |
|---|---:|---:|---:|---:|
| 64516 x 16 | 1.283 | 1.175 | 0.790 | 1.62x |
| 260100 x 16 | 5.013 | 4.643 | 2.880 | 1.74x |
| 64516 x 32 | 2.715 | 2.472 | 2.502 | 1.08x |
| 260100 x 32 | 10.915 | 9.964 | 10.125 | 1.08x |

Kokkos Serial (team size 1, so `team-rows` cannot share anything across
threads and only saves the recomputation inside the single thread):

| columns x N | range | team-columns | team-rows |
|---|---:|---:|---:|
| 64516 x 16 | 2.107 | 2.109 | 1.863 |
| 260100 x 16 | 14.397 | 14.397 | 12.958 |
| 64516 x 32 | 5.280 | 5.285 | 4.189 |

Reading:

- TeamPolicy itself costs nothing here: `team-columns` is 8% *faster* than
  the range kernel on SW64, so the team machinery is not the problem the
  earlier profile hinted at.
- Sharing the x-face flux is a real 1.6-1.7x win at the 16-level
  configuration this model uses, and it also helps single-threaded.
- The win collapses at 32 levels, where the shared buffer doubles to
  66 KB per row and lives in host memory: the extra write-then-read
  traffic cancels the saved arithmetic. Moving that buffer into the team
  scratch (LDM, `set_scratch_size(0, PerTeam(bytes))` +
  `team.team_scratch(0)`, as `Kokkos/example/sw64_team_first_stage`
  does) is the obvious next step before wiring this into the model.
- The y-face flux is still evaluated twice (once by each of two adjacent
  rows), so roughly half of the original duplication remains.

None of this is wired into `step3d_t.F` yet; the production path is still
the range kernel. The refactor that made this possible (`column` split
into `column_body` plus a flux source) is shared by both.

## Every step2d kernel: RangePolicy vs TeamPolicy + scratch

`kokkos_step2d_team.cpp` adds a TeamPolicy entry for each of the twelve
step2d kernels; `bench_step2d_team.cpp` runs both variants from one
restored state, checks the whole array set, then times each.  Production
code is untouched -- these entries compile only under `KK_STEP2D_TEAM`.

Mapping: a league covers `rt = ceil(65536/nx)` rows (capped at the row
count), the members split those rows' cells with `TeamThreadRange`.  A
league therefore carries 64k cells, which is what stops the ~15 us
per-league cost of this backend from dominating; one row per league was
10-20x slower.

Scratch is used where two cells of the same row need the same value:
depth_flux (`Drhs`), coriolis (`UFx`) and rhs_fused (`ddx(ubar)`,
`ddx2(DUon)`, `ddx(vbar)`, `ddy2(DUon)` for rows j and j-1).  The slice
is a caller-supplied buffer, **not** `team_scratch()`: the pinned fork
declares an 8 MB `__thread_local_share` array regardless of the size
requested, and with the `-cache_size 128` used for every run here
`-ldm_share_mode 5 -ldm_share_size 128` dies at startup with
`__ldm_space: __thread_local data is too big (8427232 B)` (or, once the
shared array is placed, `(38608 B)`, i.e. the ordinary TLS no longer
fits), while without the share mode the first access faults with
`accessed wrong shared LDM space`.

`-cache_size` matters here, and 128 is the faster setting, so this is not
a parameter mistake to fix: with the Kokkos `run.sh` value of 32 the
share mode does start, but rhs_fused's *range* kernel goes from 2.12 to
7.28 ms at 256^2 and from 0.185 to 0.288 ms at 41x80.  The remaining nine
kernels have no row-level value to share, so their team entry runs with
zero scratch and measures the policy itself.

Correctness: eleven kernels reproduce the range output **bitwise**.
`momentum_step` differs by 7.1e-15 (about one ulp) in `ubar`/`vbar` on
SW64 and is bitwise identical on the Serial backend -- the same
expression compiled into two different loop shapes, not a logic change.
The check is exhaustive: every element of every array is compared.

SW64, milliseconds per call, one core group, rank-0 range entry first:

| kernel | 41x80 range/team | 65x48 range/team | 39x4 range/team | 256^2 range/team | 512^2 range/team |
|---|---:|---:|---:|---:|---:|
| depth_flux | 0.092 / 0.112 | 0.092 / 0.115 | 0.090 / 0.082 | 0.303 / 0.632 | 0.930 / 2.215 |
| time_average | 0.066 / 0.142 | 0.066 / 0.143 | 0.060 / 0.117 | 0.266 / 0.665 | 0.878 / 2.265 |
| zeta_step | 0.085 / 0.121 | 0.081 / 0.116 | 0.059 / 0.094 | 0.509 / 0.665 | 1.962 / 2.416 |
| pressure_grad | 0.079 / 0.109 | 0.079 / 0.110 | 0.060 / 0.076 | 0.428 / 0.589 | 1.657 / 2.210 |
| hadv_c4 | 0.603 / 0.855 | 0.600 / 0.915 | 0.586 / 0.746 | 1.683 / 3.091 | 4.989 / 9.990 |
| coriolis | 0.102 / 0.120 | 0.099 / 0.117 | 0.090 / 0.083 | 0.379 / 0.685 | 1.218 / 2.469 |
| psi_depth | 0.030 / 0.044 | 0.030 / 0.047 | 0.030 / 0.036 | 0.079 / 0.203 | 0.203 / 0.678 |
| hvisc2 | 0.164 / 0.227 | 0.157 / 0.221 | 0.120 / 0.158 | 0.995 / 1.278 | 3.881 / 4.721 |
| coupling | 0.135 / 0.178 | 0.121 / 0.159 | 0.060 / 0.083 | 1.288 / 1.422 | 5.233 / 5.372 |
| momentum_step | 0.094 / 0.141 | 0.098 / 0.146 | 0.088 / 0.110 | 0.389 / 0.700 | 1.355 / 2.526 |
| store_rhs | 0.133 / 0.174 | 0.120 / 0.156 | 0.061 / 0.083 | 1.254 / 1.321 | 5.095 / 5.034 |
| rhs_fused | 0.185 / 0.275 | 0.172 / 0.268 | 0.080 / 0.137 | 2.135 / 2.824 | 9.214 / 11.906 |

Team/range ratio, same columns.  Above 1.0 means the team version wins:

| kernel | 41x80 | 65x48 | 39x4 | 256^2 | 512^2 |
|---|---:|---:|---:|---:|---:|
| depth_flux | 0.82 | 0.81 | **1.10** | 0.48 | 0.42 |
| time_average | 0.47 | 0.46 | 0.51 | 0.40 | 0.39 |
| zeta_step | 0.70 | 0.69 | 0.63 | 0.77 | 0.81 |
| pressure_grad | 0.73 | 0.72 | 0.79 | 0.73 | 0.75 |
| hadv_c4 | 0.71 | 0.66 | 0.79 | 0.55 | 0.50 |
| coriolis | 0.86 | 0.85 | **1.09** | 0.55 | 0.49 |
| psi_depth | 0.68 | 0.65 | 0.81 | 0.39 | 0.30 |
| hvisc2 | 0.72 | 0.71 | 0.76 | 0.78 | 0.82 |
| coupling | 0.76 | 0.76 | 0.72 | 0.91 | 0.97 |
| momentum_step | 0.67 | 0.67 | 0.80 | 0.56 | 0.54 |
| store_rhs | 0.76 | 0.77 | 0.73 | 0.95 | 1.01 |
| rhs_fused | 0.67 | 0.64 | 0.59 | 0.76 | 0.77 |

Only three entries reach parity, all at sizes where the work is a few
microseconds and the launch cost dominates both variants.  The other
45 cells are 3-70% slower under TeamPolicy.  rhs_fused is the telling
case: the team entry removes the duplicate xi-derivative work (roughly
40% of its arithmetic) and is still 24% slower at 256^2, so the mapping
costs more than the sharing saves.

The per-cell cost, not a per-launch constant, is what remains: at
1024^2 (16 leagues, 1M cells) store_rhs and coupling reach parity while
everything else stays 15-65% slower.  The range kernels' inner
i-loop is a straight `for` the compiler vectorises; the team body is
invoked per cell, and the same arithmetic is not vectorised.

Serial backend, same binary (TEAM=1, so no parallelism to win back):
team/range is 0.04-0.68 everywhere, i.e. 1.5-25x slower.  The team
entries exist to be measured, not used, on that backend.

## Launch parameters: -cache_size and shared LDM

These are two separate knobs and have to be measured separately.  The
controlled comparison is cache 32 with and without the share mode;
comparing "cache 32 + LDM" against "cache 128" changes both at once.

Everything above ran with `-cache_size 128`, which is what iAMAS, the
Kokkos `tools/bsub.sh` and this repository's `run.sh` use.  The Kokkos
LBM3D `run.sh` uses `-cache_size 32 -ldm_share_mode 5
-ldm_share_size 128`.  Same binary, same input, only the flags change:

| | model 41x80, 2 steps | model 256^2, 2 steps | rhs_fused 41x80 | rhs_fused 256^2 | step3d_t 254^2 range | step3d_t 254^2 team-rows |
|---|---:|---:|---:|---:|---:|---:|
| cache 32 | 0.768 s | 13.200 s | 0.286 ms | 7.283 ms | 1.427 s | 1.346 s |
| cache 32 + LDM share | 0.790 s | 13.221 s | 0.272 ms | 7.302 ms | 1.421 s | 1.346 s |
| cache 128 | 0.653 s | 11.890 s | 0.185 ms | 2.130 ms | 1.283 s | 0.790 s |

The share mode changes nothing measurable (0.2-3%, within run noise) --
the production binary never calls `team_scratch()`, so enabling the pool
is free but useless.  The cache size is the real knob: 128 is 11-19%
faster end to end, 3.4x faster on rhs_fused, and step3d_t's team-rows
advantage falls from 1.62x to 1.06x at cache 32.  All of that is the
cache size, not the share mode.

With `-cache_size 32` the share mode is usable, so a second benchmark
(`bench_step2d_team_ldm_kokkos`, `-DKK_STEP2D_TEAM_LDM`) takes the team
slice from `team_scratch()` instead of the caller's host buffer:

| kernel | host scratch team | LDM scratch team | range | LDM/range |
|---|---:|---:|---:|---:|
| depth_flux 256^2 | 0.630 ms | 0.634 ms | 0.303 ms | 0.47 |
| coriolis 256^2 | 0.678 ms | 0.679 ms | 0.381 ms | 0.56 |
| rhs_fused 256^2 | 10.10 ms | 8.49 ms | 7.30 ms | 0.86 |
| rhs_fused 41x80 | 0.465 ms | 0.387 ms | 0.272 ms | 0.72 |

LDM scratch buys rhs_fused about 16% over host memory, and nothing for
the two small kernels (their caches are only a few KB).  It still does
not reach the range kernel, so the scratch medium is not what limits the
team mapping.

## step3d_t under the same comparison

`bench_step3d_t_team_kokkos` now takes `reps ndt nx ny levels`.  The
team-rows variant is the Team+Scratch entry (shared x-face flux, host
buffer).  Both team variants are bitwise identical to the range kernel
at every size on both backends (`max|dt| = 0`, `max|ddiag| = 0`).

| case | backend | range | team-columns | team-rows (scratch) | rows/range |
|---|---|---:|---:|---:|---:|
| 41x80x16 | SW64 | 0.0629 s | 0.0624 s | 0.0622 s | 1.01 |
| 65x48x15 | SW64 | 0.0322 s | 0.0403 s | 0.0403 s | 0.80 |
| 39x4x30 | SW64 | 0.0056 s | 0.0057 s | 0.0056 s | 1.00 |
| 254x254x16 | SW64 | 1.283 s | 1.175 s | 0.790 s | **1.62** |
| 41x80x16 | Serial | 0.1077 s | 0.1078 s | 0.0924 s | **1.17** |
| 65x48x15 | Serial | 0.0952 s | 0.0952 s | 0.0835 s | **1.14** |
| 39x4x30 | Serial | 0.0098 s | 0.0098 s | 0.0086 s | **1.14** |

The shared-flux win needs a large column count per launch to pay for the
team hand-off: it is 1.62x at 65k columns, while at the 3-4k columns of
the model's own ranks it is break-even at best.  The 41x80 SW64 point is
not stable across sessions: an earlier run of the same binary gave
0.0434 s for range and 0.0508 s for team-rows (0.85), so treat 3k-column
SW64 ratios as +/-20% noise, not as a ranking.  On Serial the saving is
real at every size because there is no team overhead to pay, only the
removed duplicate arithmetic.

Conclusion for the backend: TeamPolicy is not a drop-in win for the
migrated kernels.  It is worth wiring into `step3d_t` only for tiles of
tens of thousands of columns, and it should stay out of every step2d
kernel.

## Whole time step, measured end to end

Upwelling, two computed steps, no output, `ROMS time stepping wall` from
the model itself (not a kernel sum).  Serial and SW64 are separate
binaries; every job used `bsub -i roms_upwelling.in`, without which the
model reads its input, prints the parameter banner and then blocks.

| grid | backend / flags | stepping wall, 2 steps | per step | Model 2D kernel, 2 steps | per step | 2D share |
|---|---|---:|---:|---:|---:|---:|
| 41x80x16 | SW64, cache 128 | 0.653 s | 0.327 s | 0.360 s | 0.180 s | 55% |
| 41x80x16 | SW64, cache 32 + LDM | 0.790 s | 0.395 s | 0.460 s | 0.230 s | 58% |
| 41x80x16 | Serial, cache 128 | 1.392 s | 0.696 s | 0.960 s | 0.480 s | 69% |
| 128^2x16 | SW64 | 2.930 s | 1.465 s | 1.430 s | 0.715 s | 49% |
| 128^2x16 | Serial | 6.731 s | 3.365 s | 4.550 s | 2.275 s | 68% |
| 256^2x16 | SW64 | 11.890 s | 5.945 s | 5.570 s | 2.785 s | 47% |
| 256^2x16 | Serial | 28.014 s | 14.007 s | 18.960 s | 9.480 s | 68% |
| 512^2x16 | SW64 | 47.680 s | 23.840 s | 21.710 s | 10.855 s | 46% |
| 512^2x16 | Serial | 115.809 s | 57.904 s | 73.200 s | 36.600 s | 63% |

Whole-step speedup, SW64 / Serial: **2.13x** at 41x80, 2.30x at 128^2,
2.36x at 256^2, 2.43x at 512^2.  The kernel-level ratios (3.2-4.4x) are
damped by the Fortran 3-D half, which is about half the step.

The launch parameters show up end to end too, but only the cache size:
at 41x80 the step is 0.653 s at cache 128, 0.768 s at cache 32 and
0.790 s at cache 32 + LDM share; at 256^2 it is 11.890 / 13.200 /
13.221 s.  The share mode is within noise; cache 32 costs 11-18%.

The 2D region is 46-69% of a step, and every production 2D kernel's
team entry is slower than its range entry (0.58-0.86 at 41x80), so the
team build cannot make the whole step faster; a measured end-to-end team
number would need the team entries wired into `step2d_LF_AM3.h`, which
has not been done.

## step2d: the Fortran left on the management core

The step2d module was not limited by its kernels.  At 256^2 the isolated
SW64 kernels of one module call add up to 0.93 s over two steps, against a
`Model 2D kernel` of 5.58 s; the Kokkos Serial build gives 14.05 s of
kernels against 18.96 s.  Both builds leave the same ~4.7 s, which is the
Fortran that still ran on the MPE inside `step2d_tile`.  That remainder
scaled with the tile area, so it was not boundary code but three
whole-tile loops:

- the DIAGNOSTICS_UV time integration on every corrector call
  (`DiaU2int`/`DiaU2wrk`, NDM2d-1 = 10 passes each for U and V, five
  fields read and two written per point): 47.8 ms per call at 256^2,
  84 calls per two steps;
- the `M2bstr` zero fill on every call, a dead store because every
  branch of `k_step2d_coupling` assigns those points before they are read;
- the `rzeta(:,:,krhs)` copy on every predictor call.

Changes, all bitwise-neutral at source level:

- `k_step2d_diag_int` (stage flag `KK_STAGE_DIAG`, added to the build
  scripts) runs the integration on the CPEs with the Fortran coefficients.
- `k_step2d_zeta_step` stores `rzeta` in the same launch; the Fortran keeps
  only the halo exchange.  The `M2bstr` fill is compiled out when
  `KK_STAGE_COUP` is on.
- One launch per kernel call instead of 2-3 (depth_flux, time_average,
  zeta_step, rhs_fused, coupling, momentum_step, store_rhs).  A value that
  another row owns (`Drhs(i,j-1)`, `Dstp(i,j-1)`) is recomputed from its
  two operands instead of re-read.  An empty launch costs about 30 us, so
  this mainly helps the 41x80 tile.
- `ROMS_KK_TIMING=1` prints every entry point's in-model wall time.

Correctness: `test_step2d_kokkos` passes on both backends.  The team bench
still holds every range kernel bitwise equal to the unchanged staged team
entries: 12/12 on Serial at 41x80 and 256^2, 11/12 on SW64 with the known
7.1e-15 `momentum_step` loop-shape difference.  The 41x80 two-step run with
output, new SW64 and new Serial each against pure Fortran: his 5/5, avg
5/5, dia 23/23 at `--rtol 1e-10 --zero-tol 1e-20` (worst nonzero field
4.9e-14 normalised, zero fields at most 1.4e-26 absolute).  New and old
SW64 binaries are not bitwise equal: zeta differs by 3.4e-21 (1e-15
relative), and five physically zero diagnostics (ubar_prsgrd, ubar_xadv,
vbar_xadv, ubar_xvisc, vbar_xvisc) carry different 1e-24..1e-30 noise.
Against the Fortran reference, where those fields are exactly zero, both
binaries pass.

`Model 2D kernel`, upwelling, two steps, `-cache_size 128`, seconds.
Rows marked * reuse the earlier sweep's Fortran and old-SW64 runs; the
41x80 and 256^2 controls were re-run alongside the new binary:

| grid | Fortran | SW64 before | SW64 now | before/Fortran | now/Fortran |
|---|---:|---:|---:|---:|---:|
| 41x80 | 0.790 | 0.360 | 0.140 | 2.19 | **5.64** |
| 128^2 * | 4.130 | 1.430 | 0.340 | 2.89 | **12.15** |
| 256^2 | 17.360 | 5.580 | 1.260 | 3.11 | **13.78** |
| 512^2 * | 67.250 | 21.710 | 5.060 | 3.10 | **13.29** |

`ROMS time stepping wall`, same runs:

| grid | Fortran | SW64 before | SW64 now | before/Fortran | now/Fortran |
|---|---:|---:|---:|---:|---:|
| 41x80 | 1.109 | 0.656 | 0.432 | 1.69 | **2.57** |
| 128^2 * | 5.722 | 2.930 | 1.850 | 1.95 | **3.09** |
| 256^2 | 24.106 | 11.932 | 7.593 | 2.02 | **3.17** |
| 512^2 * | 94.657 | 47.680 | 31.048 | 1.99 | **3.05** |

In-model kernel time from `ROMS_KK_TIMING=1`, microseconds per call:

| kernel | calls | 41x80 Serial | 41x80 SW64 | ratio | 256^2 Serial | 256^2 SW64 | ratio |
|---|---:|---:|---:|---:|---:|---:|---:|
| depth_flux | 170 | 87 | 51 | 1.7 | 1447 | 256 | 5.7 |
| time_average | 170 | 53 | 43 | 1.2 | 1144 | 192 | 6.0 |
| zeta_step | 168 | 291 | 69 | 4.2 | 5634 | 503 | 11.2 |
| rhs_fused | 168 | 2034 | 174 | 11.7 | 43377 | 2196 | 19.8 |
| coupling | 168 | 914 | 126 | 7.2 | 12598 | 1384 | 9.1 |
| momentum_step | 168 | 279 | 59 | 4.7 | 5800 | 335 | 17.3 |
| store_rhs | 84 | 280 | 127 | 2.2 | 5156 | 1326 | 3.9 |
| diag_int | 84 | 2461 | 225 | 10.9 | 47803 | 3108 | 15.4 |
| **step2d sum, s** | | 0.845 | 0.117 | 7.2 | 16.214 | 1.191 | 13.6 |
| step3d_t | 2 | 93856 | 26015 | 3.6 | 1898479 | 537591 | 3.5 |

The step2d kernel sum is now 0.117 of the 0.12-0.14 s module at 41x80 and
1.19 of 1.24-1.26 s at 256^2: the Fortran left inside step2d is the
boundary and exchange code, at most a few hundredths of a second.  The
module speedup over Fortran therefore equals the kernel speedup to within
that margin.  Further step2d gains have to come from the kernels
themselves; at 256^2 the four memory-bound ones (diag_int, rhs_fused,
coupling, store_rhs) are 84% of it.  For the whole model the step2d module
is now 1.26 of the 7.59 s step at 256^2, step3d_t 1.12 s, and the
un-migrated 3-D Fortran most of the remaining 5.2 s (next section).

## 3-D helpers: mixing, pressure gradient, omega, set_zeta

The same recipe applied to the Fortran still on the management core:
port the pure loops, keep exchanges and boundaries, then subtract the
in-model kernel time from the ROMS region to see what is left.  Five
kernels in `ROMS/Cpp/kokkos_rhs3d.cpp`, each behind its own stage flag
(`KK_STAGE_UV3DMIX`, `_T3DMIX`, `_PRSGRD`, `_OMEGA`, `_SETZETA`, added to
the build scripts):

- `uv3dmix2_s` and `t3dmix2_s`: one work item per row, flux carried
  along the face direction and recomputed for the neighbouring row.
- `prsgrd32`: the column pressure recursion (dR/dZ in a reusable
  workspace) plus the two gradient passes.
- `omega`: one work item per column.
- `set_zeta`: the reload of zeta(:,:,1:2) from Zt_avg1.

One bug worth recording: the first version made (j,k) the work item for
uv3dmix.  `rufrc`/`rvfrc` are accumulated over k, so parallel k raced on
them and the two-step run missed the reference by 1.6e-4 relative in
zeta, with the viscous diagnostics off by 5x.  The fix is one work item
per row with the level loop inside, which also restores the Fortran
summation order.  After it, the 41x80 two-step run with output passes
his 5/5, avg 5/5, dia 23/23 (`--rtol 1e-10 --zero-tol 1e-20`) on both
SW64 and Serial.

Region times at 256^2, two steps, seconds (same run, before -> after):

| region | Fortran path | now | kernel timer calls / per call |
|---|---:|---:|---:|
| Harmonic stress tensor (uv3dmix) | 0.58 | 0.02 | 2 x 16.0 ms |
| Harmonic mixing of tracers (t3dmix) | 0.26 | 0.03 | 2 x 10.9 ms |
| Pressure gradient (prsgrd) | 0.48 | 0.19 | 2 x 96.6 ms |
| Omega vertical velocity | 0.11 | 0.03 | 6 x 2.0 ms |
| 2D/3D coupling, vertical metrics (set_zeta) | 0.21 | 0.22 | 3 x 0.2 ms |

prsgrd is the weakest of the five (2.5x against its Fortran region, the
others 10-25x): the two face passes carry dZx/dRx along the face and are
not vectorised, and the pressure recursion touches an 8.6 MB tile array
per call.  The last row did not move because set_zeta is only a small
part of that region.  *Correction (2026-11-04):* an earlier version of
this paragraph called the 0.22 s unexplained and claimed EWperiodic and
NSperiodic were false.  Both were wrong.  The upwelling input sets
`LBC == Per Clo Per Clo` (west, south, east, north), so the case is
periodic in xi.  Region 12 is shared by three routines that each time
themselves under it: `set_depth` (z_w, z_r and Hz over the whole grid,
called after the 2-D steps of every time step), `set_massflux` (Huon and
Hvom) and `set_zeta`.  The 0.22 s is the first two, whole-grid Fortran
loops that could be ported the same way.

Whole model, upwelling, two steps, SW64, seconds:

| grid | before | after | delta |
|---|---:|---:|---:|
| 41x80 | 0.432 | 0.368 | -0.064 |
| 128^2 | 1.850 | 1.554 | -0.296 |
| 256^2 | 7.593 | 6.434 | -1.159 |
| 512^2 | 31.048 | 26.580 | -4.468 |

The region deltas sum to exactly the wall delta at 256^2 (-1.16 s), so
the gain is the five kernels and nothing else moved.  What remains at
256^2 is pre_step3d 1.12 s, rhs3d proper 0.73 s, step3d_uv 0.12 s,
EOS 0.08 s, the step2d module 1.26 s, step3d_t 1.13 s, and about 1.6 s
of residual non-profiled code.

## step3d_t: evaluating each xi-face flux once

The per-column kernel gave every column its own two xi-face fluxes, so
every interior face was evaluated twice.  `k_step3d_t_rows` (now the
production entry) gives a work item one (j, tracer) row and walks i with
two rolling face buffers of N doubles, so each face is evaluated once.
The face expression, index and accumulation order are unchanged, so the
result is bitwise equal; the scratch is 2N + 2(N+1) doubles per row
(270 KB at 256^2), against 2(N+1) per column before.

This is not the TeamPolicy experiment: no team, no cross-item sharing,
no large scratch.  The earlier team-rows variant needed a 17 MB buffer at
this size and was 1.6x on the isolated benchmark; the plain row range
gets the same sharing with a 270 KB buffer and no team launch cost.

256^2, two steps, SW64:

| | column kernel | row kernel |
|---|---:|---:|
| step3d_t kernel timer | 1.070 s | **0.490 s** |
| `Corrector time-step for tracers` region | 1.130 s | **0.540 s** |

The kernel is now 2.8x the pure-Fortran region (1.520 s) instead of
1.35x.  Serial passes the same two-step output check (his 5/5, dia
23/23, avg 5/5).

Whole model, upwelling, two steps, SW64, seconds:

| grid | before rows | after rows |
|---|---:|---:|
| 41x80 | 0.368 | 0.346 |
| 128^2 | 1.554 | 1.379 |
| 256^2 | 6.434 | 5.868 |
| 512^2 | 26.580 | 23.774 |

## step2d review: evaluation order, and a validation gap

Every production step2d kernel (depth_flux, time_average, zeta_step,
rhs_fused, coupling, momentum_step, diag_int, store_rhs) was compared
statement by statement with `step2d_LF_AM3.h` for the compiled macro set:
index ranges, the FIRST_2D_STEP / predictor / Adams-Moulton branch
selection, the closed-edge folds of the fourth-order advection, and the
write ownership of every fused launch.  All of that matches.  Two places
evaluated the right formula in a different floating-point order:

- `k_step2d_coupling`, second and later 3-D steps:
  `rhs_ubar += 1.5*rufrc - 0.5*ru` adds the difference, the Fortran
  `rhs_ubar+1.5*rufrc-0.5*ru` adds left to right.  Same for the
  three-term form and for DiaU2rhs/DiaV2rhs.
- `k_step2d_rhs_fused`, viscosity: `on_r*on_r*visc2_r*Drhs*0.5*(...)`
  multiplied left to right, the Fortran forms `cff=visc2_r*Drhs*0.5*(...)`
  first and then `on_r*on_r*cff`.  Same for om_r, om_p and on_p.

Both now follow the Fortran (also in the TeamPolicy copies).  Before the
fix the Kokkos Serial build differed from pure Fortran only in eight
viscous diagnostics (2-D and 3-D hvisc/yvisc) by at most 8e-28 absolute;
the prognostic fields were already equal.  After it, a Serial binary with
only the step2d stages (`KK_STAGES=...`, build/serial-s2d) **and** the full
production Serial binary are value-for-value equal to pure Fortran in all
101 time-dependent fields of the 41x80 two-step run.  Both compilers are
GCC 7.1 (swg++ and swgfortran), so the same expression tree gives the
same rounding, FMA contraction included.  SW64 stays within 5e-14
normalized (worst field ubar_bstr).

The validation gap: every earlier "his 5/5, avg 5/5, dia 23/23" result
in this file used `nccmp.py`'s default list, which holds zeta, ubar,
vbar, u, v and the 2-D momentum diagnostics only.  temp, salt, w, omega,
rho, the 3-D momentum and tracer budget terms and the tracer averages
were never compared, although t3dmix and step3d_t write them.
`cmp_all.sh` now lists all 101 fields; the rows kernel, the 3-D helpers
and the step2d fix were re-checked with it (Serial exact, SW64 within
5e-14).

## pre_step3d and step3d_uv on the CPEs

`KK_STAGE_PRE3D` and `KK_STAGE_STEP3DUV` (kokkos_step3d.cpp) port the
compute loops of `pre_step3d.F` and `step3d_uv.F`; lateral boundary
conditions, point sources and exchanges stay in Fortran.  pre_step3d
falls back to Fortran for the whole tile when a tracer scheme is not
implemented or tracer point sources are on.  Both reject MASKING,
WET_DRY, WEC, BODYFORCE and the other unported options at compile time.
Every expression keeps the Fortran operand order: the Serial build is
still exactly equal to pure Fortran in all 101 fields.

The loop order mattered more than anything else.  The first versions
gave a work item one row and walked it column by column, every level of
every array per column (25 arrays for the momentum predictor).  Replaying
the Fortran loops inside the row instead, i innermost over chunks of 32
columns, changed the per-call cost at 256^2 on SW64 as follows (ms):

| kernel | column at a time | i innermost | Kokkos Serial (i innermost) |
|---|---:|---:|---:|
| pre_step3d tracers | 86.6 | **23.8** | 272 |
| pre_step3d momentum | 93.7 | **19.5** | 171 |
| step3d_uv solve | 245.5 | **80.5** | 367 |
| step3d_uv coupling | 6.8 | 7.0 | 62 |
| step3d_t (rows -> chunks) | 244.8 | **56.4** | 1551 |

The Serial build of the momentum kernels also got faster with the same
change (pre_step3d momentum 221 -> 171 ms), so this is cache behaviour,
not a CPE quirk; step3d_t on Serial is compute-bound (HSIMT limiter) and
stayed at 1.55 s.  `k_step3d_t_chunks` is now the production step3d_t
entry; `k_step3d_t_rows` is kept for the benchmark.  The step3d_t region
went from 0.54 to 0.16 s, 9.5x the pure-Fortran region (1.52 s).

### step3d_uv was never in its timer region

`Corrector time-step for 3D momentum` printed 0.12-0.16 s at 256^2 in
every profile, and porting the module did not move it.  The reason is in
`timers.F`: `wclock_on` stores the start time per region, and
`uv_C2A_grid`, called at the end of `step3d_uv_tile`, opens and closes
region 34 again.  The outer `wclock_off` then measures from the inner
start, so region 34 holds only `uv_C2A_grid` (twice) and the exchanges,
and the whole time step of the momentum equations was part of the
"unprofiled" remainder.  From the stepping wall, the Fortran step3d_uv
loops cost about 1.1 s per two steps at 256^2, not 0.12 s.

## Whole model after this round

Upwelling, two steps, no output, `ROMS time stepping wall`, SW64 one core
group, seconds:

| grid | Fortran | start of round | + pre_step3d | + step3d_uv | i innermost | now vs Fortran |
|---|---:|---:|---:|---:|---:|---:|
| 41x80 | 1.109 | 0.346 | 0.298 | 0.259 | **0.231** | 4.80x |
| 128^2 | 5.722 | 1.379 | 1.148 | 0.895 | **0.837** | 6.84x |
| 256^2 | 24.106 | 5.868 | 5.112 | 4.364 | **3.524** | 6.84x |
| 512^2 | 94.657 | 23.774 | 21.044 | 17.839 | **14.114** | 6.71x |

"+ step3d_uv" also contains the row-order pre_step3d momentum kernel;
"i innermost" is the chunked tracer, step3d_uv and step3d_t kernels.  The
128^2 value is the median of three runs (0.882, 0.837, 0.835 s); the
other sizes are single runs.  Correctness for the final binaries: the
41x80 two-step run with output, all 101 fields, Serial equal to pure
Fortran value for value, SW64 within 4.95e-14 normalized (ubar_bstr);
`test_step2d_kokkos` 131/131 on both backends.

At 256^2 the 3.52 s now splits as: Model 2D kernel 1.24, rhs3d proper
0.73, set_depth/set_massflux (region 12) 0.23, prsgrd 0.20, global
integrals (`diag`, every step here because NINFO=1) 0.20, step3d_uv
kernels 0.18 plus 0.14 of uv_C2A_grid and exchanges, step3d_t 0.16,
pre_step3d 0.09, EOS 0.07, mixing and omega 0.07, other ~0.2.

## What is left to migrate, by measured size

Upwelling 256^2, two steps, after this round (3.52 s stepping):

1. step2d kernels, 1.24 s.  Already 13.8x the Fortran module; the four
   memory-bound kernels (diag_int, rhs_fused, coupling, store_rhs) are
   most of it.
2. rhs3d proper (`rhs3d_tile`: 3-D advection of momentum, Coriolis,
   vertical advection), 0.73 s, still Fortran.  The largest single item.
3. set_depth and set_massflux (region 12), 0.23 s, whole-grid Fortran.
4. prsgrd, 0.20 s.  Its eta pass gives a work item one (i, k) column of
   faces and walks j, the strided order that cost 3-5x elsewhere in this
   round; a row-order version is the obvious next try.
5. `diag` global integrals, 0.20 s with NINFO=1; production runs print
   less often.
6. uv_C2A_grid inside step3d_uv, EOS, mixing, omega: under 0.3 s
   together.

Done in earlier rounds or this one: step2d, step3d_t, pre_step3d,
step3d_uv, uv3dmix, t3dmix, prsgrd (column version), omega, set_zeta.

## Build deviation for the canyon case

`canyon.h` needs `SOLVE3D` from outside, and the existing step2d kernel
requires `VAR_RHO_2D` and `DIAGNOSTICS_UV`. Those three macros were added
for the canyon build only (`build_canyon.sh`); upwelling is untouched.
