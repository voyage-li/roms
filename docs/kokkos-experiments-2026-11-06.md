# Kokkos/SW64 port: scaling, stage ablation and physics experiments

Date: 2026-11-06
Build: `854c41f1` (branch `feat/kokkos`), the same source as the published
12-rank campaign with the carried-stencil `rhs_fused` change.

This note collects the experiments run after the clean-up: strong and weak
scaling, the contribution of each stage group, a long integration checked
against the original Fortran, the GLS and KPP closures, and the checks that
the committed kernel reproduces the clean build.
All runs use the SUNWAY system, queue `q_share`, one tile per rank,
`ROMS_WALL_PROFILE` builds and the `run_app.sh` / `make_app_case.py` tools.
Timings are the rank-0 stepping wall of a two-step, no-output run, divided
by two.

## Strong scaling

UPWELLING, 1440x704, two steps. The one-rank point does not fit in the
15 GB per-rank memory limit (one tile of that grid needs about 19 GB), so
the table starts at two ranks. `speedup` is the original Fortran build
divided by the Kokkos/SW64 build at the same rank count.

| ranks | Fortran s/step | Kokkos s/step | speedup |
|---:|---:|---:|---:|
| 2 | 95.911 | 8.420 | 11.39x |
| 4 | 48.353 | 4.368 | 11.07x |
| 6 | 33.299 | 2.934 | 11.35x |
| 12 | 16.946 | 1.832 | 9.25x |

From 2 to 12 ranks (six times the ranks) the original build speeds up 5.66x
and the port 4.60x.

UPWELLING, 720x352, two steps, including the one-rank point:

| ranks | Fortran s/step | Kokkos s/step | speedup | efficiency (Fortran / Kokkos) |
|---:|---:|---:|---:|---:|
| 1 | 47.966 | 4.269 | 11.24x | 1.00 / 1.00 |
| 2 | 23.991 | 2.172 | 11.05x | 1.00 / 0.98 |
| 4 | 12.521 | 1.140 | 10.98x | 0.96 / 0.94 |
| 6 | 8.524 | 0.784 | 10.87x | 0.94 / 0.91 |
| 12 | 4.267 | 0.467 | 9.14x | 0.94 / 0.76 |
| 24 | 2.244 | 0.268 | 8.37x | 0.89 / 0.66 |

The 6- and 12-rank entries are medians of three samples (original 8.522-8.527
and 4.259-4.299 s/step, port 0.782-0.784 and 0.466-0.476); the other rows are
single samples. Efficiency is the one-rank time divided by `ranks` times the
measured time.

Both curves follow `T(N) = f + p/N`, the fixed cost plus the work that is
divided among the ranks. A least-squares fit gives, in s/step:

| grid | build | f | p | largest residual |
|---|---|---:|---:|---:|
| 720x352 | original Fortran | 0.410 | 47.5 | 0.23 s (0.5%) |
| 720x352 | Kokkos/SW64 | 0.100 | 4.16 | 0.02 s (0.5%) |
| 1440x704 | original Fortran | 1.368 | 189.0 | 0.43 s (0.4%) |
| 1440x704 | Kokkos/SW64 | 0.390 | 16.0 | 0.12 s (1.4%) |

`f` covers everything that does not shrink with the rank count: the serial
remainder, the halo exchange and the I/O. It is *larger in absolute terms*
for the original build (1.37 s against 0.39 s at 1440x704). What drops the
port's efficiency is its share of the step: at 12 ranks `f` is 8% of the
original's step but 21% of the port's, which is ten times shorter; at 24
ranks (720x352 only) it is 18% against 37%. That is why the speedup falls
from 11.2x at one rank to 8.4x at 24 while the original still holds 89%
efficiency at four nodes.

## Weak scaling

UPWELLING, one 360x176 tile per rank, two steps. The grid grows with the
rank count so every rank does the same work.

| ranks | grid | Fortran s/step | Kokkos s/step | speedup |
|---:|---|---:|---:|---:|
| 1 | 360x176 | 11.921 | 1.104 | 10.80x |
| 2 | 720x176 | 12.255 | 1.110 | 11.04x |
| 4 | 720x352 | 12.359 | 1.140 | 10.84x |
| 6 | 1080x352 | 12.617 | 1.147 | 11.00x |
| 12 | 2160x352 | 12.752 | 1.151 | 11.08x |

Both builds keep their per-step time within 7% from one to twelve ranks
(original 11.92 to 12.75 s, port 1.104 to 1.151 s), so weak scaling is close
to ideal across two nodes, and the speedup stays at 11.0-11.1x: the
per-tile speedup and the whole-machine speedup agree when the work per rank
is held fixed.

## Stage ablation

UPWELLING, 1440x704, 12 ranks, two steps. Each build adds one stage group to
the previous one, so the table separates the three parts of the port.

| build | stages enabled | s/step | speedup |
|---|---|---:|---:|
| original Fortran | none | 16.946 | 1.00x |
| S1 | 2-D step (FUSED, TAVG, COUP, MOMT, DIAG, STOR) | 5.274 | 3.21x |
| S2 | S1 + 3-D momentum (UV3DMIX, T3DMIX, PRSGRD, OMEGA, SETZETA, STEP3DUV) | 3.614 | 4.69x |
| S3 | S2 + PRE3D | 2.575 | 6.58x |
| S4 | S3 + REMAINDER (the production set) | 1.832 | 9.25x |

The 2-D step alone is 3.2x; the 3-D momentum kernels take it to 4.7x; the
predictor (PRE3D) to 6.6x; the row ports (REMAINDER) to 9.25x.

The timings above come from no-output runs. To check the fields, S1, S2 and
S3 were rebuilt against the same input as the validation run (UPWELLING,
41x80, two ranks, four steps, all output) and compared with the original
Fortran run of the clean-up validation: **each build matches it bitwise,
101/101 fields, maximum absolute difference 0**. A stage that is not
compiled runs its Fortran path, and these builds show that the remaining
paths reproduce the original exactly.

## Long integration

UPWELLING, 160x80, one rank, 200 steps, output every 100 steps (two records
of history, average and diagnostics).

Both builds are run-to-run bitwise reproducible: two independent 200-step
runs of the original and two of the port produced identical files, so the
Fortran/port difference below is deterministic arithmetic, not scheduling
noise.

At the same configuration with output every step, the two builds agree
exactly through step 15. The first non-zero difference is in the record at
t=4800 s (step 16): 7.6e-19 in `zeta`, at the level of the last bit of the
field. It grows slowly - 5.9e-17 in `zeta` by step 20, 2.6e-16 in the
100-step average, and 5.5e-9 in the salt transport `Hvom_salt` after 200
steps (the largest absolute difference of all fields; `temp` 5.1e-13).

Of the 101 compared fields at 200 steps, 100 pass the 1e-10 normalized gate.
The exception is `salt_vdiff`: its difference is 4.3e-19, but the reference
field itself has a largest magnitude of about 4e-19, so the normalized test
has no scale to work with. By absolute difference every field is at or below
5.5e-9.

Serial and SW64 agree bitwise with each other over all 200 steps (0 in every
field), and the carried-stencil kernel of `854c41f1` reproduces the clean
build bitwise over 200 steps on both backends (101/101 fields, 0
difference). The kernel change therefore does not move the model any closer
to or further from the original.

## GLS closure

UPWELLING with `GLS_MIXING` from `MY_CPP_FLAGS` (the header then selects
`KANTHA_CLAYSON`, `N2S2_HORAVG`, `RI_SPLINES` and the standard k-epsilon
parameters of `ROMS/External/roms_upwelling.in`), two ranks, four steps,
41x80, all output.

- Serial against original Fortran: **101/101 fields exact** (maximum
  difference 0).
- SW64 against original Fortran: **101/101 fields exact** (maximum
  difference 0), which also satisfies the 1e-10 gate.
- The GLS entry clocks are present and equal across ranks and builds
  (`gls_prestep` 304 calls, `gls_corstep` 740 calls), no row fallback line
  appears, and a timed run of each build is bitwise identical to its
  untimed counterpart.

The comparison covers the standard output fields. `tke` and `gls` are not
written with the stock input; the direct comparison of the prognostic
fields is the one recorded in `GLS.md`, which this run repeats at the timer
level with the current source.

## KPP closure

BL_TEST with `LMD_MIXING;PERFECT_RESTART` and the official `bl_test`
header (which selects LMD_RIMIX, LMD_CONVEC, LMD_SKPP, LMD_BKPP,
LMD_NONLOCAL, LMD_DDMIX and RI_SPLINES), two ranks, four steps, 39x4x30,
all output. `KK_STAGE_LMD` is added and `KK_STAGE_PRE3D` left out, as in
`KPP.md`.

- SW64 against the original Fortran: **99/99 fields** (57 diagnostic, 24
  history, 18 station) within the original normalized gate (1e-10 for
  nonzero reference fields, 1e-20 for zero-reference fields).
- Serial against the original Fortran differs in 15 of the 99 fields, with
  the same values as the SW64 difference (largest `u` 6.1e-18, in the last
  record). Both C++ builds therefore differ from the original in the same
  places, so this is not produced by the SW64 vector code. Against the
  exact default comparison that is 84/99; against the 1e-10 gate it is
  99/99.
- Serial candidate against the matched Serial control (same build without
  `KK_STAGE_LMD`): **99/99 fields bitwise identical**, which is the
  criterion `KPP.md` sets.

## Method notes

- Builds: `build_app.sh UPWELLING` with `MPI=ON`; S1-S3 override
  `KK_STAGES`; GLS adds `GLS_MIXING` as the extra flag; KPP builds BL_TEST
  with `UV_VIS2;LMD_MIXING;PERFECT_RESTART`, the official `bl_test` header
  and `KK_STAGE_LMD` instead of `KK_STAGE_PRE3D`, plus the matched control
  without `KK_STAGE_LMD`.
- Compiler flags are the same as in the clean-up validation: Fortran
  `-mieee -frepack-arrays -O3` in all three builds; the SW64 C++ build adds
  `-fno-tree-vectorize`, the Serial C++ build does not. Serial and SW64
  still agree bitwise over the 200-step run, so the flag difference does
  not change the results.
- Inputs: `make_app_case.py` with `--size`, `--steps`, `--tiles` and, for
  the long run, `--output-interval 100`. `NtileI*NtileJ` equals the rank
  count in every run.
- Jobs: `run_app.sh` with `RANKS` and `RANKS_PER_NODE=6`; 24 ranks span four
  nodes. All jobs are in `q_share`; no other account's job was touched.
- Logs and runs: `~/voyage/roms/work/cleanup-20261105T1334/{logs,new/runs}`.
