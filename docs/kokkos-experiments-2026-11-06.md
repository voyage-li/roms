# Kokkos/SW64 port: scaling, integrations and closure experiments

Date: 2026-11-06
Build: `854c41f1` (branch `feat/kokkos`). The scaling runs predate the
carried-stencil `rhs_fused` change and use the `-clean` builds; every later
run uses `-kcarry`. The two builds give bitwise identical output over 200
steps and their 12-rank two-step wall differs by under 1% (six alternating
samples each; medians 3.66 s and 3.64 s), so all results are reported
together.

This note collects the experiments run after the clean-up: strong and weak
scaling, the contribution of each stage group, where the first difference
between the port and the original appears, domain decomposition, restart,
a long integration at 160x80 and at the production grid, and the GLS, KPP
and KELVIN cases.
All runs use the SUNWAY system, queue `q_share`, one tile per rank,
`ROMS_WALL_PROFILE` builds and the `run_app.sh` / `make_app_case.py` tools.
Timings are the rank-0 stepping wall of a two-step, no-output run, divided
by two, except where a section says otherwise.

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
| 24 | 8.276 | 0.890 | 9.30x |

From 2 to 12 ranks (six times the ranks) the original build speeds up 5.66x
and the port 4.60x. At 24 ranks (12x2 tiles, median of three samples each:
original 16.545-16.556 s, port 1.778-1.785 s per two steps) the step time
halves again for both builds, 2.05x and 2.06x against 12 ranks, and the
speedup stays at 9.3x: on the production grid the extra two nodes pay off.

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
| 1440x704 | original Fortran | 0.920 | 190.3 | 0.67 s (0.7%) |
| 1440x704 | Kokkos/SW64 | 0.313 | 16.2 | 0.17 s (2.0%) |

`f` covers everything that does not shrink with the rank count: the serial
remainder, the halo exchange and the I/O. It is *larger in absolute terms*
for the original build at both grids. What drops the port's efficiency is
its share of the step: at 12 ranks `f` is 5% of the original's step on the
production grid but 17% of the port's, which is ten times shorter; at 24
ranks it is 11% against 35%, and on the 720x352 grid 18% against 37%. That
is why the speedup falls from 11.4x at two ranks to 9.3x at 24 while the
original still holds 89% efficiency on the small grid at four nodes.

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

UPWELLING, 160x80, one rank, 200 steps, output every 100 steps (three
history records, two average and two diagnostic records).

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

### Ten times the length

The same configuration ran for 2000 steps (6.9 days of model time), one
rank per build, output every 500 steps. The original takes 4837.8 s
(2.42 s/step), the Serial build 5116.6 s and the SW64 build 593.7 s, 8.15x
faster than the original. No run blows up.

After 2000 steps the port still passes the 1e-10 normalized gate on 100 of
101 fields; the exception is again `salt_vdiff`, 2.4e-19 against a
reference field of about 2e-19. The largest absolute differences are the
salt transports, 6.6e-9 (`Huon_salt`) and 5.1e-9 (`Hvom_salt`), the same
size as after 200 steps (5.5e-9). The state fields grow, but far more
slowly than the run length: over ten times the integration `temp` goes from
5.1e-13 to 3.6e-12, `u` from 1.5e-14 to 2.0e-13, `v` from 1.0e-13 to
2.0e-13, `zeta` from 4.8e-15 to 9.2e-15, a factor of two to fourteen.

Serial and SW64 remain bitwise identical over all 2000 steps (101/101
fields, zero difference).

## Where the first difference comes from

The stage-ablation builds were run for the same 20 steps as the onset
experiment (160x80, one rank, history every step) and compared with the
original Fortran run. These comparisons cover the ten history fields.

| build | stages beyond the 2-D step | first differing record | largest difference |
|---|---|---|---:|
| S1 | none | none (10/10 bitwise) | 0 |
| S2 | 3-D momentum group | step 16, `zeta` 7.6e-19 | 2.1e-14 (`salt`, step 20) |
| S3 | 3-D momentum and PRE3D | step 16, `zeta` 7.6e-19 | 2.1e-14 (`salt`, step 20) |

S1 reproduces the original bitwise for all 20 steps. S2 and S3 produce,
step by step, exactly the same differences as the production build, so
PRE3D and the row ports add nothing: the first difference enters with the
3-D momentum group (UV3DMIX, T3DMIX, PRSGRD, OMEGA, SETZETA, STEP3DUV).
The 2-D kernels, the tracer chunk kernel that every build compiles and the
remaining Fortran paths agree bitwise. (All three builds also compile the
GLS stages, which this application does not enable.)

Where the first difference appears depends on the configuration, not on a
single kernel: the KELVIN case below stays bitwise exact for 20 steps with
the same source.

The group was then split. The six stages were added one at a time, in the
order UV3DMIX, T3DMIX, PRSGRD, OMEGA, SETZETA, STEP3DUV, and each build was
run for the same 20 steps:

| stages added on top of S1 | result against the original |
|---|---|
| UV3DMIX | bitwise identical |
| + T3DMIX | bitwise identical |
| + PRSGRD | differs from step 16, same values as the production build |
| + OMEGA | bitwise identical to the PRSGRD build |
| + SETZETA | bitwise identical to the PRSGRD build |
| + STEP3DUV (= S2) | bitwise identical to the PRSGRD build |

The first Fortran/port rounding difference seen over these 20 steps
therefore comes from the pressure-gradient kernel (`KK_STAGE_PRSGRD`). The
three stages added after it change no output value, and every build from
PRSGRD onward reproduces the production differences.

## Domain decomposition

UPWELLING, 240x176, four steps, output at the first and last step. Five
decompositions: one rank with one tile, and six ranks split 6x1, 3x2, 2x3
and 1x6. The 1x6 split divides Mm=176 by six, so the partition is uneven.

- Kokkos/SW64 against the original Fortran at the same decomposition:
  **101/101 fields bitwise identical in all five cases** (28 average, 63
  diagnostic, 10 history).
- Within each build, the four six-rank decompositions and the one-rank run
  are **bitwise identical to the 6x1 run**.

At this configuration the solution does not depend on the decomposition,
and the port reproduces that invariance exactly.

The same series repeated with 20 steps gives the same picture: all five
decompositions remain **bitwise identical between the builds**, and all
five remain **bitwise identical to each other** within each build (101
fields each). On this grid the port and the original also still agree
exactly at 20 steps, unlike the 160x80 onset run, which is another sign
that where the first difference appears depends on the configuration.

## Restart

UPWELLING, 160x80, one rank: a continuous ten-step run against a five-step
run followed by a five-step continuation read from its restart file
(NRST=5; NRREC=1; ININAME pointing at `roms_rst.nc`).

- The restart file written at step 5 is **bitwise identical** between the
  original and the port (8/8 variables; restart files are not part of the
  usual application comparison, so this one was compared directly).
- The continuation's final record (t=50 min) is **bitwise identical**
  between the two builds (11/11 history fields).
- Neither build restarts bitwise continuously: the final record differs
  from the continuous run by up to 6.6e-7 (`temp` 6.6e-7, `v` 6.3e-7,
  `rho` and `u` 1.1e-7), with the same numbers in both builds. That is a
  property of the ROMS restart path at this configuration, not of the
  port.

## GLS closure

UPWELLING with `GLS_MIXING` from `MY_CPP_FLAGS` (the header then selects
`KANTHA_CLAYSON`, `N2S2_HORAVG`, `RI_SPLINES` and the standard k-epsilon
parameters of `ROMS/External/roms_upwelling.in`), two ranks, 41x80. The
runs below are four steps with output every step, twenty steps with output
every ten, and fifty steps with output every twenty-five.

- Serial against original Fortran: **101/101 fields exact** (maximum
  difference 0).
- SW64 against original Fortran: **101/101 fields exact** (maximum
  difference 0), which also satisfies the 1e-10 gate.
- SW64 against original Fortran over 20 steps: **101/101 bitwise
  identical**.
- Over 50 steps 72 of the 101 fields differ, with a largest absolute
  difference of 9.1e-13 (`Hvom_salt`). One field fails the normalized
  gate, `salt_vdiff`: its difference is 3.2e-20 against a reference field
  whose largest magnitude is 9e-18.
- Against a matched Serial control (the same build and physics without
  `KK_STAGE_GLS`): **101/101 fields bitwise identical over the same 50
  steps**, so the closure kernel itself adds no difference; the drift
  against the original comes from the kernels both builds share.
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
LMD_NONLOCAL, LMD_DDMIX and RI_SPLINES), two ranks, 39x4x30. `KK_STAGE_LMD`
is added and `KK_STAGE_PRE3D` left out, as in `KPP.md`. The runs below are
four steps with output every step, twenty steps with output every ten, and
fifty steps with output every twenty-five.

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
- Over 20 steps the gate fails in three fields, `AKv`, `AKt` and `AKs`
  (5.4e-12 absolute, 6.0e-10 relative); every other field passes.
- Over 50 steps seven comparisons fail: the three coefficients in the
  history file (9.99e-12 absolute, 7.97e-10 relative), and in the station
  file `omega` (5.9e-10 absolute, 1.09e-10 relative) plus the same three
  coefficients (1.8e-12 absolute, 1.5e-10 relative). The absolute drift
  stays between 1e-12 and 1e-10; only the normalized gate on these
  coefficients is exceeded.
- Against the matched Serial control over the same 50 steps: **99/99
  fields bitwise identical**, so the LMD kernel adds nothing of its own;
  the drift against the original comes from the shared 3-D kernels.

## KELVIN

The KELVIN test case (the official header and input, 50x30, two ranks)
brings its own header, analytic forcing and geometry.

- Four steps: **62/62 fields bitwise identical** for both the Serial and
  the SW64 build (8 history, 54 diagnostic).
- Twenty steps: again **62/62 bitwise identical** for both.

This is an independent confirmation of the carried-stencil 2-D kernel: a
different application, grid and physics set, over twenty steps, with no
difference from the original at all.

## Production-grid integration

UPWELLING at 1440x704 with 16 levels, 12 ranks on two nodes, 200 steps
(16.7 h of model time), output at the first and last step, 9.9 GB per run.
The step times include that output, so they are not comparable with the
no-output measurements above: 18.77 s/step for the original (3754.4 s in
total) and 2.99 s/step for the port (598.8 s), a speedup of 6.27x.

Of the 101 compared fields, 100 are within the 1e-10 normalized gate.

| file | result | largest difference |
|---|---|---|
| average (one 200-step record) | 28/28 pass | 3.8e-9 (`Hvom_salt`) |
| diagnostics (one record) | 62/63 pass | failure `salt_vdiff`, 2.3e-19 |
| history (first and last record) | 10/10 pass | 1.2e-13 (`v`) |

The first history record is the unchanged initial state and is bitwise
identical; the figures above are from the last record, after 200 steps.
`salt_vdiff` fails only because its reference magnitude is 2.7e-19; the
absolute difference is 2.3e-19, the same kind of near-zero field as in the
160x80 run.

## Method notes

- Builds: `build_app.sh UPWELLING` with `MPI=ON`; S1-S3 override
  `KK_STAGES`, and the incremental series adds one 3-D momentum stage at a
  time to the S1 list; GLS adds `GLS_MIXING` as the extra flag; KPP builds
  BL_TEST with `UV_VIS2;LMD_MIXING;PERFECT_RESTART`, the official `bl_test`
  header and `KK_STAGE_LMD` instead of `KK_STAGE_PRE3D`, plus the matched
  control without `KK_STAGE_LMD`; KELVIN is built with `MPI=ON
  TAG_SUFFIX=-exp` and the official `kelvin` header directory.
- The runs made after the clean-up (decomposition, restart, KELVIN,
  production) use the `-kcarry` builds, i.e. the current source; the
  original-Fortran builds are the ones of the clean-up validation, whose
  Fortran sources have not changed.
- Compiler flags are the same as in the clean-up validation: Fortran
  `-mieee -frepack-arrays -O3` in all three builds; the SW64 C++ build adds
  `-fno-tree-vectorize`, the Serial C++ build does not. Serial and SW64
  still agree bitwise over the 200-step run, so the flag difference does
  not change the results.
- Inputs: `make_app_case.py` with `--size`, `--steps`, `--tiles` and, for
  the long run, `--output-interval 100`. `NtileI*NtileJ` equals the rank
  count in every run.
- Jobs: `run_app.sh` with `RANKS` and `RANKS_PER_NODE=6`; 24 ranks span four
  nodes. The two production runs were submitted directly with `bsub` (same
  rank layout, `-timelimit 02:00:00`) and polled for `ROMS: DONE`, because
  the interactive form does not suit an hour-long job. All jobs are in
  `q_share`; no other account's job was touched.
- The GLS and KPP controls are built by `build_app.sh` with the same
  physics and a `KK_STAGES` list without `KK_STAGE_GLS` / `KK_STAGE_LMD`
  respectively, so a candidate and its control differ only by that one
  offload.
- Logs and runs: `~/voyage/roms/work/cleanup-20261105T1334/{logs,new/runs}`.
