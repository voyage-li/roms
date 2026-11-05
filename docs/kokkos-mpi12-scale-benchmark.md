# Kokkos SW64 12-rank MPI scaling benchmark

Date: 2026-11-05
Source commit: `58191be0` (`feat: port iteration-time GLS rows and validate MPI`)

This note records the iteration-time speedup of the Kokkos/SW64 port against
the original Fortran build on the Sunway system. It is a performance result,
not a new field-validation result.

## Configuration

- 12 MPI ranks on two nodes, 6 ranks per node (`bsub -n 12 -np 6 -cgsp 64`).
- Domain decomposition `NtileI=6`, `NtileJ=2` (two tiles per rank).
- Two computed steps per run; initialization, build time and queue time are
  excluded from the per-step metric.
- All history, average, diagnostic, restart and station output disabled.
- Three independent repetitions per reference and candidate point; the table
  reports the median.
- Per-step time = `ROMS time stepping wall [rank=0] / 2`.
- Speedup = median reference per-step wall / median SW64 per-step wall.

Build policy:

- Reference: `ROMS_KOKKOS=OFF`, pure Fortran, `ROMS_FAST_MATH=OFF`.
- Candidate: `ROMS_KOKKOS=ON`, `ROMS_KOKKOS_BACKEND=sw64`,
  `ROMS_FAST_MATH=OFF`; SW64 C++ computation units use
  `-fno-tree-vectorize` under the strict-arithmetic policy.
- Common CPP options: `OUT_DOUBLE`, `VAR_RHO_2D`, `DIAGNOSTICS_UV`,
  `DIAGNOSTICS_TS`, plus each application's own options.
- Candidate stages in these binaries:
  `KK_STAGE_FUSED;KK_STAGE_TAVG;KK_STAGE_COUP;KK_STAGE_MOMT;`
  `KK_STAGE_DIAG;KK_STAGE_STOR;KK_STAGE_UV3DMIX;KK_STAGE_T3DMIX;`
  `KK_STAGE_PRSGRD;KK_STAGE_OMEGA;KK_STAGE_SETZETA;`
  `KK_STAGE_STEP3DUV;KK_STAGE_GLS`.
  The coarse `KK_STAGE_PRE3D` path was **not** enabled in this matrix; see
  "Remaining Fortran time" below.

All runs were checked for:

- exactly 12 rank-specific stepping walls and agreement between ranks;
- `ROMS: DONE` and no `ERROR STOP`, abnormal termination or blow-up;
- no whole-tile Fortran fallback in an accepted run;
- candidate execution space `SW64` and a kernel timing table on every rank;
- no NetCDF output written;
- executable SHA256 unchanged before and after the run.

## Applications and grids

Five applications with five grid sizes each; every largest grid takes at
least 60 s per step in the pure-Fortran reference.

| application | grids `(Lm, Mm)` | DT | largest reference (s/step) | calibration job |
|---|---|---:|---:|---|
| UPWELLING | 360x176, 720x352, 1440x704, 2160x1056, 2880x1408 | 300 (input) | 67.251 | 8949042 |
| KELVIN | 256x128, 512x256, 1024x512, 1536x768, 2048x1024 | 900 (input) | 64.676 | 8949221 |
| CANYON | 360x272, 720x544, 1440x1088, 2160x1632, 2880x2176 | 2.5 | 78.537 | 8949078 |
| BL_TEST | 640x80, 1280x160, 2560x320, 3840x480, 5120x640 | 150 (input) | 75.321 | 8949041 |
| CHANNEL | 504x248, 1008x496, 2016x992, 3024x1488, 4032x1984 | 20 | 75.578 | 8949237 |

`CANYON`, `BL_TEST` and `CHANNEL` non-default `DT` values were selected from
separate one-step calibration runs because the stock values were unstable at
the large target grids.

## Results

Median per-step wall time over three samples.

| application | grid | reference s/step | SW64 s/step | speedup |
|---|---:|---:|---:|---:|
| UPWELLING | 360x176 | 1.074154 | 0.269049 | 3.99x |
| UPWELLING | 720x352 | 4.269514 | 0.886250 | 4.82x |
| UPWELLING | 1440x704 | 16.938750 | 3.791162 | 4.47x |
| UPWELLING | 2160x1056 | 36.744363 | 9.517893 | 3.86x |
| UPWELLING | 2880x1408 | 65.935753 | 12.893900 | 5.11x |
| KELVIN | 256x128 | 3.529580 | 0.360581 | 9.79x |
| KELVIN | 512x256 | 8.161563 | 0.821904 | 9.93x |
| KELVIN | 1024x512 | 23.906725 | 2.454217 | 9.74x |
| KELVIN | 1536x768 | 45.314329 | 4.743923 | 9.55x |
| KELVIN | 2048x1024 | 72.725680 | 7.941550 | 9.16x |
| CANYON | 360x272 | 1.149800 | 0.379647 | 3.03x |
| CANYON | 720x544 | 4.663303 | 1.398703 | 3.33x |
| CANYON | 1440x1088 | 18.463120 | 5.331920 | 3.46x |
| CANYON | 2160x1632 | 42.057119 | 12.249624 | 3.43x |
| CANYON | 2880x2176 | 77.321757 | 22.332325 | 3.46x |
| BL_TEST | 640x80 | 1.056468 | 0.397285 | 2.66x |
| BL_TEST | 1280x160 | 4.430218 | 1.435217 | 3.09x |
| BL_TEST | 2560x320 | 18.385817 | 5.759366 | 3.19x |
| BL_TEST | 3840x480 | 40.855198 | 13.035132 | 3.13x |
| BL_TEST | 5120x640 | 72.946904 | 23.145374 | 3.15x |
| CHANNEL | 504x248 | 1.280407 | 0.339523 | 3.77x |
| CHANNEL | 1008x496 | 4.572475 | 1.003767 | 4.56x |
| CHANNEL | 2016x992 | 18.585794 | 4.165960 | 4.46x |
| CHANNEL | 3024x1488 | 41.480915 | 8.944613 | 4.64x |
| CHANNEL | 4032x1984 | 74.858593 | 15.768035 | 4.75x |

Geometric means:

| application | geomean speedup |
|---|---:|
| UPWELLING | 4.42x |
| KELVIN | 9.63x |
| CANYON | 3.34x |
| BL_TEST | 3.04x |
| CHANNEL | 4.42x |
| **all 25 points** | **4.53x** |

Sample stability at the largest grids (per-step seconds, three repetitions):

| application | reference samples | SW64 samples |
|---|---|---|
| UPWELLING | 65.993, 65.935, 65.936 | 12.896, 12.891, 12.894 |
| KELVIN | 72.686, 72.726, 72.758 | 7.929, 7.964, 7.942 |
| CANYON | 77.268, 77.343, 77.322 | 22.333, 22.325, 22.332 |
| BL_TEST | 72.942, 72.947, 72.974 | 23.145, 23.148, 23.126 |
| CHANNEL | 74.833, 74.876, 74.859 | 15.776, 15.768, 15.762 |

The repetition spread is below 0.1% at the largest grids, so the medians are
representative of run-to-run noise on this machine.

## Stage 2: PRE3D and REMAINDER

The r8 binaries above omitted `KK_STAGE_PRE3D` and the
`KK_STAGE_REMAINDER` row ports. A second 225-job campaign ran three builds
interleaved at the same 12-rank configuration: the original Fortran
reference, `GENERAL+PRE3D`, and `GENERAL+PRE3D+REMAINDER`. Each point uses
three samples and the median. BL_TEST keeps PRE3D off because the old
predictor kernel cannot handle `SOLAR_SOURCE`; its PRE3D column is the
unchanged candidate rebuild, not a PRE3D gain.

Validation, two ranks and four steps with all history/average/diagnostic
output on UPWELLING:

- PRE3D Serial vs original Fortran: **101/101 fields exact**;
  PRE3D SW64 vs Fortran: **101/101 within 1e-10**;
  `pre_step3d` tracers and momentum executed four times on both ranks.
- PRE3D+REMAINDER Serial vs matched PRE3D Serial control: **101/101 exact**;
  Serial vs Fortran: **101/101 exact**;
  SW64 vs Fortran: **101/101 strict 1e-10**, no diagnostic exception.

| application | grid | reference s/step | PRE3D s/step | +REMAINDER s/step | PRE3D speedup | +REMAINDER speedup | REMAINDER vs PRE3D |
|---|---:|---:|---:|---:|---:|---:|---:|
| UPWELLING | 360x176 | 1.066405 | 0.222023 | 0.188793 | 4.80x | 5.65x | 1.18x |
| UPWELLING | 720x352 | 4.247945 | 0.691898 | 0.479258 | 6.14x | 8.86x | 1.44x |
| UPWELLING | 1440x704 | 16.941456 | 2.968821 | 1.965709 | 5.71x | 8.62x | 1.51x |
| UPWELLING | 2160x1056 | 36.625093 | 7.733304 | 5.746802 | 4.74x | 6.37x | 1.35x |
| UPWELLING | 2880x1408 | 65.982104 | 9.540085 | 5.871958 | 6.92x | 11.24x | 1.62x |
| KELVIN | 256x128 | 3.530737 | 0.336438 | 0.170649 | 10.49x | 20.69x | 1.97x |
| KELVIN | 512x256 | 8.153453 | 0.759028 | 0.390659 | 10.74x | 20.87x | 1.94x |
| KELVIN | 1024x512 | 23.919180 | 2.210228 | 1.184540 | 10.82x | 20.19x | 1.87x |
| KELVIN | 1536x768 | 45.286621 | 4.238522 | 2.377788 | 10.68x | 19.05x | 1.78x |
| KELVIN | 2048x1024 | 72.675633 | 7.054865 | 4.132861 | 10.30x | 17.58x | 1.71x |
| CANYON | 360x272 | 1.152402 | 0.323072 | 0.189015 | 3.57x | 6.10x | 1.71x |
| CANYON | 720x544 | 4.666340 | 1.165096 | 0.500650 | 4.01x | 9.32x | 2.33x |
| CANYON | 1440x1088 | 18.468176 | 4.379582 | 1.756216 | 4.22x | 10.52x | 2.49x |
| CANYON | 2160x1632 | 42.117255 | 9.997312 | 3.893084 | 4.21x | 10.82x | 2.57x |
| CANYON | 2880x2176 | 77.263289 | 17.878456 | 6.804436 | 4.32x | 11.35x | 2.63x |
| BL_TEST | 640x80 | 1.050588 | 0.391093 | 0.221677 | 2.69x | 4.74x | 1.76x |
| BL_TEST | 1280x160 | 4.421659 | 1.428039 | 0.560706 | 3.10x | 7.89x | 2.55x |
| BL_TEST | 2560x320 | 18.398036 | 5.760751 | 1.825376 | 3.19x | 10.08x | 3.16x |
| BL_TEST | 3840x480 | 40.878538 | 13.067361 | 3.923712 | 3.13x | 10.42x | 3.33x |
| BL_TEST | 5120x640 | 72.964821 | 23.124243 | 6.917207 | 3.16x | 10.55x | 3.34x |
| CHANNEL | 504x248 | 1.277124 | 0.292637 | 0.221809 | 4.36x | 5.76x | 1.32x |
| CHANNEL | 1008x496 | 4.564615 | 0.817200 | 0.442927 | 5.59x | 10.31x | 1.85x |
| CHANNEL | 2016x992 | 18.605399 | 3.405967 | 1.875161 | 5.46x | 9.92x | 1.82x |
| CHANNEL | 3024x1488 | 41.505285 | 7.186503 | 3.751980 | 5.78x | 11.06x | 1.92x |
| CHANNEL | 4032x1984 | 74.951693 | 12.635074 | 6.485636 | 5.93x | 11.56x | 1.95x |

Geometric means:

| application | PRE3D | +REMAINDER | REMAINDER vs PRE3D |
|---|---:|---:|---:|
| UPWELLING | 5.60x | 7.91x | 1.41x |
| KELVIN | 10.61x | 19.64x | 1.85x |
| CANYON | 4.06x | 9.40x | 2.32x |
| BL_TEST | 3.05x | 8.38x | 2.75x |
| CHANNEL | 5.39x | 9.45x | 1.75x |
| **all 25 points** | **5.24x** | **10.29x** | **1.96x** |

Largest-grid medians (s/step):

| application | reference | PRE3D | +REMAINDER |
|---|---:|---:|---:|
| UPWELLING | 65.982 | 9.540 | 5.872 |
| KELVIN | 72.676 | 7.055 | 4.133 |
| CANYON | 77.263 | 17.878 | 6.804 |
| BL_TEST | 72.965 | 23.124 | 6.917 |
| CHANNEL | 74.952 | 12.635 | 6.486 |

The first campaign overlapped a cluster network incident and produced
occasional slow samples (one of three, 1.3-10x). Every case/label group
whose samples disagreed by more than 5% had all three repetitions
remeasured; the medians above use the clean samples. A few groups retain
one slow sample, but the median is at the consistent value.

## Interpretation

- KELVIN is a barotropic-dominated case, so the already-ported 2-D kernel
  covers most of its work; it reaches 9.2-9.9x.
- The three-dimensional cases are limited by code that was still Fortran in
  these binaries. For UPWELLING 2880x1408 the candidate rank-0 profile is
  approximately: 2-D kernel 19.2%, `pre_step3d` predictor 20.4%, `rhs3d`
  proper 10.2%, 2-D/3-D coupling 3.4%, global information integrals 3.6%,
  pressure gradient 2.9%, tracer corrector 2.8%, EOS 1.2%, plus 13.2%
  non-profiled residual (mostly communication/other Fortran).
- In the r8 matrix, speedup is not monotonic with grid size: UPWELLING s4
  is slower (3.86x) than s3 (4.47x) and s5 (5.11x). This is a
  configuration/cache/communication interaction, not a measurement
  outlier; the three samples per point are tightly clustered.

## Remaining hot spots after Stage 2

With `KK_STAGE_PRE3D` and `KK_STAGE_REMAINDER` enabled, every
iteration-time compute loop exercised by these five configurations is now
in Kokkos. What remains in the stepping wall is dominated by the 2-D
kernel and by non-kernel work:

- about 51% of the stepping wall is the step2d kernel entries
  (`rhs_fused`, `coupling`, `diag_int`, `store_rhs`, `zeta_step`,
  `momentum_step`, `depth_flux`, `time_average`);
- about 22% is the non-kernel remainder (2-D/3-D MPI halo exchanges,
  lateral boundary fills, global reductions, launch gaps);
- the 3-D entries (pressure gradient, RHS proper, tracer corrector,
  predictor, `step3d_uv`) account for most of the rest.

There is no large un-migrated module left for these physics. The next
optimization targets, in order, are:

1. fuse `store_rhs` and `diag_int` into the producer kernels to remove two
   full-tile launches/sweeps per fast step;
2. reduce or overlap the 2-D halo exchanges, which are the largest
   non-kernel item;
3. tune `rhs_fused`/`coupling` loop order and cache blocking, subject to
   the exact-arithmetic gate.

Details and the validation protocol are in
`kokkos-remaining-optimization.md`.

## Exclusions

These applications are not part of the speedup table:

- SEAMOUNT: `step3d_t` whole-tile Fortran fallback observed in some samples
  (large grid and intermediate grid); the candidate path is not fully
  accelerated.
- OVERFLOW: same `step3d_t` whole-tile Fortran fallback.
- LAB_CANYON: analytic grid becomes invalid (`negative DY`) after large
  rescaling, so it is not a usable large-scale test.
- WINDBASIN: stock configuration does not compile with the step2d kernel
  (missing `UV_VIS2`-related declarations). The stock physics was not
  changed to force a build.

## Evidence

Run root (Sunway):

```
~/voyage/roms/work/gls-r6-20261104/runs/mpi12-scale-r8-20261104T155324/
```

Key files:

- `RESULT-five-apps.json` - full per-case results, samples, build hashes;
- `summary-five-apps.csv` - the table above;
- `analyze-five-apps.log` - analysis output;
- `STATUS-attempt3.json`, `run-matrix-attempt3.log` - preserved matrix
  attempt; the runner reports the three SEAMOUNT/CHANNEL validation errors;
- `runs/<app>/<scale>/<backend>/<rep>/run.log` - individual run logs;
- `runs/channel/s4-3024x1488/sw64/1/run.invalid-no-done-8950719.log` -
  preserved invalid sample (12 timing tables and 12 rank walls, but no
  `ROMS: DONE`); the replacement run job is `8951263`.

The validated rerun for CHANNEL s4 SW64 is `8951263`; all five applications
then have three valid samples at all five scales, both backends.

## Caveats

- This is a performance-only matrix. No field was compared for the large
  grids in this run; field validation of the migrated modules is reported
  separately in `ROMS/Cpp/tools/sw64/*.md`.
- GLS and LMD/KPP are not active in these five physics configurations
  (UPWELLING and CANYON use `ANA_VMIX`; KELVIN and BL_TEST do not enable a
  vertical mixing scheme; CHANNEL uses `FORWARD_MIXING`). The GLS stage is
  compiled but has no work in this matrix. GLS/KPP speedups are separate
  results.
- MPI communication, halo exchanges and I/O remain in Fortran and are
  included in the measured stepping wall.
