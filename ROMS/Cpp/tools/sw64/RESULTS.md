# Sunway validation — 2026-11-02

Source baseline: ROMS `e95d8768` plus the accompanying uncommitted fixes;
Kokkos remains pinned at `6e40ad34a8968a44386e01a5a757e1e37bfcacea`.
Deployment: `ssh SW`, `~/voyage/roms/`. Queue: **q_share**.

## Configuration and execution evidence

- Cross toolchain: `swgcc/1473` (GCC 7.1), `mpi/swuc` link wrapper.
- Primary runs: **MPI=OFF**, OpenMP disabled, one MPE/core group per job.
- Serial: `Kokkos::Serial`, no CPE computation.
- SW64: `Kokkos::SW64`, `RMS_MPE_CGS=1`, `RMS_CG_SPES=64`.
- Direct probe verified CPE IDs **0..63**, not a Serial fallback.
- Both Kokkos builds enable FUSED/TAVG/COUP/MOMT/STOR, plus the directly
  gated depth/flux and free-surface stages. Generated Fortran has the
  migrated calls and no duplicate pressure-gradient fallback.
- Double NetCDF output; matching conservative math policy,
  `ROMS_FAST_MATH=OFF`. Static Fortran link includes `-mhybrid`, with
  host/slave C++ runtimes added after Kokkos archives.

The older fused measurements are not a valid baseline: original Fortran
RHS loops were still executing after the fused call and overwriting its
result. This change makes the paths exclusive and fixes cross-advection
stencils, PSI metrics and complete predictor diagnostic storage.

## Numerical checks

Case: upwelling **41 x 80 x 16**, one tile, **2 computed steps**;
initial and subsequent double-precision records are compared.

Fortran reference versus Kokkos Serial: maximum difference **0** in
history fields (5), selected diagnostic fields (23), and average fields
(5). This is exact equality of decoded numerical values, not file hashes.

Kokkos Serial versus SW64:

| History field | Maximum absolute difference | Difference / reference peak |
|---|---:|---:|
| zeta | 4.23516474e-21 | 1.28634444e-15 |
| ubar | 3.81164826e-21 | 1.31060651e-15 |
| vbar | 1.37642854e-21 | 1.77234244e-15 |
| u | 1.35525272e-20 | 3.91085436e-16 |
| v | 3.38813179e-21 | 1.35631893e-16 |

All 5 history, 23 diagnostic and 5 average comparisons pass a normalized
relative threshold of `1e-10`. Identically zero reference diagnostic
fields have no defined relative error: their largest nonzero difference
is `4.57269117e-24`; these are checked separately against an explicit
absolute threshold of `1e-20`, without loosening the nonzero-field
relative criterion. Surface stress fields agree exactly.

Direct target regressions also cover asymmetric xi/eta curvature,
unequal PSI metrics, closed/periodic edges, state time levels 1..3,
complete stored stress diagnostics, and all first/second/later-step
coupling branches with both history slots and non-first/corrector cases.
Both target backends pass (131 PASS checks per backend). Negative compile checks reject MASKING and
SW64 kernel compilation without `-mhybrid-coding`. The comparison parser's
8 local regression tests cover nonfinite/fill/incomplete/missing data,
shape mismatch, tolerances and zero-reference handling.

## Two-step performance sample

**256 x 256 x 16**, one tile, 2 steps, no NetCDF output; Serial and SW64
runs were submitted sequentially, each with one core group. This is a
single sample per backend, not a statistical scaling study.

| Backend | Initialization wall (s) | Time-stepping wall (s) |
|---|---:|---:|
| Kokkos Serial | 2.656090 | 25.368187 |
| Kokkos SW64, 64 CPEs | 2.700950 | 12.018212 |

**Time-stepping speedup: 2.11x**; initialization plus stepping: **1.90x**.
`SYSTEM_CLOCK` measures both segments. Queue delay, job launch and final
teardown are outside these segments; outer `time bsub` values are not
used for the ratio. Legacy ROMS CPU-time profiles are not offload wall
time and their MPI Total would sum ranks.

## Artifacts and limits

Remote artifacts:

- `build/{reference,serial,sw64}/romsS`
- `build/{serial,sw64}/test_step2d_kokkos`
- `runs/smoke/{reference,serial,sw64}/`: inputs, outputs and run logs
- `runs/perf256/{serial,sw64}/`: two-step no-output performance logs
- `logs/compare-*.log`, `logs/test-*.log`, `logs/regression-*.log`,
  `logs/negative-*.log`, build logs and `logs/source-version.txt`

MPI+SW64 has **not** been run in this validation. Other physics macro
combinations and long integrations have not been validated. The two-step
model run does not establish long-time stability. No claim is made about
64x speedup, whole-node scaling, or the reason for the remaining runtime.
