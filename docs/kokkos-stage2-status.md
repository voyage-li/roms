# Stage-2 (PRE3D + REMAINDER) campaign — completed

Date: 2026-11-05
Source commit: `58191be0`

This note records the provenance of the Stage-2 numbers in
`kokkos-mpi12-scale-benchmark.md`.

## Campaign

- Root: `~/voyage/roms/work/gls-r6-20261104/runs/mpi12-stage2-20261105T0320/`
- 225 jobs: 5 applications × 5 grids × 3 builds (original Fortran, PRE3D,
  PRE3D+REMAINDER) × 3 samples, randomly interleaved.
- Same 12-rank/two-node configuration, two computed steps, no output, and
  the same input files as the r8 matrix.
- Result files: `RESULT-stage2.json`, `summary-stage2.csv`,
  `stage2-tables.md`, `analyze-stage2.log`.
- The first attempt with the default (not final-sizes) grids is preserved
  at `runs/mpi12-stage2-wrongsizes-20261105T0300/` and must not be used.

The campaign driver hit the cluster network outage at 220/225 and exited
on a broken pipe. All 225 jobs had already been submitted; the tolerant
`analyze_stage2.py` audited the logs. Two samples with truncated timer
headers and one with a truncated `pre_step3d` line were rerun cleanly
(jobs 8962792, 8962805, 8963031).

The first campaign overlapped the incident and produced occasional slow
samples. Every case/label group with more than 5% spread across its three
samples had all three repetitions archived (`<rep>.attempt1`) and
remeasured (75 jobs, `rerun-noise.log`, `rerun-noise.json`). A few groups
still contain one slow sample, but their medians are at the consistent
value. The final medians are in `RESULT-stage2.json`.

## Validation

Two-rank, four-step, all-output UPWELLING:

- PRE3D: Serial vs original Fortran **101/101 exact**; SW64 vs Fortran
  **101/101 within 1e-10**; four `pre_step3d` tracer and momentum calls on
  each rank.
- PRE3D+REMAINDER: Serial vs matched PRE3D Serial control **101/101
  exact**; Serial vs Fortran **101/101 exact**; SW64 vs Fortran **101/101
  strict 1e-10**, no diagnostic exception.
- Evidence: `runs/pre3d-r9-20261105T0250/validation/` and
  `runs/pre3d-r9-20261105T0250/validation-remainder/` (STATUS.json, run
  logs, per-file comparison logs).

The large-grid Stage-2 runs are performance measurements only; the field
validation above is at the small two-rank configuration.
