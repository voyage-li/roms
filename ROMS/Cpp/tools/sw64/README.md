# ROMS on Sunway (Kokkos / SW64)

How to build, run, compare and measure the Kokkos port on the Sunway system.
Results and validation records are kept in separate notes:

| note | content |
|---|---|
| [`docs/kokkos-mpi12-scale-benchmark.md`](../../../../docs/kokkos-mpi12-scale-benchmark.md) | 12-rank speed-up campaigns: five applications, five grids |
| [`docs/kokkos-remaining-optimization.md`](../../../../docs/kokkos-remaining-optimization.md) | profile after the last port, what is left |
| [REMAINDER.md](REMAINDER.md) | row-lowered ports (`KK_STAGE_REMAINDER`): design, four-application validation |
| [KPP.md](KPP.md) | LMD/KPP rows (`KK_STAGE_LMD`) and the strict-arithmetic policy |
| [GLS.md](GLS.md) | GLS closure rows (`KK_STAGE_GLS`) |

## Workspace rules

- Work from the login host reached by `ssh SW`. New files, builds and runs
  belong in `~/voyage/roms/` (a snapshot goes to `~/voyage/roms/work/<name>/`
  with `Kokkos` symlinked to the pinned `~/voyage/roms/Kokkos`).
- Do not replace the separate `~/voyage/kokkos` checkout, change the Kokkos
  revision or touch the installed NetCDF (`$HOME/ftao/local-saca`).
- Submit to `q_share`, record the exact job IDs and never cancel a job you
  did not submit.
- Never overwrite earlier evidence: use a new `TAG_SUFFIX` for a build and a
  new directory for a run. `make_app_case.py` and `run_app.sh` refuse to
  replace an existing input or `run.log`.

## 1. Build

```sh
cd ~/voyage/roms
MPI=ON TAG_SUFFIX=-r1 bash ROMS/Cpp/tools/sw64/build_app.sh UPWELLING
BACKENDS=sw64 bash ROMS/Cpp/tools/sw64/build_app.sh KELVIN '' official/kelvin
```

`build_app.sh APP [EXTRA_CPP_FLAGS] [HEADER_DIR]` builds one application with
an identical physics option set as `reference` (pure Fortran), `serial`
(Kokkos Serial) and `sw64` (Kokkos SW64) into
`build/<APP>-<backend>[-mpi]<TAG_SUFFIX>`. `APP` is the upper-case header stem
(`BL_TEST` -> `bl_test.h`); `HEADER_DIR` defaults to the checkout's
`ROMS/Include`, or point it at a `myroms/roms_test` case directory. Environment, all optional:

| variable | meaning | default |
|---|---|---|
| `BACKENDS` | builds to make | `reference serial sw64` |
| `MPI` | `ON` builds `romsM`, `OFF` builds `romsS` | `OFF` |
| `TAG_SUFFIX` | appended to the build directory name | empty |
| `KK_STAGES` | replaces the stage list of the Kokkos builds | validated list below |
| `STEP3D_T` | tracer chunk kernel in Kokkos builds | `ON` |
| `TESTS` | also build the direct kernel tests | `OFF` |
| `BUILD_JOBS` | parallel build jobs | `6` |

The script loads `swgcc/1473` and `mpi/swuc` and uses the pinned `Kokkos/`
and the NetCDF in `$HOME/ftao/local-saca` (`NF_CONFIG`, `NETCDF`,
`NETCDF_INCDIR` override it). `Compilers/toolchain_SW64.cmake` selects the
compilers: `swucxx` for C++ and `mpif90` for Fortran and the link, because the
available NetCDF/HDF5 archives need the MPI runtime even for `MPI=OFF`. The
platform flag `SW64=TRUE` admits this fork's GCC 7.1 also for the Serial
control; only the `sw64` backend enables `Kokkos_ENABLE_SW64` and hybrid
kernel compilation.

**Common options.** `OUT_DOUBLE;VAR_RHO_2D;DIAGNOSTICS_UV;DIAGNOSTICS_TS` and
`ROMS_WALL_PROFILE` go into every build together with the application's own
extras, so comparisons are like for like. The step2d kernels require
`VAR_RHO_2D` and `DIAGNOSTICS_UV`, the tracer kernel `DIAGNOSTICS_TS` with
`SPLINES_VDIFF`. This deviates from the stock headers (`VAR_RHO_2D` changes
the 2-D pressure-gradient formulation); `canyon.h` also needs `SOLVE3D` passed
as an extra. `ROMS_WALL_PROFILE` makes the module-wall profile use
`SYSTEM_CLOCK` instead of CPU time. Unsupported physics fails at compile time.

**Stages.** The default `KK_STAGES` is the validated production set
`FUSED;TAVG;COUP;MOMT;DIAG;STOR;UV3DMIX;T3DMIX;PRSGRD;OMEGA;SETZETA;STEP3DUV;GLS;PRE3D;REMAINDER`
(each with the `KK_STAGE_` prefix):

| flag | moves to Kokkos |
|---|---|
| `FUSED` | fused 2-D right-hand side (`k_step2d_rhs_fused`) |
| `TAVG` `COUP` `MOMT` `DIAG` `STOR` | 2-D time average, 2-D/3-D coupling, momentum update, diagnostic integrals, stored RHS and stress diagnostics |
| `UV3DMIX` `T3DMIX` `PRSGRD` `OMEGA` `SETZETA` | 3-D momentum and tracer mixing, pressure gradient, omega, free-surface set-up (hand-written, `kokkos_rhs3d.cpp`) |
| `STEP3DUV` `PRE3D` | `step3d_uv` and the `pre_step3d` predictor (`kokkos_step3d.cpp`) |
| `STEP3D_T` | tracer corrector chunks for U3/C4 and HSIMT; set by `STEP3D_T=ON`, not by `KK_STAGES` |
| `REMAINDER` `GLS` `LMD` | generated row kernels, see section 6; `LMD` is not in the default list |

The depth/flux and free-surface 2-D kernels need no flag; `ROMS_KOKKOS`
enables them. `PRE3D` cannot handle `SOLAR_SOURCE`, so BL_TEST has to drop it.
Boundary conditions, halo exchanges, global reductions and I/O stay in
Fortran.

**Arithmetic policy.** `ROMS_FAST_MATH=OFF`, and on SW64 the C++ units are
compiled with `-fno-tree-vectorize` (CPE offload is unaffected): swgcc-1473's
auto-vectorized CPE code changes rounding by one ULP, which KPP amplifies
beyond the field tolerance ([KPP.md](KPP.md)). Timings taken with vectorized
kernels are historical, not results of this policy.

## 2. Cases and runs

```sh
python3 ROMS/Cpp/tools/sw64/make_app_case.py official/kelvin/roms_kelvin.in \
  runs/kelvin-val --size 50 30 --steps 4 --tiles 2 1
RANKS=2 bash ROMS/Cpp/tools/sw64/run_app.sh KELVIN-sw64-mpi-r1 runs/kelvin-val
```

`make_app_case.py SOURCE_IN DIRECTORY` copies an application input and changes
only the grid (`--size LM MM`), `--steps`, `--dt`, the tiling (`--tiles NI NJ`,
whose product must equal the rank count: ROMS runs one tile per rank) and the
output frequency. `--no-output` also switches off station and float output.
`VARNAME` and station/float inputs are pointed at existing files.

`run_app.sh BUILD_TAG RUN_DIR` submits the case with `bsub -I`, waits, writes
`RUN_DIR/run.log` and fails without `ROMS: DONE` or with `ERROR STOP`,
`Abnormal termination` or `BLOWUP`. Environment: `RANKS` (default 1, more than
one needs an MPI build), `RANKS_PER_NODE` (default 6, at most 6), `QUEUE`,
`SHARE_SIZE`, `TIMELIMIT`. It prints the job ID, the stepping wall and any
fallback line.

`bsub -n` is the total number of ranks (MPE processes) and `-np` the number
per node; a node has 6 MPE cores, and `-cgsp 64` gives each rank one 64-CPE
core group.

| ranks | `bsub` | nodes | MPEs | CPEs |
|---:|---|---:|---:|---:|
| 1 | `-n 1 -np 1` | 1 | 1 | 64 |
| 6 | `-n 6 -np 6` | 1 | 6 | 384 |
| 12 | `-n 12 -np 6` | 2 | 12 | 768 |
| 24 | `-n 24 -np 6` | 4 | 24 | 1536 |

`-np 8` is rejected. Further launch rules that matter:

- The model needs `-i INPUT` **and** the input as its argument; without `-i` it
  prints the parameter banner and blocks. `run_app.sh` does both.
- `-cache_size 128` is the setting used everywhere: 11-19% faster end to end
  than 32 (3.4x on `rhs_fused` at 256^2). The LDM share mode changed nothing
  measurable.
- `OMP_NUM_THREADS=1`. Serial reserves the same resources but never launches
  CPE work. Six SW64 ranks occupy six MPEs and 384 CPEs; never compare them with
  one Serial rank as an equal-rank control.
- `ROMS_KK_TIMING=1` in the job environment prints, at shutdown, the wall time
  and call count of every Kokkos entry on every rank. Launches are
  synchronous, so these are the kernels' own in-model costs; subtracting their
  sum from `Model 2D kernel` leaves the Fortran still running on the MPE.
- The log shows `ROMS Kokkos execution space: SW64` or `Serial`.

## 3. Compare

```sh
bash ROMS/Cpp/tools/sw64/cmp_app.sh runs/x/fortran runs/x/serial
bash ROMS/Cpp/tools/sw64/cmp_app.sh runs/x/fortran runs/x/sw64 --rtol 1e-10 --zero-tol 1e-20
```

`cmp_app.sh REF_DIR TEST_DIR [nccmp options]` compares, with `nccmp.py
--all-time`, every variable that depends on `ocean_time` in every `roms_*.nc`
of the reference except the restart file (UPWELLING: 101 fields in history,
averages and diagnostics). The variable list comes from the reference file, so
it works for any application; a variable missing from one side fails. Without
`--all-time`, `nccmp.py` checks a short default list (zeta, ubar, vbar, u, v and
the 2-D momentum diagnostics) that misses the tracers, `w`, `omega`, `rho`
and the 3-D budget terms. `--vars` and `--all-time` exclude each other. The
files are read with the x86 `ncdump`; `cmp_app.sh` puts
`/usr/sw/yyzlib/local_x86` on the path (a target `ncdump` fails).

**Acceptance gates** (set before looking at results):

- NaN/Inf/fill values, a missing field, or a shape or record-time mismatch is
  a failure.
- Serial equals its matched control (the same build without the new stage)
  value for value. Against the original Fortran it is value-for-value equal on
  UPWELLING and within the gate below elsewhere (CANYON shows pre-existing
  round-off from the third step on, [REMAINDER.md](REMAINDER.md)).
- SW64 against the original Fortran: max |difference| / reference peak
  <= 1e-10 (`--rtol 1e-10`); fields whose reference is identically zero use the
  absolute `--zero-tol 1e-20`. Anything outside is reported as an exception with
  the separate absolute cross-check (`--tol 1e-15`), never relabeled PASS.
- No fallback: the log has no `whole-tile Fortran fallback` (`step3d_t`,
  `pre_step3d`) and no `Kokkos row fallback:`, and every rank prints a kernel
  table. A fallback is silent otherwise; enabling a stage proves nothing.
- Validation runs use at least four steps (first, second and later stepping
  branches) with all output on; matching KE printed in the log is not a
  comparison.

## 4. Direct kernel tests

```sh
MPI=OFF TESTS=ON TAG_SUFFIX=-tests BACKENDS="serial sw64" \
  bash ROMS/Cpp/tools/sw64/build_app.sh UPWELLING
for t in test_step2d_kokkos test_step3d_t_kokkos test_row_guards; do
  bsub -I -b -q q_share -n 1 -np 1 -cgsp 64 -share_size 15000 -cross_size 1024 \
    -host_stack 1024 -cache_size 128 -timelimit 00:10:00 build/UPWELLING-sw64-tests/$t
done
```

builds, next to `romsS`, three executables (no `-i` input is needed; run the
`serial` build the same way):

- `test_step2d_kokkos`: the store, momentum and coupling kernels against Fortran
  loops on asymmetric fields, closed/periodic edges and all time levels.
  `rhs_fused` has no direct oracle; the end-to-end Serial equality covers it.
  The SW64 run also requires CPE IDs 0-63 to be observed, so a silent fallback
  to the MPE cannot pass.
- `test_step3d_t_kokkos`: `k_step3d_t_chunks` against a Fortran copy of the
  `step3d_t` loops, including a 37-column tile that crosses the 32-column chunk.
- `test_row_guards`: the descriptor-based contiguity guard of the row kernels.

Each prints `PASS` lines and exits non-zero on a failure. Currently
`test_step2d_kokkos` prints 121 PASS lines, `test_step3d_t_kokkos` 144 (maximum
tracer and diagnostic difference 0) and `test_row_guards` 21 checks, on both
backends.

## 5. Performance protocol

- Two computed steps, all output off (`make_app_case.py --no-output`); compare
  `ROMS time stepping wall [rank=0]`, which is `SYSTEM_CLOCK`. The outer
  `time bsub` includes queueing, and the legacy ROMS `Total` is CPU time summed
  over ranks; neither is a speed-up denominator. Initialization is reported
  separately.
- Three samples per point, report the median and every sample. Serial is a
  correctness control and gets one sample. The denominator is the pure-Fortran
  build with the same options, never the Serial build.
- Region timings (`ROMS_WALL_PROFILE`) can include initialization, and region
  34 is nested: `uv_C2A_grid` reopens it inside `step3d_uv`, so that row holds
  only the C2A calls and the exchanges, not the module. Do not read the regions
  as a disjoint decomposition of the stepping wall.
- Exclude queue and build time, check that no NetCDF file was written, and
  record the executable's SHA256 before and after.

`remainder_matrix.py` and `mixing_matrix.py` automate build, validation and
timing (one STATUS.json per run with job IDs and hashes); see
[REMAINDER.md](REMAINDER.md) and [KPP.md](KPP.md).

## 6. Row generator

`ROMS/Cpp/tools/row_codegen.py` lowers the independent `DO j` loops of a fixed
list of routines to Kokkos row kernels, **after** CPP has selected the physics,
and emits the Fortran call sites next to the C++ (`f90/<name>.f90`,
`f90/<name>_rows.cpp`; build artifacts, never edited). The original Fortran
expressions are the only source of the arithmetic. `Compilers/roms_functions.cmake`
decides per file: `KK_STAGE_REMAINDER`, `KK_STAGE_LMD` and `KK_STAGE_GLS` select
the files of `row_codegen.TARGETS` that go through the generator.

The generator is a restricted translator, not a Fortran compiler, and fails
the build rather than guess. It rejects unknown statements and calls,
loop-carried scalars, scalar live-outs, parameter arrays, a DO step other than
a literal on an inner loop (the parallel loop must have unit stride), `.NOT.`
before an unparenthesized comparison (C++ `!` binds tighter), and integer
exponents other than 2, 3 and 4 (the Fortran compiler multiplies them out,
`pow` would round differently). `/=` becomes `!=`. Rows that cannot be handled at run
time (non-contiguous arrays, source terms, unsupported time-level aliasing)
keep the original Fortran loop and print a fallback line. Safety also relies on
the controlled routine list: a row that writes at `j` and reads at `j+-1` would
race, and the generator does not prove its absence.

Host tests (no Sunway needed), including compiled row kernels run forward,
reversed and concurrently:

```sh
python3 -m unittest discover -s ROMS/Cpp/tools -p 'test_*.py'
```

They also check that the stage lists in `roms_functions.cmake` and
`row_codegen.TARGETS` name the same files, and that generation does not depend
on the Python hash seed.

## 7. Rules the kernels follow

- **Operand order.** Keep the Fortran association order exactly:
  `rhs+1.5*rufrc-0.5*ru` is evaluated left to right, not `rhs += 1.5*rufrc-0.5*ru`.
  Both compilers are GCC 7.1, so the same expression tree rounds the same way,
  FMA contraction included; that is what makes Serial value-for-value equal to
  Fortran.
- **Loop order.** Replay the Fortran loops with `i` innermost over chunks of 32
  columns, one work item per row. Walking a column at a time, or a strided
  order, was 3-5x slower on SW64 (and slower on Serial too).
- **No `parallel_reduce`.** It uses the execution-space instance's reduce
  scratch and is not safe when several tiles enter concurrently; reductions
  stay in Fortran with a fixed order.
- **Fallbacks are visible**: `step3d_t` and `pre_step3d` fall back for the
  whole tile (unsupported scheme, point sources, unsupported levels or tracer
  count) and print `whole-tile Fortran fallback` once.
- **Compiled-out hooks.** KELVIN, CANYON and CHANNEL have no `SPLINES_VDIFF`,
  so the `step3d_t` hook is not compiled and their tracer work runs as generated
  rows; for them "no `step3d_t` fallback" proves nothing.

## 8. Decisions

- **No TeamPolicy.** On SW64 a team is 64 CPEs and leagues run one after another.
  Every step2d kernel's team entry ran at 0.58-0.86 of the range entry's speed
  at 41x80, and sharing the `step3d_t` x-face flux in team scratch paid
  (1.6-1.7x) only beyond about 65k columns per launch, while a rank holds 3-4k.
  The experiment and its benchmarks were removed.
- **Generated rows rather than hand ports** for the long tail of loops: the
  EOS and bulk-flux polynomials stay single-sourced in Fortran. The hand-written
  `pre_step3d`/`step3d_uv`/`step3d_t` kernels and the generated rows do not
  overlap; replacing the hand kernels would remove more code but needs its own
  A/B accuracy and performance run.
- **Strict arithmetic over speed**: `-fno-tree-vectorize` and
  `ROMS_FAST_MATH=OFF` ([KPP.md](KPP.md)).

## 9. Scope and known limits

The validated physics is the diagnostic 3-D combinations of UPWELLING, KELVIN,
CANYON, BL_TEST and CHANNEL, plus the GLS and LMD/KPP variants of
UPWELLING/BL_TEST (details in the records above). Other physics combinations
(MPDATA, WEC, WET_DRY, MASKING, nesting, open boundaries, biology, sediment) are
neither supported nor validated; `MASKING` and similar fail at compile time or
fall back. Long-time stability has not been established by the two- and
four-step runs. SEAMOUNT does not build against this ROMS revision (an
`io_err` use in `analytical.F` for that option set, unrelated to Kokkos); the
official `roms_test` headers otherwise target an older release, and `bl_test.h`
and `kelvin.h` compile unchanged. The exclusions of the benchmark campaigns are
listed in `docs/kokkos-mpi12-scale-benchmark.md`.
