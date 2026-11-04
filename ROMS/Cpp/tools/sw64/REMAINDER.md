# Remaining compute ports — four-case matrix completed, 2026-11-04

This remains **opt-in**, not the production default. The four-case single-rank
performance and two-rank correctness matrix below is complete, with the
explicit diagnostic-roundoff exceptions retained. The previous `*-r2`
controls and their evidence are preserved.

**Compilation provenance:** these measurements predate the conservative
SW64 arithmetic correction described in [KPP.md](KPP.md). They remain valid
records of those vectorized executables, not measurements of the current
`ROMS_FAST_MATH=OFF` policy. Re-measure before claiming current-code speedups.

## Scope

The target option sets are upwelling, official kelvin, 3-D canyon and
**official** bl_test. The last uses SOLAR_SOURCE but does **not** enable
LMD_MIXING/LMD_NONLOCAL; it differs from the tree's older bl_test header.
The existing Kokkos C++ kernels and pinned Kokkos submodule are not tuned.

New coverage, behind `KK_STAGE_REMAINDER`:

- `rhs3d_tile`: the actual momentum RHS, not just the already ported helpers;
- `t3dmix2_geo_tile`: canyon's geopotential tracer diffusion;
- `set_depth`, `set_depth0`, `set_massflux`;
- `rho_eos`, `bulk_flux` and its two stability functions;
- `uv_C2A_grid`, `wvelocity`, `set_vbc`;
- the remaining Fortran paths in `pre_step3d` (including solar heating)
  and `step3d_t` (including non-spline diffusion and A4/A4 advection);
- `lmd_swfrac_tile`, the active shortwave penetration helper, not the KPP
  mixing scheme; its existing coefficient arrays acquire TARGET only;
- active analytical forcing/mixing row loops;
- averages and diagnostic accumulation;
- `diag`'s per-row energy/Courant/density computation. The deterministic
  horizontal/global reductions and reporting remain in Fortran.

Boundary conditions, halo/MPI exchanges and file I/O remain in Fortran.
This is not a claim of support for every optional ROMS physics combination.

## Implementation contract

`ROMS/Cpp/tools/row_codegen.py` lowers a restricted set of explicitly named
subroutines **after CPP has selected the application's physics**. It emits
paired Fortran call sites and C++ Kokkos row kernels as build artifacts.
The original loop expressions remain the source of the arithmetic: no
separately maintained translation of the long EOS/bulk-flux polynomials.

Outer level/tracer loops and launches stay ordered. A synchronous launch
finishes before the next dependent phase. All indexing uses runtime Fortran
lower/upper bounds and ptrdiff_t strides. Tile scratch stays shared between
phases; automatic column scratch gets a separate buffer for each row. The
buffers are host-thread-local, persistent ordinary C++ storage, not Views
that outlive Kokkos finalization. Core model arrays are not copied.

The generator preserves integer scalar kinds, integer division, exponent
precedence and signed-zero SIGN. It rejects unknown statements/calls,
loop-carried scalars, host-visible scalar live-outs, parameter arrays and
oversized argument metadata rather than silently dropping operations.
Tracer-dependent advection flags are staged per tracer, not hoisted from
an out-of-range host loop index. Optional allocatable-array metadata is
accessed only when allocated. Source-enabled rows retain their original
Fortran path, avoiding access to unallocated SOURCES on source-free runs.
Noncontiguous inputs and unsupported time-level aliases also retain the
original row. Such fallbacks are reported, and the matrix rejects them for
the four supported test configurations.

C-grid/A-grid boundary fills have explicit owning rows/columns: a row must
not read another row's unfinished interpolation. Diagnostic row maxima
are folded in original j order, retaining the original first-maximum tie
rule; the original energy/volume summation order is unchanged.

## Timing and comparison protocol

`ROMS_WALL_PROFILE` switches the legacy `my_wtime` profile from CPU_TIME to
SYSTEM_CLOCK. Reference and matched controls use this same flag. The
profile still includes initialization calls for some regions, and the
existing nested use of region 34 by step3d_uv/uv_C2A_grid is not repaired
here. Do not treat those rows as a disjoint stepping-wall decomposition.
`ROMS_KK_TIMING=1` separately reports actual entry-point wall time/calls.

`remainder_matrix.py` builds pure Fortran, Serial and SW64 candidates and
a matched SW64 control with the new stage off. All use identical physics,
conservative math, one MPE/core group and 64 reserved CPEs in q_share.

1. Four **small-grid** computed steps with output exercise first, second
   and later stepping branches. Every reference time-dependent NetCDF
   field and record is checked, including history/averages/diagnostics/
   stations when enabled. Serial requires exact decoded numerical values
   against a matched Serial control with only the new stage off. Exact
   comparisons against pure Fortran are also retained, not hidden: four-step
   Canyon exposed pre-existing third-step roundoff in the old kernels.
   Physical fields must still meet the original-Fortran 1e-10 normalized
   gate; diagnostic exceptions remain explicitly reported.
2. SW64 uses normalized relative tolerance 1e-10, zero-reference absolute
   tolerance 1e-20, plus shape/time/nonfinite checks. Strict diagnostic
   failures are retained explicitly; a separate 1e-15 absolute diagnostic
   cross-check is **not relabeled as a strict PASS**.
3. Performance uses two steps with all available history/average/
   diagnostic/restart/station/float intervals off. Three samples each for
   Fortran/control/SW64; report medians and every underlying measurement.
   Serial has one timing sample. Ratios against original Fortran and the
   old-kernel SW64 control are separate.
4. Optional `--mpi` also validates each application at two ranks, comparing
   against its own two-rank Fortran reference (important for bl_test).

The launcher records exact queue job IDs, binary SHA256 values, module and
kernel timings, input/output directories and comparison logs in a fresh
`runs/remainder-*/STATUS.json`. It checks execution-space banners and new
RHS execution, rejects fallback and no-output runs that write NetCDF, and
never cancels other account jobs. No automatic commit or push.

## Evidence so far

- Host regression suite: **18/18 passed**, including actual compiled C++
  execution with forward/reverse/concurrent rows, full five-dimensional
  time/tracer layout, scratch ownership, boundary-fill ownership, optional
  arrays, source guards, scalar kinds/dependencies and no-output inputs.
  The full tool suite is **32/32 passed**: those 18 tests, 8 NetCDF-parser
  tests and 6 acceptance-policy tests (a diagnostic absolute cross-check
  cannot become a strict PASS, and new Serial/control differences stop
  the matrix).
- Four-option-set local preprocessing/code-generation/C++ syntax checks:
  **64/64 passed**. Local temporary headers explicitly reproduce the
  relevant official kelvin/bl_test header differences. This is **not**
  target numerical validation or a Sunway compiler test.
- An earlier partial Canyon Serial candidate compiled, but its run
  (`8934441`) crashed. A bounds-checked rerun (`8934474`) identified an
  eager access to unallocated SOURCES metadata in tracer argument packing.
  That candidate is preserved under `runs/apps-r3-pre/canyon/`; it must not
  be used as a successful validation or performance result. The guarded
  interface and subsequent fixes require the fresh r4 matrix.
- The SW GCC 7 frontend also lacks IS_CONTIGUOUS. The new module now
  reuses the existing rank 2--5 stride checks and adds a rank-one check;
  the merged generic compiled with the actual target compiler.
- Fresh r4 Canyon: four-step Fortran, Serial, SW64 and SW64-control runs
  completed (`8939979`, `8939982`, `8939984`, `8939985`). New Serial was
  then compared against the preserved r2 Serial (`8940005`): **all 63
  fields are exactly equal** over all four steps. Both differ slightly
  from pure Fortran starting at the third computed step. That baseline
  behavior is retained, not attributed to the new ports or fixed by
  tuning an already migrated kernel.
- Fresh bl_test four-step runs: new Serial (`8940084`), preserved r2
  Serial (`8940085`) and new SW64 (`8940086`) completed. New vs old Serial
  is **96/96 exact**, including the bulk-flux/solar paths, history,
  diagnostics and stations. This is separate from comparison with pure
  Fortran.
- Target descriptor-guard executable: **14/14 passed** (`8940125`), covering
  contiguous/strided rank 1--5 arrays, padded columns, singleton axes and
  rank-one empty sections (`ROMS/Cpp/test_row_guards.f90`).
- Complete target validation/performance matrix: **finished**, state
  `complete_with_reference_roundoff`, 76 queue jobs. Artifacts:
  `runs/remainder-20261104T040106-63596/STATUS.json`. All four applications
  completed single-rank validation/performance and two-rank validation;
  diagnostic exceptions are retained rather than relabeled as strict passes.

## Measured results so far

Canyon 256 x 192 x 15, two computed steps, no NetCDF output, one
MPE/core group, cache 128, q_share. Three samples each except Serial:

| build | stepping wall samples (s) | median (s) |
|---|---|---:|
| original Fortran | 13.829339, 13.813935, 13.797566 | 13.813935 |
| previous-kernel SW64 control | 3.253124, 3.243223, 3.236184 | 3.243223 |
| candidate Serial | 13.942205 | 13.942205 |
| candidate SW64 | 1.303858, 1.287431, 1.285686 | 1.287431 |

New SW64 is **10.73x versus original Fortran** and **2.52x versus the
matched SW64 control**. Corresponding median-run wall profile (seconds):

| region | Fortran | SW64 control | new SW64 |
|---|---:|---:|---:|
| RHS3D proper | 0.491 | 0.491 | 0.067 |
| tracer corrector | 0.592 | 0.584 | 0.063 |
| geopotential tracer mixing | 0.421 | 0.425 | 0.033 |
| depth/mass-flux/vertical-metrics region | 0.163 | 0.161 | 0.027 |
| global information integrals | 0.156 | 0.156 | 0.013 |
| equation of state | 0.043 | 0.042 | 0.009 |
| already-ported 2D kernel (untouched) | 8.681 | 0.672 | 0.683 |

These profile regions can include initialization, as noted above. The new
entry timers account for 0.065500 s in RHS3D, 0.060731 s in tracer loops
and 0.032040 s in geo mixing, close to their respective module regions.
New/old Serial remain exact in all 63 four-step fields. Against pure
Fortran, history passes 8/8 and diagnostics 54/55 at normalized 1e-10.
The sole strict diagnostic exception is temp_vdiff: maxabs 8.88178420e-18,
normrel 1.04154332e-09 in new Serial/SW64; the old controls also have this
single strict diagnostic exception. It passes the separately reported absolute cross-check, not
the strict normalized check.
All four single-rank application measurements and all four two-rank MPI
checks are complete. Strict diagnostic exceptions remain explicit below.

### Official bl_test

256 x 16 x 30, the same two-step no-output protocol and resource allocation:

| build | stepping wall samples (s) | median (s) |
|---|---|---:|
| original Fortran | 1.931278, 1.929334, 1.932693 | 1.931278 |
| previous-kernel SW64 control | 0.731402, 0.732818, 0.734755 | 0.732818 |
| candidate Serial | 2.220011 | 2.220011 |
| candidate SW64 | 0.443446, 0.444504, 0.445075 | 0.444504 |

New SW64 is **4.34x versus original Fortran** and **1.65x versus the
matched SW64 control**. Serial is a correctness control, not the speedup
denominator; its one timing sample is slower than pure Fortran.
Corresponding median-run wall profile (seconds):

| region | Fortran | SW64 control | new SW64 |
|---|---:|---:|---:|
| atmosphere-ocean bulk flux | 0.099 | 0.099 | 0.013 |
| 3-D predictor, including solar heating | 0.190 | 0.188 | 0.067 |
| RHS3D proper | 0.077 | 0.077 | 0.053 |
| equation of state | 0.057 | 0.057 | 0.028 |
| depth/mass-flux/vertical-metrics region | 0.029 | 0.030 | 0.016 |
| global information integrals | 0.027 | 0.028 | 0.006 |
| tracer corrector, mostly already ported | 0.140 | 0.022 | 0.020 |
| already-ported 2D kernel (untouched) | 0.969 | 0.178 | 0.179 |

New entry timers include bulk_flux 15 calls / 0.012402 s, pre_step3d
1202 / 0.057825 s, rhs3d 1142 / 0.049836 s, rho_eos 4 / 0.026299 s,
lmd_swfrac 58 / 0.004217 s and diag 3 / 0.005139 s. The narrow 16-row
performance grid limits the available row-level parallelism relative to the
64 CPEs. This experiment does not separately measure occupancy, launch and
memory effects. This is a coverage port, not a tuning of the pre-existing
kernels or launch strategy.

In the fresh matrix, new Serial is exact against **both** matched old
Serial and original Fortran: 57 diagnostic + 22 history + 17 station fields,
**96/96** over all four computed steps. New SW64 passes the normalized
check against old SW64 for all 96 fields. Against pure Fortran, history
22/22 and stations 17/17 pass, while diagnostics pass **49/57** strictly.
The eight exceptions are ubar_yadv, vbar_yadv, vbar_prsgrd, u_yadv, v_yadv,
v_prsgrd, temp_yadv and salt_yadv; the old SW64 control has the same
exceptions and reported error values. The maximum absolute error among
these eight is 6.92354663e-18 (salt_yadv); maximum normalized error is
7.17182019e-06 (vbar_yadv). All eight pass the separate absolute diagnostic
cross-check; they remain strict normalized failures. Current validation
jobs: Fortran 8940224, Serial 8940225, SW64 8940226, SW64 control 8940228,
Serial control 8940230. Every performance job ID is in STATUS.json.

### Kelvin and upwelling

Both use 256 x 256 horizontal grids, two computed steps and no output;
Kelvin has 10 vertical levels and upwelling 16. Three samples except Serial:

| application / build | stepping wall samples (s) | median (s) |
|---|---|---:|
| Kelvin original Fortran | 60.152516, 60.182928, 60.138423 | 60.152516 |
| Kelvin previous-kernel SW64 | 5.461132, 5.449526, 5.454499 | 5.454499 |
| Kelvin candidate Serial | 77.612823 | 77.612823 |
| Kelvin candidate SW64 | 2.977157, 2.979352, 2.978964 | 2.978964 |
| upwelling original Fortran | 24.734134, 24.744572, 24.750453 | 24.744572 |
| upwelling previous-kernel SW64 | 3.496467, 3.500505, 3.495436 | 3.496467 |
| upwelling candidate Serial | 25.213140 | 25.213140 |
| upwelling candidate SW64 | 2.109929, 2.102295, 2.107798 | 2.107798 |

Kelvin: **20.19x versus original Fortran**, **1.83x incremental versus
previous-kernel SW64**. New/old Serial: 62/62 fields exact. Serial meets the
normalized original-Fortran gate for all 62 fields. New SW64 history passes
8/8; diagnostics pass 50/54 strictly. The exceptions are temp_hdiff,
temp_xdiff, temp_ydiff and temp_vdiff. Their maximum absolute errors range
from 1.59883573e-21 to 3.94745964e-18, while normalized errors reach 2/3 for
these near-zero reference budgets. The absolute diagnostic cross-check
passes, but these are not strict normalized passes.

Upwelling: **11.74x versus original Fortran**, **1.66x incremental versus
previous-kernel SW64**. New/old Serial: 101/101 fields exact. Both Serial
and SW64 meet the normalized original-Fortran gate for all 101 fields:
28 averages, 63 diagnostics and 10 history fields.

Across the four single-rank cases, new Serial is exact against matched
pre-port Serial in all 322 compared fields over all records. SW64 passes
the strict physical-field gate in all 93 history/average/station fields.
There are 13 explicitly retained normalized diagnostic exceptions among
229 diagnostic fields (1 Canyon, 8 BL, 4 Kelvin), all passing the separate
absolute cross-check. Two-rank MPI results are separate below.

### Two-rank MPI validation

All four applications completed four-step runs with NtileI=2/NtileJ=1,
`-n 2 -np 2 -cgsp 64`: two MPEs and 128 CPEs. Each is compared against
its own two-rank Fortran and matched two-rank pre-port Serial reference.
New/old Serial are **322/322 exact**. SW64 history/average/station fields
pass **93/93** at the strict physical-field gate.

Strict SW64 diagnostic outcomes against two-rank Fortran:

| application | strict diagnostic fields | retained exceptions |
|---|---:|---|
| Canyon | 54/55 | temp_vdiff, maxabs 8.88178420e-18 |
| BL | 51/57 | vbar_prsgrd, u_yadv, v_yadv, v_prsgrd, temp_yadv, salt_yadv; maxabs 4.13085485e-18 |
| Kelvin | 50/54 | temp_hdiff, temp_xdiff, temp_ydiff, temp_vdiff; maxabs 3.94745964e-18 |
| upwelling | 63/63 | none |

These 11 exceptions pass the separate absolute diagnostic cross-check, not
the strict diagnostic check. BL's listed two-rank reference budgets are
identically zero, so they fail the original 1e-20 zero-reference absolute
threshold rather than a normalized nonzero-reference threshold. No MPI
performance speedup is inferred from these correctness runs.

The complete matrix finished with state `complete_with_reference_roundoff`;
all exact comparison failures and diagnostic cross-checks remain in STATUS.json
and their individual logs. The pinned dependency and old kernel implementations
were not tuned.

## Reproduce

On the Sunway login host, within `~/voyage/roms/`:

```sh
module load swgcc/1473 mpi/swuc
RUN_ROOT=runs/remainder-$(date -u +%Y%m%dT%H%M%S) \
  python3 ROMS/Cpp/tools/sw64/remainder_matrix.py \
    --apps canyon bl_test kelvin upwelling --mpi
```

From the local checkout, `bash ROMS/Cpp/tools/sw64/deploy_remainder.sh`
waits for the authorized `ssh SW` connection, deploys only the listed
project files and watches a detached matrix. It covers success, failure,
unexpected process exit and connection/monitor deadlines; silence is not
reported as success. Existing runs are never overwritten. All remote
artifacts remain under `~/voyage/roms/`.
