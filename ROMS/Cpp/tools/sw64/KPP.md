# Iteration-time LMD/KPP ports — verified, 2026-11-04

## Result and scope

`KK_STAGE_LMD` adds opt-in Kokkos computation for `lmd_vmix_tile`,
`lmd_skpp_tile`, `lmd_bkpp_tile` and `lmd_finish_tile`. The tested KPP
configuration passes four-step, full-output validation on **one and two
MPI ranks**. Corrected-policy one-rank performance is measured separately.
This round does not add initialization ports or change default physics.

The test is a **new KPP-enabled official BL_TEST variant**, not the earlier
non-KPP BL benchmark. Define `LMD_MIXING` externally; the official header
selects LMD_RIMIX, LMD_CONVEC, LMD_SKPP, LMD_BKPP, LMD_NONLOCAL, LMD_DDMIX
and RI_SPLINES. Every comparison also enables PERFECT_RESTART so restart
output includes nonlocal transport. The prior-kernel controls retain
KK_STAGE_REMAINDER but not KK_STAGE_LMD. KK_STAGE_PRE3D stays disabled:
solar/nonlocal predictor computation uses the original-source row port.

Column recurrences and level searches keep their original k order.
Boundaries, smoothing calls, MPI exchanges, global reductions and I/O
remain in Fortran. Successive launches remain synchronous. The finish
routine lowers its first computational j loop, not later boundary fills.
No ana_grid, ana_initial, allocation or startup routine is added to the
lowering target list. Other closures/configurations are not claimed here.
The pinned Kokkos dependency remains unchanged.

## Precision failure: root cause and correction

The initial combined candidate **and** pre-port SW64 control failed the
original-Fortran physical gate: AKv maxabs=1.57006595e-11,
normrel=1.24705662e-09, above 1e-10. w and tracer diagnostics also failed.
Agreement with the old SW64 control was not accepted as a fix.

Full binary call-boundary snapshots established the propagation path:

1. Initial compared geometry and the first KPP invocation agree exactly.
2. The first valid state discrepancy is the first barotropic momentum
   update: identical inputs produce one-ULP different outputs.
3. After the tracer corrector, temperature/salinity differences reach
   3.552713678800501e-15. The next density calculation produces two bvf
   differences, up to 5.6927949360812205e-15.
4. Convective mixing in lmd_finish amplifies that seed. At (3,4,27), Akv
   differs by 4.461044381121049e-12 in the second KPP invocation.

An identical-input replay of the **production** momentum entry reproduces
this with swgcc/1473 auto-vectorization, despite `-mieee` and no fast-math.
The same CPE entry without auto-vectorization is exact, as is an independent
scalar CPE arithmetic replay. This does not demonstrate an inherent scalar
CPE IEEE-division error. Disabling contraction alone did not fix the model.

A stage-local workaround fixed the first barotropic loop but exposed the
next discrepancy at step3d_uv; disabling vectorization only in step2d did
not fix the complete model. The maintained correction therefore applies
`-fno-tree-vectorize` to ROMS C++ computation units and direct tests/benchmarks
when the backend is SW64 and `ROMS_FAST_MATH=OFF`.

**Kokkos/CPE parallel offload remains enabled.** No physics, tolerance,
validation length or numerical expression is changed. No accelerated
module is replaced by Fortran in the accepted builds. Fortran/Serial
controls keep their existing flags. The acceptance runner checks actual
production compile commands, rejecting missing/overridden strict options.

## Integer layer-index ABI

KPP's ksbl/kbbl are INTEGER arrays. The existing argument packet is
unchanged, but views and row-private buffers preserve their element type:
`Array<Rank,T>`, with an INTEGER(C_INT) rank-two stride guard. Shared integer
array dummies acquire TARGET for C_LOC. Unsupported shared integer ranks
and logical-array reinterpretation fail closed. Grid extents Lm/Mm are
captured as integers, not inferred to be logical by their initial letter.

The actual target-preprocessed LMD sources generate 36 entry points
(4 interior/finish, 16 surface, 16 bottom). Runtime timers confirm all four
KPP computation families execute; no row fallback occurs.

## Numerical acceptance

Validation uses 39 x 4 x 30, **four computed steps**, all records in history,
diagnostics, perfect-restart and station output. Both DT=150 and DT=30 are
checked. Each case compares 125 fields: 57 diagnostics, 24 history,
26 restart and 18 station fields. Missing fields/files, shapes, times and
nonfinite values are checked. Required fields include AKv/AKt/AKs/Hsbl/Hbbl
and ghat_temp/ghat_salt.

Original normalized gates are unchanged: 1e-10 for nonzero reference fields,
1e-20 absolute for zero-reference fields. **No diagnostic absolute-crosscheck
exception is needed in the corrected KPP results.**

| Ranks | DT | New/control Serial exact | SW64/original Fortran normalized | Mixing/nonlocal fields | w normalized error |
|---:|---:|---:|---:|---|---:|
| 1 | 150 | 125/125 | 125/125 | exact | 3.00884212e-16 |
| 1 | 30 | 125/125 | 125/125 | exact | 0 |
| 2 | 150 | 125/125 | 125/125 | exact | 3.00884212e-16 |
| 2 | 30 | 125/125 | 125/125 | exact | 0 |

“Mixing/nonlocal” means AKv, AKt, AKs, Hsbl, Hbbl, ghat_temp and ghat_salt,
over all compared records. All **500/500** normalized SW64/Fortran field
comparisons pass; new/control Serial are exact in all 500 comparisons.
Two-rank SW64/Serial comparisons also pass all 250 normalized comparisons.

DT=30 Serial/original Fortran is exact. DT=150 retains small exact-comparison
differences, while every normalized gate passes. Those retained exact
comparisons account for `complete_with_reference_roundoff` in STATUS.json;
this is not a remaining physical-gate failure or an absolute-tolerance
exception. Exact original-Fortran comparison logs are not discarded.

MPI uses `-n 2 -np 2 -cgsp 64`, NtileI=2/NtileJ=1. Both ranks print SW64
execution-space banners, both tiles execute the tracer accelerator, and
both ranks report nonzero timers for all four new KPP families. All eight
MPI runs use the four verified executable hashes. No MPI speedup is inferred
from these correctness runs.

Additional regressions:

- Host suite: **46/46** passed, including generator integer-layout/kind,
  concurrent private-buffer, restart and strict-build acceptance checks.
- Target stride guards: **21/21**, including integer row/column strides,
  padded columns, singleton axes and empty sections (`8941131`).
- Independent Fortran momentum oracle: 18 cases cover first/predictor/
  corrector updates, both old slots, actual counterexamples, nonuniform
  fields and untouched halos/time slots. Vectorized negative control fails
  all 18 (`8943178`); corrected SW64 and unchanged Serial pass all 18 exactly.
- Normal CMake test targets pass on Serial (`8943268`) and SW64 (`8943275`),
  including existing stencil/coupling tests and the real 64-CPE ID probe.
  Actual compile flags, not only caches, were inspected.

## Corrected-policy one-rank performance

Two computed steps, all output disabled, q_share, OMP_NUM_THREADS=1,
cache_size=128, one MPE/core group and 64 reserved CPEs. Median of three
independent Fortran/control/SW64 samples; Serial has one timing only.
Initialization, compilation and queueing are not speedup denominators.
The control is pre-LMD-offload SW64 using the **same corrected arithmetic
policy**, not the earlier numerically failing vectorized binary.

| Grid (Lm x Mm x N), DT | Fortran wall s | Control SW64 s | New SW64 s | vs Fortran | Incremental vs control |
|---|---:|---:|---:|---:|---:|
| 256 x 16 x 30, 150 | 2.386275 | 0.807269 | 0.591667 | 4.03x | 1.36x |
| 256 x 64 x 30, 30 | 9.845208 | 2.197513 | 1.117396 | 8.81x | 1.97x |

These are independently matched configurations, not fixed-duration scaling:
DT differs, while all builds within a row use identical input/physics.
They are not the old non-KPP BL case. The 64-row DT=150 calibration trial
failed even in pure Fortran (`8940551`); its log is retained. DT=30 was
established with a fresh stable Fortran trial (`8940836`) and then applied
to **every** build in the 64-row comparison, not only the candidate.

KPP region medians use the matched wall-profile clock and identical region
boundaries. They are not a disjoint decomposition of the whole step;
profiles may include initialization, and the timestep driver also executes
endpoint KPP calls. Whole-model ratios above use stepping wall only.

| Grid rows | Fortran KPP s | Control KPP s | New KPP s | vs Fortran | vs control |
|---|---:|---:|---:|---:|---:|
| 16 | 0.412 | 0.332 | 0.120 | 3.43x | 2.77x |
| 64 | 1.660 | 1.292 | 0.218 | 7.61x | 5.93x |

Raw whole-step samples (s):

- 16 rows: Fortran [2.385261, 2.389495, 2.386275]; control
  [0.810423, 0.806435, 0.807269]; SW64 [0.594025, 0.591566, 0.591667].
  Serial 2.887067 (one sample, not the speedup denominator).
- 64 rows: Fortran [9.850606, 9.845208, 9.842293]; control
  [2.207378, 2.197513, 2.196901]; SW64 [1.117396, 1.118344, 1.113940].
  Serial 11.580932 (one sample, not the speedup denominator).

Earlier r4 measurements in REMAINDER.md remain historical evidence of
those vectorized executables. They must not be presented as current-policy
performance without remeasurement. No unrelated dependency or other project
checkout is modified.

## Artifacts and retained diagnosis

All new target files are under the isolated source snapshot
`~/voyage/roms/work/lmd-r5-20261104/`, made from ce67571b plus these changes.
The original four-application matrix remains in its original workspace.

- `runs/kpp-strict-acceptance-r5-v1/`: completed one-rank matrix, **30 jobs**,
  raw samples, field logs, entry timers and per-run executable hashes.
- `runs/kpp-strict-mpi-acceptance-r5-v1/`: completed two-rank matrix, **8 jobs**.
  DT=150 reference/Serial/SW64/control-Serial: `8943502`, `8943503`,
  `8943505`, `8943508`; DT=30: `8943519`, `8943522`, `8943524`, `8943530`.
- `runs/kpp-strict-mpi-build-r5-v1/builds.json`: four verified MPI hashes.
- `runs/kpp-strict-simd-r5-v1/`: clean control/candidate probes (`8943195`,
  `8943240`) and maintained-source hashes; no tracing markers in accepted
  executables. Experimental generated-file substitutions and contraction
  flags were removed before clean rebuilding.
- `runs/kpp-first-divergence-r5-v1/`: binary call-boundary snapshots,
  `8942719` / `8942734`. Each traced run reproduces its own original 125
  fields exactly; no rounded checksum is used as numerical evidence.
- `runs/kpp-step2d-divergence-r5-v1/`: first six barotropic calls,
  `8942798` / `8942831`. Unassigned scratch and closed U-boundary RHS
  slots are excluded from valid-point diagnosis.
- `runs/kpp-momentum-replay-r5-v1/`: identical-input production kernel
  replay; default-vectorized failure `8943111`, no-vector exact `8943112`.
  Scalar CPE arithmetic replay: `8943098`.
- Stage-local/full-step2d-only attempts (`8943122`, `8943128`) are retained
  as **failed intermediate model fixes**, not accepted results.
- Original failure: `runs/kpp-r5-full-20261104/`. Earlier EOS (`8941439`),
  momentum-corrector (`8941468`), tracer (`8941607`), contraction-off
  (`8941776`) and forcing-group (`8941840`) substitutions did not isolate
  or remove the physical error; their outputs remain diagnostic evidence.

## Reproduce

Within the Sunway ROMS tree with pinned Kokkos and official BL header/input:

```sh
module load swgcc/1473 mpi/swuc
BUILD_JOBS=2 RUN_ROOT=runs/kpp-$(date -u +%Y%m%dT%H%M%S) \
  python3 ROMS/Cpp/tools/sw64/mixing_matrix.py --mpi
```

`--reuse-builds` verifies existing matching caches and actual production
compile commands before reuse. Do not reuse manually substituted diagnostic
builds: cache checks alone cannot prove generated-source identity. Use a
clean source snapshot/rebuild, preserve failed evidence, and force executable
relinking when changing kernels (the ROMS static-library link dependency has
an existing relink gotcha).

For the direct test, configure ROMS_KOKKOS_TESTS=ON and ROMS_FAST_MATH=OFF,
build `test_step2d_kokkos`, and submit with the same one-group bsub resources.
Direct CMake configuration also requires NF_CONFIG/NETCDF/NETCDF_INCDIR as
in the build script. Model jobs pass their input through both `bsub -i`
and the executable argument. Performance inputs must not write NetCDF.
