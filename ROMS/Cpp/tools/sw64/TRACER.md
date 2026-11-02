# step3d_t tracer migration

The canonical input is mixed-scheme: temperature U3/C4, salinity
HSIMT/HSIMT. This port must keep those input choices unchanged.

Enable explicitly (the default remains the step2d-only profile):

```sh
STEP3D_T=ON bash ROMS/Cpp/tools/sw64/build.sh serial
STEP3D_T=ON bash ROMS/Cpp/tools/sw64/build.sh sw64
```

The runtime guard selects the whole tile before any accelerated write.
The supported path performs one column launch, processing both tracers,
with persistent per-host-thread scratch sized from runtime N. Predictor
slot 3 is immutable. The original Fortran path remains the fallback for
unsupported schemes/physics/time-level aliasing. Fortran retains HSIMT
prologue exchanges and the complete boundary/nudging/diagnostic epilogue.
Required successful-run evidence: `step3d_t accelerated calls=` for every
computed step, and no fallback message. Selecting the feature in CMake
alone is not proof that the fast path ran.

`step3d_t module wall` measures the entire module call, including boundary
work and exchanges. `ROMS time stepping wall` measures all of ROMS_run.
Neither is a measure of the isolated C++ launch alone. Compare against
the saved step2d-only controls, not just against a newly slower Serial
implementation, when evaluating incremental whole-model benefit.

## Results — 2026-11-02 (upwelling, 41 x 80 x 16, two computed steps)

The mixed U3/C4 temperature and HSIMT/HSIMT salinity input was left
unchanged. Double output; one tile; `q_share`.

Target test `test_step3d_t_kokkos` (128 cases: N=4/7/16/33, nnew=1/2,
four closed/periodic edge combinations, signed/zero transports, nonzero
and zero Akt, seeded diagnostics, plus a dense pivoted solve of the
spline system):

| backend | PASS | FAIL | maximum tracer / diagnostic difference |
|---|---:|---:|---:|
| Kokkos Serial | 128 | 0 | 0 / 0 |
| Kokkos SW64 (64 CPEs) | 128 | 0 | 0 / 0 |

Model fields, strict lists (his: 10 fields; avg: 20 fields; dia: all 18
temperature and salinity hadv/xadv/yadv/vadv/hdiff/xdiff/ydiff/vdiff/rate):

| pair | his | avg | dia | worst normalized relative error |
|---|---:|---:|---:|---:|
| Fortran reference -> Kokkos Serial | 10/10 | 20/20 | 18/18 | 0 (exact) |
| Kokkos Serial -> Kokkos SW64 | 10/10 | 20/20 | 18/18 | 1.77e-15 |

`step3d_t accelerated calls=2`, no fallback line. Two honest limits of
this case: `nl_tnu2=0`, so the three horizontal-diffusion diagnostics are
identically zero here (the diffusion path is evidenced by the 128
differential cases, not by these fields), and `temp`/`salt` are bitwise
equal between Serial and SW64 because their ~1e-22 tendencies are below
double rounding against ~20 degrees C.

Fallback check: with temperature horizontal advection changed to `C4`
(runtime scheme the kernel does not implement), both builds print
`step3d_t: whole-tile Fortran fallback` once, print no accelerated line,
and agree on all 48 compared fields (10/10, 20/20, 18/18, exact). This
confirms the guard is all-or-nothing and that the original path is
unmodified.

## Module wall time versus the Fortran it replaces — 256 x 256 x 16

One tile, two computed steps, no NetCDF output, one core group per job.
`step3d_t module wall` includes the boundary work and exchanges.

| `step3d_t` per call | seconds |
|---|---:|
| Fortran (measured with the same timing in a STEP3D_T=OFF build) | 0.758 |
| Kokkos Serial | 1.913 |
| Kokkos SW64, 64 CPEs | 0.549 |

| whole `ROMS_run` | initialization | time stepping |
|---|---:|---:|
| step2d only, Serial | 2.661 | 25.377 |
| step2d only, SW64 | 2.664 | 12.017 |
| step2d + step3d_t, Serial | 2.660 | 27.941 |
| step2d + step3d_t, SW64 | 2.663 | 11.862 |
| Fortran step3d_t, Serial (timing build) | - | 25.125 |
| Fortran step3d_t, SW64 (timing build) | - | 12.270 |

The SW64 kernel is 3.48x faster than the Serial kernel but only **1.38x**
faster than the Fortran module it replaces, because the kernel re-implements
what the Fortran does: it is 2.5x slower than Fortran on the Serial backend.
Incremental whole-model effect is 12.270 -> 11.862 s (-3.3%) on SW64 and
25.125 -> 27.941 s (+11%) on Serial. A previous subtraction-based estimate
of 0.63 s for the Fortran module was wrong by 20%; these numbers are
measured directly.

Cause of the Serial regression: the original computes each face flux once
(`do i=Istr,Iend+1`) and stores it, while the per-column kernel recomputes
both of a column's faces, so neighboring columns duplicate every face.
The kernel therefore does roughly twice the horizontal and vertical flux
work, with a k-inner access order against a (i,j,k,l,m) layout. Removing
that duplication is the next optimization; it is not done here.

## Numerical acceptance criteria (set before field comparisons)

- Nonzero reference fields: max absolute difference / reference peak
  must not exceed `1e-10`. Report actual absolute and relative values.
- Identically zero tracer diagnostics: a separate absolute threshold
  of `1e-16` in the output variable's units (the diagnostics are rates).
  This must not loosen the relative threshold on nonzero fields.
- Existing barotropic zero-diagnostic threshold remains `1e-20`.
- NaN/Inf, missing required fields, record/shape/time mismatch, or silent
  fallback are failures. Do not change the advection input to pass.

Compare temp/salt and velocities/free surface in history and averages.
In diagnostics explicitly select both temperature and salinity's hadv,
xadv, yadv, vadv, hdiff, xdiff, ydiff, vdiff and rate fields; the general
comparator's older default list is not sufficient for this migration.

The direct target test `test_step3d_t_kokkos` uses copied original
Fortran loop bodies as an independent oracle. It varies N=4,7,16,33,
nnew=1/2, all combinations of closed/periodic axes, signed transports,
zero transports/diffusion, constant states, and seeded diagnostics. A
separate dense pivoted solve checks the spline system on a nonuniform
column without using the kernel's Thomas recurrence. The expected full
matrix has 128 cases per backend.

Keep all new runs in separate `runs/tracer-*` directories, retaining the
previous step2d-only binaries in `runs/tracer/control/`. Source and output
remain under `~/voyage/roms/`; use `q_share` and short two-step model runs.
