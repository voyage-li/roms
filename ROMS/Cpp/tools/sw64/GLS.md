# Iteration-time GLS turbulence closure — verified, 2026-11-04

## Scope and invariant controls

`KK_STAGE_GLS` adds opt-in Kokkos row offload for `gls_prestep_tile` and
`gls_corstep_tile`. This round does not add initialization ports or change
the default physics. The tested configuration is the Upwelling application
with `GLS_MIXING` enabled externally; its header then selects
`KANTHA_CLAYSON`/`N2S2_HORAVG`/`RI_SPLINES` support and the standard
k-epsilon parameters in `ROMS/External/roms_upwelling.in`.

The generator lowers independent outer-j loops. Vertical recurrences remain
sequential in k, and the original expression ordering is retained. The
outer-k horizontal-advection loops, shear/buoyancy smoothing phases outside
outer-j blocks, boundary routines, MPI exchanges, global reductions and I/O
stay in Fortran. The generated paths are synchronous. Unsupported source
shapes fail closed or fall back to the retained original loops; the accepted
runs below had no row fallback.

## Generator change

GLS uses grid-indexed parameter arrays such as `gls_p(ng)` and
`Akt_bak(itrc,ng)`. The parameters are captured as Fortran-evaluated scalars
at the call boundary. `Akt_bak(itrc,ng)`, where `itrc` is a tracer loop
variable, is materialized as a small TARGET vector
`kk_tracer_akt_bak(1:NT(ng))` and passed as a rank-one array; using a
two-index host pointer with an assumed second stride would be wrong. The
same mechanism is available for other tracer-indexed parameter arrays.

The actual target-preprocessed sources generate **8 prestep** and **19
corstep** row entry points. Host-only generator tests cover both the
tracer-indexed materialization and the constant-index scalar path.

## Numerical validation

Four computed steps, 41 x 80 x 16, all history, average, diagnostic and
restart records. `Hout(idMtke)` and `Hout(idMtls)` are enabled for
validation only so the GLS prognostic fields are compared directly. Both
DT and physics are identical across builds.

- **Serial candidate vs pure Fortran: 114/114 fields exact.**
- **SW64 candidate vs pure Fortran: 114/114 fields within the unchanged
  normalized gates** (1e-10 nonzero, 1e-20 zero-reference).
- `tke`, `gls`, `AKv` and `AKt` are **exactly equal** on both Serial and
  SW64 in every compared record.
- Both GLS entry timers are nonzero (prestep 304 calls, corstep 868 calls
  in the four-step run); execution space is SW64; no `Kokkos row fallback`.
- The restart file compares 11 fields, including `AKv`/`AKt`; the history
  file compares 12 fields, including `tke`/`gls`.

Jobs: reference `8944013`, Serial `8944018`, SW64 `8944026`.
Inputs/outputs under `runs/gls-rst2/` and `runs/gls-rst/`.

## Two-rank MPI numerical validation

Two MPI ranks/core groups, `NtileI=2`, `NtileJ=1`, four computed steps at
41 x 80 x 16, with the same physics and complete output settings as above.
All average, history, diagnostic and restart records were compared directly.
The candidate/control builds differ only by `KK_STAGE_GLS`; executable hashes
were checked before and after every run.

- **Serial candidate vs matched Serial control: 114/114 fields exact.**
- **SW64 candidate vs original Fortran: 114/114 fields exact** (maximum
  absolute difference 0); these also satisfy the unchanged normalized gates.
- The compared files contain 28 average, 63 diagnostic, 12 history and 11
  restart fields. Direct `tke`/`gls` history and `AKv`/`AKt` restart fields
  are all exact; no field, record, file or diagnostic exception was skipped.
- Both MPI processes initialized the expected execution space and printed a
  timer table. Each candidate has two `gls_prestep` entries with 304 calls
  each and two `gls_corstep` entries with 740 calls each. The Serial control
  has no GLS entry. Neither candidate reports a row fallback.
- MPI stdout interleaves timer-table lines, so contiguous text blocks must
  not be assigned to ranks. Acceptance instead requires two tables and two
  nonzero entries for each GLS phase, together with both rank-specific ROMS
  stepping-wall lines.

| Build | Job | SHA-256 |
|---|---:|---|
| original Fortran | `8944672` | `2954cd998b5247c77b8f842437d2faf661880162c13f1559a076a7c2df9e6293` |
| matched Serial control | `8944677` | `deba3e65eca3d1672fe8e575abd9e25cb64f6ab3d315ebfa178970a5a4275933` |
| Serial candidate | `8944722` | `5df409dc734b5038abd16a1c662b30f30e8838fb67a398b0beb7853aac009e05` |
| SW64 candidate | `8944734` | `380f83bc58ea77749aad90d5409df92f24c4df5f46f7f107c18996baba150925` |

Final artifacts and per-field logs are under `runs/gls-mpi/validation-v3/`.
Earlier checker-only failures are retained in `validation-v1/` and
`validation-v2/`; they are not accepted numerical results. These correctness
runs include output and are not used for MPI speedup claims.

## Corrected-policy performance

Two computed steps, all output disabled, one rank/core group, q_share,
OMP_NUM_THREADS=1, cache_size=128, 256 x 256 x 16. Three independent samples
for each build; medians shown. All SW64 builds use the corrected
`ROMS_FAST_MATH=OFF` policy, including `-fno-tree-vectorize` for ROMS C++
computation units. The control is the same physics with `KK_STAGE_GLS`
absent, so only the GLS offload differs.

| Build | whole stepping median s | GLS region median s |
|---|---:|---:|
| pure Fortran | 28.022116 | 3.402 |
| matched SW64 control | 6.803710 | 3.401 |
| new SW64 candidate | **3.739079** | **0.333** |

- Whole step vs original Fortran: **7.49x**.
- Incremental vs matched pre-port SW64 control: **1.82x**.
- GLS module vs Fortran/control: **10.2x**.
- Fortran samples 27.963572 / 28.022116 / 28.025121; control
  6.803029 / 6.803710 / 6.807350; candidate
  3.741738 / 3.737765 / 3.739079.
- GLS samples: Fortran 3.397 / 3.402 / 3.402; control
  3.401 / 3.402 / 3.400; candidate 0.335 / 0.333 / 0.333.

At 41 x 80 x 16 the GLS region falls from 0.308 s (Fortran) to 0.075 s
(SW64); the full-run walls there are output-dominated and are not used as
speedup denominators. The 256 x 256 two-step no-output runs are the
performance evidence.

The GLS region is a module-region counter, not a disjoint decomposition of
the whole step; initialization, compilation and queueing are excluded from
speedup denominators. Earlier vectorized-policy measurements are historical
and must not be mixed with these corrected-policy numbers.

## Two-rank MPI performance

The same 256 x 256 x 16 grid was run with two MPI ranks/core groups
(`NtileI=2`, `NtileJ=1`), two computed steps, all NetCDF output disabled,
q_share, `OMP_NUM_THREADS=1`, and three independent samples for every build.
The control is the same matched SW64 build without `KK_STAGE_GLS`; all three
executables use the strict arithmetic policy. Rank 0 and rank 1 stepping
walls agree within 0.00037 s in every sample.

| Build | whole stepping median s | GLS region median s |
|---|---:|---:|
| pure Fortran | 14.244377 | 3.396 |
| matched SW64 control | 3.599448 | 3.404 |
| new SW64 candidate | **2.100678** | **0.406** |

- Whole step vs original Fortran: **6.78x**.
- Incremental vs matched pre-port SW64 control: **1.71x**.
- GLS module vs Fortran/control: about **8.4x**.
- Fortran samples 14.239544 / 14.244377 / 14.247598; control
  3.602332 / 3.590684 / 3.599448; candidate
  2.103399 / 2.093068 / 2.100678.
- GLS region samples: Fortran 3.402 / 3.387 / 3.396; control
  3.402 / 3.405 / 3.404; candidate 0.406 / 0.406 / 0.406.
- Jobs: Fortran `8944829`, `8944838`, `8944848`; control `8944864`,
  `8944872`, `8944880`; candidate `8944883`, `8944892`, `8944898`.
- The performance control SHA-256 is
  `b7efaaeaebd151eff6d69481e9895778f6caa03b8ac6e53e522028067f7e45b7`;
  the Fortran and candidate hashes are the validation-v3 hashes above.

No-output runs were checked to contain no `roms_*.nc` files. Both MPI
processes reported SW64, the candidate executed nonzero GLS prestep and
corstep entries in both processes, and the control executed none. Correctness
runs above are deliberately excluded from these speedup denominators.

## Coverage of the GLS region

At 256 x 256, the generated entry timers account for **96.5%** of the GLS
region (0.3234 s of 0.335 s; the other two samples are 0.3215/0.333 and
0.3214/0.333). The remaining ~0.0115 s per run is outer-k horizontal
advection, boundary fills and launch/guard overhead, about 0.3% of the whole
stepping time. Porting those loops would not be a meaningful performance
target; the measured evidence is recorded rather than treating the residual
as an unaddressed hotspot.

## Limitations

- Only the tested classic-GLS / upwelling configuration is claimed. CANUTO,
  Craig-Banner, wave-mixing, masked, nesting and other optional branches are
  not covered; they must fail closed or be validated separately.
- Outer-k horizontal advection and boundary/smoothing phases remain on the
  management core. Further gains require separate kernels for those phases.
- The row kernel inherits the strict arithmetic policy and uses the pinned
  Kokkos revision; no dependency was changed.
- These changes are **uncommitted** at the time of writing. The earlier
  KPP/remainder commit `f1dde43f` is independent.

## Reproduce

Within the isolated Sunway snapshot with the pinned Kokkos tree:

```sh
module load swgcc/1473 mpi/swuc
# reference
BACKENDS=reference TAG_SUFFIX=-gls-r6 MPI=OFF \
  bash ROMS/Cpp/tools/sw64/build_app.sh UPWELLING \
  'GLS_MIXING;ROMS_WALL_PROFILE' ROMS/Include
# candidates
STAGES='KK_STAGE_FUSED;KK_STAGE_TAVG;KK_STAGE_COUP;KK_STAGE_MOMT;KK_STAGE_DIAG;KK_STAGE_STOR;KK_STAGE_UV3DMIX;KK_STAGE_T3DMIX;KK_STAGE_PRSGRD;KK_STAGE_OMEGA;KK_STAGE_SETZETA;KK_STAGE_PRE3D;KK_STAGE_STEP3DUV;KK_STAGE_GLS'
BACKENDS=serial TAG_SUFFIX=-gls-r6 KK_STAGES="$STAGES" MPI=OFF \
  bash ROMS/Cpp/tools/sw64/build_app.sh UPWELLING \
  'GLS_MIXING;ROMS_WALL_PROFILE' ROMS/Include
# remove ;KK_STAGE_GLS for the matched control
```

Model launch still requires both `bsub -i INPUT` and the executable input
argument. Validation inputs enable `Hout(idMtke)`/`Hout(idMtls)` and
`NRST=1`; performance inputs disable all output. Use new run directories and
never overwrite retained evidence.
