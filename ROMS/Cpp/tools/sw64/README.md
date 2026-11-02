# ROMS on Sunway

Run from the login host reached by `ssh SW`. All deployment files, build
products and outputs belong in `~/voyage/roms/`. Do not replace the separate
`~/voyage/kokkos` checkout or change the installed NetCDF libraries.

## Build

```sh
cd ~/voyage/roms
bash ROMS/Cpp/tools/sw64/build.sh reference
bash ROMS/Cpp/tools/sw64/build.sh serial
bash ROMS/Cpp/tools/sw64/build.sh sw64
```

The script loads `swgcc/1473` and `mpi/swuc`. It uses the pinned `Kokkos/`
submodule and target NetCDF from `$HOME/ftao/local-saca` (`NF_CONFIG` and
`NETCDF_INCDIR` may override this). The platform flag `SW64=TRUE` permits
this fork's GCC 7.1 toolchain even for the Serial control; only the SW64
build enables `Kokkos_ENABLE_SW64` and hybrid kernel compilation.

`MPI=OFF` is the default. The Fortran link still uses `mpif90`, because
the available NetCDF/HDF5 archives require the MPI runtime. This does not
turn on distributed ROMS or offload the Serial build. All compared builds
use double output and `ROMS_FAST_MATH=OFF`. The mixed Fortran/C++ link also supplies host/slave C++ runtimes after
the Kokkos archives. Both Kokkos builds explicitly
enable FUSED, TAVG, COUP, MOMT and STOR; depth/flux and free-surface kernels
are enabled directly by `ROMS_KOKKOS`. The tested physics is the diagnostic
3-D upwelling combination. Unsupported physics fails compilation.

## Direct target regressions

Submit `build/serial/test_step2d_kokkos` and
`build/sw64/test_step2d_kokkos` using the same `bsub` settings below. The
first test verifies MPE-only execution; the second requires observed CPE
IDs 0 through 63. Then it compares fused versus staged stencils on
asymmetric fields, including closed/periodic edges and all three time
levels, and verifies storage of stress diagnostics after M2pgrd.

## Two-step end-to-end run

```sh
for backend in reference serial sw64; do
  python3 ROMS/Cpp/tools/sw64/make_case.py runs/smoke/$backend
  bash ROMS/Cpp/tools/sw64/run.sh $backend smoke
done
```

Default submission is `q_share`, one MPE/core group and 64 reserved CPEs,
with bounded runtime. Override `QUEUE`, `SHARE_SIZE` or `TIMELIMIT` only
when appropriate for the allocation. Serial reserves matching resources
but never launches CPE work. The input generator refuses to overwrite an
existing input; the launcher refuses to replace an existing run log.
Use a new case name to preserve earlier results.

Inspect output on the x86 login node, **not** with a target ncdump:

```sh
export PATH=/usr/sw/yyzlib/local_x86/bin:$PATH
export LD_LIBRARY_PATH=/usr/sw/yyzlib/local_x86/lib:${LD_LIBRARY_PATH:-}
python3 ROMS/Cpp/tools/nccmp.py \
  runs/smoke/reference/roms_his.nc runs/smoke/serial/roms_his.nc \
  --vars zeta,ubar,vbar,u,v --rtol 1e-10
python3 ROMS/Cpp/tools/nccmp.py \
  runs/smoke/serial/roms_his.nc runs/smoke/sw64/roms_his.nc \
  --vars zeta,ubar,vbar,u,v --rtol 1e-10
```

Also compare `roms_dia.nc` and `roms_avg.nc`. For identically zero diagnostic
reference fields, use a separate `--zero-tol 1e-20` absolute threshold;
nonzero fields still use the `1e-10` normalized relative threshold. Missing required variables,
nonfinite/fill data and mismatched shapes/record times must fail. Matching
rounded KE in stdout is not array-level or bitwise validation.

## Short performance run

Generate a separate case with `--size 256 --steps 2 --no-output` for each
backend. Compare `ROMS time stepping wall [rank=...]` values, measured by
`SYSTEM_CLOCK`; report initialization separately. The outer `time bsub`
value includes queueing, while legacy ROMS `Total` uses CPU time and sums
ranks under MPI. Neither is the stepping-wall denominator for speedup.

Optional MPI: build with `MPI=ON`; select identical `RANKS=6` and a
`2 x 3` tile decomposition for Serial and SW64. Six ranks occupy six MPEs
and six groups (384 CPEs for SW64), not an x86-style thread pool. Do not
compare six-rank SW64 against one-rank Serial as an equal-rank control.
