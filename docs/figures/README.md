# Stage-2 speed-up figure

Files:

- `kokkos_stage2_speedup.pdf` - vector version for the paper;
- `kokkos_stage2_speedup.png` - 300 dpi preview;
- `stage2_speedups.csv` - the 25 plotted points;
- `plot_stage2.py` - reproducible renderer.

Suggested caption:

> **Figure 1.** Per-step speed-up of the Kokkos/SW64 port over the original
> Fortran build on 12 MPI ranks (two nodes, six ranks per node). Each
> application uses five horizontal grids; the largest grid takes 66-77 s
> per step in the original Fortran reference. Solid filled markers show
> the final PRE3D+REMAINDER build; open markers show the PRE3D-only build.
> Colours and marker shapes identify the application; the right-hand
> summary gives the geometric-mean speed-up over the five grids (the
> PRE3D-only value and the final value). All timings are medians of three
> independent runs with output disabled; the largest-grid medians are
> 11.2x (UPWELLING), 17.6x (KELVIN), 11.4x (CANYON), 10.5x (BL_TEST) and
> 11.6x (CHANNEL).

The five-slot categorical palette (`#2a78d6`, `#eb6834`, `#1baf7a`,
`#eda100`, `#e87ba4`) passes the adjacent-pair CVD and normal-vision gates
on the `#fcfcfb` surface; the summary table provides the visible-label
relief required for the three sub-3:1 slots. Application identity is also
encoded by marker shape, and the two stages by line style and marker fill,
so the figure remains readable without colour.
