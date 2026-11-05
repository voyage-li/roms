#!/usr/bin/env python3
"""Paper figure: 12-rank Kokkos SW64 speed-up, PRE3D vs +REMAINDER.

Reads docs/figures/stage2_speedups.csv and writes a vector PDF and a PNG
preview.  Palette is the validated five-slot categorical set from the
dataviz reference; app identity uses both colour and marker shape, stage
uses line style and marker fill.
"""
import csv
import math
from pathlib import Path

import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib.lines import Line2D
from matplotlib.ticker import FixedLocator, FixedFormatter

HERE = Path(__file__).resolve().parent
SURFACE = '#fcfcfb'
INK = '#0b0b0b'
SECONDARY = '#52514e'
MUTED = '#898781'
GRID = '#e1e0d9'
AXIS = '#c3c2b7'
COLORS = {
    'upwelling': '#2a78d6',
    'kelvin': '#eb6834',
    'canyon': '#1baf7a',
    'bl_test': '#eda100',
    'channel': '#e87ba4',
}
MARKERS = {
    'upwelling': 'o',
    'kelvin': 's',
    'canyon': '^',
    'bl_test': 'D',
    'channel': 'v',
}
LABELS = {
    'upwelling': 'UPWELLING',
    'kelvin': 'KELVIN',
    'canyon': 'CANYON',
    'bl_test': 'BL_TEST',
    'channel': 'CHANNEL',
}
ORDER = ['upwelling', 'kelvin', 'canyon', 'bl_test', 'channel']


def load():
    rows = list(csv.DictReader((HERE / 'stage2_speedups.csv').open()))
    data = {name: [] for name in ORDER}
    for row in rows:
        data[row['app']].append({
            'points': int(row['Lm']) * int(row['Mm']),
            'pre3d': float(row['speedup_pre3d']),
            'rem': float(row['speedup_remainder']),
        })
    for name in data:
        data[name].sort(key=lambda x: x['points'])
    return data


def geomean(values):
    return math.exp(sum(math.log(x) for x in values) / len(values))


def main():
    data = load()
    plt.rcParams.update({
        'font.family': 'DejaVu Sans',
        'font.size': 8.4,
        'axes.labelsize': 9.0,
        'xtick.labelsize': 8.1,
        'ytick.labelsize': 8.1,
        'pdf.fonttype': 42,
        'ps.fonttype': 42,
        'axes.unicode_minus': False,
    })
    fig = plt.figure(figsize=(7.2, 4.3), dpi=200)
    fig.patch.set_facecolor(SURFACE)
    gs = fig.add_gridspec(1, 2, width_ratios=[3.55, 1.42],
                          left=0.078, right=0.985, bottom=0.145, top=0.855,
                          wspace=0.07)
    ax = fig.add_subplot(gs[0, 0])
    axt = fig.add_subplot(gs[0, 1])
    ax.set_facecolor(SURFACE)
    axt.set_facecolor(SURFACE)
    axt.set_xlim(0, 1)
    axt.set_ylim(0, 1)
    axt.axis('off')

    fig.text(0.078, 0.955,
             'Kokkos SW64 speed-up vs original Fortran on 12 MPI ranks',
             ha='left', va='top', fontsize=10.6, color=INK,
             fontweight='semibold')
    fig.text(0.078, 0.905,
             'two computed steps, five grids per application, median of three runs; '
             'largest reference 66-77 s/step',
             ha='left', va='top', fontsize=8.2, color=SECONDARY)

    for name in ORDER:
        rows = data[name]
        color = COLORS[name]
        marker = MARKERS[name]
        x = [row['points'] for row in rows]
        pre = [row['pre3d'] for row in rows]
        rem = [row['rem'] for row in rows]
        ax.vlines(x, pre, rem, color=color, linewidth=0.8, alpha=0.55,
                  linestyles=':', zorder=1)
        ax.plot(x, pre, color=color, linestyle='',
                marker=marker, markersize=4.4, markerfacecolor=SURFACE,
                markeredgecolor=color, markeredgewidth=0.9,
                zorder=2)
        ax.plot(x, rem, color=color, linewidth=1.9, linestyle='-',
                marker=marker, markersize=6.0, markerfacecolor=color,
                markeredgecolor=color, markeredgewidth=0.9,
                zorder=3, solid_capstyle='round')

    ax.set_xscale('log')
    ax.set_xlim(2.3e4, 1.15e7)
    ax.set_ylim(0, 22.5)
    ax.xaxis.set_major_locator(FixedLocator([3e4, 1e5, 1e6, 1e7]))
    ax.xaxis.set_major_formatter(FixedFormatter(['30k', '100k', '1M', '10M']))
    ax.xaxis.set_minor_locator(FixedLocator([]))
    ax.yaxis.set_major_locator(FixedLocator([0, 5, 10, 15, 20]))
    ax.grid(True, which='major', color=GRID, linewidth=0.55, zorder=0)
    ax.set_axisbelow(True)
    for side in ('top', 'right'):
        ax.spines[side].set_visible(False)
    for side in ('left', 'bottom'):
        ax.spines[side].set_color(AXIS)
        ax.spines[side].set_linewidth(0.7)
    ax.tick_params(axis='both', colors=SECONDARY, width=0.6,
                   length=3.0, pad=3.0)
    ax.set_xlabel('horizontal grid points ($L_m \\times M_m$, log scale)',
                  color=INK, labelpad=5)
    ax.set_ylabel('per-step speed-up vs original Fortran', color=INK,
                  labelpad=6)

    stage_handles = [
        Line2D([0], [0], color=SECONDARY, linewidth=0,
               linestyle='', marker='o', markersize=4.8,
               markerfacecolor=SURFACE, markeredgecolor=SECONDARY,
               markeredgewidth=1.0, label='PRE3D'),
        Line2D([0], [0], color=SECONDARY, linewidth=1.9, linestyle='-',
               marker='o', markersize=5.6, markerfacecolor=SECONDARY,
               markeredgecolor=SECONDARY, label='+ REMAINDER'),
    ]
    legend = ax.legend(handles=stage_handles, loc='lower right',
                       frameon=True, fontsize=8.1, handlelength=2.3,
                       borderpad=0.55, labelspacing=0.35)
    legend.get_frame().set_facecolor(SURFACE)
    legend.get_frame().set_edgecolor(GRID)
    legend.get_frame().set_linewidth(0.6)
    for text in legend.get_texts():
        text.set_color(SECONDARY)

    # Right-hand summary table (also the accessible table view).
    axt.text(0.02, 0.975, 'geomean speed-up', fontsize=8.6,
             color=INK, fontweight='semibold', va='top')
    axt.text(0.02, 0.907, 'application', fontsize=7.2, color=MUTED,
             va='center')
    axt.text(0.74, 0.907, 'PRE3D', fontsize=7.2, color=MUTED,
             ha='right', va='center')
    axt.text(0.99, 0.907, '+REM', fontsize=7.2, color=MUTED,
             ha='right', va='center')
    axt.plot([0.02, 0.99], [0.878, 0.878], color=GRID, linewidth=0.6,
             transform=axt.transAxes, clip_on=False)

    y = 0.825
    for name in ORDER:
        values = [row['pre3d'] for row in data[name]]
        rem = [row['rem'] for row in data[name]]
        axt.plot([0.045], [y], marker=MARKERS[name], markersize=5.2,
                 color=COLORS[name], markeredgecolor=COLORS[name],
                 linestyle='', transform=axt.transAxes, clip_on=False)
        axt.text(0.11, y, LABELS[name], fontsize=7.8, color=INK,
                 va='center')
        axt.text(0.74, y, '%.1f\u00d7' % geomean(values), fontsize=7.8,
                 color=SECONDARY, ha='right', va='center')
        axt.text(0.99, y, '%.1f\u00d7' % geomean(rem), fontsize=7.8,
                 color=INK, ha='right', va='center')
        y -= 0.103

    axt.plot([0.02, 0.99], [y + 0.045, y + 0.045], color=GRID,
             linewidth=0.6, transform=axt.transAxes, clip_on=False)
    all_pre = [row['pre3d'] for name in ORDER for row in data[name]]
    all_rem = [row['rem'] for name in ORDER for row in data[name]]
    axt.text(0.11, y, 'all 25 points', fontsize=7.8, color=INK,
             va='center', fontweight='semibold')
    axt.text(0.74, y, '%.1f\u00d7' % geomean(all_pre), fontsize=7.8,
             color=SECONDARY, ha='right', va='center', fontweight='semibold')
    axt.text(0.99, y, '%.1f\u00d7' % geomean(all_rem), fontsize=7.8,
             color=INK, ha='right', va='center', fontweight='semibold')

    fig.savefig(HERE / 'kokkos_stage2_speedup.pdf', facecolor=SURFACE)
    fig.savefig(HERE / 'kokkos_stage2_speedup.png', facecolor=SURFACE,
                dpi=300)
    print('wrote', HERE / 'kokkos_stage2_speedup.pdf')
    print('wrote', HERE / 'kokkos_stage2_speedup.png')


if __name__ == '__main__':
    main()
