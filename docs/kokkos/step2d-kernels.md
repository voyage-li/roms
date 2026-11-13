# step2d：完整原始 Fortran 与当前 Kokkos kernel 对照

<a id="entry-compute-modes"></a>

## 入口计算模式速查

**pointwise（逐点）**指同一空间点上的代数运算或时间历史更新；**stencil（邻点模板）**指读取相邻空间点。`+` 表示同一入口包含多种模式；逐点的时间加权累计不是空间全局归约。

| 入口 | 主要计算模式 | 依赖范围 / 方向 | 简短说明 |
|---|---|---|---|
| [`k_step2d_depth_flux`](#depth-flux) | pointwise + 一阶邻距 stencil | rho 本点与西邻（U）/南邻（V） | `zeta+h` 得到水深，再插值形成面输运 `DUon/DVom`。 |
| [`k_step2d_time_average`](#time-average) | pointwise | 同点跨快时间步，无空间邻点读取 | 清零、缩放并按时间权重累计自由表面与输运。 |
| [`k_step2d_zeta_step`](#zeta-step) | 水平通量散度 stencil + pointwise | rho 点四周的 U/V 面输运；状态与时间历史同点 | 由连续方程 RHS 推进自由表面，并生成水深、密度相关辅助量。 |
| [`k_step2d_rhs_fused`](#rhs-fused) | 多场交错网格稀疏复合 stencil | rho/U/V/psi 多场；最宽约二阶邻距 | 融合压力、四阶平流、科氏及谐波黏性等；不是稠密 5×5 模板。 |
| [`k_step2d_coupling`](#coupling) | pointwise | 同点跨慢时间槽，无垂向遍历 | 用已给定的三维积分强迫校正二维 RHS，按时间历史外推或复用强迫。 |
| [`k_step2d_momentum_step`](#momentum-step) | 一阶邻距 stencil + pointwise | 水深/度量读西邻或南邻；速度与 RHS 历史同点 | 插值旧/新水深，再逐点作速度时间推进。 |
| [`k_step2d_diag_int`](#diag-int) | pointwise 时间积分 + 度量 stencil | 诊断状态同点；`pm` 读西邻、`pn` 读南邻 | 时间积分并累计诊断量；系数读取邻点，非严格 radius-0。 |
| [`k_step2d_store_rhs`](#store-rhs) | pointwise 拷贝 | 同点当前 RHS → 指定历史槽，无空间邻点读取 | 保存主 RHS 与诊断 RHS，供后续 corrector 使用。 |

---

- **生成日期**：2026-11-14。
- **当前分支**：`feat/kokkos`。
- **当前源码提交**：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`。
- **原始 Fortran 提交**：`cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`，文件 `ROMS/Nonlinear/step2d_LF_AM3.h`，共 **3058 行**。
- **历史关系**：上述原始提交是首次 step2d Kokkos 移植提交 `a0c9c795600760c2f7aa8dbf9032d72624d64ac5` 的唯一父提交。
- **本文中的“优化后”**：只指当前提交中已经接入的八个计算入口，不把 `feat/kokkos.share` 的后续实验算进来。

本文是完整代码对照和计算讲解，不是伪代码、迁移建议书或性能承诺。每个入口都按“**完整原始计算段 → 完整当前 C++ 函数 → 解释**”排列，保留原有数值公式、U/V 两方向、启动分支、时间历史、诊断和条件编译。英文源码注释原样保留；正文说明以实际条件和算式为准，个别旧注释的不准确之处另行指出。

八段原始 Fortran 都是 `step2d_tile` 内的源码片段，不是独立子程序；片段外的 CPP 上下文会逐段说明。[附录 A](#appendix-original)另收录原始文件全文，保留声明、通信、边界及未移植物理的确切上下文。当前 `kokkos_step2d.cpp` 则用正文八段与[附录 B](#appendix-cpp)的前后文**无缺行地分块收录**，不再重复粘贴整份实现。

所有源码块均机械按行提取，没有人工重新排版或改写算式。文末[来源清单](#source-manifest)给出每块 ID、提交、文件、闭区间行号、行数和 SHA256。SHA256 计算对象为代码围栏内的原始 UTF-8 字节，包含每行原有空白和末尾换行，不含 Markdown 标记。源码注释中原本出现的 `...` 也保留，它们不是本文添加的代码占位符。

## 目录

0. [入口计算模式速查](#entry-compute-modes)
1. [整体流程、八入口总表](#flow)
2. [变量、交错网格与时间层](#variables)
3. [共同的并行、内存与适用条件](#common)
4. [depth_flux：总水深与体积输运](#depth-flux)
5. [time_average：快时间平均](#time-average)
6. [zeta_step：自由表面推进](#zeta-step)
7. [rhs_fused：二维动量右端融合](#rhs-fused)
8. [coupling：二维与三维耦合](#coupling)
9. [momentum_step：二维速度推进](#momentum-step)
10. [diag_int：动量诊断积分](#diag-int)
11. [store_rhs：保存 predictor RHS 历史](#store-rhs)
12. [附录 A：完整原始 Fortran](#appendix-original)
13. [附录 B：当前 C++ 共用 helper、文件重组与结束部分](#appendix-cpp)
14. [附录 C：数组、执行空间、生命周期与计时](#appendix-config)
15. [附录 D：完整 C++ 声明、BIND(C) 接口及当前调用端](#appendix-abi)
16. [当前实现、未合入实验与历史验证的边界](#status-validation)
17. [来源清单与文本完整性检查](#source-manifest)

<a id="flow"></a>

## 1. 整体流程、八入口总表

### 1.1 真实执行顺序

这里的顺序来自当前 `ROMS/Nonlinear/step2d_LF_AM3.h` 的调用点，不是 C++ 文件的函数定义顺序。

> `depth_flux` → 通量通信与开边界处理 → `time_average` → 辅助步判断与返回 → `zeta_step` → 点源、自由表面边界与通信 → `rhs_fused` → 其他仍在 Fortran 的动量项 → `coupling` → `momentum_step` → `diag_int`（仅 corrector）→ `store_rhs`（仅 predictor）→ 速度边界、点源与通信。

省略在这条流程文字中的外围工作，并未从模型移除；完整原始上下文见附录 A。支持配置中不活跃的湿干、波流、双调和黏性等分支仍须与“已经移植到 C++”区别开。

**`time_average` 在本次自由表面更新之前。** 非首个 predictor 采样的是前一个 corrector 已完成、更新边界并同步的状态；corrector 继续累加第二套输运平均。不能把它解释为“先得到本次新 zeta，再立即对它平均”。

### 1.2 快步、慢步和辅助步

- `ndtfast` 是慢快步分裂比，`dtfast = dt / REAL(ndtfast,r8)`；依据 `ROMS/Utility/inp_par.F:629`。
- `nfast` 是完成以慢时间层 `n+1` 为中心的正压滤波平均所需的**实际**快步数，`ROMS/Utility/set_weights.F` 按滤波支持域确定它；不能假定 `nfast == ndtfast`。定义见 `ROMS/Modules/mod_scalars.F:204–216`。
- 在当前要求的 `SOLVE3D` 配置下，`FIRST_2D_STEP` 是 `iif(ng)==1`，即**每轮快时间积分的首个快步**，不是整个模拟只有一次的首步。纯二维时它才是 `iic==ntfirst`；真实宏见附录 D。
- `ROMS/Nonlinear/main3d.F:810–889` 循环至 `nfast+1`。最后的辅助步只调用 predictor，没有对应 corrector；它仍生成 depth/flux、完成平均及必要通信，随后当前 `step2d_LF_AM3.h:1002` 的 `IF (iif(ng).gt.nfast(ng)) RETURN` 提前返回。原始版本相同判断位于第 883 行。
- 因而辅助步只可能进入八入口中的前两个，后六个不执行。这不是“多推进一个物理快步”。

### 1.3 总表

下表中原始行号属于 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` 的 `ROMS/Nonlinear/step2d_LF_AM3.h`；C++ 行号属于当前提交的 `ROMS/Cpp/kokkos_step2d.cpp`。所有行号均为 1 起、包含首尾。当前调用行号属于当前 `ROMS/Nonlinear/step2d_LF_AM3.h`。表中的阶段宏均还要求 `ROMS_KOKKOS`。

| 顺序 | 入口（完整名字均为 `k_step2d_` 加下列后缀） | 原始 Fortran 完整计算段 | 当前 C++ 完整函数 | 当前 Fortran 调用 | 额外条件与阶段宏 | 静态分支中的 launch 点数 / 每次实际执行 |
|---:|---|---:|---:|---:|---|---:|
| 1 | `depth_flux` | 609–703 | 258–319 | 682–689 | `KK_STAGE_FUSED` | 1 / 1 |
| 2 | `time_average` | 738–810 | 326–408 | 848–858 | `SOLVE3D`，`KK_STAGE_TAVG` | 3 / 1 |
| 3 | `zeta_step` | 885–1019 | 420–571 | 1024–1038 | `KK_STAGE_FUSED` | 3 / 1 |
| 4 | `rhs_fused` | 1079–1652 | 1170–1506 | 1244–1264 | `KK_STAGE_FUSED` | 1 / 1 |
| 5 | `coupling` | 2223–2460 | 585–806 | 2457–2468 | `SOLVE3D`，`KK_STAGE_COUP` | 4 / 1 |
| 6 | `momentum_step` | 2487–2674 | 819–975 | 2729–2740 | `KK_STAGE_MOMT` | 3 / 1 |
| 7 | `diag_int` | 2700–2754 | 1000–1080 | 2974–2983 | `SOLVE3D && DIAGNOSTICS_UV`，`KK_STAGE_DIAG`，仅 corrector | 2 / 1 |
| 8 | `store_rhs` | 2836–2865 | 1090–1150 | 3121–3128 | `KK_STAGE_STOR`，仅 predictor | 1 / 1 |

`KK_STAGE_FUSED` 同时控制 depth、zeta 和 RHS 三个入口，不能把它误认为只控制 `rhs_fused`。当前文件共有 **18 个静态 `Kokkos::parallel_for` 调用点**，但同一个入口中的分支互斥，正常被调用时实际只有一次 launch。八阶段全开时，普通 predictor 和 corrector 各执行七个入口；二者并非都执行八个。

<a id="variables"></a>

## 2. 变量、交错网格与时间层

### 2.1 物理量与存储角色

ROMS 使用水平交错网格：`rho` 点放自由表面、水深和部分度量；`U`、`V` 点分别放两个方向速度；`psi` 点用于角点量及部分黏性通量。不同场的邻点关系不同，不能把所有输入都当成同一张标量网格。

| 变量 | 本文采用的含义与单位 | 主要用途 |
|---|---|---|
| `zeta` | 自由表面高度，m | 快时间层状态 |
| `ubar/vbar` | 深度平均 U/V 速度，m/s | 不是体积输运本身 |
| `h` | `rho` 点静水地形深度，m | 与 zeta 相加得到总水深 |
| `Drhs` | `zeta(:,:,krhs)+h` | RHS 时间层水深，tile-private scratch |
| `Dstp` | `zeta(:,:,kstp)+h` | 推进基准时间层水深，tile-private scratch |
| `Dnew` | `zeta_new+h` | 本次输出状态的水深，tile-private scratch |
| `pm/pn` | XI/ETA 方向逆网格尺度，1/m | 面积及度量转换 |
| `on_u/om_v` | U 点 ETA 宽度 / V 点 XI 宽度，m | 速度乘水深再乘横向宽度形成输运 |
| `DUon/DVom` | 面上的垂向积分体积输运，m³/s | 连续方程、对流与时间平均 |
| `fomn` | `f/(pm*pn)` | 科氏项的复合系数 |
| `rhoA/rhoS` | 归一化垂向平均密度 / 平均密度扰动 | 自由表面工作量与变密度压力梯度 |
| `zwrk/gzeta/gzeta2/gzetaSA` | 时间加权自由表面及其密度相关组合 | 压力梯度的中间量，不能一概视为新 zeta |
| `rhs_ubar/rhs_vbar` | 当前二维动量 RHS，m⁴/s² | tile-private 计算结果 |
| `rubar/rvbar` | 二维动量 RHS 的共享历史槽，m⁴/s² | corrector 的时间历史输入 |
| `rhs_zeta/rzeta` | 当前连续方程 RHS / 共享历史，m³/s | 自由表面时间推进 |
| `DU/DV_avg1` | 二维方程用的时间平均输运，m³/s | 第一套平均 |
| `DU/DV_avg2` | 三维耦合用的时间平均输运，m³/s | 第二套平均 |
| `Zt_avg1` | 时间平均自由表面，m | 快慢模耦合 |
| `rufrc/rvfrc` | 二维动量耦合强迫，m⁴/s² | 首快步 predictor 中生成，再供后续快步使用 |
| `UFx/UFe/VFx/VFe` | 在原始各阶段反复复用的二维临时通量数组 | 名字不代表跨阶段不变的同一物理场 |
| `DiaU2rhs/DiaV2rhs` | 各动量 RHS 贡献的私有 scratch | 按诊断项索引存储 |
| `DiaRUbar/DiaRVbar` | 诊断 RHS 的共享时间历史 | 保存供后续 corrector 读取 |
| `DiaU2int/DiaV2int`、`DiaU2wrk/DiaV2wrk` | 诊断积分状态及加权累计工作量 | 三维诊断耦合 |

变量含义主要据 `ROMS/Modules/mod_ocean.F:13–18`、`mod_grid.F:40–74` 和 `mod_coupling.F:12–20`；临时量及诊断的计算以原始文件为准。`mod_ocean.F` 的英文概述把 `ubar/vbar` 写作 vertically integrated component，但其单位和此处算式是速度；本文用“深度平均速度”避免与 `DUon/DVom` 混淆。

### 2.2 快时间层不等于慢时间层

- `krhs`：本次计算 RHS 使用的快时间层。
- `kstp`：本次推进所基于的快时间层。
- `knew`：本次写入的快时间层。
- `ptsk = 3-kstp`：两个 RHS 历史槽中的另一个槽；不能笼统称为“当前预测层”。
- 原始 `nstp/nnew` 是慢时间层索引；当前 `step2d_tile` 的局部形参名为 `istp/inew`，外层调用仍传 `nstp(ng)/nnew(ng)`。这是避免命名冲突的接口重命名，不是新的积分算法。

定义依据 `ROMS/Modules/mod_stepping.F:25–45`。`zeta/ubar/vbar` 有三个快状态槽（1:3），`rzeta/rubar/rvbar` 有两个 RHS 历史槽（1:2）。`ru/rv` 是含 `0:UBk` 垂向维度的四维数组，耦合阶段只访问第三维 0，但该维的真实长度仍决定时间维 stride；不可把它当成没有垂向维度的二维数组，也不可把 `UBk` 当成二维状态的时间层数。

<a id="common"></a>

## 3. 共同的并行、内存与适用条件

### 3.1 所有入口都是“行并行，i 连续”

共用的 `range_policy` 是 `Kokkos::RangePolicy<exec_space, Kokkos::IndexType<int>>`。每个工作项接收一个 `j`，在该行内用普通 `for (int i=...)` 遍历。上下界以 Fortran 的包含端点范围表达；Kokkos 的上端点为半开区间，所以传 `Jend+1`。带符号的 `int` 允许负的 halo 下界。

这八个入口**没有** `TeamPolicy`、`MDRangePolicy`、二维空间块、沿 `i` 的 `KB=32` 列分块或 `parallel_reduce`。少数函数中的 `id` 是诊断项索引，不是垂向 `k`。空间点上的时间累计、几项 RHS 相加或相邻面差分都不是全局求和归约。

Fortran 第一维 `i` 连续。保留 i 最内层可形成连续访问，部分循环因此具备更有利的编译器向量化条件；是否真的生成 SIMD、是否减少 DRAM 流量及性能收益，仍需编译报告和测量。不能只凭源码中的“读取几个场”推算实际带宽，也不能保证所有局部标量始终驻留寄存器。

SW64 当前后端的 `Kokkos_SW64_Parallel_Range.hpp:50–59` 将行区间分给 64 个 CPE，每个 CPE 可处理多行；不是每一行独占一颗永久线程。可利用并行度受行数及每行负载限制，短而宽的 tile 不能仅靠 i 很长就保证 64 个 CPE 负载均匀。

### 3.2 借用 Fortran 存储，不进行数组搬运

附录 C 的 `Array2d/3d/4d` 保存裸指针、原始下界和 stride，索引按 Fortran 列主序展开；没有边界检查，也不拥有内存。附录 B 的 `grid*` 和 `priv*` 分别包装 grid-bounded 数组与 tile-private scratch。`DiaU2rhs/DiaV2rhs` 用私有 bounds，其他多数组如 `DiaRUbar/DiaRVbar/DiaRUfrc/DiaRVfrc` 用 grid bounds，二者混用会静默产生错误地址。

Fortran 用 `C_LOC` 传原存储，数组需具有 `TARGET/POINTER` 属性；实际存储必须满足接口假定的连续性、尺寸、下界和生命周期。C++ 的 `void*` 本身不携带这些信息。增加 `TARGET`、C 绑定和形参改名是互操作改动，不是物理公式优化。

当前选择轻量 `Array*d`；这不意味着 `Kokkos::View` 必然分配、复制或产生引用计数开销。正确配置的 unmanaged `Kokkos::View` 也能零拷贝借用外部存储。

### 3.3 同步、alias 与 tile 所有权

以下解释建立在原 Fortran 调用契约上：不同物理场与 scratch 没有意外指针重叠，历史索引合法，所有输入 halo 已就绪，grid 输出区域的 tile 所有权无冲突，不并发推进同一份状态。源码没有用 `restrict` 自动证明这些条件。

各 kernel 的输出主要按工作项所属行、i、诊断项和时间槽唯一写入；是否能融合要看它有没有读取**另一工作项尚未生成的 scratch**。`depth_flux` 和 `momentum_step` 正是通过重算南邻水深消除这种依赖；这不等于所有入口不读邻点。

当前 Serial 后端直接完成循环；仓库 SW64 后端在 `Kokkos_SW64_KernelLaunch.hpp:34–38` 内调用 `athread_join_cgs()`，且 `Kokkos_SW64.hpp:49` 的 memory space 是 `HostSpace`。所以当前 SW64 返回后 Fortran 可以继续访问同一存储。这里的同步和零拷贝依据不能推广成“所有 Kokkos 后端都同步且共享主机内存”。OpenMP 内核并行时 ROMS 外层应使用单线程，避免嵌套；ROMS 自己按 tile 用 OpenMP 时，当前说明要求选 Serial kernel 后端。

### 3.4 当前配置门禁，不是原始全部物理的通用替代

当六个 step2d 阶段宏中的任意一个启用时，当前 C++ 编译检查要求：

- 必须定义 `SOLVE3D`、`VAR_RHO_2D`、`UV_ADV`、`UV_COR`、`UV_VIS2`、`DIAGNOSTICS_UV`。
- 拒绝 `MASKING`、`WET_DRY`、`WEC`、`WEC_VF`、`NESTING`、`CURVGRID`、`UV_C2ADVECTION`、`UV_VIS4`、`SINGLE_PRECISION`。
- 所以当前支持的是这组诊断三维、变密度、四阶中心对流、科氏力和水平谐波黏性路径；完整门禁见附录 B 的真实源码，而非本段文字的猜测。

如果六个阶段宏全不启用，这一组门禁也不启用，不能把它说成所有 Kokkos 构建都受同样限制。原始 Fortran 的可选物理更宽，下面仍完整保留相关 CPP 分支；出现这些分支不表示 C++ 已支持它们。halo、MPI、自由表面/速度边界、体积守恒开边界和点源处理继续由 Fortran 承担。

### 3.5 原始与当前 Fortran 回退路径的一致性核实

本次轻量文本检查将当前文件里的 `ROMS_KOKKOS` 条件选择为关闭，**保留其余所有 CPP 条件及两侧源码**，再比较从 `#include "set_bounds.h"` 开始的整个执行区。删除注释、空白，并统一 `nstp→istp`、`nnew→inew` 后，原始与当前都是 **2143 个非空行，逐行相同（PASS）**。

另外，depth、平均、zeta 主计算/回写/历史保存、压力、对流、科氏与曲率、psi 深度、谐波黏性、诊断、保存 RHS 的 12 个细分区段逐字一致；coupling 和 momentum 两段仅有上述 dummy 名字差别，替换后逐字一致。因此正文展示真正移植前的原名，而不把改名后的 fallback 伪称为逐字原件。

这项 PASS 是**源码文本一致性**，不是 C++ 数值运行结果，也不是对任意 `ROMS_KOKKOS` 开启、阶段宏混搭组合的保证。它不替代编译、边界条件检查或整模数值验收。

<a id="depth-flux"></a>

## 4. depth_flux：总水深与体积输运

本段包括 `DISTRIBUTE && !NESTING` 及其 `#else` 两套范围，并完整保留 WEC/湿干相关可选代码。第 704 行以后的通信没有放进计算段，仍见附录 A。当前 C++ 的整个函数、数组包装、计时及范围分支如下。

### 4.1 完整原始 Fortran 计算段

<a id="src-f-depth"></a>

**源码块 `F-DEPTH`**：提交 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **609–703**，共 **95 行**。

<details>
<summary>完整原始 Fortran 计算段（点击展开完整代码）</summary>
<!-- source-block:F-DEPTH -->

```fortran
!-----------------------------------------------------------------------
!  Compute total depth (m) and vertically integrated mass fluxes.
!-----------------------------------------------------------------------
!
#if defined DISTRIBUTE && !defined NESTING

!  In distributed-memory, the I- and J-ranges are different and a
!  special exchange is done to avoid having three ghost points for
!  high order numerical stencils. Notice that a private array is
!  passed below to the exchange routine. It also applies periodic
!  boundary conditions, if appropriate and no partitions in I- or
!  J-directions.
!
      DO j=JstrV-2,Jendp2
        DO i=IstrU-2,Iendp2
          Drhs(i,j)=zeta(i,j,krhs)+h(i,j)
        END DO
      END DO
      DO j=JstrV-2,Jendp2
        DO i=IstrU-1,Iendp2
          cff=0.5_r8*on_u(i,j)
          cff1=cff*(Drhs(i,j)+Drhs(i-1,j))
          DUon(i,j)=ubar(i,j,krhs)*cff1
# ifdef WEC
#  ifdef WET_DRY
          cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,ubar_stokes(i,j))*umask_wet(i,j)
          cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          cff1=cff1*cff7
#  endif
          DUSon(i,j)=ubar_stokes(i,j)*cff1
          DUon(i,j)=DUon(i,j)+DUSon(i,j)
# endif
        END DO
      END DO
      DO j=JstrV-1,Jendp2
        DO i=IstrU-2,Iendp2
          cff=0.5_r8*om_v(i,j)
          cff1=cff*(Drhs(i,j)+Drhs(i,j-1))
          DVom(i,j)=vbar(i,j,krhs)*cff1
# ifdef WEC
#  ifdef WET_DRY
          cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,vbar_stokes(i,j))*vmask_wet(i,j)
          cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          cff1=cff1*cff7
#  endif
          DVSom(i,j)=vbar_stokes(i,j)*cff1
          DVom(i,j)=DVom(i,j)+DVSom(i,j)
# endif
        END DO
      END DO

#else

      DO j=JstrVm2-1,Jendp2
        DO i=IstrUm2-1,Iendp2
          Drhs(i,j)=zeta(i,j,krhs)+h(i,j)
        END DO
      END DO
      DO j=JstrVm2-1,Jendp2
        DO i=IstrUm2,Iendp2
          cff=0.5_r8*on_u(i,j)
          cff1=cff*(Drhs(i,j)+Drhs(i-1,j))
          DUon(i,j)=ubar(i,j,krhs)*cff1
# ifdef WEC
#  ifdef WET_DRY
          cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,ubar_stokes(i,j))*umask_wet(i,j)
          cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          cff1=cff1*cff7
#  endif
          DUSon(i,j)=ubar_stokes(i,j)*cff1
          DUon(i,j)=DUon(i,j)+DUSon(i,j)
# endif
        END DO
      END DO
      DO j=JstrVm2,Jendp2
        DO i=IstrUm2-1,Iendp2
          cff=0.5_r8*om_v(i,j)
          cff1=cff*(Drhs(i,j)+Drhs(i,j-1))
          DVom(i,j)=vbar(i,j,krhs)*cff1
# ifdef WEC
#  ifdef WET_DRY
          cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,vbar_stokes(i,j))*vmask_wet(i,j)
          cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          cff1=cff1*cff7
#  endif
          DVSom(i,j)=vbar_stokes(i,j)*cff1
          DVom(i,j)=DVom(i,j)+DVSom(i,j)
# endif
        END DO
      END DO
#endif
```

</details>

### 4.2 完整当前 Kokkos 函数

本源码块还保留紧邻函数的原注释和空行；完整函数本身为 `ROMS/Cpp/kokkos_step2d.cpp:258–319`。

<a id="src-c-depth"></a>

**源码块 `C-DEPTH`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d.cpp`，原始行 **250–320**，共 **71 行**。

<details>
<summary>完整当前 C++ 函数及原始横幅（点击展开完整代码）</summary>
<!-- source-block:C-DEPTH -->

```cpp
/*======================================================================
** Compute total depth (m) and vertically integrated mass fluxes.
**
**   Drhs(i,j) = zeta(i,j,krhs) + h(i,j)
**   DUon(i,j) = 0.5*on_u*(Drhs(i,j)+Drhs(i-1,j)) * ubar(i,j,krhs)
**   DVom(i,j) = 0.5*om_v*(Drhs(i,j)+Drhs(i,j-1)) * vbar(i,j,krhs)
**====================================================================*/

void k_step2d_depth_flux(int ng, int tile, int krhs,
                         int LBi, int UBi, int LBj, int UBj,
                         int IminS, int ImaxS, int JminS, int JmaxS,
                         void* zeta_p, void* h_p, void* on_u_p, void* om_v_p,
                         void* ubar_p, void* vbar_p, void* Drhs_p,
                         void* DUon_p, void* DVom_p,
                         const TileBounds* t) {
    roms::initialize();
    ROMS_KK_TIMER("step2d depth_flux");
    (void)ng;
    (void)tile;

    const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

    Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
    Array2d<double> h = grid2d(h_p, s);
    Array2d<double> on_u = grid2d(on_u_p, s);
    Array2d<double> om_v = grid2d(om_v_p, s);
    Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
    Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);

    Array2d<double> Drhs = priv2d(Drhs_p, s);
    Array2d<double> DUon = priv2d(DUon_p, s);
    Array2d<double> DVom = priv2d(DVom_p, s);

#if defined DISTRIBUTE && !defined NESTING
    const int j_start = t->JstrV - 2;
    const int i_start = t->IstrU - 2;
#else
    const int j_start = t->JstrVm2 - 1;
    const int i_start = t->IstrUm2 - 1;
#endif

    const int i_end = t->Iendp2;
    const int j_end = t->Jendp2;

    /*
    ** One launch per call: every row computes its own Drhs, then DUon from
    ** that row.  DVom needs Drhs of row j-1, which another work item owns,
    ** so it is recomputed from the same two operands -- the identical sum,
    ** not a re-read -- and no row waits for another.
    */
    Kokkos::parallel_for(
        range_policy(j_start, j_end + 1), KOKKOS_LAMBDA(int j) {
            for (int i = i_start; i <= i_end; ++i) {
                Drhs(i, j) = zeta(i, j, krhs) + h(i, j);
            }
            for (int i = i_start + 1; i <= i_end; ++i) {
                const double cff = 0.5 * on_u(i, j);
                const double cff1 = cff * (Drhs(i, j) + Drhs(i - 1, j));
                DUon(i, j) = ubar(i, j, krhs) * cff1;
            }
            if (j > j_start) {
                for (int i = i_start; i <= i_end; ++i) {
                    const double drhs_s = zeta(i, j - 1, krhs) + h(i, j - 1);
                    const double cff = 0.5 * om_v(i, j);
                    const double cff1 = cff * (Drhs(i, j) + drhs_s);
                    DVom(i, j) = vbar(i, j, krhs) * cff1;
                }
            }
        });
}

```

</details>

### 4.3 解释

**物理作用与输入输出。** 输入 RHS 层 `zeta/ubar/vbar`、地形 `h` 和面宽 `on_u/om_v`；输出总水深 `Drhs` 与 U/V 面体积输运 `DUon/DVom`。U 面水深由当前 rho 点和西邻平均，V 面由当前 rho 点和南邻平均。它不是只复制速度，也没有做垂向循环。

**原始多趟循环。** 每套范围先完整计算二维 `Drhs`，再完整扫 U 输运，再扫 V 输运。原始 Fortran 的 V 循环读取的是前面已经生成的 `Drhs(i,j-1)`，并没有在该循环中重算 `zeta+h`。

**计算模式与映射。** 总水深是同点运算；面输运包含一阶邻距的水深插值，是交错网格邻点计算而不是归约。当前一次 `RangePolicy` 覆盖 `j_start:Jendp2`，每行依次写水深、算 U，满足 `j>j_start` 再算 V。分布式无 nesting 的起点为 `JstrV-2/IstrU-2`；其他情况为 `JstrVm2-1/IstrUm2-1`。U 从 `i_start+1` 开始，V 从 `i_start` 开始，不能把它们写成同一范围。

**真正优化。** 把三趟阶段计算放进一个 dispatch。U 可直接使用本行刚写好的 `Drhs`；V 所需南邻若读 scratch 就会依赖别的工作项，因此 C++ 改为重算 `zeta(i,j-1,krhs)+h(i,j-1)`。它用额外的输入读取和加法换去跨行阶段依赖/独立 dispatch，不是“完全没有 scratch”：仍完整写 `Drhs`，每行仍有多个 i 循环。

**未改与成立条件。** 支持配置下仍使用同样的水深加法及面输运表达式，通信和开边界体积修正没有融合进来。当前源码的正常调用中，zeta/h 只读，scratch 互相独立，每行拥有自己的写入区，因而不需要在行间等待。不能把这一点推广到原始 WEC、湿干或任意 alias 输入。

<a id="time-average"></a>

## 5. time_average：快时间平均

本段位于原始第 736 行开启的外层 `#ifdef SOLVE3D` 内，该外层到第 856 行才关闭。此处不伪造额外包装；内层 predictor/corrector、首步和 WEC 分支全部保留。后续平均场边界及交换仍在 Fortran。

### 5.1 完整原始 Fortran 计算段

<a id="src-f-tavg"></a>

**源码块 `F-TAVG`**：提交 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **738–810**，共 **73 行**。

<details>
<summary>完整原始 Fortran 计算段（点击展开完整代码）</summary>

<!-- source-block:F-TAVG -->
```fortran
!-----------------------------------------------------------------------
!  Compute time averaged fields over all short time-steps.
!-----------------------------------------------------------------------
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        IF (FIRST_2D_STEP) THEN
!
!  Reset arrays for 2D fields averaged within the short time-steps.
!
          cff2=(-1.0_r8/12.0_r8)*weight(2,iif(ng)+1,ng)
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              Zt_avg1(i,j)=0.0_r8
            END DO
            DO i=Istr,IendR
              DU_avg1(i,j)=0.0_r8
              DU_avg2(i,j)=cff2*DUon(i,j)
            END DO
          END DO
          DO j=Jstr,JendR
            DO i=IstrR,IendR
              DV_avg1(i,j)=0.0_r8
              DV_avg2(i,j)=cff2*DVom(i,j)
            END DO
          END DO
        ELSE
!
!  Accumulate field averages of previous time-step after they are
!  computed in the previous corrector step, updated their boundaries,
!  and synchronized.
!
          cff1=weight(1,iif(ng)-1,ng)
          cff2=(8.0_r8/12.0_r8)*weight(2,iif(ng)  ,ng)-                 &
     &         (1.0_r8/12.0_r8)*weight(2,iif(ng)+1,ng)
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              Zt_avg1(i,j)=Zt_avg1(i,j)+cff1*zeta(i,j,krhs)
            END DO
            DO i=Istr,IendR
              DU_avg1(i,j)=DU_avg1(i,j)+cff1*DUon(i,j)
# ifdef WEC
              DU_avg1(i,j)=DU_avg1(i,j)-cff1*DUSon(i,j)
# endif
              DU_avg2(i,j)=DU_avg2(i,j)+cff2*DUon(i,j)
            END DO
          END DO
          DO j=Jstr,JendR
            DO i=IstrR,IendR
              DV_avg1(i,j)=DV_avg1(i,j)+cff1*DVom(i,j)
# ifdef WEC
              DV_avg1(i,j)=DV_avg1(i,j)-cff1*DVSom(i,j)
# endif
              DV_avg2(i,j)=DV_avg2(i,j)+cff2*DVom(i,j)
            END DO
          END DO
        END IF
      ELSE
        IF (FIRST_2D_STEP) THEN
          cff2=weight(2,iif(ng),ng)
        ELSE
          cff2=(5.0_r8/12.0_r8)*weight(2,iif(ng),ng)
        END IF
        DO j=JstrR,JendR
          DO i=Istr,IendR
            DU_avg2(i,j)=DU_avg2(i,j)+cff2*DUon(i,j)
          END DO
        END DO
        DO j=Jstr,JendR
          DO i=IstrR,IendR
            DV_avg2(i,j)=DV_avg2(i,j)+cff2*DVom(i,j)
          END DO
        END DO
      END IF
```

</details>

### 5.2 完整当前 Kokkos 函数

本源码块还保留紧邻函数的原注释和空行；完整函数本身为 `ROMS/Cpp/kokkos_step2d.cpp:326–408`。

<a id="src-c-tavg"></a>

**源码块 `C-TAVG`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d.cpp`，原始行 **321–409**，共 **89 行**。

<details>
<summary>完整当前 C++ 函数及原始横幅（点击展开完整代码）</summary>

<!-- source-block:C-TAVG -->
```cpp
/*======================================================================
** Compute time averaged fields over all short time-steps (SOLVE3D).
** The caller computes the two coefficients cff1/cff2 from weight(:,:,ng).
**====================================================================*/

void k_step2d_time_average(int ng, int tile, int krhs,
                           int predictor, int first_step,
                           int LBi, int UBi, int LBj, int UBj,
                           int IminS, int ImaxS, int JminS, int JmaxS,
                           void* zeta_p, void* DUon_p, void* DVom_p,
                           void* Zt_avg1_p, void* DU_avg1_p, void* DU_avg2_p,
                           void* DV_avg1_p, void* DV_avg2_p,
                           double cff1, double cff2,
                           const TileBounds* t) {
    roms::initialize();
    ROMS_KK_TIMER("step2d time_average");
    (void)ng;
    (void)tile;

    const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

    Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);

    Array2d<double> DUon = priv2d(DUon_p, s);
    Array2d<double> DVom = priv2d(DVom_p, s);

    Array2d<double> Zt_avg1 = grid2d(Zt_avg1_p, s);
    Array2d<double> DU_avg1 = grid2d(DU_avg1_p, s);
    Array2d<double> DU_avg2 = grid2d(DU_avg2_p, s);
    Array2d<double> DV_avg1 = grid2d(DV_avg1_p, s);
    Array2d<double> DV_avg2 = grid2d(DV_avg2_p, s);

    const int Istr = t->Istr;
    const int IstrR = t->IstrR, IendR = t->IendR;
    const int Jstr = t->Jstr;
    const int JstrR = t->JstrR, JendR = t->JendR;

    /* U and V rows share one launch.  Every update is pointwise, and the
    ** V rows [Jstr, JendR] lie inside the U rows [JstrR, JendR]. */
    if (predictor) {
        if (first_step) {
            Kokkos::parallel_for(
                range_policy(JstrR, JendR + 1), KOKKOS_LAMBDA(int j) {
                    for (int i = IstrR; i <= IendR; ++i)
                        Zt_avg1(i, j) = 0.0;
                    for (int i = Istr; i <= IendR; ++i) {
                        DU_avg1(i, j) = 0.0;
                        DU_avg2(i, j) = cff2 * DUon(i, j);
                    }
                    if (j >= Jstr) {
                        for (int i = IstrR; i <= IendR; ++i) {
                            DV_avg1(i, j) = 0.0;
                            DV_avg2(i, j) = cff2 * DVom(i, j);
                        }
                    }
                });
        } else {
            Kokkos::parallel_for(
                range_policy(JstrR, JendR + 1), KOKKOS_LAMBDA(int j) {
                    for (int i = IstrR; i <= IendR; ++i) {
                        Zt_avg1(i, j) += cff1 * zeta(i, j, krhs);
                    }
                    for (int i = Istr; i <= IendR; ++i) {
                        DU_avg1(i, j) += cff1 * DUon(i, j);
                        DU_avg2(i, j) += cff2 * DUon(i, j);
                    }
                    if (j >= Jstr) {
                        for (int i = IstrR; i <= IendR; ++i) {
                            DV_avg1(i, j) += cff1 * DVom(i, j);
                            DV_avg2(i, j) += cff2 * DVom(i, j);
                        }
                    }
                });
        }
    } else {
        Kokkos::parallel_for(
            range_policy(JstrR, JendR + 1), KOKKOS_LAMBDA(int j) {
                for (int i = Istr; i <= IendR; ++i) {
                    DU_avg2(i, j) += cff2 * DUon(i, j);
                }
                if (j >= Jstr) {
                    for (int i = IstrR; i <= IendR; ++i) {
                        DV_avg2(i, j) += cff2 * DVom(i, j);
                    }
                }
            });
    }
}

```

</details>

### 5.3 解释

**物理作用与输入输出。** 把快模状态按 `weight` 的两套系数累计为自由表面平均和两套输运平均：输入 `zeta(:,:,krhs)`、已经通信/修正过的 `DUon/DVom`，读写 `Zt_avg1`、`DU/DV_avg1/avg2`。这是逐格的时间累计，不是在空间上把整个海域求和。

**原始循环与三种行为。** 首快步 predictor 清零第一套平均，用负权系数初始化第二套输运平均；后续 predictor 累加自由表面和两套输运；corrector 只更新 `DU/DV_avg2`。U/自由表面和 V 原本分开扫行。当前三条 launch 分支互斥，corrector 的首步/后续系数差异已经体现在实参 `cff2`，不是第四条 C++ launch 分支。

**计算模式与映射。** 这里是同点清零、缩放和 AXPY 型读改写，没有空间 stencil 和全局归约。统一行域 `JstrR:JendR`；本行先自由表面/U，再在 `j>=Jstr` 时处理 V。不同 i 起点照旧保留。所有更新只读写本点，不存在要求别的 j 先产出的中间量。

**真正优化。** 合并 U/V 的 dispatch，保持连续 i 访问，把 predictor/首步分支放在 host 选择。`weight` 是没有 `TARGET` 的 Fortran 模块数组，当前调用端 `830–847` 行先算出两标量，再传进 C++，不为它建立或拷贝数组；完整调用与系数见附录 D。这是数据接口和调度优化，滤波权重本身没变。

**顺序和 alias 条件。** 平均发生在本次 zeta 推进前，后续 predictor 采样前次 corrector 已完成并同步的值。只在首快步 predictor 初始化，不能每次调用清零。输入输运、状态和五个平均输出须符合原始独立存储契约；若任意把平均数组与输入或其他平均数组重叠，点式结构也不能保证原始语义。

<a id="zeta-step"></a>

## 6. zeta_step：自由表面推进

原始片段完整覆盖三种时间推进、内部区 zeta 回写、predictor RHS 保存及其闭合的 `IF`。第 1007–1018 行的周期/分布式 RHS 通信特意保留在原始摘录中以免截断控制流；这些通信并没有进入 C++。质量点源与自由表面边界处理位于片段后，仍在 Fortran。

### 6.1 完整原始 Fortran 计算段

<a id="src-f-zeta"></a>

**源码块 `F-ZETA`**：提交 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **885–1019**，共 **135 行**。

<details>
<summary>完整原始 Fortran 计算段（点击展开完整代码）</summary>

<!-- source-block:F-ZETA -->
```fortran
!=======================================================================
!  Time step free-surface equation.
!=======================================================================
!
!  During the first time-step, the predictor step is Forward-Euler
!  and the corrector step is Backward-Euler. Otherwise, the predictor
!  step is Leap-frog and the corrector step is Adams-Moulton.
#if defined VAR_RHO_2D && defined SOLVE3D
!  Recall that the vertical averaged density (rhoA) and density
!  pertubation (rhoS) are nondimensional quantities.
!
      fac=1000.0_r8/rho0                                ! nondimensional
#endif
!
      IF (FIRST_2D_STEP) THEN
        cff1=dtfast(ng)
        DO j=JstrV-1,Jend
          DO i=IstrU-1,Iend
            rhs_zeta(i,j)=(DUon(i,j)-DUon(i+1,j))+                      &
     &                    (DVom(i,j)-DVom(i,j+1))
            zeta_new(i,j)=zeta(i,j,kstp)+                               &
     &                    pm(i,j)*pn(i,j)*cff1*rhs_zeta(i,j)
#ifdef MASKING
            zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
#endif
            Dnew(i,j)=zeta_new(i,j)+h(i,j)
!
            zwrk(i,j)=0.5_r8*(zeta(i,j,kstp)+zeta_new(i,j))
#if defined VAR_RHO_2D && defined SOLVE3D
            gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
            gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
            gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
#else
            gzeta(i,j)=zwrk(i,j)
            gzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
#endif
          END DO
        END DO
      ELSE IF (PREDICTOR_2D_STEP(ng)) THEN
        cff1=2.0_r8*dtfast(ng)
        cff4=4.0_r8/25.0_r8
        cff5=1.0_r8-2.0_r8*cff4
        DO j=JstrV-1,Jend
          DO i=IstrU-1,Iend
            rhs_zeta(i,j)=(DUon(i,j)-DUon(i+1,j))+                      &
     &                    (DVom(i,j)-DVom(i,j+1))
            zeta_new(i,j)=zeta(i,j,kstp)+                               &
     &                    pm(i,j)*pn(i,j)*cff1*rhs_zeta(i,j)
#ifdef MASKING
            zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
#endif
            Dnew(i,j)=zeta_new(i,j)+h(i,j)
!
            zwrk(i,j)=cff5*zeta(i,j,krhs)+                              &
     &                cff4*(zeta(i,j,kstp)+zeta_new(i,j))
#if defined VAR_RHO_2D && defined SOLVE3D
            gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
            gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
            gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
#else
            gzeta(i,j)=zwrk(i,j)
            gzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
#endif
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=dtfast(ng)*5.0_r8/12.0_r8
        cff2=dtfast(ng)*8.0_r8/12.0_r8
        cff3=dtfast(ng)*1.0_r8/12.0_r8
        cff4=2.0_r8/5.0_r8
        cff5=1.0_r8-cff4
        DO j=JstrV-1,Jend
          DO i=IstrU-1,Iend
            cff=cff1*((DUon(i,j)-DUon(i+1,j))+                          &
     &                (DVom(i,j)-DVom(i,j+1)))
            zeta_new(i,j)=zeta(i,j,kstp)+                               &
     &                    pm(i,j)*pn(i,j)*(cff+                         &
     &                                     cff2*rzeta(i,j,kstp)-        &
     &                                     cff3*rzeta(i,j,ptsk))
#ifdef MASKING
            zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
#endif
            Dnew(i,j)=zeta_new(i,j)+h(i,j)
!
            zwrk(i,j)=cff5*zeta_new(i,j)+cff4*zeta(i,j,krhs)
#if defined VAR_RHO_2D && defined SOLVE3D
            gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
            gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
            gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
#else
            gzeta(i,j)=zwrk(i,j)
            gzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
#endif
          END DO
        END DO
      END IF
!
!  Load new free-surface values into shared array at both predictor
!  and corrector steps.
#ifdef WET_DRY
!  Modify new free-surface to Ensure that depth is > Dcrit for masked
!  cells.
#endif
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
          zeta(i,j,knew)=zeta_new(i,j)
#if defined WET_DRY && defined MASKING
          zeta(i,j,knew)=zeta(i,j,knew)+                                &
     &                   (Dcrit(ng)-h(i,j))*(1.0_r8-rmask(i,j))
#endif
        END DO
      END DO
!
!  If predictor step, load right-side-term into shared array.
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        DO j=Jstr,Jend
          DO i=Istr,Iend
            rzeta(i,j,krhs)=rhs_zeta(i,j)
          END DO
        END DO
        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_r2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            rzeta(:,:,krhs))
        END IF
#ifdef DISTRIBUTE
        CALL mp_exchange2d (ng, tile, iNLM, 1,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints,                               &
     &                      EWperiodic(ng), NSperiodic(ng),             &
     &                      rzeta(:,:,krhs))
#endif
      END IF
```

</details>

### 6.2 完整当前 Kokkos 函数

本源码块还保留紧邻函数的原注释和空行；完整函数本身为 `ROMS/Cpp/kokkos_step2d.cpp:420–571`。

<a id="src-c-zeta"></a>

**源码块 `C-ZETA`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d.cpp`，原始行 **410–572**，共 **163 行**。

<details>
<summary>完整当前 C++ 函数及原始横幅（点击展开完整代码）</summary>

<!-- source-block:C-ZETA -->
```cpp
/*======================================================================
** Time step free-surface equation.
**
** Branch selection follows FIRST_2D_STEP, which is iif==1 when SOLVE3D
** is defined and iic==ntfirst otherwise (ROMS/Include/globaldefs.h).
** The caller resolves that and passes the result -- getting this wrong
** silently runs the wrong integrator for every fast step after the
** first one.
**====================================================================*/

void k_step2d_zeta_step(int ng, int tile, int krhs, int kstp, int knew,
                        int predictor, int first_2d_step,
                        int LBi, int UBi, int LBj, int UBj,
                        int IminS, int ImaxS, int JminS, int JmaxS,
                        void* zeta_p, void* h_p, void* pm_p, void* pn_p,
                        void* DUon_p, void* DVom_p,
                        void* gzeta_p, void* gzeta2_p,
                        void* gzetaSA_p, void* rhoA_p, void* rhoS_p,
                        void* rhs_zeta_p, void* zeta_new_p,
                        void* Dnew_p, void* zwrk_p,
                        void* rzeta_p, double dtfast, double rho0,
                        const TileBounds* t) {
    roms::initialize();
    ROMS_KK_TIMER("step2d zeta_step");
    (void)ng;
    (void)tile;

    const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

    Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
    Array2d<double> h = grid2d(h_p, s);
    Array2d<double> pm = grid2d(pm_p, s);
    Array2d<double> pn = grid2d(pn_p, s);

    Array2d<double> DUon = priv2d(DUon_p, s);
    Array2d<double> DVom = priv2d(DVom_p, s);

    Array2d<double> gzeta = priv2d(gzeta_p, s);
    Array2d<double> gzeta2 = priv2d(gzeta2_p, s);
    Array2d<double> gzetaSA = priv2d(gzetaSA_p, s);
    Array2d<double> rhoA = grid2d(rhoA_p, s);
    Array2d<double> rhoS = grid2d(rhoS_p, s);

    Array2d<double> rhs_zeta = priv2d(rhs_zeta_p, s);
    Array2d<double> zeta_new = priv2d(zeta_new_p, s);
    Array2d<double> Dnew = priv2d(Dnew_p, s);
    Array2d<double> zwrk = priv2d(zwrk_p, s);

    Array3d<double> rzeta = roms::as_3d<double>(rzeta_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);

    /*
    ** VAR_RHO_2D is switched on automatically whenever SOLVE3D is defined
    ** (ROMS/Include/globaldefs.h).  rhoA and rhoS are nondimensional, so
    ** fac makes the contribution dimensional.
    */

    const double fac = 1000.0 / rho0;
    const int ptsk = 3 - kstp;

    const int j0 = t->JstrV - 1, j1 = t->Jend;
    const int i0 = t->IstrU - 1, i1 = t->Iend;

    /*
    ** The reload of zeta(:,:,knew) and, on predictor steps, of
    ** rzeta(:,:,krhs) run in the same launch as the update.  Their rows
    ** [Jstr, Jend] and columns [Istr, Iend] lie inside the update's
    ** [JstrV-1, Jend] x [IstrU-1, Iend], and each row copies only values
    ** it has just computed.  No work item reads zeta or rzeta of another
    ** row, so the copy cannot overtake a reader even if knew aliases kstp
    ** or krhs.  The rzeta copy used to be a separate Fortran loop on the
    ** management core.
    */
    const int ai0 = t->Istr, ai1 = t->Iend;
    const int aj0 = t->Jstr;
    const int store = predictor;

    if (first_2d_step) {
        const double cff1 = dtfast;

        Kokkos::parallel_for(
            range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
                for (int i = i0; i <= i1; ++i) {
                    const double rz = (DUon(i, j) - DUon(i + 1, j)) +
                                      (DVom(i, j) - DVom(i, j + 1));
                    rhs_zeta(i, j) = rz;
                    const double zn = zeta(i, j, kstp) + pm(i, j) * pn(i, j) * cff1 * rz;
                    zeta_new(i, j) = zn;
                    Dnew(i, j) = zn + h(i, j);
                    const double zw = 0.5 * (zeta(i, j, kstp) + zn);
                    zwrk(i, j) = zw;
                    gzeta(i, j) = (fac + rhoS(i, j)) * zw;
                    gzeta2(i, j) = gzeta(i, j) * zw;
                    gzetaSA(i, j) = zw * (rhoS(i, j) - rhoA(i, j));
                }
                if (j >= aj0) {
                    for (int i = ai0; i <= ai1; ++i)
                        zeta(i, j, knew) = zeta_new(i, j);
                    if (store) {
                        for (int i = ai0; i <= ai1; ++i)
                            rzeta(i, j, krhs) = rhs_zeta(i, j);
                    }
                }
            });
    } else if (predictor) {
        const double cff1 = 2.0 * dtfast;
        const double cff4 = 4.0 / 25.0;
        const double cff5 = 1.0 - 2.0 * cff4;

        Kokkos::parallel_for(
            range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
                for (int i = i0; i <= i1; ++i) {
                    const double rz = (DUon(i, j) - DUon(i + 1, j)) +
                                      (DVom(i, j) - DVom(i, j + 1));
                    rhs_zeta(i, j) = rz;
                    const double zn = zeta(i, j, kstp) + pm(i, j) * pn(i, j) * cff1 * rz;
                    zeta_new(i, j) = zn;
                    Dnew(i, j) = zn + h(i, j);
                    const double zw =
                        cff5 * zeta(i, j, krhs) + cff4 * (zeta(i, j, kstp) + zn);
                    zwrk(i, j) = zw;
                    gzeta(i, j) = (fac + rhoS(i, j)) * zw;
                    gzeta2(i, j) = gzeta(i, j) * zw;
                    gzetaSA(i, j) = zw * (rhoS(i, j) - rhoA(i, j));
                }
                if (j >= aj0) {
                    for (int i = ai0; i <= ai1; ++i)
                        zeta(i, j, knew) = zeta_new(i, j);
                    for (int i = ai0; i <= ai1; ++i)
                        rzeta(i, j, krhs) = rhs_zeta(i, j);
                }
            });
    } else {
        const double cff1 = dtfast * 5.0 / 12.0;
        const double cff2 = dtfast * 8.0 / 12.0;
        const double cff3 = dtfast * 1.0 / 12.0;
        const double cff4 = 2.0 / 5.0;
        const double cff5 = 1.0 - cff4;

        Kokkos::parallel_for(
            range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
                for (int i = i0; i <= i1; ++i) {
                    const double cff = cff1 * ((DUon(i, j) - DUon(i + 1, j)) +
                                               (DVom(i, j) - DVom(i, j + 1)));
                    const double zn = zeta(i, j, kstp) +
                                      pm(i, j) * pn(i, j) *
                                          (cff + cff2 * rzeta(i, j, kstp) -
                                           cff3 * rzeta(i, j, ptsk));
                    zeta_new(i, j) = zn;
                    Dnew(i, j) = zn + h(i, j);
                    const double zw = cff5 * zn + cff4 * zeta(i, j, krhs);
                    zwrk(i, j) = zw;
                    gzeta(i, j) = (fac + rhoS(i, j)) * zw;
                    gzeta2(i, j) = gzeta(i, j) * zw;
                    gzetaSA(i, j) = zw * (rhoS(i, j) - rhoA(i, j));
                }
                if (j >= aj0) {
                    for (int i = ai0; i <= ai1; ++i)
                        zeta(i, j, knew) = zeta_new(i, j);
                }
            });
    }
}

```

</details>

### 6.3 解释

**物理作用与输入输出。** 四个交错面的输运差形成连续方程 RHS，再由面积逆因子 `pm*pn` 和快时间积分更新自由表面。输出 `zeta_new`、`Dnew`，以及后续压力梯度使用的 `zwrk/gzeta/gzeta2/gzetaSA`；在内部区回写 `zeta(:,:,knew)`，predictor 还保存 `rzeta(:,:,krhs)`。`rhoA/rhoS` 为归一化量，`fac=1000/rho0` 参与相应组合，不能把这几种工作量全当成同一时间层。

**三种时间公式。** 首快步使用 `dtfast` 的启动式，原始注释分别把 predictor/corrector 称为 Forward-Euler/Backward-Euler；不能把首步两次都称为 FE。后续 predictor 使用 LF 与其自由表面工作权重；后续 corrector 使用 AM3 的 `5/12,8/12,-1/12` RHS 历史组合。C++ 在 host 选择一套分支，不是在一次调用中计算三套公式。

**原始多趟与当前映射。** 原始先完整计算工作区域，然后一趟内部区回写 zeta，再在 predictor 单独扫一趟保存 RHS，随后通信。当前每个 j 工作项先完成 `IstrU-1:Iend` 主计算，再对本行 `Istr:Iend` 回写；主行域是 `JstrV-1:Jend`，回写要求 `j>=Jstr`。这把主计算、同排回填与历史保存合并到一次 launch，边界/通信仍留外部。

**计算模式。** 输运散度是四面邻点 stencil；自由表面本身和 RHS 历史读取是同点时间递推；密度工作量为同点派生。四面相减不是全局归约。所有自定义数值 helper 和包装定义都在附录 B/C。

**alias 与写竞争。** 主循环读 zeta/rzeta 的本点时间层，不读相邻行的这两个状态；空间邻点读取来自此入口只读的输运。先算完整本行再回写，保留旧时间层读取完成后才写新时间层的顺序，即便 `knew` 与输入状态槽可能重合也不会越过本行读者。每个工作项只回写自己那一行；这不授权让 `DUon/DVom`、`zeta_new`、密度和状态等不同物理数组任意重叠，也不允许无序地与后续边界或 halo 交换并发执行。

<a id="rhs-fused"></a>

## 7. rhs_fused：二维动量右端融合

原始段一次性收录整个压力梯度至谐波黏性区域，所有内部 CPP 分支均在原序保留。它包括比当前移植支持集更宽的二阶对流、波流、曲率、掩码及 psi 深度范围分支，不能把这些分支都视为 C++ 已实现。后续 `UV_VIS4`、其他强迫与耦合等不属于本入口。

### 7.1 完整原始 Fortran 计算段

<a id="src-f-rhs"></a>

**源码块 `F-RHS`**：提交 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **1079–1652**，共 **574 行**。

<details>
<summary>完整原始 Fortran 计算段（点击展开完整代码）</summary>

<!-- source-block:F-RHS -->
```fortran
!-----------------------------------------------------------------------
!  Compute pressure gradient terms.
!-----------------------------------------------------------------------
!
      cff1=0.5_r8*g
      cff2=1.0_r8/3.0_r8
#if !defined SOLVE3D && defined ATM_PRESS
      fac3=0.5_r8*100.0_r8/rho0
#endif
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          rhs_ubar(i,j)=cff1*on_u(i,j)*                                 &
     &                  ((h(i-1,j)+                                     &
     &                    h(i ,j))*                                     &
     &                   (gzeta(i-1,j)-                                 &
     &                    gzeta(i  ,j))+                                &
#if defined VAR_RHO_2D && defined SOLVE3D
     &                   (h(i-1,j)-                                     &
     &                    h(i  ,j))*                                    &
     &                   (gzetaSA(i-1,j)+                               &
     &                    gzetaSA(i  ,j)+                               &
     &                    cff2*(rhoA(i-1,j)-                            &
     &                          rhoA(i  ,j))*                           &
     &                         (zwrk(i-1,j)-                            &
     &                          zwrk(i  ,j)))+                          &
#endif
     &                   (gzeta2(i-1,j)-                                &
     &                    gzeta2(i  ,j)))
#if defined ATM_PRESS && !defined SOLVE3D
          rhs_ubar(i,j)=rhs_ubar(i,j)-                                  &
     &                  fac3*on_u(i,j)*                                 &
     &                  (h(i-1,j)+h(i,j)+                               &
     &                   gzeta(i-1,j)+gzeta(i,j))*                      &
     &                  (Pair(i,j)-Pair(i-1,j))
#endif
#if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
          rhs_ubar(i,j)=rhs_ubar(i,j)-                                  &
     &                  cff1*on_u(i,j)*                                 &
     &                  (h(i-1,j)+h(i,j)+                               &
     &                   gzeta(i-1,j)+gzeta(i,j))*                      &
     &                  (eq_tide(i,j)-eq_tide(i-1,j))
#endif
#ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2pgrd)=rhs_ubar(i,j)
#endif
#if defined WEC_VF
          cff3=0.5_r8*on_u(i,j)*                                        &
     &         (h(i-1,j)+h(i,j)+                                        &
     &         gzeta(i-1,j)+gzeta(i,j))
          cff4=cff3*g*(zetaw(i-1,j)-zetaw(i,j))
          cff5=cff3*g*(qsp(i-1,j)-qsp(i,j))
          cff6=cff3*(bh(i-1,j)-bh(i,j))
          cff7=rukvf2d(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)-cff4-cff5+cff6+cff7
# ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2pgrd)
          DiaU2rhs(i,j,M2pgrd)=DiaU2rhs(i,j,M2pgrd)-cff4-cff5+cff6
          DiaU2rhs(i,j,M2zetw)=-cff4
          DiaU2rhs(i,j,M2zqsp)=-cff5
          DiaU2rhs(i,j,M2zbeh)=cff6
          DiaU2rhs(i,j,M2kvrf)=cff7
#  ifndef UV_ADV
          DiaU2rhs(i,j,M2hjvf)=0.0_r8
#  endif
# endif
#endif
        END DO
        IF (j.ge.JstrV) THEN
          DO i=Istr,Iend
            rhs_vbar(i,j)=cff1*om_v(i,j)*                               &
     &                    ((h(i,j-1)+                                   &
     &                      h(i,j  ))*                                  &
     &                     (gzeta(i,j-1)-                               &
     &                      gzeta(i,j  ))+                              &
#if defined VAR_RHO_2D && defined SOLVE3D
     &                     (h(i,j-1)-                                   &
     &                      h(i,j  ))*                                  &
     &                     (gzetaSA(i,j-1)+                             &
     &                      gzetaSA(i,j  )+                             &
     &                      cff2*(rhoA(i,j-1)-                          &
     &                            rhoA(i,j  ))*                         &
     &                           (zwrk(i,j-1)-                          &
     &                            zwrk(i,j  )))+                        &
#endif
     &                     (gzeta2(i,j-1)-                              &
     &                      gzeta2(i,j  )))
#if defined ATM_PRESS && !defined SOLVE3D
            rhs_vbar(i,j)=rhs_vbar(i,j)-                                &
     &                    fac3*om_v(i,j)*                               &
     &                    (h(i,j-1)+h(i,j)+                             &
     &                     gzeta(i,j-1)+gzeta(i,j))*                    &
     &                    (Pair(i,j)-Pair(i,j-1))
#endif
#if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
            rhs_vbar(i,j)=rhs_vbar(i,j)-                                &
     &                    cff1*om_v(i,j)*                               &
     &                    (h(i,j-1)+h(i,j)+                             &
     &                     gzeta(i,j-1)+gzeta(i,j))*                    &
     &                    (eq_tide(i,j)-eq_tide(i,j-1))
#endif
#ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2pgrd)=rhs_vbar(i,j)
#endif
#if defined WEC_VF
            cff3=0.5_r8*om_v(i,j)*                                      &
     &           (h(i,j-1)+h(i,j)+                                      &
     &           gzeta(i,j-1)+gzeta(i,j))
            cff4=cff3*g*(zetaw(i,j-1)-zetaw(i,j))
            cff5=cff3*g*(qsp(i,j-1)-qsp(i,j))
            cff6=cff3*(bh(i,j-1)-bh(i,j))
            cff7=rvkvf2d(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)-cff4-cff5+cff6+cff7
# ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2pgrd)
            DiaV2rhs(i,j,M2pgrd)=DiaV2rhs(i,j,M2pgrd)-cff4-cff5+cff6
            DiaV2rhs(i,j,M2zetw)=-cff4
            DiaV2rhs(i,j,M2zqsp)=-cff5
            DiaV2rhs(i,j,M2zbeh)=cff6
            DiaV2rhs(i,j,M2kvrf)=cff7
#  ifndef UV_ADV
            DiaV2rhs(i,j,M2hjvf)=0.0_r8
#  endif
# endif
#endif
          END DO
        END IF
      END DO
#ifdef UV_ADV
!
!-----------------------------------------------------------------------
!  Add in horizontal advection of momentum.
!-----------------------------------------------------------------------

# ifdef UV_C2ADVECTION
!
!  Second-order, centered differences advection fluxes.
!
      DO j=Jstr,Jend
        DO i=IstrU-1,Iend
          UFx(i,j)=0.25_r8*(DUon(i,j)+DUon(i+1,j))*                     &
     &                     (ubar(i  ,j,krhs)+                           &
     &                      ubar(i+1,j,krhs))
        END DO
      END DO
!
      DO j=Jstr,Jend+1
        DO i=IstrU,Iend
          UFe(i,j)=0.25_r8*(DVom(i,j)+DVom(i-1,j))*                     &
     &                     (ubar(i,j  ,krhs)+                           &
     &                      ubar(i,j-1,krhs))
        END DO
      END DO
!
      DO j=JstrV,Jend
        DO i=Istr,Iend+1
          VFx(i,j)=0.25_r8*(DUon(i,j)+DUon(i,j-1))*                     &
     &                     (vbar(i  ,j,krhs)+                           &
     &                      vbar(i-1,j,krhs))
        END DO
      END DO
!
      DO j=JstrV-1,Jend
        DO i=Istr,Iend
          VFe(i,j)=0.25_r8*(DVom(i,j)+DVom(i,j+1))*                     &
     &                     (vbar(i,j  ,krhs)+                           &
     &                      vbar(i,j+1,krhs))
        END DO
      END DO
# else
!
!  Fourth-order, centered differences advection fluxes.
!
      DO j=Jstr,Jend
        DO i=IstrUm1,Iendp1
          grad (i,j)=ubar(i-1,j,krhs)-2.0_r8*ubar(i,j,krhs)+            &
     &               ubar(i+1,j,krhs)
          Dgrad(i,j)=DUon(i-1,j)-2.0_r8*DUon(i,j)+DUon(i+1,j)
        END DO
      END DO
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=Jstr,Jend
            grad (Istr,j)=grad (Istr+1,j)
            Dgrad(Istr,j)=Dgrad(Istr+1,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=Jstr,Jend
            grad (Iend+1,j)=grad (Iend,j)
            Dgrad(Iend+1,j)=Dgrad(Iend,j)
          END DO
        END IF
      END IF

      cff=1.0_r8/6.0_r8
      DO j=Jstr,Jend
        DO i=IstrU-1,Iend
          UFx(i,j)=0.25_r8*(ubar(i  ,j,krhs)+                           &
     &                      ubar(i+1,j,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i+1,j)))*             &
     &                     (DUon(i,j)+DUon(i+1,j)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i+1,j)))
        END DO
      END DO
!
      DO j=Jstrm1,Jendp1
        DO i=IstrU,Iend
          grad(i,j)=ubar(i,j-1,krhs)-2.0_r8*ubar(i,j,krhs)+             &
     &              ubar(i,j+1,krhs)
        END DO
      END DO
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=IstrU,Iend
            grad(i,Jstr-1)=grad(i,Jstr)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=IstrU,Iend
            grad(i,Jend+1)=grad(i,Jend)
          END DO
        END IF
      END IF
      DO j=Jstr,Jend+1
        DO i=IstrU-1,Iend
          Dgrad(i,j)=DVom(i-1,j)-2.0_r8*DVom(i,j)+DVom(i+1,j)
        END DO
      END DO

      cff=1.0_r8/6.0_r8
      DO j=Jstr,Jend+1
        DO i=IstrU,Iend
          UFe(i,j)=0.25_r8*(ubar(i,j  ,krhs)+                           &
     &                      ubar(i,j-1,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i,j-1)))*             &
     &                     (DVom(i,j)+DVom(i-1,j)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i-1,j)))
        END DO
      END DO
!
      DO j=JstrV,Jend
        DO i=Istrm1,Iendp1
          grad(i,j)=vbar(i-1,j,krhs)-2.0_r8*vbar(i,j,krhs)+             &
     &              vbar(i+1,j,krhs)
        END DO
      END DO
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=JstrV,Jend
            grad(Istr-1,j)=grad(Istr,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=JstrV,Jend
            grad(Iend+1,j)=grad(Iend,j)
          END DO
        END IF
      END IF
      DO j=JstrV-1,Jend
        DO i=Istr,Iend+1
          Dgrad(i,j)=DUon(i,j-1)-2.0_r8*DUon(i,j)+DUon(i,j+1)
        END DO
      END DO

      cff=1.0_r8/6.0_r8
      DO j=JstrV,Jend
        DO i=Istr,Iend+1
          VFx(i,j)=0.25_r8*(vbar(i  ,j,krhs)+                           &
     &                      vbar(i-1,j,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i-1,j)))*             &
     &                     (DUon(i,j)+DUon(i,j-1)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i,j-1)))
        END DO
      END DO
!
      DO j=JstrVm1,Jendp1
        DO i=Istr,Iend
          grad(i,j)=vbar(i,j-1,krhs)-2.0_r8*vbar(i,j,krhs)+             &
     &              vbar(i,j+1,krhs)
          Dgrad(i,j)=DVom(i,j-1)-2.0_r8*DVom(i,j)+DVom(i,j+1)
        END DO
      END DO
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=Istr,Iend
            grad (i,Jstr)=grad (i,Jstr+1)
            Dgrad(i,Jstr)=Dgrad(i,Jstr+1)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=Istr,Iend
            grad (i,Jend+1)=grad (i,Jend)
            Dgrad(i,Jend+1)=Dgrad(i,Jend)
          END DO
        END IF
      END IF

      cff=1.0_r8/6.0_r8
      DO j=JstrV-1,Jend
        DO i=Istr,Iend
          VFe(i,j)=0.25_r8*(vbar(i,j  ,krhs)+                           &
     &                      vbar(i,j+1,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i,j+1)))*             &
     &                     (DVom(i,j)+DVom(i,j+1)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i,j+1)))
        END DO
      END DO
# endif
!
!  Add advection to RHS terms.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=UFx(i,j)-UFx(i-1,j)
          cff2=UFe(i,j+1)-UFe(i,j)
          fac=cff1+cff2
          rhs_ubar(i,j)=rhs_ubar(i,j)-fac
# if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2xadv)=-cff1
          DiaU2rhs(i,j,M2yadv)=-cff2
          DiaU2rhs(i,j,M2hadv)=-fac
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff1=VFx(i+1,j)-VFx(i,j)
          cff2=VFe(i,j)-VFe(i,j-1)
          fac=cff1+cff2
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac
# if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2xadv)=-cff1
          DiaV2rhs(i,j,M2yadv)=-cff2
          DiaV2rhs(i,j,M2hadv)=-fac
# endif
        END DO
      END DO
#endif

#ifdef UV_COR
!
!-----------------------------------------------------------------------
!  Add in Coriolis term.
!-----------------------------------------------------------------------
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=0.5_r8*Drhs(i,j)*fomn(i,j)
          UFx(i,j)=cff*(vbar(i,j  ,krhs)+                               &
     &                  vbar(i,j+1,krhs))
          VFe(i,j)=cff*(ubar(i  ,j,krhs)+                               &
     &                  ubar(i+1,j,krhs))
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac1
# if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2fcor)=fac1
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac1
# if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2fcor)=-fac1
# endif
        END DO
      END DO
!
# ifdef WEC
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=0.5_r8*Drhs(i,j)*fomn(i,j)
          UFx(i,j)=cff*(vbar_stokes(i,j  )+                             &
     &                  vbar_stokes(i,j+1))
          VFe(i,j)=cff*(ubar_stokes(i  ,j)+                             &
     &                  ubar_stokes(i+1,j))
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac1
#  if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2fsco)=fac1
#  endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac1
#  if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2fsco)=-fac1
#  endif
        END DO
      END DO
# endif
#endif

#if defined CURVGRID && defined UV_ADV
!
!-----------------------------------------------------------------------
!  Add in curvilinear transformation terms.
!-----------------------------------------------------------------------
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff1=0.5_r8*(vbar(i,j  ,krhs)+                                &
# ifdef WEC
     &                 vbar_stokes(i,j  )+                              &
     &                 vbar_stokes(i,j+1)+                              &
# endif
     &                 vbar(i,j+1,krhs))
          cff2=0.5_r8*(ubar(i  ,j,krhs)+                                &
# ifdef WEC
     &                 ubar_stokes(i  ,j)+                              &
     &                 ubar_stokes(i+1,j)+                              &
# endif
     &                 ubar(i+1,j,krhs))
          cff3=cff1*dndx(i,j)
          cff4=cff2*dmde(i,j)
# ifdef WEC_VF
          cff5=0.5_r8*(vbar_stokes(i,j  )+                             &
     &                 vbar_stokes(i,j+1))
          cff6=0.5_r8*(ubar_stokes(i  ,j)+                             &
     &                 ubar_stokes(i+1,j))
          cff7=cff5*dndx(i,j)
          cff8=cff6*dmde(i,j)
# endif
          cff=Drhs(i,j)*(cff3-cff4)
          UFx(i,j)=cff*cff1
          VFe(i,j)=cff*cff2
# ifdef WEC_VF
          UFx(i,j)=UFx(i,j)-(cff5*Drhs(i,j)*(cff7-cff8))
          VFe(i,j)=VFe(i,j)-(cff6*Drhs(i,j)*(cff7-cff8))
# endif
# if defined DIAGNOSTICS_UV
          cff=Drhs(i,j)*cff4
          Uwrk(i,j)=-cff*cff1                  ! ubar equation, ETA-term
          Vwrk(i,j)=-cff*cff2                  ! vbar equation, ETA-term
#  ifdef WEC_VF
            Uwrk(i,j)=Uwrk(i,j)+Drhs(i,j)*cff5*cff8
            Vwrk(i,j)=Vwrk(i,j)-Drhs(i,j)*cff6*cff8
#  endif
# endif
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac1
# if defined DIAGNOSTICS_UV
          fac2=0.5_r8*(Uwrk(i,j)+Uwrk(i-1,j))
          DiaU2rhs(i,j,M2xadv)=DiaU2rhs(i,j,M2xadv)+fac1-fac2
          DiaU2rhs(i,j,M2yadv)=DiaU2rhs(i,j,M2yadv)+fac2
          DiaU2rhs(i,j,M2hadv)=DiaU2rhs(i,j,M2hadv)+fac1
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac1
# if defined DIAGNOSTICS_UV
          fac2=0.5_r8*(Vwrk(i,j)+Vwrk(i,j-1))
          DiaV2rhs(i,j,M2xadv)=DiaV2rhs(i,j,M2xadv)-fac1+fac2
          DiaV2rhs(i,j,M2yadv)=DiaV2rhs(i,j,M2yadv)-fac2
          DiaV2rhs(i,j,M2hadv)=DiaV2rhs(i,j,M2hadv)-fac1
# endif
        END DO
      END DO
#endif
#if defined UV_VIS2 || defined UV_VIS4
!
!-----------------------------------------------------------------------
!  If horizontal mixing, compute total depth at PSI-points.
!-----------------------------------------------------------------------
!
# ifdef UV_VIS4
      DO j=Jstrm1,Jendp2
        DO i=Istrm1,Iendp2
# else
      DO j=Jstr,Jend+1
        DO i=Istr,Iend+1
# endif
          Drhs_p(i,j)=0.25_r8*(Drhs(i,j  )+Drhs(i-1,j  )+               &
     &                         Drhs(i,j-1)+Drhs(i-1,j-1))
        END DO
      END DO
#endif
#ifdef UV_VIS2
!
!-----------------------------------------------------------------------
!  Add in horizontal harmonic viscosity.
!-----------------------------------------------------------------------
!
!  Compute flux-components of the horizontal divergence of the stress
!  tensor (m5/s2) in XI- and ETA-directions.
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=visc2_r(i,j)*Drhs(i,j)*0.5_r8*                            &
     &        (pmon_r(i,j)*                                             &
     &         ((pn(i  ,j)+pn(i+1,j))*ubar(i+1,j,krhs)-                 &
     &          (pn(i-1,j)+pn(i  ,j))*ubar(i  ,j,krhs))-                &
     &         pnom_r(i,j)*                                             &
     &         ((pm(i,j  )+pm(i,j+1))*vbar(i,j+1,krhs)-                 &
     &          (pm(i,j-1)+pm(i,j  ))*vbar(i,j  ,krhs)))
          UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
          VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
        END DO
      END DO
      DO j=Jstr,Jend+1
        DO i=Istr,Iend+1
          cff=visc2_p(i,j)*Drhs_p(i,j)*0.5_r8*                          &
     &        (pmon_p(i,j)*                                             &
     &         ((pn(i  ,j-1)+pn(i  ,j))*vbar(i  ,j,krhs)-               &
     &          (pn(i-1,j-1)+pn(i-1,j))*vbar(i-1,j,krhs))+              &
     &         pnom_p(i,j)*                                             &
     &         ((pm(i-1,j  )+pm(i,j  ))*ubar(i,j  ,krhs)-               &
     &          (pm(i-1,j-1)+pm(i,j-1))*ubar(i,j-1,krhs)))
#  ifdef MASKING
          cff=cff*pmask(i,j)
#  endif
#  ifdef WET_DRY
          cff=cff*pmask_wet(i,j)
#  endif
          UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
          VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
        END DO
      END DO
!
!  Add in harmonic viscosity.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=0.5_r8*(pn(i-1,j)+pn(i,j))*(UFx(i,j  )-UFx(i-1,j))
          cff2=0.5_r8*(pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-UFe(i  ,j))
          fac=cff1+cff2
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac
# if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2hvis)=fac
          DiaU2rhs(i,j,M2xvis)=cff1
          DiaU2rhs(i,j,M2yvis)=cff2
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff1=0.5_r8*(pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-VFx(i,j  ))
          cff2=0.5_r8*(pm(i,j-1)+pm(i,j))*(VFe(i  ,j)-VFe(i,j-1))
          fac=cff1-cff2
          rhs_vbar(i,j)=rhs_vbar(i,j)+fac
# if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2hvis)=fac
          DiaV2rhs(i,j,M2xvis)= cff1
          DiaV2rhs(i,j,M2yvis)=-cff2
# endif
        END DO
      END DO
#endif
```

</details>

### 7.2 完整当前 Kokkos 函数

本源码块还保留紧邻函数的原注释和空行；完整函数本身为 `ROMS/Cpp/kokkos_step2d.cpp:1170–1506`。

<a id="src-c-rhs"></a>

**源码块 `C-RHS`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d.cpp`，原始行 **1152–1506**，共 **355 行**。

<details>
<summary>完整当前 C++ 函数及原始横幅（点击展开完整代码）</summary>

<!-- source-block:C-RHS -->
```cpp
/*
** Fused right-hand-side kernel for the 2-D barotropic step.
**
** Pressure gradient, advection, Coriolis and harmonic viscosity share
** the same input fields and have no intervening halo exchange.  Inline
** their flux stencils to avoid scratch-array traffic and intermediate
** dispatches.  It is validated end to end: Serial output must equal the
** original Fortran exactly (ROMS/Cpp/tools/sw64/README.md).
**
** Closed edges: the Fortran fills a halo cell from its inward
** neighbour at one index per direction -- grad(Istr), grad(Iend+1),
** grad(Jstr-1), grad(Jend+1) in the xi/eta ubar passes; grad(Istr-1),
** grad(Iend+1), grad(Jstr), grad(Jend+1) in the vbar passes.  The fold
** helper below reproduces that, and only at those exact indices: the
** fill loops themselves start at IstrUm1/Jstrm1, which are outside the
** fold point and use the raw stencil.
*/

void k_step2d_rhs_fused(int ng, int tile, int krhs,
                        int LBi, int UBi, int LBj, int UBj,
                        int IminS, int ImaxS, int JminS, int JmaxS,
                        void* h_p, void* on_u_p, void* om_v_p,
                        void* gzeta_p, void* gzeta2_p, void* gzetaSA_p,
                        void* rhoA_p, void* zwrk_p,
                        void* Drhs_p, void* fomn_p,
                        void* ubar_p, void* vbar_p,
                        void* DUon_p, void* DVom_p,
                        void* pm_p, void* pn_p,
                        void* pmon_r_p, void* pnom_r_p,
                        void* pmon_p_p, void* pnom_p_p,
                        void* om_r_p, void* on_r_p,
                        void* om_p_p, void* on_p_p,
                        void* visc2_r_p, void* visc2_p2_p,
                        void* rhs_ubar_p, void* rhs_vbar_p,
                        void* DiaU2rhs_p, void* DiaV2rhs_p,
                        int NDM2d,
                        int M2pgrd, int M2xadv, int M2yadv, int M2hadv,
                        int M2fcor, int M2hvis, int M2xvis, int M2yvis,
                        double g,
                        const EdgeFlags* e,
                        const TileBounds* t) {
    roms::initialize();
    ROMS_KK_TIMER("step2d rhs_fused");
    (void)ng;
    (void)tile;

    const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

    Array2d<double> h = grid2d(h_p, s);
    Array2d<double> on_u = grid2d(on_u_p, s);
    Array2d<double> om_v = grid2d(om_v_p, s);
    Array2d<double> gzeta = priv2d(gzeta_p, s);
    Array2d<double> gzeta2 = priv2d(gzeta2_p, s);
    Array2d<double> gzetaSA = priv2d(gzetaSA_p, s);
    Array2d<double> rhoA = grid2d(rhoA_p, s);
    Array2d<double> zwrk = priv2d(zwrk_p, s);

    Array2d<double> Drhs = priv2d(Drhs_p, s);
    Array2d<double> fomn = grid2d(fomn_p, s);

    Array3d<double> ubar = grid3d(ubar_p, s, TLEV0, TLEV1);
    Array3d<double> vbar = grid3d(vbar_p, s, TLEV0, TLEV1);

    Array2d<double> DUon = priv2d(DUon_p, s);
    Array2d<double> DVom = priv2d(DVom_p, s);

    Array2d<double> pm = grid2d(pm_p, s);
    Array2d<double> pn = grid2d(pn_p, s);

    Array2d<double> pmon_r = grid2d(pmon_r_p, s);
    Array2d<double> pnom_r = grid2d(pnom_r_p, s);
    Array2d<double> pmon_p = grid2d(pmon_p_p, s);
    Array2d<double> pnom_p = grid2d(pnom_p_p, s);
    Array2d<double> om_r = grid2d(om_r_p, s);
    Array2d<double> on_r = grid2d(on_r_p, s);
    Array2d<double> om_p = grid2d(om_p_p, s);
    Array2d<double> on_p = grid2d(on_p_p, s);
    Array2d<double> visc2_r = grid2d(visc2_r_p, s);
    Array2d<double> visc2_pp = grid2d(visc2_p2_p, s);

    Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
    Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);

    Array3d<double> DiaU2rhs = priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
    Array3d<double> DiaV2rhs = priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);

    const int Istr = t->Istr, Iend = t->Iend;
    const int Jstr = t->Jstr, Jend = t->Jend;
    const int IstrU = t->IstrU, JstrV = t->JstrV;

    /* Closed-edge folds.  Each matches one of the halo fills in the
    ** Fortran, which are guarded by
    **     .not.(CompositeGrid(iedge,ng).or.<dir>periodic(ng))
    **     .and.DOMAIN(ng)%<Edge>_Edge(tile)
    ** exactly as reproduced here. */

    const int fw = (!e->composite_w && !e->periodic_ew && e->west);
    const int fe = (!e->composite_e && !e->periodic_ew && e->east);
    const int fs = (!e->composite_s && !e->periodic_ns && e->south);
    const int fn = (!e->composite_n && !e->periodic_ns && e->north);

    const double cpg = 0.5 * g;
    const double cpg2 = 1.0 / 3.0;
    const double c6 = 1.0 / 6.0;

    /*--------------------------------------------------------------------
    ** U rows.  i runs over [IstrU, Iend], which is exactly the Fortran
    ** rhs_ubar loop.  Every term the Fortran computes at a
    ** neighbouring point is recomputed here; the arithmetic order inside
    ** each term matches, so the accumulations into rhs_ubar are the same
    ** sequence of operations.
    **------------------------------------------------------------------*/

    /* One launch covers both families: the V rows [JstrV, Jend] lie inside
    ** the U rows [Jstr, Jend], every input is read-only here and every
    ** output (rhs_ubar/rhs_vbar and their diagnostics) is written only at
    ** its own point, so the two loops of a row are independent. */
    Kokkos::parallel_for(
        range_policy(Jstr, Jend + 1), KOKKOS_LAMBDA(int j) {
            /* The xi second differences and the rho-point viscosity flux at
            ** i-1 are what this cell needs as its "m" values, so they are
            ** carried across iterations instead of being evaluated twice. */
            double gxm = ddx(ubar, krhs, IstrU - 1, j, fw, fe, Istr, Iend + 1);
            double gx0 = ddx(ubar, krhs, IstrU, j, fw, fe, Istr, Iend + 1);
            double dxm = ddx2(DUon, IstrU - 1, j, fw, fe, Istr, Iend + 1);
            double dx0 = ddx2(DUon, IstrU, j, fw, fe, Istr, Iend + 1);
            double uRm = u_rho_flux(on_r, visc2_r, Drhs, pmon_r, pnom_r, pm, pn,
                                    ubar, vbar, krhs, IstrU - 1, j);
            for (int i = IstrU; i <= Iend; ++i) {
                /* --- pressure gradient --- */
                const double pg =
                    cpg * on_u(i, j) *
                    ((h(i - 1, j) + h(i, j)) * (gzeta(i - 1, j) - gzeta(i, j)) +
                     (h(i - 1, j) - h(i, j)) *
                         (gzetaSA(i - 1, j) + gzetaSA(i, j) +
                          cpg2 * (rhoA(i - 1, j) - rhoA(i, j)) *
                              (zwrk(i - 1, j) - zwrk(i, j))) +
                     (gzeta2(i - 1, j) - gzeta2(i, j)));
                double acc = pg;
                DiaU2rhs(i, j, M2pgrd) = pg;

                /* --- advection ---
                ** Fluxes at the two xi faces and two eta faces around (i,j).
                ** In the Fortran these live in UFx/UFe with grad holding
                ** the xi and then the eta second differences of ubar, and
                ** Dgrad the matching differences of DUon/DVom. */
                const double gx1 = ddx(ubar, krhs, i + 1, j, fw, fe, Istr, Iend + 1);
                const double dx1 = ddx2(DUon, i + 1, j, fw, fe, Istr, Iend + 1);
                const double ufx =
                    0.25 * (ubar(i, j, krhs) + ubar(i + 1, j, krhs) - c6 * (gx0 + gx1)) *
                    (DUon(i, j) + DUon(i + 1, j) - c6 * (dx0 + dx1));

                const double ufxm =
                    0.25 * (ubar(i - 1, j, krhs) + ubar(i, j, krhs) - c6 * (gxm + gx0)) *
                    (DUon(i - 1, j) + DUon(i, j) - c6 * (dxm + dx0));

                /* eta-direction second differences of ubar at U points. */
                /*
                ** Folds here are Jstr-1 / Jend+1, not Jstr / Jend+1: the
                ** Fortran refills grad over j in [Jstrm1, Jendp1] and then
                ** sets grad(i,Jstr-1)=grad(i,Jstr).  Dgrad gets no fold in
                ** this block at all -- it is computed over [Jstr, Jend+1]
                ** and only ever read in the xi direction.
                */
                const double gy0 = ddy(ubar, krhs, i, j, fs, fn, Jstr - 1, Jend + 1);
                const double dgy0 = ddx2(DVom, i, j, 0, 0, 0, 0);
                const double gy_1 = ddy(ubar, krhs, i, j - 1, fs, fn, Jstr - 1, Jend + 1);
                const double dgy_1 = ddx2(DVom, i - 1, j, 0, 0, 0, 0);
                const double ufe =
                    0.25 * (ubar(i, j, krhs) + ubar(i, j - 1, krhs) - c6 * (gy0 + gy_1)) *
                    (DVom(i, j) + DVom(i - 1, j) - c6 * (dgy0 + dgy_1));

                const double gy1 = ddy(ubar, krhs, i, j + 1, fs, fn, Jstr - 1, Jend + 1);
                const double dgy1 = ddx2(DVom, i, j + 1, 0, 0, 0, 0);
                const double dgy1m = ddx2(DVom, i - 1, j + 1, 0, 0, 0, 0);
                const double ufe1 =
                    0.25 * (ubar(i, j + 1, krhs) + ubar(i, j, krhs) - c6 * (gy1 + gy0)) *
                    (DVom(i, j + 1) + DVom(i - 1, j + 1) - c6 * (dgy1 + dgy1m));

                const double axadv = ufx - ufxm;
                const double ayadv = ufe1 - ufe;
                const double aadv = axadv + ayadv;
                acc -= aadv;
                DiaU2rhs(i, j, M2xadv) = -axadv;
                DiaU2rhs(i, j, M2yadv) = -ayadv;
                DiaU2rhs(i, j, M2hadv) = -aadv;

                /* --- coriolis --- */
                const double fcor =
                    0.5 * (0.5 * Drhs(i, j) * fomn(i, j) *
                               (vbar(i, j, krhs) + vbar(i, j + 1, krhs)) +
                           0.5 * Drhs(i - 1, j) * fomn(i - 1, j) *
                               (vbar(i - 1, j, krhs) + vbar(i - 1, j + 1, krhs)));
                acc += fcor;
                DiaU2rhs(i, j, M2fcor) = fcor;

                /* --- harmonic viscosity ---
                ** Two flux families: at rho points from visc2_r, and at psi
                ** points from visc2_p with the depth averaged to psi points
                ** (Drhs_p in the Fortran, inlined here). */
                const double dp00 = 0.25 * (Drhs(i, j) + Drhs(i - 1, j) +
                                            Drhs(i, j - 1) + Drhs(i - 1, j - 1));
                const double dp0p = 0.25 * (Drhs(i, j + 1) + Drhs(i - 1, j + 1) +
                                            Drhs(i, j) + Drhs(i - 1, j));
                const double uR = u_rho_flux(on_r, visc2_r, Drhs, pmon_r, pnom_r,
                                             pm, pn, ubar, vbar, krhs, i, j);

                const double uP =
                    om_p(i, j) * om_p(i, j) * (visc2_pp(i, j) * dp00 * 0.5 * (pmon_p(i, j) * ((pn(i, j - 1) + pn(i, j)) * vbar(i, j, krhs) - (pn(i - 1, j - 1) + pn(i - 1, j)) * vbar(i - 1, j, krhs)) + pnom_p(i, j) * ((pm(i - 1, j) + pm(i, j)) * ubar(i, j, krhs) - (pm(i - 1, j - 1) + pm(i, j - 1)) * ubar(i, j - 1, krhs))));
                const double uPp =
                    om_p(i, j + 1) * om_p(i, j + 1) *
                    (visc2_pp(i, j + 1) * dp0p * 0.5 *
                     (pmon_p(i, j + 1) *
                          ((pn(i, j) + pn(i, j + 1)) * vbar(i, j + 1, krhs) -
                           (pn(i - 1, j) + pn(i - 1, j + 1)) * vbar(i - 1, j + 1, krhs)) +
                      pnom_p(i, j + 1) *
                          ((pm(i - 1, j + 1) + pm(i, j + 1)) * ubar(i, j + 1, krhs) -
                           (pm(i - 1, j) + pm(i, j)) * ubar(i, j, krhs))));

                const double vx1 = 0.5 * (pn(i - 1, j) + pn(i, j)) * (uR - uRm);
                const double vy1 = 0.5 * (pm(i - 1, j) + pm(i, j)) * (uPp - uP);
                const double hv = vx1 + vy1;
                acc += hv;
                DiaU2rhs(i, j, M2hvis) = hv;
                DiaU2rhs(i, j, M2xvis) = vx1;
                DiaU2rhs(i, j, M2yvis) = vy1;

                rhs_ubar(i, j) = acc;

                /* This cell's centre values are the next cell's "m" values. */
                gxm = gx0;
                gx0 = gx1;
                dxm = dx0;
                dx0 = dx1;
                uRm = uR;
            }

            /*------------------------------------------------------------------
            ** V rows.  i over [Istr, Iend], j over [JstrV, Jend] -- the Fortran
            ** rhs_vbar loop.
            **----------------------------------------------------------------*/

            if (j < JstrV)
                return;
            /* Same carry as the ubar row: the xi second difference, the eta
            ** second difference of DUon and the psi-point viscosity flux at
            ** i-1 / i+1 are the next cell's centre values. */
            double vgxm = ddx(vbar, krhs, Istr - 1, j, fw, fe, Istr - 1, Iend + 1);
            double vgx0 = ddx(vbar, krhs, Istr, j, fw, fe, Istr - 1, Iend + 1);
            double vdxm = ddy2(DUon, Istr, j - 1, 0, 0, 0, 0);
            double vdx0 = ddy2(DUon, Istr, j, 0, 0, 0, 0);
            double vPm = v_psi_flux(on_p, visc2_pp,
                                    0.25 * (Drhs(Istr, j) + Drhs(Istr - 1, j) +
                                            Drhs(Istr, j - 1) + Drhs(Istr - 1, j - 1)),
                                    pmon_p, pnom_p, pm, pn, ubar, vbar, krhs, Istr, j);
            for (int i = Istr; i <= Iend; ++i) {
                /* --- pressure gradient --- */
                const double pg =
                    cpg * om_v(i, j) *
                    ((h(i, j - 1) + h(i, j)) * (gzeta(i, j - 1) - gzeta(i, j)) +
                     (h(i, j - 1) - h(i, j)) *
                         (gzetaSA(i, j - 1) + gzetaSA(i, j) +
                          cpg2 * (rhoA(i, j - 1) - rhoA(i, j)) *
                              (zwrk(i, j - 1) - zwrk(i, j))) +
                     (gzeta2(i, j - 1) - gzeta2(i, j)));
                double acc = pg;
                DiaV2rhs(i, j, M2pgrd) = pg;

                /* --- advection ---
                ** VFx block: grad over xi at V points folds at Istr-1 /
                ** Iend+1; Dgrad holds DUon second differences along eta over
                ** [JstrV-1, Jend] with no fold. */
                const double vfx =
                    0.25 * (vbar(i, j, krhs) + vbar(i - 1, j, krhs) - c6 * (vgx0 + vgxm)) *
                    (DUon(i, j) + DUon(i, j - 1) - c6 * (vdx0 + vdxm));

                const double gx1 = ddx(vbar, krhs, i + 1, j, fw, fe, Istr - 1, Iend + 1);
                const double dx1 = ddy2(DUon, i + 1, j, 0, 0, 0, 0);
                const double dx1m = ddy2(DUon, i + 1, j - 1, 0, 0, 0, 0);
                const double vfx1 =
                    0.25 * (vbar(i + 1, j, krhs) + vbar(i, j, krhs) - c6 * (gx1 + vgx0)) *
                    (DUon(i + 1, j) + DUon(i + 1, j - 1) - c6 * (dx1 + dx1m));

                /* VFe block: grad over eta at V points folds at Jstr /
                ** Jend+1, and here Dgrad folds the same way. */
                const double gy0 = ddy(vbar, krhs, i, j, fs, fn, Jstr, Jend + 1);
                const double dgy0 = ddy2(DVom, i, j, fs, fn, Jstr, Jend + 1);
                const double gy1 = ddy(vbar, krhs, i, j + 1, fs, fn, Jstr, Jend + 1);
                const double dgy1 = ddy2(DVom, i, j + 1, fs, fn, Jstr, Jend + 1);
                const double vfe =
                    0.25 * (vbar(i, j, krhs) + vbar(i, j + 1, krhs) - c6 * (gy0 + gy1)) *
                    (DVom(i, j) + DVom(i, j + 1) - c6 * (dgy0 + dgy1));

                const double gym = ddy(vbar, krhs, i, j - 1, fs, fn, Jstr, Jend + 1);
                const double dgym = ddy2(DVom, i, j - 1, fs, fn, Jstr, Jend + 1);
                const double vfem =
                    0.25 * (vbar(i, j - 1, krhs) + vbar(i, j, krhs) - c6 * (gym + gy0)) *
                    (DVom(i, j - 1) + DVom(i, j) - c6 * (dgym + dgy0));

                const double axadv = vfx1 - vfx;
                const double ayadv = vfe - vfem;
                const double aadv = axadv + ayadv;
                acc -= aadv;
                DiaV2rhs(i, j, M2xadv) = -axadv;
                DiaV2rhs(i, j, M2yadv) = -ayadv;
                DiaV2rhs(i, j, M2hadv) = -aadv;

                /* --- coriolis --- */
                const double fcor =
                    0.5 * (0.5 * Drhs(i, j) * fomn(i, j) *
                               (ubar(i, j, krhs) + ubar(i + 1, j, krhs)) +
                           0.5 * Drhs(i, j - 1) * fomn(i, j - 1) *
                               (ubar(i, j - 1, krhs) + ubar(i + 1, j - 1, krhs)));
                acc -= fcor;
                DiaV2rhs(i, j, M2fcor) = -fcor;

                /* --- harmonic viscosity --- */
                const double dp10 = 0.25 * (Drhs(i + 1, j) + Drhs(i, j) +
                                            Drhs(i + 1, j - 1) + Drhs(i, j - 1));

                const double vPp = v_psi_flux(on_p, visc2_pp, dp10, pmon_p, pnom_p,
                                              pm, pn, ubar, vbar, krhs, i + 1, j);

                /* VFe at rho points from visc2_r. */
                const double vR =
                    om_r(i, j) * om_r(i, j) * (visc2_r(i, j) * Drhs(i, j) * 0.5 * (pmon_r(i, j) * ((pn(i, j) + pn(i + 1, j)) * ubar(i + 1, j, krhs) - (pn(i - 1, j) + pn(i, j)) * ubar(i, j, krhs)) - pnom_r(i, j) * ((pm(i, j) + pm(i, j + 1)) * vbar(i, j + 1, krhs) - (pm(i, j - 1) + pm(i, j)) * vbar(i, j, krhs))));
                const double vRm =
                    om_r(i, j - 1) * om_r(i, j - 1) *
                    (visc2_r(i, j - 1) * Drhs(i, j - 1) * 0.5 *
                     (pmon_r(i, j - 1) *
                          ((pn(i, j - 1) + pn(i + 1, j - 1)) * ubar(i + 1, j - 1, krhs) -
                           (pn(i - 1, j - 1) + pn(i, j - 1)) * ubar(i, j - 1, krhs)) -
                      pnom_r(i, j - 1) *
                          ((pm(i, j - 1) + pm(i, j)) * vbar(i, j, krhs) -
                           (pm(i, j - 2) + pm(i, j - 1)) * vbar(i, j - 1, krhs))));

                const double vx1 = 0.5 * (pn(i, j - 1) + pn(i, j)) * (vPp - vPm);
                const double vy1 = 0.5 * (pm(i, j - 1) + pm(i, j)) * (vR - vRm);
                const double hv = vx1 - vy1;
                acc += hv;
                DiaV2rhs(i, j, M2hvis) = hv;
                DiaV2rhs(i, j, M2xvis) = vx1;
                DiaV2rhs(i, j, M2yvis) = -vy1;

                rhs_vbar(i, j) = acc;

                /* This cell's centre values are the next cell's "m" values. */
                vgxm = vgx0;
                vgx0 = gx1;
                vdxm = dx1m;
                vdx0 = dx1;
                vPm = vPp;
            }
        });
}
```

</details>

### 7.3 解释

**物理作用与输入输出。** 从地形、自由表面/密度工作量、RHS 层速度与输运、网格度量、科氏参数及黏性系数，形成二维动量 RHS 和各项诊断贡献。U/V 采用不同交错位置。当前每点按原来阶段顺序做压力梯度 → 减平流 → U 加/V 减 Coriolis → 加谐波黏性，最后写一次主 RHS；各分方向诊断仍按原符号分别写出。

#### 原始区域不能只摘“压力加对流”

| 阶段 | 原始行范围 | 原始计算组织 | 当前支持集的处理 |
|---|---:|---|---|
| 压力梯度 | 1079–1205 | U/V 压力表达式及诊断；还有纯二维气压/潮汐、WEC_VF 可选项 | 内联当前 `SOLVE3D && VAR_RHO_2D` 路径 |
| 水平对流 | 1206–1424 | 二阶或四阶通量；四阶先写 `grad/Dgrad`，边界复制，再写四族通量，最后作散度 | 当前要求四阶中心，按点展开所需差分与通量 |
| 科氏力 | 1426–1489 | 先生成中间 `UFx/VFe`，再插值到 U/V；还有 Stokes 可选项 | 内联普通速度的 Coriolis 表达式 |
| 曲线坐标变换项 | 1491–1563 | 额外的 `CURVGRID && UV_ADV` 物理及诊断 | 当前明确拒绝 `CURVGRID`；这段不是科氏力，原文仍完整呈现 |
| psi 点水深 | 1564–1581 | 根据 `UV_VIS2/UV_VIS4` 选择范围后写 `Drhs_p` | 在需要的黏性通量处直接求四点平均 |
| 水平谐波黏性 | 1582–1652 | rho/psi 应力通量构造，再扫 U/V 加散度并记分方向诊断 | 内联通量，在局部 RHS 上累积 |

通常所说“五个被融合阶段”是压力、对流、科氏、psi 深度、谐波黏性；表中特列曲率，是为了避免无声删除原始的第六段可选物理。当前一条 launch 不代表用它替代原始文件里所有 RHS 物理。

#### 计算模式：多场交错网格的稀疏复合 stencil

四阶中心通量把二阶差分嵌入相邻面表达式，展开后可触达二阶邻距；压力、科氏、psi 水深与应力又各有不同场和邻点位置。所以它是 rho/U/V/psi 多场的**稀疏复合 stencil**，不是每个场都读取 25 个点的稠密 5×5 模板。“5×5”即使用来谈外围包围盒，也不能当成真实访问集合或 DRAM 字节数。

`RangePolicy` 覆盖 `Jstr:Jend`。每行先做 U 的 `IstrU:Iend`，若 `j<JstrV` 就返回，否则再做 V 的 `Istr:Iend`。输入场在本入口中只读，主 RHS 和各诊断只写本点，故 U/V 可共享 launch；它们不是共享可写的邻行通量缓存。

#### 真正优化：不物化若干二维中间场，并作部分行内滚动复用

1. 原始 `grad/Dgrad`、对流四族通量、科氏中间通量、psi 水深和黏性通量依次写 scratch 再读。当前在需要处内联表达式，相关中间场不在这条 C++ 路径中物化为完整二维数组，减少阶段遍历与 dispatch。
2. 每个 U/V 点用局部 `acc` 依次累积各贡献，主 `rhs_*` 最后写回。诊断并没有删除，仍有各自写入。
3. 已有 xi 滚动复用：U 携带 `gxm/gx0`、`dxm/dx0`、`uRm`；V 携带 `vgxm/vgx0`、`vdxm/vdx0`、`vPm`，让上一单元的部分中心量成为下一单元的左侧量。
4. **当前还没有携带完整 xi 平流面通量。** 代码中的 `ufx/ufxm`、`vfx/vfx1` 仍分别计算；不能把 share 上后来增加的 face carry 写成此 HEAD 已完成的优化。

“没有在该路径写这些 scratch”不等于 Fortran 已经删掉所有 scratch 声明、分配或所有后续用途；`Drhs`、自由表面工作量、RHS 与诊断等都还在。局部标量也不保证全在寄存器，展开与重算可能增加工作集、指令数和寄存器压力，甚至产生 spill；本次没有硬件计数器证据证明唯一瓶颈。

#### 边界 fold 不是通用 clamp

原始四阶阶段会在特定物理边界把二阶差分从向内一点复制出来。共用 `ddx/ddy/ddx2/ddy2` 只在**指定索引恰好相等**时重定向差分中心，且 `fw/fe/fs/fn` 只在非 composite、非 periodic、tile 位于对应域边时成立。U/V、xi/eta 的 fold 位置并不完全相同：例如 U 的 eta 差分折在 `Jstr-1`，而 V 的 eta 差分折在 `Jstr`；有些 `Dgrad` 根本不 fold。不能改讲成所有 halo 统一 clamp 到有效区，也不能说没有边界分支。

**alias 与成立条件。** 每行的滚动量是工作项局部状态，不和别的行通信；输出点唯一，诊断索引由 Fortran 正确传入。该性质要求只读输入与 RHS/诊断输出不发生意外重叠、halo 和 `EdgeFlags/TileBounds` 正确。真实数值等价性还依赖受控算术、编译器和配置；源码保留运算关联顺序的意图，不等于本次已重跑验证所有配置逐位相等。

<a id="coupling"></a>

## 8. coupling：二维与三维耦合

此段处于原始第 2204 行开启的外层 `#ifdef SOLVE3D` 的真分支内；该条件在第 2461 行转入纯二维表面应力处理，第 2485 行关闭。这里保留完整耦合分支和诊断，不人为补写外层 CPP；外围湿干和纯二维分支的真实上下文见附录 A。

### 8.1 完整原始 Fortran 计算段

<a id="src-f-coup"></a>

**源码块 `F-COUP`**：提交 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **2223–2460**，共 **238 行**。

<details>
<summary>完整原始 Fortran 计算段（点击展开完整代码）</summary>

<!-- source-block:F-COUP -->
```fortran
!
!-----------------------------------------------------------------------
!  Coupling between 2D and 3D equations.
!-----------------------------------------------------------------------
!
!  Before the predictor step of the first barotropic time-step,
!  arrays "rufrc" and "rvfrc" contain the vertical integrals of
!  the 3D right-hand-side terms for momentum equations (including
!  surface and bottom stresses, if so prescribed).
!
!  Convert them into forcing terms by subtracting the fast time
!  "rhs_ubar" and "rhs_vbar" from them; Also, immediately apply
!  these forcing terms "rhs_ubar" and "rhs_vbar".
!
!  From now on, these newly computed forcing terms will remain
!  constant during the fast time stepping and will added to
!  "rhs_ubar" and "rhs_vbar" during all subsequent time steps.
!
      IF (FIRST_2D_STEP.and.PREDICTOR_2D_STEP(ng)) THEN
        IF (iic(ng).eq.ntfirst(ng)) THEN
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+rufrc(i,j)
              ru(i,j,0,nstp)=rufrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              DiaRUfrc(i,j,3,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                &
     &                             DiaRUfrc(i,j,3,M2pgrd)
#  endif
# endif
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+rvfrc(i,j)
              rv(i,j,0,nstp)=rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              DiaRVfrc(i,j,3,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                &
     &                             DiaRVfrc(i,j,3,M2pgrd)
#  endif
# endif
            END DO
          END DO
        ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+                              &
     &                      1.5_r8*rufrc(i,j)-0.5_r8*ru(i,j,0,nnew)
              ru(i,j,0,nstp)=rufrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              1.5_r8*DiaRUfrc(i,j,3,idiag)-       &
     &                              0.5_r8*DiaRUfrc(i,j,nnew,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=1.5_r8*DiaRUfrc(i,j,3,M2sstr)-       &
     &                             0.5_r8*DiaRUfrc(i,j,nnew,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=1.5_r8*DiaRUfrc(i,j,3,M2bstr)-       &
     &                             0.5_r8*DiaRUfrc(i,j,nnew,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#   ifdef WEC_VF
              DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                &
     &                             1.5_r8*DiaRUfrc(i,j,3,M2pgrd)-       &
     &                             0.5_r8*DiaRUfrc(i,j,nnew,M2pgrd)
#   endif
#  endif
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+                              &
     &                      1.5_r8*rvfrc(i,j)-0.5_r8*rv(i,j,0,nnew)
              rv(i,j,0,nstp)=rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              1.5_r8*DiaRVfrc(i,j,3,idiag)-       &
     &                              0.5_r8*DiaRVfrc(i,j,nnew,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=1.5_r8*DiaRVfrc(i,j,3,M2sstr)-       &
     &                             0.5_r8*DiaRVfrc(i,j,nnew,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=1.5_r8*DiaRVfrc(i,j,3,M2bstr)-       &
     &                             0.5_r8*DiaRVfrc(i,j,nnew,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                &
     &                             1.5_r8*DiaRVfrc(i,j,3,M2pgrd)-       &
     &                             0.5_r8*DiaRVfrc(i,j,nnew,M2pgrd)
#  endif
# endif
            END DO
          END DO
        ELSE
          cff1=23.0_r8/12.0_r8
          cff2=16.0_r8/12.0_r8
          cff3= 5.0_r8/12.0_r8
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+                              &
     &                      cff1*rufrc(i,j)-                            &
     &                      cff2*ru(i,j,0,nnew)+                        &
     &                      cff3*ru(i,j,0,nstp)
              ru(i,j,0,nstp)=rufrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              cff1*DiaRUfrc(i,j,3,idiag)-         &
     &                              cff2*DiaRUfrc(i,j,nnew,idiag)+      &
     &                              cff3*DiaRUfrc(i,j,nstp,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=cff1*DiaRUfrc(i,j,3,M2sstr)-         &
     &                             cff2*DiaRUfrc(i,j,nnew,M2sstr)+      &
     &                             cff3*DiaRUfrc(i,j,nstp,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=cff1*DiaRUfrc(i,j,3,M2bstr)-         &
     &                             cff2*DiaRUfrc(i,j,nnew,M2bstr)+      &
     &                             cff3*DiaRUfrc(i,j,nstp,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                &
     &                             cff1*DiaRUfrc(i,j,3,M2pgrd)-         &
     &                             cff2*DiaRUfrc(i,j,nnew,M2pgrd)+      &
     &                             cff3*DiaRUfrc(i,j,nstp,M2pgrd)
#  endif
# endif
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+                              &
     &                      cff1*rvfrc(i,j)-                            &
     &                      cff2*rv(i,j,0,nnew)+                        &
     &                      cff3*rv(i,j,0,nstp)
              rv(i,j,0,nstp)=rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              cff1*DiaRVfrc(i,j,3,idiag)-         &
     &                              cff2*DiaRVfrc(i,j,nnew,idiag)+      &
     &                              cff3*DiaRVfrc(i,j,nstp,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=cff1*DiaRVfrc(i,j,3,M2sstr)-         &
     &                             cff2*DiaRVfrc(i,j,nnew,M2sstr)+      &
     &                             cff3*DiaRVfrc(i,j,nstp,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=cff1*DiaRVfrc(i,j,3,M2bstr)-         &
     &                             cff2*DiaRVfrc(i,j,nnew,M2bstr)+      &
     &                             cff3*DiaRVfrc(i,j,nstp,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                &
     &                             cff1*DiaRVfrc(i,j,3,M2pgrd)-         &
     &                             cff2*DiaRVfrc(i,j,nnew,M2pgrd)+      &
     &                             cff3*DiaRVfrc(i,j,nstp,M2pgrd)
#  endif
# endif
            END DO
          END DO
        END IF
      ELSE
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            rhs_ubar(i,j)=rhs_ubar(i,j)+rufrc(i,j)
# ifdef DIAGNOSTICS_UV
            DO idiag=1,M2pgrd
              DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                  &
     &                            DiaRUfrc(i,j,3,idiag)
            END DO
            DiaU2rhs(i,j,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
            DiaU2rhs(i,j,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
            DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                  &
     &                           DiaRUfrc(i,j,3,M2pgrd)
#  endif
# endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            rhs_vbar(i,j)=rhs_vbar(i,j)+rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
            DO idiag=1,M2pgrd
              DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                  &
     &                            DiaRVfrc(i,j,3,idiag)
            END DO
            DiaV2rhs(i,j,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
            DiaV2rhs(i,j,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
            DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                  &
     &                           DiaRVfrc(i,j,3,M2pgrd)
#  endif
# endif
          END DO
        END DO
      END IF
```

</details>

### 8.2 完整当前 Kokkos 函数

本源码块还保留紧邻函数的原注释和空行；完整函数本身为 `ROMS/Cpp/kokkos_step2d.cpp:585–806`。

<a id="src-c-coup"></a>

**源码块 `C-COUP`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d.cpp`，原始行 **573–807**，共 **235 行**。

<details>
<summary>完整当前 C++ 函数及原始横幅（点击展开完整代码）</summary>

<!-- source-block:C-COUP -->
```cpp
/*======================================================================
** Coupling between 2D and 3D equations.
**
** On the first barotropic predictor step the vertically integrated 3-D
** forcing (rufrc/rvfrc) is turned into a correction: subtract the fast
** rhs, store the result, add it back.  Subsequent steps add the stored
** forcing, with 3/2-1/2 extrapolation on the second 3-D step.
**
** ru(:,:,0,:) and rv(:,:,0,:) hold the correction history; the third
** index 0 is the surface level, which is all this stage touches.
**====================================================================*/

void k_step2d_coupling(int ng, int tile, int istp, int inew,
                       int iic, int ntfirst, int first_2d_step,
                       int predictor,
                       int LBi, int UBi, int LBj, int UBj,
                       int IminS, int ImaxS, int JminS, int JmaxS,
                       void* rufrc_p, void* rvfrc_p,
                       void* ru_p, void* rv_p,
                       void* rhs_ubar_p, void* rhs_vbar_p,
                       void* DiaU2rhs_p, void* DiaV2rhs_p,
                       void* DiaRUfrc_p, void* DiaRVfrc_p,
                       int NDM2d, int M2pgrd, int M2sstr, int M2bstr,
                       const TileBounds* t) {
    roms::initialize();
    ROMS_KK_TIMER("step2d coupling");
    (void)ng;
    (void)tile;

    const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

    Array2d<double> rufrc = grid2d(rufrc_p, s);
    Array2d<double> rvfrc = grid2d(rvfrc_p, s);

    /*
    ** ru/rv are (LBi:UBi, LBj:UBj, 0:UBk, 2) -- OCEAN(ng) % ru is
    ** allocated 0:N(ng).  Only level 0 is touched here, but the k extent
    ** still sets the stride of the trailing time-level dimension, so it
    ** must be the real UBk rather than 0.
    */

    Array4d<double> ru = grid4d(ru_p, s, 0, t->UBk, 1, 2);
    Array4d<double> rv = grid4d(rv_p, s, 0, t->UBk, 1, 2);

    Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
    Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);

    Array3d<double> DiaU2rhs =
        priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
    Array3d<double> DiaV2rhs =
        priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
    Array4d<double> DiaRUfrc =
        grid4d(DiaRUfrc_p, s, 1, 3, 1, NDM2d - 1);
    Array4d<double> DiaRVfrc =
        grid4d(DiaRVfrc_p, s, 1, 3, 1, NDM2d - 1);

    const int iu0 = t->IstrU, iu1 = t->Iend;
    const int iv0 = t->Istr, iv1 = t->Iend;
    const int jv0 = t->Jstr, jv1 = t->Jend;
    const int jstr_v = t->JstrV;

    const bool first = (first_2d_step != 0) && (predictor != 0);

    /* Each branch is one launch: the U rows [Jstr, Jend] contain the V
    ** rows [JstrV, Jend], and U and V touch disjoint arrays, each only
    ** at its own point. */

    if (first && iic == ntfirst) {
        Kokkos::parallel_for(
            range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
                for (int i = iu0; i <= iu1; ++i) {
                    rufrc(i, j) = rufrc(i, j) - rhs_ubar(i, j);
                    rhs_ubar(i, j) += rufrc(i, j);
                    ru(i, j, 0, istp) = rufrc(i, j);
                    for (int id = 1; id <= M2pgrd; ++id) {
                        DiaRUfrc(i, j, 3, id) -= DiaU2rhs(i, j, id);
                        DiaU2rhs(i, j, id) += DiaRUfrc(i, j, 3, id);
                        DiaRUfrc(i, j, istp, id) = DiaRUfrc(i, j, 3, id);
                    }
                    DiaU2rhs(i, j, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
                    DiaRUfrc(i, j, istp, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
                    DiaU2rhs(i, j, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
                    DiaRUfrc(i, j, istp, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
                }
                if (j >= jstr_v) {
                    for (int i = iv0; i <= iv1; ++i) {
                        rvfrc(i, j) = rvfrc(i, j) - rhs_vbar(i, j);
                        rhs_vbar(i, j) += rvfrc(i, j);
                        rv(i, j, 0, istp) = rvfrc(i, j);
                        for (int id = 1; id <= M2pgrd; ++id) {
                            DiaRVfrc(i, j, 3, id) -= DiaV2rhs(i, j, id);
                            DiaV2rhs(i, j, id) += DiaRVfrc(i, j, 3, id);
                            DiaRVfrc(i, j, istp, id) = DiaRVfrc(i, j, 3, id);
                        }
                        DiaV2rhs(i, j, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
                        DiaRVfrc(i, j, istp, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
                        DiaV2rhs(i, j, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
                        DiaRVfrc(i, j, istp, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
                    }
                }
            });
    } else if (first && iic == ntfirst + 1) {
        Kokkos::parallel_for(
            range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
                for (int i = iu0; i <= iu1; ++i) {
                    rufrc(i, j) = rufrc(i, j) - rhs_ubar(i, j);
                    rhs_ubar(i, j) = rhs_ubar(i, j) + 1.5 * rufrc(i, j) - 0.5 * ru(i, j, 0, inew);
                    ru(i, j, 0, istp) = rufrc(i, j);
                    for (int id = 1; id <= M2pgrd; ++id) {
                        DiaRUfrc(i, j, 3, id) -= DiaU2rhs(i, j, id);
                        DiaU2rhs(i, j, id) = DiaU2rhs(i, j, id) + 1.5 * DiaRUfrc(i, j, 3, id) -
                                             0.5 * DiaRUfrc(i, j, inew, id);
                        DiaRUfrc(i, j, istp, id) = DiaRUfrc(i, j, 3, id);
                    }
                    DiaU2rhs(i, j, M2sstr) =
                        1.5 * DiaRUfrc(i, j, 3, M2sstr) -
                        0.5 * DiaRUfrc(i, j, inew, M2sstr);
                    DiaRUfrc(i, j, istp, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
                    DiaU2rhs(i, j, M2bstr) =
                        1.5 * DiaRUfrc(i, j, 3, M2bstr) -
                        0.5 * DiaRUfrc(i, j, inew, M2bstr);
                    DiaRUfrc(i, j, istp, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
                }
                if (j >= jstr_v) {
                    for (int i = iv0; i <= iv1; ++i) {
                        rvfrc(i, j) = rvfrc(i, j) - rhs_vbar(i, j);
                        rhs_vbar(i, j) = rhs_vbar(i, j) + 1.5 * rvfrc(i, j) - 0.5 * rv(i, j, 0, inew);
                        rv(i, j, 0, istp) = rvfrc(i, j);
                        for (int id = 1; id <= M2pgrd; ++id) {
                            DiaRVfrc(i, j, 3, id) -= DiaV2rhs(i, j, id);
                            DiaV2rhs(i, j, id) = DiaV2rhs(i, j, id) + 1.5 * DiaRVfrc(i, j, 3, id) -
                                                 0.5 * DiaRVfrc(i, j, inew, id);
                            DiaRVfrc(i, j, istp, id) = DiaRVfrc(i, j, 3, id);
                        }
                        DiaV2rhs(i, j, M2sstr) =
                            1.5 * DiaRVfrc(i, j, 3, M2sstr) -
                            0.5 * DiaRVfrc(i, j, inew, M2sstr);
                        DiaRVfrc(i, j, istp, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
                        DiaV2rhs(i, j, M2bstr) =
                            1.5 * DiaRVfrc(i, j, 3, M2bstr) -
                            0.5 * DiaRVfrc(i, j, inew, M2bstr);
                        DiaRVfrc(i, j, istp, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
                    }
                }
            });
    } else if (first) {
        /*
        ** Third and later barotropic predictor steps inside the first 3-D
        ** step: the correction is extrapolated from the last three values.
        ** Dropping this branch (folding it into the plain "add rufrc" case)
        ** leaves the first two fast steps exact and then diverges.
        */

        constexpr double cff1 = 23.0 / 12.0;
        constexpr double cff2 = 16.0 / 12.0;
        constexpr double cff3 = 5.0 / 12.0;

        Kokkos::parallel_for(
            range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
                for (int i = iu0; i <= iu1; ++i) {
                    rufrc(i, j) = rufrc(i, j) - rhs_ubar(i, j);
                    rhs_ubar(i, j) = rhs_ubar(i, j) + cff1 * rufrc(i, j) -
                                     cff2 * ru(i, j, 0, inew) +
                                     cff3 * ru(i, j, 0, istp);
                    ru(i, j, 0, istp) = rufrc(i, j);
                    for (int id = 1; id <= M2pgrd; ++id) {
                        DiaRUfrc(i, j, 3, id) -= DiaU2rhs(i, j, id);
                        DiaU2rhs(i, j, id) = DiaU2rhs(i, j, id) + cff1 * DiaRUfrc(i, j, 3, id) -
                                             cff2 * DiaRUfrc(i, j, inew, id) +
                                             cff3 * DiaRUfrc(i, j, istp, id);
                        DiaRUfrc(i, j, istp, id) = DiaRUfrc(i, j, 3, id);
                    }
                    DiaU2rhs(i, j, M2sstr) =
                        cff1 * DiaRUfrc(i, j, 3, M2sstr) -
                        cff2 * DiaRUfrc(i, j, inew, M2sstr) +
                        cff3 * DiaRUfrc(i, j, istp, M2sstr);
                    DiaRUfrc(i, j, istp, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
                    DiaU2rhs(i, j, M2bstr) =
                        cff1 * DiaRUfrc(i, j, 3, M2bstr) -
                        cff2 * DiaRUfrc(i, j, inew, M2bstr) +
                        cff3 * DiaRUfrc(i, j, istp, M2bstr);
                    DiaRUfrc(i, j, istp, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
                }
                if (j >= jstr_v) {
                    for (int i = iv0; i <= iv1; ++i) {
                        rvfrc(i, j) = rvfrc(i, j) - rhs_vbar(i, j);
                        rhs_vbar(i, j) = rhs_vbar(i, j) + cff1 * rvfrc(i, j) -
                                         cff2 * rv(i, j, 0, inew) +
                                         cff3 * rv(i, j, 0, istp);
                        rv(i, j, 0, istp) = rvfrc(i, j);
                        for (int id = 1; id <= M2pgrd; ++id) {
                            DiaRVfrc(i, j, 3, id) -= DiaV2rhs(i, j, id);
                            DiaV2rhs(i, j, id) = DiaV2rhs(i, j, id) + cff1 * DiaRVfrc(i, j, 3, id) -
                                                 cff2 * DiaRVfrc(i, j, inew, id) +
                                                 cff3 * DiaRVfrc(i, j, istp, id);
                            DiaRVfrc(i, j, istp, id) = DiaRVfrc(i, j, 3, id);
                        }
                        DiaV2rhs(i, j, M2sstr) =
                            cff1 * DiaRVfrc(i, j, 3, M2sstr) -
                            cff2 * DiaRVfrc(i, j, inew, M2sstr) +
                            cff3 * DiaRVfrc(i, j, istp, M2sstr);
                        DiaRVfrc(i, j, istp, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
                        DiaV2rhs(i, j, M2bstr) =
                            cff1 * DiaRVfrc(i, j, 3, M2bstr) -
                            cff2 * DiaRVfrc(i, j, inew, M2bstr) +
                            cff3 * DiaRVfrc(i, j, istp, M2bstr);
                        DiaRVfrc(i, j, istp, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
                    }
                }
            });
    } else {
        Kokkos::parallel_for(
            range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
                for (int i = iu0; i <= iu1; ++i) {
                    rhs_ubar(i, j) += rufrc(i, j);
                    for (int id = 1; id <= M2pgrd; ++id) {
                        DiaU2rhs(i, j, id) += DiaRUfrc(i, j, 3, id);
                    }
                    DiaU2rhs(i, j, M2sstr) = DiaRUfrc(i, j, 3, M2sstr);
                    DiaU2rhs(i, j, M2bstr) = DiaRUfrc(i, j, 3, M2bstr);
                }
                if (j >= jstr_v) {
                    for (int i = iv0; i <= iv1; ++i) {
                        rhs_vbar(i, j) += rvfrc(i, j);
                        for (int id = 1; id <= M2pgrd; ++id) {
                            DiaV2rhs(i, j, id) += DiaRVfrc(i, j, 3, id);
                        }
                        DiaV2rhs(i, j, M2sstr) = DiaRVfrc(i, j, 3, M2sstr);
                        DiaV2rhs(i, j, M2bstr) = DiaRVfrc(i, j, 3, M2bstr);
                    }
                }
            });
    }
}

```

</details>

### 8.3 解释

**物理作用与输入输出。** 在每轮快积分首个 predictor 到来前，`rufrc/rvfrc` 含三维动量 RHS 的垂向积分。此时减去本次二维 RHS 得到快模耦合强迫，并立即按慢步历史外推加入二维 RHS；保存该新强迫供本轮后续快步使用。输入/输出还包括 `ru/rv(:,:,0,:)` 的慢时间历史、诊断强迫历史与当前诊断 RHS。

**必须分清的四个分支。** C++ 的 `first = first_2d_step && predictor`，再用慢步 `iic` 判断：

| 实际条件 | 主强迫行为 | 历史处理 |
|---|---|---|
| `first && iic==ntfirst` | 输入三维积分 RHS 减当前二维 RHS；直接加入新强迫 | 保存到 `istp` |
| `first && iic==ntfirst+1` | 生成新强迫，按 `3/2,-1/2` 两历史组合加入 | 先读 `inew`，再写 `istp` |
| 其他 `first` | 生成新强迫，按 `23/12,-16/12,5/12` 三历史组合加入 | 先读 `inew/istp` 旧值，再写 `istp` |
| `!first` | 只把已保存的 `rufrc/rvfrc` 加进当前二维 RHS | 不重新减二维 RHS，不更新慢步强迫历史 |

最后一行也包括**首快步的 corrector**，不是只有后续快步。绝不能概括为“每个 predictor 都更新耦合强迫”。诊断中的压力项之前各贡献、表面应力和底应力按原来相应槽和表达式处理，不能因为主 RHS 看似“减后加”就代数消去历史保存或浮点顺序。

**原始多趟与当前映射。** 原始每个分支分别扫 U 和 V，每个点内再遍历诊断。当前每个分支只 launch 一次，行域 `Jstr:Jend`，先 U `IstrU:Iend`，`j>=JstrV` 再 V `Istr:Iend`，诊断 id 仍在 i 内。它是空间同点、跨慢时间槽的历史更新，没有空间 stencil、垂向遍历或归约。

**真正优化与保留。** U/V 同 dispatch、host 选择时间启动分支、直接借用原历史数组，保留“先读旧历史，再覆盖新强迫”的顺序。只访问 `ru/rv` 的垂向索引 0，仍须按真实 `0:UBk` 包装以保持末维时间槽地址正确；索引 0 在此承担二维强迫历史存储，不能因原 C++ 横幅用了 surface 一词就把它当成遍历三维表层。

**源码注释订正。** 完整 C++ 中第 720–723 行英文注释把第三分支描述成首个三维慢步内第三个及以后快 predictor，不符合实际条件。真正条件是：**从第三个三维慢步起，每个慢步首个快步的 predictor** 才采用三历史外推。正文按 `FIRST_2D_STEP` 宏和 `iic` 分支解释，原注释为保持逐字源码不作修改。

**alias 与写竞争。** 读写的历史依赖都在同一空间点，旧槽读取必须先于覆盖；U/V 是不同数组，j 间没有共享中间写。正确慢步索引与诊断槽、非意外 alias 的字段、串行的快慢步调用关系是前提。逐点并行不能把不同时间调用本身也无序并行。

<a id="momentum-step"></a>

## 9. momentum_step：二维速度推进

本段从计算 `Dstp` 开始，保留首步、后续 predictor、后续 corrector，以及 MASKING/WET_DRY/NESTING 的原始可选代码。当前门禁排除这些可选物理，不能因为完整原件中有它们就认为 C++ 同时支持。

### 9.1 完整原始 Fortran 计算段

<a id="src-f-momt"></a>

**源码块 `F-MOMT`**：提交 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **2487–2674**，共 **188 行**。

<details>
<summary>完整原始 Fortran 计算段（点击展开完整代码）</summary>

<!-- source-block:F-MOMT -->
```fortran
!=======================================================================
!  Time step 2D momentum equations.
!=======================================================================
!
!  Compute total water column depth.
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          Dstp(i,j)=zeta(i,j,kstp)+h(i,j)
        END DO
      END DO
!
!  During the first time-step, the predictor step is Forward-Euler
!  and the corrector step is Backward-Euler. Otherwise, the predictor
!  step is Leap-frog and the corrector step is Adams-Moulton.
!
      IF (FIRST_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)
#ifdef WET_DRY
        cff2=1.0_r8/cff1
#endif
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*cff1*rhs_ubar(i,j))*fac
#ifdef MASKING
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,ubar(i,j,knew))*umask_wet(i,j)
            cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            ubar(i,j,knew)=ubar(i,j,knew)*cff7
            rhs_ubar(i,j)=rhs_ubar(i,j)*cff7
# ifdef SOLVE3D
            IF (PREDICTOR_2D_STEP(ng)) THEN
              rufrc(i,j)=rufrc(i,j)*cff7
              ru(i,j,0,nstp)=rufrc(i,j)
            END IF
# endif
#endif
#if defined NESTING && !defined SOLVE3D
            DU_flux(i,j)=ubar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
#endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*cff1*rhs_vbar(i,j))*fac
#ifdef MASKING
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,vbar(i,j,knew))*vmask_wet(i,j)
            cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            vbar(i,j,knew)=vbar(i,j,knew)*cff7
            rhs_vbar(i,j)=rhs_vbar(i,j)*cff7
# ifdef SOLVE3D
            IF (PREDICTOR_2D_STEP(ng)) THEN
              rvfrc(i,j)=rvfrc(i,j)*cff7
              rv(i,j,0,nstp)=rvfrc(i,j)
            END IF
# endif
#endif
#if defined NESTING && !defined SOLVE3D
            DV_flux(i,j)=vbar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
#endif
          END DO
        END DO
      ELSE IF (PREDICTOR_2D_STEP(ng)) THEN
        cff1=dtfast(ng)
#ifdef WET_DRY
        cff2=1.0_r8/cff1
#endif
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*cff1*rhs_ubar(i,j))*fac
#ifdef MASKING
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,ubar(i,j,knew))*umask_wet(i,j)
            cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            ubar(i,j,knew)=ubar(i,j,knew)*cff7
            rhs_ubar(i,j)=rhs_ubar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DU_flux(i,j)=ubar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
#endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*cff1*rhs_vbar(i,j))*fac
#ifdef MASKING
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,vbar(i,j,knew))*vmask_wet(i,j)
            cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            vbar(i,j,knew)=vbar(i,j,knew)*cff7
            rhs_vbar(i,j)=rhs_vbar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DV_flux(i,j)=vbar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
#endif
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)*5.0_r8/12.0_r8
        cff2=0.5_r8*dtfast(ng)*8.0_r8/12.0_r8
        cff3=0.5_r8*dtfast(ng)*1.0_r8/12.0_r8
#ifdef WET_DRY
        cff4=1.0_r8/cff1
#endif
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*(cff1*rhs_ubar(i,j)+                    &
     &                           cff2*rubar(i,j,kstp)-                  &
     &                           cff3*rubar(i,j,ptsk)))*fac
#ifdef MASKING
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,ubar(i,j,knew))*umask_wet(i,j)
            cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            ubar(i,j,knew)=ubar(i,j,knew)*cff7
            rhs_ubar(i,j)=rhs_ubar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DU_flux(i,j)=ubar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
#endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*(cff1*rhs_vbar(i,j)+                    &
     &                           cff2*rvbar(i,j,kstp)-                  &
     &                           cff3*rvbar(i,j,ptsk)))*fac
#ifdef MASKING
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,vbar(i,j,knew))*vmask_wet(i,j)
            cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            vbar(i,j,knew)=vbar(i,j,knew)*cff7
            rhs_vbar(i,j)=rhs_vbar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DV_flux(i,j)=vbar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
#endif
          END DO
        END DO
      END IF
```

</details>

### 9.2 完整当前 Kokkos 函数

本源码块还保留紧邻函数的原注释和空行；完整函数本身为 `ROMS/Cpp/kokkos_step2d.cpp:819–975`。

<a id="src-c-momt"></a>

**源码块 `C-MOMT`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d.cpp`，原始行 **808–976**，共 **169 行**。

<details>
<summary>完整当前 C++ 函数及原始横幅（点击展开完整代码）</summary>

<!-- source-block:C-MOMT -->
```cpp
/*======================================================================
** Time step 2D momentum equations.
**
**   Dstp(i,j) = zeta(i,j,kstp) + h(i,j)
**   ubar(:,:,knew) = (ubar(:,:,kstp)*(Dstp(i,j)+Dstp(i-1,j))
**                     + pm*pn*dtfast*rhs_ubar) / (Dnew(i,j)+Dnew(i-1,j))
**
** Forward Euler, Leap-frog and Adams-Moulton differ only in the
** constant on rhs_ubar (and, for AM3, the extra rubar history terms).
**====================================================================*/

void k_step2d_momentum_step(int ng, int tile, int kstp, int knew,
                            int LBi, int UBi, int LBj, int UBj,
                            int IminS, int ImaxS, int JminS, int JmaxS,
                            void* zeta_p, void* h_p,
                            void* ubar_p, void* vbar_p,
                            void* rubar_p, void* rvbar_p,
                            void* pm_p, void* pn_p,
                            void* rhs_ubar_p, void* rhs_vbar_p,
                            void* Dstp_p, void* Dnew_p,
                            int predictor, int first_2d_step, double dtfast,
                            const TileBounds* t) {
    roms::initialize();
    ROMS_KK_TIMER("step2d momentum_step");
    (void)ng;
    (void)tile;

    const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

    Array3d<double> zeta = roms::as_3d<double>(zeta_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
    Array3d<double> ubar = roms::as_3d<double>(ubar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
    Array3d<double> vbar = roms::as_3d<double>(vbar_p, LBi, UBi, LBj, UBj, TLEV0, TLEV1);
    Array3d<double> rubar = roms::as_3d<double>(rubar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);
    Array3d<double> rvbar = roms::as_3d<double>(rvbar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);

    Array2d<double> h = grid2d(h_p, s);
    Array2d<double> pm = grid2d(pm_p, s);
    Array2d<double> pn = grid2d(pn_p, s);

    Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
    Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);
    Array2d<double> Dstp = priv2d(Dstp_p, s);
    Array2d<double> Dnew = priv2d(Dnew_p, s);

    const int ptsk = 3 - kstp;

    const int j0 = t->JstrV - 1, j1 = t->Jend;
    const int i0 = t->IstrU - 1, i1 = t->Iend;

    const int iu0 = t->IstrU, iu1 = t->Iend;
    const int iv0 = t->Istr, iv1 = t->Iend;
    const int ju0 = t->Jstr;
    const int jstr_v = t->JstrV;

    /*
    ** One launch per call.  Each row stores its Dstp over [IstrU-1, Iend]
    ** and then advances ubar (rows [Jstr, Jend]) and vbar (rows
    ** [JstrV, Jend]), all inside [JstrV-1, Jend].  The vbar update also
    ** needs Dstp of row j-1, which another work item owns; it is
    ** recomputed from the same operands, zeta(kstp) + h, so the sum is the
    ** identical value rather than a racing read.  Every other input is
    ** read-only and every output is written at its own point.
    */

    if (first_2d_step) {
        const double cff1 = 0.5 * dtfast;

        Kokkos::parallel_for(
            range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
                for (int i = i0; i <= i1; ++i) {
                    Dstp(i, j) = zeta(i, j, kstp) + h(i, j);
                }
                if (j >= ju0) {
                    for (int i = iu0; i <= iu1; ++i) {
                        const double cff =
                            (pm(i, j) + pm(i - 1, j)) * (pn(i, j) + pn(i - 1, j));
                        const double div = 1.0 / (Dnew(i, j) + Dnew(i - 1, j));
                        ubar(i, j, knew) =
                            (ubar(i, j, kstp) * (Dstp(i, j) + Dstp(i - 1, j)) +
                             cff * cff1 * rhs_ubar(i, j)) *
                            div;
                    }
                }
                if (j >= jstr_v) {
                    for (int i = iv0; i <= iv1; ++i) {
                        const double dstp_s = zeta(i, j - 1, kstp) + h(i, j - 1);
                        const double cff =
                            (pm(i, j) + pm(i, j - 1)) * (pn(i, j) + pn(i, j - 1));
                        const double div = 1.0 / (Dnew(i, j) + Dnew(i, j - 1));
                        vbar(i, j, knew) =
                            (vbar(i, j, kstp) * (Dstp(i, j) + dstp_s) +
                             cff * cff1 * rhs_vbar(i, j)) *
                            div;
                    }
                }
            });
    } else if (predictor) {
        const double cff1 = dtfast;

        Kokkos::parallel_for(
            range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
                for (int i = i0; i <= i1; ++i) {
                    Dstp(i, j) = zeta(i, j, kstp) + h(i, j);
                }
                if (j >= ju0) {
                    for (int i = iu0; i <= iu1; ++i) {
                        const double cff =
                            (pm(i, j) + pm(i - 1, j)) * (pn(i, j) + pn(i - 1, j));
                        const double div = 1.0 / (Dnew(i, j) + Dnew(i - 1, j));
                        ubar(i, j, knew) =
                            (ubar(i, j, kstp) * (Dstp(i, j) + Dstp(i - 1, j)) +
                             cff * cff1 * rhs_ubar(i, j)) *
                            div;
                    }
                }
                if (j >= jstr_v) {
                    for (int i = iv0; i <= iv1; ++i) {
                        const double dstp_s = zeta(i, j - 1, kstp) + h(i, j - 1);
                        const double cff =
                            (pm(i, j) + pm(i, j - 1)) * (pn(i, j) + pn(i, j - 1));
                        const double div = 1.0 / (Dnew(i, j) + Dnew(i, j - 1));
                        vbar(i, j, knew) =
                            (vbar(i, j, kstp) * (Dstp(i, j) + dstp_s) +
                             cff * cff1 * rhs_vbar(i, j)) *
                            div;
                    }
                }
            });
    } else {
        const double cff1 = 0.5 * dtfast * 5.0 / 12.0;
        const double cff2 = 0.5 * dtfast * 8.0 / 12.0;
        const double cff3 = 0.5 * dtfast * 1.0 / 12.0;

        Kokkos::parallel_for(
            range_policy(j0, j1 + 1), KOKKOS_LAMBDA(int j) {
                for (int i = i0; i <= i1; ++i) {
                    Dstp(i, j) = zeta(i, j, kstp) + h(i, j);
                }
                if (j >= ju0) {
                    for (int i = iu0; i <= iu1; ++i) {
                        const double cff =
                            (pm(i, j) + pm(i - 1, j)) * (pn(i, j) + pn(i - 1, j));
                        const double div = 1.0 / (Dnew(i, j) + Dnew(i - 1, j));
                        ubar(i, j, knew) =
                            (ubar(i, j, kstp) * (Dstp(i, j) + Dstp(i - 1, j)) +
                             cff * (cff1 * rhs_ubar(i, j) +
                                    cff2 * rubar(i, j, kstp) -
                                    cff3 * rubar(i, j, ptsk))) *
                            div;
                    }
                }
                if (j >= jstr_v) {
                    for (int i = iv0; i <= iv1; ++i) {
                        const double dstp_s = zeta(i, j - 1, kstp) + h(i, j - 1);
                        const double cff =
                            (pm(i, j) + pm(i, j - 1)) * (pn(i, j) + pn(i, j - 1));
                        const double div = 1.0 / (Dnew(i, j) + Dnew(i, j - 1));
                        vbar(i, j, knew) =
                            (vbar(i, j, kstp) * (Dstp(i, j) + dstp_s) +
                             cff * (cff1 * rhs_vbar(i, j) +
                                    cff2 * rvbar(i, j, kstp) -
                                    cff3 * rvbar(i, j, ptsk))) *
                            div;
                    }
                }
            });
    }
}

```

</details>

### 9.3 解释

**物理作用与输入输出。** 使用已组装且完成耦合的 `rhs_ubar/rhs_vbar`，以及 `Dstp`、`Dnew`、网格度量和必要的 RHS 历史，推进深度平均速度到 `knew`。旧速度先乘旧相邻水深之和，加动量增量后再除新相邻水深之和；所以输出是速度，不是 `DUon/DVom` 体积输运。

**原始多趟。** 先完整生成 `Dstp = zeta(:,:,kstp)+h`，然后选择首步/后续 predictor/corrector，分别完整扫 U 与 V。首步按原注释区分 FE predictor 与 BE corrector；后续 predictor 用 LF，corrector 用 AM3 和 `rubar/rvbar` 两历史槽。原始中的湿干限制、掩码和纯二维 nesting 输运另外存在，但不在当前支持组里。

**计算模式与映射。** 速度与 RHS 历史是同点时间递推，水深/度量插值读西邻或南邻，是浅邻点 stencil 与时间更新的组合，不是纯 radius-0 或求和归约。三条互斥 launch 都覆盖 `JstrV-1:Jend`：每行先写 `IstrU-1:Iend` 的 `Dstp`，满足 `j>=Jstr` 后做 U，满足 `j>=JstrV` 后做 V。

**真正优化。** 将 Dstp 生成与 U/V 推进合并一次 dispatch，host 选择积分分支；V 的南邻 `Dstp` 不从另一工作项正在写的 scratch 读取，而是重算同样的 `zeta(i,j-1,kstp)+h(i,j-1)`。这与 depth kernel 的取舍相同：额外输入读取/加法换去跨行依赖。`Dnew` 是先前 zeta 阶段已经完成的只读输入，读取它的南邻不构成同类写读竞争。

**未改、alias 与成立条件。** `Dstp` 明确仍是 scratch，每个时间分支都写完整所需范围，不能称 scratch-free。此入口不写 zeta，U/V 的旧速度只从同点读取，即使合法时间层槽重用也按表达式先读后写；没有读取邻点速度后再覆盖它的模式。仍要求水深分母符合原物理前提、不同字段不意外重叠、前一入口和边界阶段有正确先后关系。没有新增干水深保护来替代原算法，也没有证明任意湿干配置可用。

<a id="diag-int"></a>

## 10. diag_int：动量诊断积分

这里完整呈现原始 `DIAGNOSTICS_UV`（第 2676 行开启）之下的 `SOLVE3D` 分支（第 2699 行开启）核心。片段含 `fac`、首 corrector 初始化和后续 corrector 累积；外层 CPP 未伪造补写。第 2755–2834 行的纯二维诊断分支不属于本入口，仍完整保存在附录 A。

### 10.1 完整原始 Fortran 计算段

<a id="src-f-diag"></a>

**源码块 `F-DIAG`**：提交 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **2700–2754**，共 **55 行**。

<details>
<summary>完整原始 Fortran 计算段（点击展开完整代码）</summary>

<!-- source-block:F-DIAG -->
```fortran
!
!  The arrays "DiaU2rhs" and "DiaV2rhs" contain the contributions of
!  each of the 2D right-hand-side terms for the momentum equations.
!
!  These values are integrated, time-stepped and converted to mass flux
!  units (m3 s-1) for coupling with the 3D diagnostic terms.
!
      fac=weight(1,iif(ng),ng)
      IF (FIRST_2D_STEP.and.CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaU2int(i,j,idiag)=cff1*DiaU2rhs(i,j,idiag)
              DiaU2wrk(i,j,idiag)=DiaU2int(i,j,idiag)*                  &
     &                            (pm(i-1,j)+pm(i,j))*fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaV2int(i,j,idiag)=cff1*DiaV2rhs(i,j,idiag)
              DiaV2wrk(i,j,idiag)=DiaV2int(i,j,idiag)*                  &
     &                            (pn(i,j)+pn(i,j-1))*fac
            END DO
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)*5.0_r8/12.0_r8
        cff2=0.5_r8*dtfast(ng)*8.0_r8/12.0_r8
        cff3=0.5_r8*dtfast(ng)*1.0_r8/12.0_r8
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaU2int(i,j,idiag)=DiaU2int(i,j,idiag)+                  &
     &                            (cff1*DiaU2rhs(i,j,idiag)+            &
     &                             cff2*DiaRUbar(i,j,kstp,idiag)-       &
     &                             cff3*DiaRUbar(i,j,ptsk,idiag))
              DiaU2wrk(i,j,idiag)=DiaU2wrk(i,j,idiag)+                  &
     &                            DiaU2int(i,j,idiag)*                  &
     &                            (pm(i-1,j)+pm(i,j))*fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaV2int(i,j,idiag)=DiaV2int(i,j,idiag)+                  &
     &                            (cff1*DiaV2rhs(i,j,idiag)+            &
     &                             cff2*DiaRVbar(i,j,kstp,idiag)-       &
     &                             cff3*DiaRVbar(i,j,ptsk,idiag))
              DiaV2wrk(i,j,idiag)=DiaV2wrk(i,j,idiag)+                  &
     &                            DiaV2int(i,j,idiag)*                  &
     &                            (pn(i,j)+pn(i,j-1))*fac
            END DO
          END DO
        END DO
      END IF
```

</details>

### 10.2 完整当前 Kokkos 函数

本源码块还保留紧邻函数的原注释和空行；完整函数本身为 `ROMS/Cpp/kokkos_step2d.cpp:1000–1080`。

<a id="src-c-diag"></a>

**源码块 `C-DIAG`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d.cpp`，原始行 **977–1081**，共 **105 行**。

<details>
<summary>完整当前 C++ 函数及原始横幅（点击展开完整代码）</summary>

<!-- source-block:C-DIAG -->
```cpp
/*======================================================================
** Time step 2D momentum diagnostic terms (SOLVE3D).
**
** Corrector steps only.  On the first barotropic step
**
**   DiaU2int = cff1*DiaU2rhs
**   DiaU2wrk = DiaU2int*(pm(i-1,j)+pm(i,j))*fac
**
** and on every later one the Adams-Moulton accumulation
**
**   DiaU2int = DiaU2int + (cff1*DiaU2rhs + cff2*DiaRUbar(kstp)
**                                         - cff3*DiaRUbar(ptsk))
**   DiaU2wrk = DiaU2wrk + DiaU2int*(pm(i-1,j)+pm(i,j))*fac
**
** with pn(i,j)+pn(i,j-1) for the V terms.  The caller computes cff1-3
** and fac exactly as the Fortran block does.  All updates are pointwise
** in (i, j, idiag), so U and V of a row share one launch.
**
** This block makes NDM2d-1 passes over each of U and V per corrector
** call, reading five fields and writing two.  It was the largest piece
** of step2d left on the management core.
**====================================================================*/

void k_step2d_diag_int(int ng, int tile, int kstp, int first_2d_step,
                       int LBi, int UBi, int LBj, int UBj,
                       int IminS, int ImaxS, int JminS, int JmaxS,
                       void* pm_p, void* pn_p,
                       void* DiaU2rhs_p, void* DiaV2rhs_p,
                       void* DiaRUbar_p, void* DiaRVbar_p,
                       void* DiaU2int_p, void* DiaV2int_p,
                       void* DiaU2wrk_p, void* DiaV2wrk_p,
                       int NDM2d, double cff1, double cff2, double cff3,
                       double fac, const TileBounds* t) {
    roms::initialize();
    ROMS_KK_TIMER("step2d diag_int");
    (void)ng;
    (void)tile;

    const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

    Array2d<double> pm = grid2d(pm_p, s);
    Array2d<double> pn = grid2d(pn_p, s);

    /* DiaU2rhs/DiaV2rhs are tile scratch; the other four are full fields. */
    Array3d<double> DiaU2rhs = priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
    Array3d<double> DiaV2rhs = priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);
    Array4d<double> DiaRUbar = grid4d(DiaRUbar_p, s, 1, 2, 1, NDM2d - 1);
    Array4d<double> DiaRVbar = grid4d(DiaRVbar_p, s, 1, 2, 1, NDM2d - 1);
    Array3d<double> DiaU2int = grid3d(DiaU2int_p, s, 1, NDM2d);
    Array3d<double> DiaV2int = grid3d(DiaV2int_p, s, 1, NDM2d);
    Array3d<double> DiaU2wrk = grid3d(DiaU2wrk_p, s, 1, NDM2d);
    Array3d<double> DiaV2wrk = grid3d(DiaV2wrk_p, s, 1, NDM2d);

    const int ptsk = 3 - kstp;
    const int nterm = NDM2d - 1;
    const int iu0 = t->IstrU, iv0 = t->Istr, i1 = t->Iend;
    const int Jstr = t->Jstr, Jend = t->Jend, JstrV = t->JstrV;

    if (first_2d_step) {
        Kokkos::parallel_for(
            range_policy(Jstr, Jend + 1), KOKKOS_LAMBDA(int j) {
                for (int id = 1; id <= nterm; ++id) {
                    for (int i = iu0; i <= i1; ++i) {
                        const double v = cff1 * DiaU2rhs(i, j, id);
                        DiaU2int(i, j, id) = v;
                        DiaU2wrk(i, j, id) = v * (pm(i - 1, j) + pm(i, j)) * fac;
                    }
                    if (j >= JstrV) {
                        for (int i = iv0; i <= i1; ++i) {
                            const double v = cff1 * DiaV2rhs(i, j, id);
                            DiaV2int(i, j, id) = v;
                            DiaV2wrk(i, j, id) = v * (pn(i, j) + pn(i, j - 1)) * fac;
                        }
                    }
                }
            });
    } else {
        Kokkos::parallel_for(
            range_policy(Jstr, Jend + 1), KOKKOS_LAMBDA(int j) {
                for (int id = 1; id <= nterm; ++id) {
                    for (int i = iu0; i <= i1; ++i) {
                        const double v = DiaU2int(i, j, id) +
                                         (cff1 * DiaU2rhs(i, j, id) +
                                          cff2 * DiaRUbar(i, j, kstp, id) -
                                          cff3 * DiaRUbar(i, j, ptsk, id));
                        DiaU2int(i, j, id) = v;
                        DiaU2wrk(i, j, id) =
                            DiaU2wrk(i, j, id) + v * (pm(i - 1, j) + pm(i, j)) * fac;
                    }
                    if (j >= JstrV) {
                        for (int i = iv0; i <= i1; ++i) {
                            const double v = DiaV2int(i, j, id) +
                                             (cff1 * DiaV2rhs(i, j, id) +
                                              cff2 * DiaRVbar(i, j, kstp, id) -
                                              cff3 * DiaRVbar(i, j, ptsk, id));
                            DiaV2int(i, j, id) = v;
                            DiaV2wrk(i, j, id) =
                                DiaV2wrk(i, j, id) + v * (pn(i, j) + pn(i, j - 1)) * fac;
                        }
                    }
                }
            });
    }
}

```

</details>

### 10.3 解释

**物理作用与输入输出。** 把每项二维动量 RHS 贡献按与动量一致的时间权重积分，并按 `weight(1,iif,ng)` 和网格度量转为用于三维诊断耦合的累计量。输入当前诊断 RHS、`DiaRUbar/DiaRVbar` 历史、`pm/pn` 和 Fortran 算出的系数；读写 `DiaU2int/DiaV2int` 与 `DiaU2wrk/DiaV2wrk`。实际处理诊断项 `1:NDM2d-1`，不是遍历垂向层。

**真实调用约束。** C++ 自身只判断 `first_2d_step`，不带 predictor 参数；Fortran 第 `2964–2984` 行先确保 `CORRECTOR_2D_STEP` 才调用。首快步 corrector 初始化积分与工作量，后续 corrector 采用 `5/12,8/12,-1/12` 历史组合继续累计。predictor 不能把这两个分支再执行一次。

**原始与当前循环。** 原始 `idiag → j → i(U)` 与 `j → i(V)` 分开扫；当前将 j 外提到 `RangePolicy(Jstr,Jend+1)`，行内为 `id → i(U) → 条件 i(V)`。每个 id 都保持 i 连续；V 要求 `j>=JstrV`。没有 `k` 循环，也没有跨诊断项的求和归约。

**计算模式。** 诊断状态/时间历史更新都是同点，但 U 读取 `pm(i-1,j)`，V 读取 `pn(i,j-1)`；因此整个 kernel 不能严格说成“任意条件下 radius 0、不读邻居”。度量只读，读取邻行度量无需等待另一工作项写它。

**真正优化与代价。** U/V 同 launch、host 区分首步、用局部 `v` 连接新积分与工作量更新。当前并没有把度量系数显式缓存在诊断项 id 之外；后续 share 的 i-blocked 变体不是这份代码。源码横幅的“五个场读、两个场写”是概述，不能直接等同每格 DRAM 访问次数、静态字节模型或实测带宽。

**alias 与写竞争。** 每个 `(i,j,id)` 只由一条工作项/循环路径更新，旧 int/wrk 在本点先读后写；pm/pn 与 RHS 历史只读。int、wrk、rhs、历史数组要保持原存储关系，不能任意重叠。不同 id 的循环重排成立也依赖各诊断项是独立存储，而非通过错误 stride 混在一起。

<a id="store-rhs"></a>

## 11. store_rhs：保存 predictor RHS 历史

原始段保留完整外层 predictor `IF`、U/V 主 RHS 复制、`DIAGNOSTICS_UV` 分支及诊断复制。当前 C++ 的 predictor guard 位于 Fortran 调用端；没有把它从模型删掉，附录 D 给出实际 `IF` 和调用。

### 11.1 完整原始 Fortran 计算段

<a id="src-f-stor"></a>

**源码块 `F-STOR`**：提交 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **2836–2865**，共 **30 行**。

<details>
<summary>完整原始 Fortran 计算段（点击展开完整代码）</summary>

<!-- source-block:F-STOR -->
```fortran
!
!  If predictor step, load right-side-term into shared arrays for
!  future use during the subsequent corrector step.
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            rubar(i,j,krhs)=rhs_ubar(i,j)
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            rvbar(i,j,krhs)=rhs_vbar(i,j)
          END DO
        END DO
#ifdef DIAGNOSTICS_UV
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaRUbar(i,j,krhs,idiag)=DiaU2rhs(i,j,idiag)
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaRVbar(i,j,krhs,idiag)=DiaV2rhs(i,j,idiag)
            END DO
          END DO
        END DO
#endif
      END IF
```

</details>

### 11.2 完整当前 Kokkos 函数

本源码块还保留紧邻函数的原注释和空行；完整函数本身为 `ROMS/Cpp/kokkos_step2d.cpp:1090–1150`。

<a id="src-c-stor"></a>

**源码块 `C-STOR`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d.cpp`，原始行 **1082–1151**，共 **70 行**。

<details>
<summary>完整当前 C++ 函数及原始横幅（点击展开完整代码）</summary>

<!-- source-block:C-STOR -->
```cpp
/*======================================================================
** If predictor step, load right-side-term into shared arrays for
** future use during the subsequent corrector step.
**
**   rubar(i,j,krhs) = rhs_ubar(i,j)
**   rvbar(i,j,krhs) = rhs_vbar(i,j)
**====================================================================*/

void k_step2d_store_rhs(int ng, int tile, int krhs,
                        int LBi, int UBi, int LBj, int UBj,
                        int IminS, int ImaxS, int JminS, int JmaxS,
                        void* rubar_p, void* rvbar_p,
                        void* rhs_ubar_p, void* rhs_vbar_p,
                        void* DiaRUbar_p, void* DiaRVbar_p,
                        void* DiaU2rhs_p, void* DiaV2rhs_p,
                        int NDM2d,
                        const TileBounds* t) {
    roms::initialize();
    ROMS_KK_TIMER("step2d store_rhs");
    (void)ng;
    (void)tile;

    const Shape s{LBi, UBi, LBj, UBj, IminS, ImaxS, JminS, JmaxS};

    Array3d<double> rubar = roms::as_3d<double>(rubar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);
    Array3d<double> rvbar = roms::as_3d<double>(rvbar_p, LBi, UBi, LBj, UBj, RLEV0, RLEV1);

    Array2d<double> rhs_ubar = priv2d(rhs_ubar_p, s);
    Array2d<double> rhs_vbar = priv2d(rhs_vbar_p, s);

    Array4d<double> DiaRUbar =
        grid4d(DiaRUbar_p, s, 1, 2, 1, NDM2d - 1);
    Array4d<double> DiaRVbar =
        grid4d(DiaRVbar_p, s, 1, 2, 1, NDM2d - 1);
    Array3d<double> DiaU2rhs =
        priv3d(DiaU2rhs_p, s, 1, NDM2d - 1);
    Array3d<double> DiaV2rhs =
        priv3d(DiaV2rhs_p, s, 1, NDM2d - 1);

    const int iu0 = t->IstrU, iu1 = t->Iend;
    const int iv0 = t->Istr, iv1 = t->Iend;
    const int jv0 = t->Jstr, jv1 = t->Jend;
    const int jstr_v = t->JstrV;

    /* The diagnostic index is the outer loop.  With it innermost the
    ** vectoriser refuses the whole body ("not enough data-refs"); hoisted,
    ** every pass is a contiguous copy along i.  U and V share one launch. */
    Kokkos::parallel_for(
        range_policy(jv0, jv1 + 1), KOKKOS_LAMBDA(int j) {
            for (int i = iu0; i <= iu1; ++i) {
                rubar(i, j, krhs) = rhs_ubar(i, j);
            }
            for (int id = 1; id < NDM2d; ++id) {
                for (int i = iu0; i <= iu1; ++i) {
                    DiaRUbar(i, j, krhs, id) = DiaU2rhs(i, j, id);
                }
            }
            if (j >= jstr_v) {
                for (int i = iv0; i <= iv1; ++i) {
                    rvbar(i, j, krhs) = rhs_vbar(i, j);
                }
                for (int id = 1; id < NDM2d; ++id) {
                    for (int i = iv0; i <= iv1; ++i) {
                        DiaRVbar(i, j, krhs, id) = DiaV2rhs(i, j, id);
                    }
                }
            }
        });
}

```

</details>

### 11.3 解释

**复制方向是关键。** 这是 predictor 结束时**保存** RHS 历史，供随后 corrector 使用：`rhs_ubar/rhs_vbar → rubar/rvbar(:,:,krhs)`，`DiaU2rhs/DiaV2rhs → DiaRUbar/DiaRVbar(:,:,krhs,id)`。不是从共享历史数组加载当前 RHS。源是 tile-private scratch，目标是 grid-bounded 历史数组。

**原始循环。** 原始在 predictor 下先各扫 U/V 主 RHS，再以 `idiag` 外循环扫 U/V 诊断。当前调用端仍只在 predictor 进入；函数内部不再判断 predictor，不能拿它当任意时刻都可调用的无条件保存接口。

**计算模式与映射。** 它是严格同点、同时间槽的复制，没有空间邻点 stencil，也没有归约。行域 `Jstr:Jend`；每行先复制 U 主 RHS，再 `id→i` 复制 U 诊断；满足 `j>=JstrV` 后才做 V 主 RHS 与 V 诊断。诊断项仍为 `1:NDM2d-1`。

**真正优化。** 用一个 dispatch 承载 U/V 和诊断复制；id 外提使 i 最内层连续，对向量化更友好。英文注释引用了曾经的编译器现象，但本文没有重跑向量化报告或计时，不能据此断言当前编译一定生成 SIMD 或量化其收益。

**alias 与写竞争。** 不同 `(i,j,krhs,id)` 写入互不冲突，读取源 scratch 不读邻行。必须保证 grid/private 的 stride 包装正确、目标历史槽合法、tile 输出所有权正确，且复制源与目标没有意外重叠。它保存本次已经完成耦合的 RHS，并没有替代最终速度边界、点源或通信。

<a id="appendix-original"></a>

## 附录 A：完整原始 step2d 文件

下面是移植前 3058 行原始 `step2d_LF_AM3.h` 全文；包括上层入口、`step2d_tile` 参数/声明、所有 CPP 分支、临时数组、外围未移植物理、点源、边界、通信和结束部分。正文的八个片段与这里重复，是为同时满足按入口学习和准确查阅外围上下文。

这是 ROMS 构建体系纳入的 Fortran 头文件，不是脱离模块、include 文件和 CPP 配置就能单独编译的程序。其内部引用的 `mod_*`、边界/通信子程序仍属于 ROMS；本文没有把所有 ROMS 源码一并复制。

<a id="src-f-original-all"></a>

**源码块 `F-ORIGINAL-ALL`**：提交 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **1–3058**，共 **3058 行**。

<details>
<summary>完整原始 step2d_LF_AM3.h（3058 行）（点击展开完整代码）</summary>

<!-- source-block:F-ORIGINAL-ALL -->
```fortran
      MODULE step2d_mod
!
!git $Id$
!=======================================================================
!                                                                      !
!  Nonlinear shallow-water primitive equations predictor (Leap-frog)   !
!  and corrector (Adams-Moulton) time-stepping engine.                 !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: step2d
!
      CONTAINS
!
      SUBROUTINE step2d (ng, tile)

      USE mod_param
#ifdef SOLVE3D
      USE mod_coupling
#endif
#ifdef DIAGNOSTICS_UV
      USE mod_diags
#endif
      USE mod_forces
      USE mod_grid
#if defined UV_VIS2 || defined UV_VIS4
      USE mod_mixing
#endif
      USE mod_ocean
#if defined SEDIMENT && defined SED_MORPH && defined SOLVE3D
      USE mod_sedbed
#endif
      USE mod_stepping
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile =                          &
     &  __FILE__
!
#include "tile.h"
!
#ifdef PROFILE
      CALL wclock_on (ng, iNLM, 9, __LINE__, MyFile)
#endif
      CALL step2d_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj, N(ng),                      &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
     &                  krhs(ng), kstp(ng), knew(ng),                   &
#ifdef SOLVE3D
     &                  nstp(ng), nnew(ng),                             &
#endif
#ifdef MASKING
     &                  GRID(ng) % pmask,       GRID(ng) % rmask,       &
     &                  GRID(ng) % umask,       GRID(ng) % vmask,       &
#endif
#ifdef WET_DRY
     &                  GRID(ng) % pmask_wet,   GRID(ng) % pmask_full,  &
     &                  GRID(ng) % rmask_wet,   GRID(ng) % rmask_full,  &
     &                  GRID(ng) % umask_wet,   GRID(ng) % umask_full,  &
     &                  GRID(ng) % vmask_wet,   GRID(ng) % vmask_full,  &
# ifdef SOLVE3D
     &                  GRID(ng) % rmask_wet_avg,                       &
# endif
#endif
     &                  GRID(ng) % fomn,        GRID(ng) % h,           &
     &                  GRID(ng) % om_u,        GRID(ng) % om_v,        &
     &                  GRID(ng) % on_u,        GRID(ng) % on_v,        &
     &                  GRID(ng) % omn,                                 &
     &                  GRID(ng) % pm,          GRID(ng) % pn,          &
#if defined CURVGRID && defined UV_ADV
     &                  GRID(ng) % dndx,        GRID(ng) % dmde,        &
#endif
#if defined UV_VIS2 || defined UV_VIS4
     &                  GRID(ng) % pmon_r,      GRID(ng) % pnom_r,      &
     &                  GRID(ng) % pmon_p,      GRID(ng) % pnom_p,      &
     &                  GRID(ng) % om_r,        GRID(ng) % on_r,        &
     &                  GRID(ng) % om_p,        GRID(ng) % on_p,        &
# ifdef UV_VIS2
     &                  MIXING(ng) % visc2_p,   MIXING(ng) % visc2_r,   &
# endif
# ifdef UV_VIS4
     &                  MIXING(ng) % visc4_p,   MIXING(ng) % visc4_r,   &
# endif
#endif
#if defined SEDIMENT && defined SED_MORPH
     &                 SEDBED(ng) % bed_thick,                          &
#endif

#ifdef WEC
# ifdef WEC_VF
#  ifdef WEC_ROLLER
     &                  MIXING(ng) % rurol2d,                           &
     &                  MIXING(ng) % rvrol2d,                           &
#  endif
#  ifdef BOTTOM_STREAMING
     &                  MIXING(ng) % rubst2d,                           &
     &                  MIXING(ng) % rvbst2d,                           &
#  endif
#  ifdef SURFACE_STREAMING
     &                  MIXING(ng) % russt2d,                           &
     &                  MIXING(ng) % rvsst2d,                           &
#  endif
     &                  MIXING(ng) % rubrk2d,                           &
     &                  MIXING(ng) % rvbrk2d,                           &
     &                  MIXING(ng) % rukvf2d,                           &
     &                  MIXING(ng) % rvkvf2d,                           &
     &                  OCEAN(ng) % bh,                                 &
     &                  OCEAN(ng) % qsp,                                &
     &                  OCEAN(ng) % zetaw,                              &
# endif
     &                  OCEAN(ng) % ubar_stokes,                        &
     &                  OCEAN(ng) % vbar_stokes,                        &
#endif
#if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
     &                  OCEAN(ng) % eq_tide,                            &
#endif
#ifndef SOLVE3D
     &                  FORCES(ng) % sustr,     FORCES(ng) % svstr,     &
     &                  FORCES(ng) % bustr,     FORCES(ng) % bvstr,     &
# ifdef ATM_PRESS
     &                  FORCES(ng) % Pair,                              &
# endif
#else
# ifdef VAR_RHO_2D
     &                  COUPLING(ng) % rhoA,    COUPLING(ng) % rhoS,    &
# endif
     &                  COUPLING(ng) % DU_avg1, COUPLING(ng) % DU_avg2, &
     &                  COUPLING(ng) % DV_avg1, COUPLING(ng) % DV_avg2, &
     &                  COUPLING(ng) % Zt_avg1,                         &
     &                  COUPLING(ng) % rufrc,   COUPLING(ng) % rvfrc,   &
     &                  OCEAN(ng) % ru,         OCEAN(ng) % rv,         &
#endif
#ifdef DIAGNOSTICS_UV
     &                  DIAGS(ng) % DiaU2wrk,   DIAGS(ng) % DiaV2wrk,   &
     &                  DIAGS(ng) % DiaRUbar,   DIAGS(ng) % DiaRVbar,   &
# ifdef SOLVE3D
     &                  DIAGS(ng) % DiaU2int,   DIAGS(ng) % DiaV2int,   &
     &                  DIAGS(ng) % DiaRUfrc,   DIAGS(ng) % DiaRVfrc,   &
# endif
#endif
#if defined NESTING && !defined SOLVE3D
     &                  OCEAN(ng) % DU_flux,    OCEAN(ng) % DV_flux,    &
#endif
     &                  OCEAN(ng) % rubar,      OCEAN(ng) % rvbar,      &
     &                  OCEAN(ng) % rzeta,                              &
     &                  OCEAN(ng) % ubar,       OCEAN(ng) % vbar,       &
     &                  OCEAN(ng) % zeta)
#ifdef PROFILE
      CALL wclock_off (ng, iNLM, 9, __LINE__, MyFile)
#endif
!
      RETURN
      END SUBROUTINE step2d
!
!***********************************************************************
      SUBROUTINE step2d_tile (ng, tile,                                 &
     &                        LBi, UBi, LBj, UBj, UBk,                  &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        krhs, kstp, knew,                         &
#ifdef SOLVE3D
     &                        nstp, nnew,                               &
#endif
#ifdef MASKING
     &                        pmask, rmask, umask, vmask,               &
#endif
#ifdef WET_DRY
     &                        pmask_wet, pmask_full,                    &
     &                        rmask_wet, rmask_full,                    &
     &                        umask_wet, umask_full,                    &
     &                        vmask_wet, vmask_full,                    &
# ifdef SOLVE3D
     &                        rmask_wet_avg,                            &
# endif
#endif
     &                        fomn, h,                                  &
     &                        om_u, om_v, on_u, on_v, omn, pm, pn,      &
#if defined CURVGRID && defined UV_ADV
     &                        dndx, dmde,                               &
#endif
#if defined UV_VIS2 || defined UV_VIS4
     &                        pmon_r, pnom_r, pmon_p, pnom_p,           &
     &                        om_r, on_r, om_p, on_p,                   &
# ifdef UV_VIS2
     &                        visc2_p, visc2_r,                         &
# endif
# ifdef UV_VIS4
     &                        visc4_p, visc4_r,                         &
# endif
#endif
#if defined SEDIMENT && defined SED_MORPH
     &                        bed_thick,                                &
#endif
#ifdef WEC
# ifdef WEC_VF
#  ifdef WEC_ROLLER
     &                        rurol2d, rvrol2d,                         &
#  endif
#  ifdef BOTTOM_STREAMING
     &                        rubst2d, rvbst2d,                         &
#  endif
#  ifdef SURFACE_STREAMING
     &                        russt2d, rvsst2d,                         &
#  endif
     &                        rubrk2d, rvbrk2d,                         &
     &                        rukvf2d, rvkvf2d,                         &
     &                        bh, qsp, zetaw,                           &
# endif
     &                        ubar_stokes, vbar_stokes,                 &
#endif
#if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
     &                        eq_tide,                                  &
#endif
#ifndef SOLVE3D
     &                        sustr, svstr, bustr, bvstr,               &
# ifdef ATM_PRESS
     &                        Pair,                                     &
# endif
#else
# ifdef VAR_RHO_2D
     &                        rhoA, rhoS,                               &
# endif
     &                        DU_avg1, DU_avg2,                         &
     &                        DV_avg1, DV_avg2,                         &
     &                        Zt_avg1,                                  &
     &                        rufrc, rvfrc, ru, rv,                     &
#endif
#ifdef DIAGNOSTICS_UV
     &                        DiaU2wrk, DiaV2wrk,                       &
     &                        DiaRUbar, DiaRVbar,                       &
# ifdef SOLVE3D
     &                        DiaU2int, DiaV2int,                       &
     &                        DiaRUfrc, DiaRVfrc,                       &
# endif
#endif
#if defined NESTING && !defined SOLVE3D
     &                        DU_flux, DV_flux,                         &
#endif
     &                        rubar, rvbar, rzeta,                      &
     &                        ubar,  vbar, zeta)
!***********************************************************************
!
      USE mod_param
      USE mod_clima
      USE mod_ncparam
      USE mod_scalars
#if defined SEDIMENT && defined SED_MORPH
      USE mod_sedbed
#endif
      USE mod_sources
!
      USE exchange_2d_mod
#ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange2d
#endif
      USE obc_volcons_mod, ONLY : obc_flux_tile, set_DUV_bc_tile
      USE u2dbc_mod,       ONLY : u2dbc_tile
      USE v2dbc_mod,       ONLY : v2dbc_tile
      USE zetabc_mod,      ONLY : zetabc_tile
#ifdef WET_DRY
      USE wetdry_mod,      ONLY : wetdry_tile
#endif
!
!  Imported variable declarations.
!
      integer, intent(in    ) :: ng, tile
      integer, intent(in    ) :: LBi, UBi, LBj, UBj, UBk
      integer, intent(in    ) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in    ) :: krhs, kstp, knew
#ifdef SOLVE3D
      integer, intent(in    ) :: nstp, nnew
#endif
!
#ifdef ASSUMED_SHAPE
# ifdef MASKING
      real(r8), intent(in   ) :: pmask(LBi:,LBj:)
      real(r8), intent(in   ) :: rmask(LBi:,LBj:)
      real(r8), intent(in   ) :: umask(LBi:,LBj:)
      real(r8), intent(in   ) :: vmask(LBi:,LBj:)
# endif
      real(r8), intent(in   ) :: fomn(LBi:,LBj:)
# if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(inout) :: h(LBi:,LBj:)
# else
      real(r8), intent(in   ) :: h(LBi:,LBj:)
# endif
      real(r8), intent(in   ) :: om_u(LBi:,LBj:)
      real(r8), intent(in   ) :: om_v(LBi:,LBj:)
      real(r8), intent(in   ) :: on_u(LBi:,LBj:)
      real(r8), intent(in   ) :: on_v(LBi:,LBj:)
      real(r8), intent(in   ) :: omn(LBi:,LBj:)
      real(r8), intent(in   ) :: pm(LBi:,LBj:)
      real(r8), intent(in   ) :: pn(LBi:,LBj:)
# if defined CURVGRID && defined UV_ADV
      real(r8), intent(in   ) :: dndx(LBi:,LBj:)
      real(r8), intent(in   ) :: dmde(LBi:,LBj:)
# endif
# if defined UV_VIS2 || defined UV_VIS4
      real(r8), intent(in   ) :: pmon_r(LBi:,LBj:)
      real(r8), intent(in   ) :: pnom_r(LBi:,LBj:)
      real(r8), intent(in   ) :: pmon_p(LBi:,LBj:)
      real(r8), intent(in   ) :: pnom_p(LBi:,LBj:)
      real(r8), intent(in   ) :: om_r(LBi:,LBj:)
      real(r8), intent(in   ) :: on_r(LBi:,LBj:)
      real(r8), intent(in   ) :: om_p(LBi:,LBj:)
      real(r8), intent(in   ) :: on_p(LBi:,LBj:)
#  ifdef UV_VIS2
      real(r8), intent(in   ) :: visc2_p(LBi:,LBj:)
      real(r8), intent(in   ) :: visc2_r(LBi:,LBj:)
#  endif
#  ifdef UV_VIS4
      real(r8), intent(in   ) :: visc4_p(LBi:,LBj:)
      real(r8), intent(in   ) :: visc4_r(LBi:,LBj:)
#  endif
# endif
# if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(in   ) :: bed_thick(LBi:,LBj:,:)
# endif
# ifdef WEC
#  ifdef WEC_VF
#   ifdef WEC_ROLLER
      real(r8), intent(in) :: rurol2d(LBi:,LBj:)
      real(r8), intent(in) :: rvrol2d(LBi:,LBj:)
#   endif
#   ifdef BOTTOM_STREAMING
      real(r8), intent(in) :: rubst2d(LBi:,LBj:)
      real(r8), intent(in) :: rvbst2d(LBi:,LBj:)
#   endif
#   ifdef SURFACE_STREAMING
      real(r8), intent(in) :: russt2d(LBi:,LBj:)
      real(r8), intent(in) :: rvsst2d(LBi:,LBj:)
#   endif
      real(r8), intent(in) :: rubrk2d(LBi:,LBj:)
      real(r8), intent(in) :: rvbrk2d(LBi:,LBj:)
      real(r8), intent(in) :: rukvf2d(LBi:,LBj:)
      real(r8), intent(in) :: rvkvf2d(LBi:,LBj:)
      real(r8), intent(in) :: bh(LBi:,LBj:)
      real(r8), intent(in) :: qsp(LBi:,LBj:)
      real(r8), intent(in) :: zetaw(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: ubar_stokes(LBi:,LBj:)
      real(r8), intent(in) :: vbar_stokes(LBi:,LBj:)
# endif
# if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
      real(r8), intent(in   ) :: eq_tide(LBi:,LBj:)
# endif
# ifndef SOLVE3D
      real(r8), intent(in   ) :: sustr(LBi:,LBj:)
      real(r8), intent(in   ) :: svstr(LBi:,LBj:)
      real(r8), intent(in   ) :: bustr(LBi:,LBj:)
      real(r8), intent(in   ) :: bvstr(LBi:,LBj:)
#  ifdef ATM_PRESS
      real(r8), intent(in   ) :: Pair(LBi:,LBj:)
#  endif
# else
#  ifdef VAR_RHO_2D
      real(r8), intent(in   ) :: rhoA(LBi:,LBj:)
      real(r8), intent(in   ) :: rhoS(LBi:,LBj:)
#  endif
      real(r8), intent(inout) :: DU_avg1(LBi:,LBj:)
      real(r8), intent(inout) :: DU_avg2(LBi:,LBj:)
      real(r8), intent(inout) :: DV_avg1(LBi:,LBj:)
      real(r8), intent(inout) :: DV_avg2(LBi:,LBj:)
      real(r8), intent(inout) :: Zt_avg1(LBi:,LBj:)
      real(r8), intent(inout) :: rufrc(LBi:,LBj:)
      real(r8), intent(inout) :: rvfrc(LBi:,LBj:)
      real(r8), intent(inout) :: ru(LBi:,LBj:,0:,:)
      real(r8), intent(inout) :: rv(LBi:,LBj:,0:,:)
# endif
# ifdef WET_DRY
      real(r8), intent(inout) :: pmask_full(LBi:,LBj:)
      real(r8), intent(inout) :: rmask_full(LBi:,LBj:)
      real(r8), intent(inout) :: umask_full(LBi:,LBj:)
      real(r8), intent(inout) :: vmask_full(LBi:,LBj:)

      real(r8), intent(inout) :: pmask_wet(LBi:,LBj:)
      real(r8), intent(inout) :: rmask_wet(LBi:,LBj:)
      real(r8), intent(inout) :: umask_wet(LBi:,LBj:)
      real(r8), intent(inout) :: vmask_wet(LBi:,LBj:)
#  ifdef SOLVE3D
      real(r8), intent(inout) :: rmask_wet_avg(LBi:,LBj:)
#  endif
# endif
# ifdef DIAGNOSTICS_UV
      real(r8), intent(inout) :: DiaU2wrk(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaV2wrk(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaRUbar(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaRVbar(LBi:,LBj:,:,:)
#  ifdef SOLVE3D
      real(r8), intent(inout) :: DiaU2int(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaV2int(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaRUfrc(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaRVfrc(LBi:,LBj:,:,:)
#  endif
# endif
      real(r8), intent(inout) :: rubar(LBi:,LBj:,:)
      real(r8), intent(inout) :: rvbar(LBi:,LBj:,:)
      real(r8), intent(inout) :: rzeta(LBi:,LBj:,:)
      real(r8), intent(inout) :: ubar(LBi:,LBj:,:)
      real(r8), intent(inout) :: vbar(LBi:,LBj:,:)
      real(r8), intent(inout) :: zeta(LBi:,LBj:,:)
# if defined NESTING && !defined SOLVE3D
      real(r8), intent(out  ) :: DU_flux(LBi:,LBj:)
      real(r8), intent(out  ) :: DV_flux(LBi:,LBj:)
# endif

#else

# ifdef MASKING
      real(r8), intent(in   ) :: pmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: rmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: vmask(LBi:UBi,LBj:UBj)
# endif
      real(r8), intent(in   ) :: fomn(LBi:UBi,LBj:UBj)
# if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(inout) :: h(LBi:UBi,LBj:UBj)
# else
      real(r8), intent(in   ) :: h(LBi:UBi,LBj:UBj)
# endif
      real(r8), intent(in   ) :: om_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: om_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: on_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: on_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: omn(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: pn(LBi:UBi,LBj:UBj)
# if defined CURVGRID && defined UV_ADV
      real(r8), intent(in   ) :: dndx(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: dmde(LBi:UBi,LBj:UBj)
# endif
# if defined UV_VIS2 || defined UV_VIS4
      real(r8), intent(in   ) :: pmon_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: pnom_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: pmon_p(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: pnom_p(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: om_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: on_r(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: om_p(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: on_p(LBi:UBi,LBj:UBj)
#  ifdef UV_VIS2
      real(r8), intent(in   ) :: visc2_p(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: visc2_r(LBi:UBi,LBj:UBj)
#  endif
#  ifdef UV_VIS4
      real(r8), intent(in   ) :: visc4_p(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: visc4_r(LBi:UBi,LBj:UBj)
#  endif
# endif
# if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(in   ) :: bed_thick(LBi:UBi,LBj:UBj,1:3)
# endif
# ifdef WEC
#  ifdef WEC_VF
#   ifdef WEC_ROLLER
      real(r8), intent(in) :: rurol2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rvrol2d(LBi:UBi,LBj:UBj)
#   endif
#   ifdef BOTTOM_STREAMING
      real(r8), intent(in) :: rubst2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rvbst2d(LBi:UBi,LBj:UBj)
#   endif
#   ifdef SURFACE_STREAMING
      real(r8), intent(in) :: russt2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rvsst2d(LBi:UBi,LBj:UBj)
#   endif
      real(r8), intent(in) :: rubrk2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rvbrk2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rukvf2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: rvkvf2d(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bh(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: qsp(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: zetaw(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: ubar_stokes(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vbar_stokes(LBi:UBi,LBj:UBj)
# endif
# if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
      real(r8), intent(in   ) :: eq_tide(LBi:UBi,LBj:UBj)
# endif
# ifndef SOLVE3D
      real(r8), intent(in   ) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: svstr(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: bustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: bvstr(LBi:UBi,LBj:UBj)
#  ifdef ATM_PRESS
      real(r8), intent(in   ) :: Pair(LBi:UBi,LBj:UBj)
#  endif
# else
#  ifdef VAR_RHO_2D
      real(r8), intent(in   ) :: rhoA(LBi:UBi,LBj:UBj)
      real(r8), intent(in   ) :: rhoS(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(inout) :: DU_avg1(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: DU_avg2(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: DV_avg1(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: DV_avg2(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: Zt_avg1(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: rufrc(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: rvfrc(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: ru(LBi:UBi,LBj:UBj,0:UBk,2)
      real(r8), intent(inout) :: rv(LBi:UBi,LBj:UBj,0:UBk,2)
# endif
# ifdef WET_DRY
      real(r8), intent(inout) :: pmask_full(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: rmask_full(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: umask_full(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: vmask_full(LBi:UBi,LBj:UBj)

      real(r8), intent(inout) :: pmask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: rmask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: umask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(inout) :: vmask_wet(LBi:UBi,LBj:UBj)
#  ifdef SOLVE3D
      real(r8), intent(inout) :: rmask_wet_avg(LBi:UBi,LBj:UBj)
#  endif
# endif
# ifdef DIAGNOSTICS_UV
      real(r8), intent(inout) :: DiaU2wrk(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaV2wrk(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaRUbar(LBi:UBi,LBj:UBj,2,NDM2d-1)
      real(r8), intent(inout) :: DiaRVbar(LBi:UBi,LBj:UBj,2,NDM2d-1)
#  ifdef SOLVE3D
      real(r8), intent(inout) :: DiaU2int(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaV2int(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaRUfrc(LBi:UBi,LBj:UBj,3,NDM2d-1)
      real(r8), intent(inout) :: DiaRVfrc(LBi:UBi,LBj:UBj,3,NDM2d-1)
#  endif
# endif
      real(r8), intent(inout) :: rubar(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: rvbar(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: rzeta(LBi:UBi,LBj:UBj,2)
      real(r8), intent(inout) :: ubar(LBi:UBi,LBj:UBj,:)
      real(r8), intent(inout) :: vbar(LBi:UBi,LBj:UBj,:)
      real(r8), intent(inout) :: zeta(LBi:UBi,LBj:UBj,:)
# if defined NESTING && !defined SOLVE3D
      real(r8), intent(out  ) :: DU_flux(LBi:UBi,LBj:UBj)
      real(r8), intent(out  ) :: DV_flux(LBi:UBi,LBj:UBj)
# endif
#endif
!
!  Local variable declarations.
!
      logical :: CORRECTOR_2D_STEP
!
      integer :: i, is, j, ptsk
#ifdef DIAGNOSTICS_UV
      integer :: idiag
#endif
!
      real(r8) :: cff, cff1, cff2, cff3, cff4, cff5, cff6, cff7
      real(r8) :: fac, fac1, fac2, fac3
!
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Dgrad
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Dnew
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Drhs
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Drhs_p
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Dstp
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DUon
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DVom
#ifdef WEC
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DUSon
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DVSom
#endif
#ifdef WEC_VF
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DUSom
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: DVSon
#endif
#ifdef UV_VIS4
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: LapU
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: LapV
#endif
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: UFe
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: UFx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: VFe
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: VFx
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: grad
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: gzeta
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: gzeta2
#if defined VAR_RHO_2D && defined SOLVE3D
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: gzetaSA
#endif
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: rhs_ubar
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: rhs_vbar
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: rhs_zeta
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: zeta_new
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: zwrk
#ifdef WET_DRY
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: wetdry
#endif
#ifdef DIAGNOSTICS_UV
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Uwrk
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: Vwrk
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,NDM2d-1) :: DiaU2rhs
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,NDM2d-1) :: DiaV2rhs
#endif

#include "set_bounds.h"
!
      ptsk=3-kstp
      CORRECTOR_2D_STEP=.not.PREDICTOR_2D_STEP(ng)
!
!-----------------------------------------------------------------------
!  Compute total depth (m) and vertically integrated mass fluxes.
!-----------------------------------------------------------------------
!
#if defined DISTRIBUTE && !defined NESTING

!  In distributed-memory, the I- and J-ranges are different and a
!  special exchange is done to avoid having three ghost points for
!  high order numerical stencils. Notice that a private array is
!  passed below to the exchange routine. It also applies periodic
!  boundary conditions, if appropriate and no partitions in I- or
!  J-directions.
!
      DO j=JstrV-2,Jendp2
        DO i=IstrU-2,Iendp2
          Drhs(i,j)=zeta(i,j,krhs)+h(i,j)
        END DO
      END DO
      DO j=JstrV-2,Jendp2
        DO i=IstrU-1,Iendp2
          cff=0.5_r8*on_u(i,j)
          cff1=cff*(Drhs(i,j)+Drhs(i-1,j))
          DUon(i,j)=ubar(i,j,krhs)*cff1
# ifdef WEC
#  ifdef WET_DRY
          cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,ubar_stokes(i,j))*umask_wet(i,j)
          cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          cff1=cff1*cff7
#  endif
          DUSon(i,j)=ubar_stokes(i,j)*cff1
          DUon(i,j)=DUon(i,j)+DUSon(i,j)
# endif
        END DO
      END DO
      DO j=JstrV-1,Jendp2
        DO i=IstrU-2,Iendp2
          cff=0.5_r8*om_v(i,j)
          cff1=cff*(Drhs(i,j)+Drhs(i,j-1))
          DVom(i,j)=vbar(i,j,krhs)*cff1
# ifdef WEC
#  ifdef WET_DRY
          cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,vbar_stokes(i,j))*vmask_wet(i,j)
          cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          cff1=cff1*cff7
#  endif
          DVSom(i,j)=vbar_stokes(i,j)*cff1
          DVom(i,j)=DVom(i,j)+DVSom(i,j)
# endif
        END DO
      END DO

#else

      DO j=JstrVm2-1,Jendp2
        DO i=IstrUm2-1,Iendp2
          Drhs(i,j)=zeta(i,j,krhs)+h(i,j)
        END DO
      END DO
      DO j=JstrVm2-1,Jendp2
        DO i=IstrUm2,Iendp2
          cff=0.5_r8*on_u(i,j)
          cff1=cff*(Drhs(i,j)+Drhs(i-1,j))
          DUon(i,j)=ubar(i,j,krhs)*cff1
# ifdef WEC
#  ifdef WET_DRY
          cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,ubar_stokes(i,j))*umask_wet(i,j)
          cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          cff1=cff1*cff7
#  endif
          DUSon(i,j)=ubar_stokes(i,j)*cff1
          DUon(i,j)=DUon(i,j)+DUSon(i,j)
# endif
        END DO
      END DO
      DO j=JstrVm2,Jendp2
        DO i=IstrUm2-1,Iendp2
          cff=0.5_r8*om_v(i,j)
          cff1=cff*(Drhs(i,j)+Drhs(i,j-1))
          DVom(i,j)=vbar(i,j,krhs)*cff1
# ifdef WEC
#  ifdef WET_DRY
          cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,vbar_stokes(i,j))*vmask_wet(i,j)
          cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          cff1=cff1*cff7
#  endif
          DVSom(i,j)=vbar_stokes(i,j)*cff1
          DVom(i,j)=DVom(i,j)+DVSom(i,j)
# endif
        END DO
      END DO
#endif
#ifdef DISTRIBUTE
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          DUon)
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          DVom)
      END IF
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    IminS, ImaxS, JminS, JmaxS,                   &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    DUon, DVom)
#endif
!
!  Set vertically integrated mass fluxes DUon and DVom along the open
!  boundaries in such a way that the integral volume is conserved.
!
      IF (ANY(VolCons(:,ng))) THEN
        CALL set_DUV_bc_tile (ng, tile,                                 &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        krhs,                                     &
#ifdef MASKING
     &                        umask, vmask,                             &
#endif
     &                        om_v, on_u,                               &
     &                        ubar, vbar,                               &
     &                        Drhs, DUon, DVom)
      END IF
#ifdef SOLVE3D
!
!-----------------------------------------------------------------------
!  Compute time averaged fields over all short time-steps.
!-----------------------------------------------------------------------
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        IF (FIRST_2D_STEP) THEN
!
!  Reset arrays for 2D fields averaged within the short time-steps.
!
          cff2=(-1.0_r8/12.0_r8)*weight(2,iif(ng)+1,ng)
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              Zt_avg1(i,j)=0.0_r8
            END DO
            DO i=Istr,IendR
              DU_avg1(i,j)=0.0_r8
              DU_avg2(i,j)=cff2*DUon(i,j)
            END DO
          END DO
          DO j=Jstr,JendR
            DO i=IstrR,IendR
              DV_avg1(i,j)=0.0_r8
              DV_avg2(i,j)=cff2*DVom(i,j)
            END DO
          END DO
        ELSE
!
!  Accumulate field averages of previous time-step after they are
!  computed in the previous corrector step, updated their boundaries,
!  and synchronized.
!
          cff1=weight(1,iif(ng)-1,ng)
          cff2=(8.0_r8/12.0_r8)*weight(2,iif(ng)  ,ng)-                 &
     &         (1.0_r8/12.0_r8)*weight(2,iif(ng)+1,ng)
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              Zt_avg1(i,j)=Zt_avg1(i,j)+cff1*zeta(i,j,krhs)
            END DO
            DO i=Istr,IendR
              DU_avg1(i,j)=DU_avg1(i,j)+cff1*DUon(i,j)
# ifdef WEC
              DU_avg1(i,j)=DU_avg1(i,j)-cff1*DUSon(i,j)
# endif
              DU_avg2(i,j)=DU_avg2(i,j)+cff2*DUon(i,j)
            END DO
          END DO
          DO j=Jstr,JendR
            DO i=IstrR,IendR
              DV_avg1(i,j)=DV_avg1(i,j)+cff1*DVom(i,j)
# ifdef WEC
              DV_avg1(i,j)=DV_avg1(i,j)-cff1*DVSom(i,j)
# endif
              DV_avg2(i,j)=DV_avg2(i,j)+cff2*DVom(i,j)
            END DO
          END DO
        END IF
      ELSE
        IF (FIRST_2D_STEP) THEN
          cff2=weight(2,iif(ng),ng)
        ELSE
          cff2=(5.0_r8/12.0_r8)*weight(2,iif(ng),ng)
        END IF
        DO j=JstrR,JendR
          DO i=Istr,IendR
            DU_avg2(i,j)=DU_avg2(i,j)+cff2*DUon(i,j)
          END DO
        END DO
        DO j=Jstr,JendR
          DO i=IstrR,IendR
            DV_avg2(i,j)=DV_avg2(i,j)+cff2*DVom(i,j)
          END DO
        END DO
      END IF
!
!  After all fast time steps are completed, apply boundary conditions
!  to time averaged fields.
# ifdef NESTING
!  In nesting applications with refinement grids, we need to exchange
!  the DU_avg2 and DV_avg2 fluxes boundary information for the case
!  that a contact point is at a tile partition. Notice that in such
!  cases, we need i+1 and j+1 values for spatial/temporal interpolation.
# endif
!
      IF ((iif(ng).eq.(nfast(ng)+1)).and.PREDICTOR_2D_STEP(ng)) THEN
        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_r2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            Zt_avg1)
          CALL exchange_u2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            DU_avg1)
          CALL exchange_v2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            DV_avg1)
# ifdef NESTING
          CALL exchange_u2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            DU_avg2)
          CALL exchange_v2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            DV_avg2)
# endif
        END IF
# ifdef DISTRIBUTE
        CALL mp_exchange2d (ng, tile, iNLM, 3,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints,                               &
     &                      EWperiodic(ng), NSperiodic(ng),             &
     &                      Zt_avg1, DU_avg1, DV_avg1)
#  ifdef NESTING
        CALL mp_exchange2d (ng, tile, iNLM, 2,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints,                               &
     &                      EWperiodic(ng), NSperiodic(ng),             &
     &                      DU_avg2, DV_avg2)
#  endif
# endif
      END IF
#endif
#ifdef WET_DRY
!
!-----------------------------------------------------------------------
!  Compute new wet/dry masks.
!-----------------------------------------------------------------------
!
      CALL wetdry_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
# ifdef MASKING
     &                  pmask, rmask, umask, vmask,                     &
# endif
     &                  h, zeta(:,:,kstp),                              &
# ifdef SOLVE3D
     &                  DU_avg1, DV_avg1,                               &
     &                  rmask_wet_avg,                                  &
# endif
     &                  pmask_wet, pmask_full,                          &
     &                  rmask_wet, rmask_full,                          &
     &                  umask_wet, umask_full,                          &
     &                  vmask_wet, vmask_full)
#endif
!
!  Do not perform the actual time stepping during the auxiliary
!  (nfast(ng)+1) time step.
!
      IF (iif(ng).gt.nfast(ng)) RETURN
!
!=======================================================================
!  Time step free-surface equation.
!=======================================================================
!
!  During the first time-step, the predictor step is Forward-Euler
!  and the corrector step is Backward-Euler. Otherwise, the predictor
!  step is Leap-frog and the corrector step is Adams-Moulton.
#if defined VAR_RHO_2D && defined SOLVE3D
!  Recall that the vertical averaged density (rhoA) and density
!  pertubation (rhoS) are nondimensional quantities.
!
      fac=1000.0_r8/rho0                                ! nondimensional
#endif
!
      IF (FIRST_2D_STEP) THEN
        cff1=dtfast(ng)
        DO j=JstrV-1,Jend
          DO i=IstrU-1,Iend
            rhs_zeta(i,j)=(DUon(i,j)-DUon(i+1,j))+                      &
     &                    (DVom(i,j)-DVom(i,j+1))
            zeta_new(i,j)=zeta(i,j,kstp)+                               &
     &                    pm(i,j)*pn(i,j)*cff1*rhs_zeta(i,j)
#ifdef MASKING
            zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
#endif
            Dnew(i,j)=zeta_new(i,j)+h(i,j)
!
            zwrk(i,j)=0.5_r8*(zeta(i,j,kstp)+zeta_new(i,j))
#if defined VAR_RHO_2D && defined SOLVE3D
            gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
            gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
            gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
#else
            gzeta(i,j)=zwrk(i,j)
            gzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
#endif
          END DO
        END DO
      ELSE IF (PREDICTOR_2D_STEP(ng)) THEN
        cff1=2.0_r8*dtfast(ng)
        cff4=4.0_r8/25.0_r8
        cff5=1.0_r8-2.0_r8*cff4
        DO j=JstrV-1,Jend
          DO i=IstrU-1,Iend
            rhs_zeta(i,j)=(DUon(i,j)-DUon(i+1,j))+                      &
     &                    (DVom(i,j)-DVom(i,j+1))
            zeta_new(i,j)=zeta(i,j,kstp)+                               &
     &                    pm(i,j)*pn(i,j)*cff1*rhs_zeta(i,j)
#ifdef MASKING
            zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
#endif
            Dnew(i,j)=zeta_new(i,j)+h(i,j)
!
            zwrk(i,j)=cff5*zeta(i,j,krhs)+                              &
     &                cff4*(zeta(i,j,kstp)+zeta_new(i,j))
#if defined VAR_RHO_2D && defined SOLVE3D
            gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
            gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
            gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
#else
            gzeta(i,j)=zwrk(i,j)
            gzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
#endif
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=dtfast(ng)*5.0_r8/12.0_r8
        cff2=dtfast(ng)*8.0_r8/12.0_r8
        cff3=dtfast(ng)*1.0_r8/12.0_r8
        cff4=2.0_r8/5.0_r8
        cff5=1.0_r8-cff4
        DO j=JstrV-1,Jend
          DO i=IstrU-1,Iend
            cff=cff1*((DUon(i,j)-DUon(i+1,j))+                          &
     &                (DVom(i,j)-DVom(i,j+1)))
            zeta_new(i,j)=zeta(i,j,kstp)+                               &
     &                    pm(i,j)*pn(i,j)*(cff+                         &
     &                                     cff2*rzeta(i,j,kstp)-        &
     &                                     cff3*rzeta(i,j,ptsk))
#ifdef MASKING
            zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
#endif
            Dnew(i,j)=zeta_new(i,j)+h(i,j)
!
            zwrk(i,j)=cff5*zeta_new(i,j)+cff4*zeta(i,j,krhs)
#if defined VAR_RHO_2D && defined SOLVE3D
            gzeta(i,j)=(fac+rhoS(i,j))*zwrk(i,j)
            gzeta2(i,j)=gzeta(i,j)*zwrk(i,j)
            gzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
#else
            gzeta(i,j)=zwrk(i,j)
            gzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
#endif
          END DO
        END DO
      END IF
!
!  Load new free-surface values into shared array at both predictor
!  and corrector steps.
#ifdef WET_DRY
!  Modify new free-surface to Ensure that depth is > Dcrit for masked
!  cells.
#endif
!
      DO j=Jstr,Jend
        DO i=Istr,Iend
          zeta(i,j,knew)=zeta_new(i,j)
#if defined WET_DRY && defined MASKING
          zeta(i,j,knew)=zeta(i,j,knew)+                                &
     &                   (Dcrit(ng)-h(i,j))*(1.0_r8-rmask(i,j))
#endif
        END DO
      END DO
!
!  If predictor step, load right-side-term into shared array.
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        DO j=Jstr,Jend
          DO i=Istr,Iend
            rzeta(i,j,krhs)=rhs_zeta(i,j)
          END DO
        END DO
        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_r2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            rzeta(:,:,krhs))
        END IF
#ifdef DISTRIBUTE
        CALL mp_exchange2d (ng, tile, iNLM, 1,                          &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      NghostPoints,                               &
     &                      EWperiodic(ng), NSperiodic(ng),             &
     &                      rzeta(:,:,krhs))
#endif
      END IF
!
!  Apply mass point sources (volume vertical influx), if any.
!
!    Dsrc(is) = 2,  flow across grid cell w-face (positive or negative)
!
      IF (LwSrc(ng)) THEN
        DO is=1,Nsrc(ng)
          IF (INT(SOURCES(ng)%Dsrc(is)).eq.2) THEN
            i=SOURCES(ng)%Isrc(is)
            j=SOURCES(ng)%Jsrc(is)
            IF (((IstrR.le.i).and.(i.le.IendR)).and.                    &
     &          ((JstrR.le.j).and.(j.le.JendR))) THEN
              zeta(i,j,knew)=zeta(i,j,knew)+                            &
     &                       SOURCES(ng)%Qbar(is)*                      &
     &                       pm(i,j)*pn(i,j)*dtfast(ng)
            END IF
          END IF
        END DO
      END IF

#if defined SEDIMENT && defined SED_MORPH
!
!  Scale the bed change with the fast time stepping. The half is
!  becasue we do predictor and corrector. The "ndtfast/nfast" is
!  becasue we do "nfast" steps to here.
!
      fac=0.5_r8*dtfast(ng)*ndtfast(ng)/(nfast(ng)*dt(ng))
      DO j=Jstr,Jend
        DO i=Istr,Iend
          cff=fac*(bed_thick(i,j,nstp)-bed_thick(i,j,nnew))
          h(i,j)=h(i,j)-cff
        END DO
      END DO
#endif
!
!  Set free-surface lateral boundary conditions.
!
      CALL zetabc_tile (ng, tile,                                       &
     &                  LBi, UBi, LBj, UBj,                             &
     &                  IminS, ImaxS, JminS, JmaxS,                     &
     &                  krhs, kstp, knew,                               &
     &                  zeta)
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          zeta(:,:,knew))
      END IF
#ifdef DISTRIBUTE
      CALL mp_exchange2d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    zeta(:,:,knew))
#endif
!
!=======================================================================
!  Compute right-hand-side for the 2D momentum equations.
!=======================================================================
!
!-----------------------------------------------------------------------
!  Compute pressure gradient terms.
!-----------------------------------------------------------------------
!
      cff1=0.5_r8*g
      cff2=1.0_r8/3.0_r8
#if !defined SOLVE3D && defined ATM_PRESS
      fac3=0.5_r8*100.0_r8/rho0
#endif
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          rhs_ubar(i,j)=cff1*on_u(i,j)*                                 &
     &                  ((h(i-1,j)+                                     &
     &                    h(i ,j))*                                     &
     &                   (gzeta(i-1,j)-                                 &
     &                    gzeta(i  ,j))+                                &
#if defined VAR_RHO_2D && defined SOLVE3D
     &                   (h(i-1,j)-                                     &
     &                    h(i  ,j))*                                    &
     &                   (gzetaSA(i-1,j)+                               &
     &                    gzetaSA(i  ,j)+                               &
     &                    cff2*(rhoA(i-1,j)-                            &
     &                          rhoA(i  ,j))*                           &
     &                         (zwrk(i-1,j)-                            &
     &                          zwrk(i  ,j)))+                          &
#endif
     &                   (gzeta2(i-1,j)-                                &
     &                    gzeta2(i  ,j)))
#if defined ATM_PRESS && !defined SOLVE3D
          rhs_ubar(i,j)=rhs_ubar(i,j)-                                  &
     &                  fac3*on_u(i,j)*                                 &
     &                  (h(i-1,j)+h(i,j)+                               &
     &                   gzeta(i-1,j)+gzeta(i,j))*                      &
     &                  (Pair(i,j)-Pair(i-1,j))
#endif
#if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
          rhs_ubar(i,j)=rhs_ubar(i,j)-                                  &
     &                  cff1*on_u(i,j)*                                 &
     &                  (h(i-1,j)+h(i,j)+                               &
     &                   gzeta(i-1,j)+gzeta(i,j))*                      &
     &                  (eq_tide(i,j)-eq_tide(i-1,j))
#endif
#ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2pgrd)=rhs_ubar(i,j)
#endif
#if defined WEC_VF
          cff3=0.5_r8*on_u(i,j)*                                        &
     &         (h(i-1,j)+h(i,j)+                                        &
     &         gzeta(i-1,j)+gzeta(i,j))
          cff4=cff3*g*(zetaw(i-1,j)-zetaw(i,j))
          cff5=cff3*g*(qsp(i-1,j)-qsp(i,j))
          cff6=cff3*(bh(i-1,j)-bh(i,j))
          cff7=rukvf2d(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)-cff4-cff5+cff6+cff7
# ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2pgrd)
          DiaU2rhs(i,j,M2pgrd)=DiaU2rhs(i,j,M2pgrd)-cff4-cff5+cff6
          DiaU2rhs(i,j,M2zetw)=-cff4
          DiaU2rhs(i,j,M2zqsp)=-cff5
          DiaU2rhs(i,j,M2zbeh)=cff6
          DiaU2rhs(i,j,M2kvrf)=cff7
#  ifndef UV_ADV
          DiaU2rhs(i,j,M2hjvf)=0.0_r8
#  endif
# endif
#endif
        END DO
        IF (j.ge.JstrV) THEN
          DO i=Istr,Iend
            rhs_vbar(i,j)=cff1*om_v(i,j)*                               &
     &                    ((h(i,j-1)+                                   &
     &                      h(i,j  ))*                                  &
     &                     (gzeta(i,j-1)-                               &
     &                      gzeta(i,j  ))+                              &
#if defined VAR_RHO_2D && defined SOLVE3D
     &                     (h(i,j-1)-                                   &
     &                      h(i,j  ))*                                  &
     &                     (gzetaSA(i,j-1)+                             &
     &                      gzetaSA(i,j  )+                             &
     &                      cff2*(rhoA(i,j-1)-                          &
     &                            rhoA(i,j  ))*                         &
     &                           (zwrk(i,j-1)-                          &
     &                            zwrk(i,j  )))+                        &
#endif
     &                     (gzeta2(i,j-1)-                              &
     &                      gzeta2(i,j  )))
#if defined ATM_PRESS && !defined SOLVE3D
            rhs_vbar(i,j)=rhs_vbar(i,j)-                                &
     &                    fac3*om_v(i,j)*                               &
     &                    (h(i,j-1)+h(i,j)+                             &
     &                     gzeta(i,j-1)+gzeta(i,j))*                    &
     &                    (Pair(i,j)-Pair(i,j-1))
#endif
#if defined TIDE_GENERATING_FORCES && !defined SOLVE3D
            rhs_vbar(i,j)=rhs_vbar(i,j)-                                &
     &                    cff1*om_v(i,j)*                               &
     &                    (h(i,j-1)+h(i,j)+                             &
     &                     gzeta(i,j-1)+gzeta(i,j))*                    &
     &                    (eq_tide(i,j)-eq_tide(i,j-1))
#endif
#ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2pgrd)=rhs_vbar(i,j)
#endif
#if defined WEC_VF
            cff3=0.5_r8*om_v(i,j)*                                      &
     &           (h(i,j-1)+h(i,j)+                                      &
     &           gzeta(i,j-1)+gzeta(i,j))
            cff4=cff3*g*(zetaw(i,j-1)-zetaw(i,j))
            cff5=cff3*g*(qsp(i,j-1)-qsp(i,j))
            cff6=cff3*(bh(i,j-1)-bh(i,j))
            cff7=rvkvf2d(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)-cff4-cff5+cff6+cff7
# ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2pgrd)
            DiaV2rhs(i,j,M2pgrd)=DiaV2rhs(i,j,M2pgrd)-cff4-cff5+cff6
            DiaV2rhs(i,j,M2zetw)=-cff4
            DiaV2rhs(i,j,M2zqsp)=-cff5
            DiaV2rhs(i,j,M2zbeh)=cff6
            DiaV2rhs(i,j,M2kvrf)=cff7
#  ifndef UV_ADV
            DiaV2rhs(i,j,M2hjvf)=0.0_r8
#  endif
# endif
#endif
          END DO
        END IF
      END DO
#ifdef UV_ADV
!
!-----------------------------------------------------------------------
!  Add in horizontal advection of momentum.
!-----------------------------------------------------------------------

# ifdef UV_C2ADVECTION
!
!  Second-order, centered differences advection fluxes.
!
      DO j=Jstr,Jend
        DO i=IstrU-1,Iend
          UFx(i,j)=0.25_r8*(DUon(i,j)+DUon(i+1,j))*                     &
     &                     (ubar(i  ,j,krhs)+                           &
     &                      ubar(i+1,j,krhs))
        END DO
      END DO
!
      DO j=Jstr,Jend+1
        DO i=IstrU,Iend
          UFe(i,j)=0.25_r8*(DVom(i,j)+DVom(i-1,j))*                     &
     &                     (ubar(i,j  ,krhs)+                           &
     &                      ubar(i,j-1,krhs))
        END DO
      END DO
!
      DO j=JstrV,Jend
        DO i=Istr,Iend+1
          VFx(i,j)=0.25_r8*(DUon(i,j)+DUon(i,j-1))*                     &
     &                     (vbar(i  ,j,krhs)+                           &
     &                      vbar(i-1,j,krhs))
        END DO
      END DO
!
      DO j=JstrV-1,Jend
        DO i=Istr,Iend
          VFe(i,j)=0.25_r8*(DVom(i,j)+DVom(i,j+1))*                     &
     &                     (vbar(i,j  ,krhs)+                           &
     &                      vbar(i,j+1,krhs))
        END DO
      END DO
# else
!
!  Fourth-order, centered differences advection fluxes.
!
      DO j=Jstr,Jend
        DO i=IstrUm1,Iendp1
          grad (i,j)=ubar(i-1,j,krhs)-2.0_r8*ubar(i,j,krhs)+            &
     &               ubar(i+1,j,krhs)
          Dgrad(i,j)=DUon(i-1,j)-2.0_r8*DUon(i,j)+DUon(i+1,j)
        END DO
      END DO
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=Jstr,Jend
            grad (Istr,j)=grad (Istr+1,j)
            Dgrad(Istr,j)=Dgrad(Istr+1,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=Jstr,Jend
            grad (Iend+1,j)=grad (Iend,j)
            Dgrad(Iend+1,j)=Dgrad(Iend,j)
          END DO
        END IF
      END IF

      cff=1.0_r8/6.0_r8
      DO j=Jstr,Jend
        DO i=IstrU-1,Iend
          UFx(i,j)=0.25_r8*(ubar(i  ,j,krhs)+                           &
     &                      ubar(i+1,j,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i+1,j)))*             &
     &                     (DUon(i,j)+DUon(i+1,j)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i+1,j)))
        END DO
      END DO
!
      DO j=Jstrm1,Jendp1
        DO i=IstrU,Iend
          grad(i,j)=ubar(i,j-1,krhs)-2.0_r8*ubar(i,j,krhs)+             &
     &              ubar(i,j+1,krhs)
        END DO
      END DO
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=IstrU,Iend
            grad(i,Jstr-1)=grad(i,Jstr)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=IstrU,Iend
            grad(i,Jend+1)=grad(i,Jend)
          END DO
        END IF
      END IF
      DO j=Jstr,Jend+1
        DO i=IstrU-1,Iend
          Dgrad(i,j)=DVom(i-1,j)-2.0_r8*DVom(i,j)+DVom(i+1,j)
        END DO
      END DO

      cff=1.0_r8/6.0_r8
      DO j=Jstr,Jend+1
        DO i=IstrU,Iend
          UFe(i,j)=0.25_r8*(ubar(i,j  ,krhs)+                           &
     &                      ubar(i,j-1,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i,j-1)))*             &
     &                     (DVom(i,j)+DVom(i-1,j)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i-1,j)))
        END DO
      END DO
!
      DO j=JstrV,Jend
        DO i=Istrm1,Iendp1
          grad(i,j)=vbar(i-1,j,krhs)-2.0_r8*vbar(i,j,krhs)+             &
     &              vbar(i+1,j,krhs)
        END DO
      END DO
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=JstrV,Jend
            grad(Istr-1,j)=grad(Istr,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=JstrV,Jend
            grad(Iend+1,j)=grad(Iend,j)
          END DO
        END IF
      END IF
      DO j=JstrV-1,Jend
        DO i=Istr,Iend+1
          Dgrad(i,j)=DUon(i,j-1)-2.0_r8*DUon(i,j)+DUon(i,j+1)
        END DO
      END DO

      cff=1.0_r8/6.0_r8
      DO j=JstrV,Jend
        DO i=Istr,Iend+1
          VFx(i,j)=0.25_r8*(vbar(i  ,j,krhs)+                           &
     &                      vbar(i-1,j,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i-1,j)))*             &
     &                     (DUon(i,j)+DUon(i,j-1)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i,j-1)))
        END DO
      END DO
!
      DO j=JstrVm1,Jendp1
        DO i=Istr,Iend
          grad(i,j)=vbar(i,j-1,krhs)-2.0_r8*vbar(i,j,krhs)+             &
     &              vbar(i,j+1,krhs)
          Dgrad(i,j)=DVom(i,j-1)-2.0_r8*DVom(i,j)+DVom(i,j+1)
        END DO
      END DO
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=Istr,Iend
            grad (i,Jstr)=grad (i,Jstr+1)
            Dgrad(i,Jstr)=Dgrad(i,Jstr+1)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=Istr,Iend
            grad (i,Jend+1)=grad (i,Jend)
            Dgrad(i,Jend+1)=Dgrad(i,Jend)
          END DO
        END IF
      END IF

      cff=1.0_r8/6.0_r8
      DO j=JstrV-1,Jend
        DO i=Istr,Iend
          VFe(i,j)=0.25_r8*(vbar(i,j  ,krhs)+                           &
     &                      vbar(i,j+1,krhs)-                           &
     &                      cff*(grad (i,j)+grad (i,j+1)))*             &
     &                     (DVom(i,j)+DVom(i,j+1)-                      &
     &                      cff*(Dgrad(i,j)+Dgrad(i,j+1)))
        END DO
      END DO
# endif
!
!  Add advection to RHS terms.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=UFx(i,j)-UFx(i-1,j)
          cff2=UFe(i,j+1)-UFe(i,j)
          fac=cff1+cff2
          rhs_ubar(i,j)=rhs_ubar(i,j)-fac
# if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2xadv)=-cff1
          DiaU2rhs(i,j,M2yadv)=-cff2
          DiaU2rhs(i,j,M2hadv)=-fac
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff1=VFx(i+1,j)-VFx(i,j)
          cff2=VFe(i,j)-VFe(i,j-1)
          fac=cff1+cff2
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac
# if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2xadv)=-cff1
          DiaV2rhs(i,j,M2yadv)=-cff2
          DiaV2rhs(i,j,M2hadv)=-fac
# endif
        END DO
      END DO
#endif

#ifdef UV_COR
!
!-----------------------------------------------------------------------
!  Add in Coriolis term.
!-----------------------------------------------------------------------
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=0.5_r8*Drhs(i,j)*fomn(i,j)
          UFx(i,j)=cff*(vbar(i,j  ,krhs)+                               &
     &                  vbar(i,j+1,krhs))
          VFe(i,j)=cff*(ubar(i  ,j,krhs)+                               &
     &                  ubar(i+1,j,krhs))
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac1
# if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2fcor)=fac1
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac1
# if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2fcor)=-fac1
# endif
        END DO
      END DO
!
# ifdef WEC
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=0.5_r8*Drhs(i,j)*fomn(i,j)
          UFx(i,j)=cff*(vbar_stokes(i,j  )+                             &
     &                  vbar_stokes(i,j+1))
          VFe(i,j)=cff*(ubar_stokes(i  ,j)+                             &
     &                  ubar_stokes(i+1,j))
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac1
#  if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2fsco)=fac1
#  endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac1
#  if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2fsco)=-fac1
#  endif
        END DO
      END DO
# endif
#endif

#if defined CURVGRID && defined UV_ADV
!
!-----------------------------------------------------------------------
!  Add in curvilinear transformation terms.
!-----------------------------------------------------------------------
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff1=0.5_r8*(vbar(i,j  ,krhs)+                                &
# ifdef WEC
     &                 vbar_stokes(i,j  )+                              &
     &                 vbar_stokes(i,j+1)+                              &
# endif
     &                 vbar(i,j+1,krhs))
          cff2=0.5_r8*(ubar(i  ,j,krhs)+                                &
# ifdef WEC
     &                 ubar_stokes(i  ,j)+                              &
     &                 ubar_stokes(i+1,j)+                              &
# endif
     &                 ubar(i+1,j,krhs))
          cff3=cff1*dndx(i,j)
          cff4=cff2*dmde(i,j)
# ifdef WEC_VF
          cff5=0.5_r8*(vbar_stokes(i,j  )+                             &
     &                 vbar_stokes(i,j+1))
          cff6=0.5_r8*(ubar_stokes(i  ,j)+                             &
     &                 ubar_stokes(i+1,j))
          cff7=cff5*dndx(i,j)
          cff8=cff6*dmde(i,j)
# endif
          cff=Drhs(i,j)*(cff3-cff4)
          UFx(i,j)=cff*cff1
          VFe(i,j)=cff*cff2
# ifdef WEC_VF
          UFx(i,j)=UFx(i,j)-(cff5*Drhs(i,j)*(cff7-cff8))
          VFe(i,j)=VFe(i,j)-(cff6*Drhs(i,j)*(cff7-cff8))
# endif
# if defined DIAGNOSTICS_UV
          cff=Drhs(i,j)*cff4
          Uwrk(i,j)=-cff*cff1                  ! ubar equation, ETA-term
          Vwrk(i,j)=-cff*cff2                  ! vbar equation, ETA-term
#  ifdef WEC_VF
            Uwrk(i,j)=Uwrk(i,j)+Drhs(i,j)*cff5*cff8
            Vwrk(i,j)=Vwrk(i,j)-Drhs(i,j)*cff6*cff8
#  endif
# endif
        END DO
      END DO
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac1=0.5_r8*(UFx(i,j)+UFx(i-1,j))
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac1
# if defined DIAGNOSTICS_UV
          fac2=0.5_r8*(Uwrk(i,j)+Uwrk(i-1,j))
          DiaU2rhs(i,j,M2xadv)=DiaU2rhs(i,j,M2xadv)+fac1-fac2
          DiaU2rhs(i,j,M2yadv)=DiaU2rhs(i,j,M2yadv)+fac2
          DiaU2rhs(i,j,M2hadv)=DiaU2rhs(i,j,M2hadv)+fac1
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac1=0.5_r8*(VFe(i,j)+VFe(i,j-1))
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac1
# if defined DIAGNOSTICS_UV
          fac2=0.5_r8*(Vwrk(i,j)+Vwrk(i,j-1))
          DiaV2rhs(i,j,M2xadv)=DiaV2rhs(i,j,M2xadv)-fac1+fac2
          DiaV2rhs(i,j,M2yadv)=DiaV2rhs(i,j,M2yadv)-fac2
          DiaV2rhs(i,j,M2hadv)=DiaV2rhs(i,j,M2hadv)-fac1
# endif
        END DO
      END DO
#endif
#if defined UV_VIS2 || defined UV_VIS4
!
!-----------------------------------------------------------------------
!  If horizontal mixing, compute total depth at PSI-points.
!-----------------------------------------------------------------------
!
# ifdef UV_VIS4
      DO j=Jstrm1,Jendp2
        DO i=Istrm1,Iendp2
# else
      DO j=Jstr,Jend+1
        DO i=Istr,Iend+1
# endif
          Drhs_p(i,j)=0.25_r8*(Drhs(i,j  )+Drhs(i-1,j  )+               &
     &                         Drhs(i,j-1)+Drhs(i-1,j-1))
        END DO
      END DO
#endif
#ifdef UV_VIS2
!
!-----------------------------------------------------------------------
!  Add in horizontal harmonic viscosity.
!-----------------------------------------------------------------------
!
!  Compute flux-components of the horizontal divergence of the stress
!  tensor (m5/s2) in XI- and ETA-directions.
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=visc2_r(i,j)*Drhs(i,j)*0.5_r8*                            &
     &        (pmon_r(i,j)*                                             &
     &         ((pn(i  ,j)+pn(i+1,j))*ubar(i+1,j,krhs)-                 &
     &          (pn(i-1,j)+pn(i  ,j))*ubar(i  ,j,krhs))-                &
     &         pnom_r(i,j)*                                             &
     &         ((pm(i,j  )+pm(i,j+1))*vbar(i,j+1,krhs)-                 &
     &          (pm(i,j-1)+pm(i,j  ))*vbar(i,j  ,krhs)))
          UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
          VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
        END DO
      END DO
      DO j=Jstr,Jend+1
        DO i=Istr,Iend+1
          cff=visc2_p(i,j)*Drhs_p(i,j)*0.5_r8*                          &
     &        (pmon_p(i,j)*                                             &
     &         ((pn(i  ,j-1)+pn(i  ,j))*vbar(i  ,j,krhs)-               &
     &          (pn(i-1,j-1)+pn(i-1,j))*vbar(i-1,j,krhs))+              &
     &         pnom_p(i,j)*                                             &
     &         ((pm(i-1,j  )+pm(i,j  ))*ubar(i,j  ,krhs)-               &
     &          (pm(i-1,j-1)+pm(i,j-1))*ubar(i,j-1,krhs)))
#  ifdef MASKING
          cff=cff*pmask(i,j)
#  endif
#  ifdef WET_DRY
          cff=cff*pmask_wet(i,j)
#  endif
          UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
          VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
        END DO
      END DO
!
!  Add in harmonic viscosity.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=0.5_r8*(pn(i-1,j)+pn(i,j))*(UFx(i,j  )-UFx(i-1,j))
          cff2=0.5_r8*(pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-UFe(i  ,j))
          fac=cff1+cff2
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac
# if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2hvis)=fac
          DiaU2rhs(i,j,M2xvis)=cff1
          DiaU2rhs(i,j,M2yvis)=cff2
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff1=0.5_r8*(pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-VFx(i,j  ))
          cff2=0.5_r8*(pm(i,j-1)+pm(i,j))*(VFe(i  ,j)-VFe(i,j-1))
          fac=cff1-cff2
          rhs_vbar(i,j)=rhs_vbar(i,j)+fac
# if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2hvis)=fac
          DiaV2rhs(i,j,M2xvis)= cff1
          DiaV2rhs(i,j,M2yvis)=-cff2
# endif
        END DO
      END DO
#endif
#ifdef UV_VIS4
!
!-----------------------------------------------------------------------
!  Add in horizontal biharmonic viscosity. The biharmonic operator
!  is computed by applying the harmonic operator twice.
!-----------------------------------------------------------------------
!
!  Compute flux-components of the horizontal divergence of the stress
!  tensor (m4 s^-3/2) in XI- and ETA-directions.  It is assumed here
!  that "visc4_r" and "visc4_p" are the squared root of the biharmonic
!  viscosity coefficient.  For momentum balance purposes, the total
!  thickness "D" appears only when computing the second harmonic
!  operator.
!
      DO j=JstrVm2,Jendp1
        DO i=IstrUm2,Iendp1
          cff=visc4_r(i,j)*0.5_r8*                                      &
     &        (pmon_r(i,j)*                                             &
     &         ((pn(i  ,j)+pn(i+1,j))*ubar(i+1,j,krhs)-                 &
     &          (pn(i-1,j)+pn(i  ,j))*ubar(i  ,j,krhs))-                &
     &         pnom_r(i,j)*                                             &
     &         ((pm(i,j  )+pm(i,j+1))*vbar(i,j+1,krhs)-                 &
     &          (pm(i,j-1)+pm(i,j  ))*vbar(i,j  ,krhs)))
          UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
          VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
        END DO
      END DO
      DO j=Jstrm1,Jendp2
        DO i=Istrm1,Iendp2
          cff=visc4_p(i,j)*0.5_r8*                                      &
     &        (pmon_p(i,j)*                                             &
     &         ((pn(i  ,j-1)+pn(i  ,j))*vbar(i  ,j,krhs)-               &
     &          (pn(i-1,j-1)+pn(i-1,j))*vbar(i-1,j,krhs))+              &
     &         pnom_p(i,j)*                                             &
     &         ((pm(i-1,j  )+pm(i,j  ))*ubar(i,j  ,krhs)-               &
     &          (pm(i-1,j-1)+pm(i,j-1))*ubar(i,j-1,krhs)))
# ifdef MASKING
          cff=cff*pmask(i,j)
# endif
# ifdef WET_DRY
          cff=cff*pmask_wet(i,j)
# endif
          UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
          VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
        END DO
      END DO
!
!  Compute first harmonic operator (m s^-3/2).
!
      DO j=Jstrm1,Jendp1
        DO i=IstrUm1,Iendp1
          LapU(i,j)=0.125_r8*                                           &
     &              (pm(i-1,j)+pm(i,j))*(pn(i-1,j)+pn(i,j))*            &
     &              ((pn(i-1,j)+pn(i,j))*                               &
     &               (UFx(i,j  )-UFx(i-1,j))+                           &
     &               (pm(i-1,j)+pm(i,j))*                               &
     &               (UFe(i,j+1)-UFe(i  ,j)))
        END DO
      END DO
      DO j=JstrVm1,Jendp1
        DO i=Istrm1,Iendp1
          LapV(i,j)=0.125_r8*                                           &
     &              (pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))*            &
     &              ((pn(i,j-1)+pn(i,j))*                               &
     &               (VFx(i+1,j)-VFx(i,j  ))-                           &
     &               (pm(i,j-1)+pm(i,j))*                               &
     &               (VFe(i  ,j)-VFe(i,j-1)))
        END DO
      END DO
!
!  Apply boundary conditions (other than periodic) to the first
!  harmonic operator. These are gradient or closed (free slip or
!  no slip) boundary conditions.
!
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          IF (LBC(iwest,isUbar,ng)%closed) THEN
            DO j=Jstrm1,Jendp1
              LapU(IstrU-1,j)=0.0_r8
            END DO
          ELSE
            DO j=Jstrm1,Jendp1
              LapU(IstrU-1,j)=LapU(IstrU,j)
            END DO
          END IF
          IF (LBC(iwest,isVbar,ng)%closed) THEN
            DO j=JstrVm1,Jendp1
              LapV(Istr-1,j)=gamma2(ng)*LapV(Istr,j)
            END DO
          ELSE
            DO j=JstrVm1,Jendp1
              LapV(Istr-1,j)=0.0_r8
            END DO
          END IF
        END IF
      END IF
!
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          IF (LBC(ieast,isUbar,ng)%closed) THEN
            DO j=Jstrm1,Jendp1
              LapU(Iend+1,j)=0.0_r8
            END DO
          ELSE
            DO j=Jstrm1,Jendp1
              LapU(Iend+1,j)=LapU(Iend,j)
            END DO
          END IF
          IF (LBC(ieast,isVbar,ng)%closed) THEN
            DO j=JstrVm1,Jendp1
              LapV(Iend+1,j)=gamma2(ng)*LapV(Iend,j)
            END DO
          ELSE
            DO j=JstrVm1,Jendp1
              LapV(Iend+1,j)=0.0_r8
            END DO
          END IF
        END IF
      END IF
!
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          IF (LBC(isouth,isUbar,ng)%closed) THEN
            DO i=IstrUm1,Iendp1
              LapU(i,Jstr-1)=gamma2(ng)*LapU(i,Jstr)
            END DO
          ELSE
            DO i=IstrUm1,Iendp1
              LapU(i,Jstr-1)=0.0_r8
            END DO
          END IF
          IF (LBC(isouth,isVbar,ng)%closed) THEN
            DO i=Istrm1,Iendp1
              LapV(i,JstrV-1)=0.0_r8
            END DO
          ELSE
            DO i=Istrm1,Iendp1
              LapV(i,JstrV-1)=LapV(i,JstrV)
            END DO
          END IF
        END IF
      END IF
!
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          IF (LBC(inorth,isUbar,ng)%closed) THEN
            DO i=IstrUm1,Iendp1
              LapU(i,Jend+1)=gamma2(ng)*LapU(i,Jend)
            END DO
          ELSE
            DO i=IstrUm1,Iendp1
              LapU(i,Jend+1)=0.0_r8
            END DO
          END IF
          IF (LBC(inorth,isVbar,ng)%closed) THEN
            DO i=Istrm1,Iendp1
              LapV(i,Jend+1)=0.0_r8
            END DO
          ELSE
            DO i=Istrm1,Iendp1
              LapV(i,Jend+1)=LapV(i,Jend)
            END DO
          END IF
        END IF
      END IF
!
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng).or.          &
     &          CompositeGrid(iwest ,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%SouthWest_Corner(tile)) THEN
          LapU(Istr  ,Jstr-1)=0.5_r8*(LapU(Istr+1,Jstr-1)+              &
     &                                LapU(Istr  ,Jstr  ))
          LapV(Istr-1,Jstr  )=0.5_r8*(LapV(Istr-1,Jstr+1)+              &
     &                                LapV(Istr  ,Jstr  ))
        END IF
      END IF

      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng).or.          &
     &          CompositeGrid(ieast ,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%SouthEast_Corner(tile)) THEN
          LapU(Iend+1,Jstr-1)=0.5_r8*(LapU(Iend  ,Jstr-1)+              &
     &                                LapU(Iend+1,Jstr  ))
          LapV(Iend+1,Jstr  )=0.5_r8*(LapV(Iend  ,Jstr  )+              &
     &                                LapV(Iend+1,Jstr+1))
        END IF
      END IF

      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng).or.          &
     &          CompositeGrid(iwest ,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%NorthWest_Corner(tile)) THEN
          LapU(Istr  ,Jend+1)=0.5_r8*(LapU(Istr+1,Jend+1)+              &
     &                                LapU(Istr  ,Jend  ))
          LapV(Istr-1,Jend+1)=0.5_r8*(LapV(Istr  ,Jend+1)+              &
     &                                LapV(Istr-1,Jend  ))
        END IF
      END IF

      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng).or.          &
     &          CompositeGrid(ieast ,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%NorthEast_Corner(tile)) THEN
          LapU(Iend+1,Jend+1)=0.5_r8*(LapU(Iend  ,Jend+1)+              &
     &                                LapU(Iend+1,Jend  ))
          LapV(Iend+1,Jend+1)=0.5_r8*(LapV(Iend  ,Jend+1)+              &
     &                                LapV(Iend+1,Jend  ))
        END IF
      END IF
!
!  Compute flux-components of the horizontal divergence of the
!  biharmonic stress tensor (m4/s2) in XI- and ETA-directions.
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          cff=visc4_r(i,j)*Drhs(i,j)*0.5_r8*                            &
     &        (pmon_r(i,j)*                                             &
     &         ((pn(i  ,j)+pn(i+1,j))*LapU(i+1,j)-                      &
     &          (pn(i-1,j)+pn(i  ,j))*LapU(i  ,j))-                     &
     &         pnom_r(i,j)*                                             &
     &         ((pm(i,j  )+pm(i,j+1))*LapV(i,j+1)-                      &
     &          (pm(i,j-1)+pm(i,j  ))*LapV(i,j  )))
          UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
          VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
        END DO
      END DO
      DO j=Jstr,Jend+1
        DO i=Istr,Iend+1
          cff=visc4_p(i,j)*Drhs_p(i,j)*0.5_r8*                          &
     &        (pmon_p(i,j)*                                             &
     &         ((pn(i  ,j-1)+pn(i  ,j))*LapV(i  ,j)-                    &
     &          (pn(i-1,j-1)+pn(i-1,j))*LapV(i-1,j))+                   &
     &         pnom_p(i,j)*                                             &
     &         ((pm(i-1,j  )+pm(i,j  ))*LapU(i,j  )-                    &
     &          (pm(i-1,j-1)+pm(i,j-1))*LapU(i,j-1)))
# ifdef MASKING
          cff=cff*pmask(i,j)
# endif
# ifdef WET_DRY
          cff=cff*pmask_wet(i,j)
# endif
          UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
          VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
        END DO
      END DO
!
!  Add in biharmonic viscosity.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=0.5_r8*(pn(i-1,j)+pn(i,j))*(UFx(i,j  )-UFx(i-1,j))
          cff2=0.5_r8*(pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-UFe(i  ,j))
          fac=cff1+cff2
          rhs_ubar(i,j)=rhs_ubar(i,j)-fac
# if defined DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2hvis)=-fac
          DiaU2rhs(i,j,M2xvis)=-cff1
          DiaU2rhs(i,j,M2yvis)=-cff2
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff1=0.5_r8*(pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-VFx(i,j  ))
          cff2=0.5_r8*(pm(i,j-1)+pm(i,j))*(VFe(i  ,j)-VFe(i,j-1))
          fac=cff1-cff2
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac
# if defined DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2hvis)=-fac
          DiaV2rhs(i,j,M2xvis)=-cff1
          DiaV2rhs(i,j,M2yvis)= cff2
# endif
        END DO
      END DO
#endif
#if defined WEC_VF && defined SOLVE3D
!
!-----------------------------------------------------------------------
!  Add in non-conservative roller terms.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff1=rubrk2d(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)+cff1
# ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2wbrk)=cff1
# endif
# ifdef WEC_ROLLER
          cff1=rurol2d(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2wrol)=cff1
#  endif
# else
#  ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2wrol)=0.0_r8
#  endif
# endif
# ifdef BOTTOM_STREAMING
          cff1=rubst2d(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2bstm)=cff1
#  endif
# endif
# ifdef SURFACE_STREAMING
          cff1=russt2d(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2sstm)=cff1
#  endif
# endif
        END DO
        IF (j.ge.JstrV) THEN
          DO i=Istr,Iend
            cff1=rvbrk2d(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)+cff1
# ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2wbrk)=cff1
# endif
# ifdef WEC_ROLLER
            cff1=rvrol2d(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2wrol)=cff1
#  endif
# else
#  ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2wrol)=0.0_r8
#  endif
# endif
# ifdef BOTTOM_STREAMING
            cff1=rvbst2d(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2bstm)=cff1
#  endif
# endif
# ifdef SURFACE_STREAMING
            cff1=rvsst2d(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)+cff1
#  ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2sstm)=cff1
#  endif
# endif
          END DO
        END IF
      END DO
# ifdef UV_ADV
#  ifdef DIAGNOSTICS_UV
!
!---------------------------------------------------------------------------
!  To obtain the full horizotal 'J' vortex force term:
!  Compute term for diagnostics only.  Subtract from hadv and add to vorf.
!---------------------------------------------------------------------------
!
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=0.5_r8*(Drhs(i-1,j)+Drhs(i,j))
            DVSom(i,j)=0.25_r8*cff*om_u(i,j)*                           &
     &                  (vbar_stokes(i  ,j  )+                          &
     &                   vbar_stokes(i  ,j+1)+                          &
     &                   vbar_stokes(i-1,j  )+                          &
     &                   vbar_stokes(i-1,j+1))
          END DO
        END DO
        DO j=Jstr,Jend+1
          DO i=IstrU,Iend
            UFx(i,j)=0.5_r8*(ubar(i  ,j-1,krhs)+                        &
                             ubar(i  ,j  ,krhs))
          END DO
        END DO
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff1=UFx(i,j+1)-UFx(i,j)
            cff=cff1*DVSom(i,j)
            DiaU2rhs(i,j,M2xadv)=DiaU2rhs(i,j,M2xadv)+cff
            DiaU2rhs(i,j,M2hadv)=DiaU2rhs(i,j,M2hadv)+cff
            DiaU2rhs(i,j,M2hjvf)=-cff
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=0.5_r8*(Drhs(i,j)+Drhs(i,j-1))
            DUSon(i,j)=cff*0.25_r8*on_v(i,j)*                           &
     &                  (ubar_stokes(i  ,j  )+                          &
     &                   ubar_stokes(i+1,j  )+                          &
     &                   ubar_stokes(i  ,j-1)+                          &
     &                   ubar_stokes(i+1,j-1))
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend+1
            VFe(i,j)=0.5_r8*(vbar(i-1,j  ,krhs)+                        &
     &                       vbar(i  ,j  ,krhs))
          END DO
        END DO
        DO i=Istr,Iend
          DO j=JstrV,Jend
            cff2=VFe(i+1,j)-VFe(i,j)
            cff=cff2*DUSon(i,j)
            DiaV2rhs(i,j,M2yadv)=DiaV2rhs(i,j,M2yadv)+cff
            DiaV2rhs(i,j,M2hadv)=DiaV2rhs(i,j,M2hadv)+cff
            DiaV2rhs(i,j,M2hjvf)=-cff
          END DO
        END DO
#  endif
!
!---------------------------------------------------------------------------
! Contribution of a term corresponding to product of
! Stokes and Eulerian Velocity Eqn. 26 and 27.
! This removes terms that were unneccessarily added in flux form.
!---------------------------------------------------------------------------
!
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=0.5_r8*(Drhs(i-1,j)+Drhs(i,j))
            DUSon(i,j)=cff*on_u(i,j)*ubar_stokes(i,j)
            DVSon(i,j)=0.25_r8*cff*on_u(i,j)*                           &
     &                  (vbar_stokes(i  ,j  )+                          &
     &                   vbar_stokes(i  ,j+1)+                          &
     &                   vbar_stokes(i-1,j  )+                          &
     &                   vbar_stokes(i-1,j+1))
          END DO
          DO i=IstrU-1,Iend
            UFx(i,j)=0.5_r8*(ubar(i  ,j  ,krhs)+                        &
                             ubar(i+1,j  ,krhs))
            VFx(i,j)=0.5_r8*(vbar(i  ,j  ,krhs)+                        &
     &                       vbar(i  ,j+1,krhs))
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=0.5_r8*(Drhs(i,j)+Drhs(i,j-1))
            DUSom(i,j)=cff*0.25_r8*om_v(i,j)*                           &
     &                  (ubar_stokes(i  ,j  )+                          &
     &                   ubar_stokes(i+1,j  )+                          &
     &                   ubar_stokes(i  ,j-1)+                          &
     &                   ubar_stokes(i+1,j-1))
            DVSom(i,j)=cff*om_v(i,j)*vbar_stokes(i,j)
          END DO
        END DO
        DO j=JstrV-1,Jend
          DO i=Istr,Iend
            cff=0.5_r8*(Drhs(i,j)+Drhs(i,j-1))
            UFe(i,j)=0.5_r8*(ubar(i+1,j  ,krhs)+                        &
     &                       ubar(i  ,j  ,krhs))
            VFe(i,j)=0.5_r8*(vbar(i  ,j  ,krhs)+                        &
     &                       vbar(i  ,j+1,krhs))
          END DO
        END DO
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff1=UFx(i,j)-UFx(i-1,j)
            cff2=VFx(i,j)-VFx(i-1,j)
            cff3=DUSon(i,j)*cff1
            cff4=DVSon(i,j)*cff2
            rhs_ubar(i,j)=rhs_ubar(i,j)+cff3+cff4
!           rustr2d(i,j)=rustr2d(i,j)-cff3-cff4
#  ifdef DIAGNOSTICS_UV
            DiaU2rhs(i,j,M2xadv)=DiaU2rhs(i,j,M2xadv)+cff3
            DiaU2rhs(i,j,M2hadv)=DiaU2rhs(i,j,M2hadv)+cff3
            DiaU2rhs(i,j,M2hjvf)=DiaU2rhs(i,j,M2hjvf)+cff4
#  endif
          END DO
        END DO
        DO i=Istr,Iend
          DO j=JstrV,Jend
            cff1=UFe(i,j)-UFe(i,j-1)
            cff2=VFe(i,j)-VFe(i,j-1)
            cff3=DUSom(i,j)*cff1
            cff4=DVSom(i,j)*cff2
            rhs_vbar(i,j)=rhs_vbar(i,j)+cff3+cff4
!           rvstr2d(i,j)=rvstr2d(i,j,k)-cff3-cff4
#  ifdef DIAGNOSTICS_UV
            DiaV2rhs(i,j,M2yadv)=DiaV2rhs(i,j,M2yadv)+cff4
            DiaV2rhs(i,j,M2hadv)=DiaV2rhs(i,j,M2hadv)+cff4
            DiaV2rhs(i,j,M2hjvf)=DiaV2rhs(i,j,M2hjvf)+cff3
#  endif
          END DO
        END DO
# endif
#endif
#ifndef SOLVE3D
!
!-----------------------------------------------------------------------
!  Add in bottom stress.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac=bustr(i,j)*om_u(i,j)*on_u(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)-fac
# ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2bstr)=-fac
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac=bvstr(i,j)*om_v(i,j)*on_v(i,j)
          rhs_vbar(i,j)=rhs_vbar(i,j)-fac
# ifdef DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2bstr)=-fac
# endif
        END DO
      END DO
#else
# ifdef DIAGNOSTICS_UV
!
!  Initialize the stress term if no bottom friction is defined.
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          DiaU2rhs(i,j,M2bstr)=0.0_r8
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          DiaV2rhs(i,j,M2bstr)=0.0_r8
        END DO
      END DO
# endif
#endif
!
!-----------------------------------------------------------------------
!  Add in nudging of 2D momentum climatology.
!-----------------------------------------------------------------------
!
      IF (LnudgeM2CLM(ng)) THEN
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=0.25_r8*(CLIMA(ng)%M2nudgcof(i-1,j)+                    &
     &                   CLIMA(ng)%M2nudgcof(i  ,j))*                   &
     &          om_u(i,j)*on_u(i,j)
            rhs_ubar(i,j)=rhs_ubar(i,j)+                                &
     &                    cff*(Drhs(i-1,j)+Drhs(i,j))*                  &
     &                        (CLIMA(ng)%ubarclm(i,j)-                  &
     &                         ubar(i,j,krhs))
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=0.25_r8*(CLIMA(ng)%M2nudgcof(i,j-1)+                    &
     &                   CLIMA(ng)%M2nudgcof(i,j  ))*                   &
     &          om_v(i,j)*on_v(i,j)
            rhs_vbar(i,j)=rhs_vbar(i,j)+                                &
     &                    cff*(Drhs(i,j-1)+Drhs(i,j))*                  &
     &                        (CLIMA(ng)%vbarclm(i,j)-                  &
     &                         vbar(i,j,krhs))
          END DO
        END DO
      END IF

#ifdef SOLVE3D
# ifdef WET_DRY
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,rhs_ubar(i,j))*umask_wet(i,j)
          cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          rhs_ubar(i,j)=rhs_ubar(i,j)*cff7
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
          cff6=0.5_r8+DSIGN(0.5_r8,rhs_vbar(i,j))*vmask_wet(i,j)
          cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
          rhs_vbar(i,j)=rhs_vbar(i,j)*cff7
        END DO
      END DO
# endif
!
!-----------------------------------------------------------------------
!  Coupling between 2D and 3D equations.
!-----------------------------------------------------------------------
!
!  Before the predictor step of the first barotropic time-step,
!  arrays "rufrc" and "rvfrc" contain the vertical integrals of
!  the 3D right-hand-side terms for momentum equations (including
!  surface and bottom stresses, if so prescribed).
!
!  Convert them into forcing terms by subtracting the fast time
!  "rhs_ubar" and "rhs_vbar" from them; Also, immediately apply
!  these forcing terms "rhs_ubar" and "rhs_vbar".
!
!  From now on, these newly computed forcing terms will remain
!  constant during the fast time stepping and will added to
!  "rhs_ubar" and "rhs_vbar" during all subsequent time steps.
!
      IF (FIRST_2D_STEP.and.PREDICTOR_2D_STEP(ng)) THEN
        IF (iic(ng).eq.ntfirst(ng)) THEN
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+rufrc(i,j)
              ru(i,j,0,nstp)=rufrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              DiaRUfrc(i,j,3,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                &
     &                             DiaRUfrc(i,j,3,M2pgrd)
#  endif
# endif
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+rvfrc(i,j)
              rv(i,j,0,nstp)=rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              DiaRVfrc(i,j,3,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                &
     &                             DiaRVfrc(i,j,3,M2pgrd)
#  endif
# endif
            END DO
          END DO
        ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+                              &
     &                      1.5_r8*rufrc(i,j)-0.5_r8*ru(i,j,0,nnew)
              ru(i,j,0,nstp)=rufrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              1.5_r8*DiaRUfrc(i,j,3,idiag)-       &
     &                              0.5_r8*DiaRUfrc(i,j,nnew,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=1.5_r8*DiaRUfrc(i,j,3,M2sstr)-       &
     &                             0.5_r8*DiaRUfrc(i,j,nnew,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=1.5_r8*DiaRUfrc(i,j,3,M2bstr)-       &
     &                             0.5_r8*DiaRUfrc(i,j,nnew,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#   ifdef WEC_VF
              DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                &
     &                             1.5_r8*DiaRUfrc(i,j,3,M2pgrd)-       &
     &                             0.5_r8*DiaRUfrc(i,j,nnew,M2pgrd)
#   endif
#  endif
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+                              &
     &                      1.5_r8*rvfrc(i,j)-0.5_r8*rv(i,j,0,nnew)
              rv(i,j,0,nstp)=rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              1.5_r8*DiaRVfrc(i,j,3,idiag)-       &
     &                              0.5_r8*DiaRVfrc(i,j,nnew,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=1.5_r8*DiaRVfrc(i,j,3,M2sstr)-       &
     &                             0.5_r8*DiaRVfrc(i,j,nnew,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=1.5_r8*DiaRVfrc(i,j,3,M2bstr)-       &
     &                             0.5_r8*DiaRVfrc(i,j,nnew,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                &
     &                             1.5_r8*DiaRVfrc(i,j,3,M2pgrd)-       &
     &                             0.5_r8*DiaRVfrc(i,j,nnew,M2pgrd)
#  endif
# endif
            END DO
          END DO
        ELSE
          cff1=23.0_r8/12.0_r8
          cff2=16.0_r8/12.0_r8
          cff3= 5.0_r8/12.0_r8
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              rufrc(i,j)=rufrc(i,j)-rhs_ubar(i,j)
              rhs_ubar(i,j)=rhs_ubar(i,j)+                              &
     &                      cff1*rufrc(i,j)-                            &
     &                      cff2*ru(i,j,0,nnew)+                        &
     &                      cff3*ru(i,j,0,nstp)
              ru(i,j,0,nstp)=rufrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRUfrc(i,j,3,idiag)=DiaRUfrc(i,j,3,idiag)-            &
     &                                DiaU2rhs(i,j,idiag)
                DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                &
     &                              cff1*DiaRUfrc(i,j,3,idiag)-         &
     &                              cff2*DiaRUfrc(i,j,nnew,idiag)+      &
     &                              cff3*DiaRUfrc(i,j,nstp,idiag)
                DiaRUfrc(i,j,nstp,idiag)=DiaRUfrc(i,j,3,idiag)
              END DO
              DiaU2rhs(i,j,M2sstr)=cff1*DiaRUfrc(i,j,3,M2sstr)-         &
     &                             cff2*DiaRUfrc(i,j,nnew,M2sstr)+      &
     &                             cff3*DiaRUfrc(i,j,nstp,M2sstr)
              DiaRUfrc(i,j,nstp,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
              DiaU2rhs(i,j,M2bstr)=cff1*DiaRUfrc(i,j,3,M2bstr)-         &
     &                             cff2*DiaRUfrc(i,j,nnew,M2bstr)+      &
     &                             cff3*DiaRUfrc(i,j,nstp,M2bstr)
              DiaRUfrc(i,j,nstp,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                &
     &                             cff1*DiaRUfrc(i,j,3,M2pgrd)-         &
     &                             cff2*DiaRUfrc(i,j,nnew,M2pgrd)+      &
     &                             cff3*DiaRUfrc(i,j,nstp,M2pgrd)
#  endif
# endif
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              rvfrc(i,j)=rvfrc(i,j)-rhs_vbar(i,j)
              rhs_vbar(i,j)=rhs_vbar(i,j)+                              &
     &                      cff1*rvfrc(i,j)-                            &
     &                      cff2*rv(i,j,0,nnew)+                        &
     &                      cff3*rv(i,j,0,nstp)
              rv(i,j,0,nstp)=rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M2pgrd
                DiaRVfrc(i,j,3,idiag)=DiaRVfrc(i,j,3,idiag)-            &
     &                                DiaV2rhs(i,j,idiag)
                DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                &
     &                              cff1*DiaRVfrc(i,j,3,idiag)-         &
     &                              cff2*DiaRVfrc(i,j,nnew,idiag)+      &
     &                              cff3*DiaRVfrc(i,j,nstp,idiag)
                DiaRVfrc(i,j,nstp,idiag)=DiaRVfrc(i,j,3,idiag)
              END DO
              DiaV2rhs(i,j,M2sstr)=cff1*DiaRVfrc(i,j,3,M2sstr)-         &
     &                             cff2*DiaRVfrc(i,j,nnew,M2sstr)+      &
     &                             cff3*DiaRVfrc(i,j,nstp,M2sstr)
              DiaRVfrc(i,j,nstp,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
              DiaV2rhs(i,j,M2bstr)=cff1*DiaRVfrc(i,j,3,M2bstr)-         &
     &                             cff2*DiaRVfrc(i,j,nnew,M2bstr)+      &
     &                             cff3*DiaRVfrc(i,j,nstp,M2bstr)
              DiaRVfrc(i,j,nstp,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
              DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                &
     &                             cff1*DiaRVfrc(i,j,3,M2pgrd)-         &
     &                             cff2*DiaRVfrc(i,j,nnew,M2pgrd)+      &
     &                             cff3*DiaRVfrc(i,j,nstp,M2pgrd)
#  endif
# endif
            END DO
          END DO
        END IF
      ELSE
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            rhs_ubar(i,j)=rhs_ubar(i,j)+rufrc(i,j)
# ifdef DIAGNOSTICS_UV
            DO idiag=1,M2pgrd
              DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)+                  &
     &                            DiaRUfrc(i,j,3,idiag)
            END DO
            DiaU2rhs(i,j,M2sstr)=DiaRUfrc(i,j,3,M2sstr)
            DiaU2rhs(i,j,M2bstr)=DiaRUfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
            DiaU2rhs(i,j,M2zeta)=DiaU2rhs(i,j,M2zeta)+                  &
     &                           DiaRUfrc(i,j,3,M2pgrd)
#  endif
# endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            rhs_vbar(i,j)=rhs_vbar(i,j)+rvfrc(i,j)
# ifdef DIAGNOSTICS_UV
            DO idiag=1,M2pgrd
              DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)+                  &
     &                            DiaRVfrc(i,j,3,idiag)
            END DO
            DiaV2rhs(i,j,M2sstr)=DiaRVfrc(i,j,3,M2sstr)
            DiaV2rhs(i,j,M2bstr)=DiaRVfrc(i,j,3,M2bstr)
#  ifdef WEC_VF
            DiaV2rhs(i,j,M2zeta)=DiaV2rhs(i,j,M2zeta)+                  &
     &                           DiaRVfrc(i,j,3,M2pgrd)
#  endif
# endif
          END DO
        END DO
      END IF
#else
!
!-----------------------------------------------------------------------
!  Add in surface momentum stress.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          fac=sustr(i,j)*om_u(i,j)*on_u(i,j)
          rhs_ubar(i,j)=rhs_ubar(i,j)+fac
# ifdef DIAGNOSTICS_UV
          DiaU2rhs(i,j,M2sstr)=fac
# endif
        END DO
      END DO
      DO j=JstrV,Jend
        DO i=Istr,Iend
          fac=svstr(i,j)*om_v(i,j)*on_v(i,j)
          rhs_vbar(i,j)=rhs_vbar(i,j)+fac
# ifdef DIAGNOSTICS_UV
          DiaV2rhs(i,j,M2sstr)=fac
# endif
        END DO
      END DO
#endif
!
!=======================================================================
!  Time step 2D momentum equations.
!=======================================================================
!
!  Compute total water column depth.
!
      DO j=JstrV-1,Jend
        DO i=IstrU-1,Iend
          Dstp(i,j)=zeta(i,j,kstp)+h(i,j)
        END DO
      END DO
!
!  During the first time-step, the predictor step is Forward-Euler
!  and the corrector step is Backward-Euler. Otherwise, the predictor
!  step is Leap-frog and the corrector step is Adams-Moulton.
!
      IF (FIRST_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)
#ifdef WET_DRY
        cff2=1.0_r8/cff1
#endif
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*cff1*rhs_ubar(i,j))*fac
#ifdef MASKING
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,ubar(i,j,knew))*umask_wet(i,j)
            cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            ubar(i,j,knew)=ubar(i,j,knew)*cff7
            rhs_ubar(i,j)=rhs_ubar(i,j)*cff7
# ifdef SOLVE3D
            IF (PREDICTOR_2D_STEP(ng)) THEN
              rufrc(i,j)=rufrc(i,j)*cff7
              ru(i,j,0,nstp)=rufrc(i,j)
            END IF
# endif
#endif
#if defined NESTING && !defined SOLVE3D
            DU_flux(i,j)=ubar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
#endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*cff1*rhs_vbar(i,j))*fac
#ifdef MASKING
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,vbar(i,j,knew))*vmask_wet(i,j)
            cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            vbar(i,j,knew)=vbar(i,j,knew)*cff7
            rhs_vbar(i,j)=rhs_vbar(i,j)*cff7
# ifdef SOLVE3D
            IF (PREDICTOR_2D_STEP(ng)) THEN
              rvfrc(i,j)=rvfrc(i,j)*cff7
              rv(i,j,0,nstp)=rvfrc(i,j)
            END IF
# endif
#endif
#if defined NESTING && !defined SOLVE3D
            DV_flux(i,j)=vbar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
#endif
          END DO
        END DO
      ELSE IF (PREDICTOR_2D_STEP(ng)) THEN
        cff1=dtfast(ng)
#ifdef WET_DRY
        cff2=1.0_r8/cff1
#endif
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*cff1*rhs_ubar(i,j))*fac
#ifdef MASKING
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,ubar(i,j,knew))*umask_wet(i,j)
            cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            ubar(i,j,knew)=ubar(i,j,knew)*cff7
            rhs_ubar(i,j)=rhs_ubar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DU_flux(i,j)=ubar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
#endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*cff1*rhs_vbar(i,j))*fac
#ifdef MASKING
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,vbar(i,j,knew))*vmask_wet(i,j)
            cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            vbar(i,j,knew)=vbar(i,j,knew)*cff7
            rhs_vbar(i,j)=rhs_vbar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DV_flux(i,j)=vbar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
#endif
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)*5.0_r8/12.0_r8
        cff2=0.5_r8*dtfast(ng)*8.0_r8/12.0_r8
        cff3=0.5_r8*dtfast(ng)*1.0_r8/12.0_r8
#ifdef WET_DRY
        cff4=1.0_r8/cff1
#endif
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            ubar(i,j,knew)=(ubar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i-1,j))+                    &
     &                      cff*(cff1*rhs_ubar(i,j)+                    &
     &                           cff2*rubar(i,j,kstp)-                  &
     &                           cff3*rubar(i,j,ptsk)))*fac
#ifdef MASKING
            ubar(i,j,knew)=ubar(i,j,knew)*umask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(umask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,ubar(i,j,knew))*umask_wet(i,j)
            cff7=0.5_r8*umask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            ubar(i,j,knew)=ubar(i,j,knew)*cff7
            rhs_ubar(i,j)=rhs_ubar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DU_flux(i,j)=ubar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
#endif
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            vbar(i,j,knew)=(vbar(i,j,kstp)*                             &
     &                      (Dstp(i,j)+Dstp(i,j-1))+                    &
     &                      cff*(cff1*rhs_vbar(i,j)+                    &
     &                           cff2*rvbar(i,j,kstp)-                  &
     &                           cff3*rvbar(i,j,ptsk)))*fac
#ifdef MASKING
            vbar(i,j,knew)=vbar(i,j,knew)*vmask(i,j)
#endif
#ifdef WET_DRY
            cff5=ABS(ABS(vmask_wet(i,j))-1.0_r8)
            cff6=0.5_r8+DSIGN(0.5_r8,vbar(i,j,knew))*vmask_wet(i,j)
            cff7=0.5_r8*vmask_wet(i,j)*cff5+cff6*(1.0_r8-cff5)
            vbar(i,j,knew)=vbar(i,j,knew)*cff7
            rhs_vbar(i,j)=rhs_vbar(i,j)*cff7
#endif
#if defined NESTING && !defined SOLVE3D
            DV_flux(i,j)=vbar(i,j,knew)*                                &
     &                   0.5_r8*(Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
#endif
          END DO
        END DO
      END IF

#ifdef DIAGNOSTICS_UV
!
!-----------------------------------------------------------------------
!  Time step 2D momentum diagnostic terms.
!-----------------------------------------------------------------------

# ifdef MASKING
!
!  Apply land/sea mask.
!
      DO idiag=1,NDM2d-1
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            DiaU2rhs(i,j,idiag)=DiaU2rhs(i,j,idiag)*umask(i,j)
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            DiaV2rhs(i,j,idiag)=DiaV2rhs(i,j,idiag)*vmask(i,j)
          END DO
        END DO
      END DO
# endif
# ifdef SOLVE3D
!
!  The arrays "DiaU2rhs" and "DiaV2rhs" contain the contributions of
!  each of the 2D right-hand-side terms for the momentum equations.
!
!  These values are integrated, time-stepped and converted to mass flux
!  units (m3 s-1) for coupling with the 3D diagnostic terms.
!
      fac=weight(1,iif(ng),ng)
      IF (FIRST_2D_STEP.and.CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaU2int(i,j,idiag)=cff1*DiaU2rhs(i,j,idiag)
              DiaU2wrk(i,j,idiag)=DiaU2int(i,j,idiag)*                  &
     &                            (pm(i-1,j)+pm(i,j))*fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaV2int(i,j,idiag)=cff1*DiaV2rhs(i,j,idiag)
              DiaV2wrk(i,j,idiag)=DiaV2int(i,j,idiag)*                  &
     &                            (pn(i,j)+pn(i,j-1))*fac
            END DO
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)*5.0_r8/12.0_r8
        cff2=0.5_r8*dtfast(ng)*8.0_r8/12.0_r8
        cff3=0.5_r8*dtfast(ng)*1.0_r8/12.0_r8
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaU2int(i,j,idiag)=DiaU2int(i,j,idiag)+                  &
     &                            (cff1*DiaU2rhs(i,j,idiag)+            &
     &                             cff2*DiaRUbar(i,j,kstp,idiag)-       &
     &                             cff3*DiaRUbar(i,j,ptsk,idiag))
              DiaU2wrk(i,j,idiag)=DiaU2wrk(i,j,idiag)+                  &
     &                            DiaU2int(i,j,idiag)*                  &
     &                            (pm(i-1,j)+pm(i,j))*fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaV2int(i,j,idiag)=DiaV2int(i,j,idiag)+                  &
     &                            (cff1*DiaV2rhs(i,j,idiag)+            &
     &                             cff2*DiaRVbar(i,j,kstp,idiag)-       &
     &                             cff3*DiaRVbar(i,j,ptsk,idiag))
              DiaV2wrk(i,j,idiag)=DiaV2wrk(i,j,idiag)+                  &
     &                            DiaV2int(i,j,idiag)*                  &
     &                            (pn(i,j)+pn(i,j-1))*fac
            END DO
          END DO
        END DO
      END IF
# else
!
!  Time-step the diagnostic terms.
!
      IF (FIRST_2D_STEP.and.CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
              fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
              DiaU2wrk(i,j,idiag)=cff*cff1*DiaU2rhs(i,j,idiag)*fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
              fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
              DiaV2wrk(i,j,idiag)=cff*cff1*DiaV2rhs(i,j,idiag)*fac
            END DO
          END DO
        END DO
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            DiaU2wrk(i,j,M2rate)=ubar(i,j,knew)-ubar(i,j,kstp)*         &
     &                           (Dstp(i,j)+Dstp(i-1,j))*fac
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            DiaV2wrk(i,j,M2rate)=vbar(i,j,knew)-vbar(i,j,kstp)*         &
     &                           (Dstp(i,j)+Dstp(i,j-1))*fac
          END DO
        END DO
      ELSE IF (CORRECTOR_2D_STEP) THEN
        cff1=0.5_r8*dtfast(ng)*5.0_r8/12.0_r8
        cff2=0.5_r8*dtfast(ng)*8.0_r8/12.0_r8
        cff3=0.5_r8*dtfast(ng)*1.0_r8/12.0_r8
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              cff=(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
              fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
              DiaU2wrk(i,j,idiag)=cff*(cff1*DiaU2rhs(i,j,idiag)+        &
     &                                 cff2*DiaRUbar(i,j,kstp,idiag)-   &
     &                                 cff3*DiaRUbar(i,j,ptsk,idiag))*  &
     &                                fac
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              cff=(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
              fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
              DiaV2wrk(i,j,idiag)=cff*(cff1*DiaV2rhs(i,j,idiag)+        &
     &                                 cff2*DiaRVbar(i,j,kstp,idiag)-   &
     &                                 cff3*DiaRVbar(i,j,ptsk,idiag))*  &
     &                                fac
            END DO
          END DO
        END DO
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            fac=1.0_r8/(Dnew(i,j)+Dnew(i-1,j))
            DiaU2wrk(i,j,M2rate)=ubar(i,j,knew)-                        &
     &                           ubar(i,j,kstp)*                        &
     &                           (Dstp(i,j)+Dstp(i-1,j))*fac
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            fac=1.0_r8/(Dnew(i,j)+Dnew(i,j-1))
            DiaV2wrk(i,j,M2rate)=vbar(i,j,knew)-                        &
     &                           vbar(i,j,kstp)*                        &
     &                           (Dstp(i,j)+Dstp(i,j-1))*fac
          END DO
        END DO
      END IF
# endif
#endif
!
!  If predictor step, load right-side-term into shared arrays for
!  future use during the subsequent corrector step.
!
      IF (PREDICTOR_2D_STEP(ng)) THEN
        DO j=Jstr,Jend
          DO i=IstrU,Iend
            rubar(i,j,krhs)=rhs_ubar(i,j)
          END DO
        END DO
        DO j=JstrV,Jend
          DO i=Istr,Iend
            rvbar(i,j,krhs)=rhs_vbar(i,j)
          END DO
        END DO
#ifdef DIAGNOSTICS_UV
        DO idiag=1,NDM2d-1
          DO j=Jstr,Jend
            DO i=IstrU,Iend
              DiaRUbar(i,j,krhs,idiag)=DiaU2rhs(i,j,idiag)
            END DO
          END DO
          DO j=JstrV,Jend
            DO i=Istr,Iend
              DiaRVbar(i,j,krhs,idiag)=DiaV2rhs(i,j,idiag)
            END DO
          END DO
        END DO
#endif
      END IF
!
!-----------------------------------------------------------------------
!  Apply lateral boundary conditions.
!-----------------------------------------------------------------------
!
      CALL u2dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 krhs, kstp, knew,                                &
     &                 ubar, vbar, zeta)
      CALL v2dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 krhs, kstp, knew,                                &
     &                 ubar, vbar, zeta)
!
!  Compute integral mass flux across open boundaries and adjust
!  for volume conservation.
!
      IF (ANY(VolCons(:,ng))) THEN
        CALL obc_flux_tile (ng, tile,                                   &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      IminS, ImaxS, JminS, JmaxS,                 &
     &                      knew,                                       &
#ifdef MASKING
     &                      umask, vmask,                               &
#endif
     &                      h, om_v, on_u,                              &
     &                      ubar, vbar, zeta)
      END IF

#if defined NESTING && !defined SOLVE3D
!
!-----------------------------------------------------------------------
!  Set barotropic fluxes along physical boundaries.
!-----------------------------------------------------------------------
!
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=Jstr-1,JendR
            Dnew(Istr-1,j)=h(Istr-1,j)+zeta_new(Istr-1,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=Jstr-1,JendR
            Dnew(Iend+1,j)=h(Iend+1,j)+zeta_new(Iend+1,j)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=Istr-1,IendR
            Dnew(i,Jstr-1)=h(i,Jstr-1)+zeta_new(i,Jstr-1)
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=Istr-1,IendR
            Dnew(i,Jend+1)=h(i,Jend+1)+zeta_new(i,Jend+1)
          END DO
        END IF
      END IF
!
      IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Western_Edge(tile)) THEN
          DO j=JstrR,JendR
            DU_flux(IstrU-1,j)=ubar(IstrU-1,j,knew)*on_u(IstrU-1,j)*    &
     &                         0.5_r8*(Dnew(IstrU-1,j)+Dnew(IstrU-2,j))
          END DO
          DO j=JstrV,Jend
            DV_flux(Istr-1,j)=vbar(Istr-1,j,knew)*om_v(Istr-1,j)*       &
     &                        0.5_r8*(Dnew(Istr-1,j)+Dnew(Istr-1,j-1))
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
        IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
          DO j=JstrR,JendR
            DU_flux(Iend+1,j)=ubar(Iend+1,j,knew)*on_u(iend+1,j)*       &
     &                        0.5_r8*(Dnew(Iend+1,j)+Dnew(Iend,j))
          END DO
          DO j=JstrV,Jend
            DV_flux(Iend+1,j)=vbar(Iend+1,j,knew)*om_v(Iend+1,j)*       &
     &                        0.5_r8*(Dnew(iend+1,j)+Dnew(iend+1,j-1))
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
          DO i=IstrU,Iend
            DU_flux(i,Jstr-1)=ubar(i,Jstr-1,knew)*on_u(i,Jstr-1)*       &
     &                        0.5_r8*(Dnew(i,Jstr-1)+Dnew(i-1,Jstr-1))
          END DO
          DO i=IstrR,IendR
            DV_flux(i,JstrV-1)=vbar(i,JstrV-1,knew)*om_v(i,JstrV-1)*    &
     &                         0.5_r8*(Dnew(i,JstrV-1)+Dnew(i,JstrV-2))
          END DO
        END IF
      END IF
      IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
        IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
          DO i=IstrU,Iend
            DU_flux(i,Jend+1)=ubar(i,Jend+1,knew)*on_u(i,Jend+1)*       &
     &                        0.5_r8*(Dnew(i,Jend+1)+Dnew(i-1,Jend+1))
          END DO
          DO i=IstrR,IendR
            DV_flux(i,Jend+1)=vbar(i,Jend+1,knew)*om_v(i,Jend+1)*       &
     &                        0.5_r8*(Dnew(i,Jend+1)+Dnew(i,Jend))
          END DO
        END IF
      END IF
#endif
!
!-----------------------------------------------------------------------
!  Apply momentum transport point sources (like river runoff), if any.
!
!    Dsrc(is) = 0,  flow across grid cell u-face (positive or negative)
!    Dsrc(is) = 1,  flow across grid cell v-face (positive or negative)
!-----------------------------------------------------------------------
!
      IF (LuvSrc(ng)) THEN
        DO is=1,Nsrc(ng)
          i=SOURCES(ng)%Isrc(is)
          j=SOURCES(ng)%Jsrc(is)
          IF (((IstrR.le.i).and.(i.le.IendR)).and.                      &
     &        ((JstrR.le.j).and.(j.le.JendR))) THEN
            IF (INT(SOURCES(ng)%Dsrc(is)).eq.0) THEN
              cff=1.0_r8/(on_u(i,j)*                                    &
     &                    0.5_r8*(zeta(i-1,j,knew)+h(i-1,j)+            &
     &                            zeta(i  ,j,knew)+h(i  ,j)))
              ubar(i,j,knew)=SOURCES(ng)%Qbar(is)*cff
#if defined NESTING && !defined SOLVE3D
              DU_flux(i,j)=SOURCES(ng)%Qbar(is)
#endif
            ELSE IF (INT(SOURCES(ng)%Dsrc(is)).eq.1) THEN
              cff=1.0_r8/(om_v(i,j)*                                    &
     &                    0.5_r8*(zeta(i,j-1,knew)+h(i,j-1)+            &
     &                            zeta(i,j  ,knew)+h(i,j  )))
              vbar(i,j,knew)=SOURCES(ng)%Qbar(is)*cff
#if defined NESTING && !defined SOLVE3D
              DV_flux(i,j)=SOURCES(ng)%Qbar(is)
#endif
            END IF
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Exchange boundary information.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          ubar(:,:,knew))
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          vbar(:,:,knew))

#if defined NESTING && !defined SOLVE3D
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          DU_flux)
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          DV_flux)
#endif
      END IF

#ifdef DISTRIBUTE
!
# if defined NESTING && !defined SOLVE3D
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
# else
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
# endif
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
# if defined NESTING && !defined SOLVE3D
     &                    DU_flux, DV_flux,                             &
# endif
     &                    ubar(:,:,knew),                               &
     &                    vbar(:,:,knew))
#endif
!
      RETURN
      END SUBROUTINE step2d_tile
!
      END MODULE step2d_mod
```

</details>

<a id="appendix-cpp"></a>

## 附录 B：当前 C++ 共用 helper、文件重组与结束部分

### B.1 完整文件的分块覆盖

正文按执行顺序讲解；原 C++ 定义顺序把 RHS 放在最后。每个源码块内部仍严格保持原文件行序。将下列块按表序连接，得到**完整当前 `kokkos_step2d.cpp` 的原始字节**，共 1513 行，不缺 include、外层门禁、匿名命名空间、C linkage、原注释或末尾 `#endif`。这避免全文再重复一次八个函数。

| 原文件顺序 | 块 ID | 当前原始行范围 | 内容 |
|---:|---|---:|---|
| 1 | [C-PREAMBLE](#src-c-preamble) | 1–249 | 版权、include、配置门禁、全部 helper、`extern "C"` 开始 |
| 2 | [C-DEPTH](#src-c-depth) | 250–320 | depth 函数、横幅及空行 |
| 3 | [C-TAVG](#src-c-tavg) | 321–409 | time_average 函数、横幅及空行 |
| 4 | [C-ZETA](#src-c-zeta) | 410–572 | zeta_step 函数、横幅及空行 |
| 5 | [C-COUP](#src-c-coup) | 573–807 | coupling 函数、横幅及空行 |
| 6 | [C-MOMT](#src-c-momt) | 808–976 | momentum_step 函数、横幅及空行 |
| 7 | [C-DIAG](#src-c-diag) | 977–1081 | diag_int 函数、横幅及空行 |
| 8 | [C-STOR](#src-c-stor) | 1082–1151 | store_rhs 函数、横幅及空行 |
| 9 | [C-RHS](#src-c-rhs) | 1152–1506 | rhs_fused 函数及原注释 |
| 10 | [C-FOOTER](#src-c-footer) | 1507–1513 | `extern "C"` 与 CPP 结束 |

### B.2 全部共用 helper 与配置门禁

其中 `range_policy`、`Shape`、`grid2d/grid3d/grid4d/priv2d/priv3d`、时间层常量、`ddx/ddy/ddx2/ddy2`、`u_rho_flux/v_psi_flux` 全部是真实完整定义。后两者是黏性通量数值 helper，不是标准库函数；它们的表达式也不能用“同理”替代。`Array*`、`as_*` 和 timer 定义见附录 C。

以下前导块与单个函数块本身不构成可独立编译的翻译单元；需要按 B.1 重组、带上真实头文件和项目 CPP/Kokkos 构建条件。重组后的实现仍依赖 ROMS/Kokkos，而非一个零依赖教学程序。

<a id="src-c-preamble"></a>

**源码块 `C-PREAMBLE`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d.cpp`，原始行 **1–249**，共 **249 行**。

<details>
<summary>当前 C++ 完整文件前导与全部 helper（点击展开完整代码）</summary>

<!-- source-block:C-PREAMBLE -->
```cpp
/*
** git $Id$
************************************************************************
** Copyright (c) 2002-2025 The ROMS Group                               **
**   Licensed under a MIT/X style license                               **
**   See License_ROMS.md                                                **
************************************************************************
**
** Kokkos ports of the compute stages of ROMS/Nonlinear/step2d_LF_AM3.h.
**
** Scope
** -----
** Only the pure computation sections live here.  Everything that talks
** to other tiles or ranks stays on the Fortran side and is interleaved
** between these calls: the halo exchanges (exchange_u2d_tile,
** exchange_v2d_tile, exchange_r2d_tile, mp_exchange2d), the open
** boundary routines (set_DUV_bc_tile, obc_flux_tile, u2dbc_tile,
** v2dbc_tile), zetabc_tile, wetdry_tile and the point sources.
**
** Each function corresponds to one contiguous block of loops in the
** Fortran source; its banner repeats the Fortran section comment so the
** two can be found side by side.
**
** Converged macros
** ----------------
** This implementation covers the macro combination of the upwelling
** test case: SOLVE3D, VAR_RHO_2D, UV_ADV, UV_COR, UV_VIS2 and
** DIAGNOSTICS_UV.  The compile-time guard below rejects everything
** else rather than silently computing the wrong thing.
**
** Threading
** ---------
** Backends are selected at build time:
**
**   ROMS_KOKKOS_SERIAL  -- ROMS owns the parallelism and splits work by
**                          tile across OpenMP threads; each call here
**                          runs a plain loop.  Safe under concurrent
**                          entry from several ROMS threads.
**
**   ROMS_KOKKOS_OPENMP  -- Kokkos owns the parallelism.  ROMS must run
**                          with a single OpenMP thread, otherwise the
**                          Kokkos parallel region either nests or
**                          serialises and the result is slow but still
**                          correct.  This is the useful configuration
**                          for benchmarking the kernels themselves.
**
**   ROMS_KOKKOS_SW64    -- one host thread per MPI rank dispatches to CPEs.
**                          HostSpace is shared; no array copy is required.
**
** The standalone driver initializes Kokkos before computing.  Guarded
** initialization at each C entry also supports direct/embedded callers.
**
** Do NOT use parallel_reduce: it touches the execution space instance's
** reduce scratch and is not safe under concurrent entry.
*/

#include "kokkos_step2d.hpp"

#if defined ROMS_KOKKOS

#include <cmath>

/* Only the stages that call these kernels need the physics below.  A build
** that enables none of them is free of the restriction, so an application
** with masking or a curvilinear grid can still be compiled with the stages
** it does support; the Fortran call sites are gated on the same macros
** (ROMS/Nonlinear/step2d_LF_AM3.h). */
#if defined KK_STAGE_FUSED || defined KK_STAGE_TAVG || defined KK_STAGE_COUP || \
    defined KK_STAGE_MOMT || defined KK_STAGE_DIAG || defined KK_STAGE_STOR

#if !defined SOLVE3D || !defined VAR_RHO_2D || !defined UV_ADV || \
    !defined UV_COR || !defined UV_VIS2 || !defined DIAGNOSTICS_UV
#error "Kokkos step2d currently requires the diagnostic 3-D upwelling physics"
#endif
#if defined MASKING || defined WET_DRY || defined WEC || defined WEC_VF || \
    defined NESTING || defined CURVGRID || defined UV_C2ADVECTION ||       \
    defined UV_VIS4 || defined SINGLE_PRECISION
#error "Unsupported Kokkos step2d physics or precision configuration"
#endif

namespace {

using roms::Array2d;
using roms::Array3d;
using roms::Array4d;
using roms::EdgeFlags;
using roms::TileBounds;

/*
** Execution space.  The selection is made in kokkos_config.hpp so that
** both this file and the test drivers agree on it.
*/

using exec_space = roms::exec_space;

/*
** Tile index ranges start at negative values (JstrV-2, IstrUm2-1, ...)
** because they reach into the ghost region, so the policy must be
** indexed with a signed type.  Kokkos::RangePolicy's default index type
** is size_t, which would silently wrap a negative lower bound.
*/

using range_policy = Kokkos::RangePolicy<exec_space, Kokkos::IndexType<int>>;

/*
** Bounds needed to wrap the arrays step2d_tile receives with declared
** shape (LBi:UBi, LBj:UBj).  The trailing dimension sizes come from the
** caller because the assumed-shape declarations do not carry them.
*/

struct Shape {
    int LBi, UBi, LBj, UBj;
    int IminS, ImaxS, JminS, JmaxS;
};

/* Array declared (LBi:UBi, LBj:UBj). */
inline Array2d<double> grid2d(void* p, const Shape &s) {
    return roms::as_2d<double>(p, s.LBi, s.UBi, s.LBj, s.UBj);
}

/* Array declared (LBi:UBi, LBj:UBj, lb_k:ub_k). */
inline Array3d<double> grid3d(void* p, const Shape &s, int lb_k, int ub_k) {
    return roms::as_3d<double>(p, s.LBi, s.UBi, s.LBj, s.UBj, lb_k, ub_k);
}

/* Array declared (LBi:UBi, LBj:UBj, lb_k:ub_k, lb_l:ub_l). */
inline Array4d<double> grid4d(void* p, const Shape &s, int lb_k, int ub_k,
                              int lb_l, int ub_l) {
    return roms::as_4d<double>(p, s.LBi, s.UBi, s.LBj, s.UBj, lb_k, ub_k,
                               lb_l, ub_l);
}

/* Tile-private scratch, declared (IminS:ImaxS, JminS:JmaxS). */
inline Array2d<double> priv2d(void* p, const Shape &s) {
    return roms::as_2d<double>(p, s.IminS, s.ImaxS, s.JminS, s.JmaxS);
}

/*
** The diagnostic arrays come in both shapes and confusing them is a
** silent out-of-bounds write: the wrapper's stride comes from the
** bounds, so private bounds on a grid-bounded array shift every column
** and run off the end.  In step2d_tile, DiaU2rhs/DiaV2rhs are tile
** scratch (IminS:ImaxS) while DiaRUbar/DiaRVbar/DiaRUfrc/DiaRVfrc are
** full-field (LBi:UBi) -- see the dummy declarations in
** ROMS/Nonlinear/step2d_LF_AM3.h.
*/

inline Array3d<double> priv3d(void* p, const Shape &s, int lb_k, int ub_k) {
    return roms::as_3d<double>(p, s.IminS, s.ImaxS, s.JminS, s.JmaxS, lb_k, ub_k);
}

/*
** The 2-D state arrays carry three time levels, the rhs history two.
** Neither extent is N(ng), which is what UBk means in the step2d_tile
** signature; using UBk for these would silently overrun.
*/

constexpr int TLEV0 = 1, TLEV1 = 3;
constexpr int RLEV0 = 1, RLEV1 = 2;

/*
** Second difference along xi or eta with the closed-edge fold.
**
** fold_lo / fold_hi: whether the low / high end of this direction is a
** closed edge (composite and periodic edges are excluded upstream, as
** in the Fortran).  lo_idx / hi_idx: the exact index that was filled.
*/

KOKKOS_INLINE_FUNCTION
double ddx(const roms::Array3d<double> &a, int k, int i, int j,
           int fold_lo, int fold_hi, int lo_idx, int hi_idx) {
    if (fold_lo && i == lo_idx)
        ++i;
    else if (fold_hi && i == hi_idx)
        --i;
    return a(i - 1, j, k) - 2.0 * a(i, j, k) + a(i + 1, j, k);
}

KOKKOS_INLINE_FUNCTION
double ddy(const roms::Array3d<double> &a, int k, int i, int j,
           int fold_lo, int fold_hi, int lo_idx, int hi_idx) {
    if (fold_lo && j == lo_idx)
        ++j;
    else if (fold_hi && j == hi_idx)
        --j;
    return a(i, j - 1, k) - 2.0 * a(i, j, k) + a(i, j + 1, k);
}

KOKKOS_INLINE_FUNCTION
double ddx2(const roms::Array2d<double> &a, int i, int j,
            int fold_lo, int fold_hi, int lo_idx, int hi_idx) {
    if (fold_lo && i == lo_idx)
        ++i;
    else if (fold_hi && i == hi_idx)
        --i;
    return a(i - 1, j) - 2.0 * a(i, j) + a(i + 1, j);
}

KOKKOS_INLINE_FUNCTION
double ddy2(const roms::Array2d<double> &a, int i, int j,
            int fold_lo, int fold_hi, int lo_idx, int hi_idx) {
    if (fold_lo && j == lo_idx)
        ++j;
    else if (fold_hi && j == hi_idx)
        --j;
    return a(i, j - 1) - 2.0 * a(i, j) + a(i, j + 1);
}

/*
** The rho-point viscosity flux of the ubar row at (i,j) and the psi-point
** viscosity flux of the vbar row at (i,j).  They are separate functions
** because the working item of row j evaluates them at i-1 and i+1 as
** well, and the neighbouring evaluation is the same expression: the
** caller carries it across the loop instead of evaluating it twice.
** The expressions are the Fortran ones, term by term.
*/

KOKKOS_INLINE_FUNCTION
double u_rho_flux(const Array2d<double> &on_r,
                  const Array2d<double> &visc2_r,
                  const Array2d<double> &Drhs,
                  const Array2d<double> &pmon_r,
                  const Array2d<double> &pnom_r,
                  const Array2d<double> &pm,
                  const Array2d<double> &pn,
                  const Array3d<double> &ubar,
                  const Array3d<double> &vbar,
                  int k, int i, int j) {
    return on_r(i, j) * on_r(i, j) * (visc2_r(i, j) * Drhs(i, j) * 0.5 * (pmon_r(i, j) * ((pn(i, j) + pn(i + 1, j)) * ubar(i + 1, j, k) - (pn(i - 1, j) + pn(i, j)) * ubar(i, j, k)) - pnom_r(i, j) * ((pm(i, j) + pm(i, j + 1)) * vbar(i, j + 1, k) - (pm(i, j - 1) + pm(i, j)) * vbar(i, j, k))));
}

KOKKOS_INLINE_FUNCTION
double v_psi_flux(const Array2d<double> &on_p,
                  const Array2d<double> &visc2_pp,
                  double dp,
                  const Array2d<double> &pmon_p,
                  const Array2d<double> &pnom_p,
                  const Array2d<double> &pm,
                  const Array2d<double> &pn,
                  const Array3d<double> &ubar,
                  const Array3d<double> &vbar,
                  int k, int i, int j) {
    return on_p(i, j) * on_p(i, j) * (visc2_pp(i, j) * dp * 0.5 * (pmon_p(i, j) * ((pn(i, j - 1) + pn(i, j)) * vbar(i, j, k) - (pn(i - 1, j - 1) + pn(i - 1, j)) * vbar(i - 1, j, k)) + pnom_p(i, j) * ((pm(i - 1, j) + pm(i, j)) * ubar(i, j, k) - (pm(i - 1, j - 1) + pm(i, j - 1)) * ubar(i, j - 1, k))));
}

} // namespace

extern "C" {

```

</details>

### B.3 完整结束部分

<a id="src-c-footer"></a>

**源码块 `C-FOOTER`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d.cpp`，原始行 **1507–1513**，共 **7 行**。

<details>
<summary>当前 C++ 完整文件结束部分（点击展开完整代码）</summary>

<!-- source-block:C-FOOTER -->
```cpp

} // extern "C"

#endif // KK_STAGE_FUSED || KK_STAGE_TAVG || KK_STAGE_COUP || KK_STAGE_MOMT ||
       // KK_STAGE_DIAG || KK_STAGE_STOR

#endif // ROMS_KOKKOS
```

</details>

<a id="appendix-config"></a>

## 附录 C：数组、执行空间、生命周期与计时

### C.1 完整 kokkos_config.hpp

为避免人为裁掉 namespace、CPP 和宏的上下文，下面收录完整头文件：八入口所用的 `Array2d/3d/4d`、`as_2d/as_3d/as_4d`、`EdgeFlags/TileBounds`、后端、生命周期声明、`ScopedTimer` 和计时宏都在其中。`Array5d/as_5d` 是同一真实头文件的其他成员，八入口并未使用，保留不表示 step2d 有 tracer 五维遍历。

`operator()` 的 `const` 修饰包装对象，却返回元素引用；输入只读是计算行为和调用约定，不是借此实现深层 const 或自动 alias 检查。最后一维长度不参与地址 stride 时不存储，不意味着调用者可传错实际尺寸。

<a id="src-c-config-hpp"></a>

**源码块 `C-CONFIG-HPP`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_config.hpp`，原始行 **1–346**，共 **346 行**。

<details>
<summary>完整 kokkos_config.hpp（点击展开完整代码）</summary>

<!-- source-block:C-CONFIG-HPP -->
```cpp
/*
** git $Id$
************************************************************************
** Copyright (c) 2002-2025 The ROMS Group                               **
**   Licensed under a MIT/X style license                               **
**   See License_ROMS.md                                                **
************************************************************************
**
** Kokkos kernel support for ROMS.
**
** This header provides the minimal glue needed to call Kokkos compute
** kernels from Fortran without copying arrays across the language
** boundary.  Fortran owns all memory; C++ only borrows raw pointers
** obtained from C_LOC.
**
** Design notes:
**
**   * Layout.  ROMS arrays are declared (LBi:UBi, LBj:UBj, ...).  The
**     first index varies fastest (column-major), so the linear offset
**     is  (i-lb_i) + (j-lb_j)*ni + (k-lb_k)*ni*nj.
**
**   * Execution space.  Chosen at build time (exec_space below):
**     Serial, OpenMP or SW64.  ROMS is already threaded over tiles
**     (see ROMS/Drivers/nl_roms.h, the "!$OMP PARALLEL" around main3d
**     with per-thread first_tile / last_tile), so with ROMS OpenMP
**     only the Serial backend is safe: its parallel_for expands to a
**     plain loop (core/src/Serial/Kokkos_Serial_Parallel_Range.hpp)
**     and holds no shared mutable state.  SW64 runs one host thread
**     per MPI rank and dispatches each launch to the 64 CPEs.
**
**   * No parallel_reduce.  Kokkos::Serial::ParallelReduce touches the
**     singleton's pool_reduce_local() scratch, which is not safe under
**     concurrent entry.  step2d has no global reductions in its hot
**     path, so every kernel here is a pure parallel_for.
*/

#ifndef ROMS_KOKKOS_CONFIG_HPP
#define ROMS_KOKKOS_CONFIG_HPP

#if defined ROMS_KOKKOS

/*
** The CPP option set.  This must be the same set the Fortran side is
** compiled with, and it is: "cppdefs.h" is the very header ROMS feeds
** its own preprocessor, and ROMS_HEADER / ROOT_DIR / ANALYTICAL_DIR
** already reach the C++ compile line through add_compile_definitions().
**
** Skipping this include is not a cosmetic mistake.  The Fortran caller
** hands a stage over to Kokkos by compiling the equivalent Fortran loops
** out, so a macro that only the Fortran side sees leaves the kernel
** computing a different (or empty) expression while the timestep still
** looks plausible -- the upwelling test ran to completion with KE
** bit-identical to the reference and only failed at step 72, when
** DIAGNOSTICS_UV asked for a field nothing had written.
*/

#include "cppdefs.h"

/* ROMS' CPU macro conflicts with Kokkos::SW64 and Tools::DeviceType::SW64.
** It is needed by the Fortran preprocessor, not by the C++ kernels. */
#ifdef SW64
#undef SW64
#endif

/*
** SW64 carries its own trap, and it has to be checked before Kokkos is
** pulled in so that a misconfigured build fails with this message rather
** than something incidental further down.
**
** Kokkos guards the entire SW64 backend with
**
**   #if defined(KOKKOS_ENABLE_SW64) && defined(__sw_hybrid_coding__)
**
** so a translation unit compiled without -mhybrid-coding gets an empty
** backend.  It still compiles: Kokkos_Setup_SW64.hpp falls back to an
** ordinary "KOKKOS_LAMBDA [&]" when __sw_hybrid_coding__ is absent, so
** the failure is silent rather than a build error.  Kokkos' own CMake
** applies -mhybrid-coding only to its two internal sources and does not
** propagate it, so the kernels must assert it here.
*/

#if defined(ROMS_KOKKOS_SW64) && !defined(__sw_hybrid_coding__)
#error "ROMS_KOKKOS_SW64 needs -mhybrid-coding on every kernel file; " \
         "__sw_hybrid_coding__ is not defined, which would silently "  \
         "compile the SW64 backend out."
#endif

#include <Kokkos_Core.hpp>

#include <cstddef>

/*
** Backend selection.  Exactly one must be defined; the build system sets
** it from ROMS_KOKKOS_BACKEND.
**
**   ROMS_KOKKOS_SERIAL  -- ROMS threads over tiles; kernels are plain
**                          loops and may be entered concurrently.
**   ROMS_KOKKOS_OPENMP  -- Kokkos threads inside each kernel; ROMS must
**                          run with OMP_NUM_THREADS=1 to avoid nesting.
**   ROMS_KOKKOS_SW64    -- Sunway manycore.  The SW64 execution space
**                          has memory_space == HostSpace, i.e. the
**                          "device" sees the same array the Fortran
**                          caller owns, so the borrowed-pointer scheme
**                          below works unchanged; there is no explicit
**                          staging copy to write.
*/

#if !defined(ROMS_KOKKOS_SERIAL) && !defined(ROMS_KOKKOS_OPENMP) && \
    !defined(ROMS_KOKKOS_SW64)
#define ROMS_KOKKOS_SERIAL
#endif

#if (defined(ROMS_KOKKOS_SERIAL) + defined(ROMS_KOKKOS_OPENMP) + \
     defined(ROMS_KOKKOS_SW64)) > 1
#error "Define only one of ROMS_KOKKOS_SERIAL / _OPENMP / _SW64"
#endif

namespace roms {

/*
** Borrowed Fortran array, indexed with the original Fortran bounds.
** operator() reproduces Fortran's column-major linearization so that
** kernel bodies can be written to match the Fortran source line for
** line. Only extents used as strides are stored; the final-axis extent
** is not needed for indexing, and these views do not check bounds.
*/

template <class T>
struct Array2d {
    T* p = nullptr;
    int lb_i = 0, lb_j = 0;
    int ni = 0;

    KOKKOS_INLINE_FUNCTION
    T &operator()(int i, int j) const {
        return p[std::ptrdiff_t(i - lb_i) + std::ptrdiff_t(j - lb_j) * ni];
    }
};

template <class T>
struct Array3d {
    T* p = nullptr;
    int lb_i = 0, lb_j = 0, lb_k = 0;
    int ni = 0, nj = 0;

    KOKKOS_INLINE_FUNCTION
    T &operator()(int i, int j, int k) const {
        return p[std::ptrdiff_t(i - lb_i) + std::ptrdiff_t(j - lb_j) * ni +
                 std::ptrdiff_t(k - lb_k) * ni * nj];
    }
};

template <class T>
struct Array4d {
    T* p = nullptr;
    int lb_i = 0, lb_j = 0, lb_k = 0, lb_l = 0;
    int ni = 0, nj = 0, nk = 0;

    KOKKOS_INLINE_FUNCTION
    T &operator()(int i, int j, int k, int l) const {
        return p[std::ptrdiff_t(i - lb_i) + std::ptrdiff_t(j - lb_j) * ni +
                 std::ptrdiff_t(k - lb_k) * ni * nj +
                 std::ptrdiff_t(l - lb_l) * ni * nj * nk];
    }
};

/* Tracers and their diagnostics each have five Fortran dimensions. */
template <class T>
struct Array5d {
    T* p = nullptr;
    int lb_i = 0, lb_j = 0, lb_k = 0, lb_l = 0, lb_m = 0;
    int ni = 0, nj = 0, nk = 0, nl = 0;

    KOKKOS_INLINE_FUNCTION
    T &operator()(int i, int j, int k, int l, int m) const {
        return p[std::ptrdiff_t(i - lb_i) + std::ptrdiff_t(j - lb_j) * ni +
                 std::ptrdiff_t(k - lb_k) * ni * nj +
                 std::ptrdiff_t(l - lb_l) * ni * nj * nk +
                 std::ptrdiff_t(m - lb_m) * ni * nj * nk * nl];
    }
};

/*
** Tile index metadata.  These are the subset of the 42 variables set by
** ROMS/Include/set_bounds.h that step2d_LF_AM3.h actually reads.  They
** are a pure function of (ng, tile) and are computed once per call on
** the Fortran side, then handed to every kernel as a single POD.
**
** Do not try to derive these from Istr: IstrU, JstrV and IstrR depend
** on the boundary-condition type of each edge and on periodic /
** composite / refined grid settings (see get_bounds.F around lines
** 1386-1412), so they must be taken as given.
*/

/*
** Domain edge flags.  Used by the advection stage, whose halo rows and
** columns are filled differently for physical, composite and periodic
** boundaries (see step2d_LF_AM3.h around lines 1343-1466).
*/

struct EdgeFlags {
    int west, east, south, north;
    int composite_w, composite_e, composite_s, composite_n;
    int periodic_ew, periodic_ns;
};

struct TileBounds {
    int Istr, Iend, Jstr, Jend;

    int IstrU, JstrV;
    int IstrR, JstrR, IendR, JendR;

    int Istrm1, Jstrm1;
    int Iendp1, Jendp1;
    int Iendp2, Jendp2;

    int IstrUm1, JstrVm1;
    int IstrUm2, JstrVm2;

    /* Trailing extents that the assumed-shape declarations do not carry. */

    int UBk; /* N(ng): top level of the 3-D arrays (ru/rv are 0:UBk).
              ** Not the time-level extent of ubar/vbar/zeta, which is 3. */
};

/*
** Pointer helpers.  Fortran passes TYPE(C_PTR); C++ receives void*.
** Accept full Fortran bounds at call sites, but do not store the unused
** final upper bound: only the preceding extents contribute to strides.
*/

template <class T>
inline Array2d<T> as_2d(void* ptr, int lb_i, int ub_i, int lb_j, int /*ub_j*/) {
    Array2d<T> a;
    a.p = static_cast<T*>(ptr);
    a.lb_i = lb_i;
    a.lb_j = lb_j;
    a.ni = ub_i - lb_i + 1;
    return a;
}

template <class T>
inline Array3d<T> as_3d(void* ptr, int lb_i, int ub_i, int lb_j, int ub_j,
                        int lb_k, int /*ub_k*/) {
    Array3d<T> a;
    a.p = static_cast<T*>(ptr);
    a.lb_i = lb_i;
    a.lb_j = lb_j;
    a.lb_k = lb_k;
    a.ni = ub_i - lb_i + 1;
    a.nj = ub_j - lb_j + 1;
    return a;
}

template <class T>
inline Array4d<T> as_4d(void* ptr, int lb_i, int ub_i, int lb_j, int ub_j,
                        int lb_k, int ub_k, int lb_l, int /*ub_l*/) {
    Array4d<T> a;
    a.p = static_cast<T*>(ptr);
    a.lb_i = lb_i;
    a.lb_j = lb_j;
    a.lb_k = lb_k;
    a.lb_l = lb_l;
    a.ni = ub_i - lb_i + 1;
    a.nj = ub_j - lb_j + 1;
    a.nk = ub_k - lb_k + 1;
    return a;
}

template <class T>
inline Array5d<T> as_5d(void* ptr, int li, int ui, int lj, int uj,
                        int lk, int uk, int ll, int ul, int lm, int /*um*/) {
    return {static_cast<T*>(ptr), li, lj, lk, ll, lm,
            ui - li + 1, uj - lj + 1, uk - lk + 1, ul - ll + 1};
}

/*
** The execution space the kernels run on.
*/

#if defined(ROMS_KOKKOS_OPENMP)
using exec_space = Kokkos::OpenMP;
#elif defined(ROMS_KOKKOS_SW64)
using exec_space = Kokkos::SW64;
#else
using exec_space = Kokkos::Serial;
#endif

/*
** Kokkos lifecycle.  initialize() is called once from ROMS after MPI is
** up and before the first kernel; it is guarded so that concurrent
** entry from several OpenMP threads is harmless.
**
** Scratch buffers are NOT owned here: the Fortran caller allocates them
** (it must be able to run halo exchanges on DUon/DVom/Drhs) and passes
** pointers in.  See the per-stage signatures in kokkos_step2d.hpp.
*/

void initialize();
void finalize();

/*
** Optional per-kernel wall time.  Set ROMS_KK_TIMING=1 in the job's
** environment; finalize() then prints calls, seconds and microseconds
** per call for every timed entry point.  Every launch used here returns
** only after its work is done (Serial runs the loop inline, SW64 joins
** the core group inside parallel_for), so the interval between entry and
** return is the kernel's own cost.  When disabled a timer costs one test
** of a cached flag.
*/

#if !defined(__sw_slave__)
int timer_slot(const char* name);
void timer_add(int slot, double seconds);
bool timer_enabled();
double wall_seconds();

class ScopedTimer {
public:
    explicit ScopedTimer(int slot)
        : slot_(timer_enabled() ? slot : -1),
          t0_(slot_ >= 0 ? wall_seconds() : 0.0) {}
    ~ScopedTimer() {
        if (slot_ >= 0)
            timer_add(slot_, wall_seconds() - t0_);
    }
    ScopedTimer(const ScopedTimer &) = delete;
    ScopedTimer &operator=(const ScopedTimer &) = delete;

private:
    int slot_;
    double t0_;
};

#define ROMS_KK_TIMER(name)                                    \
    static const int roms_kk_slot_ = ::roms::timer_slot(name); \
    ::roms::ScopedTimer roms_kk_timer_(roms_kk_slot_)
#else
#define ROMS_KK_TIMER(name)
#endif

} // namespace roms

#endif // ROMS_KOKKOS

#endif // ROMS_KOKKOS_CONFIG_HPP
```

</details>

### C.2 完整 kokkos_config.cpp

`roms::initialize/finalize`、可选 wall timer 和给 Fortran 的 C 生命周期入口在这里真正定义。`std::call_once` 保护初始化/结束，计时表用互斥保护；这些是运行时支持，不是海洋方程数值公式。入口计时包含实际函数调用区间，不能用它自动排除所有调度或初始化成本。对异步执行后端若未正确同步，单看同样的 host 时间范围也不能当作设备完成时间。

<a id="src-c-config-cpp"></a>

**源码块 `C-CONFIG-CPP`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_config.cpp`，原始行 **1–130**，共 **130 行**。

<details>
<summary>完整 kokkos_config.cpp（点击展开完整代码）</summary>

<!-- source-block:C-CONFIG-CPP -->
```cpp
/*
** git $Id$
************************************************************************
** Copyright (c) 2002-2025 The ROMS Group                               **
**   Licensed under a MIT/X style license                               **
**   See License_ROMS.md                                                **
************************************************************************
*/

#include "kokkos_config.hpp"

#if defined ROMS_KOKKOS && !defined(__sw_slave__)

#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <mutex>
#include <iostream>

namespace roms {

namespace {
std::once_flag init_flag;
std::once_flag fini_flag;

/* Per-kernel timing table (see ROMS_KK_TIMER in kokkos_config.hpp). */
constexpr int kMaxSlots = 64;
struct TimerSlot {
    const char* name = nullptr;
    unsigned long calls = 0;
    double seconds = 0.0;
};
TimerSlot g_slots[kMaxSlots];
int g_nslots = 0;
std::mutex g_timer_mutex;

void report_timers() {
    if (!timer_enabled() || g_nslots == 0)
        return;
    double total = 0.0;
    std::printf("\nROMS Kokkos kernel timing (wall, %s)\n", exec_space::name());
    std::printf("  %-24s %10s %12s %12s\n", "kernel", "calls", "seconds",
                "us/call");
    for (int s = 0; s < g_nslots; ++s) {
        const TimerSlot &t = g_slots[s];
        if (t.calls == 0)
            continue;
        total += t.seconds;
        std::printf("  %-24s %10lu %12.6f %12.3f\n", t.name, t.calls, t.seconds,
                    1.0e6 * t.seconds / double(t.calls));
    }
    std::printf("  %-24s %10s %12.6f\n", "sum", "", total);
    std::fflush(stdout);
}
} // namespace

bool timer_enabled() {
    static const bool on = [] {
        const char* v = std::getenv("ROMS_KK_TIMING");
        return v != nullptr && *v != '\0' && std::strcmp(v, "0") != 0;
    }();
    return on;
}

double wall_seconds() {
    return std::chrono::duration<double>(
               std::chrono::steady_clock::now().time_since_epoch())
        .count();
}

int timer_slot(const char* name) {
    std::lock_guard<std::mutex> lock(g_timer_mutex);
    for (int s = 0; s < g_nslots; ++s)
        if (std::strcmp(g_slots[s].name, name) == 0)
            return s;
    if (g_nslots == kMaxSlots)
        return -1;
    g_slots[g_nslots].name = name;
    return g_nslots++;
}

void timer_add(int slot, double seconds) {
    if (slot < 0 || slot >= kMaxSlots)
        return;
    std::lock_guard<std::mutex> lock(g_timer_mutex);
    ++g_slots[slot].calls;
    g_slots[slot].seconds += seconds;
}

/*
** ROMS calls main3d inside an OpenMP parallel region, so several threads
** can reach this at the same time.  std::call_once makes the first one
** do the work and the rest wait.
*/

void initialize() {
    std::call_once(init_flag, []() {
        if (!Kokkos::is_initialized()) {
            Kokkos::initialize();
        }
        std::cout << "ROMS Kokkos execution space: " << exec_space::name()
                  << std::endl;
        Kokkos::print_configuration(std::cout);
    });
}

void finalize() {
    std::call_once(fini_flag, []() {
        report_timers();
        if (Kokkos::is_initialized()) {
            Kokkos::finalize();
        }
    });
}

} // namespace roms

/*
** C entry points for the Fortran driver (ROMS/Cpp/kokkos_step2d_mod.F).
*/

extern "C" {

void roms_kokkos_initialize() { roms::initialize(); }
void roms_kokkos_finalize() { roms::finalize(); }

} // extern "C"

#endif // ROMS_KOKKOS
```

</details>

<a id="appendix-abi"></a>

## 附录 D：完整声明、BIND(C) 接口及当前调用端

### D.1 完整 C++ 入口声明

这是实现 include 的真实 `kokkos_step2d.hpp`，与附录 C 头文件配套，不是人工改写的简化签名。

<a id="src-c-entry-hpp"></a>

**源码块 `C-ENTRY-HPP`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d.hpp`，原始行 **1–169**，共 **169 行**。

<details>
<summary>完整 kokkos_step2d.hpp（点击展开完整代码）</summary>

<!-- source-block:C-ENTRY-HPP -->
```cpp
/*
** git $Id$
************************************************************************
** Copyright (c) 2002-2025 The ROMS Group                               **
**   Licensed under a MIT/X style license                               **
**   See License_ROMS.md                                                **
************************************************************************
*/

#ifndef ROMS_KOKKOS_STEP2D_HPP
#define ROMS_KOKKOS_STEP2D_HPP

#if defined ROMS_KOKKOS

#include "kokkos_config.hpp"

/*
** Kernel entry points for ROMS/Nonlinear/step2d_LF_AM3.h.
**
** Every entry point is declared extern "C" so the Fortran side can bind
** to it with BIND(C, NAME=...).  Scalar arguments are passed by value
** (INTEGER(C_INT), VALUE on the Fortran side); arrays arrive as void*
** taken from C_LOC, with the Fortran bounds needed to rebuild the index
** mapping.
**
** Conventions:
**
**   - `LBi..UBj` are the grid bounds step2d_tile uses to declare its
**     arguments; `IminS..JmaxS` are the tile-private scratch bounds.
**     Kernels never assume either starts at zero.
**
**   - `t` carries the tile index metadata, `e` the domain edge flags
**     (see kokkos_config.hpp).
**
**   - Scratch buffers are allocated by the Fortran caller and passed in,
**     so the caller can still run halo exchanges on the ones that need
**     it (Drhs, DUon, DVom).
**
**   - Diagnostic indices (M2pgrd etc.) are computed at run time in
**     ROMS/Modules/mod_scalars.F, so they are passed, not assumed.
**
** Not ported -- these stay in Fortran:
**   halo exchanges, set_DUV_bc_tile, obc_flux_tile, u2dbc_tile,
**   v2dbc_tile, zetabc_tile, wetdry_tile, point sources.
*/

extern "C" {

/* Total depth and vertically integrated mass fluxes. */

void k_step2d_depth_flux(int ng, int tile, int krhs,
                         int LBi, int UBi, int LBj, int UBj,
                         int IminS, int ImaxS, int JminS, int JmaxS,
                         void* zeta_p, void* h_p, void* on_u_p, void* om_v_p,
                         void* ubar_p, void* vbar_p, void* Drhs_p,
                         void* DUon_p, void* DVom_p,
                         const roms::TileBounds* t);

/* Time averaged fields over all short time steps (SOLVE3D). */

void k_step2d_time_average(int ng, int tile, int krhs,
                           int predictor, int first_step,
                           int LBi, int UBi, int LBj, int UBj,
                           int IminS, int ImaxS, int JminS, int JmaxS,
                           void* zeta_p, void* DUon_p, void* DVom_p,
                           void* Zt_avg1_p, void* DU_avg1_p, void* DU_avg2_p,
                           void* DV_avg1_p, void* DV_avg2_p,
                           double cff1, double cff2,
                           const roms::TileBounds* t);

/* Free surface advance (FE / LF / AM3). */

void k_step2d_zeta_step(int ng, int tile, int krhs, int kstp, int knew,
                        int predictor, int first_2d_step,
                        int LBi, int UBi, int LBj, int UBj,
                        int IminS, int ImaxS, int JminS, int JmaxS,
                        void* zeta_p, void* h_p, void* pm_p, void* pn_p,
                        void* DUon_p, void* DVom_p,
                        void* gzeta_p, void* gzeta2_p,
                        void* gzetaSA_p, void* rhoA_p, void* rhoS_p,
                        void* rhs_zeta_p, void* zeta_new_p,
                        void* Dnew_p, void* zwrk_p,
                        void* rzeta_p, double dtfast, double rho0,
                        const roms::TileBounds* t);

/* 2-D / 3-D coupling. */

void k_step2d_coupling(int ng, int tile, int istp, int inew,
                       int iic, int ntfirst, int first_2d_step,
                       int predictor,
                       int LBi, int UBi, int LBj, int UBj,
                       int IminS, int ImaxS, int JminS, int JmaxS,
                       void* rufrc_p, void* rvfrc_p,
                       void* ru_p, void* rv_p,
                       void* rhs_ubar_p, void* rhs_vbar_p,
                       void* DiaU2rhs_p, void* DiaV2rhs_p,
                       void* DiaRUfrc_p, void* DiaRVfrc_p,
                       int NDM2d, int M2pgrd, int M2sstr, int M2bstr,
                       const roms::TileBounds* t);

/* 2-D momentum time step. */

void k_step2d_momentum_step(int ng, int tile, int kstp, int knew,
                            int LBi, int UBi, int LBj, int UBj,
                            int IminS, int ImaxS, int JminS, int JmaxS,
                            void* zeta_p, void* h_p,
                            void* ubar_p, void* vbar_p,
                            void* rubar_p, void* rvbar_p,
                            void* pm_p, void* pn_p,
                            void* rhs_ubar_p, void* rhs_vbar_p,
                            void* Dstp_p, void* Dnew_p,
                            int predictor, int first_2d_step, double dtfast,
                            const roms::TileBounds* t);

/* Time-integrated 2-D momentum diagnostics (corrector). */

void k_step2d_diag_int(int ng, int tile, int kstp, int first_2d_step,
                       int LBi, int UBi, int LBj, int UBj,
                       int IminS, int ImaxS, int JminS, int JmaxS,
                       void* pm_p, void* pn_p,
                       void* DiaU2rhs_p, void* DiaV2rhs_p,
                       void* DiaRUbar_p, void* DiaRVbar_p,
                       void* DiaU2int_p, void* DiaV2int_p,
                       void* DiaU2wrk_p, void* DiaV2wrk_p,
                       int NDM2d, double cff1, double cff2, double cff3,
                       double fac, const roms::TileBounds* t);

/* Store the predictor right-hand side for the corrector. */

void k_step2d_store_rhs(int ng, int tile, int krhs,
                        int LBi, int UBi, int LBj, int UBj,
                        int IminS, int ImaxS, int JminS, int JmaxS,
                        void* rubar_p, void* rvbar_p,
                        void* rhs_ubar_p, void* rhs_vbar_p,
                        void* DiaRUbar_p, void* DiaRVbar_p,
                        void* DiaU2rhs_p, void* DiaV2rhs_p,
                        int NDM2d,
                        const roms::TileBounds* t);

/* Fused pressure, advection, Coriolis and harmonic-viscosity RHS. */
void k_step2d_rhs_fused(int ng, int tile, int krhs,
                        int LBi, int UBi, int LBj, int UBj,
                        int IminS, int ImaxS, int JminS, int JmaxS,
                        void* h_p, void* on_u_p, void* om_v_p,
                        void* gzeta_p, void* gzeta2_p, void* gzetaSA_p,
                        void* rhoA_p, void* zwrk_p,
                        void* Drhs_p, void* fomn_p,
                        void* ubar_p, void* vbar_p,
                        void* DUon_p, void* DVom_p,
                        void* pm_p, void* pn_p,
                        void* pmon_r_p, void* pnom_r_p,
                        void* pmon_p_p, void* pnom_p_p,
                        void* om_r_p, void* on_r_p,
                        void* om_p_p, void* on_p_p,
                        void* visc2_r_p, void* visc2_p2_p,
                        void* rhs_ubar_p, void* rhs_vbar_p,
                        void* DiaU2rhs_p, void* DiaV2rhs_p,
                        int NDM2d,
                        int M2pgrd, int M2xadv, int M2yadv, int M2hadv,
                        int M2fcor, int M2hvis, int M2xvis, int M2yvis,
                        double g,
                        const roms::EdgeFlags* e,
                        const roms::TileBounds* t);

} // extern "C"

#endif // ROMS_KOKKOS

#endif // ROMS_KOKKOS_STEP2D_HPP
```

</details>

### D.2 完整 Fortran BIND(C) 模块

- `INTEGER(C_INT)`、`REAL(C_DOUBLE)` 和 `C_PTR` 标量均使用 `VALUE`，分别对应 C++ 的 `int`、`double` 和裸指针值；实际 ROMS kinds 和构建需与其一致。
- `kokkos_tile_bounds/kokkos_edge_flags` 是 `BIND(C)` 派生类型，参数不带 `VALUE`，对应 C++ `const TileBounds*` / `const EdgeFlags*`。**字段顺序必须完全一致**，不能按名字想当然重新排序。
- 包装不分配数组，Fortran 保有所有权；实际调用示例见 D.4。各数组下界、tile 私有范围、时间/诊断维度缺一不可。
- 模块还包括 `kokkos_init/kokkos_finalize` 的真实生命周期接口。它本身仍依赖 `mod_kinds`，不等于脱离 ROMS 的独立项目。

<a id="src-f-abi-module"></a>

**源码块 `F-ABI-MODULE`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Cpp/kokkos_step2d_mod.F`，原始行 **1–325**，共 **325 行**。

<details>
<summary>完整 kokkos_step2d_mod.F（点击展开完整代码）</summary>

<!-- source-block:F-ABI-MODULE -->
```fortran
      MODULE kokkos_step2d_mod
!
!git $Id$
!=======================================================================
!  Copyright (c) 2002-2025 The ROMS Group                               !
!    Licensed under a MIT/X style license                               !
!    See License_ROMS.md                                                !
!=======================================================================
!                                                                      !
!  Fortran interfaces to the Kokkos compute kernels in ROMS/Cpp.       !
!                                                                      !
!  Only the pure computation stages of step2d are exposed here; the    !
!  halo exchanges, open-boundary routines and side boundary conditions !
!  stay in Fortran and are interleaved between these calls.            !
!                                                                      !
!  Arrays cross the language boundary as TYPE(C_PTR) taken from        !
!  C_LOC(), so no data is copied.  C_LOC requires the actual argument  !
!  to have the POINTER or TARGET attribute, which is why the           !
!  step2d_tile dummies and its scratch arrays carry TARGET.            !
!                                                                      !
!  Index conventions:                                                  !
!                                                                      !
!    LBi, UBi, LBj, UBj   grid bounds used to declare the arrays       !
!    IminS, ImaxS, ...    tile-private scratch bounds                  !
!    tl                   TileBounds,   see kokkos_config.hpp           !
!    de                   EdgeFlags,    see kokkos_config.hpp           !
!=======================================================================
!
      USE mod_kinds,    ONLY : r8, i4b
      USE, INTRINSIC :: iso_c_binding, ONLY : C_PTR, C_INT, C_DOUBLE
!
      implicit none
!
      PRIVATE
      PUBLIC :: kokkos_tile_bounds
      PUBLIC :: kokkos_edge_flags
      PUBLIC :: k_step2d_depth_flux
      PUBLIC :: k_step2d_time_average
      PUBLIC :: k_step2d_zeta_step
      PUBLIC :: k_step2d_rhs_fused
      PUBLIC :: k_step2d_coupling
      PUBLIC :: k_step2d_momentum_step
      PUBLIC :: k_step2d_diag_int
      PUBLIC :: k_step2d_store_rhs
      PUBLIC :: kokkos_init
      PUBLIC :: kokkos_finalize
!
!  Mirror of roms::TileBounds in ROMS/Cpp/kokkos_config.hpp.  The field
!  order must match exactly.
!
      TYPE, BIND(C) :: kokkos_tile_bounds
        integer(C_INT) :: Istr, Iend, Jstr, Jend
        integer(C_INT) :: IstrU, JstrV
        integer(C_INT) :: IstrR, JstrR, IendR, JendR
        integer(C_INT) :: Istrm1, Jstrm1
        integer(C_INT) :: Iendp1, Jendp1
        integer(C_INT) :: Iendp2, Jendp2
        integer(C_INT) :: IstrUm1, JstrVm1
        integer(C_INT) :: IstrUm2, JstrVm2
        integer(C_INT) :: UBk
      END TYPE kokkos_tile_bounds
!
!  Mirror of roms::EdgeFlags.
!
      TYPE, BIND(C) :: kokkos_edge_flags
        integer(C_INT) :: west, east, south, north
        integer(C_INT) :: composite_w, composite_e
        integer(C_INT) :: composite_s, composite_n
        integer(C_INT) :: periodic_ew, periodic_ns
      END TYPE kokkos_edge_flags
!
      INTERFACE
!
        SUBROUTINE k_step2d_depth_flux (kng, ktile, kkrhs,                  &
     &                                  LBi, UBi, LBj, UBj,              &
     &                                  IminS, ImaxS, JminS, JmaxS,      &
     &                                  zeta_p, h_p, on_u_p, om_v_p,     &
     &                                  ubar_p, vbar_p, Drhs_p,          &
     &                                  DUon_p, DVom_p, tl)              &
     &                                  BIND(C, NAME='k_step2d_depth_flux')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, kkrhs
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: zeta_p, h_p, on_u_p, om_v_p
          type(C_PTR),    VALUE :: ubar_p, vbar_p, Drhs_p
          type(C_PTR),    VALUE :: DUon_p, DVom_p
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_depth_flux
!
        SUBROUTINE k_step2d_time_average (kng, ktile, kkrhs,              &
     &                                    predictor, first_step,         &
     &                                    LBi, UBi, LBj, UBj,            &
     &                                    IminS, ImaxS, JminS, JmaxS,    &
     &                                    zeta_p, DUon_p, DVom_p,        &
     &                                    Zt_avg1_p, DU_avg1_p,          &
     &                                    DU_avg2_p, DV_avg1_p,          &
     &                                    DV_avg2_p, cff1, cff2, tl)     &
     &          BIND(C, NAME='k_step2d_time_average')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, kkrhs
          integer(C_INT), VALUE :: predictor, first_step
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: zeta_p
          type(C_PTR),    VALUE :: DUon_p, DVom_p
          type(C_PTR),    VALUE :: Zt_avg1_p, DU_avg1_p, DU_avg2_p
          type(C_PTR),    VALUE :: DV_avg1_p, DV_avg2_p
          real(C_DOUBLE), VALUE :: cff1, cff2
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_time_average
!
        SUBROUTINE k_step2d_zeta_step (kng, ktile, kkrhs, kstp, knew,       &
     &                                 predictor, first_2d_step,         &
     &                                 LBi, UBi, LBj, UBj,               &
     &                                 IminS, ImaxS, JminS, JmaxS,       &
     &                                 zeta_p, h_p, pm_p, pn_p,          &
     &                                 DUon_p, DVom_p,                   &
     &                                 gzeta_p, gzeta2_p,                &
     &                                 gzetaSA_p, rhoA_p, rhoS_p,        &
     &                                 rhs_zeta_p, zeta_new_p,           &
     &                                 Dnew_p, zwrk_p, rzeta_p,          &
     &                                 dtfast, rho0, tl)                 &
     &          BIND(C, NAME='k_step2d_zeta_step')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, kkrhs, kstp, knew
          integer(C_INT), VALUE :: predictor, first_2d_step
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: zeta_p, h_p, pm_p, pn_p
          type(C_PTR),    VALUE :: DUon_p, DVom_p
          type(C_PTR),    VALUE :: gzeta_p, gzeta2_p
          type(C_PTR),    VALUE :: gzetaSA_p, rhoA_p, rhoS_p
          type(C_PTR),    VALUE :: rhs_zeta_p, zeta_new_p
          type(C_PTR),    VALUE :: Dnew_p, zwrk_p, rzeta_p
          real(C_DOUBLE), VALUE :: dtfast, rho0
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_zeta_step
!
        SUBROUTINE k_step2d_rhs_fused (kng, ktile, krhs,                   &
     &                                 LBi, UBi, LBj, UBj,               &
     &                                 IminS, ImaxS, JminS, JmaxS,       &
     &                                 h_p, on_u_p, om_v_p,              &
     &                                 gzeta_p, gzeta2_p, gzetaSA_p,     &
     &                                 rhoA_p, zwrk_p,                   &
     &                                 Drhs_p, fomn_p,                   &
     &                                 ubar_p, vbar_p,                   &
     &                                 DUon_p, DVom_p,                   &
     &                                 pm_p, pn_p,                       &
     &                                 pmon_r_p, pnom_r_p,               &
     &                                 pmon_p_p, pnom_p_p,               &
     &                                 om_r_p, on_r_p,                   &
     &                                 om_p_p, on_p_p,                   &
     &                                 visc2_r_p, visc2_p2_p,            &
     &                                 rhs_ubar_p, rhs_vbar_p,           &
     &                                 DiaU2rhs_p, DiaV2rhs_p,           &
     &                                 NDM2d, M2pgrd, M2xadv, M2yadv,    &
     &                                 M2hadv, M2fcor, M2hvis, M2xvis,   &
     &                                 M2yvis, g, de, tl)                &
     &          BIND(C, NAME='k_step2d_rhs_fused')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds, kokkos_edge_flags
          integer(C_INT), VALUE :: kng, ktile, krhs
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: h_p, on_u_p, om_v_p
          type(C_PTR),    VALUE :: gzeta_p, gzeta2_p, gzetaSA_p
          type(C_PTR),    VALUE :: rhoA_p, zwrk_p
          type(C_PTR),    VALUE :: Drhs_p, fomn_p
          type(C_PTR),    VALUE :: ubar_p, vbar_p
          type(C_PTR),    VALUE :: DUon_p, DVom_p
          type(C_PTR),    VALUE :: pm_p, pn_p
          type(C_PTR),    VALUE :: pmon_r_p, pnom_r_p
          type(C_PTR),    VALUE :: pmon_p_p, pnom_p_p
          type(C_PTR),    VALUE :: om_r_p, on_r_p
          type(C_PTR),    VALUE :: om_p_p, on_p_p
          type(C_PTR),    VALUE :: visc2_r_p, visc2_p2_p
          type(C_PTR),    VALUE :: rhs_ubar_p, rhs_vbar_p
          type(C_PTR),    VALUE :: DiaU2rhs_p, DiaV2rhs_p
          integer(C_INT), VALUE :: NDM2d, M2pgrd, M2xadv, M2yadv
          integer(C_INT), VALUE :: M2hadv, M2fcor, M2hvis, M2xvis
          integer(C_INT), VALUE :: M2yvis
          real(C_DOUBLE), VALUE :: g
          type(kokkos_edge_flags),  INTENT(IN) :: de
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_rhs_fused
!
        SUBROUTINE k_step2d_coupling (kng, ktile, istp, inew,            &
     &                                iic, ntfirst, first_2d_step,       &
     &                                predictor,                         &
     &                                LBi, UBi, LBj, UBj,                &
     &                                IminS, ImaxS, JminS, JmaxS,        &
     &                                rufrc_p, rvfrc_p,                  &
     &                                ru_p, rv_p,                        &
     &                                rhs_ubar_p, rhs_vbar_p,            &
     &                                DiaU2rhs_p, DiaV2rhs_p,            &
     &                                DiaRUfrc_p, DiaRVfrc_p,            &
     &                                NDM2d, M2pgrd, M2sstr, M2bstr, tl) &
     &          BIND(C, NAME='k_step2d_coupling')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, istp, inew
          integer(C_INT), VALUE :: iic, ntfirst, first_2d_step, predictor
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: rufrc_p, rvfrc_p
          type(C_PTR),    VALUE :: ru_p, rv_p
          type(C_PTR),    VALUE :: rhs_ubar_p, rhs_vbar_p
          type(C_PTR),    VALUE :: DiaU2rhs_p, DiaV2rhs_p
          type(C_PTR),    VALUE :: DiaRUfrc_p, DiaRVfrc_p
          integer(C_INT), VALUE :: NDM2d, M2pgrd, M2sstr, M2bstr
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_coupling
!
        SUBROUTINE k_step2d_momentum_step (kng, ktile, kstp, knew,         &
     &                                     LBi, UBi, LBj, UBj,           &
     &                                     IminS, ImaxS, JminS, JmaxS,   &
     &                                     zeta_p, h_p,                  &
     &                                     ubar_p, vbar_p,               &
     &                                     rubar_p, rvbar_p,             &
     &                                     pm_p, pn_p,                   &
     &                                     rhs_ubar_p, rhs_vbar_p,       &
     &                                     Dstp_p, Dnew_p,               &
     &                                     predictor, first_2d_step,     &
     &                                     dtfast, tl)                   &
     &          BIND(C, NAME='k_step2d_momentum_step')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, kstp, knew
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: zeta_p, h_p
          type(C_PTR),    VALUE :: ubar_p, vbar_p
          type(C_PTR),    VALUE :: rubar_p, rvbar_p
          type(C_PTR),    VALUE :: pm_p, pn_p
          type(C_PTR),    VALUE :: rhs_ubar_p, rhs_vbar_p
          type(C_PTR),    VALUE :: Dstp_p, Dnew_p
          integer(C_INT), VALUE :: predictor, first_2d_step
          real(C_DOUBLE), VALUE :: dtfast
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_momentum_step
!
        SUBROUTINE k_step2d_diag_int (kng, ktile, kstp, first_2d_step,     &
     &                                LBi, UBi, LBj, UBj,                &
     &                                IminS, ImaxS, JminS, JmaxS,        &
     &                                pm_p, pn_p,                        &
     &                                DiaU2rhs_p, DiaV2rhs_p,            &
     &                                DiaRUbar_p, DiaRVbar_p,            &
     &                                DiaU2int_p, DiaV2int_p,            &
     &                                DiaU2wrk_p, DiaV2wrk_p,            &
     &                                NDM2d, cff1, cff2, cff3, fac, tl)  &
     &          BIND(C, NAME='k_step2d_diag_int')
          IMPORT :: C_PTR, C_INT, C_DOUBLE, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, kstp, first_2d_step
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: pm_p, pn_p
          type(C_PTR),    VALUE :: DiaU2rhs_p, DiaV2rhs_p
          type(C_PTR),    VALUE :: DiaRUbar_p, DiaRVbar_p
          type(C_PTR),    VALUE :: DiaU2int_p, DiaV2int_p
          type(C_PTR),    VALUE :: DiaU2wrk_p, DiaV2wrk_p
          integer(C_INT), VALUE :: NDM2d
          real(C_DOUBLE), VALUE :: cff1, cff2, cff3, fac
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_diag_int
!
        SUBROUTINE k_step2d_store_rhs (kng, ktile, kkrhs,                   &
     &                                 LBi, UBi, LBj, UBj,               &
     &                                 IminS, ImaxS, JminS, JmaxS,       &
     &                                 rubar_p, rvbar_p,                 &
     &                                 rhs_ubar_p, rhs_vbar_p,           &
     &                                 DiaRUbar_p, DiaRVbar_p,           &
     &                                 DiaU2rhs_p, DiaV2rhs_p,           &
     &                                 NDM2d, tl)                        &
     &          BIND(C, NAME='k_step2d_store_rhs')
          IMPORT :: C_PTR, C_INT, kokkos_tile_bounds
          integer(C_INT), VALUE :: kng, ktile, kkrhs
          integer(C_INT), VALUE :: LBi, UBi, LBj, UBj
          integer(C_INT), VALUE :: IminS, ImaxS, JminS, JmaxS
          type(C_PTR),    VALUE :: rubar_p, rvbar_p
          type(C_PTR),    VALUE :: rhs_ubar_p, rhs_vbar_p
          type(C_PTR),    VALUE :: DiaRUbar_p, DiaRVbar_p
          type(C_PTR),    VALUE :: DiaU2rhs_p, DiaV2rhs_p
          integer(C_INT), VALUE :: NDM2d
          type(kokkos_tile_bounds), INTENT(IN) :: tl
        END SUBROUTINE k_step2d_store_rhs
!
      END INTERFACE
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE kokkos_init
!***********************************************************************
!
!  Initialize Kokkos once.  Called after MPI is up and before the first
!  kernel.  The C++ side guards this with std::call_once, so it is safe
!  even though every OpenMP thread reaches the driver.
!
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE roms_kokkos_initialize () BIND(C,                      &
     &      NAME='roms_kokkos_initialize')
        END SUBROUTINE roms_kokkos_initialize
      END INTERFACE
!
      CALL roms_kokkos_initialize
!
      RETURN
      END SUBROUTINE kokkos_init
!
!***********************************************************************
      SUBROUTINE kokkos_finalize
!***********************************************************************
!
      IMPLICIT NONE
      INTERFACE
        SUBROUTINE roms_kokkos_finalize () BIND(C,                        &
     &      NAME='roms_kokkos_finalize')
        END SUBROUTINE roms_kokkos_finalize
      END INTERFACE
!
      CALL roms_kokkos_finalize
!
      RETURN
      END SUBROUTINE kokkos_finalize
!
      END MODULE kokkos_step2d_mod
```

</details>

### D.3 FIRST_2D_STEP 与元数据的真实来源

下面宏定义来自当前 `globaldefs.h`，不是按解释重造的伪代码。后续元数据块来自当前 `step2d_tile`，原样取得边界、halo 范围和 domain flags，而不是 C++ 自行从 Istr/Jstr 推导。

<a id="src-f-first-macro"></a>

**源码块 `F-FIRST-MACRO`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Include/globaldefs.h`，原始行 **168–173**，共 **6 行**。

<details>
<summary>真实 FIRST_2D_STEP 条件定义（点击展开完整代码）</summary>

<!-- source-block:F-FIRST-MACRO -->
```c
#ifdef SOLVE3D
# define FIRST_2D_STEP iif(ng).eq.1
# define FIRST_TIME_STEP iic(ng).eq.ntfirst(ng)
#else
# define FIRST_2D_STEP iic(ng).eq.ntfirst(ng)
#endif
```

</details>

<a id="src-f-tile-meta"></a>

**源码块 `F-TILE-META`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **630–671**，共 **42 行**。

<details>
<summary>当前 Fortran 的完整 tile/edge 元数据填充块（点击展开完整代码）</summary>

<!-- source-block:F-TILE-META -->
```fortran
#ifdef ROMS_KOKKOS
!
!  Populate the tile index metadata handed to the Kokkos kernels.
!
      KK_TILE%Istr   =Istr
      KK_TILE%Iend   =Iend
      KK_TILE%Jstr   =Jstr
      KK_TILE%Jend   =Jend
      KK_TILE%IstrU  =IstrU
      KK_TILE%JstrV  =JstrV
      KK_TILE%IstrR  =IstrR
      KK_TILE%JstrR  =JstrR
      KK_TILE%IendR  =IendR
      KK_TILE%JendR  =JendR
      KK_TILE%Istrm1 =Istrm1
      KK_TILE%Jstrm1 =Jstrm1
      KK_TILE%Iendp1 =Iendp1
      KK_TILE%Jendp1 =Jendp1
      KK_TILE%Iendp2 =Iendp2
      KK_TILE%Jendp2 =Jendp2
      KK_TILE%IstrUm1=IstrUm1
      KK_TILE%JstrVm1=JstrVm1
      KK_TILE%IstrUm2=IstrUm2
      KK_TILE%JstrVm2=JstrVm2
      KK_TILE%UBk    =UBk
!
!  Domain edge flags for the advection halo fills.  The Fortran
!  conditions are:
!     .not.(CompositeGrid(iedge,ng).or.<dir>periodic(ng)) .and.
!     DOMAIN(ng)%<edge>_Edge(tile)
!
      KK_EDGE%west        =MERGE(1,0,DOMAIN(ng)%Western_Edge (tile))
      KK_EDGE%east        =MERGE(1,0,DOMAIN(ng)%Eastern_Edge (tile))
      KK_EDGE%south       =MERGE(1,0,DOMAIN(ng)%Southern_Edge(tile))
      KK_EDGE%north       =MERGE(1,0,DOMAIN(ng)%Northern_Edge(tile))
      KK_EDGE%composite_w =MERGE(1,0,CompositeGrid(iwest ,ng))
      KK_EDGE%composite_e =MERGE(1,0,CompositeGrid(ieast ,ng))
      KK_EDGE%composite_s =MERGE(1,0,CompositeGrid(isouth,ng))
      KK_EDGE%composite_n =MERGE(1,0,CompositeGrid(inorth,ng))
      KK_EDGE%periodic_ew =MERGE(1,0,EWperiodic(ng))
      KK_EDGE%periodic_ns =MERGE(1,0,NSperiodic(ng))
#endif
```

</details>

### D.4 八个实际调用与留在 host 的系数

以下均是当前 Fortran 的原文调用片段，不是可单独编译的子程序，也不补造片段外 CPP。阶段/外层 guard 见第 1 节总表：depth、zeta、RHS 用 FUSED，平均用 TAVG，耦合用 COUP，动量用 MOMT，诊断用 DIAG，保存用 STOR，均要求 `ROMS_KOKKOS`。time_average、coupling 和 diag_int 还处在对应 SOLVE3D/DIAGNOSTICS_UV 外层。

平均片段完整保留两系数的 host 计算；诊断片段保留 corrector guard 与系数分支，其 `fac` 在当前第 2958 行先取 `weight(1,iif(ng),ng)`，正文原始片段已有这行。store 片段完整保留 predictor guard。其余代码只是完整 CALL 的多行实参。`MERGE(1,0,...)` 明确把逻辑条件转为 C 可互操作整数；`C_LOC` 明确传入原数组存储。

#### depth_flux

<a id="src-f-call-depth"></a>

**源码块 `F-CALL-DEPTH`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **682–689**，共 **8 行**。

<details>
<summary>当前 Fortran 调用：depth_flux（点击展开完整代码）</summary>

<!-- source-block:F-CALL-DEPTH -->
```fortran
      CALL k_step2d_depth_flux (ng, tile, krhs,                         &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          C_LOC(zeta), C_LOC(h),                  &
     &                          C_LOC(on_u), C_LOC(om_v),               &
     &                          C_LOC(ubar), C_LOC(vbar),               &
     &                          C_LOC(Drhs), C_LOC(DUon),               &
     &                          C_LOC(DVom), KK_TILE)
```

</details>

#### time_average 与 host 系数

<a id="src-f-call-tavg"></a>

**源码块 `F-CALL-TAVG`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **830–858**，共 **29 行**。

<details>
<summary>当前 Fortran 调用：time_average 与 host 系数（点击展开完整代码）</summary>

<!-- source-block:F-CALL-TAVG -->
```fortran
      IF (PREDICTOR_2D_STEP(ng)) THEN
        IF (FIRST_2D_STEP) THEN
          cff1=0.0_r8
          cff2=(-1.0_r8/12.0_r8)*weight(2,iif(ng)+1,ng)
        ELSE
          cff1=weight(1,iif(ng)-1,ng)
          cff2=(8.0_r8/12.0_r8)*weight(2,iif(ng)  ,ng)-                 &
     &         (1.0_r8/12.0_r8)*weight(2,iif(ng)+1,ng)
        END IF
      ELSE
        IF (FIRST_2D_STEP) THEN
          cff1=0.0_r8
          cff2=weight(2,iif(ng),ng)
        ELSE
          cff1=0.0_r8
          cff2=(5.0_r8/12.0_r8)*weight(2,iif(ng),ng)
        END IF
      END IF
      CALL k_step2d_time_average (ng, tile, krhs,                       &
     &                            MERGE(1,0,PREDICTOR_2D_STEP(ng)),     &
     &                            MERGE(1,0,FIRST_2D_STEP),             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
     &                            C_LOC(zeta), C_LOC(DUon),             &
     &                            C_LOC(DVom),                          &
     &                            C_LOC(Zt_avg1), C_LOC(DU_avg1),       &
     &                            C_LOC(DU_avg2), C_LOC(DV_avg1),       &
     &                            C_LOC(DV_avg2),                       &
     &                            cff1, cff2, KK_TILE)
```

</details>

#### zeta_step

<a id="src-f-call-zeta"></a>

**源码块 `F-CALL-ZETA`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **1024–1038**，共 **15 行**。

<details>
<summary>当前 Fortran 调用：zeta_step（点击展开完整代码）</summary>

<!-- source-block:F-CALL-ZETA -->
```fortran
      CALL k_step2d_zeta_step (ng, tile, krhs, kstp, knew,              &
     &                         MERGE(1,0,PREDICTOR_2D_STEP(ng)),         &
     &                         MERGE(1,0,FIRST_2D_STEP),                 &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         C_LOC(zeta), C_LOC(h),                   &
     &                         C_LOC(pm), C_LOC(pn),                    &
     &                         C_LOC(DUon), C_LOC(DVom),                &
     &                         C_LOC(gzeta), C_LOC(gzeta2),             &
     &                         C_LOC(gzetaSA), C_LOC(rhoA),             &
     &                         C_LOC(rhoS),                             &
     &                         C_LOC(rhs_zeta), C_LOC(zeta_new),        &
     &                         C_LOC(Dnew), C_LOC(zwrk),                &
     &                         C_LOC(rzeta), dtfast(ng), rho0,   &
     &                         KK_TILE)
```

</details>

#### rhs_fused

<a id="src-f-call-rhs"></a>

**源码块 `F-CALL-RHS`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **1244–1264**，共 **21 行**。

<details>
<summary>当前 Fortran 调用：rhs_fused（点击展开完整代码）</summary>

<!-- source-block:F-CALL-RHS -->
```fortran
      CALL k_step2d_rhs_fused (ng, tile, krhs,                          &
     &                         LBi, UBi, LBj, UBj,                      &
     &                         IminS, ImaxS, JminS, JmaxS,              &
     &                         C_LOC(h), C_LOC(on_u), C_LOC(om_v),      &
     &                         C_LOC(gzeta), C_LOC(gzeta2),             &
     &                         C_LOC(gzetaSA), C_LOC(rhoA),             &
     &                         C_LOC(zwrk),                             &
     &                         C_LOC(Drhs), C_LOC(fomn),                &
     &                         C_LOC(ubar), C_LOC(vbar),                &
     &                         C_LOC(DUon), C_LOC(DVom),                &
     &                         C_LOC(pm), C_LOC(pn),                    &
     &                         C_LOC(pmon_r), C_LOC(pnom_r),            &
     &                         C_LOC(pmon_p), C_LOC(pnom_p),            &
     &                         C_LOC(om_r), C_LOC(on_r),                &
     &                         C_LOC(om_p), C_LOC(on_p),                &
     &                         C_LOC(visc2_r), C_LOC(visc2_p),          &
     &                         C_LOC(rhs_ubar), C_LOC(rhs_vbar),        &
     &                         C_LOC(DiaU2rhs), C_LOC(DiaV2rhs),        &
     &                         NDM2d, M2pgrd, M2xadv, M2yadv,           &
     &                         M2hadv, M2fcor, M2hvis, M2xvis,          &
     &                         M2yvis, g, KK_EDGE, KK_TILE)
```

</details>

#### coupling

<a id="src-f-call-coup"></a>

**源码块 `F-CALL-COUP`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **2457–2468**，共 **12 行**。

<details>
<summary>当前 Fortran 调用：coupling（点击展开完整代码）</summary>

<!-- source-block:F-CALL-COUP -->
```fortran
      CALL k_step2d_coupling (ng, tile, istp, inew,                    &
     &                        iic(ng), ntfirst(ng),                     &
     &                        MERGE(1,0,FIRST_2D_STEP),                 &
     &                        MERGE(1,0,PREDICTOR_2D_STEP(ng)),         &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        C_LOC(rufrc), C_LOC(rvfrc),               &
     &                        C_LOC(ru), C_LOC(rv),                     &
     &                        C_LOC(rhs_ubar), C_LOC(rhs_vbar),         &
     &                        C_LOC(DiaU2rhs), C_LOC(DiaV2rhs),         &
     &                        C_LOC(DiaRUfrc), C_LOC(DiaRVfrc),         &
     &                        NDM2d, M2pgrd, M2sstr, M2bstr, KK_TILE)
```

</details>

#### momentum_step

<a id="src-f-call-momt"></a>

**源码块 `F-CALL-MOMT`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **2729–2740**，共 **12 行**。

<details>
<summary>当前 Fortran 调用：momentum_step（点击展开完整代码）</summary>

<!-- source-block:F-CALL-MOMT -->
```fortran
      CALL k_step2d_momentum_step (ng, tile, kstp, knew,                &
     &                             LBi, UBi, LBj, UBj,                  &
     &                             IminS, ImaxS, JminS, JmaxS,          &
     &                             C_LOC(zeta), C_LOC(h),               &
     &                             C_LOC(ubar), C_LOC(vbar),            &
     &                             C_LOC(rubar), C_LOC(rvbar),          &
     &                             C_LOC(pm), C_LOC(pn),                &
     &                             C_LOC(rhs_ubar), C_LOC(rhs_vbar),    &
     &                             C_LOC(Dstp), C_LOC(Dnew),            &
     &                             MERGE(1,0,PREDICTOR_2D_STEP(ng)),    &
     &                             MERGE(1,0,FIRST_2D_STEP),            &
     &                             dtfast(ng), KK_TILE)
```

</details>

#### diag_int、corrector guard 与 host 系数

<a id="src-f-call-diag"></a>

**源码块 `F-CALL-DIAG`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **2964–2984**，共 **21 行**。

<details>
<summary>当前 Fortran 调用：diag_int、corrector guard 与 host 系数（点击展开完整代码）</summary>

<!-- source-block:F-CALL-DIAG -->
```fortran
      IF (CORRECTOR_2D_STEP) THEN
        IF (FIRST_2D_STEP) THEN
          cff1=0.5_r8*dtfast(ng)
          cff2=0.0_r8
          cff3=0.0_r8
        ELSE
          cff1=0.5_r8*dtfast(ng)*5.0_r8/12.0_r8
          cff2=0.5_r8*dtfast(ng)*8.0_r8/12.0_r8
          cff3=0.5_r8*dtfast(ng)*1.0_r8/12.0_r8
        END IF
        CALL k_step2d_diag_int (ng, tile, kstp,                         &
     &                          MERGE(1,0,FIRST_2D_STEP),               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          C_LOC(pm), C_LOC(pn),                   &
     &                          C_LOC(DiaU2rhs), C_LOC(DiaV2rhs),       &
     &                          C_LOC(DiaRUbar), C_LOC(DiaRVbar),       &
     &                          C_LOC(DiaU2int), C_LOC(DiaV2int),       &
     &                          C_LOC(DiaU2wrk), C_LOC(DiaV2wrk),       &
     &                          NDM2d, cff1, cff2, cff3, fac, KK_TILE)
      END IF
```

</details>

#### store_rhs 与 predictor guard

<a id="src-f-call-stor"></a>

**源码块 `F-CALL-STOR`**：提交 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`，`ROMS/Nonlinear/step2d_LF_AM3.h`，原始行 **3120–3129**，共 **10 行**。

<details>
<summary>当前 Fortran 调用：store_rhs 与 predictor guard（点击展开完整代码）</summary>

<!-- source-block:F-CALL-STOR -->
```fortran
      IF (PREDICTOR_2D_STEP(ng)) THEN
        CALL k_step2d_store_rhs (ng, tile, krhs,                      &
     &                           LBi, UBi, LBj, UBj,                    &
     &                           IminS, ImaxS, JminS, JmaxS,            &
     &                           C_LOC(rubar), C_LOC(rvbar),            &
     &                           C_LOC(rhs_ubar), C_LOC(rhs_vbar),      &
     &                           C_LOC(DiaRUbar), C_LOC(DiaRVbar),      &
     &                           C_LOC(DiaU2rhs), C_LOC(DiaV2rhs),      &
     &                           NDM2d, KK_TILE)
      END IF
```

</details>

<a id="status-validation"></a>

## 12. 当前实现、未合入实验与历史验证的边界

### 12.1 当前 HEAD 已有的实现

本文逐字代码可直接确认：八入口已经有对应 Fortran 接入点和阶段开关；使用带符号 RangePolicy 按行并行、U/V 合并 dispatch、host 选择时间分支、借用原存储、部分 xi 差分/黏性通量滚动、RHS 中间表达式内联、南邻水深重算，以及 predictor 的 RHS 历史保存和 corrector 诊断下放。

“已有入口”不等于任何应用都启用，也不等于所有原始 CPP 组合都通过门禁。是否走到调用、物理开关是否生效，仍以具体构建和运行配置为准。本文没有重建模型来宣称一项特定配置当前已经执行了这些入口。

### 12.2 share 上的后续内容，不属于本文当前 kernel

本次只读核对了本地 `feat/kokkos.share` 的快照 `822e016032722c0ba8b739d9b757fc2e7b2fc294`。相关记录在该提交的 `docs/kokkos-experiments-2026-11-06.md`；当前工作分支同名文档没有后追加的这些章节，引用时必须带分支/提交，不能链接到当前文档不存在的段落。以下只是区分状态，不展开成迁移步骤。

| 内容 | share 快照中的状态 | 当前 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` 的状态 |
|---|---|---|
| 完整 xi 平流面通量 carry，提交 `0bb237a791bef29bc59685ce2411a034623e3841` | `k_step2d_rhs_fused` 的后续改动；记录了独立 bench 与短整模检查 | 未合入；本文仍分别计算 `ufx/ufxm`、`vfx/vfx1`，只已有部分差分/黏性 carry |
| `k_step2d_rhs_fused_rows` | 带宏的行块实验入口，未接入模型生产调用 | 当前文件没有这个函数 |
| `k_step2d_diag_int_blocked`，提交 `cf7bd663499d399785077076bc3810d05f29732f` | i-blocked 实验，显式在诊断项间复用度量；模型仍调用原 `diag_int`，模型级比较未运行 | 当前文件没有该入口，也没有这种显式跨 id 缓存 |
| tracer 行块及 TeamPolicy/流水变体 | 属于 step3d_t 等其他实验；并非八入口默认全改成团队执行 | 不可写成本文 step2d 的 TeamPolicy 或垂向分块 |

本次 `git merge-base --is-ancestor` 文本检查确认，完整 face carry 与 blocked diag 的上述提交都不是当前 HEAD 的祖先；当前完整 C++ 源码也没有后两种 step2d 实验入口。`docs/kokkos/migration-plan.md` 是现有草案，可供索引，但其旧分支 HEAD 不能取代本文的当前提交核对。

### 12.3 已否决、未采纳或已移除的实验

- **早期 step2d TeamPolicy**：当前 `ROMS/Cpp/tools/sw64/README.md:281–287` 记录小 tile 上速度比为 RangePolicy 的 0.58–0.86，实验与 bench 已移除。不能据此声称所有规模的 TeamPolicy 都必然差，但它不是当前实现。
- **U/V 黏性应力共享**：share 快照文档第 911–1015 行记录独立负实验，通用性能门禁 FAIL，canyon 形状约慢 4.5 倍；这不是当前的 `uRm/vPm` 局部滚动复用，更没有接入当前代码。
- **strip-major 调度**：同一 share 文档第 1102–1147 行明确标注未采纳。它的形状敏感性和组合代码生成代价不能被省去后只报最好值。
- **向量化开关探测**：share 文档第 1063–1101 行是实验记录，不是当前 `rhs_fused` 已被 SIMD 化的证据；也不是“ALU/带宽瓶颈已被硬件证明”。

这些历史结论只按现有记录陈述，本次没有重跑或复算相关性能数据。它们不能混入正文的“实际改了什么”。

### 12.4 历史数值与性能证据：引用，不冒充本次验证

| 现有记录 | 已有证据与适用范围 | 本文不能据此断言的内容 |
|---|---|---|
| [阶段消融与长积分记录](../kokkos-experiments-2026-11-06.md)，当前第 135–157、207–247 行 | UPWELLING 1440×704、12 ranks、两步无输出，S1 仅启用二维六组阶段，整步 16.946→5.274 s，3.21×；另一 41×80、两 ranks、四步全输出对照记录 S1 的 101/101 字段逐位相等，160×80 的 20 步 S1 记录十个 history 字段逐位相等 | 不是八个函数各自加速比，不是大网格计时本身做了同一数值比较，也不是当前格式化后 HEAD 的新运行 |
| 同一记录第 159–205 行 | 完整移植构建在 160×80 的 200/2000 步有与原始 Fortran 的小差异，近零 `salt_vdiff` 未通过归一化门禁；Serial/SW64 之间该记录逐位一致 | 不能无条件说整模与 Fortran 在所有步长、网格、物理上逐位相同；后端间一致也不等于对原始一致 |
| [清理后复验](../kokkos-mpi12-scale-benchmark.md)，第 295–333 行 | 记录了 2026-11-05 的预处理、重建、短步字段、性能与直接 kernel 测试；新旧保留二进制间比较、Serial/Fortran 精确比较与 SW64/Fortran 容差比较有明确区分 | 不能替代本次对当前 HEAD 重建；门禁阈值通过与逐位相等不是同一事实 |
| 同一规模记录的注意事项，第 335–346 行 | 明确大网格性能矩阵本身未比较场，MPI/halo/I/O 留在 Fortran 并计入整步时间 | 不能把整步加速全归因某一个 kernel，也不能用历史矩阵推出单函数 DRAM 流量 |
| share 快照文档第 796–910、1016–1062、1148–1180 行 | 记录后续完整 face carry 的 bench/短整模对照及 blocked diag 的 bench；blocked diag 模型级比较 NOT RUN | 不证明本文当前 kernel 已合入这些优化，不证明未做过计时的模型有同等整步加速 |

**本次验证范围**只有本地只读源码核对、机械提取和轻量 Markdown 文本检查。**编译、单元运行、整模数值运行、性能计时、远程任务均 NOT RUN。** 本次没有为格式化后的 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` 新增整模运行验证，也没有把历史文档的 PASS 当作此 HEAD 的重跑结果。当前代码注释中的历史验证用语应放回当时配置理解，不是本文重新作出的全配置保证。

<a id="source-manifest"></a>

## 13. 来源清单与文本完整性检查

### 13.1 核对方法与检查边界

- 文内每个带语言围栏的源码块都有唯一 `source-block` ID；清单列出原始闭区间范围及 payload 的 SHA256。
- 原始文件取自 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` 的 git blob，不从旧回答重造；当前文件在提取时逐字核对等于 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` 的 git blob。
- 八个 C++ 完整函数可从正文对应块提取，与当前源码完整定义核对；B.1 十块按原行序连接，应逐字等于全部 1513 行当前实现。
- 原始八个计算段各自与指定 git blob 行段逐字核对；附录 A 与原始 3058 行全文逐字核对。
- 文本检查验证 UTF-8、围栏/折叠标签配对、ID 唯一、目录锚点、无新添的省略占位、逐块字节与来源范围一致。源注释原有的 `...` 不作伪造省略判定。
- 上述检查不解析 Fortran/C++ 的编译语义，不验证物理边界数值，更不替代运行测试。

### 13.2 完整源码块清单

下表的提交、相对路径和行号共同唯一定位源码；相对路径均以本仓库根目录为基准。代码块正文自身也逐段给出完整提交和文件路径。

| 块 ID | 来源提交 | 文件 | 原始行范围 | 行数 | SHA256（围栏内原始字节） |
|---|---|---|---:|---:|---|
| [F-DEPTH](#src-f-depth) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 609–703 | 95 | `35a5edfb9dcaa21cc8fc73748d94f40dbc36887d28c761cf3a7558b4b35f82ae` |
| [C-DEPTH](#src-c-depth) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d.cpp` | 250–320 | 71 | `6e296a6b31650df217e477f0e83b4e533ad74cda8ab81b554f5f3970ad4bcd0a` |
| [F-TAVG](#src-f-tavg) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 738–810 | 73 | `babcd3e1c5dd2a72659578c4ef17d3c7a59236cbe9ba06e788557e4fd897c139` |
| [C-TAVG](#src-c-tavg) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d.cpp` | 321–409 | 89 | `73ebde3c65f013782d6a3adb5c864f8319d4774f37915bb8d636bfb6e4077710` |
| [F-ZETA](#src-f-zeta) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 885–1019 | 135 | `a6e4ebd40865cded2d94f86a1993253147be38455c10203cfb7ec3bb3d97fe64` |
| [C-ZETA](#src-c-zeta) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d.cpp` | 410–572 | 163 | `d02588253c8251ebcd0cb9c92f79536856b045499d9189ea58e40037d7fcda13` |
| [F-RHS](#src-f-rhs) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 1079–1652 | 574 | `620046f34ac521fe32a97aacdde1cec48031e226c1fffe74652a88dac1162edc` |
| [C-RHS](#src-c-rhs) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d.cpp` | 1152–1506 | 355 | `9fd0a6e6785d3852c9207184cf194cebbfb5a49da2ab9191fe4bf08628a60f87` |
| [F-COUP](#src-f-coup) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 2223–2460 | 238 | `78bbcc2e5a219cc9fed8b19820f56bc7ea9d65b2953766dc8a70151f16c1a67b` |
| [C-COUP](#src-c-coup) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d.cpp` | 573–807 | 235 | `707e4b337d801e46f3da2993e961de32f41f2702d05e282185960e5d7da65b92` |
| [F-MOMT](#src-f-momt) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 2487–2674 | 188 | `5c84d46cddc022e5a93c94cdf64e203f313567184876941651d1edb49fac33c3` |
| [C-MOMT](#src-c-momt) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d.cpp` | 808–976 | 169 | `e28b6b0742c599f1cb71e907d8c798598b74f47dafc1e14f08f3df4b161b2e59` |
| [F-DIAG](#src-f-diag) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 2700–2754 | 55 | `05ab1224ba745673f9191910a0e8b781e8698704f6b3b8a1481d0589b618d451` |
| [C-DIAG](#src-c-diag) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d.cpp` | 977–1081 | 105 | `fbe0e47f0acb4323de8bc26c1b5dcfe084713ad2965b4be5598c81768c3f17d4` |
| [F-STOR](#src-f-stor) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 2836–2865 | 30 | `1f4a2c052cf387b5cb909f02c370d697e471b1fc7c9d601d19c32cc9eb5cfb5e` |
| [C-STOR](#src-c-stor) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d.cpp` | 1082–1151 | 70 | `69b4237daedd4e028c7411c4f074cabc834aaba5a54ebaf95fdf1ec1936e01a9` |
| [F-ORIGINAL-ALL](#src-f-original-all) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 1–3058 | 3058 | `8afccd2ab45b8e5e808c8d9d9bdcb6d7d14b7d910ea6c91733659ef5bc52db8a` |
| [C-PREAMBLE](#src-c-preamble) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d.cpp` | 1–249 | 249 | `552632ba7f2858a41bd7ea078c25fcc9ace34a7693edcddd78fcf5ae030c3205` |
| [C-FOOTER](#src-c-footer) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d.cpp` | 1507–1513 | 7 | `eca8b1782c3d7da9f0ebd9119bb5911733e36aa5e595a173a7a82b6f2832cf9b` |
| [C-CONFIG-HPP](#src-c-config-hpp) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_config.hpp` | 1–346 | 346 | `59a3be05fa17305fe9778535ae99bcb1afd01ed709bc5766bd4600c1eaaa8c8e` |
| [C-CONFIG-CPP](#src-c-config-cpp) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_config.cpp` | 1–130 | 130 | `bae8e44d7e161134b1edb90bfd4f56a41d530ea07c0bb79e91bf1de927a1aa58` |
| [C-ENTRY-HPP](#src-c-entry-hpp) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d.hpp` | 1–169 | 169 | `c43099c2b93d712ddc984ecbb804011954159ada2146b468bb77dd423034a9a2` |
| [F-ABI-MODULE](#src-f-abi-module) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step2d_mod.F` | 1–325 | 325 | `1e8e457121e547317bc497447909d8f24fb769bab83e49037b5f844899f2ffd3` |
| [F-FIRST-MACRO](#src-f-first-macro) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Include/globaldefs.h` | 168–173 | 6 | `b240cba01ced05c862e2d8cbf944a53d7b7cbf9ddffd1c67448e96b7a2356b70` |
| [F-TILE-META](#src-f-tile-meta) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 630–671 | 42 | `06e310b875ce803073e0dc208075063b0f16faa188743e0e4687ccefd030c28c` |
| [F-CALL-DEPTH](#src-f-call-depth) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 682–689 | 8 | `9909cec84604fadd063db237bc9342cfa7ac56d6d1240768ed2724ac680200b9` |
| [F-CALL-TAVG](#src-f-call-tavg) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 830–858 | 29 | `f8392333bc66d76b7134a2165e6d09b6d5d28f13e381cd8614bca8e0a48723d5` |
| [F-CALL-ZETA](#src-f-call-zeta) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 1024–1038 | 15 | `7851108841c340503081ed49ab455d14edf92aed0bf127dee322ac06c65e530a` |
| [F-CALL-RHS](#src-f-call-rhs) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 1244–1264 | 21 | `359fc0228ce4462db764a5b61b7f09b7485a98d4b0d55919de39708a68c3de9b` |
| [F-CALL-COUP](#src-f-call-coup) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 2457–2468 | 12 | `2cd30dd3b58f2d37dec2a29de65248868e826ee4050eb1038e16710a53df16a8` |
| [F-CALL-MOMT](#src-f-call-momt) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 2729–2740 | 12 | `4b9f7087522403c512b058c5610bc697a02512986a8ff5d831bb5921964ecc21` |
| [F-CALL-DIAG](#src-f-call-diag) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 2964–2984 | 21 | `2b3ef1d99071640d18b1a5909f8affd6511b94311158584dd86ce604c6909d39` |
| [F-CALL-STOR](#src-f-call-stor) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step2d_LF_AM3.h` | 3120–3129 | 10 | `4979616cf746f79c7f1ec5bcf6c18155574211bb3d723900ca06a1618eae5f52` |

### 13.3 本次实际检查结果

<!-- verification-results -->
| 检查 | 实际结果 |
|---|---|
| UTF-8、原始换行与所有源码块提取 | **PASS**，33/33 块逐字等于指定 git blob 行段，清单行数和 SHA256 全部相符 |
| 八段原始 Fortran 完整计算段 | **PASS**，8/8；附录原始全文 3058/3058 行逐字一致 |
| 八个当前 C++ 完整函数 | **PASS**，8/8，完整签名与函数体逐字一致 |
| 当前 C++ 文件分块重组 | **PASS**，1513/1513 行完整覆盖、无缺口无重叠，按原序连接后字节相同 |
| launch 静态计数与调度类型 | **PASS**，18 个 `parallel_for` 静态点；实现中无 TeamPolicy/MDRangePolicy |
| Markdown 围栏、折叠块、唯一 ID 和内部链接 | **PASS**，33 对围栏、33 对 details；没有未登记代码块或无效内部锚点 |
| 省略占位检查 | **PASS**，没有新增数值代码省略；源码注释原有 `...` 完整保留 |
| 入口声明与 BIND(C) 接口覆盖 | **PASS**，8/8；`TileBounds` 21 字段与 `EdgeFlags` 10 字段的双侧顺序一致（文本检查，不是 ABI 编译测试） |
| 关闭 Kokkos 后的 Fortran 执行区 | **PASS**，注释/空白及 dummy 名字规范化后，2143/2143 行逐行相同 |
| 编译、单元运行、整模数值、性能、远程任务 | **NOT RUN**，本次没有执行 |
<!-- /verification-results -->

这些结果仅针对本 Markdown 的来源、覆盖和结构；不得读作对当前物理结果或性能的新验收。
