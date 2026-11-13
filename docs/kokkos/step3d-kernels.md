# step3d：三维预测—校正时间推进的完整 Fortran / Kokkos 对照

<a id="entry-compute-modes"></a>

## 入口计算模式速查

**pointwise（逐点）**指同一空间点上的代数运算或时间历史更新；**stencil（邻点模板）**指读取相邻空间点。**垂向递推 / 三对角追赶**指同一水柱沿 `k` 有顺序依赖；**列积分**指沿 `k` 求和得到每列量，不是全局 reduction。`+` 表示混合模式；独立列之间可并行，不代表同一列的各 `k` 层相互独立。

| 入口 | 主要计算模式 | 依赖范围 / 方向 | 简短说明 |
|---|---|---|---|
| [`k_pre_step3d_t`](#pre-t) | 水平 stencil + 垂向通量 stencil/递推 + 显式扩散 + pointwise | 水平读 `t(nstp)` 邻点；垂向读相邻层，SPLINES 分支沿整列前代/回代 | 预测 tracer、修正有效厚度并加显式垂向扩散；此处 SPLINES 递推用于平流界面重构，不是校正阶段的隐式扩散。 |
| [`k_pre_step3d_uv`](#pre-uv) | 水平系数 stencil + 垂向通量差分 + pointwise | U/V 读两侧 rho 柱几何/系数及本柱相邻层；沿 `k` 携带上一界面通量 | 显式垂向黏性通量差与 RHS 时间历史共同更新动量预测量，不作隐式求解。 |
| [`k_step3d_uv_solve`](#uv-solve) | 水平系数 stencil + 垂向三对角追赶 + 列积分/均值修正 | U/V 读邻 rho 柱系数；每列沿 `k` 前代/回代并定序求和 | 解隐式垂向黏性（普通或 spline 分支），再使内部列均值与 `avg1` 一致。 |
| [`k_step3d_uv_couple`](#uv-couple) | 水平厚度 stencil + 列积分 + pointwise 逐层修正 | U/V 面取两侧 rho 柱 `Hz`；沿 `k` 累加截面积与输运 | `avg1` 约束均值并按需修正边界速度；`avg2` 约束 `Huon/Hvom` 列输运，不是纯 pointwise。 |
| [`k_step3d_t_chunks`](#t-chunks) | 水平/垂向平流 stencil + 垂向三对角追赶 + pointwise | 平流读第 3 槽邻点/邻层；隐式扩散沿同一列 `k` 前代/回代 | 先校正平流并除以层厚，再解隐式 spline 扩散的界面梯度系统并更新浓度。 |

---

- **生成日期**：2026-11-14。
- **当前分支**：`feat/kokkos`。
- **当前源码提交（HEAD）**：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`。
- **统一原始 Fortran 基线**：`cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`。
- **Kokkos 子模块提交**：`6e40ad34a8968a44386e01a5a757e1e37bfcacea`，与当前主仓库 HEAD 的 gitlink 相同；依赖源码以此子模块提交为来源，不误标为主仓库 blob。
- **覆盖边界**：`k_pre_step3d_t`、`k_pre_step3d_uv`、`k_step3d_uv_solve`、`k_step3d_uv_couple`、`k_step3d_t_chunks` 五个入口及其全部当前 helper；不是全部三维物理的移植清单。
- **本次验证性质**：只做本地源码、历史与文档文本核验；没有新编译、模型积分、直接数值测试、性能测量或远程访问。

文件名中的 STEP3D 是三维推进阶段的合称，**原代码并不存在供本文统一对照的 `step3d.F`**。真实来源是 `pre_step3d.F`、`step3d_uv.F`、`step3d_t.F`。`rhs3d`、压力梯度、水平混合、`omega`、湍流闭合、生物和沉积物过程只在真实调用流程中定位，不冒充这五个函数。

沿用 [STEP2D 文档](step2d-kernels.md)的“**完整原始 Fortran 计算段 → 完整当前 C++ 函数 → 中文解释**”结构。原始 Fortran 的支持范围更宽，正文保留相关条件分支，**收录不等于该分支已由当前 C++ 支持**。两个当前 `.cpp` 以正文函数加附录 helper/前后文无缺行、无重复地分块收录；三个原始 Fortran 文件另以完整折叠附录保全声明、CPP、边界、源项和通信上下文。

所有源码块都是从指定 Git blob 按行机械提取的真实源码，不改空白、算式或英文注释。每块有唯一 source-block ID、完整提交、路径、闭区间行号、行数和原始 UTF-8 payload SHA256；哈希包含原有换行，不包含 Markdown 围栏。原源码注释若自带 `...`，保留它不是新增占位。解释、公式和性能表均为正文，不伪装成源码。

## 目录

0. [入口计算模式速查](#entry-compute-modes)
1. [来源核实与慢步真实调用顺序](#flow)
2. [输入输出、时间层、格点与单位](#variables)
3. [计算类型、工作项、内存与优化机制](#common)
4. [k_pre_step3d_t：tracer 预测与显式垂向扩散](#pre-t)
5. [k_pre_step3d_uv：U/V 动量预测](#pre-uv)
6. [k_step3d_uv_solve：隐式垂向黏性与内部均值替换](#uv-solve)
7. [k_step3d_uv_couple：二维三维耦合与质量通量](#uv-couple)
8. [k_step3d_t_chunks：tracer 校正与 spline 扩散](#t-chunks)
9. [编译宏、资格检查、回退与日志](#eligibility)
10. [数值保序、历史验证与性能证据的边界](#numerics)
11. [附录 A：三个完整原始 Fortran 文件](#appendix-original)
12. [附录 B：全部当前 C++ helper、前后文及重组地图](#appendix-cpp)
13. [附录 C：完整 C ABI、配置类型与当前调用端](#appendix-abi)
14. [附录 D：数组、HostWorkspace、运行时与严格构建来源](#appendix-runtime)
15. [来源清单与文本完整性核验](#source-manifest)

<a id="flow"></a>

## 1. 来源核实与慢步真实调用顺序

### 1.1 为什么这确实是未移植原版

本次对两个 C++ 实现执行 `git log --follow`，并用 `git show` 核实引入提交：

| 阶段 | 引入 / 替换提交 | 核实结果 |
|---|---|---|
| 原始 tracer Kokkos 移植 | `ce7ee9fb0e80b266440da71884c14684ec5026f0` | 首次增加 `kokkos_step3d_t.cpp` 并接入 `step3d_t.F`；不等于当前 chunks 版本 |
| PRE3D、STEP3DUV 与 tracer chunks | `c750614a4c2d7c8b7f5aeea712a7c3936d2b0dc5` | 首次增加 `kokkos_step3d.cpp`；tracer 改为 32 列 chunks |
| 清理旧实现及非 Kokkos 路径 | `ef8885a69307236d904e98ea736b93771975dc20` | 清除被替代实现、调整 Fortran 路径；不是另一个当前入口 |
| 共用 host scratch | `d93dd8007f6c2f61ac68c277d85029130ed9a3a8` | 复用 `HostWorkspace`，去除未用数组 extent |
| C++ 格式化 | `1c66e3a662b1ef6956d81670e35b5f06c4c41c24` | 当前源码行号按格式化后的 HEAD 记录 |

共同基线 `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` 是当前 HEAD 的祖先、step2d 首次移植前的提交，其 Git tree 尚无 `ROMS/Cpp`，三个原始文件均无 Kokkos hook。还逐字核实：基线的 `pre_step3d.F`、`step3d_uv.F` 分别等同于其真正引入提交的父提交 `2a9fd90db0aea1e5b7a11d34f6dde8a5371c52dc` 中对应文件；`step3d_t.F` 等同于首次 tracer 移植父提交 `0486ae62eab98c2aea3217c334c21d86aeacd3fc` 中对应文件。因此统一基线不是从旧回答回写的算式，也不混入后来移植后的代码。三个文件分别为 **1182、1846、1976 行**。

### 1.2 一个 DT 慢步中的相对位置

以下按当前 `main3d.F` 与它调用的 `rhs3d.F` 逐层展开，不按 C++ 函数定义顺序猜测：

1. `main3d.F:515–541`：垂向/水平混合系数更新，`omega`、`wvelocity(nstp)`；随后还可能有 `set_zeta`、诊断、平均和输出。
2. `main3d.F:625–640` 调用 **`rhs3d`**。其内部 `rhs3d.F:73–195` 的顺序是 **`pre_step3d` → `prsgrd` → tracer 水平混合（条件编译）→ `rhs3d_tile` 动量 RHS → U/V 水平混合（条件编译）**。
3. `pre_step3d` 内，合格 tile 先 `k_pre_step3d_t`（未定义 `TS_FIXED` 时），再 `k_pre_step3d_uv`；tracer 第 3 槽边界与交换仍在 Fortran。预测阶段在本轮压力梯度/RHS 完成之前，动量预测需要的 RHS 历史不是“刚算完的本轮全部 RHS”。
4. 返回 `main3d` 后进行正压 **step2d 快步循环**。文件保留 FB/AB3/AM4、FB/LF/AM3 等不同 CPP 路径；当前 STEP3DUV 手工实现拒绝其中两个 FB 组合。常规 LF/AM3 路径 `main3d.F:810–906` 包含 predictor/corrector 及最后仅 predictor 的辅助步。
5. 快平均之后重算 `set_depth`（`main3d.F:953–967` 的对应分支），再 `step3d_uv`（第 990 行）。该例程内部依次 **solve → `u3dbc_tile` → `v3dbc_tile` → 动量点源 → couple → 周期及 MPI exchange → `uv_C2A_grid`**。
6. `main3d.F:1014–1034` 再调用 `omega`，随后条件调用湍流校正、生物、沉积物等；第 1045 行才调用 `step3d_t`（非 `TS_FIXED`）。这些过程可以先向 `t(nnew)` 的厚度加权中间量加入贡献。
7. `step3d_t` 合格路径先保留 HSIMT 前置 exchange，再执行 `k_step3d_t_chunks`，之后回到共同 Fortran epilogue 做年龄源、tracer 侧边界、nudging、诊断 rate、周期/MPI exchange 等。

**solve 与 couple 必须分开是因为真实流程中间有边界和点源写入。原始与当前源码都没有在这两段之间先做 halo exchange；exchange 在 couple 之后。** 把两入口直接融合会跨越这些仍由 Fortran 承担的状态变更。

### 1.3 快慢步与计时口径

- `dt` / 输入 `DT` 是完整三维慢步，单位 s；整模 `s/step` 指一次完整 DT 时间推进，而不是一次 `dtfast` 或一个 kernel。
- `dtfast = dt / REAL(ndtfast,r8)`，见当前 `inp_par.F:629`；`NDTFAST` 是快慢步分裂比。
- `nfast` 是滤波平均需要的实际快步数，由 `set_weights.F` 的滤波支持域及权重调整决定；不能硬写为 `nfast == ndtfast`。定义见 `mod_scalars.F:204–216`。
- 常规路径最后的 `nfast+1` 辅助步用于完成滤波平均，不是额外推进一个三维 DT；五个三维入口不因此每个快步都执行一次。

### 1.4 五入口行段总表

原始行号均属于共同基线；当前 C++、调用行号均属于 HEAD。完整源段见后文，范围含首尾。

| 入口 | 原始 Fortran 计算段 | 当前完整函数 | 当前调用点 | 工作项 |
|---|---|---|---|---|
| `k_pre_step3d_t` | `pre_step3d.F:318–933` | `kokkos_step3d.cpp:404–552` | `pre_step3d.F:434–443` | 一个 `(j,itrc)` |
| `k_pre_step3d_uv` | `pre_step3d.F:934–1144` | `kokkos_step3d.cpp:554–614` | `pre_step3d.F:445–455` | 一行 j，先 U 后 V |
| `k_step3d_uv_solve` | `step3d_uv.F:325–1260` | `kokkos_step3d.cpp:1014–1075` | `step3d_uv.F:450–462` | 一行 j，先 U 后 V |
| `k_step3d_uv_couple` | `step3d_uv.F:1305–1756` | `kokkos_step3d.cpp:1077–1140` | `step3d_uv.F:1453–1467` | 扩展行 j，先 U 后 V |
| `k_step3d_t_chunks` | `step3d_t.F:341–1799`，含散布的平流、单位转换与扩散段 | `kokkos_step3d_t.cpp:149–239` | `step3d_t.F:490–494` | 一个 `(j,itrc)` |

两个当前 `.cpp` 分别为 **1144、241 行**。完整实现包括 `TracerFields`、`MomentumFields`、`UvDir/UvFields`、`Fields`、horizontal/vertical/difference/curvature、row/both/solve_row/couple_row 等，不能把这些入口外壳当成全部算法。

### 1.5 实际驱动源码

以下只是驱动中相关连续区段；外围是 `NONLINEAR && SOLVE3D` 的 main3d 慢步调度与网格/tile 循环，不是另一个三维 kernel。

<a id="src-flow-main3d"></a>

**source-block ID：`flow-main3d`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Nonlinear/main3d.F`；闭区间：**515–1062**；行数：**548**；UTF-8 payload SHA256：`d2f5faab528133399e4ebd2306d0600092db124ff52c208f76ab24cfb98580e1`。

<details>
<summary>main3d：混合、RHS、快步、UV 与 T 的真实相对顺序（完整原文，可展开）</summary>

<!-- source-block id=flow-main3d commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Nonlinear/main3d.F lines=515-1062 count=548 sha256=d2f5faab528133399e4ebd2306d0600092db124ff52c208f76ab24cfb98580e1 -->
```fortran
!-----------------------------------------------------------------------
!  Compute time-dependent vertical/horizontal mixing coefficients for
!  momentum and tracers. Compute S-coordinate vertical velocity,
!  diagnostically from horizontal mass divergence.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=last_tile(ng),first_tile(ng),-1
# if defined ANA_VMIX
                CALL ana_vmix (ng, tile, iNLM)
# elif defined LMD_MIXING
                CALL lmd_vmix (ng, tile)
# elif defined BVF_MIXING
                CALL bvf_mix (ng, tile)
# endif
# if defined DIFF_3DCOEF || defined VISC_3DCOEF
                CALL hmixing (ng, tile)
# endif
                CALL omega (ng, tile, iNLM)
                CALL wvelocity (ng, tile, nstp(ng))
# ifdef WEC
                CALL wec_wvelocity (ng, tile, nstp(ng))
# endif
              END DO
!$OMP BARRIER
            END DO

# if !(defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3) || \
       defined DIAGNOSTICS       || defined AVERAGES
!
!-----------------------------------------------------------------------
!  Set free-surface to it time-averaged value.  If applicable,
!  accumulate time-averaged output data which needs a irreversible
!  loop in shared-memory jobs.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=first_tile(ng),last_tile(ng),+1     ! irreversible
#  if !(defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3)
                CALL set_zeta (ng, tile)
#  endif
#  ifdef DIAGNOSTICS
                CALL set_diags (ng, tile)
#  endif
#  ifdef AVERAGES
                CALL set_avg (ng, tile)
#  endif
              END DO
!$OMP BARRIER
            END DO
# endif

# ifdef NESTING
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for 3D kernel free-surface.
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (ANY(CompositeGrid(:,ng))) THEN
                CALL nesting (ng, iNLM, nzeta)
              END IF
            END DO
# endif
!
!-----------------------------------------------------------------------
!  If appropriate, write out fields into output NetCDF files.  Notice
!  that IO data is written in delayed and serial mode.  Exit if last
!  time step.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
!$OMP MASTER
              CALL output (ng)
!$OMP END MASTER
!$OMP BARRIER
              IF ((FoundError(exit_flag, NoError, __LINE__, MyFile)).or.&
     &            ((iic(ng).eq.(ntend(ng)+1)).and.(ng.eq.Ngrids))) THEN
                RETURN
              END IF
            END DO

# ifdef NESTING
!
!-----------------------------------------------------------------------
!  If refinement grid, interpolate (space, time) state variables
!  contact points from donor grid extracted data.
#  ifdef NESTING_DEBUG
!
!  Also, fill BRY_CONTACT(:,:)%Mflux to check for mass conservation
!  between coarse and fine grids.  This is only done for diagnostic
!  purposes. Also, debugging is possible with very verbose output
!  to fort.300 is allowed by activating uppercase(nesting_debug).
#  endif
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (RefinedGrid(ng).and.(RefineScale(ng).gt.0)) THEN
                CALL nesting (ng, iNLM, nputD)
#  ifdef NESTING_DEBUG
                CALL nesting (ng, iNLM, nmflx)
#  endif
              END IF
            END DO
# endif
!
!-----------------------------------------------------------------------
!  Compute right-hand-side terms for 3D equations.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=last_tile(ng),first_tile(ng),-1
                CALL rhs3d (ng, tile)
# ifdef MY25_MIXING
                CALL my25_prestep (ng, tile)
# elif defined GLS_MIXING
                CALL gls_prestep (ng, tile)
# endif
              END DO
!$OMP BARRIER
            END DO

# ifdef NESTING
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for right-hand-side terms
!  (tracers).
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (ANY(CompositeGrid(:,ng))) THEN
                CALL nesting (ng, iNLM, nrhst)
              END IF
            END DO
# endif

# ifdef STEP2D_FB_AB3_AM4
!
!-----------------------------------------------------------------------
!  Solve nonlinear vertically integrated primitive equations for
!  free-surface and momentum components using a generalized Forward-
!  Backward, 3rd-order Adams-Bashforth / 4th-order Adams-Moulton
!  (FB AB3-AM4) time stepping scheme (Shchepetkin and McWilliams,
!  2009).
!-----------------------------------------------------------------------
!
            LOOP_2D : DO my_iif=1,MAXVAL(nfast)

              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (my_iif.le.nfast(ng)) THEN
                  iif(ng)=my_iif
                  kstp(ng)=knew(ng)
                  knew(ng)=kstp(ng)+1
                  IF (knew(ng).gt.4) knew(ng)=1

                  IF (MOD(knew(ng),2).eq.0) THEN            ! zig-zag
                    DO tile=first_tile(ng),last_tile(ng),+1 ! processing
                      CALL step2d (ng, tile)                ! sequence
                    END DO
!$OMP BARRIER
                  ELSE
                    DO tile=last_tile(ng),first_tile(ng),-1
                      CALL step2d (ng, tile)
                    END DO
!$OMP BARRIER
                  END IF
                END IF
              END DO

#  ifdef NESTING
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for the state variables
!  associated with the 2D engine CORRECTOR STEP section (KNEW INDEX).
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (ANY(CompositeGrid(:,ng))) THEN
                  CALL nesting (ng, iNLM, n2dCS)
                END IF
              END DO
#  endif
            END DO LOOP_2D

# else

#  ifdef STEP2D_FB_LF_AM3
!
!-----------------------------------------------------------------------
!  Solve the vertically integrated primitive equations for the
!  free-surface and barotropic momentum components using a predictor-
!  corrector LeapFrog / 3rd-order Adams-Moulton with a Forward-Backward
!  feeback (FB LF-AM3) stepping scheme (Shchepetkin and McWilliams,
!  2009).
!-----------------------------------------------------------------------
!
            LOOP_2D : DO my_iif=1,MAXVAL(nfast)
!
!  Predictor LF substep with FB-feedback.
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (my_iif.le.nfast(ng)) THEN
                  iif(ng)=my_iif
                  kstp(ng)=next_kstp(ng)
                  knew(ng)=3

                  DO tile=last_tile(ng),first_tile(ng),-1
                    CALL step2d (ng, tile)
                  END DO
!$OMP BARRIER
                END IF
              END DO

#   ifdef NESTING
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for the state variables
!  associated with the 2D engine PREDICTOR STEP section.
#    ifdef NESTING_DEBUG
!  If refinement, check mass flux conservation between coarse and
!  fine grids during debugging.
!  Warning: very verbose output to fort.300 ascii file to check
!           mass flux conservation.
#    endif
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (ANY(CompositeGrid(:,ng))) THEN
                  CALL nesting (ng, iNLM, n2dPS)
                END IF
#    ifdef NESTING_DEBUG
                IF (RefinedGrid(ng).and.(RefineScale(ng).gt.0)) THEN
                  CALL nesting (ng, iNLM, nmflx)
                END IF
#    endif
              END DO
#   endif
!
!  Corrector AM3 substep with FB-feedback.
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (my_iif.le.nfast(ng)) THEN
                  knew(ng)=3-kstp(ng)
                  next_kstp(ng)=knew(ng)

                  DO tile=first_tile(ng),last_tile(ng),+1
                    CALL step2d (ng, tile)
                  END DO
!$OMP BARRIER
                END IF
              END DO

#   ifdef NESTING
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for the state variables
!  associated with the 2D engine CORRECTOR STEP section (KNEW INDEX).
#    ifdef NESTING_DEBUG
!  If refinement, check mass flux conservation between coarse and
!  fine grids during debugging.
!  Warning: very verbose output to fort.300 ascii file to check
!           mass flux conservation.
#    endif
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (ANY(CompositeGrid(:,ng))) THEN
                  CALL nesting (ng, iNLM, n2dCS)
                END IF
#    ifdef NESTING_DEBUG
                IF (RefinedGrid(ng).and.(RefineScale(ng).gt.0)) THEN
                  CALL nesting (ng, iNLM, nmflx)
                END IF
#    endif
              END DO
#   endif
            END DO LOOP_2D

#  else
!
!-----------------------------------------------------------------------
!  Solve the vertically integrated primitive equations for the
!  free-surface and barotropic momentum components using a predictor-
!  corrector LeapFrog with 3rd-order Adams-Moulton (LF-AM3) time
!  stepping scheme.
!-----------------------------------------------------------------------
!
            LOOP_2D : DO my_iif=1,MAXVAL(nfast)+1
!
!  Set time indices for predictor step. The PREDICTOR_2D_STEP switch
!  it is assumed to be false before the first time-step.
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                next_indx1=3-indx1(ng)
                IF (.not.PREDICTOR_2D_STEP(ng).and.                     &
     &              my_iif.le.(nfast(ng)+1)) THEN
                  PREDICTOR_2D_STEP(ng)=.TRUE.
                  iif(ng)=my_iif
                  IF (FIRST_2D_STEP) THEN
                    kstp(ng)=indx1(ng)
                  ELSE
                    kstp(ng)=3-indx1(ng)
                  END IF
                  knew(ng)=3
                  krhs(ng)=indx1(ng)
                END IF
!
!  Predictor step - Advance barotropic equations using 2D time-step
!  ==============   predictor scheme.  No actual time-stepping is
!  performed during the auxiliary (nfast+1) time-step. It is needed
!  to finalize the fast-time averaging of 2D fields, if any, and
!  compute the new time-evolving depths.
!
                IF (my_iif.le.(nfast(ng)+1)) THEN
                  DO tile=last_tile(ng),first_tile(ng),-1
                    CALL step2d (ng, tile)
                  END DO
!$OMP BARRIER
                END IF
              END DO

#   ifdef NESTING
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for the state variables
!  associated with the 2D engine PREDICTOR STEP section.
#    ifdef NESTING_DEBUG
!  If refinement, check mass flux conservation between coarse and
!  fine grids during debugging.
!  Warning: very verbose output to fort.300 ascii file to check
!           mass flux conservation.
#    endif
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (ANY(CompositeGrid(:,ng))) THEN
                  CALL nesting (ng, iNLM, n2dPS)
                END IF
#    ifdef NESTING_DEBUG
                IF (RefinedGrid(ng).and.(RefineScale(ng).gt.0)) THEN
                  CALL nesting (ng, iNLM, nmflx)
                END IF
#    endif
              END DO
#   endif
!
!  Set time indices for corrector step.
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (PREDICTOR_2D_STEP(ng)) THEN
                  PREDICTOR_2D_STEP(ng)=.FALSE.
                  knew(ng)=next_indx1
                  kstp(ng)=3-knew(ng)
                  krhs(ng)=3
                  IF (iif(ng).lt.(nfast(ng)+1)) indx1(ng)=next_indx1
                END IF
!
!  Corrector step - Apply 2D time-step corrector scheme.  Notice that
!  ==============   there is not need for a corrector step during the
!  auxiliary (nfast+1) time-step.
!
                IF (iif(ng).lt.(nfast(ng)+1)) THEN
                  DO tile=first_tile(ng),last_tile(ng),+1
                    CALL step2d (ng, tile)
                  END DO
!$OMP BARRIER
                END IF
              END DO

#   ifdef NESTING
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for the state variables
!  associated with the 2D engine CORRECTOR STEP section (KNEW INDEX).
#    ifdef NESTING_DEBUG
!  If refinement, check mass flux conservation between coarse and
!  fine grids during debugging.
!  Warning: very verbose output to fort.300 ascii file to check
!           mass flux conservation.
#    endif
!
              DO ig=1,GridsInLayer(nl)
                ng=GridNumber(ig,nl)
                IF (ANY(CompositeGrid(:,ng))) THEN
                  CALL nesting (ng, iNLM, n2dCS)
                END IF
#    ifdef NESTING_DEBUG
                IF (RefinedGrid(ng).and.(RefineScale(ng).gt.0)) THEN
                  CALL nesting (ng, iNLM, nmflx)
                END IF
#    endif
              END DO
#   endif
            END DO LOOP_2D
#  endif

# endif

# ifdef NESTING
#  if defined MASKING && defined WET_DRY
!
!-----------------------------------------------------------------------
!  If nesting and wetting and drying, scale horizontal interpolation
!  weights to account for land/sea masking in contact areas. This needs
!  to be done at very time-step since the Land/Sea masking is time
!  dependent.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              CALL nesting (ng, iNLM, nmask)
            END DO
#  endif
!
!-----------------------------------------------------------------------
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for the time-averaged
!  momentum fluxes (DU_avg1, DV_avg1) and free-surface (Zt_avg).
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (ANY(CompositeGrid(:,ng))) THEN
                CALL nesting (ng, iNLM, n2dfx)
              END IF
            END DO
# endif

# if !(defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3)
!
!-----------------------------------------------------------------------
!  Recompute depths and thicknesses using the new time filtered
!  free-surface.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=last_tile(ng),first_tile(ng),-1
                CALL set_depth (ng, tile, iNLM)
              END DO
!$OMP BARRIER
            END DO
# endif

# ifdef NESTING
!
!  If nesting, determine vertical indices and vertical interpolation
!  weights in the contact zone using new depth arrays.
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
!             CALL nesting (ng, iNLM, nzwgt)
            END DO
# endif
!
!-----------------------------------------------------------------------
!  Time-step 3D momentum equations.
!-----------------------------------------------------------------------
!
!  Time-step 3D momentum equations and couple with vertically
!  integrated equations.
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=last_tile(ng),first_tile(ng),-1
                CALL step3d_uv (ng, tile)
              END DO
!$OMP BARRIER
            END DO

# ifdef NESTING
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for 3D momentum (u,v),
!  adjusted 2D momentum (ubar,vbar), and fluxes (Huon, Hvom).
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (ANY(CompositeGrid(:,ng))) THEN
                CALL nesting (ng, iNLM, n3duv)
              END IF
            END DO
# endif
!
!-----------------------------------------------------------------------
!  Time-step vertical mixing turbulent equations and passive tracer
!  source and sink terms, if applicable.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=first_tile(ng),last_tile(ng),+1
                CALL omega (ng, tile, iNLM)
# ifdef MY25_MIXING
                CALL my25_corstep (ng, tile)
# elif defined GLS_MIXING
                CALL gls_corstep (ng, tile)
# endif
# if defined WEC_VF && defined WAVE_MIXING
                CALL wec_wave_mix (ng, tile)
# endif
# ifdef BIOLOGY
                CALL biology (ng, tile)
# endif
# ifdef SEDIMENT
                CALL sediment (ng, tile)
# endif
              END DO
!$OMP BARRIER
            END DO

# ifndef TS_FIXED
!
!-----------------------------------------------------------------------
!  Time-step tracer equations.
!-----------------------------------------------------------------------
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              DO tile=last_tile(ng),first_tile(ng),-1
                CALL step3d_t (ng, tile)
              END DO
!$OMP BARRIER
            END DO

#  ifdef NESTING
!
!  If composite or mosaic grids, process additional points in the
!  contact zone between connected grids for Tracer Variables.
!
            DO ig=1,GridsInLayer(nl)
              ng=GridNumber(ig,nl)
              IF (ANY(CompositeGrid(:,ng))) THEN
                CALL nesting (ng, iNLM, n3dTV)
              END IF
            END DO
#  endif
# endif
```

</details>

<a id="src-flow-rhs3d"></a>

**source-block ID：`flow-rhs3d`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Nonlinear/rhs3d.F`；闭区间：**73–195**；行数：**123**；UTF-8 payload SHA256：`243a53ae736de7c57e2a3d9451d4ccfc69bcb3a6fbf702a2afd62b59a3437fa4`。

<details>
<summary>rhs3d：预测、压力梯度、水平混合及 RHS 调用（完整原文，可展开）</summary>

<!-- source-block id=flow-rhs3d commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Nonlinear/rhs3d.F lines=73-195 count=123 sha256=243a53ae736de7c57e2a3d9451d4ccfc69bcb3a6fbf702a2afd62b59a3437fa4 -->
```fortran
# include "tile.h"
!
!-----------------------------------------------------------------------
!  Initialize computations for new time step of the 3D primitive
!  variables.
!-----------------------------------------------------------------------
!
      CALL pre_step3d (ng, tile)
!
!-----------------------------------------------------------------------
!  Compute baroclinic pressure gradient.
!-----------------------------------------------------------------------
!
      CALL prsgrd (ng, tile)

# ifdef WEC_VF
!
!-----------------------------------------------------------------------
!  Compute Waves Effect on Currents.
!-----------------------------------------------------------------------
!
      CALL wec_vf (ng, tile)
#  if defined BOTTOM_STREAMING || defined SURFACE_STREAMING
      CALL wec_streaming (ng, tile)
#  endif
# endif

# ifndef TS_FIXED
#  ifdef TS_DIF2
!
!-----------------------------------------------------------------------
!  Compute horizontal harmonic mixing of tracer type variables.
!-----------------------------------------------------------------------
!
      CALL t3dmix2 (ng, tile)
#  endif
#  ifdef TS_DIF4
!
!-----------------------------------------------------------------------
!  Compute horizontal biharmonic mixing of tracer type variables.
!-----------------------------------------------------------------------
!
      CALL t3dmix4 (ng, tile)
#  endif
# endif
!
!-----------------------------------------------------------------------
!  Compute right-hand-side terms for the 3D momentum equations.
!-----------------------------------------------------------------------
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 21, __LINE__, MyFile)
# endif
      CALL rhs3d_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj,                              &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 nrhs(ng),                                        &
     &                 GRID(ng) % Hz,                                   &
     &                 GRID(ng) % Huon,                                 &
     &                 GRID(ng) % Hvom,                                 &
# if defined CURVGRID && defined UV_ADV
     &                 GRID(ng) % dmde,                                 &
     &                 GRID(ng) % dndx,                                 &
# endif
     &                 GRID(ng) % fomn,                                 &
     &                 GRID(ng) % om_u,                                 &
     &                 GRID(ng) % om_v,                                 &
     &                 GRID(ng) % on_u,                                 &
     &                 GRID(ng) % on_v,                                 &
     &                 GRID(ng) % pm,                                   &
     &                 GRID(ng) % pn,                                   &
# ifdef WET_DRY
     &                 GRID(ng)%umask_wet,                              &
     &                 GRID(ng)%vmask_wet,                              &
# endif
     &                 FORCES(ng) % bustr,                              &
     &                 FORCES(ng) % bvstr,                              &
     &                 FORCES(ng) % sustr,                              &
     &                 FORCES(ng) % svstr,                              &
     &                 OCEAN(ng) % u,                                   &
     &                 OCEAN(ng) % v,                                   &
     &                 OCEAN(ng) % W,                                   &
# ifdef WEC
     &                 OCEAN(ng) % u_stokes,                            &
     &                 OCEAN(ng) % v_stokes,                            &
     &                 OCEAN(ng) % W_stokes,                            &
     &                 MIXING(ng) % rustr3d,                            &
     &                 MIXING(ng) % rvstr3d,                            &
# endif
     &                 COUPLING(ng) % rufrc,                            &
     &                 COUPLING(ng) % rvfrc,                            &
# ifdef DIAGNOSTICS_UV
     &                 DIAGS(ng) % DiaRUfrc,                            &
     &                 DIAGS(ng) % DiaRVfrc,                            &
     &                 DIAGS(ng) % DiaRU,                               &
     &                 DIAGS(ng) % DiaRV,                               &
# endif
     &                 OCEAN(ng) % ru,                                  &
     &                 OCEAN(ng) % rv)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 21, __LINE__, MyFile)
# endif
# ifdef UV_VIS2
!
!-----------------------------------------------------------------------
!  Compute horizontal, harmonic mixing of momentum.
!-----------------------------------------------------------------------
!
      CALL uv3dmix2 (ng, tile)
# endif
# ifdef UV_VIS4
!
!-----------------------------------------------------------------------
!  Compute horizontal, biharmonic mixing of momentum.
!-----------------------------------------------------------------------
!
      CALL uv3dmix4 (ng, tile)
# endif
!
      RETURN
      END SUBROUTINE rhs3d
!
!***********************************************************************
```

</details>

<a id="src-flow-dtfast"></a>

**source-block ID：`flow-dtfast`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Utility/inp_par.F`；闭区间：**621–632**；行数：**12**；UTF-8 payload SHA256：`2a989e2d2ff600c3e6f71d9935b1b42bbcc63bf83c4b5b865df57b414a0a556b`。

<details>
<summary>ROMS/Utility/inp_par.F，621–632（完整原文，可展开）</summary>

<!-- source-block id=flow-dtfast commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Utility/inp_par.F lines=621-632 count=12 sha256=2a989e2d2ff600c3e6f71d9935b1b42bbcc63bf83c4b5b865df57b414a0a556b -->
```fortran
      IF (FoundError(exit_flag, NoError, __LINE__, MyFile)) RETURN
!
!-----------------------------------------------------------------------
!  Compute various constants.
!-----------------------------------------------------------------------
!
      gorho0=g/rho0
      DO ng=1,Ngrids
        dtfast(ng)=dt(ng)/REAL(ndtfast(ng),r8)
!
!  Take the square root of the biharmonic coefficients so it can
!  be applied to each harmonic operator.
```

</details>

<a id="src-flow-fast-definitions"></a>

**source-block ID：`flow-fast-definitions`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Modules/mod_scalars.F`；闭区间：**204–216**；行数：**13**；UTF-8 payload SHA256：`e4856eb36eea48e5095ea33b38d82dc3a28f8df49396926a8e1d016f79bad75d`。

<details>
<summary>ROMS/Modules/mod_scalars.F，204–216（完整原文，可展开）</summary>

<!-- source-block id=flow-fast-definitions commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Modules/mod_scalars.F lines=204-216 count=13 sha256=e4856eb36eea48e5095ea33b38d82dc3a28f8df49396926a8e1d016f79bad75d -->
```fortran
!    indx1         2D timestep rolling counter.
!    iic           Timestep counter for 3D primitive equations.
!    iif           Timestep counter for 2D primitive equations.
#ifdef JEDI
!    jic           Timestep counter for roms-jedi interface.
#endif
!    ndtfast       Number of barotropic timesteps between each
!                    baroclinic timestep.
!    nfast         Number of barotropic timesteps needed to compute
!                    time-averaged barotropic variables centered at
!                    time level n+1.
!    dt            Size baroclinic timestep (s).
!    dtfast        Size barotropic timestep (s).
```

</details>

<a id="src-flow-filter-weights"></a>

**source-block ID：`flow-filter-weights`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Utility/set_weights.F`；闭区间：**35–196**；行数：**162**；UTF-8 payload SHA256：`8d4598d417c2575672dfc94c33f712fc0f492795b1f7618caa6ad1ba98d77c0d`。

<details>
<summary>ROMS/Utility/set_weights.F，35–196（完整原文，可展开）</summary>

<!-- source-block id=flow-filter-weights commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Utility/set_weights.F lines=35-196 count=162 sha256=8d4598d417c2575672dfc94c33f712fc0f492795b1f7618caa6ad1ba98d77c0d -->
```fortran
      real(dp) :: arg
# endif
      real(dp) :: cff1, cff2

      real(r16) :: wsum, shift, cff
!
!=======================================================================
!  Compute time-averaging filter for barotropic fields.
!=======================================================================
!
!  Initialize both sets of weights to zero.
!
      nfast(ng)=0
      DO i=1,2*ndtfast(ng)
        weight(1,i,ng)=0.0_dp
        weight(2,i,ng)=0.0_dp
      END DO

# ifdef POWER_LAW
!
!-----------------------------------------------------------------------
!  Power-law shape filters.
!-----------------------------------------------------------------------
!
!  The power-law shape filters are given by:
!
!     F(xi)=xi^Falpha*(1-xi^Fbeta)-Fgamma*xi
!
!  where xi=scale*i/ndtfast; and scale, Falpha, Fbeta, Fgamma, and
!  normalization are chosen to yield the correct zeroth-order
!  (normalization), first-order (consistency), and second-order moments,
!  resulting in overall second-order temporal accuracy for time-averaged
!  barotropic motions resolved by baroclinic time step. There parameters
!  are set in "mod_scalars".
!
      scale=(Falpha+1.0_dp)*(Falpha+Fbeta+1.0_dp)/                      &
     &      ((Falpha+2.0_dp)*(Falpha+Fbeta+2.0_dp)*REAL(ndtfast(ng),dp))
!
!  Find center of gravity of the primary weighting shape function and
!  iteratively adjust "scale" to place the  centroid exactly at
!  "ndtfast".
!
      gamma=Fgamma*MAX(0.0_dp, 1.0_dp-10.0_dp/REAL(ndtfast(ng),dp))
      DO iter=1,16
        nfast(ng)=0
        DO i=1,2*ndtfast(ng)
          cff=scale*REAL(i,dp)
          weight(1,i,ng)=cff**Falpha-cff**(Falpha+Fbeta)-gamma*cff
          IF (weight(1,i,ng).gt.0.0_dp) nfast(ng)=i
          IF ((nfast(ng).gt.0).and.(weight(1,i,ng).lt.0.0_dp)) THEN
            weight(1,i,ng)=0.0_dp
          END IF
        END DO
        wsum=0.0_r16
        shift=0.0_r16
        DO i=1,nfast(ng)
          wsum=wsum+weight(1,i,ng)
          shift=shift+weight(1,i,ng)*REAL(i,dp)
        END DO
        scale=scale*shift/(wsum*REAL(ndtfast(ng),dp))
      END DO

# elif defined COSINE2
!
!-----------------------------------------------------------------------
!  Cosine-squared shaped filter.
!-----------------------------------------------------------------------
!
      cff=pi/REAL(ndtfast(ng),dp)
      DO i=1,2*ndtfast(ng)
        arg=cff*float(i-ndtfast(ng))
        IF ((2.0_dp*ABS(arg)).lt.pi) THEN
!!        weight(1,i,ng)=1.0_dp                     ! flat
!!        weight(1,i,ng)=(COS(arg))**2
          weight(1,i,ng)=0.0882_dp+(COS(arg))**2    ! hamming window
          nfast(ng)=i
        END IF
      END DO
# endif
!
!-----------------------------------------------------------------------
!  Post-processing of primary weights.
!-----------------------------------------------------------------------
!
!  Although it is assumed that the initial settings of the primary
!  weights has its center of gravity "reasonably close" to NDTFAST,
!  it may be not so according to the discrete rules of integration.
!  The following procedure is designed to put the center of gravity
!  exactly to NDTFAST by computing mismatch (NDTFAST-shift) and
!  applying basically an upstream advection of weights to eliminate
!  the mismatch iteratively. Once this procedure is complete primary
!  weights are normalized.
!
!  Find center of gravity of the primary weights and subsequently
!  calculate the mismatch to be compensated.
!
      DO iter=1,ndtfast(ng)
        wsum=0.0_r16
        shift=0.0_r16
        DO i=1,nfast(ng)
          wsum=wsum+weight(1,i,ng)
          shift=shift+REAL(i,dp)*weight(1,i,ng)
        END DO
        shift=shift/wsum
        cff=REAL(ndtfast(ng),dp)-shift
!
!  Apply advection step using either whole, or fractional shifts.
!  Notice that none of the four loops here is reversible.
!
        IF (cff.gt.1.0_r16) THEN
          nfast(ng)=nfast(ng)+1
          DO i=nfast(ng),2,-1
            weight(1,i,ng)=weight(1,i-1,ng)
          END DO
          weight(1,1,ng)=0.0_dp
        ELSE IF (cff.gt.0.0_r16) THEN
          wsum=1.0_r16-cff
          DO i=nfast(ng),2,-1
            weight(1,i,ng)=wsum*weight(1,i,ng)+cff*weight(1,i-1,ng)
          END DO
          weight(1,1,ng)=wsum*weight(1,1,ng)
        ELSE IF (cff.lt.-1.0_r16) THEN
          nfast(ng)=nfast(ng)-1
          DO i=1,nfast(ng),+1
            weight(1,i,ng)=weight(1,i+1,ng)
          END DO
          weight(1,nfast(ng)+1,ng)=0.0_dp
        ELSE IF (cff.lt.0.0_r16) THEN
          wsum=1.0_r16+cff
          DO i=1,nfast(ng)-1,+1
            weight(1,i,ng)=wsum*weight(1,i,ng)-cff*weight(1,i+1,ng)
          END DO
          weight(1,nfast(ng),ng)=wsum*weight(1,nfast(ng),ng)
        END IF
      END DO
!
!  Set SECONDARY weights assuming that backward Euler time step is used
!  for free surface.  Notice that array weight(2,i,ng) is assumed to
!  have all-zero status at entry in this segment of code.
!
      DO j=1,nfast(ng)
        cff=weight(1,j,ng)
        DO i=1,j
          weight(2,i,ng)=weight(2,i,ng)+cff
        END DO
      END DO
!
!  Normalize both set of weights.
!
      wsum=0.0_r16
      cff=0.0_r16
      DO i=1,nfast(ng)
        wsum=wsum+weight(1,i,ng)
        cff=cff+weight(2,i,ng)
      END DO
      wsum=1.0_r16/wsum
      cff=1.0_r16/cff
      DO i=1,nfast(ng)
        weight(1,i,ng)=wsum*weight(1,i,ng)
        weight(2,i,ng)=cff*weight(2,i,ng)
      END DO
!
```

</details>

<a id="variables"></a>

## 2. 输入输出、时间层、格点与单位

### 2.1 场与中间量

ROMS 使用 Arakawa C 型水平交错与地形跟随垂向坐标。下表的 Tunits 代表相应 tracer 的浓度单位（温度、盐度、被动 tracer 不必同量纲）。`W` 不是直接的物理垂直速度 m/s。

| 场 / 形状 | 格点与单位 | 本文五入口中的读写角色 |
|---|---|---|
| `pm/pn(i,j)` | rho 点，1/m | 只读；水平通量散度转为单位面积变化 |
| `on_u/om_v(i,j)` | U/V 面横向宽度，m | 只读；速度、厚度与体积输运之间换算 |
| `Hz(i,j,1:N)` | rho 水柱单元厚度，m | 只读；U/V 厚度由相邻 rho 单元平均 |
| `z_r(i,j,1:N)` | rho 层中心高度，m | 只读；垂向差分距离；不是层厚数组 |
| `Huon/Hvom(i,j,1:N)` | U/V 分层体积输运，m³/s | 预测及 tracer 校正只读；couple 原位修正并输出 |
| `W(i,j,0:N)` | S 坐标层界面体积通量，m³/s | 平流只读；上下界面的 tracer 平流通量设为 0 |
| `Akt(i,j,0:N,1:NAT)` | rho 柱 W 层界面扩散系数，m²/s | 只读；被动 tracer 使用 `min(NAT,itrc)` 对应的系数 |
| `Akv(i,j,0:N)` | rho 柱 W 层界面黏性系数，m²/s | 只读；U/V 点取相邻平均后构造垂向系统 |
| `btflx/stflx(i,j,1:NT)` | 底/表 tracer 边界通量，Tunits·m/s | 预测显式扩散的上下界 `dt*flux` |
| `bustr/bvstr/sustr/svstr(i,j)` | 运动学动量通量，m²/s² | U/V 预测上下边界应力；不是未经除密度的 N/m² |
| `t(i,j,1:N,1:3,1:NT)` | rho 点；各槽单位随阶段改变 | 第 3 槽存预测浓度 Tunits；`nnew` 在预测后为厚度加权 m·Tunits，在 tracer 校正除以 Hz 后恢复 Tunits |
| `u/v(i,j,1:N,1:2)` | U/V 点 | `nstp` 为 m/s；预测写 `nnew` 时为厚度加权 m²/s，solve 完成后恢复 m/s，couple 调整边界垂向均值 |
| `ru/rv(i,j,0:N,1:2)` | 动量 RHS 历史，推进所用 `k>=1` 为 m⁴/s² | 两历史槽只读；第 0 层留给二维/三维耦合用途，包装 stride 仍须包含它 |
| `DU/DV_avg1`、`DU/DV_avg2(i,j)` | U/V 面快平均体积输运，m³/s | solve/couple 只读；分别约束速度均值和修正后的分层质量通量之和 |
| `ubar/vbar(i,j,1:3)` | U/V 面深度平均速度，m/s | couple 写第 1、2 槽；不是写 `knew` 的 FB 路径 |
| `DiaTwrk(i,j,k,itrc,id)` | tracer 诊断工作量 | 预测写厚度加权 rate/vdif，校正写 x/y/h/v 平流增量并把所有诊断项除以 Hz，再累计隐式扩散；共同 epilogue 完成 rate 差值 |
| `DiaU3wrk/DiaV3wrk(i,j,k,id)` | 动量三维诊断工作量 | 预测为厚度加权，solve 转换/修正为速度增量，couple 完成 rate 差值；不是直接的最终每秒 tendency |
| `DiaRU/DiaRV(i,j,k,1:2,id)` | 诊断 RHS 历史 | 只读；与 `nrhs`、`3-nrhs` 配对 |
| `DiaU2wrk/DiaV2wrk`、`DiaU2int/DiaV2int(i,j,id)` | 二维诊断累计量 | solve 读快时间积分以校正三维项；couple 把累计质量通量变化换成速度变化并更新 rate 历史 |

同名 `FC/CF/DC/BC` 是原 Fortran 反复复用的私有工作数组，物理含义会随段落改变；例如 FC 既可能是平流通量、黏性系数，也可能是 LU 前的下对角。不能只凭变量名统一称为速度或 tracer。单位由真实表达式及 `mod_ocean.F`、`mod_grid.F`、`mod_mixing.F`、`mod_forces.F` 的声明说明核对。

### 2.2 慢时间层与历史槽

- `nstp`：慢步推进所基于的状态槽；`nnew`：本次将写入的慢状态槽；`nrhs`：慢步 RHS 槽。它们不等于快步的 `kstp/knew/krhs`。
- `indx = 3 - nrhs` 选择另一个 RHS 历史槽。`Pre3dConfig.step` 和 `Uv3dConfig.step` 由 `iic==ntfirst`、`iic==ntfirst+1`、其他三类映为 0、1、2；不是快步 predictor/corrector 标志。
- tracer 预测水平公式除首步外会先读 `t(nnew)` 中的旧历史值，再在同一列后续显式扩散中覆盖该槽；第 3 槽是单独的中间浓度，校正器只读它。
- `k_step3d_t_chunks` 强制 `nnew` 为 1 或 2，防止写入与只读第 3 槽 alias。预测器与 U/V 入口依赖模型提供合法时间层、尺寸和存储关系，不具有同等完整的运行时防御检查。
- `N` 是垂向层数，`NT` 是全部 tracer 数，`NAT` 是主动 tracer / 扩散系数末维长度，`NDT/NDM2d/NDM3d/NDrhs` 为诊断索引空间；它们与 32 列块宽不是一回事。

### 2.3 边界与 ABI 存储契约

`IstrU/JstrV/IstrP/IstrT/IendT/JstrT/JendT` 由真实 `set_bounds.h` 结果传入，不能简单全部替换成 `Istr/Jstr`。边界、周期、composite 标志决定差分折返、U/V 起点和边界均值修正。所有输入 halo 必须由原有流程准备好；C++ 无边界检查，也不做 MPI。

Fortran `C_LOC` 借用原存储，C++ `Array2d` 至 `Array5d` 只保存指针、原下界与参与 stride 的 extent，按 i 连续的列主序展开。末维 extent 不参与地址 stride 因而不保存在对象中，这不取消末维真实大小的要求。`W/Akt/Akv/ru/rv` 的第 0 层不能丢失；诊断的维序也不能与 tracer 的时间维互换。

`TYPE(C_PTR), VALUE` 对应 `void*`；配置 `TYPE(...), INTENT(IN)` **没有 VALUE**，对应指向结构体的 `const Config*`；`integer(C_INT) :: schemes(*)/hadv(*)/vadv(*)` 没有 VALUE，对应 `const int*`。结构的 `C_INT`/`C_DOUBLE` 与 C++ `int`/`double` 必须字段顺序、数组长度、类型及目标 C ABI padding 一致；不能以打包字节流传递，也不能换成 Fortran 默认 LOGICAL。各边界逻辑通过 `MERGE(1,0,...)` 传为整数。静态字段一致性可以核验，但本次不编译，不能冒称已经实测目标平台的 sizeof/offsetof。

需要双精度连续存储、正确 bounds 和存储生命周期。不同物理场与 scratch 不应意外重叠；同一个 `t/u/v` 内各合法时间槽的共享存储是有意设计。禁用诊断时，调用端传入已有状态数组的占位地址，只因相应 C++ 诊断访问已由同一宏编译掉才安全。正确配置的 unmanaged `Kokkos::View` 同样能零拷贝；本文不把“View 必然分配或复制”当作使用轻量包装的理由。

<a id="src-timelevel-definitions"></a>

**source-block ID：`timelevel-definitions`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Modules/mod_stepping.F`；闭区间：**25–45**；行数：**21**；UTF-8 payload SHA256：`91a177dd8872de098bc140f9d03c5a9988706ce4a9d35667b246f2f244ad8ab6`。

<details>
<summary>ROMS/Modules/mod_stepping.F，25–45（完整原文，可展开）</summary>

<!-- source-block id=timelevel-definitions commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Modules/mod_stepping.F lines=25-45 count=21 sha256=91a177dd8872de098bc140f9d03c5a9988706ce4a9d35667b246f2f244ad8ab6 -->
```fortran
!  knew      Barotropic (fast) time-step index corresponding to the    !
!              newest values for 2D primitive equation variables.      !
!  krhs      Barotropic (fast) time-step index used to compute the     !
!              right-hand-terms of 2D primitive equation variables.    !
!  kstp      Barotropic (fast) time-step index to which the current    !
!              changes are added to compute new 2D primitive equation  !
!              variables.                                              !
!                                                                      !
!  nfm3      Float index for time level "n-3".                         !
!  nfm2      Float index for time level "n-2".                         !
!  nfm1      Float index for time level "n-1".                         !
!  nf        Float index for time level "n".                           !
!  nfp1      Float index for time level "n+1".                         !
!                                                                      !
!  nnew      Baroclinic (slow) time-step index corresponding to the    !
!              newest values for 3D primitive equation variables.      !
!  nrhs      Baroclinic (slow) time-step index used to compute the     !
!              right-hand-terms of 3D primitive equation variables.    !
!  nstp      Baroclinic (slow) time-step index to which the current    !
!              changes are added to compute new 3D primitive equation  !
!              variables.                                              !
```

</details>

<a id="common"></a>

## 3. 计算类型、工作项、内存与优化机制

### 3.1 不要把五个入口一概称为 stencil

| 计算阶段 | 实际类型 | 并行与依赖 |
|---|---|---|
| tracer 水平平流 | 多场、交错网格、稀疏局部 stencil；U3/HSIMT 有高阶邻域和 limiter | 行/列间读取的是只读 `t(nstp)` 或预测第 3 槽以及输运、度量；写自身新槽 |
| 人工连续方程、时间历史、诊断换算 | 空间逐点的局部代数运算，叠加已经形成的面通量差分 | 给定通量后逐点；不应把形成通量所需邻点抹掉 |
| spline 垂向平流重构 | 每列沿 k 的三对角/前后递推，随后乘 W 形成界面通量 | 列与列独立；同一列内 k 不能当作任意独立点 |
| 普通显式垂向扩散/黏性 | 相邻层差分，再取界面通量散度 | 有 k 邻域；U/V 还需只读相邻 rho 柱几何与 Akv |
| 隐式垂向黏性 / spline 扩散 | 每列直接三对角追赶（Thomas/LU 前代、回代） | O(N) 列求解；不是全局 CG、不是全局稀疏矩阵迭代，也没有残差迭代循环 |
| 垂向均值替换和质量通量约束 | 列积分、定序累加与逐层修正；伴随二维快平均与诊断历史 | 保留 k 求和顺序，不能任意换成并行树形 reduction |

### 3.2 工作项和真实循环顺序

五个入口各有一个静态 `Kokkos::parallel_for` 调用点；没有 `TeamPolicy`、`MDRangePolicy`、全局 `parallel_reduce` 或显式 CPE DMA/LDM staging。

- tracer 两入口的 RangePolicy 是 `[0, ny*NT)`，`j=Jstr+index%ny`、`itrc=1+index/ny`；**j 变化最快**。一个工作项处理一个 tracer 的一行，并在该行沿 i 扫描。
- U/V 预测与 solve 的工作区间是 `[Jstr,Jend+1)`；couple 为 `[JstrT,JendT+1)`。一个工作项先 U，再在符合 `j>=JstrV` 或 `j>=Jstr` 时处理 V。RangePolicy 使用有符号 `int` 索引，能表达负 halo 下界；半开上界对应 Fortran 的闭区间。
- **KB=32 是沿水平 i 的 32 列，不是垂向 32 层**。tracer 预测、U/V solve、couple、tracer chunks 在一个 i 块内按 k 层扫，b/i 最内层；`q=k*KB+b` 表示第 k 层第 b 列。
- **`k_pre_step3d_uv` 是例外**：实际 `MomentumFields::row` 没有 `ib+=KB`，按整行 i 连续扫描；仅保存每列前一个界面通量与度量系数，scratch 与行宽相关、与 N 无关。文件开头“every ... chunks”的概述不应覆盖这个实际差别。
- 原 Fortran 也不是每个循环都以 i 最内层：U/V 初始几何循环有 `i→k`，HSIMT ETA 梯度有 `i→j`，tracer 水平/垂向/扩散各是全 tile 的不同遍历。当前实现保留每点关键表达式和每列 k 依赖顺序，但对跨点遍历做了重排；“保序”不是所有源行执行时间完全相同。

### 3.3 真实优化与代价

| 机制 | 已确认的位置 | 代价 / 限制 |
|---|---|---|
| 32 列有限工作集 | tracer 预测、UV solve/couple、tracer chunks 的 `ib` 循环 | 缩小每工作项同时活跃列数；scratch 仍覆盖整列的 N+1 层，不是只保留 32 个 double |
| i 连续访存 | 行内 k→b/i 的主循环；普通 Array 地址展开 | 有利于顺序流访问和缓存，但无编译报告不能断言实际 SIMD；严格 SW64 配置还关闭 CPE 自动向量化 |
| 共享 XI 面通量 | 两 tracer 入口的 `fx[0:nb]` | 同块内两相邻列复用一个面；相邻块公共面仍各算一次；ETA 面和 helper 的梯度/曲率有重算，不是所有面全局只算一次 |
| 多趟局部融合、去掉大中间场 | 预测水平→垂向→显式扩散在块内完成；chunks 平流→单位转换→隐式扩散 | 一次 launch 内复用 FC/CF/DC 等；可能用重复局部计算换掉全 tile FX/FE/curv/grad/oHz 存储；并未消除所有 host-memory scratch |
| 时间/方向模板 | `MomentumFields::row<STEP,V>`、`both<STEP>` | 预测 UV 按时间类与方向实例化；`if(c.step)` 在工作项 lambda 内选择，不是 host 端选择不同 launch |
| host 端准备元数据 | 配置复制、数组包装、scratch 预留；PRE3D 把 scheme 复制到 `Codes.h/v[32]` | 不在内核里解析 Fortran scheme 派生类型；tracer scheme helper 仍是运行时分支；UV solve/couple 的方向选择通过 UvDir，不是全部模板特化 |
| 诊断与物理状态同步写入 | `DIAGNOSTICS_TS/UV` 段，chunks 每个 NDT 项的换算 | 避免额外 launch，但诊断仍有实际读写与循环成本，不是免费附带 |
| 工作区复用 | 两个翻译单元各自 `thread_local HostWorkspace` | 只在超过高水位时重分配，不每个 chunk malloc；第一调用/增长有分配成本，保留容量会增加驻留内存 |

线程私有描述的是 host allocator 所有权和工作项分片，**不等于 CPE LDM 分配**。`HostWorkspace` 明确使用 `new double[]`；分片内数组、捕获对象和局部标量也不能保证全驻寄存器或始终命中缓存。源码能确认访问机制，不能单凭文本给每个入口独立加速比或认定硬件带宽/算力瓶颈。

### 3.4 scratch 的精确公式与生命周期

令 `ny=Jend-Jstr+1`、`nyt=JendT-JstrT+1`、`nx=Iend-Istr+1`、`B=KB=32`。以下数量单位为 **double 元素**，字节数在本文双精度契约下乘 8：

| 入口 | 每工作项 `per` | 本次 reserve 的总元素数 | 分片 |
|---|---|---|---|
| `pre_t` | `[2(N+1)+1]B+1` | `ny*NT*per` | `FC[(N+1)B]`、`CF[(N+1)B]`、`fx[B+1]`；偏移 `index*per` |
| `pre_uv` | `2(nx+1)` | `ny*per` | 两半 `fcm`、`dc0`，各 `nx+1`；偏移 `(j-Jstr)*per`；U/V 顺序复用 |
| `uv_solve` | `[8(N+1)+NDM2d+2]B` | `ny*per` | AK、Hzk、oHz、FC、CF、BC、DC、wrk 各 `(N+1)B`；cf1 为 B；Dwrk 留 `(NDM2d+1)B`，支持从 1 起的诊断索引 |
| `uv_couple` | `(N+5)B` | `nyt*per` | DCk 为 `(N+1)B`；DC0/CF0/FC0/cf1 各 B |
| `t_chunks` | `(B+1)+B+2(N+1)B` | `ny*NT*per` | fx 为 B+1；oh 为 B；cf/dc 各 `(N+1)B`；偏移 `index*per` |

`kokkos_step3d.cpp` 的四入口共用**同一个**每 host 线程 allocator，因此其容量是历次 reserve 需求的最大值，不是四项简单相加；`kokkos_step3d_t.cpp` 另有一个 allocator。若各入口均走过一次，单 host 线程两个 allocator 的高水位容量为 `max(pre_t总量, pre_uv总量, solve总量, couple总量) + chunks总量`，乘 8 是 payload 内存；实际进程还包括其他线程、其他翻译单元工作区和模型数组，本文不把此公式冒称整模内存。新 tile/网格尺寸可继续提高高水位。

`reserve` 只扩不缩，扩容时旧内存释放且内容不保留，算法须在读取前初始化所用位置；对象线程退出时由 unique_ptr 释放，不依赖 Kokkos 是否已 finalize。Serial 的 kernel 自身只是 parallel_for 普通循环，host 线程各有 workspace，初始化用 `call_once`、计时用 mutex；不同 tile 可按原契约并发，但重入同一线程工作区、重叠输出 tile、错误 alias 仍不安全，也不是任意 Kokkos Serial 服务均线程安全。这里不使用会访问共享 reduction scratch 的 `parallel_reduce`。

SW64 模式下 allocator 与 `reserve` 都在 `#ifndef __sw_slave__` 的 host 分支，slave 仅通过 launch 捕获的裸指针访问 HostSpace。仓库后端 `ParallelFor` 把工作区间按 `ceil(nwork/64)` 连续分片交给 64 个 CPE，短区间不保证全用满；launch 末尾 `athread_join_cgs()` 后才返回。故随后 Fortran 处理和下一次 reserve 复用有同步依据，不是把这种共享/同步性质推广到所有 Kokkos 后端。

`ROMS_KK_TIMER` 在 `initialize()` 后开始，范围包括当前入口的包装、配置准备、首次/增长 reserve 及 launch 等 wall 成本，不是单独设备算术时间；不能用整例程 wclock 时间减一个未经核实的计时口径去虚构内核收益。

<a id="pre-t"></a>

## 4. k_pre_step3d_t：tracer 预测与显式垂向扩散

### 4.1 完整原始 Fortran 计算段

原始外围是文件级 `NONLINEAR && SOLVE3D`；文件第 4 行定义 `NEUMANN`。下块完整包含 `#ifndef TS_FIXED` 的 tracer 段、所有水平和垂向 scheme、太阳辐射、年龄源、LMD nonlocal、点源、诊断和上下通量条件；未移植/被拒绝组合不能与支持范围混淆。声明与预测后侧边界/通信见附录 A。

<a id="src-original-pre-t"></a>

**source-block ID：`original-pre-t`**；commit：`cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`；路径：`ROMS/Nonlinear/pre_step3d.F`；闭区间：**318–933**；行数：**616**；UTF-8 payload SHA256：`ec06949f19ece4dc9a9385fb43e3346d3ceed17fd5a79e1eb8a0548667dcd8af`。

<details>
<summary>ROMS/Nonlinear/pre_step3d.F，318–933（完整原文，可展开）</summary>

<!-- source-block id=original-pre-t commit=cf10c0f7cc270a7075ad19ee3b8950be352a7cd0 path=ROMS/Nonlinear/pre_step3d.F lines=318-933 count=616 sha256=ec06949f19ece4dc9a9385fb43e3346d3ceed17fd5a79e1eb8a0548667dcd8af -->
```fortran
# ifndef TS_FIXED
!
!=======================================================================
!  Tracer equation(s).
!=======================================================================

#  ifdef SOLAR_SOURCE
!
!  Compute fraction of the solar shortwave radiation, "swdk"
!  (at vertical W-points) penetrating water column.
!
      DO k=1,N(ng)-1
        DO j=Jstr,Jend
          DO i=Istr,Iend
            FX(i,j)=z_w(i,j,N(ng))-z_w(i,j,k)
          END DO
        END DO
        CALL lmd_swfrac_tile (ng, tile,                                 &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        -1.0_r8, FX, FE)
        DO j=Jstr,Jend
          DO i=Istr,Iend
             swdk(i,j,k)=FE(i,j)
          END DO
        END DO
      END DO
#  endif
!
!-----------------------------------------------------------------------
!  Compute intermediate tracer at n+1/2 time-step, t(i,j,k,3,itrc).
!-----------------------------------------------------------------------
!
!  Compute time rate of change of intermediate tracer due to
!  horizontal advection.
!
      T_LOOP1 :DO itrc=1,NT(ng)
        K_LOOP: DO k=1,N(ng)
!
          HADV_FLUX : IF (Hadvection(itrc,ng)%CENTERED2) THEN
!
!  Second-order, centered differences horizontal advective fluxes.
!
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                FX(i,j)=Huon(i,j,k)*                                    &
     &                  0.5_r8*(t(i-1,j,k,nstp,itrc)+                   &
     &                          t(i  ,j,k,nstp,itrc))
              END DO
            END DO
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                FE(i,j)=Hvom(i,j,k)*                                    &
     &                  0.5_r8*(t(i,j-1,k,nstp,itrc)+                   &
     &                          t(i,j  ,k,nstp,itrc))
              END DO
            END DO
!
          ELSE IF ((Hadvection(itrc,ng)%MPDATA).or.                     &
     &             (Hadvection(itrc,ng)%HSIMT)) THEN
!
!  First-order, upstream differences horizontal advective fluxes.
!
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                cff1=MAX(Huon(i,j,k),0.0_r8)
                cff2=MIN(Huon(i,j,k),0.0_r8)
                FX(i,j)=cff1*t(i-1,j,k,nstp,itrc)+                      &
     &                  cff2*t(i  ,j,k,nstp,itrc)
              END DO
            END DO
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                cff1=MAX(Hvom(i,j,k),0.0_r8)
                cff2=MIN(Hvom(i,j,k),0.0_r8)
                FE(i,j)=cff1*t(i,j-1,k,nstp,itrc)+                      &
     &                  cff2*t(i,j  ,k,nstp,itrc)
              END DO
            END DO
!
          ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.                     &
     &             (Hadvection(itrc,ng)%CENTERED4).or.                  &
     &             (Hadvection(itrc,ng)%SPLIT_U3).or.                   &
     &             (Hadvection(itrc,ng)%UPSTREAM3)) THEN
!
!  Fourth-order Akima, fourth-order centered differences, or third-order
!  upstream-biased horizontal advective fluxes.
!
            DO j=Jstr,Jend
              DO i=Istrm1,Iendp2
                FX(i,j)=t(i  ,j,k,nstp,itrc)-                           &
     &                  t(i-1,j,k,nstp,itrc)
#  ifdef MASKING
                FX(i,j)=FX(i,j)*umask(i,j)
#  endif
              END DO
            END DO
            IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
              IF (DOMAIN(ng)%Western_Edge(tile)) THEN
                DO j=Jstr,Jend
                  FX(Istr-1,j)=FX(Istr,j)
                END DO
              END IF
            END IF
            IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
              IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
                DO j=Jstr,Jend
                  FX(Iend+2,j)=FX(Iend+1,j)
                END DO
              END IF
            END IF
!
            DO j=Jstr,Jend
              DO i=Istr-1,Iend+1
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  curv(i,j)=FX(i+1,j)-FX(i,j)
                ELSE IF (Hadvection(itrc,ng)%AKIMA4) THEN
                  cff=2.0_r8*FX(i+1,j)*FX(i,j)
                  IF (cff.gt.eps) THEN
                    grad(i,j)=cff/(FX(i+1,j)+FX(i,j))
                  ELSE
                    grad(i,j)=0.0_r8
                  END IF
                ELSE IF ((Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  grad(i,j)=0.5_r8*(FX(i+1,j)+FX(i,j))
                END IF
              END DO
            END DO
!
            cff1=1.0_r8/6.0_r8
            cff2=1.0_r8/3.0_r8
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  FX(i,j)=Huon(i,j,k)*0.5_r8*                           &
     &                    (t(i-1,j,k,nstp,itrc)+                        &
     &                     t(i  ,j,k,nstp,itrc))-                       &
     &                    cff1*(curv(i-1,j)*MAX(Huon(i,j,k),0.0_r8)+    &
     &                          curv(i  ,j)*MIN(Huon(i,j,k),0.0_r8))
                ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.               &
     &                   (Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN

                  FX(i,j)=Huon(i,j,k)*0.5_r8*                           &
     &                    (t(i-1,j,k,nstp,itrc)+                        &
     &                     t(i  ,j,k,nstp,itrc)-                        &
     &                     cff2*(grad(i  ,j)-                           &
     &                           grad(i-1,j)))
                END IF
              END DO
            END DO
!
            DO j=Jstrm1,Jendp2
              DO i=Istr,Iend
                FE(i,j)=t(i,j  ,k,nstp,itrc)-                           &
     &                  t(i,j-1,k,nstp,itrc)
#  ifdef MASKING
                FE(i,j)=FE(i,j)*vmask(i,j)
#  endif
              END DO
            END DO
            IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
              IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
                DO i=Istr,Iend
                  FE(i,Jstr-1)=FE(i,Jstr)
                END DO
              END IF
            END IF
            IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
              IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
                DO i=Istr,Iend
                  FE(i,Jend+2)=FE(i,Jend+1)
                END DO
              END IF
            END IF
!
            DO j=Jstr-1,Jend+1
              DO i=Istr,Iend
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  curv(i,j)=FE(i,j+1)-FE(i,j)
                ELSE IF (Hadvection(itrc,ng)%AKIMA4) THEN
                  cff=2.0_r8*FE(i,j+1)*FE(i,j)
                  IF (cff.gt.eps) THEN
                    grad(i,j)=cff/(FE(i,j+1)+FE(i,j))
                  ELSE
                   grad(i,j)=0.0_r8
                  END IF
                ELSE IF ((Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  grad(i,j)=0.5_r8*(FE(i,j+1)+FE(i,j))
                END IF
              END DO
            END DO
!
            cff1=1.0_r8/6.0_r8
            cff2=1.0_r8/3.0_r8
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  FE(i,j)=Hvom(i,j,k)*0.5_r8*                           &
     &                    (t(i,j-1,k,nstp,itrc)+                        &
     &                     t(i,j  ,k,nstp,itrc))-                       &
     &                    cff1*(curv(i,j-1)*MAX(Hvom(i,j,k),0.0_r8)+    &
     &                          curv(i,j  )*MIN(Hvom(i,j,k),0.0_r8))
                ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.               &
     &                   (Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  FE(i,j)=Hvom(i,j,k)*0.5_r8*                           &
     &                    (t(i,j-1,k,nstp,itrc)+                        &
     &                     t(i,j  ,k,nstp,itrc)-                        &
     &                     cff2*(grad(i,j  )-                           &
     &                           grad(i,j-1)))
                END IF
              END DO
            END DO
          END IF HADV_FLUX
!
!  Apply tracers point sources to the horizontal advection terms,
!  if any.
!
!    Dsrc(is) = 0,  flow across grid cell u-face (positive or negative)
!    Dsrc(is) = 1,  flow across grid cell v-face (positive or negative)
!
          IF (LuvSrc(ng)) THEN
            DO is=1,Nsrc(ng)
              Isrc=SOURCES(ng)%Isrc(is)
              Jsrc=SOURCES(ng)%Jsrc(is)
              IF (((Istr.le.Isrc).and.(Isrc.le.Iend+1)).and.            &
     &            ((Jstr.le.Jsrc).and.(Jsrc.le.Jend+1))) THEN
                IF (INT(SOURCES(ng)%Dsrc(is)).eq.0) THEN
                  IF (LtracerSrc(itrc,ng)) THEN
                    FX(Isrc,Jsrc)=Huon(Isrc,Jsrc,k)*                    &
     &                            SOURCES(ng)%Tsrc(is,k,itrc)
                  ELSE
                    FX(Isrc,Jsrc)=0.0_r8
                  END IF
                ELSE IF (INT(SOURCES(ng)%Dsrc(is)).eq.1) THEN
                  IF (LtracerSrc(itrc,ng)) THEN
                    FE(Isrc,Jsrc)=Hvom(Isrc,Jsrc,k)*                    &
     &                            SOURCES(ng)%Tsrc(is,k,itrc)
                  ELSE
                     FE(Isrc,Jsrc)=0.0_r8
                  END IF
                END IF
              END IF
            END DO
          END IF
!
!  Time-step horizontal advection (m Tunits).
!
          IF ((Hadvection(itrc,ng)%MPDATA).or.                          &
     &        (Hadvection(itrc,ng)%HSIMT)) THEN
            Gamma=0.5_r8
          ELSE
            Gamma=1.0_r8/6.0_r8
          END IF
          IF (iic(ng).eq.ntfirst(ng)) THEN
            cff=0.5_r8*dt(ng)
            cff1=1.0_r8
            cff2=0.0_r8
          ELSE
            cff=(1.0_r8-Gamma)*dt(ng)
            cff1=0.5_r8+Gamma
            cff2=0.5_r8-Gamma
          END IF
          DO j=Jstr,Jend
            DO i=Istr,Iend
              t(i,j,k,3,itrc)=Hz(i,j,k)*(cff1*t(i,j,k,nstp,itrc)+       &
     &                                   cff2*t(i,j,k,nnew,itrc))-      &
     &                        cff*pm(i,j)*pn(i,j)*                      &
     &                        (FX(i+1,j)-FX(i,j)+                       &
     &                         FE(i,j+1)-FE(i,j))
            END DO
          END DO
        END DO K_LOOP
      END DO T_LOOP1

#  if defined AGE_MEAN && defined T_PASSIVE
!
!  If inert passive tracer and Mean Age, compute age concentration (even
!  inert index) forced by the right-hand-side term that is concentration
!  of an associated conservative passive tracer (odd inert index).
!  Implemented and tested by W.G. Zhang and J. Wilkin. (m Tunits)
!
      DO itrc=1,NPT,2
        IF (.not.((Hadvection(inert(itrc),ng)%MPDATA).or.               &
     &            (Hadvection(inert(itrc),ng)%HSIMT))) THEN
          IF (iic(ng).eq.ntfirst(ng)) THEN
            cff=0.5_r8*dt(ng)
          ELSE
            Gamma=1.0_r8/6.0_r8
            cff=(1.0_r8-Gamma)*dt(ng)
          END IF
          iage=inert(itrc+1)                   ! even inert tracer index
          DO k=1,N(ng)
            DO j=Jstr,Jend
              DO i=Istr,Iend
                t(i,j,k,3,iage)=t(i,j,k,3,iage)+                        &
     &                          cff*Hz(i,j,k)*                          &
     &                          t(i,j,k,nnew,inert(itrc))
              END DO
            END DO
          END DO
        END IF
      END DO
#  endif
!
!-----------------------------------------------------------------------
!  Compute time rate of change of intermediate tracer due to vertical
!  advection.  Impose artificial continuity equation.
!-----------------------------------------------------------------------
!
      J_LOOP1 : DO j=Jstr,Jend
        T_LOOP2 : DO itrc=1,NT(ng)
!
          VADV_FLUX : IF (Vadvection(itrc,ng)%SPLINES) THEN
!
!  Build conservative parabolic splines for the vertical derivatives
!  "FC" of the tracer.  Then, the interfacial "FC" values are
!  converted to vertical advective flux.
!
            DO i=Istr,Iend
#  ifdef NEUMANN
              FC(i,0)=1.5_r8*t(i,j,1,nstp,itrc)
              CF(i,1)=0.5_r8
#  else
              FC(i,0)=2.0_r8*t(i,j,1,nstp,itrc)
              CF(i,1)=1.0_r8
#  endif
            END DO
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff=1.0_r8/(2.0_r8*Hz(i,j,k)+                           &
     &                      Hz(i,j,k+1)*(2.0_r8-CF(i,k)))
                CF(i,k+1)=cff*Hz(i,j,k)
                FC(i,k)=cff*(3.0_r8*(Hz(i,j,k  )*t(i,j,k+1,nstp,itrc)+  &
     &                               Hz(i,j,k+1)*t(i,j,k  ,nstp,itrc))- &
     &                       Hz(i,j,k+1)*FC(i,k-1))
              END DO
            END DO
            DO i=Istr,Iend
#  ifdef NEUMANN
              FC(i,N(ng))=(3.0_r8*t(i,j,N(ng),nstp,itrc)-               &
     &                     FC(i,N(ng)-1))/(2.0_r8-CF(i,N(ng)))
#  else
              FC(i,N(ng))=(2.0_r8*t(i,j,N(ng),nstp,itrc)-               &
     &                     FC(i,N(ng)-1))/(1.0_r8-CF(i,N(ng)))
#  endif
            END DO
            DO k=N(ng)-1,0,-1
              DO i=Istr,Iend
                FC(i,k)=FC(i,k)-CF(i,k+1)*FC(i,k+1)
#  ifdef WEC_VF
                FC(i,k+1)=(W(i,j,k+1)+W_stokes(i,j,k+1))*FC(i,k+1)
#  else
                FC(i,k+1)=W(i,j,k+1)*FC(i,k+1)
#  endif
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,N(ng))=0.0_r8
              FC(i,0)=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%AKIMA4) THEN
!
!  Fourth-order, Akima vertical advective flux.
!
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                FC(i,k)=t(i,j,k+1,nstp,itrc)-                           &
     &                  t(i,j,k  ,nstp,itrc)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=FC(i,1)
              FC(i,N(ng))=FC(i,N(ng)-1)
            END DO
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff=2.0_r8*FC(i,k)*FC(i,k-1)
                IF (cff.gt.eps) THEN
                  CF(i,k)=cff/(FC(i,k)+FC(i,k-1))
                ELSE
                  CF(i,k)=0.0_r8
                END IF
              END DO
            END DO
            cff1=1.0_r8/3.0_r8
            DO k=1,N(ng)-1
              DO i=Istr,Iend
#  ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
#  else
                FC(i,k)=W(i,j,k)*                                       &
#  endif
     &                  0.5_r8*(t(i,j,k  ,nstp,itrc)+                   &
     &                          t(i,j,k+1,nstp,itrc)-                   &
     &                          cff1*(CF(i,k+1)-CF(i,k)))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%CENTERED2) THEN
!
!  Second-order, central differences vertical advective flux.
!
            DO k=1,N(ng)-1
              DO i=Istr,Iend
#  ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
#  else
                FC(i,k)=W(i,j,k)*                                       &
#  endif
     &                  0.5_r8*(t(i,j,k  ,nstp,itrc)+                   &
     &                          t(i,j,k+1,nstp,itrc))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF ((Vadvection(itrc,ng)%MPDATA).or.                     &
     &             (Vadvection(itrc,ng)%HSIMT)) THEN
!
!  First_order, upstream differences vertical advective flux.
!
            DO k=1,N(ng)-1
              DO i=Istr,Iend
#  if defined WEC_VF
                cff1=MAX(W(i,j,k)+W_stokes(i,j,k),0.0_r8)
                cff2=MIN(W(i,j,k)+W_stokes(i,j,k),0.0_r8)
#  else
                cff1=MAX(W(i,j,k),0.0_r8)
                cff2=MIN(W(i,j,k),0.0_r8)
#  endif
                FC(i,k)=cff1*t(i,j,k  ,nstp,itrc)+                      &
     &                  cff2*t(i,j,k+1,nstp,itrc)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF ((Vadvection(itrc,ng)%CENTERED4).or.                  &
     &             (Vadvection(itrc,ng)%SPLIT_U3)) THEN
!
!  Fourth-order, central differences vertical advective flux.
!
            cff1=0.5_r8
            cff2=7.0_r8/12.0_r8
            cff3=1.0_r8/12.0_r8
            DO k=2,N(ng)-2
              DO i=Istr,Iend
#  ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
#  else
                FC(i,k)=W(i,j,k)*                                       &
#  endif
     &                  (cff2*(t(i,j,k  ,nstp,itrc)+                    &
     &                         t(i,j,k+1,nstp,itrc))-                   &
     &                   cff3*(t(i,j,k-1,nstp,itrc)+                    &
     &                         t(i,j,k+2,nstp,itrc)))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
#  ifdef WEC_VF
              FC(i,1)=(W(i,j,1)+W_stokes(i,j,1))*                       &
#  else
              FC(i,1)=W(i,j,1)*                                         &
#  endif
     &                (cff1*t(i,j,1,nstp,itrc)+                         &
     &                 cff2*t(i,j,2,nstp,itrc)-                         &
     &                 cff3*t(i,j,3,nstp,itrc))
#  ifdef WEC_VF
              FC(i,N(ng)-1)=(W(i,j,N(ng)-1)+W_stokes(i,j,N(ng)-1))*     &
#  else
              FC(i,N(ng)-1)=W(i,j,N(ng)-1)*                             &
#  endif
     &                      (cff1*t(i,j,N(ng)  ,nstp,itrc)+             &
     &                       cff2*t(i,j,N(ng)-1,nstp,itrc)-             &
     &                       cff3*t(i,j,N(ng)-2,nstp,itrc))
              FC(i,N(ng))=0.0_r8
            END DO
          END IF VADV_FLUX
!
!  Compute artificial continuity equation and load it into private
!  array DC (1/m). It is imposed to preserve tracer constancy. It is
!  not the same for all the tracer advection schemes because of the
!  Gamma value.
!
          IF ((Vadvection(itrc,ng)%MPDATA).or.                          &
     &        (Vadvection(itrc,ng)%HSIMT)) THEN
            Gamma=0.5_r8
          ELSE
            Gamma=1.0_r8/6.0_r8
          END IF
          IF (iic(ng).eq.ntfirst(ng)) THEN
            cff=0.5_r8*dt(ng)
          ELSE
            cff=(1.0_r8-Gamma)*dt(ng)
          END IF
          DO k=1,N(ng)
            DO i=Istr,Iend
              DC(i,k)=1.0_r8/(Hz(i,j,k)-                                &
     &                        cff*pm(i,j)*pn(i,j)*                      &
     &                        (Huon(i+1,j,k)-Huon(i,j,k)+               &
     &                         Hvom(i,j+1,k)-Hvom(i,j,k)+               &
#  ifdef WEC_VF
     &                        (W_stokes(i,j,k)-W_stokes(i,j,k-1))+      &
#  endif
     &                        (W(i,j,k)-W(i,j,k-1))))
            END DO
          END DO
!
! Time-step vertical advection of tracers (Tunits). Impose artificial
! continuity equation.
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              cff1=cff*pm(i,j)*pn(i,j)
              t(i,j,k,3,itrc)=DC(i,k)*                                  &
     &                        (t(i,j,k,3,itrc)-                         &
     &                         cff1*(FC(i,k)-FC(i,k-1)))
            END DO
          END DO
        END DO T_LOOP2
      END DO J_LOOP1
!
!-----------------------------------------------------------------------
!  Start computation of tracers at n+1 time-step, t(i,j,k,nnew,itrc).
!-----------------------------------------------------------------------
!
!  Compute vertical diffusive fluxes "FC" of the tracer fields at
!  current time step n, and at horizontal RHO-points and vertical
!  W-points.  Notice that the vertical diffusion coefficients for
!  passive tracers is the same as that for salinity (ltrc=NAT).
!
      DO j=Jstr,Jend
        cff3=dt(ng)*(1.0_r8-lambda)
        DO itrc=1,NT(ng)
          ltrc=MIN(NAT,itrc)
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              cff=1.0_r8/(z_r(i,j,k+1)-z_r(i,j,k))
              FC(i,k)=cff3*cff*Akt(i,j,k,ltrc)*                         &
     &                (t(i,j,k+1,nstp,itrc)-                            &
     &                 t(i,j,k  ,nstp,itrc))
            END DO
          END DO

#  ifdef LMD_NONLOCAL
!
!  Add in the nonlocal transport flux for unstable (convective)
!  forcing conditions into matrix FC when using the Large et al.
!  KPP scheme. The nonlocal transport is only applied to active
!  tracers.
!
          IF (itrc.le.NAT) THEN
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                FC(i,k)=FC(i,k)-                                        &
     &                  dt(ng)*Akt(i,j,k,itrc)*ghats(i,j,k,itrc)
              END DO
            END DO
          END IF
#  endif
#  ifdef SOLAR_SOURCE
!
!  Add in incoming solar radiation at interior W-points using decay
!  decay penetration function based on Jerlov water type.
!
          IF (itrc.eq.itemp) THEN
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                FC(i,k)=FC(i,k)+                                        &
     &                  dt(ng)*srflx(i,j)*                              &
#   ifdef WET_DRY
     &                  rmask_wet(i,j)*                                 &
#   endif
     &                  swdk(i,j,k)
              END DO
            END DO
          END IF
#  endif
!
!  Apply bottom and surface tracer flux conditions.
!
          DO i=Istr,Iend
            FC(i,0)=dt(ng)*btflx(i,j,itrc)
            FC(i,N(ng))=dt(ng)*stflx(i,j,itrc)
          END DO
!
!  Compute new tracer field (m Tunits).
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              cff1=Hz(i,j,k)*t(i,j,k,nstp,itrc)
              cff2=FC(i,k)-FC(i,k-1)
              t(i,j,k,nnew,itrc)=cff1+cff2
#  ifdef DIAGNOSTICS_TS
              DiaTwrk(i,j,k,itrc,iTrate)=cff1
              DiaTwrk(i,j,k,itrc,iTvdif)=cff2
#  endif
            END DO
          END DO
        END DO
      END DO
# endif /* !TS_FIXED */
```

</details>

### 4.2 完整当前 C++ 函数

本函数全部数组包装、scheme 表复制、分配、时间系数分支和三趟局部计算均保留。完整 `TracerFields`（`dx/de/cell_slope/flux_x/flux_e/vertical_flux`）与文件级 kEps/KB/allocator 定义在[附录 B](#appendix-cpp)，不是被省去的可选部分。

<a id="src-current-pre-t"></a>

**source-block ID：`current-pre-t`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.cpp`；闭区间：**404–552**；行数：**149**；UTF-8 payload SHA256：`234d01bcd29399951eb0ba41615e85e53b00daa46b8f2376a137cab6a7da5c6d`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.cpp，404–552（完整原文，可展开）</summary>

<!-- source-block id=current-pre-t commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.cpp lines=404-552 count=149 sha256=234d01bcd29399951eb0ba41615e85e53b00daa46b8f2376a137cab6a7da5c6d -->
```cpp
void k_pre_step3d_t(const Pre3dConfig* cfg, const int* hadv, const int* vadv,
                    void* pm_p, void* pn_p, void* Hz_p,
                    void* Huon_p, void* Hvom_p, void* z_r_p, void* W_p,
                    void* Akt_p, void* btflx_p, void* stflx_p,
                    void* t_p, void* DiaTwrk_p) {
    roms::initialize();
    ROMS_KK_TIMER("pre_step3d tracers");

    TracerFields f;
    f.c = *cfg;
    const Pre3dConfig &c = f.c;
    const int L0 = c.LBi, L1 = c.UBi, M0 = c.LBj, M1 = c.UBj;
    f.pm = roms::as_2d<double>(pm_p, L0, L1, M0, M1);
    f.pn = roms::as_2d<double>(pn_p, L0, L1, M0, M1);
    f.Hz = roms::as_3d<double>(Hz_p, L0, L1, M0, M1, 1, c.N);
    f.Huon = roms::as_3d<double>(Huon_p, L0, L1, M0, M1, 1, c.N);
    f.Hvom = roms::as_3d<double>(Hvom_p, L0, L1, M0, M1, 1, c.N);
    f.zr = roms::as_3d<double>(z_r_p, L0, L1, M0, M1, 1, c.N);
    f.W = roms::as_3d<double>(W_p, L0, L1, M0, M1, 0, c.N);
    f.btflx = roms::as_3d<double>(btflx_p, L0, L1, M0, M1, 1, c.NT);
    f.stflx = roms::as_3d<double>(stflx_p, L0, L1, M0, M1, 1, c.NT);
    f.Akt = roms::as_4d<double>(Akt_p, L0, L1, M0, M1, 0, c.N, 1, c.NAT);
    f.t = roms::as_5d<double>(t_p, L0, L1, M0, M1, 1, c.N, 1, 3, 1, c.NT);
#if defined DIAGNOSTICS_TS
    f.dia = roms::as_5d<double>(DiaTwrk_p, L0, L1, M0, M1, 1, c.N, 1, c.NT,
                                1, c.NDT);
#else
    (void)DiaTwrk_p;
#endif

    /* Scheme codes travel by value: copy them out of the Fortran array. */
    struct Codes {
        int h[32], v[32];
    } codes{};
    if (c.NT > 32)
        Kokkos::abort("k_pre_step3d_t: more than 32 tracers");
    for (int it = 0; it < c.NT; ++it) {
        codes.h[it] = hadv[it];
        codes.v[it] = vadv[it];
    }

    const int ny = c.Jend - c.Jstr + 1;
    const int N = c.N;
    const int per = (2 * (N + 1) + 1) * KB + 1; /* FC, CF, fx */
    double* scratch = nullptr;
#ifndef __sw_slave__
    scratch = workspace.reserve(std::size_t(ny) * c.NT * per);
#endif

    Kokkos::parallel_for(
        "ROMS pre_step3d tracers", range_policy(0, ny * c.NT),
        KOKKOS_LAMBDA(int index) {
            const int j = c.Jstr + index % ny;
            const int it = 1 + index / ny;
            const int hs = codes.h[it - 1];
            const int vs = codes.v[it - 1];
            double* FC = scratch + std::size_t(index) * per;
            double* CF = FC + (N + 1) * KB;
            double* fx = CF + (N + 1) * KB; /* KB+1 xi faces */

            /* Horizontal time-stepping coefficients. */
            double Gamma = (hs == roms::kUpstream1) ? 0.5 : 1.0 / 6.0;
            double cffh, cff1h, cff2h;
            if (c.step == 0) {
                cffh = 0.5 * c.dt;
                cff1h = 1.0;
                cff2h = 0.0;
            } else {
                cffh = (1.0 - Gamma) * c.dt;
                cff1h = 0.5 + Gamma;
                cff2h = 0.5 - Gamma;
            }
            /* Vertical advection coefficient. */
            Gamma = (vs == roms::kUpstream1) ? 0.5 : 1.0 / 6.0;
            double cffv;
            if (c.step == 0) {
                cffv = 0.5 * c.dt;
            } else {
                cffv = (1.0 - Gamma) * c.dt;
            }
            const double cff3d = c.dt * (1.0 - c.lambda);
            const int ltrc = (c.NAT < it) ? c.NAT : it;
            const int s = c.nstp, nn = c.nnew;

            for (int ib = c.Istr; ib <= c.Iend; ib += KB) {
                const int nb = (c.Iend - ib + 1 < KB) ? (c.Iend - ib + 1) : KB;
#define I_LOOP for (int b = 0, i = ib; b < nb; ++b, ++i)

                /* 1. Horizontal advection (m Tunits), K_LOOP.  The face
                ** shared with the next chunk is evaluated by both; it is the
                ** same expression on the same inputs. */
                for (int k = 1; k <= N; ++k) {
                    for (int b = 0; b <= nb; ++b)
                        fx[b] = f.flux_x(ib + b, j, k, it, hs);
                    I_LOOP {
                        const double fe0 = f.flux_e(i, j, k, it, hs);
                        const double fe1 = f.flux_e(i, j + 1, k, it, hs);
                        f.t(i, j, k, 3, it) =
                            f.Hz(i, j, k) * (cff1h * f.t(i, j, k, s, it) +
                                             cff2h * f.t(i, j, k, nn, it)) -
                            cffh * f.pm(i, j) * f.pn(i, j) *
                                (fx[b + 1] - fx[b] + fe1 - fe0);
                    }
                }

                /* 2. Vertical advection, artificial continuity equation. */
                f.vertical_flux(ib, nb, j, it, vs, FC, CF);
                for (int k = 1; k <= N; ++k) {
                    I_LOOP {
                        const double DC =
                            1.0 / (f.Hz(i, j, k) -
                                   cffv * f.pm(i, j) * f.pn(i, j) *
                                       (f.Huon(i + 1, j, k) - f.Huon(i, j, k) +
                                        f.Hvom(i, j + 1, k) - f.Hvom(i, j, k) +
                                        (f.W(i, j, k) - f.W(i, j, k - 1))));
                        const double cff1 = cffv * f.pm(i, j) * f.pn(i, j);
                        f.t(i, j, k, 3, it) =
                            DC * (f.t(i, j, k, 3, it) -
                                  cff1 * (FC[k * KB + b] - FC[(k - 1) * KB + b]));
                    }
                }

                /* 3. Explicit vertical diffusion into t(nnew) (m Tunits). */
                for (int k = 1; k <= N - 1; ++k) {
                    I_LOOP {
                        const double cff = 1.0 / (f.zr(i, j, k + 1) - f.zr(i, j, k));
                        FC[k * KB + b] = cff3d * cff * f.Akt(i, j, k, ltrc) *
                                         (f.t(i, j, k + 1, s, it) - f.t(i, j, k, s, it));
                    }
                }
                I_LOOP {
                    FC[b] = c.dt * f.btflx(i, j, it);
                    FC[N * KB + b] = c.dt * f.stflx(i, j, it);
                }
                for (int k = 1; k <= N; ++k) {
                    I_LOOP {
                        const double cff1 = f.Hz(i, j, k) * f.t(i, j, k, s, it);
                        const double cff2 = FC[k * KB + b] - FC[(k - 1) * KB + b];
                        f.t(i, j, k, nn, it) = cff1 + cff2;
#if defined DIAGNOSTICS_TS
                        f.dia(i, j, k, it, c.iTrate) = cff1;
                        f.dia(i, j, k, it, c.iTvdif) = cff2;
#endif
                    }
                }
#undef I_LOOP
            }
        });
}
```

</details>

### 4.3 物理与实现解释

**输入/输出。** 读几何、输运、`t(nstp)` 与原 `t(nnew)` 历史；写第 3 槽半时间预测浓度、`nnew` 槽厚度加权浓度及可选 rate/vdif 诊断。预测浓度用于后面的 tracer 校正平流，而 `nnew` 累积量继续承接水平扩散、生物等贡献。

**Fortran 多趟 → 当前块内三趟。** 原版先 `itrc→k→j→i` 构造水平通量并更新第 3 槽，再 `j→itrc` 求垂向界面通量/人工连续方程，最后单独扫描显式垂向扩散；当前一个 `(j,itrc)` 工作项沿 i 的 32 列块完成三趟，每趟仍按该列的 k 顺序与 b/i 连续扫描。水平邻点只读 `nstp`，`nnew` 旧值只在本点第一趟读，第三趟才覆盖本点，因此不需要等待别的块写新值；这依赖合法且无误 alias 的历史槽。

**水平 stencil。** CENTERED2 两侧中心通量；预测 MPDATA/HSIMT 用一阶迎风；UPSTREAM3 由二阶差分曲率给出有向修正；AKIMA4 以乘积大于 `1e-16` 才取调和式坡度；CENTERED4/SPLIT_U3 取中心坡度。closed edge 把最外一段差分折回内侧，composite/periodic 不折回。XI 面存在 `fx[nb+1]` 的块内共享；ETA 面按两侧重算。预测的一阶 HSIMT/MPDATA **不等于**最终校正器已支持完整 MPDATA。

**垂向不是全都独立点。** SPLINES 分支先按 k 前代、再从顶向底回代得到层界面 tracer 重构量（FC 乘 W 之前具有浓度单位，不是直接解最终 t），再乘 W 成为平流通量，强制 FC(0)=FC(N)=0。NEUMANN 在原文件固定启用，当前实现只保留这个实际有效端点公式；原始死 CPP 的非 NEUMANN 公式仍可在源码块追查。其他分支分别做 Akima、中心二阶、一阶迎风、中心四阶及顶底邻层专用公式；N>=3 是后者访问第 3 层的最低门槛。

**人工连续方程。** 分母是 `Hz - cffv*pm*pn*(水平输运散度+W层间差)`，并非简单 `1/Hz`。用与 scheme 时间系数一致的有效厚度维持 tracer 常量；首慢步用半步 dt，其余 Gamma=1/2（预测一阶）或 1/6。

**显式扩散。** 读 `nstp` 的层间差，乘 `dt*(1-lambda)*Akt/Δz_r`，上下界分别为 `dt*btflx`、`dt*stflx`；新槽写 `Hz*t(nstp)+FC(k)-FC(k-1)`，诊断同步记录。被动 tracer 使用 `min(NAT,itrc)`。这个写入仍为 m·Tunits，不是最终校正浓度。

**当前限制。** 太阳辐射、nonlocal、波致通量、mask、湿干、年龄组合等在 PRE3D 通过编译拒绝而非被 C++ 隐式处理；点源或不支持 scheme 等走整个 tile 的 Fortran 路径，连同动量预测一起回退。详见第 9 节。

<a id="pre-uv"></a>

## 5. k_pre_step3d_uv：U/V 动量预测

### 5.1 完整原始 Fortran 计算段

外围为文件级 `NONLINEAR && SOLVE3D`，本动量段位于 `TS_FIXED` tracer 条件之外。下块同时保留 U、V、首步/第二步/后续步、BODYFORCE 与全部相关诊断条件；当前 PRE3D 编译门禁排除 BODYFORCE。

<a id="src-original-pre-uv"></a>

**source-block ID：`original-pre-uv`**；commit：`cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`；路径：`ROMS/Nonlinear/pre_step3d.F`；闭区间：**934–1144**；行数：**211**；UTF-8 payload SHA256：`f90b0962dd7d9856c245b0309ad2d8ab01e57ae3b93d10cc3c4e26e6b4398f2f`。

<details>
<summary>ROMS/Nonlinear/pre_step3d.F，934–1144（完整原文，可展开）</summary>

<!-- source-block id=original-pre-uv commit=cf10c0f7cc270a7075ad19ee3b8950be352a7cd0 path=ROMS/Nonlinear/pre_step3d.F lines=934-1144 count=211 sha256=f90b0962dd7d9856c245b0309ad2d8ab01e57ae3b93d10cc3c4e26e6b4398f2f -->
```fortran
!
!=======================================================================
!  3D momentum equation in the XI-direction.
!=======================================================================
!
!  Compute U-component viscous vertical momentum fluxes "FC" at
!  current time-step n, and at horizontal U-points and vertical
!  W-points.
!
      J_LOOP2: DO j=Jstr,Jend
        cff3=dt(ng)*(1.0_r8-lambda)
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            cff=1.0_r8/(z_r(i,j,k+1)+z_r(i-1,j,k+1)-                    &
     &                  z_r(i,j,k  )-z_r(i-1,j,k  ))
            FC(i,k)=cff3*cff*(u(i,j,k+1,nstp)-u(i,j,k,nstp))*           &
     &              (Akv(i,j,k)+Akv(i-1,j,k))
          END DO
        END DO
!
!  Apply bottom and surface stresses, if so is prescribed.
!
        DO i=IstrU,Iend
# ifdef BODYFORCE
          FC(i,0)=0.0_r8
          FC(i,N(ng))=0.0_r8
# else
          FC(i,0)=dt(ng)*bustr(i,j)
          FC(i,N(ng))=dt(ng)*sustr(i,j)
# endif
        END DO
!
!  Compute new U-momentum (m m/s).
!
        cff=dt(ng)*0.25_r8
        DO i=IstrU,Iend
          DC(i,0)=cff*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        END DO
        indx=3-nrhs
        IF (iic(ng).eq.ntfirst(ng)) THEN
          DO k=1,N(ng)
            DO i=IstrU,Iend
              cff1=u(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i-1,j,k))
              cff2=FC(i,k)-FC(i,k-1)
              u(i,j,k,nnew)=cff1+cff2
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M3pgrd
                DiaU3wrk(i,j,k,idiag)=0.0_r8
              END DO
              DiaU3wrk(i,j,k,M3vvis)=cff2
              DiaU3wrk(i,j,k,M3rate)=cff1
# endif
            END DO
          END DO
        ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
          DO k=1,N(ng)
            DO i=IstrU,Iend
              cff1=u(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i-1,j,k))
              cff2=FC(i,k)-FC(i,k-1)
              cff3=0.5_r8*DC(i,0)
              u(i,j,k,nnew)=cff1-                                       &
     &                      cff3*ru(i,j,k,indx)+                        &
     &                      cff2
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M3pgrd
                DiaU3wrk(i,j,k,idiag)=-cff3*DiaRU(i,j,k,indx,idiag)
              END DO
              DiaU3wrk(i,j,k,M3vvis)=cff2
#  ifdef BODYFORCE
              DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)-            &
     &                               cff3*DiaRU(i,j,k,indx,M3vvis)
#  endif
              DiaU3wrk(i,j,k,M3rate)=cff1
# endif
            END DO
          END DO
        ELSE
          cff1= 5.0_r8/12.0_r8
          cff2=16.0_r8/12.0_r8
          DO k=1,N(ng)
            DO i=IstrU,Iend
              cff3=u(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i-1,j,k))
              cff4=FC(i,k)-FC(i,k-1)
              u(i,j,k,nnew)=cff3+                                       &
     &                      DC(i,0)*(cff1*ru(i,j,k,nrhs)-               &
     &                               cff2*ru(i,j,k,indx))+              &
     &                      cff4
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M3pgrd
                DiaU3wrk(i,j,k,idiag)=DC(i,0)*                          &
     &                                (cff1*DiaRU(i,j,k,nrhs,idiag)-    &
     &                                 cff2*DiaRU(i,j,k,indx,idiag))
              END DO
              DiaU3wrk(i,j,k,M3vvis)=cff4
#  ifdef BODYFORCE
              DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)+            &
     &                               DC(i,0)*                           &
     &                               (cff1*DiaRU(i,j,k,nrhs,M3vvis)-    &
     &                                cff2*DiaRU(i,j,k,indx,M3vvis))
#  endif
              DiaU3wrk(i,j,k,M3rate)=cff3
# endif
            END DO
          END DO
        END IF
!
!=======================================================================
!  3D momentum equation in the ETA-direction.
!=======================================================================
!
!  Compute V-component viscous vertical momentum fluxes "FC" at
!  current time-step n, and at horizontal V-points and vertical
!  W-points.
!
        IF (j.ge.JstrV) THEN
          cff3=dt(ng)*(1.0_r8-lambda)
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              cff=1.0_r8/(z_r(i,j,k+1)+z_r(i,j-1,k+1)-                  &
     &                    z_r(i,j,k  )-z_r(i,j-1,k  ))
              FC(i,k)=cff3*cff*(v(i,j,k+1,nstp)-v(i,j,k,nstp))*         &
     &                (Akv(i,j,k)+Akv(i,j-1,k))
            END DO
          END DO
!
!  Apply bottom and surface stresses, if so is prescribed.
!
          DO i=Istr,Iend
# ifdef BODYFORCE
            FC(i,0)=0.0_r8
            FC(i,N(ng))=0.0_r8
# else
            FC(i,0)=dt(ng)*bvstr(i,j)
            FC(i,N(ng))=dt(ng)*svstr(i,j)
# endif
          END DO
!
!  Compute new V-momentum (m m/s).
!
          cff=dt(ng)*0.25_r8
          DO i=Istr,Iend
            DC(i,0)=cff*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
          END DO
          IF (iic(ng).eq.ntfirst(ng)) THEN
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff1=v(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i,j-1,k))
                cff2=FC(i,k)-FC(i,k-1)
                v(i,j,k,nnew)=cff1+cff2
# ifdef DIAGNOSTICS_UV
                DO idiag=1,M3pgrd
                  DiaV3wrk(i,j,k,idiag)=0.0_r8
                END DO
                DiaV3wrk(i,j,k,M3vvis)=cff2
                DiaV3wrk(i,j,k,M3rate)=cff1
# endif
              END DO
            END DO
          ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff1=v(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i,j-1,k))
                cff2=FC(i,k)-FC(i,k-1)
                cff3=0.5_r8*DC(i,0)
                v(i,j,k,nnew)=cff1-                                     &
     &                        cff3*rv(i,j,k,indx)+                      &
     &                        cff2
# ifdef DIAGNOSTICS_UV
                DO idiag=1,M3pgrd
                  DiaV3wrk(i,j,k,idiag)=-cff3*DiaRV(i,j,k,indx,idiag)
                END DO
                DiaV3wrk(i,j,k,M3vvis)=cff2
#  ifdef BODYFORCE
                DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)-          &
     &                                 cff3*DiaRV(i,j,k,indx,M3vvis)
#  endif
                DiaV3wrk(i,j,k,M3rate)=cff1
# endif
              END DO
            END DO
          ELSE
            cff1= 5.0_r8/12.0_r8
            cff2=16.0_r8/12.0_r8
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff3=v(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i,j-1,k))
                cff4=FC(i,k)-FC(i,k-1)
                v(i,j,k,nnew)=cff3+                                     &
     &                        DC(i,0)*(cff1*rv(i,j,k,nrhs)-             &
     &                                 cff2*rv(i,j,k,indx))+            &
     &                        cff4
# ifdef DIAGNOSTICS_UV
                DO idiag=1,M3pgrd
                  DiaV3wrk(i,j,k,idiag)=DC(i,0)*                        &
     &                                  (cff1*DiaRV(i,j,k,nrhs,idiag)-  &
     &                                   cff2*DiaRV(i,j,k,indx,idiag))
                END DO
                DiaV3wrk(i,j,k,M3vvis)=cff4
#  ifdef BODYFORCE
                DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)+          &
     &                                 DC(i,0)*                         &
     &                                 (cff1*DiaRV(i,j,k,nrhs,M3vvis)-  &
     &                                  cff2*DiaRV(i,j,k,indx,M3vvis))
#  endif
                DiaV3wrk(i,j,k,M3rate)=cff3
# endif
              END DO
            END DO
          END IF
        END IF
      END DO J_LOOP2
```

</details>

### 5.2 完整当前 C++ 函数

首行空行是文件原有内容；函数从第 554 行开始。完整 `MomentumFields::row<STEP,V>`、`both<STEP>` 定义见[附录 B](#appendix-cpp)。它们实际包含时间分支、U/V 方向与诊断主计算，外壳本身不是全部。

<a id="src-current-pre-uv"></a>

**source-block ID：`current-pre-uv`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.cpp`；闭区间：**553–614**；行数：**62**；UTF-8 payload SHA256：`d9a34f7e52ea2059772f6706a722efc0dd084bb8313bbe2fd8a4580a5ee5adcf`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.cpp，553–614（完整原文，可展开）</summary>

<!-- source-block id=current-pre-uv commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.cpp lines=553-614 count=62 sha256=d9a34f7e52ea2059772f6706a722efc0dd084bb8313bbe2fd8a4580a5ee5adcf -->
```cpp

void k_pre_step3d_uv(const Pre3dConfig* cfg,
                     void* pm_p, void* pn_p, void* Hz_p, void* z_r_p,
                     void* Akv_p, void* bustr_p, void* bvstr_p,
                     void* sustr_p, void* svstr_p,
                     void* ru_p, void* rv_p, void* u_p, void* v_p,
                     void* DiaU3wrk_p, void* DiaV3wrk_p,
                     void* DiaRU_p, void* DiaRV_p) {
    roms::initialize();
    ROMS_KK_TIMER("pre_step3d momentum");

    MomentumFields f;
    f.c = *cfg;
    const Pre3dConfig &c = f.c;
    const int L0 = c.LBi, L1 = c.UBi, M0 = c.LBj, M1 = c.UBj;
    f.pm = roms::as_2d<double>(pm_p, L0, L1, M0, M1);
    f.pn = roms::as_2d<double>(pn_p, L0, L1, M0, M1);
    f.bustr = roms::as_2d<double>(bustr_p, L0, L1, M0, M1);
    f.bvstr = roms::as_2d<double>(bvstr_p, L0, L1, M0, M1);
    f.sustr = roms::as_2d<double>(sustr_p, L0, L1, M0, M1);
    f.svstr = roms::as_2d<double>(svstr_p, L0, L1, M0, M1);
    f.Hz = roms::as_3d<double>(Hz_p, L0, L1, M0, M1, 1, c.N);
    f.zr = roms::as_3d<double>(z_r_p, L0, L1, M0, M1, 1, c.N);
    f.Akv = roms::as_3d<double>(Akv_p, L0, L1, M0, M1, 0, c.N);
    f.ru = roms::as_4d<double>(ru_p, L0, L1, M0, M1, 0, c.N, 1, 2);
    f.rv = roms::as_4d<double>(rv_p, L0, L1, M0, M1, 0, c.N, 1, 2);
    f.u = roms::as_4d<double>(u_p, L0, L1, M0, M1, 1, c.N, 1, 2);
    f.v = roms::as_4d<double>(v_p, L0, L1, M0, M1, 1, c.N, 1, 2);
#if defined DIAGNOSTICS_UV
    f.DiaU3wrk = roms::as_4d<double>(DiaU3wrk_p, L0, L1, M0, M1, 1, c.N, 1, c.NDM3d);
    f.DiaV3wrk = roms::as_4d<double>(DiaV3wrk_p, L0, L1, M0, M1, 1, c.N, 1, c.NDM3d);
    f.DiaRU = roms::as_5d<double>(DiaRU_p, L0, L1, M0, M1, 1, c.N, 1, 2, 1, c.NDrhs);
    f.DiaRV = roms::as_5d<double>(DiaRV_p, L0, L1, M0, M1, 1, c.N, 1, 2, 1, c.NDrhs);
#else
    (void)DiaU3wrk_p;
    (void)DiaV3wrk_p;
    (void)DiaRU_p;
    (void)DiaRV_p;
#endif

    const int ny = c.Jend - c.Jstr + 1;
    const int per = 2 * (c.Iend - c.Istr + 2);
    double* scratch = nullptr;
#ifndef __sw_slave__
    scratch = workspace.reserve(std::size_t(ny) * per);
#endif
    const int half = per / 2;

    Kokkos::parallel_for(
        "ROMS pre_step3d momentum", range_policy(c.Jstr, c.Jend + 1),
        KOKKOS_LAMBDA(int j) {
            double* fcm = scratch + std::size_t(j - c.Jstr) * per;
            double* dc0 = fcm + half;
            if (c.step == 0) {
                f.both<0>(j, fcm, dc0);
            } else if (c.step == 1) {
                f.both<1>(j, fcm, dc0);
            } else {
                f.both<2>(j, fcm, dc0);
            }
        });
}
```

</details>

### 5.3 物理与实现解释

**读写。** 读 `u/v(nstp)`、`ru/rv(nrhs)` 和 `ru/rv(3-nrhs)`、`Hz/z_r/Akv`、上下运动学应力；写 `u/v(nnew)` 的厚度加权动量预测量与可选三维诊断。

**原版多趟。** 每 j 行先构造 U 的所有 FC(k) 及上下界、度量 DC(i,0)，再根据启动阶段更新各层；之后在 `j>=JstrV` 的范围重复真实 V 段。当前模板以 `(di,dj)=(1,0)`、`(0,1)` 选择相邻 rho 点与对应的速度/RHS/应力/诊断数组，并保留 U 起点 IstrU、V 起点 Istr 与 V 的行条件。

**实际没有 32 列块。** 每工作项一行，先整行初始化 `dc0` 和底界 `fcm=dt*bstr`，再 k 自底向顶、i 连续；当前 FC(k) 当场计算，上一界面 FC(k-1) 由每列 `fcm` 携带，最后一层取 `dt*sstr`。于是去掉整行×层数的 FC 存储，scratch 仅两条行数组，而保留层间通量差顺序。U 后 V 重用同一分片，没有同时写同一个物理场。

**三种慢步时间类。** STEP=0：`velocity_old*平均Hz + 通量差`；STEP=1：再减 `0.5*DC0*另一个RHS槽`；后续步：加 `DC0*((5/12)*nrhs-(16/12)*另一个槽)` 再加通量差。solve 随后再加本轮 RHS 的 1、3/2 或 23/12 项，二者共同构成原时间推进，不能只看某一个入口便误称缺少本轮 RHS。

**数值/诊断。** fc 的垂向梯度使用两相邻 rho 柱的 z_r 间距与 Akv 和，显式部分系数 `dt*(1-lambda)`。每时间类同步写 `DiaU3wrk/DiaV3wrk` 的 pressure-range 历史项、vvis 与 rate。`row<STEP,V>` 是真实编译期模板；选择 STEP 的 if 仍在每个 j 工作项内部，不是 host 全局先选 launch。计算属于只读邻柱系数的列局部递推/时间历史更新，不是全局 stencil 迭代或隐式求解。

<a id="uv-solve"></a>

## 6. k_step3d_uv_solve：隐式垂向黏性与内部均值替换

### 6.1 完整原始 Fortran 计算段

外围为 `NONLINEAR && SOLVE3D`。原版第一行循环完整收录两方向，包括 SPLINES_VVISC 与非 spline、OMEGA_IMPLICIT、WEC、mask/湿干、BODYFORCE 诊断及均值替换的所有 CPP 上下文。当前 STEP3DUV 对其中多种组合明确编译拒绝，不能把下块宽范围当成已支持物理。

<a id="src-original-uv-solve"></a>

**source-block ID：`original-uv-solve`**；commit：`cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`；路径：`ROMS/Nonlinear/step3d_uv.F`；闭区间：**325–1260**；行数：**936**；UTF-8 payload SHA256：`548c7719c60e68fbfb6ad3201a3afe00ee10ea9a8daab873bc501dc7393a7401`。

<details>
<summary>ROMS/Nonlinear/step3d_uv.F，325–1260（完整原文，可展开）</summary>

<!-- source-block id=original-uv-solve commit=cf10c0f7cc270a7075ad19ee3b8950be352a7cd0 path=ROMS/Nonlinear/step3d_uv.F lines=325-1260 count=936 sha256=548c7719c60e68fbfb6ad3201a3afe00ee10ea9a8daab873bc501dc7393a7401 -->
```fortran
!
!-----------------------------------------------------------------------
!  Time step momentum equation in the XI-direction.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          AK(i,0)=0.5_r8*(Akv(i-1,j,0)+                                 &
     &                    Akv(i  ,j,0))
          DO k=1,N(ng)
            AK(i,k)=0.5_r8*(Akv(i-1,j,k)+                               &
     &                      Akv(i  ,j,k))
            Hzk(i,k)=0.5_r8*(Hz(i-1,j,k)+                               &
     &                       Hz(i  ,j,k))
# if defined SPLINES_VVISC || defined DIAGNOSTICS_UV
            oHz(i,k)=1.0_r8/Hzk(i,k)
# endif
          END DO
        END DO
!
!  Time step right-hand-side terms.
!
        IF (iic(ng).eq.ntfirst(ng)) THEN
          cff=0.25_r8*dt(ng)
        ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
          cff=0.25_r8*dt(ng)*3.0_r8/2.0_r8
        ELSE
          cff=0.25_r8*dt(ng)*23.0_r8/12.0_r8
        END IF
        DO i=IstrU,Iend
          DC(i,0)=cff*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        END DO
        DO k=1,N(ng)
          DO i=IstrU,Iend
            u(i,j,k,nnew)=u(i,j,k,nnew)+                                &
     &                    DC(i,0)*ru(i,j,k,nrhs)
# ifdef SPLINES_VVISC
            u(i,j,k,nnew)=u(i,j,k,nnew)*oHz(i,k)
# endif
# ifdef DIAGNOSTICS_UV
            DO idiag=1,M3pgrd
              DiaU3wrk(i,j,k,idiag)=(DiaU3wrk(i,j,k,idiag)+             &
     &                               DC(i,0)*DiaRU(i,j,k,nrhs,idiag))*  &
     &                              oHz(i,k)
            END DO
#  if defined UV_VIS2 || defined UV_VIS4
            DiaU3wrk(i,j,k,M3xvis)=DiaU3wrk(i,j,k,M3xvis)*oHz(i,k)
            DiaU3wrk(i,j,k,M3yvis)=DiaU3wrk(i,j,k,M3yvis)*oHz(i,k)
            DiaU3wrk(i,j,k,M3hvis)=DiaU3wrk(i,j,k,M3hvis)*oHz(i,k)
#  endif
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)*oHz(i,k)
#  ifdef BODYFORCE
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)+              &
     &                             DC(i,0)*DiaRU(i,j,k,nrhs,M3vvis)*    &
     &                             oHz(i,k)
#  endif
            DiaU3wrk(i,j,k,M3rate)=DiaU3wrk(i,j,k,M3rate)*oHz(i,k)
# endif
          END DO
        END DO

# ifdef SPLINES_VVISC
!
!  Use conservative, parabolic spline reconstruction of vertical
!  viscosity derivatives.  Then, time step vertical viscosity term
!  implicitly by solving a tridiagonal system.
!
        cff1=1.0_r8/6.0_r8
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            FC(i,k)=cff1*Hzk(i,k  )-dt(ng)*AK(i,k-1)*oHz(i,k  )
            CF(i,k)=cff1*Hzk(i,k+1)-dt(ng)*AK(i,k+1)*oHz(i,k+1)
          END DO
        END DO
        DO i=IstrU,Iend
          CF(i,0)=0.0_r8
          DC(i,0)=0.0_r8
        END DO
!
!  LU decomposition and forward substitution.
!
        cff1=1.0_r8/3.0_r8
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            BC(i,k)=cff1*(Hzk(i,k)+Hzk(i,k+1))+                         &
     &              dt(ng)*AK(i,k)*(oHz(i,k)+oHz(i,k+1))
            cff=1.0_r8/(BC(i,k)-FC(i,k)*CF(i,k-1))
            CF(i,k)=cff*CF(i,k)
            DC(i,k)=cff*(u(i,j,k+1,nnew)-u(i,j,k,nnew)-                 &
     &                   FC(i,k)*DC(i,k-1))
          END DO
        END DO
!
!  Backward substitution.
!
        DO i=IstrU,Iend
          DC(i,N(ng))=0.0_r8
        END DO
        DO k=N(ng)-1,1,-1
          DO i=IstrU,Iend
            DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
          END DO
        END DO
!
        DO k=1,N(ng)
          DO i=IstrU,Iend
            DC(i,k)=DC(i,k)*AK(i,k)
            cff=dt(ng)*oHz(i,k)*(DC(i,k)-DC(i,k-1))
            u(i,j,k,nnew)=u(i,j,k,nnew)+cff
#  ifdef DIAGNOSTICS_UV
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)+cff
#  endif
          END DO
        END DO
# else
!
!  Compute off-diagonal coefficients [lambda*dt*Akv/Hz] for the
!  implicit vertical viscosity term at horizontal U-points and
!  vertical W-points.
!
        cff=-lambda*dt(ng)/0.5_r8
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            cff1=1.0_r8/(z_r(i,j,k+1)+z_r(i-1,j,k+1)-                   &
     &                   z_r(i,j,k  )-z_r(i-1,j,k  ))
            FC(i,k)=cff*cff1*AK(i,k)
          END DO
        END DO
        DO i=IstrU,Iend
          FC(i,0)=0.0_r8
          FC(i,N(ng))=0.0_r8
        END DO
!
!  Solve the tridiagonal system.
!
        DO k=1,N(ng)
          DO i=IstrU,Iend
            DC(i,k)=u(i,j,k,nnew)
            BC(i,k)=Hzk(i,k)-FC(i,k)-FC(i,k-1)
          END DO
        END DO
        DO i=IstrU,Iend
          cff=1.0_r8/BC(i,1)
          CF(i,1)=cff*FC(i,1)
          DC(i,1)=cff*DC(i,1)
        END DO
        DO k=2,N(ng)-1
          DO i=IstrU,Iend
            cff=1.0_r8/(BC(i,k)-FC(i,k-1)*CF(i,k-1))
            CF(i,k)=cff*FC(i,k)
            DC(i,k)=cff*(DC(i,k)-FC(i,k-1)*DC(i,k-1))
          END DO
        END DO
!
!  Compute new solution by back substitution.
!
        DO i=IstrU,Iend
#  ifdef DIAGNOSTICS_UV
          wrk(i,N(ng))=u(i,j,N(ng),nnew)*oHz(i,N(ng))
#  endif
          DC(i,N(ng))=(DC(i,N(ng))-FC(i,N(ng)-1)*DC(i,N(ng)-1))/        &
     &                (BC(i,N(ng))-FC(i,N(ng)-1)*CF(i,N(ng)-1))
          u(i,j,N(ng),nnew)=DC(i,N(ng))
#  ifdef DIAGNOSTICS_UV
          DiaU3wrk(i,j,N(ng),M3vvis)=DiaU3wrk(i,j,N(ng),M3vvis)+        &
     &                               u(i,j,N(ng),nnew)-wrk(i,N(ng))
#  endif
        END DO
        DO k=N(ng)-1,1,-1
          DO i=IstrU,Iend
#  ifdef DIAGNOSTICS_UV
            wrk(i,k)=u(i,j,k,nnew)*oHz(i,k)
#  endif
            DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
            u(i,j,k,nnew)=DC(i,k)
#  ifdef DIAGNOSTICS_UV
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)+              &
     &                             u(i,j,k,nnew)-wrk(i,k)
#  endif
          END DO
        END DO
# endif
# ifdef OMEGA_IMPLICIT
!
!  Adaptive, Courant-number based implicit vertical advection
!  contribution for u-momentum.
!
        DO i=IstrU,Iend
          WK(i,0)=0.5_r8*(Wi(i-1,j,0)+                                  &
     &                    Wi(i  ,j,0))
          DO k=1,N(ng)
            WK(i,k)=0.5_r8*(Wi(i-1,j,k)+                                &
     &                      Wi(i  ,j,k))
            Hzk(i,k)=0.5_r8*(Hz(i-1,j,k)+                               &
     &                       Hz(i  ,j,k))
          END DO
        END DO
!
!  Compute off-diagonal coefficients [dt*Wi*pm*pn] for the
!  implicit vertical viscosity term at horizontal U-points and
!  vertical W-points.
!
        cff=dt(ng)
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            cff1=cff/(on_u(i,j)*om_u(i,j))
            FCmax(i,k)=MAX(WK(i,k),0.0_r8)*cff1
            FCmin(i,k)=MIN(WK(i,k),0.0_r8)*cff1
          END DO
        END DO
        DO i=IstrU,Iend
          FCmax(i,0)=0.0_r8
          FCmin(i,0)=0.0_r8
          FCmax(i,N(ng))=0.0_r8
          FCmin(i,N(ng))=0.0_r8
        END DO
!
!  Solve the tridiagonal system.
!
        DO k=1,N(ng)
          DO i=IstrU,Iend
            BC(i,k)=Hzk(i,k)+FCmax(i,k)-FCmin(i,k-1)
            DC(i,k)=u(i,j,k,nnew)*Hzk(i,k)
          END DO
        END DO
        DO i=IstrU,Iend
          cff=1.0_r8/BC(i,1)
          CF(i,1)=cff*FCmin(i,1)
          DC(i,1)=cff*DC(i,1)
        END DO
        DO k=2,N(ng)-1
          DO i=IstrU,Iend
            cff=1.0_r8/(BC(i,k)+FCmax(i,k-1)*CF(i,k-1))
            CF(i,k)=cff*FCmin(i,k)
            DC(i,k)=cff*(DC(i,k)+FCmax(i,k-1)*DC(i,k-1))
          END DO
        END DO
!
!  Compute new solution by back substitution.
!
        DO i=IstrU,Iend
#  ifdef DIAGNOSTICS_UV
          cff1=u(i,j,N(ng),nnew)
#  endif
          cff=1.0_r8/(BC(i,N(ng))+FCmax(i,N(ng)-1)*CF(i,N(ng)-1))
          DC(i,N(ng))=cff*(DC(i,N(ng))+                                 &
     &                     FCmax(i,N(ng)-1)*DC(i,N(ng)-1))
          u(i,j,N(ng),nnew)=DC(i,N(ng))
#  ifdef DIAGNOSTICS_UV
          DiaRU(i,j,N(ng),nrhs,M3vadv)=DiaRU(i,j,N(ng),nrhs,M3vadv)+    &
     &                                 u(i,j,N(ng),nnew)-cff1
#  endif
        END DO
!
        DO k=N(ng)-1,1,-1
          DO i=IstrU,Iend
#  ifdef DIAGNOSTICS_UV
            cff1=u(i,j,k,nnew)
#  endif
            DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
            u(i,j,k,nnew)=DC(i,k)
#  ifdef DIAGNOSTICS_UV
            DiaRU(i,j,k,nrhs,M3vadv)=DiaRU(i,j,k,nrhs,M3vadv)+          &
     &                               u(i,j,k,nnew)-cff1
#  endif
          END DO
        END DO
# endif
!
!  Replace INTERIOR POINTS incorrect vertical mean with more accurate
!  barotropic component, ubar=DU_avg1/(D*on_u). Recall that, D=CF(:,0).
!
        DO i=IstrU,Iend
          CF(i,0)=Hzk(i,1)
          DC(i,0)=u(i,j,1,nnew)*Hzk(i,1)
# ifdef WEC
          DCs(i,0)=u_stokes(i,j,1)*Hzk(i,1)
# endif
# ifdef DIAGNOSTICS_UV
          Dwrk(i,M2pgrd)=DiaU3wrk(i,j,1,M3pgrd)*Hzk(i,1)
          Dwrk(i,M2bstr)=DiaU3wrk(i,j,1,M3vvis)*Hzk(i,1)
#  ifdef UV_COR
          Dwrk(i,M2fcor)=DiaU3wrk(i,j,1,M3fcor)*Hzk(i,1)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
          Dwrk(i,M2xvis)=DiaU3wrk(i,j,1,M3xvis)*Hzk(i,1)
          Dwrk(i,M2yvis)=DiaU3wrk(i,j,1,M3yvis)*Hzk(i,1)
          Dwrk(i,M2hvis)=DiaU3wrk(i,j,1,M3hvis)*Hzk(i,1)
#  endif
#  ifdef UV_ADV
          Dwrk(i,M2xadv)=DiaU3wrk(i,j,1,M3xadv)*Hzk(i,1)
          Dwrk(i,M2yadv)=DiaU3wrk(i,j,1,M3yadv)*Hzk(i,1)
          Dwrk(i,M2hadv)=DiaU3wrk(i,j,1,M3hadv)*Hzk(i,1)
#  endif
#  ifdef WEC_VF
          Dwrk(i,M2hjvf)=DiaU3wrk(i,j,1,M3hjvf)*Hzk(i,1)
          Dwrk(i,M2kvrf)=DiaU3wrk(i,j,1,M3kvrf)*Hzk(i,1)
#   ifdef UV_COR
          Dwrk(i,M2fsco)=DiaU3wrk(i,j,1,M3fsco)*Hzk(i,1)
#   endif
#   ifdef BOTTOM_STREAMING
          Dwrk(i,M2bstm)=DiaU3wrk(i,j,1,M3bstm)*Hzk(i,1)
#   endif
#   ifdef SURFACE_STREAMING
          Dwrk(i,M2sstm)=DiaU3wrk(i,j,1,M3sstm)*Hzk(i,1)
#   endif
          Dwrk(i,M2wrol)=DiaU3wrk(i,j,1,M3wrol)*Hzk(i,1)
          Dwrk(i,M2wbrk)=DiaU3wrk(i,j,1,M3wbrk)*Hzk(i,1)
#  endif
# endif
        END DO
        DO k=2,N(ng)
          DO i=IstrU,Iend
            CF(i,0)=CF(i,0)+Hzk(i,k)
            DC(i,0)=DC(i,0)+u(i,j,k,nnew)*Hzk(i,k)
# ifdef WEC
            DCs(i,0)=DCs(i,0)+u_stokes(i,j,k)*Hzk(i,k)
# endif
# ifdef DIAGNOSTICS_UV
            Dwrk(i,M2pgrd)=Dwrk(i,M2pgrd)+                              &
     &                     DiaU3wrk(i,j,k,M3pgrd)*Hzk(i,k)
            Dwrk(i,M2bstr)=Dwrk(i,M2bstr)+                              &
     &                     DiaU3wrk(i,j,k,M3vvis)*Hzk(i,k)
#  ifdef UV_COR
            Dwrk(i,M2fcor)=Dwrk(i,M2fcor)+                              &
     &                     DiaU3wrk(i,j,k,M3fcor)*Hzk(i,k)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
            Dwrk(i,M2xvis)=Dwrk(i,M2xvis)+                              &
     &                     DiaU3wrk(i,j,k,M3xvis)*Hzk(i,k)
            Dwrk(i,M2yvis)=Dwrk(i,M2yvis)+                              &
     &                     DiaU3wrk(i,j,k,M3yvis)*Hzk(i,k)
            Dwrk(i,M2hvis)=Dwrk(i,M2hvis)+                              &
     &                     DiaU3wrk(i,j,k,M3hvis)*Hzk(i,k)
#  endif
#  ifdef UV_ADV
            Dwrk(i,M2xadv)=Dwrk(i,M2xadv)+                              &
     &                     DiaU3wrk(i,j,k,M3xadv)*Hzk(i,k)
            Dwrk(i,M2yadv)=Dwrk(i,M2yadv)+                              &
     &                     DiaU3wrk(i,j,k,M3yadv)*Hzk(i,k)
            Dwrk(i,M2hadv)=Dwrk(i,M2hadv)+                              &
     &                     DiaU3wrk(i,j,k,M3hadv)*Hzk(i,k)
#  endif
#  ifdef WEC_VF
            Dwrk(i,M2hjvf)=Dwrk(i,M2hjvf)+                              &
     &                     DiaU3wrk(i,j,k,M3hjvf)*Hzk(i,k)
            Dwrk(i,M2kvrf)=Dwrk(i,M2kvrf)+                              &
     &                     DiaU3wrk(i,j,k,M3kvrf)*Hzk(i,k)
#   ifdef UV_COR
            Dwrk(i,M2fsco)=Dwrk(i,M2fsco)+                              &
     &                     DiaU3wrk(i,j,k,M3fsco)*Hzk(i,k)
#   endif
#   ifdef BOTTOM_STREAMING
            Dwrk(i,M2bstm)=Dwrk(i,M2bstm)+                              &
     &                     DiaU3wrk(i,j,k,M3bstm)*Hzk(i,k)
#   endif
#   ifdef SURFACE_STREAMING
            Dwrk(i,M2sstm)=Dwrk(i,M2sstm)+                              &
     &                     DiaU3wrk(i,j,k,M3sstm)*Hzk(i,k)
#   endif
            Dwrk(i,M2wrol)=Dwrk(i,M2wrol)+                              &
     &                     DiaU3wrk(i,j,k,M3wrol)*Hzk(i,k)
            Dwrk(i,M2wbrk)=Dwrk(i,M2wbrk)+                              &
     &                     DiaU3wrk(i,j,k,M3wbrk)*Hzk(i,k)
#  endif
# endif
          END DO
        END DO
        DO i=IstrU,Iend
          cff1=1.0_r8/(CF(i,0)*on_u(i,j))
          DC(i,0)=(DC(i,0)*on_u(i,j)-DU_avg1(i,j))*cff1      ! recursive
# ifdef WEC
          cff2=1.0_r8/CF(i,0)
          DCs(i,0)=DCs(i,0)*cff2-ubar_stokes(i,j)            ! recursive
# endif
# ifdef DIAGNOSTICS_UV
          DO idiag=1,M2pgrd
            Dwrk(i,idiag)=(Dwrk(i,idiag)*on_u(i,j)-                     &
     &                     DiaU2wrk(i,j,idiag))*cff1
          END DO
          Dwrk(i,M2bstr)=(Dwrk(i,M2bstr)*on_u(i,j)-                     &
     &                    DiaU2wrk(i,j,M2bstr)-DiaU2wrk(i,j,M2sstr))*   &
     &                   cff1
# endif
        END DO
!
!  Couple and update new solution.
!
        DO k=1,N(ng)
          DO i=IstrU,Iend
            u(i,j,k,nnew)=u(i,j,k,nnew)-DC(i,0)
# ifdef MASKING
            u(i,j,k,nnew)=u(i,j,k,nnew)*umask(i,j)
# endif
# ifdef WET_DRY
            u(i,j,k,nnew)=u(i,j,k,nnew)*umask_wet(i,j)
            ru(i,j,k,nrhs)=ru(i,j,k,nrhs)*umask_wet(i,j)
# endif
# ifdef WEC
            u_stokes(i,j,k)=u_stokes(i,j,k)-DCs(i,0)
#  ifdef MASKING
            u_stokes(i,j,k)=u_stokes(i,j,k)*umask(i,j)
#  endif
#  ifdef WET_DRY
            u_stokes(i,j,k)=u_stokes(i,j,k)*umask_wet(i,j)
#  endif
# endif
# ifdef DIAGNOSTICS_UV
            DiaU3wrk(i,j,k,M3pgrd)=DiaU3wrk(i,j,k,M3pgrd)-              &
     &                             Dwrk(i,M2pgrd)
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)-              &
     &                             Dwrk(i,M2bstr)
#  ifdef UV_COR
            DiaU3wrk(i,j,k,M3fcor)=DiaU3wrk(i,j,k,M3fcor)-              &
     &                             Dwrk(i,M2fcor)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
            DiaU3wrk(i,j,k,M3xvis)=DiaU3wrk(i,j,k,M3xvis)-              &
     &                             Dwrk(i,M2xvis)
            DiaU3wrk(i,j,k,M3yvis)=DiaU3wrk(i,j,k,M3yvis)-              &
     &                             Dwrk(i,M2yvis)
            DiaU3wrk(i,j,k,M3hvis)=DiaU3wrk(i,j,k,M3hvis)-              &
     &                             Dwrk(i,M2hvis)
#  endif
#  ifdef UV_ADV
            DiaU3wrk(i,j,k,M3xadv)=DiaU3wrk(i,j,k,M3xadv)-              &
     &                             Dwrk(i,M2xadv)
            DiaU3wrk(i,j,k,M3yadv)=DiaU3wrk(i,j,k,M3yadv)-              &
     &                             Dwrk(i,M2yadv)
            DiaU3wrk(i,j,k,M3hadv)=DiaU3wrk(i,j,k,M3hadv)-              &
     &                             Dwrk(i,M2hadv)
#  endif
#  ifdef WEC_VF
            DiaU3wrk(i,j,k,M3hjvf)=DiaU3wrk(i,j,k,M3hjvf)-              &
     &                             Dwrk(i,M2hjvf)
            DiaU3wrk(i,j,k,M3kvrf)=DiaU3wrk(i,j,k,M3kvrf)-              &
     &                             Dwrk(i,M2kvrf)
#   ifdef UV_COR
            DiaU3wrk(i,j,k,M3fsco)=DiaU3wrk(i,j,k,M3fsco)-              &
     &                             Dwrk(i,M2fsco)
#   endif
#   ifdef BOTTOM_STREAMING
            DiaU3wrk(i,j,k,M3bstm)=DiaU3wrk(i,j,k,M3bstm)-              &
     &                             Dwrk(i,M2bstm)
#   endif
#   ifdef SURFACE_STREAMING
            DiaU3wrk(i,j,k,M3sstm)=DiaU3wrk(i,j,k,M3sstm)-              &
     &                             Dwrk(i,M2sstm)
#   endif
            DiaU3wrk(i,j,k,M3wrol)=DiaU3wrk(i,j,k,M3wrol)-              &
     &                             Dwrk(i,M2wrol)
            DiaU3wrk(i,j,k,M3wbrk)=DiaU3wrk(i,j,k,M3wbrk)-              &
     &                             Dwrk(i,M2wbrk)
#  endif
# endif
          END DO
        END DO

# if defined DIAGNOSTICS_UV && defined MASKING
        DO k=1,N(ng)
          DO i=IstrU,Iend
            DO idiag=1,NDM3d
              DiaU3wrk(i,j,k,idiag)=DiaU3wrk(i,j,k,idiag)*umask(i,j)
            END DO
          END DO
        END DO
# endif
!
!-----------------------------------------------------------------------
!  Time step momentum equation in the ETA-direction.
!-----------------------------------------------------------------------
!
        IF (j.ge.JstrV) THEN
          DO i=Istr,Iend
            AK(i,0)=0.5_r8*(Akv(i,j-1,0)+                               &
     &                      Akv(i,j  ,0))
            DO k=1,N(ng)
              AK(i,k)=0.5_r8*(Akv(i,j-1,k)+                             &
     &                        Akv(i,j  ,k))
              Hzk(i,k)=0.5_r8*(Hz(i,j-1,k)+                             &
     &                         Hz(i,j  ,k))
# if defined SPLINES_VVISC || defined DIAGNOSTICS_UV
              oHz(i,k)=1.0_r8/Hzk(i,k)
# endif
            END DO
          END DO
!
!  Time step right-hand-side terms.
!
          IF (iic(ng).eq.ntfirst(ng)) THEN
            cff=0.25_r8*dt(ng)
          ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
            cff=0.25_r8*dt(ng)*3.0_r8/2.0_r8
          ELSE
            cff=0.25_r8*dt(ng)*23.0_r8/12.0_r8
          END IF
          DO i=Istr,Iend
            DC(i,0)=cff*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
          END DO
          DO k=1,N(ng)
            DO i=Istr,Iend
              v(i,j,k,nnew)=v(i,j,k,nnew)+DC(i,0)*rv(i,j,k,nrhs)
# ifdef SPLINES_VVISC
              v(i,j,k,nnew)=v(i,j,k,nnew)*oHz(i,k)
# endif
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M3pgrd
                DiaV3wrk(i,j,k,idiag)=(DiaV3wrk(i,j,k,idiag)+           &
     &                                 DC(i,0)*                         &
     &                                 DiaRV(i,j,k,nrhs,idiag))*        &
     &                                oHz(i,k)
              END DO
#  if defined UV_VIS2 || defined UV_VIS4
              DiaV3wrk(i,j,k,M3xvis)=DiaV3wrk(i,j,k,M3xvis)*oHz(i,k)
              DiaV3wrk(i,j,k,M3yvis)=DiaV3wrk(i,j,k,M3yvis)*oHz(i,k)
              DiaV3wrk(i,j,k,M3hvis)=DiaV3wrk(i,j,k,M3hvis)*oHz(i,k)
#  endif
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)*oHz(i,k)
#  ifdef BODYFORCE
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)+            &
     &                               DC(i,0)*DiaRV(i,j,k,nrhs,M3vvis)*  &
     &                               oHz(i,k)
#  endif
              DiaV3wrk(i,j,k,M3rate)=DiaV3wrk(i,j,k,M3rate)*oHz(i,k)
# endif
            END DO
          END DO

# ifdef SPLINES_VVISC
!
!  Use conservative, parabolic spline reconstruction of vertical
!  viscosity derivatives.  Then, time step vertical viscosity term
!  implicitly by solving a tridiagonal system.
!
          cff1=1.0_r8/6.0_r8
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              FC(i,k)=cff1*Hzk(i,k  )-dt(ng)*AK(i,k-1)*oHz(i,k  )
              CF(i,k)=cff1*Hzk(i,k+1)-dt(ng)*AK(i,k+1)*oHz(i,k+1)
            END DO
          END DO
          DO i=Istr,Iend
            CF(i,0)=0.0_r8
            DC(i,0)=0.0_r8
          END DO
!
!  LU decomposition and forward substitution.
!
          cff1=1.0_r8/3.0_r8
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              BC(i,k)=cff1*(Hzk(i,k)+Hzk(i,k+1))+                       &
     &                dt(ng)*AK(i,k)*(oHz(i,k)+oHz(i,k+1))
              cff=1.0_r8/(BC(i,k)-FC(i,k)*CF(i,k-1))
              CF(i,k)=cff*CF(i,k)
              DC(i,k)=cff*(v(i,j,k+1,nnew)-v(i,j,k,nnew)-               &
     &                     FC(i,k)*DC(i,k-1))
            END DO
          END DO
!
!  Backward substitution.
!
          DO i=Istr,Iend
            DC(i,N(ng))=0.0_r8
          END DO
          DO k=N(ng)-1,1,-1
            DO i=Istr,Iend
              DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
            END DO
          END DO
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              DC(i,k)=DC(i,k)*AK(i,k)
              cff=dt(ng)*oHz(i,k)*(DC(i,k)-DC(i,k-1))
              v(i,j,k,nnew)=v(i,j,k,nnew)+cff
#  ifdef DIAGNOSTICS_UV
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)+cff
#  endif
            END DO
          END DO
# else
!
!  Compute off-diagonal coefficients [lambda*dt*Akv/Hz] for the
!  implicit vertical viscosity term at horizontal V-points and
!  vertical W-points.
!
          cff=-lambda*dt(ng)/0.5_r8
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              cff1=1.0_r8/(z_r(i,j,k+1)+z_r(i,j-1,k+1)-                 &
     &                     z_r(i,j,k  )-z_r(i,j-1,k  ))
              FC(i,k)=cff*cff1*AK(i,k)
            END DO
          END DO
          DO i=Istr,Iend
            FC(i,0)=0.0_r8
            FC(i,N(ng))=0.0_r8
          END DO
!
!  Solve the tridiagonal system.
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              DC(i,k)=v(i,j,k,nnew)
              BC(i,k)=Hzk(i,k)-FC(i,k)-FC(i,k-1)
            END DO
          END DO
          DO i=Istr,Iend
            cff=1.0_r8/BC(i,1)
            CF(i,1)=cff*FC(i,1)
            DC(i,1)=cff*DC(i,1)
          END DO
          DO k=2,N(ng)-1
            DO i=Istr,Iend
              cff=1.0_r8/(BC(i,k)-FC(i,k-1)*CF(i,k-1))
              CF(i,k)=cff*FC(i,k)
              DC(i,k)=cff*(DC(i,k)-FC(i,k-1)*DC(i,k-1))
            END DO
          END DO
!
!  Compute new solution by back substitution.
!
          DO i=Istr,Iend
#  ifdef DIAGNOSTICS_UV
            wrk(i,N(ng))=v(i,j,N(ng),nnew)*oHz(i,N(ng))
#  endif
            DC(i,N(ng))=(DC(i,N(ng))-FC(i,N(ng)-1)*DC(i,N(ng)-1))/      &
     &                  (BC(i,N(ng))-FC(i,N(ng)-1)*CF(i,N(ng)-1))
            v(i,j,N(ng),nnew)=DC(i,N(ng))
#  ifdef DIAGNOSTICS_UV
            DiaV3wrk(i,j,N(ng),M3vvis)=DiaV3wrk(i,j,N(ng),M3vvis)+      &
     &                                 v(i,j,N(ng),nnew)-wrk(i,N(ng))
#  endif
          END DO
          DO k=N(ng)-1,1,-1
            DO i=Istr,Iend
#  ifdef DIAGNOSTICS_UV
              wrk(i,k)=v(i,j,k,nnew)*oHz(i,k)
#  endif
              DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
              v(i,j,k,nnew)=DC(i,k)
#  ifdef DIAGNOSTICS_UV
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)+            &
     &                               v(i,j,k,nnew)-wrk(i,k)
#  endif
            END DO
          END DO
# endif
# ifdef OMEGA_IMPLICIT
!
!  Adaptive, Courant-number based implicit vertical advection
!  contribution for v-momentum.
!
          DO i=Istr,Iend
            WK(i,0)=0.5_r8*(Wi(i,j-1,0)+                                &
     &                      Wi(i,j  ,0))
            DO k=1,N(ng)
              WK(i,k)=0.5_r8*(Wi(i,j-1,k)+                              &
     &                        Wi(i,j  ,k))
              Hzk(i,k)=0.5_r8*(Hz(i,j-1,k)+                             &
     &                         Hz(i,j  ,k))
            END DO
          END DO
!
!  Compute off-diagonal coefficients [dt*Wi*pm*pn] for the
!  implicit vertical viscosity term at horizontal V-points and
!  vertical W-points.
!
          cff=dt(ng)
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              cff1=cff/(on_v(i,j)*om_v(i,j))
              FCmax(i,k)=MAX(WK(i,k),0.0_r8)*cff1
              FCmin(i,k)=MIN(WK(i,k),0.0_r8)*cff1
            END DO
          END DO
          DO i=Istr,Iend
            FCmax(i,0)=0.0_r8
            FCmin(i,0)=0.0_r8
            FCmax(i,N(ng))=0.0_r8
            FCmin(i,N(ng))=0.0_r8
          END DO
!
!  Solve the tridiagonal system.
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              BC(i,k)=Hzk(i,k)+FCmax(i,k)-FCmin(i,k-1)
              DC(i,k)=v(i,j,k,nnew)*Hzk(i,k)
            END DO
          END DO
          DO i=Istr,Iend
            cff=1.0_r8/BC(i,1)
            CF(i,1)=cff*FCmin(i,1)
            DC(i,1)=cff*DC(i,1)
          END DO
          DO k=2,N(ng)-1
            DO i=Istr,Iend
              cff=1.0_r8/(BC(i,k)+FCmax(i,k-1)*CF(i,k-1))
              CF(i,k)=cff*FCmin(i,k)
              DC(i,k)=cff*(DC(i,k)+FCmax(i,k-1)*DC(i,k-1))
            END DO
          END DO
!
!  Compute new solution by back substitution.
!
          DO i=Istr,Iend
#  ifdef DIAGNOSTICS_UV
            cff1=v(i,j,N(ng),nnew)
#  endif
            cff=1.0_r8/(BC(i,N(ng))+FCmax(i,N(ng)-1)*CF(i,N(ng)-1))
            DC(i,N(ng))=cff*(DC(i,N(ng))+                               &
     &                       FCmax(i,N(ng)-1)*DC(i,N(ng)-1))
            v(i,j,N(ng),nnew)=DC(i,N(ng))
#  ifdef DIAGNOSTICS_UV
            DiaRV(i,j,N(ng),nrhs,M3vadv)=DiaRV(i,j,N(ng),nrhs,M3vadv)+  &
     &                                   v(i,j,N(ng),nnew)-cff1
#  endif
          END DO
!
          DO k=N(ng)-1,1,-1
            DO i=Istr,Iend
#  ifdef DIAGNOSTICS_UV
              cff1=v(i,j,k,nnew)
#  endif
              DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
              v(i,j,k,nnew)=DC(i,k)
#  ifdef DIAGNOSTICS_UV
              DiaRV(i,j,k,nrhs,M3vadv)=DiaRV(i,j,k,nrhs,M3vadv)+        &
     &                                 v(i,j,k,nnew)-cff1
#  endif
            END DO
          END DO
# endif
!
!  Replace INTERIOR POINTS incorrect vertical mean with more accurate
!  barotropic component, vbar=DV_avg1/(D*om_v). Recall that, D=CF(:,0).
!
          DO i=Istr,Iend
            CF(i,0)=Hzk(i,1)
            DC(i,0)=v(i,j,1,nnew)*Hzk(i,1)
# ifdef WEC
            DCs(i,0)=v_stokes(i,j,1)*Hzk(i,1)
# endif
# ifdef DIAGNOSTICS_UV
            Dwrk(i,M2pgrd)=DiaV3wrk(i,j,1,M3pgrd)*Hzk(i,1)
            Dwrk(i,M2bstr)=DiaV3wrk(i,j,1,M3vvis)*Hzk(i,1)
#  ifdef UV_COR
            Dwrk(i,M2fcor)=DiaV3wrk(i,j,1,M3fcor)*Hzk(i,1)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
            Dwrk(i,M2xvis)=DiaV3wrk(i,j,1,M3xvis)*Hzk(i,1)
            Dwrk(i,M2yvis)=DiaV3wrk(i,j,1,M3yvis)*Hzk(i,1)
            Dwrk(i,M2hvis)=DiaV3wrk(i,j,1,M3hvis)*Hzk(i,1)
#  endif
#  ifdef UV_ADV
            Dwrk(i,M2xadv)=DiaV3wrk(i,j,1,M3xadv)*Hzk(i,1)
            Dwrk(i,M2yadv)=DiaV3wrk(i,j,1,M3yadv)*Hzk(i,1)
            Dwrk(i,M2hadv)=DiaV3wrk(i,j,1,M3hadv)*Hzk(i,1)
#  endif
#  ifdef WEC_VF
            Dwrk(i,M2hjvf)=DiaV3wrk(i,j,1,M3hjvf)*Hzk(i,1)
            Dwrk(i,M2kvrf)=DiaV3wrk(i,j,1,M3kvrf)*Hzk(i,1)
#   ifdef UV_COR
            Dwrk(i,M2fsco)=DiaV3wrk(i,j,1,M3fsco)*Hzk(i,1)
#   endif
#   ifdef BOTTOM_STREAMING
            Dwrk(i,M2bstm)=DiaV3wrk(i,j,1,M3bstm)*Hzk(i,1)
#   endif
#   ifdef SURFACE_STREAMING
            Dwrk(i,M2sstm)=DiaV3wrk(i,j,1,M3sstm)*Hzk(i,1)
#   endif
            Dwrk(i,M2wrol)=DiaV3wrk(i,j,1,M3wrol)*Hzk(i,1)
            Dwrk(i,M2wbrk)=DiaV3wrk(i,j,1,M3wbrk)*Hzk(i,1)
#  endif
# endif
          END DO
          DO k=2,N(ng)
            DO i=Istr,Iend
              CF(i,0)=CF(i,0)+Hzk(i,k)
              DC(i,0)=DC(i,0)+v(i,j,k,nnew)*Hzk(i,k)
# ifdef WEC
              DCs(i,0)=DCs(i,0)+v_stokes(i,j,k)*Hzk(i,k)
# endif
# ifdef DIAGNOSTICS_UV
              Dwrk(i,M2pgrd)=Dwrk(i,M2pgrd)+                            &
     &                       DiaV3wrk(i,j,k,M3pgrd)*Hzk(i,k)
              Dwrk(i,M2bstr)=Dwrk(i,M2bstr)+                            &
     &                       DiaV3wrk(i,j,k,M3vvis)*Hzk(i,k)
#  ifdef UV_COR
              Dwrk(i,M2fcor)=Dwrk(i,M2fcor)+                            &
     &                       DiaV3wrk(i,j,k,M3fcor)*Hzk(i,k)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
              Dwrk(i,M2xvis)=Dwrk(i,M2xvis)+                            &
     &                       DiaV3wrk(i,j,k,M3xvis)*Hzk(i,k)
              Dwrk(i,M2yvis)=Dwrk(i,M2yvis)+                            &
     &                       DiaV3wrk(i,j,k,M3yvis)*Hzk(i,k)
              Dwrk(i,M2hvis)=Dwrk(i,M2hvis)+                            &
     &                       DiaV3wrk(i,j,k,M3hvis)*Hzk(i,k)
#  endif
#  ifdef UV_ADV
              Dwrk(i,M2xadv)=Dwrk(i,M2xadv)+                            &
     &                       DiaV3wrk(i,j,k,M3xadv)*Hzk(i,k)
              Dwrk(i,M2yadv)=Dwrk(i,M2yadv)+                            &
     &                       DiaV3wrk(i,j,k,M3yadv)*Hzk(i,k)
              Dwrk(i,M2hadv)=Dwrk(i,M2hadv)+                            &
     &                       DiaV3wrk(i,j,k,M3hadv)*Hzk(i,k)
#  endif
#  ifdef WEC_VF
              Dwrk(i,M2hjvf)=Dwrk(i,M2hjvf)+                            &
     &                       DiaV3wrk(i,j,k,M3hjvf)*Hzk(i,k)
              Dwrk(i,M2kvrf)=Dwrk(i,M2kvrf)+                            &
     &                       DiaV3wrk(i,j,k,M3kvrf)*Hzk(i,k)
#   ifdef UV_COR
              Dwrk(i,M2fsco)=Dwrk(i,M2fsco)+                            &
     &                       DiaV3wrk(i,j,k,M3fsco)*Hzk(i,k)
#   endif
#   ifdef BOTTOM_STREAMING
              Dwrk(i,M2bstm)=Dwrk(i,M2bstm)+                            &
     &                       DiaV3wrk(i,j,k,M3bstm)*Hzk(i,k)
#   endif
#   ifdef SURFACE_STREAMING
              Dwrk(i,M2sstm)=Dwrk(i,M2sstm)+                            &
     &                       DiaV3wrk(i,j,k,M3sstm)*Hzk(i,k)
#   endif
              Dwrk(i,M2wrol)=Dwrk(i,M2wrol)+                            &
     &                       DiaV3wrk(i,j,k,M3wrol)*Hzk(i,k)
              Dwrk(i,M2wbrk)=Dwrk(i,M2wbrk)+                            &
     &                       DiaV3wrk(i,j,k,M3wbrk)*Hzk(i,k)
#  endif
# endif
            END DO
          END DO
          DO i=Istr,Iend
            cff1=1.0_r8/(CF(i,0)*om_v(i,j))
            DC(i,0)=(DC(i,0)*om_v(i,j)-DV_avg1(i,j))*cff1    ! recursive
# ifdef WEC
            cff2=1.0_r8/CF(i,0)
            DCs(i,0)=DCs(i,0)*cff2-vbar_stokes(i,j)          ! recursive
# endif
# ifdef DIAGNOSTICS_UV
            DO idiag=1,M2pgrd
              Dwrk(i,idiag)=(Dwrk(i,idiag)*om_v(i,j)-                   &
     &                       DiaV2wrk(i,j,idiag))*cff1
            END DO
            Dwrk(i,M2bstr)=(Dwrk(i,M2bstr)*om_v(i,j)-                   &
     &                      DiaV2wrk(i,j,M2bstr)-DiaV2wrk(i,j,M2sstr))* &
     &                     cff1
# endif
          END DO
!
!  Couple and update new solution.
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              v(i,j,k,nnew)=v(i,j,k,nnew)-DC(i,0)
# ifdef MASKING
              v(i,j,k,nnew)=v(i,j,k,nnew)*vmask(i,j)
# endif
# ifdef WET_DRY
              v(i,j,k,nnew)=v(i,j,k,nnew)*vmask_wet(i,j)
              rv(i,j,k,nrhs)=rv(i,j,k,nrhs)*vmask_wet(i,j)
# endif
# ifdef WEC
              v_stokes(i,j,k)=v_stokes(i,j,k)-DCs(i,0)
#  ifdef MASKING
              v_stokes(i,j,k)=v_stokes(i,j,k)*vmask(i,j)
#  endif
#  ifdef WET_DRY
              v_stokes(i,j,k)=v_stokes(i,j,k)*vmask_wet(i,j)
#  endif
# endif
# ifdef DIAGNOSTICS_UV
              DiaV3wrk(i,j,k,M3pgrd)=DiaV3wrk(i,j,k,M3pgrd)-            &
     &                               Dwrk(i,M2pgrd)
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)-            &
     &                               Dwrk(i,M2bstr)
#  ifdef UV_COR
              DiaV3wrk(i,j,k,M3fcor)=DiaV3wrk(i,j,k,M3fcor)-            &
     &                               Dwrk(i,M2fcor)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
              DiaV3wrk(i,j,k,M3xvis)=DiaV3wrk(i,j,k,M3xvis)-            &
     &                               Dwrk(i,M2xvis)
              DiaV3wrk(i,j,k,M3yvis)=DiaV3wrk(i,j,k,M3yvis)-            &
     &                               Dwrk(i,M2yvis)
              DiaV3wrk(i,j,k,M3hvis)=DiaV3wrk(i,j,k,M3hvis)-            &
     &                               Dwrk(i,M2hvis)
#  endif
#  ifdef UV_ADV
              DiaV3wrk(i,j,k,M3xadv)=DiaV3wrk(i,j,k,M3xadv)-            &
     &                               Dwrk(i,M2xadv)
              DiaV3wrk(i,j,k,M3yadv)=DiaV3wrk(i,j,k,M3yadv)-            &
     &                               Dwrk(i,M2yadv)
              DiaV3wrk(i,j,k,M3hadv)=DiaV3wrk(i,j,k,M3hadv)-            &
     &                               Dwrk(i,M2hadv)
#  endif
#  ifdef WEC_VF
              DiaV3wrk(i,j,k,M3hjvf)=DiaV3wrk(i,j,k,M3hjvf)-            &
     &                               Dwrk(i,M2hjvf)
              DiaV3wrk(i,j,k,M3kvrf)=DiaV3wrk(i,j,k,M3kvrf)-            &
     &                               Dwrk(i,M2kvrf)
#   ifdef UV_COR
              DiaV3wrk(i,j,k,M3fsco)=DiaV3wrk(i,j,k,M3fsco)-            &
     &                               Dwrk(i,M2fsco)
#   endif
#   ifdef BOTTOM_STREAMING
              DiaV3wrk(i,j,k,M3bstm)=DiaV3wrk(i,j,k,M3bstm)-            &
     &                               Dwrk(i,M2bstm)
#   endif
#   ifdef SURFACE_STREAMING
              DiaV3wrk(i,j,k,M3sstm)=DiaV3wrk(i,j,k,M3sstm)-            &
     &                               Dwrk(i,M2sstm)
#   endif
              DiaV3wrk(i,j,k,M3wrol)=DiaV3wrk(i,j,k,M3wrol)-            &
     &                               Dwrk(i,M2wrol)
              DiaV3wrk(i,j,k,M3wbrk)=DiaV3wrk(i,j,k,M3wbrk)-            &
     &                               Dwrk(i,M2wbrk)
#  endif
# endif
            END DO
          END DO

# if defined DIAGNOSTICS_UV && defined MASKING
          DO k=1,N(ng)
            DO i=Istr,Iend
              DO idiag=1,NDM3d
                DiaV3wrk(i,j,k,idiag)=DiaV3wrk(i,j,k,idiag)*vmask(i,j)
              END DO
            END DO
          END DO
# endif
        END IF
      END DO
```

</details>

### 6.2 完整当前 C++ 函数

全部包装与 scratch/launch 在下块；`UvDir/UvFields` 和完整 `solve_row` 在[附录 B](#appendix-cpp)。一个 j 工作项在同一分片先处理 U，条件成立再处理 V。

<a id="src-current-uv-solve"></a>

**source-block ID：`current-uv-solve`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.cpp`；闭区间：**1014–1075**；行数：**62**；UTF-8 payload SHA256：`da490fdcec66600a32d13adfbe8b618cb754ca25a79ec5603fe4ecdf6fae07c4`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.cpp，1014–1075（完整原文，可展开）</summary>

<!-- source-block id=current-uv-solve commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.cpp lines=1014-1075 count=62 sha256=da490fdcec66600a32d13adfbe8b618cb754ca25a79ec5603fe4ecdf6fae07c4 -->
```cpp
void k_step3d_uv_solve(const Uv3dConfig* cfg,
                       void* pm_p, void* pn_p, void* on_u_p, void* om_v_p,
                       void* Hz_p, void* z_r_p, void* Akv_p,
                       void* DU_avg1_p, void* DV_avg1_p,
                       void* ru_p, void* rv_p, void* u_p, void* v_p,
                       void* DiaU2wrk_p, void* DiaV2wrk_p,
                       void* DiaU3wrk_p, void* DiaV3wrk_p,
                       void* DiaRU_p, void* DiaRV_p) {
    roms::initialize();
    ROMS_KK_TIMER("step3d_uv solve");

    UvFields f;
    f.c = *cfg;
    const Uv3dConfig &c = f.c;
    const int L0 = c.LBi, L1 = c.UBi, M0 = c.LBj, M1 = c.UBj;
    f.pm = roms::as_2d<double>(pm_p, L0, L1, M0, M1);
    f.pn = roms::as_2d<double>(pn_p, L0, L1, M0, M1);
    f.Hz = roms::as_3d<double>(Hz_p, L0, L1, M0, M1, 1, c.N);
    f.zr = roms::as_3d<double>(z_r_p, L0, L1, M0, M1, 1, c.N);
    f.Akv = roms::as_3d<double>(Akv_p, L0, L1, M0, M1, 0, c.N);
    void* metric[2] = {on_u_p, om_v_p};
    void* avg1[2] = {DU_avg1_p, DV_avg1_p};
    void* rhs[2] = {ru_p, rv_p};
    void* vel[2] = {u_p, v_p};
    void* d2w[2] = {DiaU2wrk_p, DiaV2wrk_p};
    void* d3w[2] = {DiaU3wrk_p, DiaV3wrk_p};
    void* dru[2] = {DiaRU_p, DiaRV_p};
    for (int n = 0; n < 2; ++n) {
        UvDir &d = f.d[n];
        d.di = (n == 0) ? 1 : 0;
        d.dj = (n == 0) ? 0 : 1;
        d.metric = roms::as_2d<double>(metric[n], L0, L1, M0, M1);
        d.avg1 = roms::as_2d<double>(avg1[n], L0, L1, M0, M1);
        d.rhs = roms::as_4d<double>(rhs[n], L0, L1, M0, M1, 0, c.N, 1, 2);
        d.vel = roms::as_4d<double>(vel[n], L0, L1, M0, M1, 1, c.N, 1, 2);
#if defined DIAGNOSTICS_UV
        d.d2wrk = roms::as_3d<double>(d2w[n], L0, L1, M0, M1, 1, c.NDM2d);
        d.d3wrk = roms::as_4d<double>(d3w[n], L0, L1, M0, M1, 1, c.N, 1, c.NDM3d);
        d.dRU = roms::as_5d<double>(dru[n], L0, L1, M0, M1, 1, c.N, 1, 2, 1, c.NDrhs);
#else
        (void)d2w;
        (void)d3w;
        (void)dru;
#endif
    }

    const int ny = c.Jend - c.Jstr + 1;
    const int per = UvFields::solve_scratch(c.N, c.NDM2d);
    double* scratch = nullptr;
#ifndef __sw_slave__
    scratch = workspace.reserve(std::size_t(ny) * per);
#endif

    Kokkos::parallel_for(
        "ROMS step3d_uv solve", range_policy(c.Jstr, c.Jend + 1),
        KOKKOS_LAMBDA(int j) {
            double* s = scratch + std::size_t(j - c.Jstr) * per;
            f.solve_row(f.d[0], c.IstrU, c.Iend, j, s);
            if (j >= c.JstrV)
                f.solve_row(f.d[1], c.Istr, c.Iend, j, s);
        });
}
```

</details>

### 6.3 物理与实现解释

**读写。** 读当前动量 RHS、前述预测新槽、几何与 Akv、快平均 `DU/DV_avg1`、二维/三维诊断历史；写 `u/v(nnew)` 为速度，并调整三维诊断。这里不写最终 Huon/Hvom，也不施加侧边界条件。

**计算流水。** 原版在每 j 行先为 U 形成 AK、Hzk、oHz，更新 RHS，再解垂向系统，积分垂向均值并替换，随后真实执行 V 段。当前 `solve_row` 将每方向按水平 i 的 32 列块重排，依次做相同阶段；层内 i 连续，跨 k 依赖保留。邻点只读 `Hz/z_r/Akv/pm/pn`，新速度不读别列，列间可并行，不意味着列内追赶可随便并行。

**RHS 时间系数。** 首步、第二步、后续步的 `cff` 分别是 `dt/4`、`(dt/4)*3/2`、`(dt/4)*23/12`，再乘两侧 pm 和 pn 的和。SPLINES_VVISC 在此先把厚度加权速度除以平均层厚；非 spline 的厚度由下面直接速度系统对角项处理。诊断总是在相应位置按 oHz 转换，水平黏性诊断仅在 UV_VIS2/UV_VIS4 时访问。

**spline 分支真正解的未知量。** 相邻新速度差是右端，系数由平均 Hz 和 AK 构成；前代 CF/DC 从底向顶，DC(N)=0，回代从 N-1 到 1。此时 DC 是层界面速度梯度型量，乘 AK 后是黏性通量，再以 `dt/Hzk * (DC(k)-DC(k-1))` 加到速度与 vvis 诊断。不是直接把 u/v 当成 spline 追赶系统未知量，也不是普通固定三点系数直接速度方程。

**非 spline 分支。** FC 是含 `-lambda*dt`、垂向间距和 AK 的非对角，顶底 FC=0；BC=Hzk-FC(k)-FC(k-1)，DC 载入厚度加权速度 RHS，解出的 DC 即新速度。先首层初始化、再 k=2..N-1 前代、顶层专用求解、最后从顶向底回代；旧厚度归一化值存 wrk 以记录黏性诊断增量。两种路径都是每列 O(N) 直接法，没有全局矩阵或收敛阈值，也不做主元选取；合法厚度、系数与非零分母是模型数值契约。

**内部均值替换。** 先以第 1 层初始化，再按 k=2..N 累加厚度与速度×厚度；计算 `(列输运*面宽-avg1)/(总厚度*面宽)`，逐层减去同一个速度误差。二维/三维诊断以 `m2/m3` 索引对做同样列积分与替换，vvis 对应底+表应力两个二维项。诊断 pair 是调用端按真实 CPP 组装，不是硬写所有物理项均存在。此段只替换内部点；后续 Fortran BC 可以重新写边界，couple 再处理边界均值。

**融合的边界。** 32 列块共享工作区内的多个垂向数组，避免一列一列遍历约二十多个模型数组；代价是每行分片仍有 O(N*32) host scratch。不能跨过下一节的 BC/点源把 solve 与 couple 简化为一次无条件 kernel。

<a id="uv-couple"></a>

## 7. k_step3d_uv_couple：二维三维耦合与质量通量

### 7.1 完整原始 Fortran 计算段

原版在本段前第 1261–1304 行先完成 U/V 侧边界与动量点源，完整上下文见附录 A，当前对应段也在附录 C 收录。下块含完整 U/V 耦合、快慢模不同 CPP、四边界均值修正、诊断、所有层通量修正；外围仍为 `NONLINEAR && SOLVE3D`。

<a id="src-original-uv-couple"></a>

**source-block ID：`original-uv-couple`**；commit：`cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`；路径：`ROMS/Nonlinear/step3d_uv.F`；闭区间：**1305–1756**；行数：**452**；UTF-8 payload SHA256：`0bdce07eab992b11a1022870ff8b2ea6bd675bacc6c76d0a57fb6b7d6d78b5c2`。

<details>
<summary>ROMS/Nonlinear/step3d_uv.F，1305–1756（完整原文，可展开）</summary>

<!-- source-block id=original-uv-couple commit=cf10c0f7cc270a7075ad19ee3b8950be352a7cd0 path=ROMS/Nonlinear/step3d_uv.F lines=1305-1756 count=452 sha256=0bdce07eab992b11a1022870ff8b2ea6bd675bacc6c76d0a57fb6b7d6d78b5c2 -->
```fortran
!
!-----------------------------------------------------------------------
!  Couple 2D and 3D momentum equations.
!-----------------------------------------------------------------------
!
!  Couple velocity component in the XI-direction.
!
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          DC(i,0)=0.0_r8
          CF(i,0)=0.0_r8
# ifdef WEC
          CFs(i,0)=0.0_r8
# endif
          FC(i,0)=0.0_r8
        END DO
!
!  Compute thicknesses of U-boxes DC(i,1:N), total depth of the water
!  column DC(i,0), and incorrect vertical mean CF(i,0).  Notice that
!  barotropic component is replaced with its fast-time averaged
!  values.
!
        DO k=1,N(ng)
          DO i=IstrP,IendT
            cff=0.5_r8*on_u(i,j)
            DC(i,k)=cff*(Hz(i,j,k)+Hz(i-1,j,k))
            DC(i,0)=DC(i,0)+DC(i,k)
            CF(i,0)=CF(i,0)+                                            &
     &              DC(i,k)*u(i,j,k,nnew)
# ifdef WEC
            CFs(i,0)=CFs(i,0)+                                          &
     &               DC(i,k)*u_stokes(i,j,k)
# endif
          END DO
        END DO
        DO i=IstrP,IendT
          cff1=DC(i,0)                                  ! intermediate
# ifdef WEC
!!        cff2=on_u(i,j)*DC(i,0)*ubar_stokes(i,j)
          cff2=DC(i,0)*ubar_stokes(i,j)
# endif
          DC(i,0)=1.0_r8/DC(i,0)                        ! recursive
          CF(i,0)=DC(i,0)*(CF(i,0)-DU_avg1(i,j))        ! recursive
# ifdef WEC
          CFs(i,0)=DC(i,0)*(CFs(i,0)-cff2)              ! recursive
# endif
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
          ubar(i,j,knew)=DC(i,0)*DU_avg1(i,j)
#  ifdef WET_DRY
          ubar(i,j,knew)=ubar(i,j,knew)*umask_wet(i,j)
#  endif
# else
          ubar(i,j,1)=DC(i,0)*DU_avg1(i,j)
#  ifdef WET_DRY
          ubar(i,j,1)=ubar(i,j,1)*umask_wet(i,j)
#  endif
          ubar(i,j,2)=ubar(i,j,1)
# endif
# ifdef DIAGNOSTICS_UV
          DiaU2wrk(i,j,M2rate)=ubar(i,j,1)-DiaU2int(i,j,M2rate)*DC(i,0)
          DiaU2int(i,j,M2rate)=ubar(i,j,1)*cff1
# endif
        END DO
# ifdef DIAGNOSTICS_UV
!
!  Convert the units of the fast-time integrated change in mass flux
!  diagnostic values to change in velocity (m s-1).
!
        DO idiag=1,NDM2d-1
          DO i=IstrP,IendT
            DiaU2wrk(i,j,idiag)=DC(i,0)*DiaU2wrk(i,j,idiag)
#  ifdef MASKING
            DiaU2wrk(i,j,idiag)=DiaU2wrk(i,j,idiag)*umask(i,j)
#  endif
          END DO
        END DO
# endif
!
!  Replace only BOUNDARY POINTS incorrect vertical mean with more
!  accurate barotropic component, ubar=DU_avg1/(D*on_u). Recall that,
!  D=CF(:,0).
!
!  NOTE:  Only the BOUNDARY POINTS need to be replaced. Avoid redundant
!         update in the interior again for computational purposes which
!         will not affect the nonlinear code.  However, the adjoint
!         code is wrong because the interior solution is corrected
!         twice. The replacement is avoided if the boundary edge is
!         periodic. The J-loop is pipelined, so we need to do a special
!         test for the southern and northern domain edges.
!
        IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO k=1,N(ng)
              u(Istr,j,k,nnew)=u(Istr,j,k,nnew)-CF(Istr,0)
# ifdef MASKING
              u(Istr,j,k,nnew)=u(Istr,j,k,nnew)*                        &
     &                         umask(Istr,j)
# endif
# ifdef WET_DRY
              u(Istr,j,k,nnew)=u(Istr,j,k,nnew)*                        &
     &                         umask_wet(Istr,j)
# endif
# ifdef WEC
              u_stokes(Istr,j,k)=u_stokes(Istr,j,k)-CFs(Istr,0)
#  ifdef MASKING
              u_stokes(Istr,j,k)=u_stokes(Istr,j,k)*                    &
     &                           umask(Istr,j)
#  endif
#  ifdef WET_DRY
              u_stokes(Istr,j,k)=u_stokes(Istr,j,k)*                    &
     &                           umask_wet(Istr,j)
#  endif
# endif
            END DO
          END IF
        END IF
!
        IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO k=1,N(ng)
              u(Iend+1,j,k,nnew)=u(Iend+1,j,k,nnew)-CF(Iend+1,0)
# ifdef MASKING
              u(Iend+1,j,k,nnew)=u(Iend+1,j,k,nnew)*                    &
     &                           umask(Iend+1,j)
# endif
# ifdef WET_DRY
              u(Iend+1,j,k,nnew)=u(Iend+1,j,k,nnew)*                    &
     &                           umask_wet(Iend+1,j)
# endif
# ifdef WEC
              u_stokes(Iend+1,j,k)=u_stokes(Iend+1,j,k)-CFs(Iend+1,0)
#  ifdef MASKING
              u_stokes(Iend+1,j,k)=u_stokes(Iend+1,j,k)*                &
     &                             umask(Iend+1,j)
#  endif
#  ifdef WET_DRY
              u_stokes(Iend+1,j,k)=u_stokes(Iend+1,j,k)*                &
     &                             umask_wet(Iend+1,j)
#  endif
# endif
            END DO
          END IF
        END IF
!
        IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
          IF (j.eq.0) THEN                           ! southern boundary
            DO k=1,N(ng)                             ! J-loop pipelined
              DO i=IstrU,Iend
                u(i,j,k,nnew)=u(i,j,k,nnew)-CF(i,0)
# ifdef MASKING
                u(i,j,k,nnew)=u(i,j,k,nnew)*                            &
     &                        umask(i,j)
# endif
# ifdef WET_DRY
                u(i,j,k,nnew)=u(i,j,k,nnew)*                            &
     &                        umask_wet(i,j)
# endif
# ifdef WEC
                u_stokes(i,j,k)=u_stokes(i,j,k)-CFs(i,0)
#  ifdef MASKING
                u_stokes(i,j,k)=u_stokes(i,j,k)*                        &
     &                          umask(i,j)
#  endif
#  ifdef WET_DRY
                u_stokes(i,j,k)=u_stokes(i,j,k)*                        &
     &                          umask_wet(i,j)
#  endif
# endif
              END DO
            END DO
          END IF
        END IF
!
        IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
          IF (j.eq.Mm(ng)+1) THEN                    ! northern boundary
            DO k=1,N(ng)                             ! J-loop pipelined
              DO i=IstrU,Iend
                u(i,j,k,nnew)=u(i,j,k,nnew)-CF(i,0)
# ifdef MASKING
                u(i,j,k,nnew)=u(i,j,k,nnew)*                            &
     &                        umask(i,j)
# endif
# ifdef WET_DRY
                u(i,j,k,nnew)=u(i,j,k,nnew)*                            &
     &                        umask_wet(i,j)
# endif
# ifdef WEC
                u_stokes(i,j,k)=u_stokes(i,j,k)-CFs(i,0)
#  ifdef MASKING
                u_stokes(i,j,k)=u_stokes(i,j,k)*                        &
     &                          umask(i,j)
#  endif
#  ifdef WET_DRY
                u_stokes(i,j,k)=u_stokes(i,j,k)*                        &
     &                          umask_wet(i,j)
#  endif
# endif
              END DO
            END DO
          END IF
        END IF
!
!  Compute correct mass flux, Hz*u/n.
!
        DO k=N(ng),1,-1
          DO i=IstrP,IendT
            Huon(i,j,k)=0.5_r8*(Huon(i,j,k)+u(i,j,k,nnew)*DC(i,k))
# ifdef WEC
            Huon(i,j,k)=Huon(i,j,k)+0.5_r8*u_stokes(i,j,k)*DC(i,k)
# endif
            FC(i,0)=FC(i,0)+Huon(i,j,k)
# ifdef DIAGNOSTICS_UV
            DiaU3wrk(i,j,k,M3rate)=u(i,j,k,nnew)-DiaU3wrk(i,j,k,M3rate)
# endif
          END DO
        END DO
        DO i=IstrP,IendT
          FC(i,0)=DC(i,0)*(FC(i,0)-DU_avg2(i,j))        ! recursive
        END DO
        DO k=1,N(ng)
          DO i=IstrP,IendT
            Huon(i,j,k)=Huon(i,j,k)-DC(i,k)*FC(i,0)
          END DO
        END DO
!
!  Couple velocity component in the ETA-direction.
!
        IF (j.ge.Jstr) THEN
          DO i=IstrT,IendT
            DC(i,0)=0.0_r8
            CF(i,0)=0.0_r8
# ifdef WEC
            CFs(i,0)=0.0_r8
# endif
            FC(i,0)=0.0_r8
          END DO
!
!  Compute thicknesses of V-boxes DC(i,1:N), total depth of the water
!  column DC(i,0), and incorrect vertical mean CF(i,0).  Notice that
!  barotropic component is replaced with its fast-time averaged
!  values.
!
          DO k=1,N(ng)
            DO i=IstrT,IendT
              cff=0.5_r8*om_v(i,j)
              DC(i,k)=cff*(Hz(i,j,k)+Hz(i,j-1,k))
              DC(i,0)=DC(i,0)+DC(i,k)
              CF(i,0)=CF(i,0)+                                          &
     &                DC(i,k)*v(i,j,k,nnew)
# ifdef WEC
              CFs(i,0)=CFs(i,0)+                                        &
     &                 DC(i,k)*v_stokes(i,j,k)
# endif
            END DO
          END DO
          DO i=IstrT,IendT
            cff1=DC(i,0)                                 ! Intermediate
# ifdef WEC
!!          cff2=om_v(i,j)*DC(i,0)*vbar_stokes(i,j)
            cff2=DC(i,0)*vbar_stokes(i,j)
# endif
            DC(i,0)=1.0_r8/DC(i,0)                       ! recursive
            CF(i,0)=DC(i,0)*(CF(i,0)-DV_avg1(i,j))       ! recursive
# ifdef WEC
            CFs(i,0)=DC(i,0)*(CFs(i,0)-cff2)             ! recursive
# endif
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
            vbar(i,j,knew)=DC(i,0)*DV_avg1(i,j)
#  ifdef WET_DRY
            vbar(i,j,knew)=vbar(i,j,knew)*vmask_wet(i,j)
#  endif
# else
            vbar(i,j,1)=DC(i,0)*DV_avg1(i,j)
#  ifdef WET_DRY
            vbar(i,j,1)=vbar(i,j,1)*vmask_wet(i,j)
#  endif
            vbar(i,j,2)=vbar(i,j,1)
# endif
# ifdef DIAGNOSTICS_UV
            DiaV2wrk(i,j,M2rate)=vbar(i,j,1)-                           &
     &                           DiaV2int(i,j,M2rate)*DC(i,0)
            DiaV2int(i,j,M2rate)=vbar(i,j,1)*cff1
!!          DiaV2wrk(i,j,M2rate)=vbar(i,j,1)-DiaV2int(i,j,M2rate)
!!          DiaV2int(i,j,M2rate)=vbar(i,j,1)
# endif
          END DO
# ifdef DIAGNOSTICS_UV
!
!  Convert the units of the fast-time integrated change in mass flux
!  diagnostic values to change in velocity (m s-1).
!
          DO idiag=1,NDM2d-1
            DO i=IstrT,IendT
              DiaV2wrk(i,j,idiag)=DC(i,0)*DiaV2wrk(i,j,idiag)
#  ifdef MASKING
              DiaV2wrk(i,j,idiag)=DiaV2wrk(i,j,idiag)*vmask(i,j)
#  endif
            END DO
          END DO
# endif
!
!  Replace only BOUNDARY POINTS incorrect vertical mean with more
!  accurate barotropic component, vbar=DV_avg1/(D*om_v).  Recall that,
!  D=CF(:,0).
!
!  NOTE:  Only the BOUNDARY POINTS need to be replaced. Avoid redundant
!         update in the interior again for computational purposes which
!         will not affect the nonlinear code.  However, the adjoint
!         code is wrong because the interior solution is corrected
!         twice. The replacement is avoided if the boundary edge is
!         periodic. The J-loop is pipelined, so we need to do a special
!         test for the southern and northern domain edges.
!
          IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
            IF (DOMAIN(ng)%Western_Edge(tile)) THEN
              DO k=1,N(ng)
                v(Istr-1,j,k,nnew)=v(Istr-1,j,k,nnew)-CF(Istr-1,0)
# ifdef MASKING
                v(Istr-1,j,k,nnew)=v(Istr-1,j,k,nnew)*                  &
     &                             vmask(Istr-1,j)
# endif
# ifdef WET_DRY
                v(Istr-1,j,k,nnew)=v(Istr-1,j,k,nnew)*                  &
     &                             vmask_wet(Istr-1,j)
# endif
# ifdef WEC
                v_stokes(Istr-1,j,k)=v_stokes(Istr-1,j,k)-              &
     &                               CFs(Istr-1,0)
#  ifdef MASKING
                v_stokes(Istr-1,j,k)=v_stokes(Istr-1,j,k)*              &
     &                               vmask(Istr-1,j)
#  endif
#  ifdef WET_DRY
                v_stokes(Istr-1,j,k)=v_stokes(Istr-1,j,k)*              &
     &                               vmask_wet(Istr-1,j)
#  endif
# endif
              END DO
            END IF
          END IF
!
          IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
            IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
              DO k=1,N(ng)
                v(Iend+1,j,k,nnew)=v(Iend+1,j,k,nnew)-CF(Iend+1,0)
# ifdef MASKING
                v(Iend+1,j,k,nnew)=v(Iend+1,j,k,nnew)*                  &
     &                             vmask(Iend+1,j)
# endif
# ifdef WET_DRY
                v(Iend+1,j,k,nnew)=v(Iend+1,j,k,nnew)*                  &
     &                             vmask_wet(Iend+1,j)
# endif
# ifdef WEC
                v_stokes(Iend+1,j,k)=v_stokes(Iend+1,j,k)-              &
     &                               CFs(Iend+1,0)
#  ifdef MASKING
                v_stokes(Iend+1,j,k)=v_stokes(Iend+1,j,k)*              &
     &                               vmask(Iend+1,j)
#  endif
#  ifdef WET_DRY
                v_stokes(Iend+1,j,k)=v_stokes(Iend+1,j,k)*              &
     &                               vmask_wet(Iend+1,j)
#  endif
# endif
              END DO
            END IF
          END IF
!
          IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
            IF (j.eq.1) THEN                         ! southern boundary
              DO k=1,N(ng)                           ! J-loop pipelined
                DO i=Istr,Iend
                  v(i,j,k,nnew)=v(i,j,k,nnew)-CF(i,0)
# ifdef MASKING
                  v(i,j,k,nnew)=v(i,j,k,nnew)*                          &
     &                          vmask(i,j)
# endif
# ifdef WET_DRY
                  v(i,j,k,nnew)=v(i,j,k,nnew)*                          &
     &                          vmask_wet(i,j)
# endif
# ifdef WEC
                  v_stokes(i,j,k)=v_stokes(i,j,k)-CFs(i,0)
#  ifdef MASKING
                  v_stokes(i,j,k)=v_stokes(i,j,k)*                      &
     &                            vmask(i,j)
#  endif
#  ifdef WET_DRY
                  v_stokes(i,j,k)=v_stokes(i,j,k)*                      &
     &                            vmask_wet(i,j)
#  endif
# endif
                END DO
              END DO
            END IF
          END IF
!
          IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
            IF (j.eq.Mm(ng)+1) THEN                  ! northern boundary
              DO k=1,N(ng)                           ! J-loop pipelined
                DO i=Istr,Iend
                  v(i,j,k,nnew)=v(i,j,k,nnew)-CF(i,0)
# ifdef MASKING
                  v(i,j,k,nnew)=v(i,j,k,nnew)*                          &
     &                          vmask(i,j)
# endif
# ifdef WET_DRY
                  v(i,j,k,nnew)=v(i,j,k,nnew)*                          &
     &                          vmask_wet(i,j)
# endif
# ifdef WEC
                  v_stokes(i,j,k)=v_stokes(i,j,k)-CFs(i,0)
#  ifdef MASKING
                  v_stokes(i,j,k)=v_stokes(i,j,k)*                      &
     &                            vmask(i,j)
#  endif
#  ifdef WET_DRY
                  v_stokes(i,j,k)=v_stokes(i,j,k)*                      &
     &                            vmask_wet(i,j)
#  endif
# endif
                END DO
              END DO
            END IF
          END IF
!
!  Compute correct mass flux, Hz*v/m.
!
          DO k=N(ng),1,-1
            DO i=IstrT,IendT
              Hvom(i,j,k)=0.5_r8*(Hvom(i,j,k)+v(i,j,k,nnew)*DC(i,k))
# ifdef WEC
              Hvom(i,j,k)=Hvom(i,j,k)+0.5_r8*v_stokes(i,j,k)*DC(i,k)
# endif
              FC(i,0)=FC(i,0)+Hvom(i,j,k)
# ifdef DIAGNOSTICS_UV
              DiaV3wrk(i,j,k,M3rate)=v(i,j,k,nnew)-                     &
     &                               DiaV3wrk(i,j,k,M3rate)
# endif
            END DO
          END DO
          DO i=IstrT,IendT
            FC(i,0)=DC(i,0)*(FC(i,0)-DV_avg2(i,j))      ! recursive
          END DO
          DO k=1,N(ng)
            DO i=IstrT,IendT
              Hvom(i,j,k)=Hvom(i,j,k)-DC(i,k)*FC(i,0)
            END DO
          END DO
        END IF
      END DO
```

</details>

### 7.2 完整当前 C++ 函数

函数前空行保留；`couple_row` 和 `couple_scratch` 完整定义在[附录 B](#appendix-cpp)，与 solve 复用 UvDir 类型，但输入包装与输出不同。

<a id="src-current-uv-couple"></a>

**source-block ID：`current-uv-couple`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.cpp`；闭区间：**1076–1140**；行数：**65**；UTF-8 payload SHA256：`ee153acf861cf30a14c6a01a554f2c32025872af853df051585ef0ede8e828bd`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.cpp，1076–1140（完整原文，可展开）</summary>

<!-- source-block id=current-uv-couple commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.cpp lines=1076-1140 count=65 sha256=ee153acf861cf30a14c6a01a554f2c32025872af853df051585ef0ede8e828bd -->
```cpp

void k_step3d_uv_couple(const Uv3dConfig* cfg,
                        void* on_u_p, void* om_v_p, void* Hz_p,
                        void* DU_avg1_p, void* DU_avg2_p,
                        void* DV_avg1_p, void* DV_avg2_p,
                        void* u_p, void* v_p, void* ubar_p, void* vbar_p,
                        void* Huon_p, void* Hvom_p,
                        void* DiaU2wrk_p, void* DiaV2wrk_p,
                        void* DiaU2int_p, void* DiaV2int_p,
                        void* DiaU3wrk_p, void* DiaV3wrk_p) {
    roms::initialize();
    ROMS_KK_TIMER("step3d_uv couple");

    UvFields f;
    f.c = *cfg;
    const Uv3dConfig &c = f.c;
    const int L0 = c.LBi, L1 = c.UBi, M0 = c.LBj, M1 = c.UBj;
    f.Hz = roms::as_3d<double>(Hz_p, L0, L1, M0, M1, 1, c.N);
    void* metric[2] = {on_u_p, om_v_p};
    void* avg1[2] = {DU_avg1_p, DV_avg1_p};
    void* avg2[2] = {DU_avg2_p, DV_avg2_p};
    void* vel[2] = {u_p, v_p};
    void* bar[2] = {ubar_p, vbar_p};
    void* flux[2] = {Huon_p, Hvom_p};
    void* d2w[2] = {DiaU2wrk_p, DiaV2wrk_p};
    void* d2i[2] = {DiaU2int_p, DiaV2int_p};
    void* d3w[2] = {DiaU3wrk_p, DiaV3wrk_p};
    for (int n = 0; n < 2; ++n) {
        UvDir &d = f.d[n];
        d.di = (n == 0) ? 1 : 0;
        d.dj = (n == 0) ? 0 : 1;
        d.metric = roms::as_2d<double>(metric[n], L0, L1, M0, M1);
        d.avg1 = roms::as_2d<double>(avg1[n], L0, L1, M0, M1);
        d.avg2 = roms::as_2d<double>(avg2[n], L0, L1, M0, M1);
        d.vel = roms::as_4d<double>(vel[n], L0, L1, M0, M1, 1, c.N, 1, 2);
        d.bar = roms::as_3d<double>(bar[n], L0, L1, M0, M1, 1, 3);
        d.flux = roms::as_3d<double>(flux[n], L0, L1, M0, M1, 1, c.N);
#if defined DIAGNOSTICS_UV
        d.d2wrk = roms::as_3d<double>(d2w[n], L0, L1, M0, M1, 1, c.NDM2d);
        d.d2int = roms::as_3d<double>(d2i[n], L0, L1, M0, M1, 1, c.NDM2d);
        d.d3wrk = roms::as_4d<double>(d3w[n], L0, L1, M0, M1, 1, c.N, 1, c.NDM3d);
#else
        (void)d2w;
        (void)d2i;
        (void)d3w;
#endif
    }

    const int ny = c.JendT - c.JstrT + 1;
    const int per = UvFields::couple_scratch(c.N);
    double* scratch = nullptr;
#ifndef __sw_slave__
    scratch = workspace.reserve(std::size_t(ny) * per);
#endif

    Kokkos::parallel_for(
        "ROMS step3d_uv couple", range_policy(c.JstrT, c.JendT + 1),
        KOKKOS_LAMBDA(int j) {
            double* s = scratch + std::size_t(j - c.JstrT) * per;
            f.couple_row(f.d[0], c.IstrP, c.IendT, j, c.Istr, 0, c.IstrU, s);
            if (j >= c.Jstr) {
                f.couple_row(f.d[1], c.IstrT, c.IendT, j, c.Istr - 1, 1, c.Istr, s);
            }
        });
}
```

</details>

### 7.3 物理与实现解释

**读写。** 读 BC/点源处理后的 `u/v(nnew)`、Hz、面宽、avg1/avg2；写 `ubar/vbar` 的第 1、2 槽、需要修正的三维边界速度、所有覆盖层的 Huon/Hvom，以及二维/三维 rate 等诊断。原 Huon/Hvom 是参与 0.5 平均的输入，不能把它当作纯输出缓冲而清零。

**列积分与量纲。** 每层 `DCk=0.5*面宽*(两侧Hz之和)` 单位是 m²，不是单纯 m 厚度。自 k=1..N 累加柱截面积 DC0 与输运 CF0；取面积倒数，把 `avg1/面积` 写 bar 槽 1、2，并算出三维与二维输运的速度误差。原始 STEP2D_FB_* 路径写 `knew`，但当前编译门禁排除这两个组合，故这里固定写槽 1、2 是受限路径，不是对所有正压算法通用。

**边界均值只替换边界。** 已由 solve 修正的内部不再次减均值。`rep_w/e` 同时含非 composite/非周期及真实 Western/Eastern edge，`rep_s/n` 结合具体 j 判断。U 西边 i=Istr、南边 j=0，V 西边 i=Istr-1、南边 j=1；北边均为 Mm+1，东边 Iend+1，南北 i 限制各自使用真实起点。四个 if 按西、东、南、北顺序，不改为 else-if；角点/窄区域是否重复触发以原条件为准。当前局部 i→k 的边界修正遍历与原全边界循环位置不同，但每列顺序保留。

**质量通量强约束。** k 从 N 降到 1，先取原分层通量与新速度×截面积的 0.5 平均，并按同一降序累加 FC0；用 `avg2` 计算误差，再 k=1..N 按层截面积扣回，使分层通量积分与第二套快平均相容。两套 avg 并非冗余：avg1 约束垂向平均速度，avg2 约束通量平均。顶到底求和是实际数值顺序，不能以“列独立”理由换成水平或全局 reduction。

**诊断与调度。** bar rate 用旧二维积分 history，其他二维项按面积倒数转换；三维 rate 在最终速度确定后做差。RangePolicy 扩展到 JstrT..JendT，U 走 IstrP..IendT、V 走 IstrT..IendT 且要求 j>=Jstr。每方向在 i 的 32 列块中复用一个 `(N+5)*32` 分片。两入口之间没有 halo exchange；couple 之后才统一做周期/MPI exchange 和 uv_C2A_grid，仍由 Fortran 掌管，不能删掉这些代价。

<a id="t-chunks"></a>

## 8. k_step3d_t_chunks：tracer 校正与 spline 扩散

### 8.1 完整原始 Fortran 计算段

原始文件外围为 `!TS_FIXED && NONLINEAR && SOLVE3D`。下块从原水平平流 prologue 连续覆盖至隐式垂向扩散结束，保留全部 H/V scheme、HSIMT、MPDATA 抗扩散、多种源项、NESTING、OMEGA_IMPLICIT、SED_MORPH、诊断、SPLINES_VDIFF 与非 spline 分支；这是完整原始相关计算，不是声称当前 chunks 覆盖它们全部。年龄源、侧边界、nudging、最终 rate、通信和 deallocate 在共同 epilogue，完整文件见附录 A；当前加速路径也不跳过这些共同尾段。

<a id="src-original-t-corrector"></a>

**source-block ID：`original-t-corrector`**；commit：`cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`；路径：`ROMS/Nonlinear/step3d_t.F`；闭区间：**341–1799**；行数：**1459**；UTF-8 payload SHA256：`ed85d15e771bf55d79508b7561e112f7bc9ad8591cd679afd94818fd5f124397`。

<details>
<summary>ROMS/Nonlinear/step3d_t.F，341–1799（完整原文，可展开）</summary>

<!-- source-block id=original-t-corrector commit=cf10c0f7cc270a7075ad19ee3b8950be352a7cd0 path=ROMS/Nonlinear/step3d_t.F lines=341-1799 count=1459 sha256=ed85d15e771bf55d79508b7561e112f7bc9ad8591cd679afd94818fd5f124397 -->
```fortran
!
!-----------------------------------------------------------------------
!  Time-step horizontal advection term.
!-----------------------------------------------------------------------
!
      Lhsimt =ANY(Hadvection(:,ng)%HSIMT).and.                          &
     &        ANY(Vadvection(:,ng)%HSIMT)
      Lmpdata=ANY(Hadvection(:,ng)%MPDATA).and.                         &
     &        ANY(Vadvection(:,ng)%MPDATA)
!
!  Allocate local arrays for MPDATA.
!
      IF (Lmpdata) THEN
# ifdef DIAGNOSTICS_TS
        IF (.not.allocated(Dhadv)) THEN
          allocate ( Dhadv(IminS:ImaxS,JminS:JmaxS,3) )
          Dhadv=0.0_r8
        END IF
        IF (.not.allocated(Dvadv)) THEN
          allocate ( Dvadv(IminS:ImaxS,JminS:JmaxS,N(ng),NT(ng)) )
          Dvadv=0.0_r8
        END IF
# endif
        IF (.not.allocated(Ta)) THEN
          allocate ( Ta(IminS:ImaxS,JminS:JmaxS,N(ng),NT(ng)) )
          Ta=0.0_r8
        END IF
        IF (.not.allocated(Ua)) THEN
          allocate ( Ua(IminS:ImaxS,JminS:JmaxS,N(ng)) )
          Ua=0.0_r8
        END IF
        IF (.not.allocated(Va)) THEN
          allocate ( Va(IminS:ImaxS,JminS:JmaxS,N(ng)) )
          Va=0.0_r8
        END IF
        IF (.not.allocated(Wa)) THEN
          allocate ( Wa(IminS:ImaxS,JminS:JmaxS,0:N(ng)) )
          Wa=0.0_r8
        END IF
      END IF
!
!  Compute reciprocal thickness, 1/Hz.
!
      IF (Lmpdata.or.Lhsimt) THEN
        DO k=1,N(ng)
          DO j=Jstrm2,Jendp2
            DO i=Istrm2,Iendp2
              oHz(i,j,k)=1.0_r8/Hz(i,j,k)
            END DO
          END DO
        END DO
      ELSE
        DO k=1,N(ng)
          DO j=Jstr,Jend
            DO i=Istr,Iend
              oHz(i,j,k)=1.0_r8/Hz(i,j,k)
            END DO
          END DO
        END DO
      END IF
!
!  Horizontal tracer advection.  It is possible to have a different
!  advection schme for each tracer.
!
      T_LOOP1 : DO itrc=1,NT(ng)
!
!  The MPDATA and HSIMT algorithms requires a three-point footprint, so
!  exchange boundary data on t(:,:,:,nnew,:) so other processes computed
!  earlier (horizontal diffusion, biology, or sediment) are accounted.
!
        IF ((Hadvection(itrc,ng)%MPDATA).or.                            &
     &      (Hadvection(itrc,ng)%HSIMT)) THEN
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r3d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj, 1, N(ng),       &
     &                              t(:,:,:,nnew,itrc))
          END IF

# ifdef DISTRIBUTE
          CALL mp_exchange3d (ng, tile, iNLM, 1,                        &
     &                        LBi, UBi, LBj, UBj, 1, N(ng),             &
     &                        NghostPoints,                             &
     &                        EWperiodic(ng), NSperiodic(ng),           &
     &                        t(:,:,:,nnew,itrc))
# endif
        END IF
!
!  Compute horizontal tracer advection fluxes.
!
        K_LOOP : DO k=1,N(ng)
!
          HADV_FLUX : IF (Hadvection(itrc,ng)%CENTERED2) THEN
!
!  Second-order, centered differences horizontal advective fluxes.
!
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                FX(i,j)=Huon(i,j,k)*                                    &
     &                  0.5_r8*(t(i-1,j,k,3,itrc)+                      &
     &                          t(i  ,j,k,3,itrc))
              END DO
            END DO
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                FE(i,j)=Hvom(i,j,k)*                                    &
     &                  0.5_r8*(t(i,j-1,k,3,itrc)+                      &
     &                          t(i,j  ,k,3,itrc))
              END DO
            END DO
!
          ELSE IF (Hadvection(itrc,ng)%MPDATA) THEN
!
!  First-order, upstream differences horizontal advective fluxes.
!
            DO j=JstrVm2,Jendp2i
              DO i=IstrUm2,Iendp3
                cff1=MAX(Huon(i,j,k),0.0_r8)
                cff2=MIN(Huon(i,j,k),0.0_r8)
                FX(i,j)=cff1*t(i-1,j,k,3,itrc)+                         &
     &                  cff2*t(i  ,j,k,3,itrc)
              END DO
            END DO
            DO j=JstrVm2,Jendp3
              DO i=IstrUm2,Iendp2i
                cff1=MAX(Hvom(i,j,k),0.0_r8)
                cff2=MIN(Hvom(i,j,k),0.0_r8)
                FE(i,j)=cff1*t(i,j-1,k,3,itrc)+                         &
     &                  cff2*t(i,j  ,k,3,itrc)
              END DO
            END DO
!
          ELSE IF (Hadvection(itrc,ng)%HSIMT) THEN
!
!  Third High-order Spatial Interpolation at the Middle Temporal level
!  (HSIMT; Wu and Zhu, 2010) with a Total Variation Diminishing (TVD)
!  limiter horizontal advection fluxes.
!
!  Hui Wu and Jianrong Zhu, 2010: Advection scheme with 3rd high-order
!    spatial interpolation at the middle temporal level and its
!    application to saltwater intrusion in the Changjiang Estuary,
!    Ocean Modelling 33, 33-51, doi:10.1016/j.ocemod.2009.12.001
!
            DO j=Jstr,Jend
              DO i=IstrU-1,Iendp2
                cff=0.125_r8*(pm(i-1,j)+pm(i,j))*(pn(i-1,j)+pn(i,j))*   &
     &              dt(ng)
                cff1=cff*(oHz(i-1,j,k)+oHz(i,j,k))
                gradX(i)=t(i,j,k,3,itrc)-t(i-1,j,k,3,itrc)
                KaX(i)=1.0_r8-ABS(Huon(i,j,k)*cff1)
# ifdef MASKING
                gradX(i)=gradX(i)*umask(i,j)
                KaX(i)=KaX(i)*umask(i,j)
# endif
              END DO
              IF (.not.EWperiodic(ng)) THEN
                IF (DOMAIN(ng)%Western_Edge(tile)) THEN
                  IF (Huon(Istr,j,k).ge.0.0_r8) THEN
                    gradX(Istr-1)=0.0_r8
                    KaX(Istr-1)=0.0_r8
                  END IF
                END IF
                IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
                  IF (Huon(Iend+1,j,k).lt.0.0_r8) THEN
                    gradX(Iend+2)=0.0_r8
                    KaX(Iend+2)=0.0_r8
                  END IF
                END IF
              END IF
              DO i=Istr,Iend+1
                IF (KaX(i).le.eps1) THEN
                  oKaX(i)=0.0_r8
                ELSE
                  oKaX(i)=1.0_r8/MAX(KaX(i),eps1)
                END IF
                IF (Huon(i,j,k).ge.0.0_r8) THEN
                  IF (ABS(gradX(i)).le.eps1) THEN
                    rL=0.0_r8
                    rkaL=0.0_r8
                  ELSE
                    rL=gradX(i-1)/gradX(i)
                    rkaL=KaX(i-1)*oKaX(i)
                  END IF
                  a1= cc1*KaX(i)+cc2-cc3*oKaX(i)
                  b1=-cc1*KaX(i)+cc2+cc3*oKaX(i)
                  betaL=a1+b1*rL
                  cff=0.5_r8*MAX(0.0_r8,                                &
     &                           MIN(2.0_r8, 2.0_r8*rL*rkaL, betaL))*   &
     &                gradX(i)*KaX(i)
# ifdef MASKING
                  ii=MAX(i-2,0)
                  cff=cff*rmask(ii,j)
# endif
                  sw_xi=t(i-1,j,k,3,itrc)+cff
                ELSE
                  IF (ABS(gradX(i)).le.eps1) THEN
                    rR=0.0_r8
                    rkaR=0.0_r8
                  ELSE
                    rR=gradX(i+1)/gradX(i)
                    rkaR=KaX(i+1)*oKaX(i)
                  END IF
                  a1= cc1*KaX(i)+cc2-cc3*oKaX(i)
                  b1=-cc1*KaX(i)+cc2+cc3*oKaX(i)
                  betaR=a1+b1*rR
                  cff=0.5_r8*MAX(0.0_r8,                                &
     &                           MIN(2.0_r8, 2.0_r8*rR*rkaR, betaR))*   &
     &                gradX(i)*KaX(i)
# ifdef MASKING
                  ii=MIN(i+1,Lm(ng)+1)
                  cff=cff*rmask(ii,j)
# endif
                  sw_xi=t(i,j,k,3,itrc)-cff
                END IF
                FX(i,j)=sw_xi*Huon(i,j,k)
              END DO
            END DO
!
            DO i=Istr,Iend
              DO j=JstrV-1,Jendp2
                cff=0.125_r8*(pn(i,j)+pn(i,j-1))*(pm(i,j)+pm(i,j-1))*   &
     &              dt(ng)
                cff1=cff*(oHz(i,j,k)+oHz(i,j-1,k))
                gradE(j)=t(i,j,k,3,itrc)-t(i,j-1,k,3,itrc)
                KaE(j)=1.0_r8-ABS(Hvom(i,j,k)*cff1)
# ifdef MASKING
                gradE(j)=gradE(j)*vmask(i,j)
                KaE(j)=KaE(j)*vmask(i,j)
# endif
              END DO
              IF (.not.NSperiodic(ng)) THEN
                IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
                  IF (Hvom(i,Jstr,k).ge.0.0_r8) THEN
                    gradE(Jstr-1)=0.0_r8
                    KaE(Jstr-1)=0.0_r8
                  END IF
                END IF
                IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
                  IF (Hvom(i,Jend+1,k).lt.0.0_r8) THEN
                    gradE(Jend+2)=0.0_r8
                    KaE(Jend+2)=0.0_r8
                  END IF
                END IF
              END IF
              DO j=Jstr,Jend+1
                IF (KaE(j).le.eps1) THEN
                  oKaE(j)=0.0_r8
                ELSE
                  oKaE(j)=1.0_r8/MAX(KaE(j),eps1)
                END IF
                IF (Hvom(i,j,k).ge.0.0_r8) THEN
                  IF (ABS(gradE(j)).le.eps1) THEN
                    rD=0.0_r8
                    rkaD=0.0_r8
                  ELSE
                    rD=gradE(j-1)/gradE(j)
                    rkaD=KaE(j-1)*oKaE(j)
                  END IF
                  a1= cc1*KaE(j)+cc2-cc3*oKaE(j)
                  b1=-cc1*KaE(j)+cc2+cc3*oKaE(j)
                  betaD=a1+b1*rD
                  cff=0.5_r8*MAX(0.0_r8,                                &
     &                           MIN(2.0_r8, 2.0_r8*rD*rkaD, betaD))*   &
     &                gradE(j)*KaE(j)
# ifdef MASKING
                  jj=MAX(j-2,0)
                  cff=cff*rmask(i,jj)
# endif
                  sw_eta=t(i,j-1,k,3,itrc)+cff
                ELSE
                  IF (ABS(gradE(j)).le.eps1) THEN
                    rU=0.0_r8
                    rkaU=0.0_r8
                  ELSE
                    rU=gradE(j+1)/gradE(j)
                    rkaU=KaE(j+1)*oKaE(j)
                  END IF
                  a1= cc1*KaE(j)+cc2-cc3*oKaE(j)
                  b1=-cc1*KaE(j)+cc2+cc3*oKaE(j)
                  betaU=a1+b1*rU
                  cff=0.5*MAX(0.0_r8,                                   &
     &                        MIN(2.0_r8, 2.0_r8*rU*rkaU, betaU))*      &
     &                gradE(j)*KaE(j)
# ifdef MASKING
                  jj=MIN(j+1,Mm(ng)+1)
                  cff=cff*rmask(i,jj)
# endif
                  sw_eta=t(i,j,k,3,itrc)-cff
                END IF
                FE(i,j)=sw_eta*Hvom(i,j,k)
              END DO
            END DO
!
          ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.                     &
     &             (Hadvection(itrc,ng)%CENTERED4).or.                  &
     &             (Hadvection(itrc,ng)%SPLIT_U3).or.                   &
     &             (Hadvection(itrc,ng)%UPSTREAM3)) THEN
!
!  Fourth-order Akima, fourth-order centered differences, or third-order
!  upstream-biased horizontal advective fluxes.
!
            DO j=Jstr,Jend
              DO i=Istrm1,Iendp2
                FX(i,j)=t(i  ,j,k,3,itrc)-                              &
     &                  t(i-1,j,k,3,itrc)
# ifdef MASKING
                FX(i,j)=FX(i,j)*umask(i,j)
# endif
              END DO
            END DO
            IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
              IF (DOMAIN(ng)%Western_Edge(tile)) THEN
                DO j=Jstr,Jend
                  FX(Istr-1,j)=FX(Istr,j)
                END DO
              END IF
            END IF
            IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
              IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
                DO j=Jstr,Jend
                  FX(Iend+2,j)=FX(Iend+1,j)
                END DO
              END IF
            END IF
!
            DO j=Jstr,Jend
              DO i=Istr-1,Iend+1
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  curv(i,j)=FX(i+1,j)-FX(i,j)
                ELSE IF (Hadvection(itrc,ng)%AKIMA4) THEN
                  cff=2.0_r8*FX(i+1,j)*FX(i,j)
                  IF (cff.gt.eps) THEN
                    grad(i,j)=cff/(FX(i+1,j)+FX(i,j))
                  ELSE
                    grad(i,j)=0.0_r8
                  END IF
                ELSE IF ((Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  grad(i,j)=0.5_r8*(FX(i+1,j)+FX(i,j))
                END IF
              END DO
            END DO
!
            cff1=1.0_r8/6.0_r8
            cff2=1.0_r8/3.0_r8
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  FX(i,j)=Huon(i,j,k)*0.5_r8*                           &
     &                    (t(i-1,j,k,3,itrc)+                           &
     &                     t(i  ,j,k,3,itrc))-                          &
     &                    cff1*(curv(i-1,j)*MAX(Huon(i,j,k),0.0_r8)+    &
     &                          curv(i  ,j)*MIN(Huon(i,j,k),0.0_r8))
                ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.               &
     &                   (Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  FX(i,j)=Huon(i,j,k)*0.5_r8*                           &
     &                    (t(i-1,j,k,3,itrc)+                           &
     &                     t(i  ,j,k,3,itrc)-                           &
     &                     cff2*(grad(i  ,j)-                           &
     &                           grad(i-1,j)))
                END IF
              END DO
            END DO
!
            DO j=Jstrm1,Jendp2
              DO i=Istr,Iend
                FE(i,j)=t(i,j  ,k,3,itrc)-                              &
     &                  t(i,j-1,k,3,itrc)
# ifdef MASKING
                FE(i,j)=FE(i,j)*vmask(i,j)
# endif
              END DO
            END DO
            IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
              IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
                DO i=Istr,Iend
                  FE(i,Jstr-1)=FE(i,Jstr)
                END DO
              END IF
            END IF
            IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
              IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
                DO i=Istr,Iend
                  FE(i,Jend+2)=FE(i,Jend+1)
                END DO
              END IF
            END IF
!
            DO j=Jstr-1,Jend+1
              DO i=Istr,Iend
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  curv(i,j)=FE(i,j+1)-FE(i,j)
                ELSE IF (Hadvection(itrc,ng)%AKIMA4) THEN
                  cff=2.0_r8*FE(i,j+1)*FE(i,j)
                  IF (cff.gt.eps) THEN
                    grad(i,j)=cff/(FE(i,j+1)+FE(i,j))
                  ELSE
                    grad(i,j)=0.0_r8
                  END IF
                ELSE IF ((Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  grad(i,j)=0.5_r8*(FE(i,j+1)+FE(i,j))
                END IF
              END DO
            END DO
!
            cff1=1.0_r8/6.0_r8
            cff2=1.0_r8/3.0_r8
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  FE(i,j)=Hvom(i,j,k)*0.5_r8*                           &
     &                    (t(i,j-1,k,3,itrc)+                           &
     &                     t(i,j  ,k,3,itrc))-                          &
     &                    cff1*(curv(i,j-1)*MAX(Hvom(i,j,k),0.0_r8)+    &
     &                          curv(i,j  )*MIN(Hvom(i,j,k),0.0_r8))
                ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.               &
     &                   (Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  FE(i,j)=Hvom(i,j,k)*0.5_r8*                           &
     &                    (t(i,j-1,k,3,itrc)+                           &
     &                     t(i,j  ,k,3,itrc)-                           &
     &                     cff2*(grad(i,j  )-                           &
     &                           grad(i,j-1)))
                END IF
              END DO
            END DO
          END IF HADV_FLUX
!
!  Apply tracers point sources to the horizontal advection terms,
!  if any.
!
!    Dsrc(is) = 0,  flow across grid cell u-face (positive or negative)
!    Dsrc(is) = 1,  flow across grid cell v-face (positive or negative)
!
          IF (LuvSrc(ng)) THEN
            DO is=1,Nsrc(ng)
              Isrc=SOURCES(ng)%Isrc(is)
              Jsrc=SOURCES(ng)%Jsrc(is)
              IF (INT(SOURCES(ng)%Dsrc(is)).eq.0) THEN
                IF ((Hadvection(itrc,ng)%MPDATA).or.                    &
     &              (Hadvection(itrc,ng)%HSIMT)) THEN
                  LapplySrc=(IstrUm2.le.Isrc).and.                      &
     &                      (Isrc.le.Iendp3).and.                       &
     &                      (JstrVm2.le.Jsrc).and.                      &
     &                      (Jsrc.le.Jendp2i)
                ELSE
                  LapplySrc=(Istr.le.Isrc).and.                         &
     &                      (Isrc.le.Iend+1).and.                       &
     &                      (Jstr.le.Jsrc).and.                         &
     &                      (Jsrc.le.Jend)
                END IF
                IF (LapplySrc) THEN
                  IF (LtracerSrc(itrc,ng)) THEN
                    FX(Isrc,Jsrc)=Huon(Isrc,Jsrc,k)*                    &
     &                            SOURCES(ng)%Tsrc(is,k,itrc)
# ifdef MASKING
                  ELSE
                    IF ((rmask(Isrc  ,Jsrc).eq.0.0_r8).and.             &
     &                  (rmask(Isrc-1,Jsrc).eq.1.0_r8)) THEN
                      FX(Isrc,Jsrc)=Huon(Isrc,Jsrc,k)*                  &
     &                              t(Isrc-1,Jsrc,k,3,itrc)
                    ELSE IF ((rmask(Isrc  ,Jsrc).eq.1.0_r8).and.        &
     &                       (rmask(Isrc-1,Jsrc).eq.0.0_r8)) THEN
                      FX(Isrc,Jsrc)=Huon(Isrc,Jsrc,k)*                  &
     &                              t(Isrc  ,Jsrc,k,3,itrc)
                    END IF
# endif
                  END IF
                END IF
              ELSE IF (INT(SOURCES(ng)%Dsrc(is)).eq.1) THEN
                IF ((Hadvection(itrc,ng)%MPDATA).or.                    &
     &              (Hadvection(itrc,ng)%HSIMT)) THEN
                  LapplySrc=(IstrUm2.le.Isrc).and.                      &
     &                      (Isrc.le.Iendp2i).and.                      &
     &                      (JstrVm2.le.Jsrc).and.                      &
     &                      (Jsrc.le.Jendp3)
                ELSE
                  LapplySrc=(Istr.le.Isrc).and.                         &
     &                      (Isrc.le.Iend).and.                         &
     &                      (Jstr.le.Jsrc).and.                         &
     &                      (Jsrc.le.Jend+1)
                END IF
                IF (LapplySrc) THEN
                  IF (LtracerSrc(itrc,ng)) THEN
                    FE(Isrc,Jsrc)=Hvom(Isrc,Jsrc,k)*                    &
     &                            SOURCES(ng)%Tsrc(is,k,itrc)
# ifdef MASKING
                  ELSE
                    IF ((rmask(Isrc,Jsrc  ).eq.0.0_r8).and.             &
     &                  (rmask(Isrc,Jsrc-1).eq.1.0_r8)) THEN
                      FE(Isrc,Jsrc)=Hvom(Isrc,Jsrc,k)*                  &
     &                              t(Isrc,Jsrc-1,k,3,itrc)
                    ELSE IF ((rmask(Isrc,Jsrc  ).eq.1.0_r8).and.        &
     &                       (rmask(Isrc,Jsrc-1).eq.0.0_r8)) THEN
                      FE(Isrc,Jsrc)=Hvom(Isrc,Jsrc,k)*                  &
     &                              t(Isrc,Jsrc  ,k,3,itrc)
                    END IF
# endif
                  END IF
                END IF
              END IF
            END DO
          END IF

# if defined NESTING && !defined ONE_WAY
!
!  If refinement grids, extract tracer horizontal advection fluxes
!  (Hz*u*T/n, Hz*v*T/m) at the grid contact boundary (physical
!  domain perimeter) to be used in two-way nesting.
!
          IF (RefinedGrid(ng)) THEN
            DO cr=1,Ncontact
              dg=Rcontact(cr)%donor_grid
              rg=Rcontact(cr)%receiver_grid
              IF (ng.eq.rg) THEN
                CALL bry_fluxes (dg, rg, cr, iNLM, tile,                &
     &                           IminS, ImaxS, JminS, JmaxS,            &
     &                           ILB, IUB, JLB, JUB,                    &
     &                           dt(ng), FX, FE,                        &
     &                          BRY_CONTACT(iwest, cr)%Tflux(:,k,itrc), &
     &                          BRY_CONTACT(ieast, cr)%Tflux(:,k,itrc), &
     &                          BRY_CONTACT(isouth,cr)%Tflux(:,k,itrc), &
     &                          BRY_CONTACT(inorth,cr)%Tflux(:,k,itrc))
              END IF
            END DO
          END IF
# endif
!
!  If MPDATA, time-step horizontal advection for intermediate diffusive
!  tracer, Ta (m Tunits).
!
          HADV_STEPPING : IF (Hadvection(itrc,ng)%MPDATA) THEN
            DO j=JstrVm2,Jendp2i
              DO i=IstrUm2,Iendp2i
                cff=dt(ng)*pm(i,j)*pn(i,j)
                cff1=cff*(FX(i+1,j)-FX(i,j))
                cff2=cff*(FE(i,j+1)-FE(i,j))
                cff3=cff1+cff2
                Ta(i,j,k,itrc)=t(i,j,k,nnew,itrc)-cff3
# ifdef DIAGNOSTICS_TS
                Dhadv(i,j,iTxadv)=-cff1
                Dhadv(i,j,iTyadv)=-cff2
                Dhadv(i,j,iThadv)=-cff3
# endif
              END DO
            END DO
# ifdef DIAGNOSTICS_TS
!
            DO j=Jstr,Jend
              DO i=Istr,Iend
                DiaTwrk(i,j,k,itrc,iTxadv)=Dhadv(i,j,iTxadv)
                DiaTwrk(i,j,k,itrc,iTyadv)=Dhadv(i,j,iTyadv)
                DiaTwrk(i,j,k,itrc,iThadv)=Dhadv(i,j,iThadv)
              END DO
            END DO
# endif
!
!  OTHERWISE, time-step horizontal advection term.  Advective fluxes
!  have units of Tunits m3/s.  The new tracer has units of m Tunits.
!
          ELSE
            DO j=Jstr,Jend
              DO i=Istr,Iend
                cff=dt(ng)*pm(i,j)*pn(i,j)
                cff1=cff*(FX(i+1,j)-FX(i,j))
                cff2=cff*(FE(i,j+1)-FE(i,j))
                cff3=cff1+cff2
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-cff3
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTxadv)=-cff1
                DiaTwrk(i,j,k,itrc,iTyadv)=-cff2
                DiaTwrk(i,j,k,itrc,iThadv)=-cff3
# endif
              END DO
            END DO
          END IF HADV_STEPPING
        END DO K_LOOP
      END DO T_LOOP1
!
!-----------------------------------------------------------------------
!  Time-step vertical advection term.
!-----------------------------------------------------------------------
!
      T_LOOP2 : DO itrc=1,NT(ng)
        IF (Vadvection(itrc,ng)%MPDATA) THEN
          JminT=JstrVm2
          JmaxT=Jendp2i
        ELSE
          JminT=Jstr
          JmaxT=Jend
        END IF
!
        J_LOOP1 : DO j=JminT,JmaxT              ! start pipelined J-loop
!
          VADV_FLUX : IF (Vadvection(itrc,ng)%SPLINES) THEN
!
!  Build conservative parabolic splines for the vertical derivatives
!  "FC" of the tracer.  Then, the interfacial "FC" values are
!  converted to vertical advective flux (Tunits m3/s).
!
            DO i=Istr,Iend
# ifdef NEUMANN
              FC(i,0)=1.5_r8*t(i,j,1,3,itrc)
              CF(i,1)=0.5_r8
# else
              FC(i,0)=2.0_r8*t(i,j,1,3,itrc)
              CF(i,1)=1.0_r8
# endif
            END DO
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff=1.0_r8/(2.0_r8*Hz(i,j,k)+                           &
     &                      Hz(i,j,k+1)*(2.0_r8-CF(i,k)))
                CF(i,k+1)=cff*Hz(i,j,k)
                FC(i,k)=cff*(3.0_r8*(Hz(i,j,k  )*t(i,j,k+1,3,itrc)+     &
     &                               Hz(i,j,k+1)*t(i,j,k  ,3,itrc))-    &
     &                       Hz(i,j,k+1)*FC(i,k-1))
              END DO
            END DO
            DO i=Istr,Iend
# ifdef NEUMANN
              FC(i,N(ng))=(3.0_r8*t(i,j,N(ng),3,itrc)-FC(i,N(ng)-1))/   &
     &                    (2.0_r8-CF(i,N(ng)))
# else
              FC(i,N(ng))=(2.0_r8*t(i,j,N(ng),3,itrc)-FC(i,N(ng)-1))/   &
     &                    (1.0_r8-CF(i,N(ng)))
# endif
            END DO
            DO k=N(ng)-1,0,-1
              DO i=Istr,Iend
                FC(i,k)=FC(i,k)-CF(i,k+1)*FC(i,k+1)
#  ifdef WEC_VF
                FC(i,k+1)=(W(i,j,k+1)+W_stokes(i,j,k+1))*FC(i,k+1)
#  else
                FC(i,k+1)=W(i,j,k+1)*FC(i,k+1)
#  endif
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,N(ng))=0.0_r8
              FC(i,0)=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%AKIMA4) THEN
!
!  Fourth-order, Akima vertical advective flux (Tunits m3/s).
!
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                FC(i,k)=t(i,j,k+1,3,itrc)-                              &
     &                  t(i,j,k  ,3,itrc)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=FC(i,1)
              FC(i,N(ng))=FC(i,N(ng)-1)
            END DO
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff=2.0_r8*FC(i,k)*FC(i,k-1)
                IF (cff.gt.eps) THEN
                  CF(i,k)=cff/(FC(i,k)+FC(i,k-1))
                ELSE
                  CF(i,k)=0.0_r8
                END IF
              END DO
            END DO
            cff1=1.0_r8/3.0_r8
            DO k=1,N(ng)-1
              DO i=Istr,Iend
# ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
# else
                FC(i,k)=W(i,j,k)*                                       &
# endif
     &                  0.5_r8*(t(i,j,k  ,3,itrc)+                      &
     &                          t(i,j,k+1,3,itrc)-                      &
     &                          cff1*(CF(i,k+1)-CF(i,k)))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%CENTERED2) THEN
!
!  Second-order, central differences vertical advective flux
!  (Tunits m3/s).
!
            DO k=1,N(ng)-1
              DO i=Istr,Iend
# ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
# else
                FC(i,k)=W(i,j,k)*                                       &
# endif
     &                  0.5_r8*(t(i,j,k  ,3,itrc)+                      &
     &                          t(i,j,k+1,3,itrc))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%MPDATA) THEN
!
!  First_order, upstream differences vertical advective flux
!  (Tunits m3/s).
!
            DO i=IstrUm2,Iendp2i
              DO k=1,N(ng)-1
# ifdef WEC_VF
                cff1=MAX(W(i,j,k)+W_stokes(i,j,k),0.0_r8)
                cff2=MIN(W(i,j,k)+W_stokes(i,j,k),0.0_r8)
# else
                cff1=MAX(W(i,j,k),0.0_r8)
                cff2=MIN(W(i,j,k),0.0_r8)
# endif
                FC(i,k)=cff1*t(i,j,k  ,3,itrc)+                         &
     &                  cff2*t(i,j,k+1,3,itrc)
              END DO
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%HSIMT) THEN
!
!  Third High-order Spatial Interpolation at the Middle Temporal level
!  (HSIMT; Wu and Zhu, 2010) with a Total Variation Diminishing (TVD)
!  limiter vertical advection flux (Tunits m3/s).
!
            DO i=Istr,Iend
              KaZ(0)=0.0_r8
              oKaZ(0)=0.0_r8
              gradZ(0)=0.0_r8
              DO k=1,N(ng)-1
                cff=pm(i,j)*pn(i,j)*dt(ng)
# ifdef WEC_VF
                KaZ(k)=1.0_r8-ABS(cff*(W(i,j,k)+W_stokes(i,j,k))/       &
     &                            (z_r(i,j,k+1)-z_r(i,j,k)))
# else
                KaZ(k)=1.0_r8-ABS(cff*W(i,j,k)/                         &
     &                            (z_r(i,j,k+1)-z_r(i,j,k)))
# endif
                oKaZ(k)=1.0_r8/KaZ(k)
                gradZ(k)=t(i,j,k+1,3,itrc)-t(i,j,k,3,itrc)
              END DO
              KaZ(N(ng))=0.0_r8
              okaZ(N(ng))=0.0_r8
              gradZ(N(ng))=0.0_r8
!
              DO k=1,N(ng)-1
# ifdef WEC_VF
                cff1=W(i,j,k)+W_stokes(i,j,k)
# else
                cff1=W(i,j,k)
# endif
                IF ((k.eq.1).and.(cff1.ge.0.0_r8)) THEN
                  FC(i,k)=cff1*t(i,j,k,3,itrc)
                ELSE IF ((k.eq.N(ng)-1).and.(cff1.lt.0.0_r8)) THEN
                  FC(i,k)=cff1*t(i,j,k+1,3,itrc)
                ELSE
                  IF (cff1.ge.0) THEN
                    IF (ABS(gradZ(k)).le.eps1) THEN
                      rD=0.0_r8
                      rkaD=0.0_r8
                    ELSE
                      rD=gradZ(k-1)/gradZ(k)
                      rkaD=KaZ(k-1)*oKaZ(k)
                    END IF
                    a1= cc1*KaZ(k)+cc2-cc3*oKaZ(k)
                    b1=-cc1*KaZ(k)+cc2+cc3*oKaZ(k)
                    betaD=a1+b1*rD
                    cff=0.5_r8*MAX(0.0_r8,                              &
     &                             MIN(2.0_r8, 2.0_r8*rD*rkaD, betaD))* &
     &                  gradZ(k)*KaZ(k)
                    sw=t(i,j,k,3,itrc)+cff
                  ELSE
                    IF (ABS(gradZ(k)).le.eps1) THEN
                      rU=0.0_r8
                      rkaU=0.0_r8
                    ELSE
                      rU=gradZ(k+1)/gradZ(k)
                      rkaU=KaZ(k+1)*oKaZ(k)
                    END IF
                    a1= cc1*KaZ(k)+cc2-cc3*oKaZ(k)
                    b1=-cc1*KaZ(k)+cc2+cc3*oKaZ(k)
                    betaU=a1+b1*rU
                    cff=0.5_r8*MAX(0.0_r8,                              &
     &                             MIN(2.0_r8, 2.0_r8*rU*rkaU, betaU))* &
     &                  gradZ(k)*KaZ(k)
                    sw=t(i,j,k+1,3,itrc)-cff
                  END IF
                  FC(i,k)=cff1*sw
                END IF
              END DO
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF ((Vadvection(itrc,ng)%CENTERED4).or.                  &
     &             (Vadvection(itrc,ng)%SPLIT_U3)) THEN
!
!  Fourth-order, central differences vertical advective flux
!  (Tunits m3/s).
!
            cff1=0.5_r8
            cff2=7.0_r8/12.0_r8
            cff3=1.0_r8/12.0_r8
            DO k=2,N(ng)-2
              DO i=Istr,Iend
# ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
# else
                FC(i,k)=W(i,j,k)*                                       &
# endif
     &                  (cff2*(t(i,j,k  ,3,itrc)+                       &
     &                         t(i,j,k+1,3,itrc))-                      &
     &                   cff3*(t(i,j,k-1,3,itrc)+                       &
     &                         t(i,j,k+2,3,itrc)))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
# ifdef WEC_VF
              FC(i,1)=(W(i,j,1)+W_stokes(i,j,1))*                       &
# else
              FC(i,1)=W(i,j,1)*                                         &
# endif
     &                (cff1*t(i,j,1,3,itrc)+                            &
     &                 cff2*t(i,j,2,3,itrc)-                            &
     &                 cff3*t(i,j,3,3,itrc))
# ifdef WEC_VF
              FC(i,N(ng)-1)=(W(i,j,N(ng)-1)+W_stokes(i,j,N(ng)-1))*     &
# else
              FC(i,N(ng)-1)=W(i,j,N(ng)-1)*                             &
# endif
     &                    (cff1*t(i,j,N(ng)  ,3,itrc)+                  &
     &                     cff2*t(i,j,N(ng)-1,3,itrc)-                  &
     &                     cff3*t(i,j,N(ng)-2,3,itrc))
              FC(i,N(ng))=0.0_r8
            END DO
          END IF VADV_FLUX
!
!  If MPDATA and cell-centered point source (LwSrc), augment the
!  intermediate tracer, Ta, with the tracer divergence. For other
!  advection schemes LwSrc is applied after the vertical advection
!  is completed (J. Wilkin).
!
!    Dsrc(is) = 2,  flow across grid cell w-face (positive or negative)
!
          IF (LwSrc(ng)) THEN
            IF (Vadvection(itrc,ng)%MPDATA) THEN
              DO is=1,Nsrc(ng)
                IF (INT(SOURCES(ng)%Dsrc(is)).eq.2) THEN
                  Isrc=SOURCES(ng)%Isrc(is)
                  Jsrc=SOURCES(ng)%Jsrc(is)
                  IF (((Istr.le.Isrc).and.(Isrc.le.Iend+1)).and.        &
     &                 ((Jstr.le.Jsrc).and.(Jsrc.le.Jend+1)).and.       &
     &                 (j.eq.Jsrc)) THEN
                    DO k=1,N(ng)
                      cff=dt(ng)*pm(Isrc,Jsrc)*pn(Isrc,Jsrc)
                      IF (LtracerSrc(itrc,ng)) THEN
                        cff3=SOURCES(ng)%Tsrc(is,k,itrc)
                      ELSE
                        cff3=t(Isrc,Jsrc,k,3,itrc)
                      END IF
                      Ta(Isrc,Jsrc,k,itrc)=Ta(Isrc,Jsrc,k,itrc)+        &
     &                                     cff*SOURCES(ng)%Qsrc(is,k)*  &
     &                                     cff3
                    END DO
                  END IF
                END IF
              END DO
            END IF
          END IF

# ifdef SED_MORPH
!
!  If MPDATA, augment the intermediate tracer, Ta, with the tracer
!  divergence. For other advection schemes the bed change is applied
!  after the vertical advection is completed (similar to Lwsrc).
!  Use 1/N to distribute the bed change in each water column layer.
!
          IF (Vadvection(itrc,ng)%MPDATA) THEN
            cff1=1.0_r8/N(ng)
            DO k=1,N(ng)
              DO i=IstrUm2,Iendp2i
                cff=cff1*(bed_thick(i,j,nstp)-bed_thick(i,j,3))
                cff3=t(i,j,k,3,itrc)
                Ta(i,j,k,itrc)=Ta(i,j,k,itrc)-cff*cff3
              END DO
            END DO
          END IF
# endif
!
!  If MPDATA, time-step vertical advection for intermediate diffusive
!  tracer, Ta (Tunits).
# ifdef DIAGNOSTICS_TS
!  Convert units of tracer diagnostic terms to Tunits.
# endif
!
          VADV_STEPPING : IF (Vadvection(itrc,ng)%MPDATA) THEN
            DO i=IstrUm2,Iendp2i
              CF(i,0)=dt(ng)*pm(i,j)*pn(i,j)
            END DO
            DO k=1,N(ng)
              DO i=IstrUm2,Iendp2i
                cff1=CF(i,0)*(FC(i,k)-FC(i,k-1))
                Ta(i,j,k,itrc)=(Ta(i,j,k,itrc)-cff1)*oHz(i,j,k)
# ifdef DIAGNOSTICS_TS
                Dvadv(i,j,k,itrc)=-cff1
# endif
              END DO
            END DO

# ifdef OMEGA_IMPLICIT
!
!  If MPDATA, compute off-diagonal coefficients FC [dt*Wi*pm*pn] for the
!  implicit vertical advection term located at horizontal RHO-points and
!  vertical W-points. Also, set FC at the top and bottom levels.
!
!  It needs to be done after the Wexplicit and before the anti-diffusion
!  steps.
!
            cff=dt(ng)
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff1=cff*pm(i,j)*pn(i,j)
                FCmax(i,k)=MAX(Wi(i,j,k),0.0_r8)*cff1
                FCmin(i,k)=MIN(Wi(i,j,k),0.0_r8)*cff1
              END DO
            END DO
            DO i=Istr,Iend
              FCmax(i,0)=0.0_r8
              FCmin(i,0)=0.0_r8
              FCmax(i,N(ng))=0.0_r8
              FCmin(i,N(ng))=0.0_r8
            END DO
!
!  Compute diagonal matrix coefficients BC and load right-hand-side
!  terms for the tracer equation into DC.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                BC(i,k)=Hz(i,j,k)+FCmax(i,k)-FCmin(i,k-1)
                DC(i,k)=Ta(i,j,k,itrc)*Hz(i,j,k)
              END DO
            END DO
!
!  Solve the tridiagonal system.
!
            DO i=Istr,Iend
              cff=1.0_r8/BC(i,1)
              CF(i,1)=cff*FCmin(i,1)
              DC(i,1)=cff*DC(i,1)
            END DO
            DO k=2,N(ng)-1
              DO i=Istr,Iend
                cff=1.0_r8/(BC(i,k)+FCmax(i,k-1)*CF(i,k-1))
                CF(i,k)=cff*FCmin(i,k)
                DC(i,k)=cff*(DC(i,k)+FCmax(i,k-1)*DC(i,k-1))
              END DO
            END DO
!
!  Compute new solution by back substitution.
!
            DO i=Istr,Iend
#  ifdef DIAGNOSTICS_TS
              cff1=Ta(i,j,N(ng),itrc)*oHz(i,j,N(ng))
#  endif
              cff=1.0_r8/(BC(i,N(ng))+FCmax(i,N(ng)-1)*CF(i,N(ng)-1))
              DC(i,N(ng))=cff*(DC(i,N(ng))+                            &
     &                         FCmax(i,N(ng)-1)*DC(i,N(ng)-1))
              Ta(i,j,N(ng),itrc)=DC(i,N(ng))
#  ifdef DIAGNOSTICS_TS
              DiaTwrk(i,j,N(ng),itrc,iTvadv)=                          &
     &                             DiaTwrk(i,j,N(ng),itrc,iTvadv)+     &
     &                             Ta(i,j,N(ng),itrc)-cff1
#  endif
            END DO
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
#  ifdef DIAGNOSTICS_TS
                cff1=Ta(i,j,k,itrc)*oHz(i,j,k)
#  endif
                DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
                Ta(i,j,k,itrc)=DC(i,k)
#  ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvadv)=DiaTwrk(i,j,k,itrc,iTvadv)+&
     &                                     Ta(i,j,k,itrc)-cff1
#  endif
              END DO
            END DO
# endif
!
!  OTHERWISE, time-step vertical advection term (m Tunits, or Tunits if
!  conservative, parabolic splines diffusion).
# ifdef DIAGNOSTICS_TS
!  Convert units of tracer diagnostic terms to Tunits.
# endif
!
          ELSE
            DO i=Istr,Iend
              CF(i,0)=dt(ng)*pm(i,j)*pn(i,j)
            END DO
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff1=CF(i,0)*(FC(i,k)-FC(i,k-1))
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-cff1
# ifdef SPLINES_VDIFF
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)*oHz(i,j,k)
# endif
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvadv)=-cff1
                DO idiag=1,NDT
                  DiaTwrk(i,j,k,itrc,idiag)=DiaTwrk(i,j,k,itrc,idiag)*  &
     &                                      oHz(i,j,k)
                END DO
# endif
              END DO
            END DO
          END IF VADV_STEPPING
        END DO J_LOOP1
      END DO T_LOOP2
!
!-----------------------------------------------------------------------
!  Compute anti-diffusive velocities to corrected advected tracers
!  using MPDATA recursive method.  Notice that pipelined J-loop ended.
!-----------------------------------------------------------------------
!
      T_LOOP3 : DO itrc=1,NT(ng)
        MPDATA : IF ((Hadvection(itrc,ng)%MPDATA).and.                  &
     &               (Vadvection(itrc,ng)%MPDATA)) THEN
          CALL mpdata_adiff_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
# ifdef MASKING
     &                            rmask, umask, vmask,                  &
# endif
# ifdef WET_DRY
     &                            rmask_wet, umask_wet, vmask_wet,      &
# endif
     &                            pm, pn, omn, om_u, on_v,              &
     &                            z_r, oHz,                             &
     &                            Huon, Hvom, W,                        &
#  ifdef WEC_VF
     &                            W_stokes,                             &
#  endif
#  ifdef OMEGA_IMPLICIT
     &                            Wi,                                   &
#  endif
     &                            t(:,:,:,3,itrc),                      &
     &                            Ta(:,:,:,itrc),  Ua, Va, Wa)
!
!  Compute anti-diffusive corrected advection fluxes.
!
          DO k=1,N(ng)
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                cff1=MAX(Ua(i,j,k),0.0_r8)
                cff2=MIN(Ua(i,j,k),0.0_r8)
                FX(i,j)=(cff1*Ta(i-1,j,k,itrc)+                         &
     &                   cff2*Ta(i  ,j,k,itrc))*                        &
     &                  0.5_r8*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)
              END DO
            END DO
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                cff1=MAX(Va(i,j,k),0.0_r8)
                cff2=MIN(Va(i,j,k),0.0_r8)
                FE(i,j)=(cff1*Ta(i,j-1,k,itrc)+                         &
     &                   cff2*Ta(i,j  ,k,itrc))*                        &
     &                  0.5_r8*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)
              END DO
            END DO
!
!  Time-step corrected horizontal advection (Tunits m).
!
            DO j=Jstr,Jend
              DO i=Istr,Iend
                cff=dt(ng)*pm(i,j)*pn(i,j)
                cff1=cff*(FX(i+1,j)-FX(i,j))
                cff2=cff*(FE(i,j+1)-FE(i,j))
                cff3=cff1+cff2
                t(i,j,k,nnew,itrc)=Ta(i,j,k,itrc)*Hz(i,j,k)-cff3
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTxadv)=DiaTwrk(i,j,k,itrc,iTxadv)-  &
     &                                     cff1
                DiaTwrk(i,j,k,itrc,iTyadv)=DiaTwrk(i,j,k,itrc,iTyadv)-  &
     &                                     cff2
                DiaTwrk(i,j,k,itrc,iThadv)=DiaTwrk(i,j,k,itrc,iThadv)-  &
     &                                     cff3
# endif
              END DO
            END DO
          END DO
!
!  Compute anti-diffusive corrected vertical advection flux.
!
          DO j=Jstr,Jend
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff1=MAX(Wa(i,j,k),0.0_r8)
                cff2=MIN(Wa(i,j,k),0.0_r8)
                FC(i,k)=cff1*Ta(i,j,k  ,itrc)+                          &
     &                  cff2*Ta(i,j,k+1,itrc)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
!  Time-step corrected vertical advection (Tunits).
# ifdef DIAGNOSTICS_TS
!  Convert units of tracer diagnostic terms to Tunits.
# endif
!
            DO i=Istr,Iend
              CF(i,0)=dt(ng)*pm(i,j)*pn(i,j)
            END DO
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff1=CF(i,0)*(FC(i,k)-FC(i,k-1))
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-cff1
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvadv)=Dvadv(i,j,k,itrc)-           &
     &                                     cff1
                DO idiag=1,NDT
                  DiaTwrk(i,j,k,itrc,idiag)=DiaTwrk(i,j,k,itrc,idiag)*  &
     &                                      oHz(i,j,k)
                END DO
# endif
              END DO
            END DO
          END DO
        END IF MPDATA
      END DO T_LOOP3
!
!-----------------------------------------------------------------------
!  Add tracer divergence due to cell-centered (LwSrc) point sources.
!-----------------------------------------------------------------------
!
!  When LTracerSrc is .true. the inflowing concentration is Tsrc.
!  When LtracerSrc is .false. we add tracer mass to compensate for the
!  added volume to keep the receiving cell concentration unchanged.
!  J. Levin (Jupiter Intelligence Inc.) and J. Wilkin
!
!    Dsrc(is) = 2,  flow across grid cell w-face (positive or negative)
!
      IF (LwSrc(ng)) THEN
        DO itrc=1,NT(ng)
          IF (.not.((Hadvection(itrc,ng)%MPDATA).and.                   &
     &              (Vadvection(itrc,ng)%MPDATA))) THEN
            DO is=1,Nsrc(ng)
             IF (INT(SOURCES(ng)%Dsrc(is)).eq.2) THEN
                Isrc=SOURCES(ng)%Isrc(is)
                Jsrc=SOURCES(ng)%Jsrc(is)
                IF (((Istr.le.Isrc).and.(Isrc.le.Iend+1)).and.          &
     &              ((Jstr.le.Jsrc).and.(Jsrc.le.Jend+1))) THEN
                  DO k=1,N(ng)
                    cff=dt(ng)*pm(Isrc,Jsrc)*pn(Isrc,Jsrc)
# ifdef SPLINES_VDIFF
                    cff=cff*oHz(Isrc,Jsrc,k)
# endif
                    IF (LtracerSrc(itrc,ng)) THEN
                      cff3=SOURCES(ng)%Tsrc(is,k,itrc)
                    ELSE
                      cff3=t(Isrc,Jsrc,k,3,itrc)
                    END IF
                    t(Isrc,Jsrc,k,nnew,itrc)=t(Isrc,Jsrc,k,nnew,itrc)+  &
     &                                       cff*SOURCES(ng)%Qsrc(is,k)*&
     &                                       cff3
                  END DO
                END IF
              END IF
            END DO
          END IF
        END DO
      END IF

# if defined SEDIMENT && defined SED_MORPH
!
!  If not MPDATA, add tracer mass to compensate for the added volume to
!  keep the receiving cell concentration unchanged.
!  Use 1/N to distribute the bed change in each water column layer.
!
      cff1=1.0_r8/N(ng)
      DO itrc=1,NT(ng)
        IF (.not.((Hadvection(itrc,ng)%MPDATA).and.                     &
     &            (Vadvection(itrc,ng)%MPDATA))) THEN
          DO j=Jstr,Jend
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff3=t(i,j,k,3,itrc)
#  ifdef SPLINES_VDIFF
                cff3=cff3*oHz(i,j,k)
#  endif
                cff=cff1*(bed_thick(i,j,nstp)-bed_thick(i,j,3))
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-                  &
     &                             cff*cff3
              END DO
            END DO
          END DO
        END IF
      END DO
# endif

# ifdef OMEGA_IMPLICIT
!
!-----------------------------------------------------------------------
!  Add adaptive, Courant-number based implicit vertical advection term.
!-----------------------------------------------------------------------
!
!  Compute off-diagonal coefficients FC [dt*Wi*pm*pn] for the implicit
!  vertical advection term located at horizontal RHO-points and vertical
!  W-points. Also, set FC at the top and bottom levels.
!
!  It needs to be before the Diffusion.
!
      DO j=Jstr,Jend
        DO itrc=1,NT(ng)
          IF (.not.Vadvection(itrc,ng)%MPDATA) THEN
            cff=dt(ng)
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff1=cff*pm(i,j)*pn(i,j)
                FCmax(i,k)=MAX(Wi(i,j,k),0.0_r8)*cff1
                FCmin(i,k)=MIN(Wi(i,j,k),0.0_r8)*cff1
              END DO
            END DO
            DO i=Istr,Iend
              FCmax(i,0)=0.0_r8
              FCmin(i,0)=0.0_r8
              FCmax(i,N(ng))=0.0_r8
              FCmin(i,N(ng))=0.0_r8
            END DO
!
!  Compute diagonal matrix coefficients BC and load right-hand-side
!  terms for the tracer equation into DC.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                BC(i,k)=Hz(i,j,k)+FCmax(i,k)-FCmin(i,k-1)
#  ifdef SPLINES_VDIFF
                DC(i,k)=t(i,j,k,nnew,itrc)*Hz(i,j,k)
#  else
                DC(i,k)=t(i,j,k,nnew,itrc)
#  endif
              END DO
            END DO
!
!  Solve the tridiagonal system.
!
            DO i=Istr,Iend
              cff=1.0_r8/BC(i,1)
              CF(i,1)=cff*FCmin(i,1)
              DC(i,1)=cff*DC(i,1)
            END DO
            DO k=2,N(ng)-1
              DO i=Istr,Iend
                cff=1.0_r8/(BC(i,k)+FCmax(i,k-1)*CF(i,k-1))
                CF(i,k)=cff*FCmin(i,k)
                DC(i,k)=cff*(DC(i,k)+FCmax(i,k-1)*DC(i,k-1))
              END DO
            END DO
!
!  Compute new solution by back substitution.
!
            DO i=Istr,Iend
#  ifdef DIAGNOSTICS_TS
              cff1=t(i,j,N(ng),nnew,itrc)*oHz(i,j,N(ng))
#  endif
              cff=1.0_r8/(BC(i,N(ng))+                                  &
     &                    FCmax(i,N(ng)-1)*CF(i,N(ng)-1))
              DC(i,N(ng))=cff*(DC(i,N(ng))+                             &
     &                         FCmax(i,N(ng)-1)*DC(i,N(ng)-1))
#  ifdef SPLINES_VDIFF
              t(i,j,N(ng),nnew,itrc)=DC(i,N(ng))
#  else
              t(i,j,N(ng),nnew,itrc)=DC(i,N(ng))*Hz(i,j,N(ng))
#  endif
#  ifdef DIAGNOSTICS_TS
              DiaTwrk(i,j,N(ng),itrc,iTvadv)=DiaTwrk(i,j,N(ng),itrc,    &
     &                                               iTvadv)+           &
     &                                       DC(i,N(ng))-cff1
#  endif
            END DO
!
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
#  ifdef DIAGNOSTICS_TS
                cff1=t(i,j,k,nnew,itrc)*oHz(i,j,k)
#  endif
                DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
#  ifdef SPLINES_VDIFF
                t(i,j,k,nnew,itrc)=DC(i,k)
#  else
                t(i,j,k,nnew,itrc)=DC(i,k)*Hz(i,j,k)
#  endif
#  ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvadv)=DiaTwrk(i,j,k,itrc,iTvadv)+  &
     &                                     DC(i,k)-cff1
#  endif
              END DO
            END DO
          END IF
        END DO
      END DO
# endif
!
!-----------------------------------------------------------------------
!  Time-step vertical diffusion term.
!-----------------------------------------------------------------------
!
      J_LOOP2 : DO j=Jstr,Jend                  ! start pipelined J-loop
        DO itrc=1,NT(ng)
          ltrc=MIN(NAT,itrc)

# ifdef SPLINES_VDIFF
          IF (.not.((Hadvection(itrc,ng)%MPDATA).and.                   &
     &              (Vadvection(itrc,ng)%MPDATA))) THEN
!
!  Use conservative, parabolic spline reconstruction of vertical
!  diffusion derivatives.  Then, time step vertical diffusion term
!  implicitly.
!
            cff1=1.0_r8/6.0_r8
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                FC(i,k)=cff1*Hz(i,j,k  )-                               &
     &                  dt(ng)*Akt(i,j,k-1,ltrc)*oHz(i,j,k  )
                CF(i,k)=cff1*Hz(i,j,k+1)-                               &
     &                  dt(ng)*Akt(i,j,k+1,ltrc)*oHz(i,j,k+1)
              END DO
            END DO
            DO i=Istr,Iend
              CF(i,0)=0.0_r8
              DC(i,0)=0.0_r8
            END DO
!
!  LU decomposition and forward substitution.
!
            cff1=1.0_r8/3.0_r8
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                BC(i,k)=cff1*(Hz(i,j,k)+Hz(i,j,k+1))+                   &
     &                  dt(ng)*Akt(i,j,k,ltrc)*(oHz(i,j,k)+oHz(i,j,k+1))
                cff=1.0_r8/(BC(i,k)-FC(i,k)*CF(i,k-1))
                CF(i,k)=cff*CF(i,k)
                DC(i,k)=cff*(t(i,j,k+1,nnew,itrc)-t(i,j,k,nnew,itrc)-   &
     &                       FC(i,k)*DC(i,k-1))
              END DO
            END DO
!
!  Backward substitution.
!
            DO i=Istr,Iend
              DC(i,N(ng))=0.0_r8
            END DO
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
                DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
              END DO
            END DO
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                DC(i,k)=DC(i,k)*Akt(i,j,k,ltrc)
                cff1=dt(ng)*oHz(i,j,k)*(DC(i,k)-DC(i,k-1))
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+cff1
#  ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvdif)=DiaTwrk(i,j,k,itrc,iTvdif)+  &
     &                                     cff1
#  endif
              END DO
            END DO
          ELSE
# endif
!
!  Compute off-diagonal coefficients FC [lambda*dt*Akt/Hz] for the
!  implicit vertical diffusion terms at future time step, located
!  at horizontal RHO-points and vertical W-points.
!  Also set FC at the top and bottom levels.
!
            cff=-dt(ng)*lambda
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff1=1.0_r8/(z_r(i,j,k+1)-z_r(i,j,k))
                FC(i,k)=cff*cff1*Akt(i,j,k,ltrc)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
!  Compute diagonal matrix coefficients BC and load right-hand-side
!  terms for the tracer equation into DC.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                BC(i,k)=Hz(i,j,k)-FC(i,k)-FC(i,k-1)
                DC(i,k)=t(i,j,k,nnew,itrc)
              END DO
            END DO
!
!  Solve the tridiagonal system.
!
            DO i=Istr,Iend
              cff=1.0_r8/BC(i,1)
              CF(i,1)=cff*FC(i,1)
              DC(i,1)=cff*DC(i,1)
            END DO
            DO k=2,N(ng)-1
              DO i=Istr,Iend
                cff=1.0_r8/(BC(i,k)-FC(i,k-1)*CF(i,k-1))
                CF(i,k)=cff*FC(i,k)
                DC(i,k)=cff*(DC(i,k)-FC(i,k-1)*DC(i,k-1))
              END DO
            END DO
!
!  Compute new solution by back substitution.
!
            DO i=Istr,Iend
# ifdef DIAGNOSTICS_TS
               cff1=t(i,j,N(ng),nnew,itrc)*oHz(i,j,N(ng))
# endif
               DC(i,N(ng))=(DC(i,N(ng))-FC(i,N(ng)-1)*DC(i,N(ng)-1))/   &
     &                     (BC(i,N(ng))-FC(i,N(ng)-1)*CF(i,N(ng)-1))
               t(i,j,N(ng),nnew,itrc)=DC(i,N(ng))
# ifdef DIAGNOSTICS_TS
               DiaTwrk(i,j,N(ng),itrc,iTvdif)=                          &
     &                              DiaTwrk(i,j,N(ng),itrc,iTvdif)+     &
     &                              t(i,j,N(ng),nnew,itrc)-cff1
# endif
            END DO
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
# ifdef DIAGNOSTICS_TS
                cff1=t(i,j,k,nnew,itrc)*oHz(i,j,k)
# endif
                DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
                t(i,j,k,nnew,itrc)=DC(i,k)
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvdif)=DiaTwrk(i,j,k,itrc,iTvdif)+  &
     &                                     t(i,j,k,nnew,itrc)-cff1
# endif
              END DO
            END DO
# ifdef SPLINES_VDIFF
          END IF
# endif
        END DO
      END DO J_LOOP2
```

</details>

### 8.2 完整当前 C++ 函数

下块含完整签名、合法性 abort、全部包装、scratch 与 launch。实现依赖的 `Fields/Slope`、`difference/curvature/horizontal_slope/correction/horizontal/vertical_slope/vertical` 从文件第 1 行至第 148 行完整见附录 B；它们才是 U3/C4 与 HSIMT 通量的实质，不是可忽略的外壳细节。

<a id="src-current-t-chunks"></a>

**source-block ID：`current-t-chunks`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d_t.cpp`；闭区间：**149–239**；行数：**91**；UTF-8 payload SHA256：`0b86075d8ad7dde7ea2b8d84cf23754a509a34c4467925081f21225c41eae7e5`。

<details>
<summary>ROMS/Cpp/kokkos_step3d_t.cpp，149–239（完整原文，可展开）</summary>

<!-- source-block id=current-t-chunks commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d_t.cpp lines=149-239 count=91 sha256=0b86075d8ad7dde7ea2b8d84cf23754a509a34c4467925081f21225c41eae7e5 -->
```cpp
extern "C" void k_step3d_t_chunks(const TracerConfig* config, const int* schemes,
                                  void* pm, void* pn, void* Hz, void* Huon, void* Hvom, void* W,
                                  void* zr, void* Akt, void* tracer, void* diagnostics) {
    roms::initialize();
    ROMS_KK_TIMER("step3d_t chunks");
    const TracerConfig p = *config;
    if (p.N < 3 || p.NT < 1 || p.NAT < 1 || p.nnew < 1 || p.nnew > 2)
        Kokkos::abort("Invalid Kokkos tracer dimensions or predictor alias");
    const int nx = p.Iend - p.Istr + 1, ny = p.Jend - p.Jstr + 1;
    if (nx < 1 || ny < 1)
        Kokkos::abort("Invalid tracer tile");
    for (int it = 0; it < p.NT; ++it)
        if (schemes[it] != 0 && schemes[it] != 1)
            Kokkos::abort("Unsupported tracer scheme");
    constexpr int KB = 32;
    const int per = (KB + 1) + KB + 2 * (p.N + 1) * KB; /* fx, oh, cf, dc */
    double* scratch = nullptr;
#ifndef __sw_slave__
    scratch = workspace.reserve(std::size_t(ny) * p.NT * per);
#endif
    const auto a2 = [&](void* v) { return roms::as_2d<double>(v, p.LBi, p.UBi, p.LBj, p.UBj); };
    const auto a3 = [&](void* v, int low) { return roms::as_3d<double>(v, p.LBi, p.UBi, p.LBj, p.UBj, low, p.N); };
    const Fields f{p, a2(pm), a2(pn), a3(Hz, 1), a3(Huon, 1), a3(Hvom, 1), a3(W, 0), a3(zr, 1),
                   roms::as_4d<double>(Akt, p.LBi, p.UBi, p.LBj, p.UBj, 0, p.N, 1, p.NAT),
                   roms::as_5d<double>(tracer, p.LBi, p.UBi, p.LBj, p.UBj, 1, p.N, 1, 3, 1, p.NT),
                   roms::as_5d<double>(diagnostics, p.LBi, p.UBi, p.LBj, p.UBj, 1, p.N, 1, p.NT, 1, p.NDT)};
    Kokkos::parallel_for("ROMS step3d_t chunks", Kokkos::RangePolicy<roms::exec_space, Kokkos::IndexType<int>>(0, ny * p.NT), KOKKOS_LAMBDA(int index) {
        const int j=p.Jstr+index%ny;
        const int it=1+index/ny;
        const int scheme=schemes[it-1];
        const int N=p.N, nn=p.nnew;
        const int tracer=it<p.NAT?it:p.NAT;
        double* fx=scratch+std::size_t(index)*per;
        double* oh=fx+(KB+1);
        double* cf=oh+KB;
        double* dc=cf+(N+1)*KB;
        for (int ib=p.Istr;ib<=p.Iend;ib+=KB) {
          const int nb=(p.Iend-ib+1<KB)?(p.Iend-ib+1):KB;
          /* Advection and the unit conversion of the diagnostics. */
          for (int k=1;k<=N;++k) {
            for (int b=0;b<=nb;++b) fx[b]=f.horizontal(ib+b,j,k,it,true,scheme);
            for (int b=0;b<nb;++b) {
              const int i=ib+b;
              const double c=p.dt*f.pm(i,j)*f.pn(i,j);
              const double dx=c*(fx[b+1]-fx[b]);
              const double dy=c*(f.horizontal(i,j+1,k,it,false,scheme)-
                                 f.horizontal(i,j,k,it,false,scheme));
              const double d=dx+dy;
              f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)-d;
              const double dz=c*(f.vertical(i,j,k,it,scheme)-f.vertical(i,j,k-1,it,scheme));
              f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)-dz;
              oh[b]=1.0/f.hz(i,j,k);
              f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)*oh[b];
              f.diag(i,j,k,it,p.xadv)=-dx;
              f.diag(i,j,k,it,p.yadv)=-dy;
              f.diag(i,j,k,it,p.hadv)=-d;
              f.diag(i,j,k,it,p.vadv)=-dz;
            }
            for (int id=1;id<=p.NDT;++id)
              for (int b=0;b<nb;++b)
                f.diag(ib+b,j,k,it,id)=f.diag(ib+b,j,k,it,id)*oh[b];
          }
          /* Implicit spline vertical diffusion. */
          for (int b=0;b<nb;++b) { cf[b]=0.0; dc[b]=0.0; }
          for (int k=1;k<N;++k) {
            for (int b=0;b<nb;++b) {
              const int i=ib+b, q=k*KB+b;
              const double o=1.0/f.hz(i,j,k), o1=1.0/f.hz(i,j,k+1);
              const double lower=(1.0/6.0)*f.hz(i,j,k)-p.dt*f.akt(i,j,k-1,tracer)*o;
              const double upper=(1.0/6.0)*f.hz(i,j,k+1)-p.dt*f.akt(i,j,k+1,tracer)*o1;
              const double bb=(1.0/3.0)*(f.hz(i,j,k)+f.hz(i,j,k+1))+
                              p.dt*f.akt(i,j,k,tracer)*(o+o1);
              const double inverse=1.0/(bb-lower*cf[q-KB]);
              cf[q]=inverse*upper;
              dc[q]=inverse*(f.t(i,j,k+1,nn,it)-f.t(i,j,k,nn,it)-lower*dc[q-KB]);
            }
          }
          for (int b=0;b<nb;++b) dc[N*KB+b]=0.0;
          for (int k=N-1;k>=1;--k)
            for (int b=0;b<nb;++b) dc[k*KB+b]=dc[k*KB+b]-cf[k*KB+b]*dc[(k+1)*KB+b];
          for (int k=1;k<=N;++k) {
            for (int b=0;b<nb;++b) {
              const int i=ib+b, q=k*KB+b;
              dc[q]=dc[q]*f.akt(i,j,k,tracer);
              const double change=p.dt*(1.0/f.hz(i,j,k))*(dc[q]-dc[q-KB]);
              f.t(i,j,k,nn,it)=f.t(i,j,k,nn,it)+change;
              f.diag(i,j,k,it,p.vdif)=f.diag(i,j,k,it,p.vdif)+change;
            }
          }
        } });
}
```

</details>

### 8.3 物理与实现解释

**输入/输出。** 读第 3 槽中间浓度、各通量/几何/扩散系数，以及此前过程已更新的 `t(nnew)`；原位把厚度加权新槽推进成最终浓度，并写平流和扩散诊断。第 3 槽保持只读，非输出槽不写。N>=3，NAT/NT>=1，nnew∈{1,2}，每个 scheme 编码只能是 0 或 1；其余资格先在 Fortran 检查。

**scheme 与 stencil。** 0 是 H=UPSTREAM3、V=CENTERED4；1 是 H/V 都 HSIMT。不同 tracer 可分别选这两种配对，但不能任意水平/垂向混搭，也不支持最终 MPDATA、AKIMA4/A4、中心二阶、垂向 SPLINES 等其他原始平流路径。U3 通过差分与曲率 helper 重算局部 stencil，边缘 difference 按非 composite/非周期规则折返；C4 顶底相邻界面用 1/2、7/12、-1/12 的专用公式，FC(0)=FC(N)=0。

**HSIMT 真实 limiter。** `horizontal_slope` 算梯度和 `ka=1-|通量*局部Courant因子|`，入流边缘外侧梯度/ka 按原条件归零；correction 依据迎风邻坡度、ka 比、cc1/cc2/cc3 和 TVD min/max limiter 修正。水平逆 ka 在 `ka<=1e-12` 时置零，否则取带下限倒数；梯度接近零时 r/rka 置零。垂向上下界相邻入流界面直接迎风，其他层使用 `1/s.ka`，**并没有同样的水平 ka 保护**。因此不能宣称已解决所有 CFL 或非有限值情形；合法 Courant/厚度与原物理稳定性要求仍须满足，且不把极端浮点 NaN/Inf 的 min/max 行为推广为全条件逐位等价。

**原版多趟 → chunks。** 原版先全 tile 水平平流，再逐 tracer/行做垂向平流、除 Hz、诊断换算，最后独立垂向扩散；当前一个 `(j,itrc)` 工作项沿 i 分成 32 列，每块逐层先算 `fx[0:nb]`，更新水平散度，再计算垂向通量差，再除 Hz，并对所有 NDT 项逐项乘 oHz。块内 XI 面只算一次共享，块间重复一个公共面；ETA、垂向面和邻坡度 helper 可能重算。所有跨列平流输入只读第 3 槽，所以不读取其他工作项尚未更新的新槽。

**spline 扩散不是直接解 t。** 对 `k=1..N-1`，下对角由 `Hz(k)/6-dt*Akt(k-1)/Hz(k)` 构成，上对角由 `Hz(k+1)/6-dt*Akt(k+1)/Hz(k+1)` 构成，对角为 `(Hz(k)+Hz(k+1))/3 + dt*Akt(k)*(1/Hz(k)+1/Hz(k+1))`。右端是已完成平流并归一化的新浓度层差。CF/DC 从 k=1 向顶前代，端点 DC(0)=DC(N)=0，再从 N-1 到 1 回代；DC 此时是层界面浓度梯度型未知量，乘 Akt 后才是扩散通量，最后按从底向顶的顺序取通量差更新浓度和 vdif 诊断。底界 DC=0 与表界 DC=0 是这一步增量的条件；原外加 tracer 边界通量已在预测显式部分加入，不能解释为整个模型没有表底通量。

**数值顺序与代价。** 水平先 dx、dy，再 d=dx+dy，先减 d、再减 dz、再乘 oHz；诊断先写未除厚度的平流增量，再统一乘 oHz。spline 回代和随后原位乘 Akt/通量差保持源码顺序，不替换为通用矩阵解算器。中间数组较原多趟 tile 工作区更局部，但仍分配 ny*NT 个 host-memory 分片；一个工作项在 N 上有严格递推依赖，不能把 k 完全摊平并行。

**Fortran 共同处理仍存在。** 合格 HSIMT 路径先完成原 nnew 槽 prologue exchange；chunks 只替换计算中段，末尾的年龄源、侧边界、nudging、rate 差、周期/MPI 通信与 floats 相关后处理仍走 Fortran。某段物理在 epilogue 不被 guard 排除不等于它已由当前 chunks 实现，五入口本身的范围必须保持清晰。

<a id="eligibility"></a>

## 9. 编译宏、资格检查、回退与日志

### 9.1 编译门禁与运行时门禁不同

所有 hook 还受原文件 NONLINEAR/SOLVE3D/TS_FIXED 等外围 CPP 控制，Fortran 与 C++ 必须包含一致的 `cppdefs.h`。ROOT_DIR/ROMS_HEADER 等也必须对应同一物理选项集，否则 Fortran 跳过的段可能不等于 C++ 计算的段。

| 路径 | 开启条件 | 实际拒绝 / 回退范围 |
|---|---|---|
| PRE3D 两入口 | `ROMS_KOKKOS && KK_STAGE_PRE3D`；tracer 调用另受 `!TS_FIXED` | C++ 对 MASKING、WET_DRY、WEC_VF、SOLAR_SOURCE、LMD_NONLOCAL、BODYFORCE、`AGE_MEAN && T_PASSIVE`、SINGLE_PRECISION 直接 `#error` |
| STEP3DUV 两入口 | `ROMS_KOKKOS && KK_STAGE_STEP3DUV` | C++ 对 MASKING、WET_DRY、WEC、WEC_VF、OMEGA_IMPLICIT、BODYFORCE、STEP2D_FB_AB3_AM4、STEP2D_FB_LF_AM3、SINGLE_PRECISION 直接 `#error`；两种 SPLINES_VVISC 设置均有实际实现，UV 诊断可开可关 |
| tracer chunks hook | `ROMS_KOKKOS && KK_STAGE_STEP3D_T && SPLINES_VDIFF && DIAGNOSTICS_TS`，且原文件 `!TS_FIXED` | hook 编译后，OMEGA_IMPLICIT、WEC_VF、NESTING、MASKING、WET_DRY、`SEDIMENT && SED_MORPH` 令运行时 `kk_eligible` 保持 FALSE，走整 tile Fortran；不是同 PRE3D 的 `#error` 机制 |
| tracer chunks C++ 定义 | `kokkos_step3d_t.cpp` 的外层只有 `ROMS_KOKKOS` | 编进可执行文件不等于模型一定调用；CMake `ROMS_KOKKOS_STEP3D_T=ON` 才加 `KK_STAGE_STEP3D_T`，默认 OFF；还需满足上面的 hook 宏 |

PRE3D 的 whole-tile 判定逐 tracer 解析两方向，H 支持 CENTERED2、预测 MPDATA/HSIMT 一阶、UPSTREAM3、AKIMA4、CENTERED4/SPLIT_U3；V 支持 SPLINES、AKIMA4、CENTERED2、预测 MPDATA/HSIMT 一阶、CENTERED4/SPLIT_U3。还要求 `.not.LuvSrc`、N>=3、NT<=32。任一 tracer 未识别，**tracer 与 momentum 预测一起回退**。`TS_FIXED` 下不调用 tracer，但 scheme/尺寸判断仍在共同路径，不能凭固定 tracer 便说无需判定。C++ 另对 NT>32 abort；没有同 chunks 一样全面校验 NT/NAT/timelevel/contiguity/halo。

STEP3DUV 宏开启且编译门禁通过后直接替换两段，没有 PRE3D/chunks 式 `kk_eligible` whole-tile runtime fallback。动量点源留在两段之间的 Fortran，并没有因为使用 STEP3DUV 自动禁止点源。它依赖模型保证合法 N、时间层、连续性、bounds、诊断索引与至多 12 个 pair 等契约，不能把缺少防御检查解释成支持任意输入。

### 9.2 chunks 的整 tile 资格

当前 `step3d_t.F:399–507` 在任何加速写入之前决定：

1. nnew 在 1..2、N>=3、无水平 LuvSrc、无垂向 LwSrc；C++ 另要求 NT>=1、NAT>=1、非空 nx/ny，scheme 编码只准 0/1，否则 abort。
2. 所有 tracer 均为 U3/C4 或 HSIMT/HSIMT 配对，否则整个 tile 走原计算；这不是逐 tracer 部分加速后再混写回退。
3. 对 t、DiaTwrk、Hz、Huon、Hvom、Akt、W、z_r、pm、pn 调用 `kk_is_contiguous`。该 helper 不是只检查第一维：它以实际 descriptor/C_LOC 验证每个非平凡维度的 stride，按 8 字节 double 递乘；尺寸为 1 的维不另检查步长。它也不是完整的空数组/边界/alias 验证器。
4. 每个水平边要求通常两格 halo；仅真实物理边缘、非周期、非 composite 的 fold 允许少一格。确切不等式包含 `LBi<=Istr-2+MERGE(1,0,fold_w)`、`UBi>=Iend+2-MERGE(1,0,fold_e)`，j 同理，完整真实表达式见附录 C。不能把所有 tile 一律当作只需一格。
5. 对通过资格的 HSIMT tracer，原 nnew 槽的周期/MPI prologue exchange **先做完**，然后才调用 chunks；之后共同 epilogue 仍运行。

`pre_step3d`/`step3d_uv` 的本地手工入口没有调用 chunks 的 descriptor guard；不能写成“所有五入口都主动验证连续性”。PRECISION/存储也要区分：PRE3D/STEP3DUV 显式拒绝 SINGLE_PRECISION；chunks 这段门禁没有相同 `#error`，但 `double`、`C_DOUBLE`、8 字节 stride 的 ABI 契约仍不可违反。

### 9.3 日志如何判读

- PRE3D 首次回退打印 `pre_step3d: whole-tile Fortran fallback (point sources, scheme, levels or tracer count)`。
- chunks 首次回退打印 `step3d_t: whole-tile Fortran fallback (scheme, physics, layout, halo or time-level guard)`；首次实际加速调用打印 `step3d_t accelerated (first call) tile=` 加 tile 编号。
- 这些 `SAVE` 计数是调用端的首次计数提示，源码没有给每个 tile 单独的计数数组，也没有为它们增加 OpenMP 原子保护；不要把“一条成功日志”推成每个 tile/每个 rank 全覆盖证据。
- 没有 fallback 行，可能是 hook 未编译、TS_FIXED/物理使例程未调用，或该模块没有这种日志；**无回退不等于五入口全部加速**。应结合真实宏、每 rank 后端、加速首次调用以及非零调用计时判定，且不是本次已做的运行验证。
- `KK_STAGE_REMAINDER` 的生成行路径是另一层覆盖，可能处理本手工 kernel 不支持的 Fortran 段。`Kokkos row fallback:` 与 whole-tile fallback 不是同一门禁；本文件不把生成行算进五个手工入口。
- 例如 BL_TEST 的 SOLAR_SOURCE 必须关闭 PRE3D 阶段才可构建该手工组合；不满足 SPLINES_VDIFF/DIAGNOSTICS_TS 的应用即使允许链接 chunks，也未必编译调用 hook。SEAMOUNT 默认 A4/A4 的历史记录明确走回退，而不是任意配置全加速。

完整配置填充、入口调用和边界/epilogue 的原文在[附录 C](#appendix-abi)。

<a id="numerics"></a>

## 10. 数值保序、历史验证与性能证据的边界

### 10.1 保序原则不是性能收益本身

当前源码注释宣称 Serial 可逐位复现，实际需要限定在受支持配置、同输入与相同数值编译条件。正文没有据文本近似把这种意图升级为所有 CPP/边界/极端浮点值的证明。

- 保留关键表达式操作数次序与括号关联；例如水平 dx/dy 合并的先后、预测时间历史组合、逐层黏性通量差分，不任意做代数因式分解或 FMA 改写。
- 保留垂向前代/回代、均值积分 k=1→N（或第 1 层初始化后从 2 起）以及 couple 通量积分 k=N→1 的顺序；不换成非确定性 reduction。
- 保序约束之外，真实优化是跨独立列/行/tracer 调度、32 列工作集、减少大工作场、通量共享和 scratch 复用。原始全 tile 多趟顺序、部分 i→k/HSIMT i→j 遍历已改成块内顺序，这些是算法数据流重排，不是简单翻译器逐行照搬。
- C++ 名称 `zr/hz/nn/it/ib/b`、U/V 共用方向 offset 和 `step=0/1/2` 是接口/局部表示重组；它们不增添新物理时间层。原版所有 CPP 分支并未被移植，例如年龄/solar/nonlocal 预测组合被拒绝，chunks 只接两种平流配对。
- 三对角追赶不做主元选取、未主动检查各分母非零，HSIMT 的垂向 ka 逆没有水平那样的防护；本次没有新边界稳定性或 NaN/Inf 等价性实验。

### 10.2 严格 SW64 数值构建的真实来源

当前 `Compilers/toolchain_SW64.cmake` 给 C++ 和 Fortran `-mieee`，C++17；SW64 kernel 由根 `CMakeLists.txt:394–419` 加 `-mhybrid-coding` 与 `-fpermissive`，当 `ROMS_FAST_MATH=OFF` 时还给 C++ `-fno-tree-vectorize`。后者是已有相同输入动量 replay 的 CPE 自动向量化末位变化经验所对应的保守开关，不是关闭 CPE 并行。开启 FAST_MATH 的另一路只加 alias-check limit，不能当成相同验证条件。

GNU Fortran flags 文件把 `ROMS_FAST_MATH` **默认设为 ON**，Release/RelWithDebInfo 在开启时附加 `-ffast-math`；历史 `build_app.sh:71–78` 明确以 `-DROMS_FAST_MATH=OFF` 构建。不能只看到仓库 toolchain 便认定任意现有可执行文件是严格配置。本次没有编译命令实测或新的 executable，也不凭空声称使用 `-ffp-contract=off` 等未出现在所引来源的选项。相关真实定义见附录 D。

### 10.3 已有数值证据（只引用，不标作本次执行）

| 证据 | 基线、范围与实际记录 | 不能外推的内容 |
|---|---|---|
| 当前直接测试源码及既有结果 | `ROMS/Cpp/test_step3d_t_kokkos.cpp` 对照 `test_step3d_t_reference.F`；已有 `ROMS/Cpp/tools/sw64/README.md:193–205` 记载 Serial/SW64 各 144 PASS、tracer/诊断最大差 0 | 不是五入口分别都有直接 oracle，也不是本次 HEAD 重跑 |
| 直接测试静态覆盖 | 9 列：N=4/7/16/33、nnew=1/2、四种周期开关、四种 mode；37 列跨 32 列边界：N=4/33、两种 nnew、周期 0/3、mode 1/2；两个 tracer 同时用 scheme 0/1。mode 3 另以带主元的稠密消元独立复核 spline 系统 | 比较阈值实际为相对缩放 `1e-12`；非输出时间槽还核实不写。并未覆盖任意 NT、点源或未支持物理，不能把“输出差 0 的历史记录”改写成测试总是严格按位断言 |
| 短积分消融验证 | `docs/kokkos-experiments-2026-11-06.md:151–157`：UPWELLING 41×80、2 ranks、4 步、all output，S1/S2/S3 各对原 Fortran 101/101 字段逐位一致、最大绝对差 0 | S1/S2/S3 是阶段组，不是单入口自证；未移植路径照常运行 |
| 长积分 200 步 | 同记录第 159–186 行：UPWELLING 160×80、1 rank；Serial 与 SW64 互相逐位一致。对原 Fortran 前 15 步一致，第 16 步 zeta 首差 7.6e-19；200 步 100/101 通过归一化 1e-10 门槛，salt_vdiff 因参考近零未过，绝对差 4.3e-19 | 不是全字段对原 Fortran 逐位一致，也不是严格归一化 101/101 PASS |
| 长积分 2000 步 | 同记录第 188–205 行：Serial/SW64 101/101 互相逐位一致；对原 Fortran 仍为 100/101 归一化门槛，salt_vdiff 绝对差 2.4e-19；temp 最大差 3.6e-12、盐输运最大 6.6e-9 | 不应掩盖近零诊断门槛失败；这是整模既有记录 |
| 差异定位消融 | 同记录第 207–247 行：20 步 onset 实验把已观察到的首差定位到 PRSGRD；后加 STEP3DUV、PRE3D 未改变该组记录的差值 | 只说明所测配置中的增量观察，不证明任意配置或任意积分长度的全条件等价 |

原始性能/数值记录可能早于格式化提交；本文引用的是当前 HEAD 中保留的历史文字，不把旧 job 的源码身份偷换为已经新测 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e`。当前直接测试入口和参数可在仓库真实测试文件追查，本文没有执行它们。

### 10.4 消融性能正确读法

既有 `docs/kokkos-experiments-2026-11-06.md:135–149` 记录 UPWELLING 1440×704、12 ranks、两 DT 步、无输出的整模数据：

| 构建 | 阶段组 | 整模 s/完整 DT step | 对原 Fortran 整模加速比 |
|---|---|---:|---:|
| original Fortran | none | 16.946 | 1.00× |
| S1 | 二维 FUSED/TAVG/COUP/MOMT/DIAG/STOR | 5.274 | 3.21× |
| S2 | S1 + UV3DMIX/T3DMIX/PRSGRD/OMEGA/SETZETA/STEP3DUV | 3.614 | 4.69× |
| S3 | S2 + PRE3D | 2.575 | 6.58× |
| S4 | S3 + REMAINDER | 1.832 | 9.25× |

原记录明确 S1/S2/S3 都另行编译了 tracer chunks，GLS 阶段也编译但该应用不启用；这不包括 original Fortran。因此这张表不是“每一行只包含文字列表里的唯一加速入口”。`6.58×` 是增加 PRE3D 后的**整模相对原 Fortran**加速比，不是 pre_t、pre_uv 任一单 kernel 的收益。S2 也包含压力梯度等六个阶段，不能把 4.69× 全归 STEP3DUV。`step3d_uv` 的 Fortran region 34 还与 uv_C2A_grid 嵌套，已有 README 特别指出例程计时口径问题；entry timer 与整模块残余应分开看。

长积分 2000 步既有记录给出原 Fortran 4837.8 s、Serial 5116.6 s、SW64 593.7 s、整模 8.15×；它同样不是五入口中某个函数的速度。

### 10.5 当前 chunks 与未合入实验

本地 `feat/kokkos.share` 在 HEAD 之外存在 tracer row-block sharing 提交 `9e4326546fde6cd5f94abe13d7cec961f4091b5d`。row/team/pipe 等共享实验不属于本文 `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` 的当前 chunks；本次没有切换分支或 worktree，也未借这些实验的测速给当前函数背书。以当前两个 `.cpp` 实际存在的 RangePolicy、KB 和 HostWorkspace 为准。

<a id="appendix-original"></a>

## 11. 附录 A：三个完整原始 Fortran 文件

以下三个块各为统一未移植基线的文件全文，包括完整模块/子程序签名、数组声明、CPP、物理分支、边界、源项和通信。正文重复其中相关计算段是为了逐入口对照；这里的全文用于还原真实外围，不把其余物理自动算入当前 C++ 覆盖。

<a id="src-original-pre-full"></a>

**source-block ID：`original-pre-full`**；commit：`cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`；路径：`ROMS/Nonlinear/pre_step3d.F`；闭区间：**1–1182**；行数：**1182**；UTF-8 payload SHA256：`dae760b92740155c9cec5f086ae502e6638583af96e6bf6f5e09fbc254fc201a`。

<details>
<summary>ROMS/Nonlinear/pre_step3d.F，1–1182（完整原文，可展开）</summary>

<!-- source-block id=original-pre-full commit=cf10c0f7cc270a7075ad19ee3b8950be352a7cd0 path=ROMS/Nonlinear/pre_step3d.F lines=1-1182 count=1182 sha256=dae760b92740155c9cec5f086ae502e6638583af96e6bf6f5e09fbc254fc201a -->
```fortran
#include "cppdefs.h"
      MODULE pre_step3d_mod
#if defined NONLINEAR && defined SOLVE3D
# define NEUMANN
!
!git $Id$
!=======================================================================
!  Copyright (c) 2002-2025 The ROMS Group                              !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                            Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  This subroutine initialize computations for new time step of the    !
!  3D primitive variables.                                             !
!                                                                      !
!  Both n-1 and n-2 time-step contributions of the  Adams/Bashforth    !
!  scheme are added here to u and v at time index "nnew", since the    !
!  right-hand-side  arrays ru and rv at  n-2  will be overwriten in    !
!  subsequent calls to routines within the 3D engine.                  !
!                                                                      !
!  It also computes the time  "n"  vertical viscosity and diffusion    !
!  contributions of the Crank-Nicholson implicit scheme because the    !
!  thicknesses "Hz" will be overwriten at the end of the  2D engine    !
!  (barotropic mode) computations.                                     !
!                                                                      !
!  The actual time step will be carried out in routines "step3d_uv"    !
!  and "step3d_t".                                                     !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: pre_step3d
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE pre_step3d (ng, tile)
!***********************************************************************
!
      USE mod_param
# ifdef DIAGNOSTICS
      USE mod_diags
# endif
      USE mod_forces
      USE mod_grid
      USE mod_mixing
      USE mod_ocean
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
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 22, __LINE__, MyFile)
# endif
      CALL pre_step3d_tile (ng, tile,                                   &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      IminS, ImaxS, JminS, JmaxS,                 &
     &                      nrhs(ng), nstp(ng), nnew(ng),               &
# ifdef MASKING
     &                      GRID(ng) % rmask,                           &
     &                      GRID(ng) % umask,                           &
     &                      GRID(ng) % vmask,                           &
#  if defined SOLAR_SOURCE && defined WET_DRY
     &                      GRID(ng) % rmask_wet,                       &
#  endif
# endif

     &                      GRID(ng) % pm,                              &
     &                      GRID(ng) % pn,                              &
     &                      GRID(ng) % Hz,                              &
     &                      GRID(ng) % Huon,                            &
     &                      GRID(ng) % Hvom,                            &
     &                      GRID(ng) % z_r,                             &
     &                      GRID(ng) % z_w,                             &
     &                      FORCES(ng) % btflx,                         &
     &                      FORCES(ng) % bustr,                         &
     &                      FORCES(ng) % bvstr,                         &
     &                      FORCES(ng) % stflx,                         &
     &                      FORCES(ng) % sustr,                         &
     &                      FORCES(ng) % svstr,                         &
# ifdef SOLAR_SOURCE
     &                      FORCES(ng) % srflx,                         &
# endif
     &                      MIXING(ng) % Akt,                           &
     &                      MIXING(ng) % Akv,                           &
# ifdef LMD_NONLOCAL
     &                      MIXING(ng) % ghats,                         &
# endif
     &                      OCEAN(ng) % W,                              &
# ifdef WEC_VF
     &                      OCEAN(ng) % W_stokes,                       &
# endif
     &                      OCEAN(ng) % ru,                             &
     &                      OCEAN(ng) % rv,                             &
# ifdef DIAGNOSTICS_TS
     &                      DIAGS(ng) % DiaTwrk,                        &
# endif
# ifdef DIAGNOSTICS_UV
     &                      DIAGS(ng) % DiaU3wrk,                       &
     &                      DIAGS(ng) % DiaV3wrk,                       &
     &                      DIAGS(ng) % DiaRU,                          &
     &                      DIAGS(ng) % DiaRV,                          &
# endif
     &                      OCEAN(ng) % t,                              &
     &                      OCEAN(ng) % u,                              &
     &                      OCEAN(ng) % v)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 22, __LINE__, MyFile)
# endif
!
      RETURN
      END SUBROUTINE pre_step3d
!
!***********************************************************************
      SUBROUTINE pre_step3d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
     &                            nrhs, nstp, nnew,                     &
# ifdef MASKING
     &                            rmask, umask, vmask,                  &
#  if defined SOLAR_SOURCE && defined WET_DRY
     &                            rmask_wet,                            &
#  endif
# endif
     &                            pm, pn,                               &
     &                            Hz, Huon, Hvom,                       &
     &                            z_r, z_w,                             &
     &                            btflx, bustr, bvstr,                  &
     &                            stflx, sustr, svstr,                  &
# ifdef SOLAR_SOURCE
     &                            srflx,                                &
# endif
     &                            Akt, Akv,                             &
# ifdef LMD_NONLOCAL
     &                            ghats,                                &
# endif
     &                            W,                                    &
# ifdef WEC_VF
     &                            W_stokes,                             &
# endif
     &                            ru, rv,                               &
# ifdef DIAGNOSTICS_TS
     &                            DiaTwrk,                              &
# endif
# ifdef DIAGNOSTICS_UV
     &                            DiaU3wrk, DiaV3wrk,                   &
     &                            DiaRU, DiaRV,                         &
# endif
     &                            t, u, v)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
      USE mod_sources
!
      USE exchange_3d_mod, ONLY : exchange_r3d_tile
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange4d
# endif
      USE t3dbc_mod, ONLY : t3dbc_tile
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nrhs, nstp, nnew
!
# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
#   if defined SOLAR_SOURCE && defined WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:,LBj:)
#   endif
#  endif
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: Huon(LBi:,LBj:,:)
      real(r8), intent(in) :: Hvom(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: btflx(LBi:,LBj:,:)
      real(r8), intent(in) :: bustr(LBi:,LBj:)
      real(r8), intent(in) :: bvstr(LBi:,LBj:)
      real(r8), intent(in) :: stflx(LBi:,LBj:,:)
      real(r8), intent(in) :: sustr(LBi:,LBj:)
      real(r8), intent(in) :: svstr(LBi:,LBj:)
#  ifdef SOLAR_SOURCE
      real(r8), intent(in) :: srflx(LBi:,LBj:)
#  endif
#  ifdef SUN
      real(r8), intent(in) :: Akt(LBi:UBi,LBj:UBj,0:N(ng),NAT)
#  else
      real(r8), intent(in) :: Akt(LBi:,LBj:,0:,:)
#  endif
      real(r8), intent(in) :: Akv(LBi:,LBj:,0:)
#  ifdef LMD_NONLOCAL
      real(r8), intent(in) :: ghats(LBi:,LBj:,0:,:)
#  endif
      real(r8), intent(in) :: W(LBi:,LBj:,0:)
#  ifdef WEC_VF
      real(r8), intent(in) :: W_stokes(LBi:,LBj:,0:)
#  endif
      real(r8), intent(in) :: ru(LBi:,LBj:,0:,:)
      real(r8), intent(in) :: rv(LBi:,LBj:,0:,:)
#  ifdef DIAGNOSTICS_TS
      real(r8), intent(inout) :: DiaTwrk(LBi:,LBj:,:,:,:)
#  endif
#  ifdef DIAGNOSTICS_UV
      real(r8), intent(inout) :: DiaU3wrk(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaV3wrk(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaRU(LBi:,LBj:,:,:,:)
      real(r8), intent(inout) :: DiaRV(LBi:,LBj:,:,:,:)
#  endif
#  ifdef SUN
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#  else
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)
#  endif
      real(r8), intent(inout) :: u(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: v(LBi:,LBj:,:,:)

# else

#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask(LBi:UBi,LBj:UBj)
#   if defined SOLAR_SOURCE && defined WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:UBi,LBj:UBj)
#   endif
#  endif
      real(r8), intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: Huon(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: Hvom(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:N(ng))
      real(r8), intent(in) :: btflx(LBi:UBi,LBj:UBj,NT(ng))
      real(r8), intent(in) :: bustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: bvstr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: stflx(LBi:UBi,LBj:UBj,NT(ng))
      real(r8), intent(in) :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: svstr(LBi:UBi,LBj:UBj)
#  ifdef SOLAR_SOURCE
      real(r8), intent(in) :: srflx(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: Akt(LBi:UBi,LBj:UBj,0:N(ng),NAT)
      real(r8), intent(in) :: Akv(LBi:UBi,LBj:UBj,0:N(ng))
#  ifdef LMD_NONLOCAL
      real(r8), intent(in) :: ghats(LBi:UBi,LBj:UBj,0:N(ng),NAT)
#  endif
      real(r8), intent(in) :: W(LBi:UBi,LBj:UBj,0:N(ng))
#  ifdef WEC_VF
      real(r8), intent(in) :: W_stokes(LBi:UBi,LBj:UBj,0:N(ng))
#  endif
      real(r8), intent(in) :: ru(LBi:UBi,LBj:UBj,0:N(ng),2)
      real(r8), intent(in) :: rv(LBi:UBi,LBj:UBj,0:N(ng),2)
#  ifdef DIAGNOSTICS_TS
      real(r8), intent(inout) :: DiaTwrk(LBi:UBi,LBj:UBj,N(ng),NT(ng),  &
     &                                   NDT)
#  endif
#  ifdef DIAGNOSTICS_UV
      real(r8), intent(inout) :: DiaU3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d)
      real(r8), intent(inout) :: DiaV3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d)
      real(r8), intent(inout) :: DiaRU(LBi:UBi,LBj:UBj,N(ng),2,NDrhs)
      real(r8), intent(inout) :: DiaRV(LBi:UBi,LBj:UBj,N(ng),2,NDrhs)
#  endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
      real(r8), intent(inout) :: u(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(inout) :: v(LBi:UBi,LBj:UBj,N(ng),2)
# endif
!
!  Local variable declarations.
!
      integer :: Isrc, Jsrc
      integer :: i, ic, indx, is, itrc, j, k, ltrc
# if defined AGE_MEAN && defined T_PASSIVE
      integer :: iage
# endif
# if defined DIAGNOSTICS_TS || defined DIAGNOSTICS_UV
      integer :: idiag
# endif
      real(r8), parameter :: eps = 1.0E-16_r8

      real(r8) :: cff, cff1, cff2, cff3, cff4
      real(r8) :: Gamma

      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: CF
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: DC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC

# ifdef SOLAR_SOURCE
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,0:N(ng)) :: swdk
# endif
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: FE
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: FX
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: curv
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: grad

# include "set_bounds.h"

# ifndef TS_FIXED
!
!=======================================================================
!  Tracer equation(s).
!=======================================================================

#  ifdef SOLAR_SOURCE
!
!  Compute fraction of the solar shortwave radiation, "swdk"
!  (at vertical W-points) penetrating water column.
!
      DO k=1,N(ng)-1
        DO j=Jstr,Jend
          DO i=Istr,Iend
            FX(i,j)=z_w(i,j,N(ng))-z_w(i,j,k)
          END DO
        END DO
        CALL lmd_swfrac_tile (ng, tile,                                 &
     &                        LBi, UBi, LBj, UBj,                       &
     &                        IminS, ImaxS, JminS, JmaxS,               &
     &                        -1.0_r8, FX, FE)
        DO j=Jstr,Jend
          DO i=Istr,Iend
             swdk(i,j,k)=FE(i,j)
          END DO
        END DO
      END DO
#  endif
!
!-----------------------------------------------------------------------
!  Compute intermediate tracer at n+1/2 time-step, t(i,j,k,3,itrc).
!-----------------------------------------------------------------------
!
!  Compute time rate of change of intermediate tracer due to
!  horizontal advection.
!
      T_LOOP1 :DO itrc=1,NT(ng)
        K_LOOP: DO k=1,N(ng)
!
          HADV_FLUX : IF (Hadvection(itrc,ng)%CENTERED2) THEN
!
!  Second-order, centered differences horizontal advective fluxes.
!
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                FX(i,j)=Huon(i,j,k)*                                    &
     &                  0.5_r8*(t(i-1,j,k,nstp,itrc)+                   &
     &                          t(i  ,j,k,nstp,itrc))
              END DO
            END DO
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                FE(i,j)=Hvom(i,j,k)*                                    &
     &                  0.5_r8*(t(i,j-1,k,nstp,itrc)+                   &
     &                          t(i,j  ,k,nstp,itrc))
              END DO
            END DO
!
          ELSE IF ((Hadvection(itrc,ng)%MPDATA).or.                     &
     &             (Hadvection(itrc,ng)%HSIMT)) THEN
!
!  First-order, upstream differences horizontal advective fluxes.
!
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                cff1=MAX(Huon(i,j,k),0.0_r8)
                cff2=MIN(Huon(i,j,k),0.0_r8)
                FX(i,j)=cff1*t(i-1,j,k,nstp,itrc)+                      &
     &                  cff2*t(i  ,j,k,nstp,itrc)
              END DO
            END DO
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                cff1=MAX(Hvom(i,j,k),0.0_r8)
                cff2=MIN(Hvom(i,j,k),0.0_r8)
                FE(i,j)=cff1*t(i,j-1,k,nstp,itrc)+                      &
     &                  cff2*t(i,j  ,k,nstp,itrc)
              END DO
            END DO
!
          ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.                     &
     &             (Hadvection(itrc,ng)%CENTERED4).or.                  &
     &             (Hadvection(itrc,ng)%SPLIT_U3).or.                   &
     &             (Hadvection(itrc,ng)%UPSTREAM3)) THEN
!
!  Fourth-order Akima, fourth-order centered differences, or third-order
!  upstream-biased horizontal advective fluxes.
!
            DO j=Jstr,Jend
              DO i=Istrm1,Iendp2
                FX(i,j)=t(i  ,j,k,nstp,itrc)-                           &
     &                  t(i-1,j,k,nstp,itrc)
#  ifdef MASKING
                FX(i,j)=FX(i,j)*umask(i,j)
#  endif
              END DO
            END DO
            IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
              IF (DOMAIN(ng)%Western_Edge(tile)) THEN
                DO j=Jstr,Jend
                  FX(Istr-1,j)=FX(Istr,j)
                END DO
              END IF
            END IF
            IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
              IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
                DO j=Jstr,Jend
                  FX(Iend+2,j)=FX(Iend+1,j)
                END DO
              END IF
            END IF
!
            DO j=Jstr,Jend
              DO i=Istr-1,Iend+1
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  curv(i,j)=FX(i+1,j)-FX(i,j)
                ELSE IF (Hadvection(itrc,ng)%AKIMA4) THEN
                  cff=2.0_r8*FX(i+1,j)*FX(i,j)
                  IF (cff.gt.eps) THEN
                    grad(i,j)=cff/(FX(i+1,j)+FX(i,j))
                  ELSE
                    grad(i,j)=0.0_r8
                  END IF
                ELSE IF ((Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  grad(i,j)=0.5_r8*(FX(i+1,j)+FX(i,j))
                END IF
              END DO
            END DO
!
            cff1=1.0_r8/6.0_r8
            cff2=1.0_r8/3.0_r8
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  FX(i,j)=Huon(i,j,k)*0.5_r8*                           &
     &                    (t(i-1,j,k,nstp,itrc)+                        &
     &                     t(i  ,j,k,nstp,itrc))-                       &
     &                    cff1*(curv(i-1,j)*MAX(Huon(i,j,k),0.0_r8)+    &
     &                          curv(i  ,j)*MIN(Huon(i,j,k),0.0_r8))
                ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.               &
     &                   (Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN

                  FX(i,j)=Huon(i,j,k)*0.5_r8*                           &
     &                    (t(i-1,j,k,nstp,itrc)+                        &
     &                     t(i  ,j,k,nstp,itrc)-                        &
     &                     cff2*(grad(i  ,j)-                           &
     &                           grad(i-1,j)))
                END IF
              END DO
            END DO
!
            DO j=Jstrm1,Jendp2
              DO i=Istr,Iend
                FE(i,j)=t(i,j  ,k,nstp,itrc)-                           &
     &                  t(i,j-1,k,nstp,itrc)
#  ifdef MASKING
                FE(i,j)=FE(i,j)*vmask(i,j)
#  endif
              END DO
            END DO
            IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
              IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
                DO i=Istr,Iend
                  FE(i,Jstr-1)=FE(i,Jstr)
                END DO
              END IF
            END IF
            IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
              IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
                DO i=Istr,Iend
                  FE(i,Jend+2)=FE(i,Jend+1)
                END DO
              END IF
            END IF
!
            DO j=Jstr-1,Jend+1
              DO i=Istr,Iend
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  curv(i,j)=FE(i,j+1)-FE(i,j)
                ELSE IF (Hadvection(itrc,ng)%AKIMA4) THEN
                  cff=2.0_r8*FE(i,j+1)*FE(i,j)
                  IF (cff.gt.eps) THEN
                    grad(i,j)=cff/(FE(i,j+1)+FE(i,j))
                  ELSE
                   grad(i,j)=0.0_r8
                  END IF
                ELSE IF ((Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  grad(i,j)=0.5_r8*(FE(i,j+1)+FE(i,j))
                END IF
              END DO
            END DO
!
            cff1=1.0_r8/6.0_r8
            cff2=1.0_r8/3.0_r8
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  FE(i,j)=Hvom(i,j,k)*0.5_r8*                           &
     &                    (t(i,j-1,k,nstp,itrc)+                        &
     &                     t(i,j  ,k,nstp,itrc))-                       &
     &                    cff1*(curv(i,j-1)*MAX(Hvom(i,j,k),0.0_r8)+    &
     &                          curv(i,j  )*MIN(Hvom(i,j,k),0.0_r8))
                ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.               &
     &                   (Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  FE(i,j)=Hvom(i,j,k)*0.5_r8*                           &
     &                    (t(i,j-1,k,nstp,itrc)+                        &
     &                     t(i,j  ,k,nstp,itrc)-                        &
     &                     cff2*(grad(i,j  )-                           &
     &                           grad(i,j-1)))
                END IF
              END DO
            END DO
          END IF HADV_FLUX
!
!  Apply tracers point sources to the horizontal advection terms,
!  if any.
!
!    Dsrc(is) = 0,  flow across grid cell u-face (positive or negative)
!    Dsrc(is) = 1,  flow across grid cell v-face (positive or negative)
!
          IF (LuvSrc(ng)) THEN
            DO is=1,Nsrc(ng)
              Isrc=SOURCES(ng)%Isrc(is)
              Jsrc=SOURCES(ng)%Jsrc(is)
              IF (((Istr.le.Isrc).and.(Isrc.le.Iend+1)).and.            &
     &            ((Jstr.le.Jsrc).and.(Jsrc.le.Jend+1))) THEN
                IF (INT(SOURCES(ng)%Dsrc(is)).eq.0) THEN
                  IF (LtracerSrc(itrc,ng)) THEN
                    FX(Isrc,Jsrc)=Huon(Isrc,Jsrc,k)*                    &
     &                            SOURCES(ng)%Tsrc(is,k,itrc)
                  ELSE
                    FX(Isrc,Jsrc)=0.0_r8
                  END IF
                ELSE IF (INT(SOURCES(ng)%Dsrc(is)).eq.1) THEN
                  IF (LtracerSrc(itrc,ng)) THEN
                    FE(Isrc,Jsrc)=Hvom(Isrc,Jsrc,k)*                    &
     &                            SOURCES(ng)%Tsrc(is,k,itrc)
                  ELSE
                     FE(Isrc,Jsrc)=0.0_r8
                  END IF
                END IF
              END IF
            END DO
          END IF
!
!  Time-step horizontal advection (m Tunits).
!
          IF ((Hadvection(itrc,ng)%MPDATA).or.                          &
     &        (Hadvection(itrc,ng)%HSIMT)) THEN
            Gamma=0.5_r8
          ELSE
            Gamma=1.0_r8/6.0_r8
          END IF
          IF (iic(ng).eq.ntfirst(ng)) THEN
            cff=0.5_r8*dt(ng)
            cff1=1.0_r8
            cff2=0.0_r8
          ELSE
            cff=(1.0_r8-Gamma)*dt(ng)
            cff1=0.5_r8+Gamma
            cff2=0.5_r8-Gamma
          END IF
          DO j=Jstr,Jend
            DO i=Istr,Iend
              t(i,j,k,3,itrc)=Hz(i,j,k)*(cff1*t(i,j,k,nstp,itrc)+       &
     &                                   cff2*t(i,j,k,nnew,itrc))-      &
     &                        cff*pm(i,j)*pn(i,j)*                      &
     &                        (FX(i+1,j)-FX(i,j)+                       &
     &                         FE(i,j+1)-FE(i,j))
            END DO
          END DO
        END DO K_LOOP
      END DO T_LOOP1

#  if defined AGE_MEAN && defined T_PASSIVE
!
!  If inert passive tracer and Mean Age, compute age concentration (even
!  inert index) forced by the right-hand-side term that is concentration
!  of an associated conservative passive tracer (odd inert index).
!  Implemented and tested by W.G. Zhang and J. Wilkin. (m Tunits)
!
      DO itrc=1,NPT,2
        IF (.not.((Hadvection(inert(itrc),ng)%MPDATA).or.               &
     &            (Hadvection(inert(itrc),ng)%HSIMT))) THEN
          IF (iic(ng).eq.ntfirst(ng)) THEN
            cff=0.5_r8*dt(ng)
          ELSE
            Gamma=1.0_r8/6.0_r8
            cff=(1.0_r8-Gamma)*dt(ng)
          END IF
          iage=inert(itrc+1)                   ! even inert tracer index
          DO k=1,N(ng)
            DO j=Jstr,Jend
              DO i=Istr,Iend
                t(i,j,k,3,iage)=t(i,j,k,3,iage)+                        &
     &                          cff*Hz(i,j,k)*                          &
     &                          t(i,j,k,nnew,inert(itrc))
              END DO
            END DO
          END DO
        END IF
      END DO
#  endif
!
!-----------------------------------------------------------------------
!  Compute time rate of change of intermediate tracer due to vertical
!  advection.  Impose artificial continuity equation.
!-----------------------------------------------------------------------
!
      J_LOOP1 : DO j=Jstr,Jend
        T_LOOP2 : DO itrc=1,NT(ng)
!
          VADV_FLUX : IF (Vadvection(itrc,ng)%SPLINES) THEN
!
!  Build conservative parabolic splines for the vertical derivatives
!  "FC" of the tracer.  Then, the interfacial "FC" values are
!  converted to vertical advective flux.
!
            DO i=Istr,Iend
#  ifdef NEUMANN
              FC(i,0)=1.5_r8*t(i,j,1,nstp,itrc)
              CF(i,1)=0.5_r8
#  else
              FC(i,0)=2.0_r8*t(i,j,1,nstp,itrc)
              CF(i,1)=1.0_r8
#  endif
            END DO
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff=1.0_r8/(2.0_r8*Hz(i,j,k)+                           &
     &                      Hz(i,j,k+1)*(2.0_r8-CF(i,k)))
                CF(i,k+1)=cff*Hz(i,j,k)
                FC(i,k)=cff*(3.0_r8*(Hz(i,j,k  )*t(i,j,k+1,nstp,itrc)+  &
     &                               Hz(i,j,k+1)*t(i,j,k  ,nstp,itrc))- &
     &                       Hz(i,j,k+1)*FC(i,k-1))
              END DO
            END DO
            DO i=Istr,Iend
#  ifdef NEUMANN
              FC(i,N(ng))=(3.0_r8*t(i,j,N(ng),nstp,itrc)-               &
     &                     FC(i,N(ng)-1))/(2.0_r8-CF(i,N(ng)))
#  else
              FC(i,N(ng))=(2.0_r8*t(i,j,N(ng),nstp,itrc)-               &
     &                     FC(i,N(ng)-1))/(1.0_r8-CF(i,N(ng)))
#  endif
            END DO
            DO k=N(ng)-1,0,-1
              DO i=Istr,Iend
                FC(i,k)=FC(i,k)-CF(i,k+1)*FC(i,k+1)
#  ifdef WEC_VF
                FC(i,k+1)=(W(i,j,k+1)+W_stokes(i,j,k+1))*FC(i,k+1)
#  else
                FC(i,k+1)=W(i,j,k+1)*FC(i,k+1)
#  endif
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,N(ng))=0.0_r8
              FC(i,0)=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%AKIMA4) THEN
!
!  Fourth-order, Akima vertical advective flux.
!
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                FC(i,k)=t(i,j,k+1,nstp,itrc)-                           &
     &                  t(i,j,k  ,nstp,itrc)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=FC(i,1)
              FC(i,N(ng))=FC(i,N(ng)-1)
            END DO
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff=2.0_r8*FC(i,k)*FC(i,k-1)
                IF (cff.gt.eps) THEN
                  CF(i,k)=cff/(FC(i,k)+FC(i,k-1))
                ELSE
                  CF(i,k)=0.0_r8
                END IF
              END DO
            END DO
            cff1=1.0_r8/3.0_r8
            DO k=1,N(ng)-1
              DO i=Istr,Iend
#  ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
#  else
                FC(i,k)=W(i,j,k)*                                       &
#  endif
     &                  0.5_r8*(t(i,j,k  ,nstp,itrc)+                   &
     &                          t(i,j,k+1,nstp,itrc)-                   &
     &                          cff1*(CF(i,k+1)-CF(i,k)))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%CENTERED2) THEN
!
!  Second-order, central differences vertical advective flux.
!
            DO k=1,N(ng)-1
              DO i=Istr,Iend
#  ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
#  else
                FC(i,k)=W(i,j,k)*                                       &
#  endif
     &                  0.5_r8*(t(i,j,k  ,nstp,itrc)+                   &
     &                          t(i,j,k+1,nstp,itrc))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF ((Vadvection(itrc,ng)%MPDATA).or.                     &
     &             (Vadvection(itrc,ng)%HSIMT)) THEN
!
!  First_order, upstream differences vertical advective flux.
!
            DO k=1,N(ng)-1
              DO i=Istr,Iend
#  if defined WEC_VF
                cff1=MAX(W(i,j,k)+W_stokes(i,j,k),0.0_r8)
                cff2=MIN(W(i,j,k)+W_stokes(i,j,k),0.0_r8)
#  else
                cff1=MAX(W(i,j,k),0.0_r8)
                cff2=MIN(W(i,j,k),0.0_r8)
#  endif
                FC(i,k)=cff1*t(i,j,k  ,nstp,itrc)+                      &
     &                  cff2*t(i,j,k+1,nstp,itrc)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF ((Vadvection(itrc,ng)%CENTERED4).or.                  &
     &             (Vadvection(itrc,ng)%SPLIT_U3)) THEN
!
!  Fourth-order, central differences vertical advective flux.
!
            cff1=0.5_r8
            cff2=7.0_r8/12.0_r8
            cff3=1.0_r8/12.0_r8
            DO k=2,N(ng)-2
              DO i=Istr,Iend
#  ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
#  else
                FC(i,k)=W(i,j,k)*                                       &
#  endif
     &                  (cff2*(t(i,j,k  ,nstp,itrc)+                    &
     &                         t(i,j,k+1,nstp,itrc))-                   &
     &                   cff3*(t(i,j,k-1,nstp,itrc)+                    &
     &                         t(i,j,k+2,nstp,itrc)))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
#  ifdef WEC_VF
              FC(i,1)=(W(i,j,1)+W_stokes(i,j,1))*                       &
#  else
              FC(i,1)=W(i,j,1)*                                         &
#  endif
     &                (cff1*t(i,j,1,nstp,itrc)+                         &
     &                 cff2*t(i,j,2,nstp,itrc)-                         &
     &                 cff3*t(i,j,3,nstp,itrc))
#  ifdef WEC_VF
              FC(i,N(ng)-1)=(W(i,j,N(ng)-1)+W_stokes(i,j,N(ng)-1))*     &
#  else
              FC(i,N(ng)-1)=W(i,j,N(ng)-1)*                             &
#  endif
     &                      (cff1*t(i,j,N(ng)  ,nstp,itrc)+             &
     &                       cff2*t(i,j,N(ng)-1,nstp,itrc)-             &
     &                       cff3*t(i,j,N(ng)-2,nstp,itrc))
              FC(i,N(ng))=0.0_r8
            END DO
          END IF VADV_FLUX
!
!  Compute artificial continuity equation and load it into private
!  array DC (1/m). It is imposed to preserve tracer constancy. It is
!  not the same for all the tracer advection schemes because of the
!  Gamma value.
!
          IF ((Vadvection(itrc,ng)%MPDATA).or.                          &
     &        (Vadvection(itrc,ng)%HSIMT)) THEN
            Gamma=0.5_r8
          ELSE
            Gamma=1.0_r8/6.0_r8
          END IF
          IF (iic(ng).eq.ntfirst(ng)) THEN
            cff=0.5_r8*dt(ng)
          ELSE
            cff=(1.0_r8-Gamma)*dt(ng)
          END IF
          DO k=1,N(ng)
            DO i=Istr,Iend
              DC(i,k)=1.0_r8/(Hz(i,j,k)-                                &
     &                        cff*pm(i,j)*pn(i,j)*                      &
     &                        (Huon(i+1,j,k)-Huon(i,j,k)+               &
     &                         Hvom(i,j+1,k)-Hvom(i,j,k)+               &
#  ifdef WEC_VF
     &                        (W_stokes(i,j,k)-W_stokes(i,j,k-1))+      &
#  endif
     &                        (W(i,j,k)-W(i,j,k-1))))
            END DO
          END DO
!
! Time-step vertical advection of tracers (Tunits). Impose artificial
! continuity equation.
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              cff1=cff*pm(i,j)*pn(i,j)
              t(i,j,k,3,itrc)=DC(i,k)*                                  &
     &                        (t(i,j,k,3,itrc)-                         &
     &                         cff1*(FC(i,k)-FC(i,k-1)))
            END DO
          END DO
        END DO T_LOOP2
      END DO J_LOOP1
!
!-----------------------------------------------------------------------
!  Start computation of tracers at n+1 time-step, t(i,j,k,nnew,itrc).
!-----------------------------------------------------------------------
!
!  Compute vertical diffusive fluxes "FC" of the tracer fields at
!  current time step n, and at horizontal RHO-points and vertical
!  W-points.  Notice that the vertical diffusion coefficients for
!  passive tracers is the same as that for salinity (ltrc=NAT).
!
      DO j=Jstr,Jend
        cff3=dt(ng)*(1.0_r8-lambda)
        DO itrc=1,NT(ng)
          ltrc=MIN(NAT,itrc)
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              cff=1.0_r8/(z_r(i,j,k+1)-z_r(i,j,k))
              FC(i,k)=cff3*cff*Akt(i,j,k,ltrc)*                         &
     &                (t(i,j,k+1,nstp,itrc)-                            &
     &                 t(i,j,k  ,nstp,itrc))
            END DO
          END DO

#  ifdef LMD_NONLOCAL
!
!  Add in the nonlocal transport flux for unstable (convective)
!  forcing conditions into matrix FC when using the Large et al.
!  KPP scheme. The nonlocal transport is only applied to active
!  tracers.
!
          IF (itrc.le.NAT) THEN
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                FC(i,k)=FC(i,k)-                                        &
     &                  dt(ng)*Akt(i,j,k,itrc)*ghats(i,j,k,itrc)
              END DO
            END DO
          END IF
#  endif
#  ifdef SOLAR_SOURCE
!
!  Add in incoming solar radiation at interior W-points using decay
!  decay penetration function based on Jerlov water type.
!
          IF (itrc.eq.itemp) THEN
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                FC(i,k)=FC(i,k)+                                        &
     &                  dt(ng)*srflx(i,j)*                              &
#   ifdef WET_DRY
     &                  rmask_wet(i,j)*                                 &
#   endif
     &                  swdk(i,j,k)
              END DO
            END DO
          END IF
#  endif
!
!  Apply bottom and surface tracer flux conditions.
!
          DO i=Istr,Iend
            FC(i,0)=dt(ng)*btflx(i,j,itrc)
            FC(i,N(ng))=dt(ng)*stflx(i,j,itrc)
          END DO
!
!  Compute new tracer field (m Tunits).
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              cff1=Hz(i,j,k)*t(i,j,k,nstp,itrc)
              cff2=FC(i,k)-FC(i,k-1)
              t(i,j,k,nnew,itrc)=cff1+cff2
#  ifdef DIAGNOSTICS_TS
              DiaTwrk(i,j,k,itrc,iTrate)=cff1
              DiaTwrk(i,j,k,itrc,iTvdif)=cff2
#  endif
            END DO
          END DO
        END DO
      END DO
# endif /* !TS_FIXED */
!
!=======================================================================
!  3D momentum equation in the XI-direction.
!=======================================================================
!
!  Compute U-component viscous vertical momentum fluxes "FC" at
!  current time-step n, and at horizontal U-points and vertical
!  W-points.
!
      J_LOOP2: DO j=Jstr,Jend
        cff3=dt(ng)*(1.0_r8-lambda)
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            cff=1.0_r8/(z_r(i,j,k+1)+z_r(i-1,j,k+1)-                    &
     &                  z_r(i,j,k  )-z_r(i-1,j,k  ))
            FC(i,k)=cff3*cff*(u(i,j,k+1,nstp)-u(i,j,k,nstp))*           &
     &              (Akv(i,j,k)+Akv(i-1,j,k))
          END DO
        END DO
!
!  Apply bottom and surface stresses, if so is prescribed.
!
        DO i=IstrU,Iend
# ifdef BODYFORCE
          FC(i,0)=0.0_r8
          FC(i,N(ng))=0.0_r8
# else
          FC(i,0)=dt(ng)*bustr(i,j)
          FC(i,N(ng))=dt(ng)*sustr(i,j)
# endif
        END DO
!
!  Compute new U-momentum (m m/s).
!
        cff=dt(ng)*0.25_r8
        DO i=IstrU,Iend
          DC(i,0)=cff*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        END DO
        indx=3-nrhs
        IF (iic(ng).eq.ntfirst(ng)) THEN
          DO k=1,N(ng)
            DO i=IstrU,Iend
              cff1=u(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i-1,j,k))
              cff2=FC(i,k)-FC(i,k-1)
              u(i,j,k,nnew)=cff1+cff2
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M3pgrd
                DiaU3wrk(i,j,k,idiag)=0.0_r8
              END DO
              DiaU3wrk(i,j,k,M3vvis)=cff2
              DiaU3wrk(i,j,k,M3rate)=cff1
# endif
            END DO
          END DO
        ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
          DO k=1,N(ng)
            DO i=IstrU,Iend
              cff1=u(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i-1,j,k))
              cff2=FC(i,k)-FC(i,k-1)
              cff3=0.5_r8*DC(i,0)
              u(i,j,k,nnew)=cff1-                                       &
     &                      cff3*ru(i,j,k,indx)+                        &
     &                      cff2
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M3pgrd
                DiaU3wrk(i,j,k,idiag)=-cff3*DiaRU(i,j,k,indx,idiag)
              END DO
              DiaU3wrk(i,j,k,M3vvis)=cff2
#  ifdef BODYFORCE
              DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)-            &
     &                               cff3*DiaRU(i,j,k,indx,M3vvis)
#  endif
              DiaU3wrk(i,j,k,M3rate)=cff1
# endif
            END DO
          END DO
        ELSE
          cff1= 5.0_r8/12.0_r8
          cff2=16.0_r8/12.0_r8
          DO k=1,N(ng)
            DO i=IstrU,Iend
              cff3=u(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i-1,j,k))
              cff4=FC(i,k)-FC(i,k-1)
              u(i,j,k,nnew)=cff3+                                       &
     &                      DC(i,0)*(cff1*ru(i,j,k,nrhs)-               &
     &                               cff2*ru(i,j,k,indx))+              &
     &                      cff4
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M3pgrd
                DiaU3wrk(i,j,k,idiag)=DC(i,0)*                          &
     &                                (cff1*DiaRU(i,j,k,nrhs,idiag)-    &
     &                                 cff2*DiaRU(i,j,k,indx,idiag))
              END DO
              DiaU3wrk(i,j,k,M3vvis)=cff4
#  ifdef BODYFORCE
              DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)+            &
     &                               DC(i,0)*                           &
     &                               (cff1*DiaRU(i,j,k,nrhs,M3vvis)-    &
     &                                cff2*DiaRU(i,j,k,indx,M3vvis))
#  endif
              DiaU3wrk(i,j,k,M3rate)=cff3
# endif
            END DO
          END DO
        END IF
!
!=======================================================================
!  3D momentum equation in the ETA-direction.
!=======================================================================
!
!  Compute V-component viscous vertical momentum fluxes "FC" at
!  current time-step n, and at horizontal V-points and vertical
!  W-points.
!
        IF (j.ge.JstrV) THEN
          cff3=dt(ng)*(1.0_r8-lambda)
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              cff=1.0_r8/(z_r(i,j,k+1)+z_r(i,j-1,k+1)-                  &
     &                    z_r(i,j,k  )-z_r(i,j-1,k  ))
              FC(i,k)=cff3*cff*(v(i,j,k+1,nstp)-v(i,j,k,nstp))*         &
     &                (Akv(i,j,k)+Akv(i,j-1,k))
            END DO
          END DO
!
!  Apply bottom and surface stresses, if so is prescribed.
!
          DO i=Istr,Iend
# ifdef BODYFORCE
            FC(i,0)=0.0_r8
            FC(i,N(ng))=0.0_r8
# else
            FC(i,0)=dt(ng)*bvstr(i,j)
            FC(i,N(ng))=dt(ng)*svstr(i,j)
# endif
          END DO
!
!  Compute new V-momentum (m m/s).
!
          cff=dt(ng)*0.25_r8
          DO i=Istr,Iend
            DC(i,0)=cff*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
          END DO
          IF (iic(ng).eq.ntfirst(ng)) THEN
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff1=v(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i,j-1,k))
                cff2=FC(i,k)-FC(i,k-1)
                v(i,j,k,nnew)=cff1+cff2
# ifdef DIAGNOSTICS_UV
                DO idiag=1,M3pgrd
                  DiaV3wrk(i,j,k,idiag)=0.0_r8
                END DO
                DiaV3wrk(i,j,k,M3vvis)=cff2
                DiaV3wrk(i,j,k,M3rate)=cff1
# endif
              END DO
            END DO
          ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff1=v(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i,j-1,k))
                cff2=FC(i,k)-FC(i,k-1)
                cff3=0.5_r8*DC(i,0)
                v(i,j,k,nnew)=cff1-                                     &
     &                        cff3*rv(i,j,k,indx)+                      &
     &                        cff2
# ifdef DIAGNOSTICS_UV
                DO idiag=1,M3pgrd
                  DiaV3wrk(i,j,k,idiag)=-cff3*DiaRV(i,j,k,indx,idiag)
                END DO
                DiaV3wrk(i,j,k,M3vvis)=cff2
#  ifdef BODYFORCE
                DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)-          &
     &                                 cff3*DiaRV(i,j,k,indx,M3vvis)
#  endif
                DiaV3wrk(i,j,k,M3rate)=cff1
# endif
              END DO
            END DO
          ELSE
            cff1= 5.0_r8/12.0_r8
            cff2=16.0_r8/12.0_r8
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff3=v(i,j,k,nstp)*0.5_r8*(Hz(i,j,k)+Hz(i,j-1,k))
                cff4=FC(i,k)-FC(i,k-1)
                v(i,j,k,nnew)=cff3+                                     &
     &                        DC(i,0)*(cff1*rv(i,j,k,nrhs)-             &
     &                                 cff2*rv(i,j,k,indx))+            &
     &                        cff4
# ifdef DIAGNOSTICS_UV
                DO idiag=1,M3pgrd
                  DiaV3wrk(i,j,k,idiag)=DC(i,0)*                        &
     &                                  (cff1*DiaRV(i,j,k,nrhs,idiag)-  &
     &                                   cff2*DiaRV(i,j,k,indx,idiag))
                END DO
                DiaV3wrk(i,j,k,M3vvis)=cff4
#  ifdef BODYFORCE
                DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)+          &
     &                                 DC(i,0)*                         &
     &                                 (cff1*DiaRV(i,j,k,nrhs,M3vvis)-  &
     &                                  cff2*DiaRV(i,j,k,indx,M3vvis))
#  endif
                DiaV3wrk(i,j,k,M3rate)=cff3
# endif
              END DO
            END DO
          END IF
        END IF
      END DO J_LOOP2

# ifndef TS_FIXED
!
!=======================================================================
!  Apply intermediate tracers lateral boundary conditions.
!=======================================================================
!
      ic=0
      DO itrc=1,NT(ng)
        IF (LtracerCLM(itrc,ng).and.LnudgeTCLM(itrc,ng)) THEN
          ic=ic+1
        END IF
        CALL t3dbc_tile (ng, tile, itrc, ic,                            &
     &                   LBi, UBi, LBj, UBj, N(ng), NT(ng),             &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   nstp, 3,                                       &
     &                   t)

        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_r3d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj, 1, N(ng),         &
     &                            t(:,:,:,3,itrc))
        END IF
      END DO

#  ifdef DISTRIBUTE
      CALL mp_exchange4d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj, 1, N(ng), 1, NT(ng),      &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    t(:,:,:,3,:))
#  endif
# endif
!
      RETURN
      END SUBROUTINE pre_step3d_tile
#endif
      END MODULE pre_step3d_mod
```

</details>

<a id="src-original-uv-full"></a>

**source-block ID：`original-uv-full`**；commit：`cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`；路径：`ROMS/Nonlinear/step3d_uv.F`；闭区间：**1–1846**；行数：**1846**；UTF-8 payload SHA256：`94a7107047fc94c351a9c874673d1092cfd46be836c2f4634cc0c6fc085a8316`。

<details>
<summary>ROMS/Nonlinear/step3d_uv.F，1–1846（完整原文，可展开）</summary>

<!-- source-block id=original-uv-full commit=cf10c0f7cc270a7075ad19ee3b8950be352a7cd0 path=ROMS/Nonlinear/step3d_uv.F lines=1-1846 count=1846 sha256=94a7107047fc94c351a9c874673d1092cfd46be836c2f4634cc0c6fc085a8316 -->
```fortran
#include "cppdefs.h"
      MODULE step3d_uv_mod

#if defined NONLINEAR && defined SOLVE3D
!
!git $Id$
!=======================================================================
!  Copyright (c) 2002-2025 The ROMS Group                              !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.md                    Alexander F. Shchepetkin   !
!==================================================== John C. Warner ===
!                                                                      !
!  This subroutine time-steps the nonlinear  horizontal  momentum      !
!  equations. The vertical viscosity terms are time-stepped using      !
!  an implicit algorithm.                                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_coupling
# ifdef DIAGNOSTICS_UV
      USE mod_diags
# endif
      USE mod_forces
      USE mod_grid
      USE mod_mixing
      USE mod_ncparam
      USE mod_ocean
      USE mod_scalars
      USE mod_sources
!
      USE exchange_2d_mod
      USE exchange_3d_mod
# ifdef DISTRIBUTE
      USE mp_exchange_mod,   ONLY : mp_exchange2d, mp_exchange3d
# endif
      USE uv_var_change_mod, ONLY : uv_C2A_grid
      USE u3dbc_mod,         ONLY : u3dbc_tile
      USE v3dbc_mod,         ONLY : v3dbc_tile
!
      implicit none
!
      PRIVATE
      PUBLIC  :: step3d_uv
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE step3d_uv (ng, tile)
!***********************************************************************
!
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
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 34, __LINE__, MyFile)
# endif
      CALL step3d_uv_tile (ng, tile,                                    &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     IminS, ImaxS, JminS, JmaxS,                  &
     &                     nrhs(ng), nstp(ng), nnew(ng),                &
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
     &                     knew(ng),                                    &
# endif
# ifdef MASKING
     &                     GRID(ng) % umask,                            &
     &                     GRID(ng) % vmask,                            &
# endif
# ifdef WET_DRY
     &                     GRID(ng) % umask_wet,                        &
     &                     GRID(ng) % vmask_wet,                        &
# endif
     &                     GRID(ng) % om_v,                             &
     &                     GRID(ng) % on_u,                             &
# ifdef OMEGA_IMPLICIT
     &                     GRID(ng) % om_u,                             &
     &                     GRID(ng) % on_v,                             &
# endif
     &                     GRID(ng) % pm,                               &
     &                     GRID(ng) % pn,                               &
     &                     GRID(ng) % Hz,                               &
     &                     GRID(ng) % z_r,                              &
     &                     GRID(ng) % z_w,                              &
     &                     MIXING(ng) % Akv,                            &
     &                     COUPLING(ng) % DU_avg1,                      &
     &                     COUPLING(ng) % DV_avg1,                      &
     &                     COUPLING(ng) % DU_avg2,                      &
     &                     COUPLING(ng) % DV_avg2,                      &
     &                     OCEAN(ng) % ru,                              &
     &                     OCEAN(ng) % rv,                              &
# ifdef DIAGNOSTICS_UV
     &                     DIAGS(ng) % DiaU2wrk,                        &
     &                     DIAGS(ng) % DiaV2wrk,                        &
     &                     DIAGS(ng) % DiaU2int,                        &
     &                     DIAGS(ng) % DiaV2int,                        &
     &                     DIAGS(ng) % DiaU3wrk,                        &
     &                     DIAGS(ng) % DiaV3wrk,                        &
     &                     DIAGS(ng) % DiaRU,                           &
     &                     DIAGS(ng) % DiaRV,                           &
# endif
     &                     OCEAN(ng) % u,                               &
     &                     OCEAN(ng) % v,                               &
     &                     OCEAN(ng) % ubar,                            &
     &                     OCEAN(ng) % vbar,                            &
# ifdef WEC
     &                     OCEAN(ng) % ubar_stokes,                     &
     &                     OCEAN(ng) % vbar_stokes,                     &
     &                     OCEAN(ng) % u_stokes,                        &
     &                     OCEAN(ng) % v_stokes,                        &
# endif
# ifdef OMEGA_IMPLICIT
     &                     OCEAN(ng) % Wi,                              &
# endif
     &                     GRID(ng) % Huon,                             &
     &                     GRID(ng) % Hvom)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 34, __LINE__, MyFile)
# endif
!
      RETURN
      END SUBROUTINE step3d_uv
!
!***********************************************************************
      SUBROUTINE step3d_uv_tile (ng, tile,                              &
     &                           LBi, UBi, LBj, UBj,                    &
     &                           IminS, ImaxS, JminS, JmaxS,            &
     &                           nrhs, nstp, nnew,                      &
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
     &                           knew,                                  &
# endif
# ifdef MASKING
     &                           umask, vmask,                          &
# endif
# ifdef WET_DRY
     &                           umask_wet, vmask_wet,                  &
# endif
     &                           om_v, on_u,                            &
# ifdef OMEGA_IMPLICIT
     &                           om_u, on_v,                            &
# endif
     &                           pm, pn,                                &
     &                           Hz, z_r, z_w,                          &
     &                           Akv,                                   &
     &                           DU_avg1, DV_avg1,                      &
     &                           DU_avg2, DV_avg2,                      &
     &                           ru, rv,                                &
# ifdef DIAGNOSTICS_UV
     &                           DiaU2wrk, DiaV2wrk,                    &
     &                           DiaU2int, DiaV2int,                    &
     &                           DiaU3wrk, DiaV3wrk,                    &
     &                           DiaRU,    DiaRV,                       &
# endif
     &                           u, v,                                  &
     &                           ubar, vbar,                            &
# ifdef WEC
     &                           ubar_stokes, vbar_stokes,              &
     &                           u_stokes, v_stokes,                    &
# endif
# ifdef OMEGA_IMPLICIT
     &                           Wi,                                    &
# endif
     &                           Huon, Hvom)
!***********************************************************************
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nrhs, nstp, nnew
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
      integer, intent(in) :: knew
# endif
!
# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: umask_wet(LBi:,LBj:)
      real(r8), intent(in) :: vmask_wet(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: om_v(LBi:,LBj:)
      real(r8), intent(in) :: on_u(LBi:,LBj:)
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: om_u(LBi:,LBj:)
      real(r8), intent(in) :: on_v(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in) :: Akv(LBi:,LBj:,0:)
      real(r8), intent(in) :: DU_avg1(LBi:,LBj:)
      real(r8), intent(in) :: DV_avg1(LBi:,LBj:)
      real(r8), intent(in) :: DU_avg2(LBi:,LBj:)
      real(r8), intent(in) :: DV_avg2(LBi:,LBj:)
#  ifdef WEC
      real(r8), intent(in) :: ubar_stokes(LBi:,LBj:)
      real(r8), intent(in) :: vbar_stokes(LBi:,LBj:)
#  endif
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: Wi(LBi:,LBj:,0:)
#  endif
      real(r8), intent(inout) :: ru(LBi:,LBj:,0:,:)
      real(r8), intent(inout) :: rv(LBi:,LBj:,0:,:)
#  ifdef DIAGNOSTICS_UV
      real(r8), intent(inout) :: DiaU2wrk(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaV2wrk(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaU2int(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaV2int(LBi:,LBj:,:)
      real(r8), intent(inout) :: DiaU3wrk(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaV3wrk(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: DiaRU(LBi:,LBj:,:,:,:)
      real(r8), intent(inout) :: DiaRV(LBi:,LBj:,:,:,:)
#  endif
      real(r8), intent(inout) :: u(LBi:,LBj:,:,:)
      real(r8), intent(inout) :: v(LBi:,LBj:,:,:)
#  ifdef WEC
      real(r8), intent(inout) :: u_stokes(LBi:,LBj:,:)
      real(r8), intent(inout) :: v_stokes(LBi:,LBj:,:)
#  endif
      real(r8), intent(out) :: ubar(LBi:,LBj:,:)
      real(r8), intent(out) :: vbar(LBi:,LBj:,:)
      real(r8), intent(out) :: Huon(LBi:,LBj:,:)
      real(r8), intent(out) :: Hvom(LBi:,LBj:,:)

# else

#  ifdef MASKING
      real(r8), intent(in) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask(LBi:UBi,LBj:UBj)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: umask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask_wet(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: om_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_u(LBi:UBi,LBj:UBj)
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: om_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_v(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:N(ng))
      real(r8), intent(in) :: Akv(LBi:UBi,LBj:UBj,0:N(ng))
      real(r8), intent(in) :: DU_avg1(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: DV_avg1(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: DU_avg2(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: DV_avg2(LBi:UBi,LBj:UBj)
#  ifdef WEC
      real(r8), intent(in) :: ubar_stokes(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vbar_stokes(LBi:UBi,LBj:UBj)
#  endif
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: Wi(LBi:UBi,LBj:UBj,0:N(ng))
#  endif
      real(r8), intent(inout) :: ru(LBi:UBi,LBj:UBj,0:N(ng),2)
      real(r8), intent(inout) :: rv(LBi:UBi,LBj:UBj,0:N(ng),2)
#  ifdef DIAGNOSTICS_UV
      real(r8), intent(inout) :: DiaU2wrk(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaV2wrk(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaU2int(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaV2int(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout) :: DiaU3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d)
      real(r8), intent(inout) :: DiaV3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d)
      real(r8), intent(inout) :: DiaRU(LBi:UBi,LBj:UBj,N(ng),2,NDrhs)
      real(r8), intent(inout) :: DiaRV(LBi:UBi,LBj:UBj,N(ng),2,NDrhs)
#  endif
      real(r8), intent(inout) :: u(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(inout) :: v(LBi:UBi,LBj:UBj,N(ng),2)
#  ifdef WEC
      real(r8), intent(inout) :: u_stokes(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(inout) :: v_stokes(LBi:UBi,LBj:UBj,N(ng))
#  endif
      real(r8), intent(out) :: ubar(LBi:UBi,LBj:UBj,:)
      real(r8), intent(out) :: vbar(LBi:UBi,LBj:UBj,:)
      real(r8), intent(out) :: Huon(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(out) :: Hvom(LBi:UBi,LBj:UBj,N(ng))
# endif
!
!  Local variable declarations.
!
      integer :: i, idiag, is, j, k
!
      real(r8) :: cff, cff1, cff2
!
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: AK
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: BC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: CF
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: DC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC
# ifdef OMEGA_IMPLICIT
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FCmin
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FCmax
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: WK
# endif
# ifdef WEC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: CFs
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: DCs
# endif
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hzk
      real(r8), dimension(IminS:ImaxS,N(ng)) :: oHz
# ifdef DIAGNOSTICS_UV
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: wrk
      real(r8), dimension(IminS:ImaxS,1:NDM2d-1) :: Dwrk
# endif

# include "set_bounds.h"
!
!-----------------------------------------------------------------------
!  Time step momentum equation in the XI-direction.
!-----------------------------------------------------------------------
!
      DO j=Jstr,Jend
        DO i=IstrU,Iend
          AK(i,0)=0.5_r8*(Akv(i-1,j,0)+                                 &
     &                    Akv(i  ,j,0))
          DO k=1,N(ng)
            AK(i,k)=0.5_r8*(Akv(i-1,j,k)+                               &
     &                      Akv(i  ,j,k))
            Hzk(i,k)=0.5_r8*(Hz(i-1,j,k)+                               &
     &                       Hz(i  ,j,k))
# if defined SPLINES_VVISC || defined DIAGNOSTICS_UV
            oHz(i,k)=1.0_r8/Hzk(i,k)
# endif
          END DO
        END DO
!
!  Time step right-hand-side terms.
!
        IF (iic(ng).eq.ntfirst(ng)) THEN
          cff=0.25_r8*dt(ng)
        ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
          cff=0.25_r8*dt(ng)*3.0_r8/2.0_r8
        ELSE
          cff=0.25_r8*dt(ng)*23.0_r8/12.0_r8
        END IF
        DO i=IstrU,Iend
          DC(i,0)=cff*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        END DO
        DO k=1,N(ng)
          DO i=IstrU,Iend
            u(i,j,k,nnew)=u(i,j,k,nnew)+                                &
     &                    DC(i,0)*ru(i,j,k,nrhs)
# ifdef SPLINES_VVISC
            u(i,j,k,nnew)=u(i,j,k,nnew)*oHz(i,k)
# endif
# ifdef DIAGNOSTICS_UV
            DO idiag=1,M3pgrd
              DiaU3wrk(i,j,k,idiag)=(DiaU3wrk(i,j,k,idiag)+             &
     &                               DC(i,0)*DiaRU(i,j,k,nrhs,idiag))*  &
     &                              oHz(i,k)
            END DO
#  if defined UV_VIS2 || defined UV_VIS4
            DiaU3wrk(i,j,k,M3xvis)=DiaU3wrk(i,j,k,M3xvis)*oHz(i,k)
            DiaU3wrk(i,j,k,M3yvis)=DiaU3wrk(i,j,k,M3yvis)*oHz(i,k)
            DiaU3wrk(i,j,k,M3hvis)=DiaU3wrk(i,j,k,M3hvis)*oHz(i,k)
#  endif
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)*oHz(i,k)
#  ifdef BODYFORCE
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)+              &
     &                             DC(i,0)*DiaRU(i,j,k,nrhs,M3vvis)*    &
     &                             oHz(i,k)
#  endif
            DiaU3wrk(i,j,k,M3rate)=DiaU3wrk(i,j,k,M3rate)*oHz(i,k)
# endif
          END DO
        END DO

# ifdef SPLINES_VVISC
!
!  Use conservative, parabolic spline reconstruction of vertical
!  viscosity derivatives.  Then, time step vertical viscosity term
!  implicitly by solving a tridiagonal system.
!
        cff1=1.0_r8/6.0_r8
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            FC(i,k)=cff1*Hzk(i,k  )-dt(ng)*AK(i,k-1)*oHz(i,k  )
            CF(i,k)=cff1*Hzk(i,k+1)-dt(ng)*AK(i,k+1)*oHz(i,k+1)
          END DO
        END DO
        DO i=IstrU,Iend
          CF(i,0)=0.0_r8
          DC(i,0)=0.0_r8
        END DO
!
!  LU decomposition and forward substitution.
!
        cff1=1.0_r8/3.0_r8
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            BC(i,k)=cff1*(Hzk(i,k)+Hzk(i,k+1))+                         &
     &              dt(ng)*AK(i,k)*(oHz(i,k)+oHz(i,k+1))
            cff=1.0_r8/(BC(i,k)-FC(i,k)*CF(i,k-1))
            CF(i,k)=cff*CF(i,k)
            DC(i,k)=cff*(u(i,j,k+1,nnew)-u(i,j,k,nnew)-                 &
     &                   FC(i,k)*DC(i,k-1))
          END DO
        END DO
!
!  Backward substitution.
!
        DO i=IstrU,Iend
          DC(i,N(ng))=0.0_r8
        END DO
        DO k=N(ng)-1,1,-1
          DO i=IstrU,Iend
            DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
          END DO
        END DO
!
        DO k=1,N(ng)
          DO i=IstrU,Iend
            DC(i,k)=DC(i,k)*AK(i,k)
            cff=dt(ng)*oHz(i,k)*(DC(i,k)-DC(i,k-1))
            u(i,j,k,nnew)=u(i,j,k,nnew)+cff
#  ifdef DIAGNOSTICS_UV
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)+cff
#  endif
          END DO
        END DO
# else
!
!  Compute off-diagonal coefficients [lambda*dt*Akv/Hz] for the
!  implicit vertical viscosity term at horizontal U-points and
!  vertical W-points.
!
        cff=-lambda*dt(ng)/0.5_r8
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            cff1=1.0_r8/(z_r(i,j,k+1)+z_r(i-1,j,k+1)-                   &
     &                   z_r(i,j,k  )-z_r(i-1,j,k  ))
            FC(i,k)=cff*cff1*AK(i,k)
          END DO
        END DO
        DO i=IstrU,Iend
          FC(i,0)=0.0_r8
          FC(i,N(ng))=0.0_r8
        END DO
!
!  Solve the tridiagonal system.
!
        DO k=1,N(ng)
          DO i=IstrU,Iend
            DC(i,k)=u(i,j,k,nnew)
            BC(i,k)=Hzk(i,k)-FC(i,k)-FC(i,k-1)
          END DO
        END DO
        DO i=IstrU,Iend
          cff=1.0_r8/BC(i,1)
          CF(i,1)=cff*FC(i,1)
          DC(i,1)=cff*DC(i,1)
        END DO
        DO k=2,N(ng)-1
          DO i=IstrU,Iend
            cff=1.0_r8/(BC(i,k)-FC(i,k-1)*CF(i,k-1))
            CF(i,k)=cff*FC(i,k)
            DC(i,k)=cff*(DC(i,k)-FC(i,k-1)*DC(i,k-1))
          END DO
        END DO
!
!  Compute new solution by back substitution.
!
        DO i=IstrU,Iend
#  ifdef DIAGNOSTICS_UV
          wrk(i,N(ng))=u(i,j,N(ng),nnew)*oHz(i,N(ng))
#  endif
          DC(i,N(ng))=(DC(i,N(ng))-FC(i,N(ng)-1)*DC(i,N(ng)-1))/        &
     &                (BC(i,N(ng))-FC(i,N(ng)-1)*CF(i,N(ng)-1))
          u(i,j,N(ng),nnew)=DC(i,N(ng))
#  ifdef DIAGNOSTICS_UV
          DiaU3wrk(i,j,N(ng),M3vvis)=DiaU3wrk(i,j,N(ng),M3vvis)+        &
     &                               u(i,j,N(ng),nnew)-wrk(i,N(ng))
#  endif
        END DO
        DO k=N(ng)-1,1,-1
          DO i=IstrU,Iend
#  ifdef DIAGNOSTICS_UV
            wrk(i,k)=u(i,j,k,nnew)*oHz(i,k)
#  endif
            DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
            u(i,j,k,nnew)=DC(i,k)
#  ifdef DIAGNOSTICS_UV
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)+              &
     &                             u(i,j,k,nnew)-wrk(i,k)
#  endif
          END DO
        END DO
# endif
# ifdef OMEGA_IMPLICIT
!
!  Adaptive, Courant-number based implicit vertical advection
!  contribution for u-momentum.
!
        DO i=IstrU,Iend
          WK(i,0)=0.5_r8*(Wi(i-1,j,0)+                                  &
     &                    Wi(i  ,j,0))
          DO k=1,N(ng)
            WK(i,k)=0.5_r8*(Wi(i-1,j,k)+                                &
     &                      Wi(i  ,j,k))
            Hzk(i,k)=0.5_r8*(Hz(i-1,j,k)+                               &
     &                       Hz(i  ,j,k))
          END DO
        END DO
!
!  Compute off-diagonal coefficients [dt*Wi*pm*pn] for the
!  implicit vertical viscosity term at horizontal U-points and
!  vertical W-points.
!
        cff=dt(ng)
        DO k=1,N(ng)-1
          DO i=IstrU,Iend
            cff1=cff/(on_u(i,j)*om_u(i,j))
            FCmax(i,k)=MAX(WK(i,k),0.0_r8)*cff1
            FCmin(i,k)=MIN(WK(i,k),0.0_r8)*cff1
          END DO
        END DO
        DO i=IstrU,Iend
          FCmax(i,0)=0.0_r8
          FCmin(i,0)=0.0_r8
          FCmax(i,N(ng))=0.0_r8
          FCmin(i,N(ng))=0.0_r8
        END DO
!
!  Solve the tridiagonal system.
!
        DO k=1,N(ng)
          DO i=IstrU,Iend
            BC(i,k)=Hzk(i,k)+FCmax(i,k)-FCmin(i,k-1)
            DC(i,k)=u(i,j,k,nnew)*Hzk(i,k)
          END DO
        END DO
        DO i=IstrU,Iend
          cff=1.0_r8/BC(i,1)
          CF(i,1)=cff*FCmin(i,1)
          DC(i,1)=cff*DC(i,1)
        END DO
        DO k=2,N(ng)-1
          DO i=IstrU,Iend
            cff=1.0_r8/(BC(i,k)+FCmax(i,k-1)*CF(i,k-1))
            CF(i,k)=cff*FCmin(i,k)
            DC(i,k)=cff*(DC(i,k)+FCmax(i,k-1)*DC(i,k-1))
          END DO
        END DO
!
!  Compute new solution by back substitution.
!
        DO i=IstrU,Iend
#  ifdef DIAGNOSTICS_UV
          cff1=u(i,j,N(ng),nnew)
#  endif
          cff=1.0_r8/(BC(i,N(ng))+FCmax(i,N(ng)-1)*CF(i,N(ng)-1))
          DC(i,N(ng))=cff*(DC(i,N(ng))+                                 &
     &                     FCmax(i,N(ng)-1)*DC(i,N(ng)-1))
          u(i,j,N(ng),nnew)=DC(i,N(ng))
#  ifdef DIAGNOSTICS_UV
          DiaRU(i,j,N(ng),nrhs,M3vadv)=DiaRU(i,j,N(ng),nrhs,M3vadv)+    &
     &                                 u(i,j,N(ng),nnew)-cff1
#  endif
        END DO
!
        DO k=N(ng)-1,1,-1
          DO i=IstrU,Iend
#  ifdef DIAGNOSTICS_UV
            cff1=u(i,j,k,nnew)
#  endif
            DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
            u(i,j,k,nnew)=DC(i,k)
#  ifdef DIAGNOSTICS_UV
            DiaRU(i,j,k,nrhs,M3vadv)=DiaRU(i,j,k,nrhs,M3vadv)+          &
     &                               u(i,j,k,nnew)-cff1
#  endif
          END DO
        END DO
# endif
!
!  Replace INTERIOR POINTS incorrect vertical mean with more accurate
!  barotropic component, ubar=DU_avg1/(D*on_u). Recall that, D=CF(:,0).
!
        DO i=IstrU,Iend
          CF(i,0)=Hzk(i,1)
          DC(i,0)=u(i,j,1,nnew)*Hzk(i,1)
# ifdef WEC
          DCs(i,0)=u_stokes(i,j,1)*Hzk(i,1)
# endif
# ifdef DIAGNOSTICS_UV
          Dwrk(i,M2pgrd)=DiaU3wrk(i,j,1,M3pgrd)*Hzk(i,1)
          Dwrk(i,M2bstr)=DiaU3wrk(i,j,1,M3vvis)*Hzk(i,1)
#  ifdef UV_COR
          Dwrk(i,M2fcor)=DiaU3wrk(i,j,1,M3fcor)*Hzk(i,1)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
          Dwrk(i,M2xvis)=DiaU3wrk(i,j,1,M3xvis)*Hzk(i,1)
          Dwrk(i,M2yvis)=DiaU3wrk(i,j,1,M3yvis)*Hzk(i,1)
          Dwrk(i,M2hvis)=DiaU3wrk(i,j,1,M3hvis)*Hzk(i,1)
#  endif
#  ifdef UV_ADV
          Dwrk(i,M2xadv)=DiaU3wrk(i,j,1,M3xadv)*Hzk(i,1)
          Dwrk(i,M2yadv)=DiaU3wrk(i,j,1,M3yadv)*Hzk(i,1)
          Dwrk(i,M2hadv)=DiaU3wrk(i,j,1,M3hadv)*Hzk(i,1)
#  endif
#  ifdef WEC_VF
          Dwrk(i,M2hjvf)=DiaU3wrk(i,j,1,M3hjvf)*Hzk(i,1)
          Dwrk(i,M2kvrf)=DiaU3wrk(i,j,1,M3kvrf)*Hzk(i,1)
#   ifdef UV_COR
          Dwrk(i,M2fsco)=DiaU3wrk(i,j,1,M3fsco)*Hzk(i,1)
#   endif
#   ifdef BOTTOM_STREAMING
          Dwrk(i,M2bstm)=DiaU3wrk(i,j,1,M3bstm)*Hzk(i,1)
#   endif
#   ifdef SURFACE_STREAMING
          Dwrk(i,M2sstm)=DiaU3wrk(i,j,1,M3sstm)*Hzk(i,1)
#   endif
          Dwrk(i,M2wrol)=DiaU3wrk(i,j,1,M3wrol)*Hzk(i,1)
          Dwrk(i,M2wbrk)=DiaU3wrk(i,j,1,M3wbrk)*Hzk(i,1)
#  endif
# endif
        END DO
        DO k=2,N(ng)
          DO i=IstrU,Iend
            CF(i,0)=CF(i,0)+Hzk(i,k)
            DC(i,0)=DC(i,0)+u(i,j,k,nnew)*Hzk(i,k)
# ifdef WEC
            DCs(i,0)=DCs(i,0)+u_stokes(i,j,k)*Hzk(i,k)
# endif
# ifdef DIAGNOSTICS_UV
            Dwrk(i,M2pgrd)=Dwrk(i,M2pgrd)+                              &
     &                     DiaU3wrk(i,j,k,M3pgrd)*Hzk(i,k)
            Dwrk(i,M2bstr)=Dwrk(i,M2bstr)+                              &
     &                     DiaU3wrk(i,j,k,M3vvis)*Hzk(i,k)
#  ifdef UV_COR
            Dwrk(i,M2fcor)=Dwrk(i,M2fcor)+                              &
     &                     DiaU3wrk(i,j,k,M3fcor)*Hzk(i,k)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
            Dwrk(i,M2xvis)=Dwrk(i,M2xvis)+                              &
     &                     DiaU3wrk(i,j,k,M3xvis)*Hzk(i,k)
            Dwrk(i,M2yvis)=Dwrk(i,M2yvis)+                              &
     &                     DiaU3wrk(i,j,k,M3yvis)*Hzk(i,k)
            Dwrk(i,M2hvis)=Dwrk(i,M2hvis)+                              &
     &                     DiaU3wrk(i,j,k,M3hvis)*Hzk(i,k)
#  endif
#  ifdef UV_ADV
            Dwrk(i,M2xadv)=Dwrk(i,M2xadv)+                              &
     &                     DiaU3wrk(i,j,k,M3xadv)*Hzk(i,k)
            Dwrk(i,M2yadv)=Dwrk(i,M2yadv)+                              &
     &                     DiaU3wrk(i,j,k,M3yadv)*Hzk(i,k)
            Dwrk(i,M2hadv)=Dwrk(i,M2hadv)+                              &
     &                     DiaU3wrk(i,j,k,M3hadv)*Hzk(i,k)
#  endif
#  ifdef WEC_VF
            Dwrk(i,M2hjvf)=Dwrk(i,M2hjvf)+                              &
     &                     DiaU3wrk(i,j,k,M3hjvf)*Hzk(i,k)
            Dwrk(i,M2kvrf)=Dwrk(i,M2kvrf)+                              &
     &                     DiaU3wrk(i,j,k,M3kvrf)*Hzk(i,k)
#   ifdef UV_COR
            Dwrk(i,M2fsco)=Dwrk(i,M2fsco)+                              &
     &                     DiaU3wrk(i,j,k,M3fsco)*Hzk(i,k)
#   endif
#   ifdef BOTTOM_STREAMING
            Dwrk(i,M2bstm)=Dwrk(i,M2bstm)+                              &
     &                     DiaU3wrk(i,j,k,M3bstm)*Hzk(i,k)
#   endif
#   ifdef SURFACE_STREAMING
            Dwrk(i,M2sstm)=Dwrk(i,M2sstm)+                              &
     &                     DiaU3wrk(i,j,k,M3sstm)*Hzk(i,k)
#   endif
            Dwrk(i,M2wrol)=Dwrk(i,M2wrol)+                              &
     &                     DiaU3wrk(i,j,k,M3wrol)*Hzk(i,k)
            Dwrk(i,M2wbrk)=Dwrk(i,M2wbrk)+                              &
     &                     DiaU3wrk(i,j,k,M3wbrk)*Hzk(i,k)
#  endif
# endif
          END DO
        END DO
        DO i=IstrU,Iend
          cff1=1.0_r8/(CF(i,0)*on_u(i,j))
          DC(i,0)=(DC(i,0)*on_u(i,j)-DU_avg1(i,j))*cff1      ! recursive
# ifdef WEC
          cff2=1.0_r8/CF(i,0)
          DCs(i,0)=DCs(i,0)*cff2-ubar_stokes(i,j)            ! recursive
# endif
# ifdef DIAGNOSTICS_UV
          DO idiag=1,M2pgrd
            Dwrk(i,idiag)=(Dwrk(i,idiag)*on_u(i,j)-                     &
     &                     DiaU2wrk(i,j,idiag))*cff1
          END DO
          Dwrk(i,M2bstr)=(Dwrk(i,M2bstr)*on_u(i,j)-                     &
     &                    DiaU2wrk(i,j,M2bstr)-DiaU2wrk(i,j,M2sstr))*   &
     &                   cff1
# endif
        END DO
!
!  Couple and update new solution.
!
        DO k=1,N(ng)
          DO i=IstrU,Iend
            u(i,j,k,nnew)=u(i,j,k,nnew)-DC(i,0)
# ifdef MASKING
            u(i,j,k,nnew)=u(i,j,k,nnew)*umask(i,j)
# endif
# ifdef WET_DRY
            u(i,j,k,nnew)=u(i,j,k,nnew)*umask_wet(i,j)
            ru(i,j,k,nrhs)=ru(i,j,k,nrhs)*umask_wet(i,j)
# endif
# ifdef WEC
            u_stokes(i,j,k)=u_stokes(i,j,k)-DCs(i,0)
#  ifdef MASKING
            u_stokes(i,j,k)=u_stokes(i,j,k)*umask(i,j)
#  endif
#  ifdef WET_DRY
            u_stokes(i,j,k)=u_stokes(i,j,k)*umask_wet(i,j)
#  endif
# endif
# ifdef DIAGNOSTICS_UV
            DiaU3wrk(i,j,k,M3pgrd)=DiaU3wrk(i,j,k,M3pgrd)-              &
     &                             Dwrk(i,M2pgrd)
            DiaU3wrk(i,j,k,M3vvis)=DiaU3wrk(i,j,k,M3vvis)-              &
     &                             Dwrk(i,M2bstr)
#  ifdef UV_COR
            DiaU3wrk(i,j,k,M3fcor)=DiaU3wrk(i,j,k,M3fcor)-              &
     &                             Dwrk(i,M2fcor)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
            DiaU3wrk(i,j,k,M3xvis)=DiaU3wrk(i,j,k,M3xvis)-              &
     &                             Dwrk(i,M2xvis)
            DiaU3wrk(i,j,k,M3yvis)=DiaU3wrk(i,j,k,M3yvis)-              &
     &                             Dwrk(i,M2yvis)
            DiaU3wrk(i,j,k,M3hvis)=DiaU3wrk(i,j,k,M3hvis)-              &
     &                             Dwrk(i,M2hvis)
#  endif
#  ifdef UV_ADV
            DiaU3wrk(i,j,k,M3xadv)=DiaU3wrk(i,j,k,M3xadv)-              &
     &                             Dwrk(i,M2xadv)
            DiaU3wrk(i,j,k,M3yadv)=DiaU3wrk(i,j,k,M3yadv)-              &
     &                             Dwrk(i,M2yadv)
            DiaU3wrk(i,j,k,M3hadv)=DiaU3wrk(i,j,k,M3hadv)-              &
     &                             Dwrk(i,M2hadv)
#  endif
#  ifdef WEC_VF
            DiaU3wrk(i,j,k,M3hjvf)=DiaU3wrk(i,j,k,M3hjvf)-              &
     &                             Dwrk(i,M2hjvf)
            DiaU3wrk(i,j,k,M3kvrf)=DiaU3wrk(i,j,k,M3kvrf)-              &
     &                             Dwrk(i,M2kvrf)
#   ifdef UV_COR
            DiaU3wrk(i,j,k,M3fsco)=DiaU3wrk(i,j,k,M3fsco)-              &
     &                             Dwrk(i,M2fsco)
#   endif
#   ifdef BOTTOM_STREAMING
            DiaU3wrk(i,j,k,M3bstm)=DiaU3wrk(i,j,k,M3bstm)-              &
     &                             Dwrk(i,M2bstm)
#   endif
#   ifdef SURFACE_STREAMING
            DiaU3wrk(i,j,k,M3sstm)=DiaU3wrk(i,j,k,M3sstm)-              &
     &                             Dwrk(i,M2sstm)
#   endif
            DiaU3wrk(i,j,k,M3wrol)=DiaU3wrk(i,j,k,M3wrol)-              &
     &                             Dwrk(i,M2wrol)
            DiaU3wrk(i,j,k,M3wbrk)=DiaU3wrk(i,j,k,M3wbrk)-              &
     &                             Dwrk(i,M2wbrk)
#  endif
# endif
          END DO
        END DO

# if defined DIAGNOSTICS_UV && defined MASKING
        DO k=1,N(ng)
          DO i=IstrU,Iend
            DO idiag=1,NDM3d
              DiaU3wrk(i,j,k,idiag)=DiaU3wrk(i,j,k,idiag)*umask(i,j)
            END DO
          END DO
        END DO
# endif
!
!-----------------------------------------------------------------------
!  Time step momentum equation in the ETA-direction.
!-----------------------------------------------------------------------
!
        IF (j.ge.JstrV) THEN
          DO i=Istr,Iend
            AK(i,0)=0.5_r8*(Akv(i,j-1,0)+                               &
     &                      Akv(i,j  ,0))
            DO k=1,N(ng)
              AK(i,k)=0.5_r8*(Akv(i,j-1,k)+                             &
     &                        Akv(i,j  ,k))
              Hzk(i,k)=0.5_r8*(Hz(i,j-1,k)+                             &
     &                         Hz(i,j  ,k))
# if defined SPLINES_VVISC || defined DIAGNOSTICS_UV
              oHz(i,k)=1.0_r8/Hzk(i,k)
# endif
            END DO
          END DO
!
!  Time step right-hand-side terms.
!
          IF (iic(ng).eq.ntfirst(ng)) THEN
            cff=0.25_r8*dt(ng)
          ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
            cff=0.25_r8*dt(ng)*3.0_r8/2.0_r8
          ELSE
            cff=0.25_r8*dt(ng)*23.0_r8/12.0_r8
          END IF
          DO i=Istr,Iend
            DC(i,0)=cff*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
          END DO
          DO k=1,N(ng)
            DO i=Istr,Iend
              v(i,j,k,nnew)=v(i,j,k,nnew)+DC(i,0)*rv(i,j,k,nrhs)
# ifdef SPLINES_VVISC
              v(i,j,k,nnew)=v(i,j,k,nnew)*oHz(i,k)
# endif
# ifdef DIAGNOSTICS_UV
              DO idiag=1,M3pgrd
                DiaV3wrk(i,j,k,idiag)=(DiaV3wrk(i,j,k,idiag)+           &
     &                                 DC(i,0)*                         &
     &                                 DiaRV(i,j,k,nrhs,idiag))*        &
     &                                oHz(i,k)
              END DO
#  if defined UV_VIS2 || defined UV_VIS4
              DiaV3wrk(i,j,k,M3xvis)=DiaV3wrk(i,j,k,M3xvis)*oHz(i,k)
              DiaV3wrk(i,j,k,M3yvis)=DiaV3wrk(i,j,k,M3yvis)*oHz(i,k)
              DiaV3wrk(i,j,k,M3hvis)=DiaV3wrk(i,j,k,M3hvis)*oHz(i,k)
#  endif
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)*oHz(i,k)
#  ifdef BODYFORCE
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)+            &
     &                               DC(i,0)*DiaRV(i,j,k,nrhs,M3vvis)*  &
     &                               oHz(i,k)
#  endif
              DiaV3wrk(i,j,k,M3rate)=DiaV3wrk(i,j,k,M3rate)*oHz(i,k)
# endif
            END DO
          END DO

# ifdef SPLINES_VVISC
!
!  Use conservative, parabolic spline reconstruction of vertical
!  viscosity derivatives.  Then, time step vertical viscosity term
!  implicitly by solving a tridiagonal system.
!
          cff1=1.0_r8/6.0_r8
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              FC(i,k)=cff1*Hzk(i,k  )-dt(ng)*AK(i,k-1)*oHz(i,k  )
              CF(i,k)=cff1*Hzk(i,k+1)-dt(ng)*AK(i,k+1)*oHz(i,k+1)
            END DO
          END DO
          DO i=Istr,Iend
            CF(i,0)=0.0_r8
            DC(i,0)=0.0_r8
          END DO
!
!  LU decomposition and forward substitution.
!
          cff1=1.0_r8/3.0_r8
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              BC(i,k)=cff1*(Hzk(i,k)+Hzk(i,k+1))+                       &
     &                dt(ng)*AK(i,k)*(oHz(i,k)+oHz(i,k+1))
              cff=1.0_r8/(BC(i,k)-FC(i,k)*CF(i,k-1))
              CF(i,k)=cff*CF(i,k)
              DC(i,k)=cff*(v(i,j,k+1,nnew)-v(i,j,k,nnew)-               &
     &                     FC(i,k)*DC(i,k-1))
            END DO
          END DO
!
!  Backward substitution.
!
          DO i=Istr,Iend
            DC(i,N(ng))=0.0_r8
          END DO
          DO k=N(ng)-1,1,-1
            DO i=Istr,Iend
              DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
            END DO
          END DO
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              DC(i,k)=DC(i,k)*AK(i,k)
              cff=dt(ng)*oHz(i,k)*(DC(i,k)-DC(i,k-1))
              v(i,j,k,nnew)=v(i,j,k,nnew)+cff
#  ifdef DIAGNOSTICS_UV
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)+cff
#  endif
            END DO
          END DO
# else
!
!  Compute off-diagonal coefficients [lambda*dt*Akv/Hz] for the
!  implicit vertical viscosity term at horizontal V-points and
!  vertical W-points.
!
          cff=-lambda*dt(ng)/0.5_r8
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              cff1=1.0_r8/(z_r(i,j,k+1)+z_r(i,j-1,k+1)-                 &
     &                     z_r(i,j,k  )-z_r(i,j-1,k  ))
              FC(i,k)=cff*cff1*AK(i,k)
            END DO
          END DO
          DO i=Istr,Iend
            FC(i,0)=0.0_r8
            FC(i,N(ng))=0.0_r8
          END DO
!
!  Solve the tridiagonal system.
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              DC(i,k)=v(i,j,k,nnew)
              BC(i,k)=Hzk(i,k)-FC(i,k)-FC(i,k-1)
            END DO
          END DO
          DO i=Istr,Iend
            cff=1.0_r8/BC(i,1)
            CF(i,1)=cff*FC(i,1)
            DC(i,1)=cff*DC(i,1)
          END DO
          DO k=2,N(ng)-1
            DO i=Istr,Iend
              cff=1.0_r8/(BC(i,k)-FC(i,k-1)*CF(i,k-1))
              CF(i,k)=cff*FC(i,k)
              DC(i,k)=cff*(DC(i,k)-FC(i,k-1)*DC(i,k-1))
            END DO
          END DO
!
!  Compute new solution by back substitution.
!
          DO i=Istr,Iend
#  ifdef DIAGNOSTICS_UV
            wrk(i,N(ng))=v(i,j,N(ng),nnew)*oHz(i,N(ng))
#  endif
            DC(i,N(ng))=(DC(i,N(ng))-FC(i,N(ng)-1)*DC(i,N(ng)-1))/      &
     &                  (BC(i,N(ng))-FC(i,N(ng)-1)*CF(i,N(ng)-1))
            v(i,j,N(ng),nnew)=DC(i,N(ng))
#  ifdef DIAGNOSTICS_UV
            DiaV3wrk(i,j,N(ng),M3vvis)=DiaV3wrk(i,j,N(ng),M3vvis)+      &
     &                                 v(i,j,N(ng),nnew)-wrk(i,N(ng))
#  endif
          END DO
          DO k=N(ng)-1,1,-1
            DO i=Istr,Iend
#  ifdef DIAGNOSTICS_UV
              wrk(i,k)=v(i,j,k,nnew)*oHz(i,k)
#  endif
              DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
              v(i,j,k,nnew)=DC(i,k)
#  ifdef DIAGNOSTICS_UV
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)+            &
     &                               v(i,j,k,nnew)-wrk(i,k)
#  endif
            END DO
          END DO
# endif
# ifdef OMEGA_IMPLICIT
!
!  Adaptive, Courant-number based implicit vertical advection
!  contribution for v-momentum.
!
          DO i=Istr,Iend
            WK(i,0)=0.5_r8*(Wi(i,j-1,0)+                                &
     &                      Wi(i,j  ,0))
            DO k=1,N(ng)
              WK(i,k)=0.5_r8*(Wi(i,j-1,k)+                              &
     &                        Wi(i,j  ,k))
              Hzk(i,k)=0.5_r8*(Hz(i,j-1,k)+                             &
     &                         Hz(i,j  ,k))
            END DO
          END DO
!
!  Compute off-diagonal coefficients [dt*Wi*pm*pn] for the
!  implicit vertical viscosity term at horizontal V-points and
!  vertical W-points.
!
          cff=dt(ng)
          DO k=1,N(ng)-1
            DO i=Istr,Iend
              cff1=cff/(on_v(i,j)*om_v(i,j))
              FCmax(i,k)=MAX(WK(i,k),0.0_r8)*cff1
              FCmin(i,k)=MIN(WK(i,k),0.0_r8)*cff1
            END DO
          END DO
          DO i=Istr,Iend
            FCmax(i,0)=0.0_r8
            FCmin(i,0)=0.0_r8
            FCmax(i,N(ng))=0.0_r8
            FCmin(i,N(ng))=0.0_r8
          END DO
!
!  Solve the tridiagonal system.
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              BC(i,k)=Hzk(i,k)+FCmax(i,k)-FCmin(i,k-1)
              DC(i,k)=v(i,j,k,nnew)*Hzk(i,k)
            END DO
          END DO
          DO i=Istr,Iend
            cff=1.0_r8/BC(i,1)
            CF(i,1)=cff*FCmin(i,1)
            DC(i,1)=cff*DC(i,1)
          END DO
          DO k=2,N(ng)-1
            DO i=Istr,Iend
              cff=1.0_r8/(BC(i,k)+FCmax(i,k-1)*CF(i,k-1))
              CF(i,k)=cff*FCmin(i,k)
              DC(i,k)=cff*(DC(i,k)+FCmax(i,k-1)*DC(i,k-1))
            END DO
          END DO
!
!  Compute new solution by back substitution.
!
          DO i=Istr,Iend
#  ifdef DIAGNOSTICS_UV
            cff1=v(i,j,N(ng),nnew)
#  endif
            cff=1.0_r8/(BC(i,N(ng))+FCmax(i,N(ng)-1)*CF(i,N(ng)-1))
            DC(i,N(ng))=cff*(DC(i,N(ng))+                               &
     &                       FCmax(i,N(ng)-1)*DC(i,N(ng)-1))
            v(i,j,N(ng),nnew)=DC(i,N(ng))
#  ifdef DIAGNOSTICS_UV
            DiaRV(i,j,N(ng),nrhs,M3vadv)=DiaRV(i,j,N(ng),nrhs,M3vadv)+  &
     &                                   v(i,j,N(ng),nnew)-cff1
#  endif
          END DO
!
          DO k=N(ng)-1,1,-1
            DO i=Istr,Iend
#  ifdef DIAGNOSTICS_UV
              cff1=v(i,j,k,nnew)
#  endif
              DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
              v(i,j,k,nnew)=DC(i,k)
#  ifdef DIAGNOSTICS_UV
              DiaRV(i,j,k,nrhs,M3vadv)=DiaRV(i,j,k,nrhs,M3vadv)+        &
     &                                 v(i,j,k,nnew)-cff1
#  endif
            END DO
          END DO
# endif
!
!  Replace INTERIOR POINTS incorrect vertical mean with more accurate
!  barotropic component, vbar=DV_avg1/(D*om_v). Recall that, D=CF(:,0).
!
          DO i=Istr,Iend
            CF(i,0)=Hzk(i,1)
            DC(i,0)=v(i,j,1,nnew)*Hzk(i,1)
# ifdef WEC
            DCs(i,0)=v_stokes(i,j,1)*Hzk(i,1)
# endif
# ifdef DIAGNOSTICS_UV
            Dwrk(i,M2pgrd)=DiaV3wrk(i,j,1,M3pgrd)*Hzk(i,1)
            Dwrk(i,M2bstr)=DiaV3wrk(i,j,1,M3vvis)*Hzk(i,1)
#  ifdef UV_COR
            Dwrk(i,M2fcor)=DiaV3wrk(i,j,1,M3fcor)*Hzk(i,1)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
            Dwrk(i,M2xvis)=DiaV3wrk(i,j,1,M3xvis)*Hzk(i,1)
            Dwrk(i,M2yvis)=DiaV3wrk(i,j,1,M3yvis)*Hzk(i,1)
            Dwrk(i,M2hvis)=DiaV3wrk(i,j,1,M3hvis)*Hzk(i,1)
#  endif
#  ifdef UV_ADV
            Dwrk(i,M2xadv)=DiaV3wrk(i,j,1,M3xadv)*Hzk(i,1)
            Dwrk(i,M2yadv)=DiaV3wrk(i,j,1,M3yadv)*Hzk(i,1)
            Dwrk(i,M2hadv)=DiaV3wrk(i,j,1,M3hadv)*Hzk(i,1)
#  endif
#  ifdef WEC_VF
            Dwrk(i,M2hjvf)=DiaV3wrk(i,j,1,M3hjvf)*Hzk(i,1)
            Dwrk(i,M2kvrf)=DiaV3wrk(i,j,1,M3kvrf)*Hzk(i,1)
#   ifdef UV_COR
            Dwrk(i,M2fsco)=DiaV3wrk(i,j,1,M3fsco)*Hzk(i,1)
#   endif
#   ifdef BOTTOM_STREAMING
            Dwrk(i,M2bstm)=DiaV3wrk(i,j,1,M3bstm)*Hzk(i,1)
#   endif
#   ifdef SURFACE_STREAMING
            Dwrk(i,M2sstm)=DiaV3wrk(i,j,1,M3sstm)*Hzk(i,1)
#   endif
            Dwrk(i,M2wrol)=DiaV3wrk(i,j,1,M3wrol)*Hzk(i,1)
            Dwrk(i,M2wbrk)=DiaV3wrk(i,j,1,M3wbrk)*Hzk(i,1)
#  endif
# endif
          END DO
          DO k=2,N(ng)
            DO i=Istr,Iend
              CF(i,0)=CF(i,0)+Hzk(i,k)
              DC(i,0)=DC(i,0)+v(i,j,k,nnew)*Hzk(i,k)
# ifdef WEC
              DCs(i,0)=DCs(i,0)+v_stokes(i,j,k)*Hzk(i,k)
# endif
# ifdef DIAGNOSTICS_UV
              Dwrk(i,M2pgrd)=Dwrk(i,M2pgrd)+                            &
     &                       DiaV3wrk(i,j,k,M3pgrd)*Hzk(i,k)
              Dwrk(i,M2bstr)=Dwrk(i,M2bstr)+                            &
     &                       DiaV3wrk(i,j,k,M3vvis)*Hzk(i,k)
#  ifdef UV_COR
              Dwrk(i,M2fcor)=Dwrk(i,M2fcor)+                            &
     &                       DiaV3wrk(i,j,k,M3fcor)*Hzk(i,k)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
              Dwrk(i,M2xvis)=Dwrk(i,M2xvis)+                            &
     &                       DiaV3wrk(i,j,k,M3xvis)*Hzk(i,k)
              Dwrk(i,M2yvis)=Dwrk(i,M2yvis)+                            &
     &                       DiaV3wrk(i,j,k,M3yvis)*Hzk(i,k)
              Dwrk(i,M2hvis)=Dwrk(i,M2hvis)+                            &
     &                       DiaV3wrk(i,j,k,M3hvis)*Hzk(i,k)
#  endif
#  ifdef UV_ADV
              Dwrk(i,M2xadv)=Dwrk(i,M2xadv)+                            &
     &                       DiaV3wrk(i,j,k,M3xadv)*Hzk(i,k)
              Dwrk(i,M2yadv)=Dwrk(i,M2yadv)+                            &
     &                       DiaV3wrk(i,j,k,M3yadv)*Hzk(i,k)
              Dwrk(i,M2hadv)=Dwrk(i,M2hadv)+                            &
     &                       DiaV3wrk(i,j,k,M3hadv)*Hzk(i,k)
#  endif
#  ifdef WEC_VF
              Dwrk(i,M2hjvf)=Dwrk(i,M2hjvf)+                            &
     &                       DiaV3wrk(i,j,k,M3hjvf)*Hzk(i,k)
              Dwrk(i,M2kvrf)=Dwrk(i,M2kvrf)+                            &
     &                       DiaV3wrk(i,j,k,M3kvrf)*Hzk(i,k)
#   ifdef UV_COR
              Dwrk(i,M2fsco)=Dwrk(i,M2fsco)+                            &
     &                       DiaV3wrk(i,j,k,M3fsco)*Hzk(i,k)
#   endif
#   ifdef BOTTOM_STREAMING
              Dwrk(i,M2bstm)=Dwrk(i,M2bstm)+                            &
     &                       DiaV3wrk(i,j,k,M3bstm)*Hzk(i,k)
#   endif
#   ifdef SURFACE_STREAMING
              Dwrk(i,M2sstm)=Dwrk(i,M2sstm)+                            &
     &                       DiaV3wrk(i,j,k,M3sstm)*Hzk(i,k)
#   endif
              Dwrk(i,M2wrol)=Dwrk(i,M2wrol)+                            &
     &                       DiaV3wrk(i,j,k,M3wrol)*Hzk(i,k)
              Dwrk(i,M2wbrk)=Dwrk(i,M2wbrk)+                            &
     &                       DiaV3wrk(i,j,k,M3wbrk)*Hzk(i,k)
#  endif
# endif
            END DO
          END DO
          DO i=Istr,Iend
            cff1=1.0_r8/(CF(i,0)*om_v(i,j))
            DC(i,0)=(DC(i,0)*om_v(i,j)-DV_avg1(i,j))*cff1    ! recursive
# ifdef WEC
            cff2=1.0_r8/CF(i,0)
            DCs(i,0)=DCs(i,0)*cff2-vbar_stokes(i,j)          ! recursive
# endif
# ifdef DIAGNOSTICS_UV
            DO idiag=1,M2pgrd
              Dwrk(i,idiag)=(Dwrk(i,idiag)*om_v(i,j)-                   &
     &                       DiaV2wrk(i,j,idiag))*cff1
            END DO
            Dwrk(i,M2bstr)=(Dwrk(i,M2bstr)*om_v(i,j)-                   &
     &                      DiaV2wrk(i,j,M2bstr)-DiaV2wrk(i,j,M2sstr))* &
     &                     cff1
# endif
          END DO
!
!  Couple and update new solution.
!
          DO k=1,N(ng)
            DO i=Istr,Iend
              v(i,j,k,nnew)=v(i,j,k,nnew)-DC(i,0)
# ifdef MASKING
              v(i,j,k,nnew)=v(i,j,k,nnew)*vmask(i,j)
# endif
# ifdef WET_DRY
              v(i,j,k,nnew)=v(i,j,k,nnew)*vmask_wet(i,j)
              rv(i,j,k,nrhs)=rv(i,j,k,nrhs)*vmask_wet(i,j)
# endif
# ifdef WEC
              v_stokes(i,j,k)=v_stokes(i,j,k)-DCs(i,0)
#  ifdef MASKING
              v_stokes(i,j,k)=v_stokes(i,j,k)*vmask(i,j)
#  endif
#  ifdef WET_DRY
              v_stokes(i,j,k)=v_stokes(i,j,k)*vmask_wet(i,j)
#  endif
# endif
# ifdef DIAGNOSTICS_UV
              DiaV3wrk(i,j,k,M3pgrd)=DiaV3wrk(i,j,k,M3pgrd)-            &
     &                               Dwrk(i,M2pgrd)
              DiaV3wrk(i,j,k,M3vvis)=DiaV3wrk(i,j,k,M3vvis)-            &
     &                               Dwrk(i,M2bstr)
#  ifdef UV_COR
              DiaV3wrk(i,j,k,M3fcor)=DiaV3wrk(i,j,k,M3fcor)-            &
     &                               Dwrk(i,M2fcor)
#  endif
#  if defined UV_VIS2 || defined UV_VIS4
              DiaV3wrk(i,j,k,M3xvis)=DiaV3wrk(i,j,k,M3xvis)-            &
     &                               Dwrk(i,M2xvis)
              DiaV3wrk(i,j,k,M3yvis)=DiaV3wrk(i,j,k,M3yvis)-            &
     &                               Dwrk(i,M2yvis)
              DiaV3wrk(i,j,k,M3hvis)=DiaV3wrk(i,j,k,M3hvis)-            &
     &                               Dwrk(i,M2hvis)
#  endif
#  ifdef UV_ADV
              DiaV3wrk(i,j,k,M3xadv)=DiaV3wrk(i,j,k,M3xadv)-            &
     &                               Dwrk(i,M2xadv)
              DiaV3wrk(i,j,k,M3yadv)=DiaV3wrk(i,j,k,M3yadv)-            &
     &                               Dwrk(i,M2yadv)
              DiaV3wrk(i,j,k,M3hadv)=DiaV3wrk(i,j,k,M3hadv)-            &
     &                               Dwrk(i,M2hadv)
#  endif
#  ifdef WEC_VF
              DiaV3wrk(i,j,k,M3hjvf)=DiaV3wrk(i,j,k,M3hjvf)-            &
     &                               Dwrk(i,M2hjvf)
              DiaV3wrk(i,j,k,M3kvrf)=DiaV3wrk(i,j,k,M3kvrf)-            &
     &                               Dwrk(i,M2kvrf)
#   ifdef UV_COR
              DiaV3wrk(i,j,k,M3fsco)=DiaV3wrk(i,j,k,M3fsco)-            &
     &                               Dwrk(i,M2fsco)
#   endif
#   ifdef BOTTOM_STREAMING
              DiaV3wrk(i,j,k,M3bstm)=DiaV3wrk(i,j,k,M3bstm)-            &
     &                               Dwrk(i,M2bstm)
#   endif
#   ifdef SURFACE_STREAMING
              DiaV3wrk(i,j,k,M3sstm)=DiaV3wrk(i,j,k,M3sstm)-            &
     &                               Dwrk(i,M2sstm)
#   endif
              DiaV3wrk(i,j,k,M3wrol)=DiaV3wrk(i,j,k,M3wrol)-            &
     &                               Dwrk(i,M2wrol)
              DiaV3wrk(i,j,k,M3wbrk)=DiaV3wrk(i,j,k,M3wbrk)-            &
     &                               Dwrk(i,M2wbrk)
#  endif
# endif
            END DO
          END DO

# if defined DIAGNOSTICS_UV && defined MASKING
          DO k=1,N(ng)
            DO i=Istr,Iend
              DO idiag=1,NDM3d
                DiaV3wrk(i,j,k,idiag)=DiaV3wrk(i,j,k,idiag)*vmask(i,j)
              END DO
            END DO
          END DO
# endif
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Set lateral boundary conditions.
!-----------------------------------------------------------------------
!
      CALL u3dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj, N(ng),                       &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 nstp, nnew,                                      &
     &                 u)
      CALL v3dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj, N(ng),                       &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 nstp, nnew,                                      &
     &                 v)
!
!-----------------------------------------------------------------------
!  Apply momentum transport point sources (like river runoff), if any.
!-----------------------------------------------------------------------
!
      IF (LuvSrc(ng)) THEN
        DO is=1,Nsrc(ng)
          i=SOURCES(ng)%Isrc(is)
          j=SOURCES(ng)%Jsrc(is)
          IF (((IstrR.le.i).and.(i.le.IendR)).and.                      &
     &        ((JstrR.le.j).and.(j.le.JendR))) THEN
            IF (INT(SOURCES(ng)%Dsrc(is)).eq.0) THEN
              DO k=1,N(ng)
                cff1=1.0_r8/(on_u(i,j)*                                 &
     &                       0.5_r8*(z_w(i-1,j,k)-z_w(i-1,j,k-1)+       &
     &                               z_w(i  ,j,k)-z_w(i  ,j,k-1)))
                u(i,j,k,nnew)=SOURCES(ng)%Qsrc(is,k)*cff1
              END DO
            ELSE IF (INT(SOURCES(ng)%Dsrc(is)).eq.1) THEN
              DO k=1,N(ng)
                cff1=1.0_r8/(om_v(i,j)*                                 &
     &                       0.5_r8*(z_w(i,j-1,k)-z_w(i,j-1,k-1)+       &
     &                               z_w(i,j  ,k)-z_w(i,j  ,k-1)))
                v(i,j,k,nnew)=SOURCES(ng)%Qsrc(is,k)*cff1
              END DO
            END IF
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Couple 2D and 3D momentum equations.
!-----------------------------------------------------------------------
!
!  Couple velocity component in the XI-direction.
!
      DO j=JstrT,JendT
        DO i=IstrP,IendT
          DC(i,0)=0.0_r8
          CF(i,0)=0.0_r8
# ifdef WEC
          CFs(i,0)=0.0_r8
# endif
          FC(i,0)=0.0_r8
        END DO
!
!  Compute thicknesses of U-boxes DC(i,1:N), total depth of the water
!  column DC(i,0), and incorrect vertical mean CF(i,0).  Notice that
!  barotropic component is replaced with its fast-time averaged
!  values.
!
        DO k=1,N(ng)
          DO i=IstrP,IendT
            cff=0.5_r8*on_u(i,j)
            DC(i,k)=cff*(Hz(i,j,k)+Hz(i-1,j,k))
            DC(i,0)=DC(i,0)+DC(i,k)
            CF(i,0)=CF(i,0)+                                            &
     &              DC(i,k)*u(i,j,k,nnew)
# ifdef WEC
            CFs(i,0)=CFs(i,0)+                                          &
     &               DC(i,k)*u_stokes(i,j,k)
# endif
          END DO
        END DO
        DO i=IstrP,IendT
          cff1=DC(i,0)                                  ! intermediate
# ifdef WEC
!!        cff2=on_u(i,j)*DC(i,0)*ubar_stokes(i,j)
          cff2=DC(i,0)*ubar_stokes(i,j)
# endif
          DC(i,0)=1.0_r8/DC(i,0)                        ! recursive
          CF(i,0)=DC(i,0)*(CF(i,0)-DU_avg1(i,j))        ! recursive
# ifdef WEC
          CFs(i,0)=DC(i,0)*(CFs(i,0)-cff2)              ! recursive
# endif
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
          ubar(i,j,knew)=DC(i,0)*DU_avg1(i,j)
#  ifdef WET_DRY
          ubar(i,j,knew)=ubar(i,j,knew)*umask_wet(i,j)
#  endif
# else
          ubar(i,j,1)=DC(i,0)*DU_avg1(i,j)
#  ifdef WET_DRY
          ubar(i,j,1)=ubar(i,j,1)*umask_wet(i,j)
#  endif
          ubar(i,j,2)=ubar(i,j,1)
# endif
# ifdef DIAGNOSTICS_UV
          DiaU2wrk(i,j,M2rate)=ubar(i,j,1)-DiaU2int(i,j,M2rate)*DC(i,0)
          DiaU2int(i,j,M2rate)=ubar(i,j,1)*cff1
# endif
        END DO
# ifdef DIAGNOSTICS_UV
!
!  Convert the units of the fast-time integrated change in mass flux
!  diagnostic values to change in velocity (m s-1).
!
        DO idiag=1,NDM2d-1
          DO i=IstrP,IendT
            DiaU2wrk(i,j,idiag)=DC(i,0)*DiaU2wrk(i,j,idiag)
#  ifdef MASKING
            DiaU2wrk(i,j,idiag)=DiaU2wrk(i,j,idiag)*umask(i,j)
#  endif
          END DO
        END DO
# endif
!
!  Replace only BOUNDARY POINTS incorrect vertical mean with more
!  accurate barotropic component, ubar=DU_avg1/(D*on_u). Recall that,
!  D=CF(:,0).
!
!  NOTE:  Only the BOUNDARY POINTS need to be replaced. Avoid redundant
!         update in the interior again for computational purposes which
!         will not affect the nonlinear code.  However, the adjoint
!         code is wrong because the interior solution is corrected
!         twice. The replacement is avoided if the boundary edge is
!         periodic. The J-loop is pipelined, so we need to do a special
!         test for the southern and northern domain edges.
!
        IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
          IF (DOMAIN(ng)%Western_Edge(tile)) THEN
            DO k=1,N(ng)
              u(Istr,j,k,nnew)=u(Istr,j,k,nnew)-CF(Istr,0)
# ifdef MASKING
              u(Istr,j,k,nnew)=u(Istr,j,k,nnew)*                        &
     &                         umask(Istr,j)
# endif
# ifdef WET_DRY
              u(Istr,j,k,nnew)=u(Istr,j,k,nnew)*                        &
     &                         umask_wet(Istr,j)
# endif
# ifdef WEC
              u_stokes(Istr,j,k)=u_stokes(Istr,j,k)-CFs(Istr,0)
#  ifdef MASKING
              u_stokes(Istr,j,k)=u_stokes(Istr,j,k)*                    &
     &                           umask(Istr,j)
#  endif
#  ifdef WET_DRY
              u_stokes(Istr,j,k)=u_stokes(Istr,j,k)*                    &
     &                           umask_wet(Istr,j)
#  endif
# endif
            END DO
          END IF
        END IF
!
        IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
          IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
            DO k=1,N(ng)
              u(Iend+1,j,k,nnew)=u(Iend+1,j,k,nnew)-CF(Iend+1,0)
# ifdef MASKING
              u(Iend+1,j,k,nnew)=u(Iend+1,j,k,nnew)*                    &
     &                           umask(Iend+1,j)
# endif
# ifdef WET_DRY
              u(Iend+1,j,k,nnew)=u(Iend+1,j,k,nnew)*                    &
     &                           umask_wet(Iend+1,j)
# endif
# ifdef WEC
              u_stokes(Iend+1,j,k)=u_stokes(Iend+1,j,k)-CFs(Iend+1,0)
#  ifdef MASKING
              u_stokes(Iend+1,j,k)=u_stokes(Iend+1,j,k)*                &
     &                             umask(Iend+1,j)
#  endif
#  ifdef WET_DRY
              u_stokes(Iend+1,j,k)=u_stokes(Iend+1,j,k)*                &
     &                             umask_wet(Iend+1,j)
#  endif
# endif
            END DO
          END IF
        END IF
!
        IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
          IF (j.eq.0) THEN                           ! southern boundary
            DO k=1,N(ng)                             ! J-loop pipelined
              DO i=IstrU,Iend
                u(i,j,k,nnew)=u(i,j,k,nnew)-CF(i,0)
# ifdef MASKING
                u(i,j,k,nnew)=u(i,j,k,nnew)*                            &
     &                        umask(i,j)
# endif
# ifdef WET_DRY
                u(i,j,k,nnew)=u(i,j,k,nnew)*                            &
     &                        umask_wet(i,j)
# endif
# ifdef WEC
                u_stokes(i,j,k)=u_stokes(i,j,k)-CFs(i,0)
#  ifdef MASKING
                u_stokes(i,j,k)=u_stokes(i,j,k)*                        &
     &                          umask(i,j)
#  endif
#  ifdef WET_DRY
                u_stokes(i,j,k)=u_stokes(i,j,k)*                        &
     &                          umask_wet(i,j)
#  endif
# endif
              END DO
            END DO
          END IF
        END IF
!
        IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
          IF (j.eq.Mm(ng)+1) THEN                    ! northern boundary
            DO k=1,N(ng)                             ! J-loop pipelined
              DO i=IstrU,Iend
                u(i,j,k,nnew)=u(i,j,k,nnew)-CF(i,0)
# ifdef MASKING
                u(i,j,k,nnew)=u(i,j,k,nnew)*                            &
     &                        umask(i,j)
# endif
# ifdef WET_DRY
                u(i,j,k,nnew)=u(i,j,k,nnew)*                            &
     &                        umask_wet(i,j)
# endif
# ifdef WEC
                u_stokes(i,j,k)=u_stokes(i,j,k)-CFs(i,0)
#  ifdef MASKING
                u_stokes(i,j,k)=u_stokes(i,j,k)*                        &
     &                          umask(i,j)
#  endif
#  ifdef WET_DRY
                u_stokes(i,j,k)=u_stokes(i,j,k)*                        &
     &                          umask_wet(i,j)
#  endif
# endif
              END DO
            END DO
          END IF
        END IF
!
!  Compute correct mass flux, Hz*u/n.
!
        DO k=N(ng),1,-1
          DO i=IstrP,IendT
            Huon(i,j,k)=0.5_r8*(Huon(i,j,k)+u(i,j,k,nnew)*DC(i,k))
# ifdef WEC
            Huon(i,j,k)=Huon(i,j,k)+0.5_r8*u_stokes(i,j,k)*DC(i,k)
# endif
            FC(i,0)=FC(i,0)+Huon(i,j,k)
# ifdef DIAGNOSTICS_UV
            DiaU3wrk(i,j,k,M3rate)=u(i,j,k,nnew)-DiaU3wrk(i,j,k,M3rate)
# endif
          END DO
        END DO
        DO i=IstrP,IendT
          FC(i,0)=DC(i,0)*(FC(i,0)-DU_avg2(i,j))        ! recursive
        END DO
        DO k=1,N(ng)
          DO i=IstrP,IendT
            Huon(i,j,k)=Huon(i,j,k)-DC(i,k)*FC(i,0)
          END DO
        END DO
!
!  Couple velocity component in the ETA-direction.
!
        IF (j.ge.Jstr) THEN
          DO i=IstrT,IendT
            DC(i,0)=0.0_r8
            CF(i,0)=0.0_r8
# ifdef WEC
            CFs(i,0)=0.0_r8
# endif
            FC(i,0)=0.0_r8
          END DO
!
!  Compute thicknesses of V-boxes DC(i,1:N), total depth of the water
!  column DC(i,0), and incorrect vertical mean CF(i,0).  Notice that
!  barotropic component is replaced with its fast-time averaged
!  values.
!
          DO k=1,N(ng)
            DO i=IstrT,IendT
              cff=0.5_r8*om_v(i,j)
              DC(i,k)=cff*(Hz(i,j,k)+Hz(i,j-1,k))
              DC(i,0)=DC(i,0)+DC(i,k)
              CF(i,0)=CF(i,0)+                                          &
     &                DC(i,k)*v(i,j,k,nnew)
# ifdef WEC
              CFs(i,0)=CFs(i,0)+                                        &
     &                 DC(i,k)*v_stokes(i,j,k)
# endif
            END DO
          END DO
          DO i=IstrT,IendT
            cff1=DC(i,0)                                 ! Intermediate
# ifdef WEC
!!          cff2=om_v(i,j)*DC(i,0)*vbar_stokes(i,j)
            cff2=DC(i,0)*vbar_stokes(i,j)
# endif
            DC(i,0)=1.0_r8/DC(i,0)                       ! recursive
            CF(i,0)=DC(i,0)*(CF(i,0)-DV_avg1(i,j))       ! recursive
# ifdef WEC
            CFs(i,0)=DC(i,0)*(CFs(i,0)-cff2)             ! recursive
# endif
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
            vbar(i,j,knew)=DC(i,0)*DV_avg1(i,j)
#  ifdef WET_DRY
            vbar(i,j,knew)=vbar(i,j,knew)*vmask_wet(i,j)
#  endif
# else
            vbar(i,j,1)=DC(i,0)*DV_avg1(i,j)
#  ifdef WET_DRY
            vbar(i,j,1)=vbar(i,j,1)*vmask_wet(i,j)
#  endif
            vbar(i,j,2)=vbar(i,j,1)
# endif
# ifdef DIAGNOSTICS_UV
            DiaV2wrk(i,j,M2rate)=vbar(i,j,1)-                           &
     &                           DiaV2int(i,j,M2rate)*DC(i,0)
            DiaV2int(i,j,M2rate)=vbar(i,j,1)*cff1
!!          DiaV2wrk(i,j,M2rate)=vbar(i,j,1)-DiaV2int(i,j,M2rate)
!!          DiaV2int(i,j,M2rate)=vbar(i,j,1)
# endif
          END DO
# ifdef DIAGNOSTICS_UV
!
!  Convert the units of the fast-time integrated change in mass flux
!  diagnostic values to change in velocity (m s-1).
!
          DO idiag=1,NDM2d-1
            DO i=IstrT,IendT
              DiaV2wrk(i,j,idiag)=DC(i,0)*DiaV2wrk(i,j,idiag)
#  ifdef MASKING
              DiaV2wrk(i,j,idiag)=DiaV2wrk(i,j,idiag)*vmask(i,j)
#  endif
            END DO
          END DO
# endif
!
!  Replace only BOUNDARY POINTS incorrect vertical mean with more
!  accurate barotropic component, vbar=DV_avg1/(D*om_v).  Recall that,
!  D=CF(:,0).
!
!  NOTE:  Only the BOUNDARY POINTS need to be replaced. Avoid redundant
!         update in the interior again for computational purposes which
!         will not affect the nonlinear code.  However, the adjoint
!         code is wrong because the interior solution is corrected
!         twice. The replacement is avoided if the boundary edge is
!         periodic. The J-loop is pipelined, so we need to do a special
!         test for the southern and northern domain edges.
!
          IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
            IF (DOMAIN(ng)%Western_Edge(tile)) THEN
              DO k=1,N(ng)
                v(Istr-1,j,k,nnew)=v(Istr-1,j,k,nnew)-CF(Istr-1,0)
# ifdef MASKING
                v(Istr-1,j,k,nnew)=v(Istr-1,j,k,nnew)*                  &
     &                             vmask(Istr-1,j)
# endif
# ifdef WET_DRY
                v(Istr-1,j,k,nnew)=v(Istr-1,j,k,nnew)*                  &
     &                             vmask_wet(Istr-1,j)
# endif
# ifdef WEC
                v_stokes(Istr-1,j,k)=v_stokes(Istr-1,j,k)-              &
     &                               CFs(Istr-1,0)
#  ifdef MASKING
                v_stokes(Istr-1,j,k)=v_stokes(Istr-1,j,k)*              &
     &                               vmask(Istr-1,j)
#  endif
#  ifdef WET_DRY
                v_stokes(Istr-1,j,k)=v_stokes(Istr-1,j,k)*              &
     &                               vmask_wet(Istr-1,j)
#  endif
# endif
              END DO
            END IF
          END IF
!
          IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
            IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
              DO k=1,N(ng)
                v(Iend+1,j,k,nnew)=v(Iend+1,j,k,nnew)-CF(Iend+1,0)
# ifdef MASKING
                v(Iend+1,j,k,nnew)=v(Iend+1,j,k,nnew)*                  &
     &                             vmask(Iend+1,j)
# endif
# ifdef WET_DRY
                v(Iend+1,j,k,nnew)=v(Iend+1,j,k,nnew)*                  &
     &                             vmask_wet(Iend+1,j)
# endif
# ifdef WEC
                v_stokes(Iend+1,j,k)=v_stokes(Iend+1,j,k)-              &
     &                               CFs(Iend+1,0)
#  ifdef MASKING
                v_stokes(Iend+1,j,k)=v_stokes(Iend+1,j,k)*              &
     &                               vmask(Iend+1,j)
#  endif
#  ifdef WET_DRY
                v_stokes(Iend+1,j,k)=v_stokes(Iend+1,j,k)*              &
     &                               vmask_wet(Iend+1,j)
#  endif
# endif
              END DO
            END IF
          END IF
!
          IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
            IF (j.eq.1) THEN                         ! southern boundary
              DO k=1,N(ng)                           ! J-loop pipelined
                DO i=Istr,Iend
                  v(i,j,k,nnew)=v(i,j,k,nnew)-CF(i,0)
# ifdef MASKING
                  v(i,j,k,nnew)=v(i,j,k,nnew)*                          &
     &                          vmask(i,j)
# endif
# ifdef WET_DRY
                  v(i,j,k,nnew)=v(i,j,k,nnew)*                          &
     &                          vmask_wet(i,j)
# endif
# ifdef WEC
                  v_stokes(i,j,k)=v_stokes(i,j,k)-CFs(i,0)
#  ifdef MASKING
                  v_stokes(i,j,k)=v_stokes(i,j,k)*                      &
     &                            vmask(i,j)
#  endif
#  ifdef WET_DRY
                  v_stokes(i,j,k)=v_stokes(i,j,k)*                      &
     &                            vmask_wet(i,j)
#  endif
# endif
                END DO
              END DO
            END IF
          END IF
!
          IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
            IF (j.eq.Mm(ng)+1) THEN                  ! northern boundary
              DO k=1,N(ng)                           ! J-loop pipelined
                DO i=Istr,Iend
                  v(i,j,k,nnew)=v(i,j,k,nnew)-CF(i,0)
# ifdef MASKING
                  v(i,j,k,nnew)=v(i,j,k,nnew)*                          &
     &                          vmask(i,j)
# endif
# ifdef WET_DRY
                  v(i,j,k,nnew)=v(i,j,k,nnew)*                          &
     &                          vmask_wet(i,j)
# endif
# ifdef WEC
                  v_stokes(i,j,k)=v_stokes(i,j,k)-CFs(i,0)
#  ifdef MASKING
                  v_stokes(i,j,k)=v_stokes(i,j,k)*                      &
     &                            vmask(i,j)
#  endif
#  ifdef WET_DRY
                  v_stokes(i,j,k)=v_stokes(i,j,k)*                      &
     &                            vmask_wet(i,j)
#  endif
# endif
                END DO
              END DO
            END IF
          END IF
!
!  Compute correct mass flux, Hz*v/m.
!
          DO k=N(ng),1,-1
            DO i=IstrT,IendT
              Hvom(i,j,k)=0.5_r8*(Hvom(i,j,k)+v(i,j,k,nnew)*DC(i,k))
# ifdef WEC
              Hvom(i,j,k)=Hvom(i,j,k)+0.5_r8*v_stokes(i,j,k)*DC(i,k)
# endif
              FC(i,0)=FC(i,0)+Hvom(i,j,k)
# ifdef DIAGNOSTICS_UV
              DiaV3wrk(i,j,k,M3rate)=v(i,j,k,nnew)-                     &
     &                               DiaV3wrk(i,j,k,M3rate)
# endif
            END DO
          END DO
          DO i=IstrT,IendT
            FC(i,0)=DC(i,0)*(FC(i,0)-DV_avg2(i,j))      ! recursive
          END DO
          DO k=1,N(ng)
            DO i=IstrT,IendT
              Hvom(i,j,k)=Hvom(i,j,k)-DC(i,k)*FC(i,0)
            END DO
          END DO
        END IF
      END DO
!
!-----------------------------------------------------------------------
!  Exchange boundary data.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          u(:,:,:,nnew))
        CALL exchange_v3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          v(:,:,:,nnew))
#  ifdef WEC
        CALL exchange_u3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          u_stokes(:,:,:))
        CALL exchange_v3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          v_stokes(:,:,:))
#  endif
        CALL exchange_u3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          Huon)
        CALL exchange_v3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          Hvom)
!
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          ubar(:,:,knew))
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          vbar(:,:,knew))
# else
        DO k=1,2
          CALL exchange_u2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            ubar(:,:,k))
          CALL exchange_v2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            vbar(:,:,k))
        END DO
# endif
      END IF

# ifdef DISTRIBUTE
!
      CALL mp_exchange3d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    u(:,:,:,nnew), v(:,:,:,nnew),                 &
     &                    Huon, Hvom)
!
#  if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    ubar(:,:,knew), vbar(:,:,knew))
#  else
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    ubar(:,:,1), vbar(:,:,1),                     &
     &                    ubar(:,:,2), vbar(:,:,2))
#  endif
#  ifdef WEC
      CALL mp_exchange3d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    u_stokes(:,:,:), v_stokes(:,:,:))
#  endif
# endif
!
!-----------------------------------------------------------------------
!  Compute 3D momentum (ua, va) at RHO-points (A-Grid) for output
!  purposes and data assimilation where the observations and state
!  vector is located at the cell-center.
!-----------------------------------------------------------------------
!
      CALL uv_C2A_grid (ng, tile, iNLM, nnew)
!
      RETURN
      END SUBROUTINE step3d_uv_tile
#endif
      END MODULE step3d_uv_mod
```

</details>

<a id="src-original-t-full"></a>

**source-block ID：`original-t-full`**；commit：`cf10c0f7cc270a7075ad19ee3b8950be352a7cd0`；路径：`ROMS/Nonlinear/step3d_t.F`；闭区间：**1–1976**；行数：**1976**；UTF-8 payload SHA256：`f59d48307a773e3cdf094371081e016bd2fe3caf37c76384ce137bc80f905e73`。

<details>
<summary>ROMS/Nonlinear/step3d_t.F，1–1976（完整原文，可展开）</summary>

<!-- source-block id=original-t-full commit=cf10c0f7cc270a7075ad19ee3b8950be352a7cd0 path=ROMS/Nonlinear/step3d_t.F lines=1-1976 count=1976 sha256=f59d48307a773e3cdf094371081e016bd2fe3caf37c76384ce137bc80f905e73 -->
```fortran
#include "cppdefs.h"
      MODULE step3d_t_mod
#if !defined TS_FIXED && (defined NONLINEAR && defined SOLVE3D)
!
!git $Id$
!=======================================================================
!  Copyright (c) 2002-2025 The ROMS Group                              !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                            Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  This routine time-steps tracer equations.  Notice that advective    !
!  and diffusive terms are time-stepped differently. It applies the    !
!  corrector time-step for horizontal/vertical advection,  vertical    !
!  diffusion, nudging if necessary, and lateral boundary conditions.   !
!                                                                      !
!  A different horizontal/vertical advection scheme is allowed for     !
!  each tracer. If the MPDATA or HSIMT monotonic scheme, it is applied !
!  to both horizontal and vertical advective fluxes.                   !
!                                                                      !
!  Notice that at input the tracer arrays have:                        !
!                                                                      !
!    t(:,:,:,nnew,:)   m Tunits  n+1     horizontal/vertical diffusion !
!                                        terms plus source/sink terms  !
!                                        (biology, sediment), if any   !
!                                                                      !
!    t(:,:,:,3   ,:)   Tunits    n+1/2   advective terms and vertical  !
!                                        diffusion predictor step      !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: step3d_t
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE step3d_t (ng, tile)
!***********************************************************************
!
      USE mod_param
# ifdef DIAGNOSTICS_TS
      USE mod_diags
# endif
      USE mod_grid
      USE mod_mixing
      USE mod_ocean
# if defined SEDIMENT && defined SED_MORPH
      USE mod_sedbed
# endif
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
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 35, __LINE__, MyFile)
# endif
      CALL step3d_t_tile (ng, tile,                                     &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    IminS, ImaxS, JminS, JmaxS,                   &
     &                    nrhs(ng), nstp(ng), nnew(ng),                 &
# ifdef MASKING
     &                    GRID(ng) % rmask,                             &
     &                    GRID(ng) % umask,                             &
     &                    GRID(ng) % vmask,                             &
# endif
# ifdef WET_DRY
     &                    GRID(ng) % rmask_wet,                         &
     &                    GRID(ng) % umask_wet,                         &
     &                    GRID(ng) % vmask_wet,                         &
# endif
     &                    GRID(ng) % omn,                               &
     &                    GRID(ng) % om_u,                              &
     &                    GRID(ng) % om_v,                              &
     &                    GRID(ng) % on_u,                              &
     &                    GRID(ng) % on_v,                              &
     &                    GRID(ng) % pm,                                &
     &                    GRID(ng) % pn,                                &
     &                    GRID(ng) % Hz,                                &
     &                    GRID(ng) % Huon,                              &
     &                    GRID(ng) % Hvom,                              &
     &                    GRID(ng) % z_r,                               &
     &                    MIXING(ng) % Akt,                             &
     &                    OCEAN(ng) % W,                                &
# ifdef OMEGA_IMPLICIT
     &                    OCEAN(ng) % Wi,                               &
# endif
# ifdef WEC_VF
     &                    OCEAN(ng) % W_stokes,                         &
# endif
# if defined SEDIMENT && defined SED_MORPH
     &                    SEDBED(ng) % bed_thick,                       &
# endif
# if defined FLOATS && defined FLOAT_VWALK
     &                    MIXING(ng) % dAktdz,                          &
# endif
# ifdef DIAGNOSTICS_TS
     &                    DIAGS(ng) % DiaTwrk,                          &
# endif
     &                    OCEAN(ng) % t)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 35, __LINE__, MyFile)
# endif
!
      RETURN
      END SUBROUTINE step3d_t
!
!***********************************************************************
      SUBROUTINE step3d_t_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          IminS, ImaxS, JminS, JmaxS,             &
     &                          nrhs, nstp, nnew,                       &
# ifdef MASKING
     &                          rmask, umask, vmask,                    &
# endif
# ifdef WET_DRY
     &                          rmask_wet, umask_wet, vmask_wet,        &
# endif
     &                          omn, om_u, om_v, on_u, on_v,            &
     &                          pm, pn,                                 &
     &                          Hz, Huon, Hvom,                         &
     &                          z_r,                                    &
     &                          Akt,                                    &
     &                          W,                                      &
# ifdef OMEGA_IMPLICIT
     &                          Wi,                                     &
# endif
# ifdef WEC_VF
     &                          W_stokes,                               &
# endif
# if defined SEDIMENT && defined SED_MORPH
     &                          bed_thick,                              &
# endif
# if defined FLOATS && defined FLOAT_VWALK
     &                          dAktdz,                                 &
# endif
# ifdef DIAGNOSTICS_TS
     &                          DiaTwrk,                                &
# endif
     &                          t)
!***********************************************************************
!
      USE mod_param
      USE mod_clima
      USE mod_ncparam
# if defined NESTING && !defined ONE_WAY
      USE mod_nesting
# endif
      USE mod_scalars
      USE mod_sources
!
      USE exchange_3d_mod, ONLY : exchange_r3d_tile
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange3d
      USE mp_exchange_mod, ONLY : mp_exchange4d
# endif
      USE mpdata_adiff_mod
# ifdef NESTING
      USE nesting_mod, ONLY : bry_fluxes
# endif
      USE t3dbc_mod, ONLY : t3dbc_tile
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nrhs, nstp, nnew
!
# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:,LBj:)
      real(r8), intent(in) :: umask_wet(LBi:,LBj:)
      real(r8), intent(in) :: vmask_wet(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: omn(LBi:,LBj:)
      real(r8), intent(in) :: om_u(LBi:,LBj:)
      real(r8), intent(in) :: om_v(LBi:,LBj:)
      real(r8), intent(in) :: on_u(LBi:,LBj:)
      real(r8), intent(in) :: on_v(LBi:,LBj:)
      real(r8), intent(in) :: pm(LBi:,LBj:)
      real(r8), intent(in) :: pn(LBi:,LBj:)
      real(r8), intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), intent(in) :: Huon(LBi:,LBj:,:)
      real(r8), intent(in) :: Hvom(LBi:,LBj:,:)
      real(r8), intent(in) :: z_r(LBi:,LBj:,:)
#  ifdef SUN
      real(r8), intent(in) :: Akt(LBi:UBi,LBj:UBj,0:N(ng),NAT)
#  else
      real(r8), intent(in) :: Akt(LBi:,LBj:,0:,:)
#  endif
      real(r8), intent(in) :: W(LBi:,LBj:,0:)
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: Wi(LBi:,LBj:,0:)
#  endif
#  ifdef WEC_VF
      real(r8), intent(in) :: W_stokes(LBi:,LBj:,0:)
#  endif
#  if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(in) :: bed_thick(LBi:,LBj:,:)
#  endif
#  ifdef DIAGNOSTICS_TS
      real(r8), intent(inout) :: DiaTwrk(LBi:,LBj:,:,:,:)
#  endif
#  ifdef SUN
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#  else
      real(r8), intent(inout) :: t(LBi:,LBj:,:,:,:)
#  endif
#  if defined FLOATS && defined FLOAT_VWALK
      real(r8), intent(out) :: dAktdz(LBi:,LBj:,:)
#  endif

# else

#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask(LBi:UBi,LBj:UBj)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask_wet(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: omn(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: om_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: om_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: Hz(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: Huon(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: Hvom(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: Akt(LBi:UBi,LBj:UBj,0:N(ng),NAT)
      real(r8), intent(in) :: W(LBi:UBi,LBj:UBj,0:N(ng))
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: Wi(LBi:UBi,LBj:UBj,0:N(ng))
#  endif
#  ifdef WEC_VF
      real(r8), intent(in) :: W_stokes(LBi:UBi,LBj:UBj,0:N(ng))
#  endif
#  if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(in) :: bed_thick(LBi:UBi,LBj:UBj,3)
#  endif
#  ifdef DIAGNOSTICS_TS
      real(r8), intent(inout) :: DiaTwrk(LBi:UBi,LBj:UBj,N(ng),NT(ng),  &
     &                                   NDT)
#  endif
      real(r8), intent(inout) :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))

#  if defined FLOATS && defined FLOAT_VWALK
      real(r8), intent(out) :: dAktdz(LBi:UBi,LBj:UBj,N(ng))
#  endif
# endif
!
!  Local variable declarations.
!
      logical :: LapplySrc, Lhsimt, Lmpdata
!
# ifdef NESTING
      integer :: ILB, IUB, JLB, JUB
      integer :: dg, cr, rg
# endif
      integer :: IminT, ImaxT, JminT, JmaxT
      integer :: Isrc, Jsrc
      integer :: i, ic, ii, is, itrc, j, jj, k, ltrc
# if defined AGE_MEAN && defined T_PASSIVE
      integer :: iage
# endif
# ifdef DIAGNOSTICS_TS
      integer :: idiag
# endif
      real(r8) :: eps = 1.0E-16_r8
      real(r8) :: eps1 = 1.0E-12_r8

      real(r8) :: cff, cff1, cff2, cff3
      real(r8) :: betaL, betaR, betaD, betaU
      real(r8) :: rL, rR, rD, rU, rkaL, rkaR, rkaD, rkaU
      real(r8) :: a1, b1, sw, sw_eta, sw_xi

      real(r8), dimension(IminS:ImaxS) :: gradX, KaX, oKaX
      real(r8), dimension(JminS:JmaxS) :: gradE, KaE, oKaE
      real(r8), dimension(0:N(ng))     :: gradZ, KaZ, oKaZ

      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: CF
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: BC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: DC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC
# ifdef OMEGA_IMPLICIT
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FCmin
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FCmax
# endif

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: FE
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: FX
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: curv
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: grad

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,N(ng)) :: oHz

# ifdef DIAGNOSTICS_TS
      real(r8), allocatable :: Dhadv(:,:,:)
      real(r8), allocatable :: Dvadv(:,:,:,:)
# endif
      real(r8), allocatable :: Ta(:,:,:,:)
      real(r8), allocatable :: Ua(:,:,:)
      real(r8), allocatable :: Va(:,:,:)
      real(r8), allocatable :: Wa(:,:,:)

# include "set_bounds.h"

# ifdef NESTING
!
!  Notice that the trace flux boundary arrays are dimensioned with the
!  global dimensions of grid to facilitate processing.
!
      ILB=BOUNDS(ng)%LBi(-1)
      IUB=BOUNDS(ng)%UBi(-1)
      JLB=BOUNDS(ng)%LBj(-1)
      JUB=BOUNDS(ng)%UBj(-1)
# endif
!
!-----------------------------------------------------------------------
!  Time-step horizontal advection term.
!-----------------------------------------------------------------------
!
      Lhsimt =ANY(Hadvection(:,ng)%HSIMT).and.                          &
     &        ANY(Vadvection(:,ng)%HSIMT)
      Lmpdata=ANY(Hadvection(:,ng)%MPDATA).and.                         &
     &        ANY(Vadvection(:,ng)%MPDATA)
!
!  Allocate local arrays for MPDATA.
!
      IF (Lmpdata) THEN
# ifdef DIAGNOSTICS_TS
        IF (.not.allocated(Dhadv)) THEN
          allocate ( Dhadv(IminS:ImaxS,JminS:JmaxS,3) )
          Dhadv=0.0_r8
        END IF
        IF (.not.allocated(Dvadv)) THEN
          allocate ( Dvadv(IminS:ImaxS,JminS:JmaxS,N(ng),NT(ng)) )
          Dvadv=0.0_r8
        END IF
# endif
        IF (.not.allocated(Ta)) THEN
          allocate ( Ta(IminS:ImaxS,JminS:JmaxS,N(ng),NT(ng)) )
          Ta=0.0_r8
        END IF
        IF (.not.allocated(Ua)) THEN
          allocate ( Ua(IminS:ImaxS,JminS:JmaxS,N(ng)) )
          Ua=0.0_r8
        END IF
        IF (.not.allocated(Va)) THEN
          allocate ( Va(IminS:ImaxS,JminS:JmaxS,N(ng)) )
          Va=0.0_r8
        END IF
        IF (.not.allocated(Wa)) THEN
          allocate ( Wa(IminS:ImaxS,JminS:JmaxS,0:N(ng)) )
          Wa=0.0_r8
        END IF
      END IF
!
!  Compute reciprocal thickness, 1/Hz.
!
      IF (Lmpdata.or.Lhsimt) THEN
        DO k=1,N(ng)
          DO j=Jstrm2,Jendp2
            DO i=Istrm2,Iendp2
              oHz(i,j,k)=1.0_r8/Hz(i,j,k)
            END DO
          END DO
        END DO
      ELSE
        DO k=1,N(ng)
          DO j=Jstr,Jend
            DO i=Istr,Iend
              oHz(i,j,k)=1.0_r8/Hz(i,j,k)
            END DO
          END DO
        END DO
      END IF
!
!  Horizontal tracer advection.  It is possible to have a different
!  advection schme for each tracer.
!
      T_LOOP1 : DO itrc=1,NT(ng)
!
!  The MPDATA and HSIMT algorithms requires a three-point footprint, so
!  exchange boundary data on t(:,:,:,nnew,:) so other processes computed
!  earlier (horizontal diffusion, biology, or sediment) are accounted.
!
        IF ((Hadvection(itrc,ng)%MPDATA).or.                            &
     &      (Hadvection(itrc,ng)%HSIMT)) THEN
          IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
            CALL exchange_r3d_tile (ng, tile,                           &
     &                              LBi, UBi, LBj, UBj, 1, N(ng),       &
     &                              t(:,:,:,nnew,itrc))
          END IF

# ifdef DISTRIBUTE
          CALL mp_exchange3d (ng, tile, iNLM, 1,                        &
     &                        LBi, UBi, LBj, UBj, 1, N(ng),             &
     &                        NghostPoints,                             &
     &                        EWperiodic(ng), NSperiodic(ng),           &
     &                        t(:,:,:,nnew,itrc))
# endif
        END IF
!
!  Compute horizontal tracer advection fluxes.
!
        K_LOOP : DO k=1,N(ng)
!
          HADV_FLUX : IF (Hadvection(itrc,ng)%CENTERED2) THEN
!
!  Second-order, centered differences horizontal advective fluxes.
!
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                FX(i,j)=Huon(i,j,k)*                                    &
     &                  0.5_r8*(t(i-1,j,k,3,itrc)+                      &
     &                          t(i  ,j,k,3,itrc))
              END DO
            END DO
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                FE(i,j)=Hvom(i,j,k)*                                    &
     &                  0.5_r8*(t(i,j-1,k,3,itrc)+                      &
     &                          t(i,j  ,k,3,itrc))
              END DO
            END DO
!
          ELSE IF (Hadvection(itrc,ng)%MPDATA) THEN
!
!  First-order, upstream differences horizontal advective fluxes.
!
            DO j=JstrVm2,Jendp2i
              DO i=IstrUm2,Iendp3
                cff1=MAX(Huon(i,j,k),0.0_r8)
                cff2=MIN(Huon(i,j,k),0.0_r8)
                FX(i,j)=cff1*t(i-1,j,k,3,itrc)+                         &
     &                  cff2*t(i  ,j,k,3,itrc)
              END DO
            END DO
            DO j=JstrVm2,Jendp3
              DO i=IstrUm2,Iendp2i
                cff1=MAX(Hvom(i,j,k),0.0_r8)
                cff2=MIN(Hvom(i,j,k),0.0_r8)
                FE(i,j)=cff1*t(i,j-1,k,3,itrc)+                         &
     &                  cff2*t(i,j  ,k,3,itrc)
              END DO
            END DO
!
          ELSE IF (Hadvection(itrc,ng)%HSIMT) THEN
!
!  Third High-order Spatial Interpolation at the Middle Temporal level
!  (HSIMT; Wu and Zhu, 2010) with a Total Variation Diminishing (TVD)
!  limiter horizontal advection fluxes.
!
!  Hui Wu and Jianrong Zhu, 2010: Advection scheme with 3rd high-order
!    spatial interpolation at the middle temporal level and its
!    application to saltwater intrusion in the Changjiang Estuary,
!    Ocean Modelling 33, 33-51, doi:10.1016/j.ocemod.2009.12.001
!
            DO j=Jstr,Jend
              DO i=IstrU-1,Iendp2
                cff=0.125_r8*(pm(i-1,j)+pm(i,j))*(pn(i-1,j)+pn(i,j))*   &
     &              dt(ng)
                cff1=cff*(oHz(i-1,j,k)+oHz(i,j,k))
                gradX(i)=t(i,j,k,3,itrc)-t(i-1,j,k,3,itrc)
                KaX(i)=1.0_r8-ABS(Huon(i,j,k)*cff1)
# ifdef MASKING
                gradX(i)=gradX(i)*umask(i,j)
                KaX(i)=KaX(i)*umask(i,j)
# endif
              END DO
              IF (.not.EWperiodic(ng)) THEN
                IF (DOMAIN(ng)%Western_Edge(tile)) THEN
                  IF (Huon(Istr,j,k).ge.0.0_r8) THEN
                    gradX(Istr-1)=0.0_r8
                    KaX(Istr-1)=0.0_r8
                  END IF
                END IF
                IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
                  IF (Huon(Iend+1,j,k).lt.0.0_r8) THEN
                    gradX(Iend+2)=0.0_r8
                    KaX(Iend+2)=0.0_r8
                  END IF
                END IF
              END IF
              DO i=Istr,Iend+1
                IF (KaX(i).le.eps1) THEN
                  oKaX(i)=0.0_r8
                ELSE
                  oKaX(i)=1.0_r8/MAX(KaX(i),eps1)
                END IF
                IF (Huon(i,j,k).ge.0.0_r8) THEN
                  IF (ABS(gradX(i)).le.eps1) THEN
                    rL=0.0_r8
                    rkaL=0.0_r8
                  ELSE
                    rL=gradX(i-1)/gradX(i)
                    rkaL=KaX(i-1)*oKaX(i)
                  END IF
                  a1= cc1*KaX(i)+cc2-cc3*oKaX(i)
                  b1=-cc1*KaX(i)+cc2+cc3*oKaX(i)
                  betaL=a1+b1*rL
                  cff=0.5_r8*MAX(0.0_r8,                                &
     &                           MIN(2.0_r8, 2.0_r8*rL*rkaL, betaL))*   &
     &                gradX(i)*KaX(i)
# ifdef MASKING
                  ii=MAX(i-2,0)
                  cff=cff*rmask(ii,j)
# endif
                  sw_xi=t(i-1,j,k,3,itrc)+cff
                ELSE
                  IF (ABS(gradX(i)).le.eps1) THEN
                    rR=0.0_r8
                    rkaR=0.0_r8
                  ELSE
                    rR=gradX(i+1)/gradX(i)
                    rkaR=KaX(i+1)*oKaX(i)
                  END IF
                  a1= cc1*KaX(i)+cc2-cc3*oKaX(i)
                  b1=-cc1*KaX(i)+cc2+cc3*oKaX(i)
                  betaR=a1+b1*rR
                  cff=0.5_r8*MAX(0.0_r8,                                &
     &                           MIN(2.0_r8, 2.0_r8*rR*rkaR, betaR))*   &
     &                gradX(i)*KaX(i)
# ifdef MASKING
                  ii=MIN(i+1,Lm(ng)+1)
                  cff=cff*rmask(ii,j)
# endif
                  sw_xi=t(i,j,k,3,itrc)-cff
                END IF
                FX(i,j)=sw_xi*Huon(i,j,k)
              END DO
            END DO
!
            DO i=Istr,Iend
              DO j=JstrV-1,Jendp2
                cff=0.125_r8*(pn(i,j)+pn(i,j-1))*(pm(i,j)+pm(i,j-1))*   &
     &              dt(ng)
                cff1=cff*(oHz(i,j,k)+oHz(i,j-1,k))
                gradE(j)=t(i,j,k,3,itrc)-t(i,j-1,k,3,itrc)
                KaE(j)=1.0_r8-ABS(Hvom(i,j,k)*cff1)
# ifdef MASKING
                gradE(j)=gradE(j)*vmask(i,j)
                KaE(j)=KaE(j)*vmask(i,j)
# endif
              END DO
              IF (.not.NSperiodic(ng)) THEN
                IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
                  IF (Hvom(i,Jstr,k).ge.0.0_r8) THEN
                    gradE(Jstr-1)=0.0_r8
                    KaE(Jstr-1)=0.0_r8
                  END IF
                END IF
                IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
                  IF (Hvom(i,Jend+1,k).lt.0.0_r8) THEN
                    gradE(Jend+2)=0.0_r8
                    KaE(Jend+2)=0.0_r8
                  END IF
                END IF
              END IF
              DO j=Jstr,Jend+1
                IF (KaE(j).le.eps1) THEN
                  oKaE(j)=0.0_r8
                ELSE
                  oKaE(j)=1.0_r8/MAX(KaE(j),eps1)
                END IF
                IF (Hvom(i,j,k).ge.0.0_r8) THEN
                  IF (ABS(gradE(j)).le.eps1) THEN
                    rD=0.0_r8
                    rkaD=0.0_r8
                  ELSE
                    rD=gradE(j-1)/gradE(j)
                    rkaD=KaE(j-1)*oKaE(j)
                  END IF
                  a1= cc1*KaE(j)+cc2-cc3*oKaE(j)
                  b1=-cc1*KaE(j)+cc2+cc3*oKaE(j)
                  betaD=a1+b1*rD
                  cff=0.5_r8*MAX(0.0_r8,                                &
     &                           MIN(2.0_r8, 2.0_r8*rD*rkaD, betaD))*   &
     &                gradE(j)*KaE(j)
# ifdef MASKING
                  jj=MAX(j-2,0)
                  cff=cff*rmask(i,jj)
# endif
                  sw_eta=t(i,j-1,k,3,itrc)+cff
                ELSE
                  IF (ABS(gradE(j)).le.eps1) THEN
                    rU=0.0_r8
                    rkaU=0.0_r8
                  ELSE
                    rU=gradE(j+1)/gradE(j)
                    rkaU=KaE(j+1)*oKaE(j)
                  END IF
                  a1= cc1*KaE(j)+cc2-cc3*oKaE(j)
                  b1=-cc1*KaE(j)+cc2+cc3*oKaE(j)
                  betaU=a1+b1*rU
                  cff=0.5*MAX(0.0_r8,                                   &
     &                        MIN(2.0_r8, 2.0_r8*rU*rkaU, betaU))*      &
     &                gradE(j)*KaE(j)
# ifdef MASKING
                  jj=MIN(j+1,Mm(ng)+1)
                  cff=cff*rmask(i,jj)
# endif
                  sw_eta=t(i,j,k,3,itrc)-cff
                END IF
                FE(i,j)=sw_eta*Hvom(i,j,k)
              END DO
            END DO
!
          ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.                     &
     &             (Hadvection(itrc,ng)%CENTERED4).or.                  &
     &             (Hadvection(itrc,ng)%SPLIT_U3).or.                   &
     &             (Hadvection(itrc,ng)%UPSTREAM3)) THEN
!
!  Fourth-order Akima, fourth-order centered differences, or third-order
!  upstream-biased horizontal advective fluxes.
!
            DO j=Jstr,Jend
              DO i=Istrm1,Iendp2
                FX(i,j)=t(i  ,j,k,3,itrc)-                              &
     &                  t(i-1,j,k,3,itrc)
# ifdef MASKING
                FX(i,j)=FX(i,j)*umask(i,j)
# endif
              END DO
            END DO
            IF (.not.(CompositeGrid(iwest,ng).or.EWperiodic(ng))) THEN
              IF (DOMAIN(ng)%Western_Edge(tile)) THEN
                DO j=Jstr,Jend
                  FX(Istr-1,j)=FX(Istr,j)
                END DO
              END IF
            END IF
            IF (.not.(CompositeGrid(ieast,ng).or.EWperiodic(ng))) THEN
              IF (DOMAIN(ng)%Eastern_Edge(tile)) THEN
                DO j=Jstr,Jend
                  FX(Iend+2,j)=FX(Iend+1,j)
                END DO
              END IF
            END IF
!
            DO j=Jstr,Jend
              DO i=Istr-1,Iend+1
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  curv(i,j)=FX(i+1,j)-FX(i,j)
                ELSE IF (Hadvection(itrc,ng)%AKIMA4) THEN
                  cff=2.0_r8*FX(i+1,j)*FX(i,j)
                  IF (cff.gt.eps) THEN
                    grad(i,j)=cff/(FX(i+1,j)+FX(i,j))
                  ELSE
                    grad(i,j)=0.0_r8
                  END IF
                ELSE IF ((Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  grad(i,j)=0.5_r8*(FX(i+1,j)+FX(i,j))
                END IF
              END DO
            END DO
!
            cff1=1.0_r8/6.0_r8
            cff2=1.0_r8/3.0_r8
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  FX(i,j)=Huon(i,j,k)*0.5_r8*                           &
     &                    (t(i-1,j,k,3,itrc)+                           &
     &                     t(i  ,j,k,3,itrc))-                          &
     &                    cff1*(curv(i-1,j)*MAX(Huon(i,j,k),0.0_r8)+    &
     &                          curv(i  ,j)*MIN(Huon(i,j,k),0.0_r8))
                ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.               &
     &                   (Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  FX(i,j)=Huon(i,j,k)*0.5_r8*                           &
     &                    (t(i-1,j,k,3,itrc)+                           &
     &                     t(i  ,j,k,3,itrc)-                           &
     &                     cff2*(grad(i  ,j)-                           &
     &                           grad(i-1,j)))
                END IF
              END DO
            END DO
!
            DO j=Jstrm1,Jendp2
              DO i=Istr,Iend
                FE(i,j)=t(i,j  ,k,3,itrc)-                              &
     &                  t(i,j-1,k,3,itrc)
# ifdef MASKING
                FE(i,j)=FE(i,j)*vmask(i,j)
# endif
              END DO
            END DO
            IF (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))) THEN
              IF (DOMAIN(ng)%Southern_Edge(tile)) THEN
                DO i=Istr,Iend
                  FE(i,Jstr-1)=FE(i,Jstr)
                END DO
              END IF
            END IF
            IF (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))) THEN
              IF (DOMAIN(ng)%Northern_Edge(tile)) THEN
                DO i=Istr,Iend
                  FE(i,Jend+2)=FE(i,Jend+1)
                END DO
              END IF
            END IF
!
            DO j=Jstr-1,Jend+1
              DO i=Istr,Iend
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  curv(i,j)=FE(i,j+1)-FE(i,j)
                ELSE IF (Hadvection(itrc,ng)%AKIMA4) THEN
                  cff=2.0_r8*FE(i,j+1)*FE(i,j)
                  IF (cff.gt.eps) THEN
                    grad(i,j)=cff/(FE(i,j+1)+FE(i,j))
                  ELSE
                    grad(i,j)=0.0_r8
                  END IF
                ELSE IF ((Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  grad(i,j)=0.5_r8*(FE(i,j+1)+FE(i,j))
                END IF
              END DO
            END DO
!
            cff1=1.0_r8/6.0_r8
            cff2=1.0_r8/3.0_r8
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
                  FE(i,j)=Hvom(i,j,k)*0.5_r8*                           &
     &                    (t(i,j-1,k,3,itrc)+                           &
     &                     t(i,j  ,k,3,itrc))-                          &
     &                    cff1*(curv(i,j-1)*MAX(Hvom(i,j,k),0.0_r8)+    &
     &                          curv(i,j  )*MIN(Hvom(i,j,k),0.0_r8))
                ELSE IF ((Hadvection(itrc,ng)%AKIMA4).or.               &
     &                   (Hadvection(itrc,ng)%CENTERED4).or.            &
     &                   (Hadvection(itrc,ng)%SPLIT_U3)) THEN
                  FE(i,j)=Hvom(i,j,k)*0.5_r8*                           &
     &                    (t(i,j-1,k,3,itrc)+                           &
     &                     t(i,j  ,k,3,itrc)-                           &
     &                     cff2*(grad(i,j  )-                           &
     &                           grad(i,j-1)))
                END IF
              END DO
            END DO
          END IF HADV_FLUX
!
!  Apply tracers point sources to the horizontal advection terms,
!  if any.
!
!    Dsrc(is) = 0,  flow across grid cell u-face (positive or negative)
!    Dsrc(is) = 1,  flow across grid cell v-face (positive or negative)
!
          IF (LuvSrc(ng)) THEN
            DO is=1,Nsrc(ng)
              Isrc=SOURCES(ng)%Isrc(is)
              Jsrc=SOURCES(ng)%Jsrc(is)
              IF (INT(SOURCES(ng)%Dsrc(is)).eq.0) THEN
                IF ((Hadvection(itrc,ng)%MPDATA).or.                    &
     &              (Hadvection(itrc,ng)%HSIMT)) THEN
                  LapplySrc=(IstrUm2.le.Isrc).and.                      &
     &                      (Isrc.le.Iendp3).and.                       &
     &                      (JstrVm2.le.Jsrc).and.                      &
     &                      (Jsrc.le.Jendp2i)
                ELSE
                  LapplySrc=(Istr.le.Isrc).and.                         &
     &                      (Isrc.le.Iend+1).and.                       &
     &                      (Jstr.le.Jsrc).and.                         &
     &                      (Jsrc.le.Jend)
                END IF
                IF (LapplySrc) THEN
                  IF (LtracerSrc(itrc,ng)) THEN
                    FX(Isrc,Jsrc)=Huon(Isrc,Jsrc,k)*                    &
     &                            SOURCES(ng)%Tsrc(is,k,itrc)
# ifdef MASKING
                  ELSE
                    IF ((rmask(Isrc  ,Jsrc).eq.0.0_r8).and.             &
     &                  (rmask(Isrc-1,Jsrc).eq.1.0_r8)) THEN
                      FX(Isrc,Jsrc)=Huon(Isrc,Jsrc,k)*                  &
     &                              t(Isrc-1,Jsrc,k,3,itrc)
                    ELSE IF ((rmask(Isrc  ,Jsrc).eq.1.0_r8).and.        &
     &                       (rmask(Isrc-1,Jsrc).eq.0.0_r8)) THEN
                      FX(Isrc,Jsrc)=Huon(Isrc,Jsrc,k)*                  &
     &                              t(Isrc  ,Jsrc,k,3,itrc)
                    END IF
# endif
                  END IF
                END IF
              ELSE IF (INT(SOURCES(ng)%Dsrc(is)).eq.1) THEN
                IF ((Hadvection(itrc,ng)%MPDATA).or.                    &
     &              (Hadvection(itrc,ng)%HSIMT)) THEN
                  LapplySrc=(IstrUm2.le.Isrc).and.                      &
     &                      (Isrc.le.Iendp2i).and.                      &
     &                      (JstrVm2.le.Jsrc).and.                      &
     &                      (Jsrc.le.Jendp3)
                ELSE
                  LapplySrc=(Istr.le.Isrc).and.                         &
     &                      (Isrc.le.Iend).and.                         &
     &                      (Jstr.le.Jsrc).and.                         &
     &                      (Jsrc.le.Jend+1)
                END IF
                IF (LapplySrc) THEN
                  IF (LtracerSrc(itrc,ng)) THEN
                    FE(Isrc,Jsrc)=Hvom(Isrc,Jsrc,k)*                    &
     &                            SOURCES(ng)%Tsrc(is,k,itrc)
# ifdef MASKING
                  ELSE
                    IF ((rmask(Isrc,Jsrc  ).eq.0.0_r8).and.             &
     &                  (rmask(Isrc,Jsrc-1).eq.1.0_r8)) THEN
                      FE(Isrc,Jsrc)=Hvom(Isrc,Jsrc,k)*                  &
     &                              t(Isrc,Jsrc-1,k,3,itrc)
                    ELSE IF ((rmask(Isrc,Jsrc  ).eq.1.0_r8).and.        &
     &                       (rmask(Isrc,Jsrc-1).eq.0.0_r8)) THEN
                      FE(Isrc,Jsrc)=Hvom(Isrc,Jsrc,k)*                  &
     &                              t(Isrc,Jsrc  ,k,3,itrc)
                    END IF
# endif
                  END IF
                END IF
              END IF
            END DO
          END IF

# if defined NESTING && !defined ONE_WAY
!
!  If refinement grids, extract tracer horizontal advection fluxes
!  (Hz*u*T/n, Hz*v*T/m) at the grid contact boundary (physical
!  domain perimeter) to be used in two-way nesting.
!
          IF (RefinedGrid(ng)) THEN
            DO cr=1,Ncontact
              dg=Rcontact(cr)%donor_grid
              rg=Rcontact(cr)%receiver_grid
              IF (ng.eq.rg) THEN
                CALL bry_fluxes (dg, rg, cr, iNLM, tile,                &
     &                           IminS, ImaxS, JminS, JmaxS,            &
     &                           ILB, IUB, JLB, JUB,                    &
     &                           dt(ng), FX, FE,                        &
     &                          BRY_CONTACT(iwest, cr)%Tflux(:,k,itrc), &
     &                          BRY_CONTACT(ieast, cr)%Tflux(:,k,itrc), &
     &                          BRY_CONTACT(isouth,cr)%Tflux(:,k,itrc), &
     &                          BRY_CONTACT(inorth,cr)%Tflux(:,k,itrc))
              END IF
            END DO
          END IF
# endif
!
!  If MPDATA, time-step horizontal advection for intermediate diffusive
!  tracer, Ta (m Tunits).
!
          HADV_STEPPING : IF (Hadvection(itrc,ng)%MPDATA) THEN
            DO j=JstrVm2,Jendp2i
              DO i=IstrUm2,Iendp2i
                cff=dt(ng)*pm(i,j)*pn(i,j)
                cff1=cff*(FX(i+1,j)-FX(i,j))
                cff2=cff*(FE(i,j+1)-FE(i,j))
                cff3=cff1+cff2
                Ta(i,j,k,itrc)=t(i,j,k,nnew,itrc)-cff3
# ifdef DIAGNOSTICS_TS
                Dhadv(i,j,iTxadv)=-cff1
                Dhadv(i,j,iTyadv)=-cff2
                Dhadv(i,j,iThadv)=-cff3
# endif
              END DO
            END DO
# ifdef DIAGNOSTICS_TS
!
            DO j=Jstr,Jend
              DO i=Istr,Iend
                DiaTwrk(i,j,k,itrc,iTxadv)=Dhadv(i,j,iTxadv)
                DiaTwrk(i,j,k,itrc,iTyadv)=Dhadv(i,j,iTyadv)
                DiaTwrk(i,j,k,itrc,iThadv)=Dhadv(i,j,iThadv)
              END DO
            END DO
# endif
!
!  OTHERWISE, time-step horizontal advection term.  Advective fluxes
!  have units of Tunits m3/s.  The new tracer has units of m Tunits.
!
          ELSE
            DO j=Jstr,Jend
              DO i=Istr,Iend
                cff=dt(ng)*pm(i,j)*pn(i,j)
                cff1=cff*(FX(i+1,j)-FX(i,j))
                cff2=cff*(FE(i,j+1)-FE(i,j))
                cff3=cff1+cff2
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-cff3
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTxadv)=-cff1
                DiaTwrk(i,j,k,itrc,iTyadv)=-cff2
                DiaTwrk(i,j,k,itrc,iThadv)=-cff3
# endif
              END DO
            END DO
          END IF HADV_STEPPING
        END DO K_LOOP
      END DO T_LOOP1
!
!-----------------------------------------------------------------------
!  Time-step vertical advection term.
!-----------------------------------------------------------------------
!
      T_LOOP2 : DO itrc=1,NT(ng)
        IF (Vadvection(itrc,ng)%MPDATA) THEN
          JminT=JstrVm2
          JmaxT=Jendp2i
        ELSE
          JminT=Jstr
          JmaxT=Jend
        END IF
!
        J_LOOP1 : DO j=JminT,JmaxT              ! start pipelined J-loop
!
          VADV_FLUX : IF (Vadvection(itrc,ng)%SPLINES) THEN
!
!  Build conservative parabolic splines for the vertical derivatives
!  "FC" of the tracer.  Then, the interfacial "FC" values are
!  converted to vertical advective flux (Tunits m3/s).
!
            DO i=Istr,Iend
# ifdef NEUMANN
              FC(i,0)=1.5_r8*t(i,j,1,3,itrc)
              CF(i,1)=0.5_r8
# else
              FC(i,0)=2.0_r8*t(i,j,1,3,itrc)
              CF(i,1)=1.0_r8
# endif
            END DO
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff=1.0_r8/(2.0_r8*Hz(i,j,k)+                           &
     &                      Hz(i,j,k+1)*(2.0_r8-CF(i,k)))
                CF(i,k+1)=cff*Hz(i,j,k)
                FC(i,k)=cff*(3.0_r8*(Hz(i,j,k  )*t(i,j,k+1,3,itrc)+     &
     &                               Hz(i,j,k+1)*t(i,j,k  ,3,itrc))-    &
     &                       Hz(i,j,k+1)*FC(i,k-1))
              END DO
            END DO
            DO i=Istr,Iend
# ifdef NEUMANN
              FC(i,N(ng))=(3.0_r8*t(i,j,N(ng),3,itrc)-FC(i,N(ng)-1))/   &
     &                    (2.0_r8-CF(i,N(ng)))
# else
              FC(i,N(ng))=(2.0_r8*t(i,j,N(ng),3,itrc)-FC(i,N(ng)-1))/   &
     &                    (1.0_r8-CF(i,N(ng)))
# endif
            END DO
            DO k=N(ng)-1,0,-1
              DO i=Istr,Iend
                FC(i,k)=FC(i,k)-CF(i,k+1)*FC(i,k+1)
#  ifdef WEC_VF
                FC(i,k+1)=(W(i,j,k+1)+W_stokes(i,j,k+1))*FC(i,k+1)
#  else
                FC(i,k+1)=W(i,j,k+1)*FC(i,k+1)
#  endif
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,N(ng))=0.0_r8
              FC(i,0)=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%AKIMA4) THEN
!
!  Fourth-order, Akima vertical advective flux (Tunits m3/s).
!
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                FC(i,k)=t(i,j,k+1,3,itrc)-                              &
     &                  t(i,j,k  ,3,itrc)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=FC(i,1)
              FC(i,N(ng))=FC(i,N(ng)-1)
            END DO
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff=2.0_r8*FC(i,k)*FC(i,k-1)
                IF (cff.gt.eps) THEN
                  CF(i,k)=cff/(FC(i,k)+FC(i,k-1))
                ELSE
                  CF(i,k)=0.0_r8
                END IF
              END DO
            END DO
            cff1=1.0_r8/3.0_r8
            DO k=1,N(ng)-1
              DO i=Istr,Iend
# ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
# else
                FC(i,k)=W(i,j,k)*                                       &
# endif
     &                  0.5_r8*(t(i,j,k  ,3,itrc)+                      &
     &                          t(i,j,k+1,3,itrc)-                      &
     &                          cff1*(CF(i,k+1)-CF(i,k)))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%CENTERED2) THEN
!
!  Second-order, central differences vertical advective flux
!  (Tunits m3/s).
!
            DO k=1,N(ng)-1
              DO i=Istr,Iend
# ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
# else
                FC(i,k)=W(i,j,k)*                                       &
# endif
     &                  0.5_r8*(t(i,j,k  ,3,itrc)+                      &
     &                          t(i,j,k+1,3,itrc))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%MPDATA) THEN
!
!  First_order, upstream differences vertical advective flux
!  (Tunits m3/s).
!
            DO i=IstrUm2,Iendp2i
              DO k=1,N(ng)-1
# ifdef WEC_VF
                cff1=MAX(W(i,j,k)+W_stokes(i,j,k),0.0_r8)
                cff2=MIN(W(i,j,k)+W_stokes(i,j,k),0.0_r8)
# else
                cff1=MAX(W(i,j,k),0.0_r8)
                cff2=MIN(W(i,j,k),0.0_r8)
# endif
                FC(i,k)=cff1*t(i,j,k  ,3,itrc)+                         &
     &                  cff2*t(i,j,k+1,3,itrc)
              END DO
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF (Vadvection(itrc,ng)%HSIMT) THEN
!
!  Third High-order Spatial Interpolation at the Middle Temporal level
!  (HSIMT; Wu and Zhu, 2010) with a Total Variation Diminishing (TVD)
!  limiter vertical advection flux (Tunits m3/s).
!
            DO i=Istr,Iend
              KaZ(0)=0.0_r8
              oKaZ(0)=0.0_r8
              gradZ(0)=0.0_r8
              DO k=1,N(ng)-1
                cff=pm(i,j)*pn(i,j)*dt(ng)
# ifdef WEC_VF
                KaZ(k)=1.0_r8-ABS(cff*(W(i,j,k)+W_stokes(i,j,k))/       &
     &                            (z_r(i,j,k+1)-z_r(i,j,k)))
# else
                KaZ(k)=1.0_r8-ABS(cff*W(i,j,k)/                         &
     &                            (z_r(i,j,k+1)-z_r(i,j,k)))
# endif
                oKaZ(k)=1.0_r8/KaZ(k)
                gradZ(k)=t(i,j,k+1,3,itrc)-t(i,j,k,3,itrc)
              END DO
              KaZ(N(ng))=0.0_r8
              okaZ(N(ng))=0.0_r8
              gradZ(N(ng))=0.0_r8
!
              DO k=1,N(ng)-1
# ifdef WEC_VF
                cff1=W(i,j,k)+W_stokes(i,j,k)
# else
                cff1=W(i,j,k)
# endif
                IF ((k.eq.1).and.(cff1.ge.0.0_r8)) THEN
                  FC(i,k)=cff1*t(i,j,k,3,itrc)
                ELSE IF ((k.eq.N(ng)-1).and.(cff1.lt.0.0_r8)) THEN
                  FC(i,k)=cff1*t(i,j,k+1,3,itrc)
                ELSE
                  IF (cff1.ge.0) THEN
                    IF (ABS(gradZ(k)).le.eps1) THEN
                      rD=0.0_r8
                      rkaD=0.0_r8
                    ELSE
                      rD=gradZ(k-1)/gradZ(k)
                      rkaD=KaZ(k-1)*oKaZ(k)
                    END IF
                    a1= cc1*KaZ(k)+cc2-cc3*oKaZ(k)
                    b1=-cc1*KaZ(k)+cc2+cc3*oKaZ(k)
                    betaD=a1+b1*rD
                    cff=0.5_r8*MAX(0.0_r8,                              &
     &                             MIN(2.0_r8, 2.0_r8*rD*rkaD, betaD))* &
     &                  gradZ(k)*KaZ(k)
                    sw=t(i,j,k,3,itrc)+cff
                  ELSE
                    IF (ABS(gradZ(k)).le.eps1) THEN
                      rU=0.0_r8
                      rkaU=0.0_r8
                    ELSE
                      rU=gradZ(k+1)/gradZ(k)
                      rkaU=KaZ(k+1)*oKaZ(k)
                    END IF
                    a1= cc1*KaZ(k)+cc2-cc3*oKaZ(k)
                    b1=-cc1*KaZ(k)+cc2+cc3*oKaZ(k)
                    betaU=a1+b1*rU
                    cff=0.5_r8*MAX(0.0_r8,                              &
     &                             MIN(2.0_r8, 2.0_r8*rU*rkaU, betaU))* &
     &                  gradZ(k)*KaZ(k)
                    sw=t(i,j,k+1,3,itrc)-cff
                  END IF
                  FC(i,k)=cff1*sw
                END IF
              END DO
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
          ELSE IF ((Vadvection(itrc,ng)%CENTERED4).or.                  &
     &             (Vadvection(itrc,ng)%SPLIT_U3)) THEN
!
!  Fourth-order, central differences vertical advective flux
!  (Tunits m3/s).
!
            cff1=0.5_r8
            cff2=7.0_r8/12.0_r8
            cff3=1.0_r8/12.0_r8
            DO k=2,N(ng)-2
              DO i=Istr,Iend
# ifdef WEC_VF
                FC(i,k)=(W(i,j,k)+W_stokes(i,j,k))*                     &
# else
                FC(i,k)=W(i,j,k)*                                       &
# endif
     &                  (cff2*(t(i,j,k  ,3,itrc)+                       &
     &                         t(i,j,k+1,3,itrc))-                      &
     &                   cff3*(t(i,j,k-1,3,itrc)+                       &
     &                         t(i,j,k+2,3,itrc)))
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
# ifdef WEC_VF
              FC(i,1)=(W(i,j,1)+W_stokes(i,j,1))*                       &
# else
              FC(i,1)=W(i,j,1)*                                         &
# endif
     &                (cff1*t(i,j,1,3,itrc)+                            &
     &                 cff2*t(i,j,2,3,itrc)-                            &
     &                 cff3*t(i,j,3,3,itrc))
# ifdef WEC_VF
              FC(i,N(ng)-1)=(W(i,j,N(ng)-1)+W_stokes(i,j,N(ng)-1))*     &
# else
              FC(i,N(ng)-1)=W(i,j,N(ng)-1)*                             &
# endif
     &                    (cff1*t(i,j,N(ng)  ,3,itrc)+                  &
     &                     cff2*t(i,j,N(ng)-1,3,itrc)-                  &
     &                     cff3*t(i,j,N(ng)-2,3,itrc))
              FC(i,N(ng))=0.0_r8
            END DO
          END IF VADV_FLUX
!
!  If MPDATA and cell-centered point source (LwSrc), augment the
!  intermediate tracer, Ta, with the tracer divergence. For other
!  advection schemes LwSrc is applied after the vertical advection
!  is completed (J. Wilkin).
!
!    Dsrc(is) = 2,  flow across grid cell w-face (positive or negative)
!
          IF (LwSrc(ng)) THEN
            IF (Vadvection(itrc,ng)%MPDATA) THEN
              DO is=1,Nsrc(ng)
                IF (INT(SOURCES(ng)%Dsrc(is)).eq.2) THEN
                  Isrc=SOURCES(ng)%Isrc(is)
                  Jsrc=SOURCES(ng)%Jsrc(is)
                  IF (((Istr.le.Isrc).and.(Isrc.le.Iend+1)).and.        &
     &                 ((Jstr.le.Jsrc).and.(Jsrc.le.Jend+1)).and.       &
     &                 (j.eq.Jsrc)) THEN
                    DO k=1,N(ng)
                      cff=dt(ng)*pm(Isrc,Jsrc)*pn(Isrc,Jsrc)
                      IF (LtracerSrc(itrc,ng)) THEN
                        cff3=SOURCES(ng)%Tsrc(is,k,itrc)
                      ELSE
                        cff3=t(Isrc,Jsrc,k,3,itrc)
                      END IF
                      Ta(Isrc,Jsrc,k,itrc)=Ta(Isrc,Jsrc,k,itrc)+        &
     &                                     cff*SOURCES(ng)%Qsrc(is,k)*  &
     &                                     cff3
                    END DO
                  END IF
                END IF
              END DO
            END IF
          END IF

# ifdef SED_MORPH
!
!  If MPDATA, augment the intermediate tracer, Ta, with the tracer
!  divergence. For other advection schemes the bed change is applied
!  after the vertical advection is completed (similar to Lwsrc).
!  Use 1/N to distribute the bed change in each water column layer.
!
          IF (Vadvection(itrc,ng)%MPDATA) THEN
            cff1=1.0_r8/N(ng)
            DO k=1,N(ng)
              DO i=IstrUm2,Iendp2i
                cff=cff1*(bed_thick(i,j,nstp)-bed_thick(i,j,3))
                cff3=t(i,j,k,3,itrc)
                Ta(i,j,k,itrc)=Ta(i,j,k,itrc)-cff*cff3
              END DO
            END DO
          END IF
# endif
!
!  If MPDATA, time-step vertical advection for intermediate diffusive
!  tracer, Ta (Tunits).
# ifdef DIAGNOSTICS_TS
!  Convert units of tracer diagnostic terms to Tunits.
# endif
!
          VADV_STEPPING : IF (Vadvection(itrc,ng)%MPDATA) THEN
            DO i=IstrUm2,Iendp2i
              CF(i,0)=dt(ng)*pm(i,j)*pn(i,j)
            END DO
            DO k=1,N(ng)
              DO i=IstrUm2,Iendp2i
                cff1=CF(i,0)*(FC(i,k)-FC(i,k-1))
                Ta(i,j,k,itrc)=(Ta(i,j,k,itrc)-cff1)*oHz(i,j,k)
# ifdef DIAGNOSTICS_TS
                Dvadv(i,j,k,itrc)=-cff1
# endif
              END DO
            END DO

# ifdef OMEGA_IMPLICIT
!
!  If MPDATA, compute off-diagonal coefficients FC [dt*Wi*pm*pn] for the
!  implicit vertical advection term located at horizontal RHO-points and
!  vertical W-points. Also, set FC at the top and bottom levels.
!
!  It needs to be done after the Wexplicit and before the anti-diffusion
!  steps.
!
            cff=dt(ng)
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff1=cff*pm(i,j)*pn(i,j)
                FCmax(i,k)=MAX(Wi(i,j,k),0.0_r8)*cff1
                FCmin(i,k)=MIN(Wi(i,j,k),0.0_r8)*cff1
              END DO
            END DO
            DO i=Istr,Iend
              FCmax(i,0)=0.0_r8
              FCmin(i,0)=0.0_r8
              FCmax(i,N(ng))=0.0_r8
              FCmin(i,N(ng))=0.0_r8
            END DO
!
!  Compute diagonal matrix coefficients BC and load right-hand-side
!  terms for the tracer equation into DC.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                BC(i,k)=Hz(i,j,k)+FCmax(i,k)-FCmin(i,k-1)
                DC(i,k)=Ta(i,j,k,itrc)*Hz(i,j,k)
              END DO
            END DO
!
!  Solve the tridiagonal system.
!
            DO i=Istr,Iend
              cff=1.0_r8/BC(i,1)
              CF(i,1)=cff*FCmin(i,1)
              DC(i,1)=cff*DC(i,1)
            END DO
            DO k=2,N(ng)-1
              DO i=Istr,Iend
                cff=1.0_r8/(BC(i,k)+FCmax(i,k-1)*CF(i,k-1))
                CF(i,k)=cff*FCmin(i,k)
                DC(i,k)=cff*(DC(i,k)+FCmax(i,k-1)*DC(i,k-1))
              END DO
            END DO
!
!  Compute new solution by back substitution.
!
            DO i=Istr,Iend
#  ifdef DIAGNOSTICS_TS
              cff1=Ta(i,j,N(ng),itrc)*oHz(i,j,N(ng))
#  endif
              cff=1.0_r8/(BC(i,N(ng))+FCmax(i,N(ng)-1)*CF(i,N(ng)-1))
              DC(i,N(ng))=cff*(DC(i,N(ng))+                            &
     &                         FCmax(i,N(ng)-1)*DC(i,N(ng)-1))
              Ta(i,j,N(ng),itrc)=DC(i,N(ng))
#  ifdef DIAGNOSTICS_TS
              DiaTwrk(i,j,N(ng),itrc,iTvadv)=                          &
     &                             DiaTwrk(i,j,N(ng),itrc,iTvadv)+     &
     &                             Ta(i,j,N(ng),itrc)-cff1
#  endif
            END DO
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
#  ifdef DIAGNOSTICS_TS
                cff1=Ta(i,j,k,itrc)*oHz(i,j,k)
#  endif
                DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
                Ta(i,j,k,itrc)=DC(i,k)
#  ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvadv)=DiaTwrk(i,j,k,itrc,iTvadv)+&
     &                                     Ta(i,j,k,itrc)-cff1
#  endif
              END DO
            END DO
# endif
!
!  OTHERWISE, time-step vertical advection term (m Tunits, or Tunits if
!  conservative, parabolic splines diffusion).
# ifdef DIAGNOSTICS_TS
!  Convert units of tracer diagnostic terms to Tunits.
# endif
!
          ELSE
            DO i=Istr,Iend
              CF(i,0)=dt(ng)*pm(i,j)*pn(i,j)
            END DO
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff1=CF(i,0)*(FC(i,k)-FC(i,k-1))
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-cff1
# ifdef SPLINES_VDIFF
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)*oHz(i,j,k)
# endif
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvadv)=-cff1
                DO idiag=1,NDT
                  DiaTwrk(i,j,k,itrc,idiag)=DiaTwrk(i,j,k,itrc,idiag)*  &
     &                                      oHz(i,j,k)
                END DO
# endif
              END DO
            END DO
          END IF VADV_STEPPING
        END DO J_LOOP1
      END DO T_LOOP2
!
!-----------------------------------------------------------------------
!  Compute anti-diffusive velocities to corrected advected tracers
!  using MPDATA recursive method.  Notice that pipelined J-loop ended.
!-----------------------------------------------------------------------
!
      T_LOOP3 : DO itrc=1,NT(ng)
        MPDATA : IF ((Hadvection(itrc,ng)%MPDATA).and.                  &
     &               (Vadvection(itrc,ng)%MPDATA)) THEN
          CALL mpdata_adiff_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
# ifdef MASKING
     &                            rmask, umask, vmask,                  &
# endif
# ifdef WET_DRY
     &                            rmask_wet, umask_wet, vmask_wet,      &
# endif
     &                            pm, pn, omn, om_u, on_v,              &
     &                            z_r, oHz,                             &
     &                            Huon, Hvom, W,                        &
#  ifdef WEC_VF
     &                            W_stokes,                             &
#  endif
#  ifdef OMEGA_IMPLICIT
     &                            Wi,                                   &
#  endif
     &                            t(:,:,:,3,itrc),                      &
     &                            Ta(:,:,:,itrc),  Ua, Va, Wa)
!
!  Compute anti-diffusive corrected advection fluxes.
!
          DO k=1,N(ng)
            DO j=Jstr,Jend
              DO i=Istr,Iend+1
                cff1=MAX(Ua(i,j,k),0.0_r8)
                cff2=MIN(Ua(i,j,k),0.0_r8)
                FX(i,j)=(cff1*Ta(i-1,j,k,itrc)+                         &
     &                   cff2*Ta(i  ,j,k,itrc))*                        &
     &                  0.5_r8*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)
              END DO
            END DO
            DO j=Jstr,Jend+1
              DO i=Istr,Iend
                cff1=MAX(Va(i,j,k),0.0_r8)
                cff2=MIN(Va(i,j,k),0.0_r8)
                FE(i,j)=(cff1*Ta(i,j-1,k,itrc)+                         &
     &                   cff2*Ta(i,j  ,k,itrc))*                        &
     &                  0.5_r8*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)
              END DO
            END DO
!
!  Time-step corrected horizontal advection (Tunits m).
!
            DO j=Jstr,Jend
              DO i=Istr,Iend
                cff=dt(ng)*pm(i,j)*pn(i,j)
                cff1=cff*(FX(i+1,j)-FX(i,j))
                cff2=cff*(FE(i,j+1)-FE(i,j))
                cff3=cff1+cff2
                t(i,j,k,nnew,itrc)=Ta(i,j,k,itrc)*Hz(i,j,k)-cff3
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTxadv)=DiaTwrk(i,j,k,itrc,iTxadv)-  &
     &                                     cff1
                DiaTwrk(i,j,k,itrc,iTyadv)=DiaTwrk(i,j,k,itrc,iTyadv)-  &
     &                                     cff2
                DiaTwrk(i,j,k,itrc,iThadv)=DiaTwrk(i,j,k,itrc,iThadv)-  &
     &                                     cff3
# endif
              END DO
            END DO
          END DO
!
!  Compute anti-diffusive corrected vertical advection flux.
!
          DO j=Jstr,Jend
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff1=MAX(Wa(i,j,k),0.0_r8)
                cff2=MIN(Wa(i,j,k),0.0_r8)
                FC(i,k)=cff1*Ta(i,j,k  ,itrc)+                          &
     &                  cff2*Ta(i,j,k+1,itrc)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
!  Time-step corrected vertical advection (Tunits).
# ifdef DIAGNOSTICS_TS
!  Convert units of tracer diagnostic terms to Tunits.
# endif
!
            DO i=Istr,Iend
              CF(i,0)=dt(ng)*pm(i,j)*pn(i,j)
            END DO
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff1=CF(i,0)*(FC(i,k)-FC(i,k-1))
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-cff1
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvadv)=Dvadv(i,j,k,itrc)-           &
     &                                     cff1
                DO idiag=1,NDT
                  DiaTwrk(i,j,k,itrc,idiag)=DiaTwrk(i,j,k,itrc,idiag)*  &
     &                                      oHz(i,j,k)
                END DO
# endif
              END DO
            END DO
          END DO
        END IF MPDATA
      END DO T_LOOP3
!
!-----------------------------------------------------------------------
!  Add tracer divergence due to cell-centered (LwSrc) point sources.
!-----------------------------------------------------------------------
!
!  When LTracerSrc is .true. the inflowing concentration is Tsrc.
!  When LtracerSrc is .false. we add tracer mass to compensate for the
!  added volume to keep the receiving cell concentration unchanged.
!  J. Levin (Jupiter Intelligence Inc.) and J. Wilkin
!
!    Dsrc(is) = 2,  flow across grid cell w-face (positive or negative)
!
      IF (LwSrc(ng)) THEN
        DO itrc=1,NT(ng)
          IF (.not.((Hadvection(itrc,ng)%MPDATA).and.                   &
     &              (Vadvection(itrc,ng)%MPDATA))) THEN
            DO is=1,Nsrc(ng)
             IF (INT(SOURCES(ng)%Dsrc(is)).eq.2) THEN
                Isrc=SOURCES(ng)%Isrc(is)
                Jsrc=SOURCES(ng)%Jsrc(is)
                IF (((Istr.le.Isrc).and.(Isrc.le.Iend+1)).and.          &
     &              ((Jstr.le.Jsrc).and.(Jsrc.le.Jend+1))) THEN
                  DO k=1,N(ng)
                    cff=dt(ng)*pm(Isrc,Jsrc)*pn(Isrc,Jsrc)
# ifdef SPLINES_VDIFF
                    cff=cff*oHz(Isrc,Jsrc,k)
# endif
                    IF (LtracerSrc(itrc,ng)) THEN
                      cff3=SOURCES(ng)%Tsrc(is,k,itrc)
                    ELSE
                      cff3=t(Isrc,Jsrc,k,3,itrc)
                    END IF
                    t(Isrc,Jsrc,k,nnew,itrc)=t(Isrc,Jsrc,k,nnew,itrc)+  &
     &                                       cff*SOURCES(ng)%Qsrc(is,k)*&
     &                                       cff3
                  END DO
                END IF
              END IF
            END DO
          END IF
        END DO
      END IF

# if defined SEDIMENT && defined SED_MORPH
!
!  If not MPDATA, add tracer mass to compensate for the added volume to
!  keep the receiving cell concentration unchanged.
!  Use 1/N to distribute the bed change in each water column layer.
!
      cff1=1.0_r8/N(ng)
      DO itrc=1,NT(ng)
        IF (.not.((Hadvection(itrc,ng)%MPDATA).and.                     &
     &            (Vadvection(itrc,ng)%MPDATA))) THEN
          DO j=Jstr,Jend
            DO k=1,N(ng)
              DO i=Istr,Iend
                cff3=t(i,j,k,3,itrc)
#  ifdef SPLINES_VDIFF
                cff3=cff3*oHz(i,j,k)
#  endif
                cff=cff1*(bed_thick(i,j,nstp)-bed_thick(i,j,3))
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-                  &
     &                             cff*cff3
              END DO
            END DO
          END DO
        END IF
      END DO
# endif

# ifdef OMEGA_IMPLICIT
!
!-----------------------------------------------------------------------
!  Add adaptive, Courant-number based implicit vertical advection term.
!-----------------------------------------------------------------------
!
!  Compute off-diagonal coefficients FC [dt*Wi*pm*pn] for the implicit
!  vertical advection term located at horizontal RHO-points and vertical
!  W-points. Also, set FC at the top and bottom levels.
!
!  It needs to be before the Diffusion.
!
      DO j=Jstr,Jend
        DO itrc=1,NT(ng)
          IF (.not.Vadvection(itrc,ng)%MPDATA) THEN
            cff=dt(ng)
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff1=cff*pm(i,j)*pn(i,j)
                FCmax(i,k)=MAX(Wi(i,j,k),0.0_r8)*cff1
                FCmin(i,k)=MIN(Wi(i,j,k),0.0_r8)*cff1
              END DO
            END DO
            DO i=Istr,Iend
              FCmax(i,0)=0.0_r8
              FCmin(i,0)=0.0_r8
              FCmax(i,N(ng))=0.0_r8
              FCmin(i,N(ng))=0.0_r8
            END DO
!
!  Compute diagonal matrix coefficients BC and load right-hand-side
!  terms for the tracer equation into DC.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                BC(i,k)=Hz(i,j,k)+FCmax(i,k)-FCmin(i,k-1)
#  ifdef SPLINES_VDIFF
                DC(i,k)=t(i,j,k,nnew,itrc)*Hz(i,j,k)
#  else
                DC(i,k)=t(i,j,k,nnew,itrc)
#  endif
              END DO
            END DO
!
!  Solve the tridiagonal system.
!
            DO i=Istr,Iend
              cff=1.0_r8/BC(i,1)
              CF(i,1)=cff*FCmin(i,1)
              DC(i,1)=cff*DC(i,1)
            END DO
            DO k=2,N(ng)-1
              DO i=Istr,Iend
                cff=1.0_r8/(BC(i,k)+FCmax(i,k-1)*CF(i,k-1))
                CF(i,k)=cff*FCmin(i,k)
                DC(i,k)=cff*(DC(i,k)+FCmax(i,k-1)*DC(i,k-1))
              END DO
            END DO
!
!  Compute new solution by back substitution.
!
            DO i=Istr,Iend
#  ifdef DIAGNOSTICS_TS
              cff1=t(i,j,N(ng),nnew,itrc)*oHz(i,j,N(ng))
#  endif
              cff=1.0_r8/(BC(i,N(ng))+                                  &
     &                    FCmax(i,N(ng)-1)*CF(i,N(ng)-1))
              DC(i,N(ng))=cff*(DC(i,N(ng))+                             &
     &                         FCmax(i,N(ng)-1)*DC(i,N(ng)-1))
#  ifdef SPLINES_VDIFF
              t(i,j,N(ng),nnew,itrc)=DC(i,N(ng))
#  else
              t(i,j,N(ng),nnew,itrc)=DC(i,N(ng))*Hz(i,j,N(ng))
#  endif
#  ifdef DIAGNOSTICS_TS
              DiaTwrk(i,j,N(ng),itrc,iTvadv)=DiaTwrk(i,j,N(ng),itrc,    &
     &                                               iTvadv)+           &
     &                                       DC(i,N(ng))-cff1
#  endif
            END DO
!
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
#  ifdef DIAGNOSTICS_TS
                cff1=t(i,j,k,nnew,itrc)*oHz(i,j,k)
#  endif
                DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
#  ifdef SPLINES_VDIFF
                t(i,j,k,nnew,itrc)=DC(i,k)
#  else
                t(i,j,k,nnew,itrc)=DC(i,k)*Hz(i,j,k)
#  endif
#  ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvadv)=DiaTwrk(i,j,k,itrc,iTvadv)+  &
     &                                     DC(i,k)-cff1
#  endif
              END DO
            END DO
          END IF
        END DO
      END DO
# endif
!
!-----------------------------------------------------------------------
!  Time-step vertical diffusion term.
!-----------------------------------------------------------------------
!
      J_LOOP2 : DO j=Jstr,Jend                  ! start pipelined J-loop
        DO itrc=1,NT(ng)
          ltrc=MIN(NAT,itrc)

# ifdef SPLINES_VDIFF
          IF (.not.((Hadvection(itrc,ng)%MPDATA).and.                   &
     &              (Vadvection(itrc,ng)%MPDATA))) THEN
!
!  Use conservative, parabolic spline reconstruction of vertical
!  diffusion derivatives.  Then, time step vertical diffusion term
!  implicitly.
!
            cff1=1.0_r8/6.0_r8
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                FC(i,k)=cff1*Hz(i,j,k  )-                               &
     &                  dt(ng)*Akt(i,j,k-1,ltrc)*oHz(i,j,k  )
                CF(i,k)=cff1*Hz(i,j,k+1)-                               &
     &                  dt(ng)*Akt(i,j,k+1,ltrc)*oHz(i,j,k+1)
              END DO
            END DO
            DO i=Istr,Iend
              CF(i,0)=0.0_r8
              DC(i,0)=0.0_r8
            END DO
!
!  LU decomposition and forward substitution.
!
            cff1=1.0_r8/3.0_r8
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                BC(i,k)=cff1*(Hz(i,j,k)+Hz(i,j,k+1))+                   &
     &                  dt(ng)*Akt(i,j,k,ltrc)*(oHz(i,j,k)+oHz(i,j,k+1))
                cff=1.0_r8/(BC(i,k)-FC(i,k)*CF(i,k-1))
                CF(i,k)=cff*CF(i,k)
                DC(i,k)=cff*(t(i,j,k+1,nnew,itrc)-t(i,j,k,nnew,itrc)-   &
     &                       FC(i,k)*DC(i,k-1))
              END DO
            END DO
!
!  Backward substitution.
!
            DO i=Istr,Iend
              DC(i,N(ng))=0.0_r8
            END DO
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
                DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
              END DO
            END DO
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                DC(i,k)=DC(i,k)*Akt(i,j,k,ltrc)
                cff1=dt(ng)*oHz(i,j,k)*(DC(i,k)-DC(i,k-1))
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+cff1
#  ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvdif)=DiaTwrk(i,j,k,itrc,iTvdif)+  &
     &                                     cff1
#  endif
              END DO
            END DO
          ELSE
# endif
!
!  Compute off-diagonal coefficients FC [lambda*dt*Akt/Hz] for the
!  implicit vertical diffusion terms at future time step, located
!  at horizontal RHO-points and vertical W-points.
!  Also set FC at the top and bottom levels.
!
            cff=-dt(ng)*lambda
            DO k=1,N(ng)-1
              DO i=Istr,Iend
                cff1=1.0_r8/(z_r(i,j,k+1)-z_r(i,j,k))
                FC(i,k)=cff*cff1*Akt(i,j,k,ltrc)
              END DO
            END DO
            DO i=Istr,Iend
              FC(i,0)=0.0_r8
              FC(i,N(ng))=0.0_r8
            END DO
!
!  Compute diagonal matrix coefficients BC and load right-hand-side
!  terms for the tracer equation into DC.
!
            DO k=1,N(ng)
              DO i=Istr,Iend
                BC(i,k)=Hz(i,j,k)-FC(i,k)-FC(i,k-1)
                DC(i,k)=t(i,j,k,nnew,itrc)
              END DO
            END DO
!
!  Solve the tridiagonal system.
!
            DO i=Istr,Iend
              cff=1.0_r8/BC(i,1)
              CF(i,1)=cff*FC(i,1)
              DC(i,1)=cff*DC(i,1)
            END DO
            DO k=2,N(ng)-1
              DO i=Istr,Iend
                cff=1.0_r8/(BC(i,k)-FC(i,k-1)*CF(i,k-1))
                CF(i,k)=cff*FC(i,k)
                DC(i,k)=cff*(DC(i,k)-FC(i,k-1)*DC(i,k-1))
              END DO
            END DO
!
!  Compute new solution by back substitution.
!
            DO i=Istr,Iend
# ifdef DIAGNOSTICS_TS
               cff1=t(i,j,N(ng),nnew,itrc)*oHz(i,j,N(ng))
# endif
               DC(i,N(ng))=(DC(i,N(ng))-FC(i,N(ng)-1)*DC(i,N(ng)-1))/   &
     &                     (BC(i,N(ng))-FC(i,N(ng)-1)*CF(i,N(ng)-1))
               t(i,j,N(ng),nnew,itrc)=DC(i,N(ng))
# ifdef DIAGNOSTICS_TS
               DiaTwrk(i,j,N(ng),itrc,iTvdif)=                          &
     &                              DiaTwrk(i,j,N(ng),itrc,iTvdif)+     &
     &                              t(i,j,N(ng),nnew,itrc)-cff1
# endif
            END DO
            DO k=N(ng)-1,1,-1
              DO i=Istr,Iend
# ifdef DIAGNOSTICS_TS
                cff1=t(i,j,k,nnew,itrc)*oHz(i,j,k)
# endif
                DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
                t(i,j,k,nnew,itrc)=DC(i,k)
# ifdef DIAGNOSTICS_TS
                DiaTwrk(i,j,k,itrc,iTvdif)=DiaTwrk(i,j,k,itrc,iTvdif)+  &
     &                                     t(i,j,k,nnew,itrc)-cff1
# endif
              END DO
            END DO
# ifdef SPLINES_VDIFF
          END IF
# endif
        END DO
      END DO J_LOOP2

# if defined AGE_MEAN && defined T_PASSIVE
!
!-----------------------------------------------------------------------
!  If inert passive tracer and Mean Age, compute age concentration (even
!  inert index) forced by the right-hand-side term that is concentration
!  of an associated conservative passive tracer (odd inert index). Mean
!  Age is age concentration divided by conservative passive tracer
!  concentration. Code implements NPT/2 mean age tracer pairs.
!
!  Implemented and tested by W.G. Zhang and J. Wilkin. See following
!  reference for details.
!
!   Zhang et al. (2010): Simulation of water age and residence time in
!      the New York Bight, JPO, 40,965-982, doi:10.1175/2009JPO4249.1
!-----------------------------------------------------------------------
!
      DO itrc=1,NPT,2
        iage=inert(itrc+1)                     ! even inert tracer index
        DO k=1,N(ng)
          DO j=Jstr,Jend
            DO i=Istr,Iend
              IF ((Hadvection(inert(itrc),ng)%MPDATA).and.              &
     &            (Vadvection(inert(itrc),ng)%MPDATA)) THEN
                t(i,j,k,nnew,iage)=t(i,j,k,nnew,iage)+                  &
     &                             dt(ng)*t(i,j,k,nnew,inert(itrc))
              ELSE
                t(i,j,k,nnew,iage)=t(i,j,k,nnew,iage)+                  &
     &                             dt(ng)*t(i,j,k,3,inert(itrc))
              END IF
            END DO
          END DO
        END DO
      END DO
# endif
!
!-----------------------------------------------------------------------
!  Apply lateral boundary conditions and, if appropriate, nudge
!  to tracer data and apply Land/Sea mask.
!-----------------------------------------------------------------------
!
!  Initialize tracer counter index. The "tclm" array is only allocated
!  to the NTCLM fields that need to be processed. This is done to
!  reduce memory.
!
      ic=0
!
      DO itrc=1,NT(ng)
!
!  Set compact reduced memory tracer index for nudging coefficients and
!  climatology arrays.
!
        IF (LtracerCLM(itrc,ng).and.LnudgeTCLM(itrc,ng)) THEN
          ic=ic+1
        END IF
!
!  Set lateral boundary conditions.
!
        CALL t3dbc_tile (ng, tile, itrc, ic,                            &
     &                   LBi, UBi, LBj, UBj, N(ng), NT(ng),             &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   nstp, nnew,                                    &
     &                   t)
!
!  Nudge towards tracer climatology.
!
        IF (LtracerCLM(itrc,ng).and.LnudgeTCLM(itrc,ng)) THEN
          DO k=1,N(ng)
            DO j=JstrR,JendR
              DO i=IstrR,IendR
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+                  &
     &                             dt(ng)*                              &
     &                             CLIMA(ng)%Tnudgcof(i,j,k,ic)*        &
     &                             (CLIMA(ng)%tclm(i,j,k,ic)-           &
     &                              t(i,j,k,nnew,itrc))
              END DO
            END DO
          END DO
        END IF

# ifdef MASKING
!
!  Apply Land/Sea mask.
!
        DO k=1,N(ng)
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)*rmask(i,j)
            END DO
          END DO
        END DO
# endif
# ifdef DIAGNOSTICS_TS
!
!  Compute time-rate-of-change diagnostic term.
!
        DO k=1,N(ng)
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              DiaTwrk(i,j,k,itrc,iTrate)=t(i,j,k,nnew,itrc)-            &
     &                                   DiaTwrk(i,j,k,itrc,iTrate)
!!            DiaTwrk(i,j,k,itrc,iTrate)=t(i,j,k,nnew,itrc)-            &
!!   &                                   t(i,j,k,nstp,itrc)
            END DO
          END DO
        END DO
# endif
!
!  Apply periodic boundary conditions.
!
        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_r3d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj, 1, N(ng),         &
     &                            t(:,:,:,nnew,itrc))
        END IF
      END DO
# ifdef DISTRIBUTE
!
!  Exchange boundary data.
!
      CALL mp_exchange4d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj, 1, N(ng), 1, NT(ng),      &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    t(:,:,:,nnew,:))
# endif
# if defined FLOATS && defined FLOAT_VWALK
!
!-----------------------------------------------------------------------
!  Compute vertical gradient in vertical T-diffusion coefficient for
!  floats random walk.
!-----------------------------------------------------------------------
!
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          DO k=1,N(ng)
            dAktdz(i,j,k)=(Akt(i,j,k,1)-Akt(i,j,k-1,1))/Hz(i,j,k)
          END DO
        END DO
      END DO
!
!  Apply periodic boundary conditions.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          dAktdz)
      END IF

#  ifdef DISTRIBUTE
      CALL mp_exchange3d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    dAktdz)
#  endif
# endif
!
!-----------------------------------------------------------------------
!  If applicable, deallocate local arrays.
!-----------------------------------------------------------------------
!
      IF (Lmpdata) THEN
# ifdef DIAGNOSTICS_TS
        IF (allocated(Dhadv)) deallocate (Dhadv)
        IF (allocated(Dvadv)) deallocate (Dvadv)
# endif
        IF (allocated(Ta))    deallocate (Ta)
        IF (allocated(Ua))    deallocate (Ua)
        IF (allocated(Va))    deallocate (Va)
        IF (allocated(Wa))    deallocate (Wa)
      END IF
!
      RETURN
      END SUBROUTINE step3d_t_tile
#endif
      END MODULE step3d_t_mod
```

</details>

<a id="appendix-cpp"></a>

## 12. 附录 B：全部当前 C++ helper、前后文及重组地图

### 12.1 kokkos_step3d.cpp 无缺行重组

按下表的**文件行号顺序**连接这些 source-block，结果逐字等于当前完整文件；不是按文档出现顺序直接拼接。仅共用/前后文放在此附录，正文函数不再整份重复。

| 闭区间 | source-block ID | 内容 |
|---|---|---|
| 1–82 | `cpp-step3d-common` | 原说明、include、RangePolicy、KB、thread_local workspace、PRE3D 编译门禁、kEps |
| 83–297 | `cpp-pre-t-fields` | 完整 TracerFields 和全部水平/垂向 helper |
| 298–403 | `cpp-pre-uv-fields` | 完整 MomentumFields::row/both、namespace 和 extern C 起始 |
| 404–552 | `current-pre-t` | 正文完整 tracer 预测入口 |
| 553–614 | `current-pre-uv` | 正文完整 U/V 预测入口及原空行 |
| 615–673 | `cpp-uv-fields-prefix` | 前阶段收尾、STEP3DUV 门禁、完整 UvDir、UvFields 公用字段及 solve_scratch |
| 674–921 | `cpp-uv-solve-row` | 完整 solve_row，含两种隐式黏性分支、诊断与内部均值替换 |
| 922–1013 | `cpp-uv-couple-row` | 完整 couple_scratch/couple_row、类型/namespace 收尾和 extern C 起始 |
| 1014–1075 | `current-uv-solve` | 正文完整 solve 入口 |
| 1076–1140 | `current-uv-couple` | 正文完整 couple 入口及原空行 |
| 1141–1144 | `cpp-step3d-tail` | extern C / CPP 收尾 |

`UvFields` 的类体由 prefix、solve-row、couple-row 三块接续，不能将任意单块误当独立可编译文件。所有 helper 模板和 U/V 选择均保留原定义。

<a id="src-cpp-step3d-common"></a>

**source-block ID：`cpp-step3d-common`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.cpp`；闭区间：**1–82**；行数：**82**；UTF-8 payload SHA256：`ac9cf421a7e3c587573b9ab86d1d3f718033115dacca14a5ec8b9290ccc3d051`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.cpp，1–82（完整原文，可展开）</summary>

<!-- source-block id=cpp-step3d-common commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.cpp lines=1-82 count=82 sha256=ac9cf421a7e3c587573b9ab86d1d3f718033115dacca14a5ec8b9290ccc3d051 -->
```cpp
/*
** Kokkos ports of the compute loops of ROMS/Nonlinear/pre_step3d.F and
** ROMS/Nonlinear/step3d_uv.F.
**
** Work decomposition
** ------------------
** Every launch gives a work item one grid row j (the tracer kernel one
** (row, tracer) pair) and replays the Fortran loops of that row with i
** innermost, in chunks of KB columns.  The i-innermost order matters on
** the CPEs: an earlier column-at-a-time version of the momentum
** predictor touched every level of ~25 arrays per column and ran 4.8x
** slower.
**
** pre_step3d tracers, per chunk: (1) horizontal advection into the
** predictor t(:,:,:,3,itrc), (2) vertical advection with the artificial
** continuity equation, (3) the explicit vertical diffusion into
** t(:,:,:,nnew,itrc) and DiaTwrk.  Doing the three Fortran passes chunk
** by chunk is safe: pass 1 reads the old t(nnew) only at its own point,
** pass 3 overwrites it only there, and every other read is of t(nstp),
** which nothing writes.
**
** pre_step3d momentum and both step3d_uv row loops are column local; the
** step3d_uv coupling loop runs after u3dbc/v3dbc, which stay in Fortran.
**
** Expressions keep the Fortran operand order and grouping, so a serial
** backend reproduces the Fortran result bit for bit.
*/

#include "kokkos_step3d.hpp"

#if defined ROMS_KOKKOS && \
    (defined KK_STAGE_PRE3D || defined KK_STAGE_STEP3DUV)

#include <cmath>
#ifndef __sw_slave__
#include "kokkos_workspace.hpp"
#endif

namespace {

using roms::Array2d;
using roms::Array3d;
using roms::Array4d;
using roms::Array5d;
using exec_space = roms::exec_space;
using range_policy = Kokkos::RangePolicy<exec_space, Kokkos::IndexType<int>>;

/* Columns per chunk.  Within a chunk every Fortran loop is replayed with
** i innermost, so each array row is streamed contiguously (the order the
** Fortran uses and the one the CPE cache needs; a column-at-a-time sweep
** touches every level of every array before moving on and was 4.8x
** slower for the momentum predictor).  Chunking only bounds the scratch;
** columns are independent, so the results do not depend on KB. */
constexpr int KB = 32;

#ifndef __sw_slave__
/* Per-host-thread scratch, raw ownership (no Kokkos at teardown).  Every
** launch joins before returning, so it can be reused on the next call. */
thread_local roms::HostWorkspace workspace;
#endif

} // namespace

#endif // ROMS_KOKKOS && (KK_STAGE_PRE3D || KK_STAGE_STEP3DUV)

/*======================================================================
** pre_step3d.F
**====================================================================*/

#if defined ROMS_KOKKOS && defined KK_STAGE_PRE3D

#if defined MASKING || defined WET_DRY || defined WEC_VF ||              \
    defined SOLAR_SOURCE || defined LMD_NONLOCAL || defined BODYFORCE || \
    (defined AGE_MEAN && defined T_PASSIVE) || defined SINGLE_PRECISION
#error "Kokkos pre_step3d does not support this physics configuration"
#endif

namespace {

using roms::Pre3dConfig;

constexpr double kEps = 1.0E-16; /* eps in pre_step3d.F */
```

</details>

<a id="src-cpp-pre-t-fields"></a>

**source-block ID：`cpp-pre-t-fields`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.cpp`；闭区间：**83–297**；行数：**215**；UTF-8 payload SHA256：`8f7279c01e395002901bd6e9969a071278fea0efd9e5c662d50aa69cec2f1510`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.cpp，83–297（完整原文，可展开）</summary>

<!-- source-block id=cpp-pre-t-fields commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.cpp lines=83-297 count=215 sha256=8f7279c01e395002901bd6e9969a071278fea0efd9e5c662d50aa69cec2f1510 -->
```cpp

struct TracerFields {
    Pre3dConfig c;
    Array2d<double> pm, pn;
    Array3d<double> Hz, Huon, Hvom, zr, W, btflx, stflx;
    Array4d<double> Akt;
    Array5d<double> t;
#if defined DIAGNOSTICS_TS
    Array5d<double> dia;
#endif

    /* FX(i) = t(i)-t(i-1) with the closed-edge folds FX(Istr-1)=FX(Istr),
    ** FX(Iend+2)=FX(Iend+1). */
    KOKKOS_INLINE_FUNCTION
    double dx(int i, int j, int k, int it) const {
        if (c.fold_w && i == c.Istr - 1)
            i = c.Istr;
        else if (c.fold_e && i == c.Iend + 2)
            i = c.Iend + 1;
        return t(i, j, k, c.nstp, it) - t(i - 1, j, k, c.nstp, it);
    }

    KOKKOS_INLINE_FUNCTION
    double de(int i, int j, int k, int it) const {
        if (c.fold_s && j == c.Jstr - 1)
            j = c.Jstr;
        else if (c.fold_n && j == c.Jend + 2)
            j = c.Jend + 1;
        return t(i, j, k, c.nstp, it) - t(i, j - 1, k, c.nstp, it);
    }

    /* curv / grad at a cell from the two differences around it. */
    KOKKOS_INLINE_FUNCTION
    static double cell_slope(double a, double b, int scheme) {
        if (scheme == roms::kUpstream3)
            return a - b;
        if (scheme == roms::kAkima4) {
            const double cff = 2.0 * a * b;
            if (cff > kEps)
                return cff / (a + b);
            return 0.0;
        }
        return 0.5 * (a + b);
    }

    /* Xi-face flux FX(i,j) at level k (face between i-1 and i). */
    KOKKOS_INLINE_FUNCTION
    double flux_x(int i, int j, int k, int it, int scheme) const {
        const double hu = Huon(i, j, k);
        const double tl = t(i - 1, j, k, c.nstp, it);
        const double tr = t(i, j, k, c.nstp, it);
        if (scheme == roms::kCentered2)
            return hu * 0.5 * (tl + tr);
        if (scheme == roms::kUpstream1) {
            const double cff1 = Kokkos::max(hu, 0.0);
            const double cff2 = Kokkos::min(hu, 0.0);
            return cff1 * tl + cff2 * tr;
        }
        const double dm = dx(i - 1, j, k, it), d0 = dx(i, j, k, it);
        const double dp = dx(i + 1, j, k, it);
        const double sm = cell_slope(d0, dm, scheme); /* cell i-1 */
        const double s0 = cell_slope(dp, d0, scheme); /* cell i   */
        if (scheme == roms::kUpstream3) {
            return hu * 0.5 * (tl + tr) -
                   (1.0 / 6.0) * (sm * Kokkos::max(hu, 0.0) + s0 * Kokkos::min(hu, 0.0));
        }
        return hu * 0.5 * (tl + tr - (1.0 / 3.0) * (s0 - sm));
    }

    /* Eta-face flux FE(i,j) at level k (face between j-1 and j). */
    KOKKOS_INLINE_FUNCTION
    double flux_e(int i, int j, int k, int it, int scheme) const {
        const double hv = Hvom(i, j, k);
        const double tl = t(i, j - 1, k, c.nstp, it);
        const double tr = t(i, j, k, c.nstp, it);
        if (scheme == roms::kCentered2)
            return hv * 0.5 * (tl + tr);
        if (scheme == roms::kUpstream1) {
            const double cff1 = Kokkos::max(hv, 0.0);
            const double cff2 = Kokkos::min(hv, 0.0);
            return cff1 * tl + cff2 * tr;
        }
        const double dm = de(i, j - 1, k, it), d0 = de(i, j, k, it);
        const double dp = de(i, j + 1, k, it);
        const double sm = cell_slope(d0, dm, scheme); /* cell j-1 */
        const double s0 = cell_slope(dp, d0, scheme); /* cell j   */
        if (scheme == roms::kUpstream3) {
            return hv * 0.5 * (tl + tr) -
                   (1.0 / 6.0) * (sm * Kokkos::max(hv, 0.0) + s0 * Kokkos::min(hv, 0.0));
        }
        return hv * 0.5 * (tl + tr - (1.0 / 3.0) * (s0 - sm));
    }

    /* Vertical advective flux FC(0:N) of the columns ib..ib+nb-1 of row j
    ** from t(nstp), FC[k*KB+b] = FC(ib+b,k), the Fortran loops with i
    ** innermost. */
    KOKKOS_INLINE_FUNCTION
    void vertical_flux(int ib, int nb, int j, int it, int scheme, double* FC,
                       double* CF) const {
        const int N = c.N, s = c.nstp;
#define I_LOOP for (int b = 0, i = ib; b < nb; ++b, ++i)
        if (scheme == roms::kSplines) {
            /* NEUMANN is always defined in pre_step3d.F. */
            I_LOOP {
                FC[b] = 1.5 * t(i, j, 1, s, it);
                CF[KB + b] = 0.5;
            }
            for (int k = 1; k <= N - 1; ++k) {
                I_LOOP {
                    const int p = k * KB + b;
                    const double cff =
                        1.0 / (2.0 * Hz(i, j, k) + Hz(i, j, k + 1) * (2.0 - CF[p]));
                    CF[p + KB] = cff * Hz(i, j, k);
                    FC[p] = cff * (3.0 * (Hz(i, j, k) * t(i, j, k + 1, s, it) +
                                          Hz(i, j, k + 1) * t(i, j, k, s, it)) -
                                   Hz(i, j, k + 1) * FC[p - KB]);
                }
            }
            I_LOOP {
                const int p = N * KB + b;
                FC[p] = (3.0 * t(i, j, N, s, it) - FC[p - KB]) / (2.0 - CF[p]);
            }
            for (int k = N - 1; k >= 0; --k) {
                I_LOOP {
                    const int p = k * KB + b;
                    FC[p] = FC[p] - CF[p + KB] * FC[p + KB];
                    FC[p + KB] = W(i, j, k + 1) * FC[p + KB];
                }
            }
            I_LOOP {
                FC[N * KB + b] = 0.0;
                FC[b] = 0.0;
            }
        } else if (scheme == roms::kAkima4) {
            for (int k = 1; k <= N - 1; ++k) {
                I_LOOP { FC[k * KB + b] = t(i, j, k + 1, s, it) - t(i, j, k, s, it); }
            }
            I_LOOP {
                FC[b] = FC[KB + b];
                FC[N * KB + b] = FC[(N - 1) * KB + b];
            }
            for (int k = 1; k <= N; ++k) {
                I_LOOP {
                    const int p = k * KB + b;
                    const double cff = 2.0 * FC[p] * FC[p - KB];
                    if (cff > kEps) {
                        CF[p] = cff / (FC[p] + FC[p - KB]);
                    } else {
                        CF[p] = 0.0;
                    }
                }
            }
            const double cff1 = 1.0 / 3.0;
            for (int k = 1; k <= N - 1; ++k) {
                I_LOOP {
                    const int p = k * KB + b;
                    FC[p] = W(i, j, k) * 0.5 *
                            (t(i, j, k, s, it) + t(i, j, k + 1, s, it) -
                             cff1 * (CF[p + KB] - CF[p]));
                }
            }
            I_LOOP {
                FC[b] = 0.0;
                FC[N * KB + b] = 0.0;
            }
        } else if (scheme == roms::kCentered2) {
            for (int k = 1; k <= N - 1; ++k) {
                I_LOOP {
                    FC[k * KB + b] =
                        W(i, j, k) * 0.5 * (t(i, j, k, s, it) + t(i, j, k + 1, s, it));
                }
            }
            I_LOOP {
                FC[b] = 0.0;
                FC[N * KB + b] = 0.0;
            }
        } else if (scheme == roms::kUpstream1) {
            for (int k = 1; k <= N - 1; ++k) {
                I_LOOP {
                    const double cff1 = Kokkos::max(W(i, j, k), 0.0);
                    const double cff2 = Kokkos::min(W(i, j, k), 0.0);
                    FC[k * KB + b] = cff1 * t(i, j, k, s, it) + cff2 * t(i, j, k + 1, s, it);
                }
            }
            I_LOOP {
                FC[b] = 0.0;
                FC[N * KB + b] = 0.0;
            }
        } else { /* CENTERED4 or SPLIT_U3 */
            const double cff1 = 0.5;
            const double cff2 = 7.0 / 12.0;
            const double cff3 = 1.0 / 12.0;
            for (int k = 2; k <= N - 2; ++k) {
                I_LOOP {
                    FC[k * KB + b] =
                        W(i, j, k) *
                        (cff2 * (t(i, j, k, s, it) + t(i, j, k + 1, s, it)) -
                         cff3 * (t(i, j, k - 1, s, it) + t(i, j, k + 2, s, it)));
                }
            }
            I_LOOP {
                FC[b] = 0.0;
                FC[KB + b] = W(i, j, 1) * (cff1 * t(i, j, 1, s, it) +
                                           cff2 * t(i, j, 2, s, it) -
                                           cff3 * t(i, j, 3, s, it));
                FC[(N - 1) * KB + b] = W(i, j, N - 1) * (cff1 * t(i, j, N, s, it) +
                                                         cff2 * t(i, j, N - 1, s, it) -
                                                         cff3 * t(i, j, N - 2, s, it));
                FC[N * KB + b] = 0.0;
            }
        }
#undef I_LOOP
    }
};

```

</details>

<a id="src-cpp-pre-uv-fields"></a>

**source-block ID：`cpp-pre-uv-fields`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.cpp`；闭区间：**298–403**；行数：**106**；UTF-8 payload SHA256：`9f5b7dd5f16ee1b7f7698c3f4973817f4d859cee992a9300b4ce34bfd9600eab`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.cpp，298–403（完整原文，可展开）</summary>

<!-- source-block id=cpp-pre-uv-fields commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.cpp lines=298-403 count=106 sha256=9f5b7dd5f16ee1b7f7698c3f4973817f4d859cee992a9300b4ce34bfd9600eab -->
```cpp
struct MomentumFields {
    Pre3dConfig c;
    Array2d<double> pm, pn, bustr, bvstr, sustr, svstr;
    Array3d<double> Hz, zr, Akv;
    Array4d<double> ru, rv, u, v;
#if defined DIAGNOSTICS_UV
    Array4d<double> DiaU3wrk, DiaV3wrk;
    Array5d<double> DiaRU, DiaRV;
#endif

    /* One row of the U (V=false) or V (V=true) predictor, swept level by
    ** level with i innermost, the Fortran loop order.  FC(k-1) of every
    ** column is carried in fcm[] and DC(i,0) in dc0[], both indexed i-i0.
    ** The values are the Fortran expressions; only the storage differs. */
    template <int STEP, bool V>
    KOKKOS_INLINE_FUNCTION void row(int j, double* fcm, double* dc0) const {
        const int N = c.N, s = c.nstp, nn = c.nnew, nr = c.nrhs;
        const int indx = 3 - nr;
        const int i0 = V ? c.Istr : c.IstrU, i1 = c.Iend;
        const int di = V ? 0 : 1, dj = V ? 1 : 0;
        const Array4d<double> &vel = V ? v : u;
        const Array4d<double> &rhs = V ? rv : ru;
        const Array2d<double> &bstr = V ? bvstr : bustr;
        const Array2d<double> &sstr = V ? svstr : sustr;
#if defined DIAGNOSTICS_UV
        const Array4d<double> &d3 = V ? DiaV3wrk : DiaU3wrk;
        const Array5d<double> &dr = V ? DiaRV : DiaRU;
#endif
        const double cffd = c.dt * 0.25;
        const double cfl = c.dt * (1.0 - c.lambda);
        for (int i = i0; i <= i1; ++i) {
            dc0[i - i0] = cffd * (pm(i, j) + pm(i - di, j - dj)) *
                          (pn(i, j) + pn(i - di, j - dj));
            fcm[i - i0] = c.dt * bstr(i, j);
        }
        for (int k = 1; k <= N; ++k) {
            for (int i = i0; i <= i1; ++i) {
                double fck;
                if (k < N) {
                    const double cff =
                        1.0 / (zr(i, j, k + 1) + zr(i - di, j - dj, k + 1) -
                               zr(i, j, k) - zr(i - di, j - dj, k));
                    fck = cfl * cff * (vel(i, j, k + 1, s) - vel(i, j, k, s)) *
                          (Akv(i, j, k) + Akv(i - di, j - dj, k));
                } else {
                    fck = c.dt * sstr(i, j);
                }
                const double DC0 = dc0[i - i0];
                if (STEP == 0) {
                    const double cff1 =
                        vel(i, j, k, s) * 0.5 * (Hz(i, j, k) + Hz(i - di, j - dj, k));
                    const double cff2 = fck - fcm[i - i0];
                    vel(i, j, k, nn) = cff1 + cff2;
#if defined DIAGNOSTICS_UV
                    for (int id = 1; id <= c.M3pgrd; ++id)
                        d3(i, j, k, id) = 0.0;
                    d3(i, j, k, c.M3vvis) = cff2;
                    d3(i, j, k, c.M3rate) = cff1;
#endif
                } else if (STEP == 1) {
                    const double cff1 =
                        vel(i, j, k, s) * 0.5 * (Hz(i, j, k) + Hz(i - di, j - dj, k));
                    const double cff2 = fck - fcm[i - i0];
                    const double cff3 = 0.5 * DC0;
                    vel(i, j, k, nn) = cff1 - cff3 * rhs(i, j, k, indx) + cff2;
#if defined DIAGNOSTICS_UV
                    for (int id = 1; id <= c.M3pgrd; ++id)
                        d3(i, j, k, id) = -cff3 * dr(i, j, k, indx, id);
                    d3(i, j, k, c.M3vvis) = cff2;
                    d3(i, j, k, c.M3rate) = cff1;
#endif
                } else {
                    const double cff1 = 5.0 / 12.0;
                    const double cff2 = 16.0 / 12.0;
                    const double cff3 =
                        vel(i, j, k, s) * 0.5 * (Hz(i, j, k) + Hz(i - di, j - dj, k));
                    const double cff4 = fck - fcm[i - i0];
                    vel(i, j, k, nn) = cff3 +
                                       DC0 * (cff1 * rhs(i, j, k, nr) -
                                              cff2 * rhs(i, j, k, indx)) +
                                       cff4;
#if defined DIAGNOSTICS_UV
                    for (int id = 1; id <= c.M3pgrd; ++id)
                        d3(i, j, k, id) = DC0 * (cff1 * dr(i, j, k, nr, id) -
                                                 cff2 * dr(i, j, k, indx, id));
                    d3(i, j, k, c.M3vvis) = cff4;
                    d3(i, j, k, c.M3rate) = cff3;
#endif
                }
                fcm[i - i0] = fck;
            }
        }
    }

    template <int STEP>
    KOKKOS_INLINE_FUNCTION void both(int j, double* fcm, double* dc0) const {
        row<STEP, false>(j, fcm, dc0);
        if (j >= c.JstrV)
            row<STEP, true>(j, fcm, dc0);
    }
};

} // namespace

extern "C" {

```

</details>

<a id="src-cpp-uv-fields-prefix"></a>

**source-block ID：`cpp-uv-fields-prefix`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.cpp`；闭区间：**615–673**；行数：**59**；UTF-8 payload SHA256：`a90fb38e9733541e419266102260a46fdef783d6d7323d53ddaefce4e147f9e4`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.cpp，615–673（完整原文，可展开）</summary>

<!-- source-block id=cpp-uv-fields-prefix commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.cpp lines=615-673 count=59 sha256=a90fb38e9733541e419266102260a46fdef783d6d7323d53ddaefce4e147f9e4 -->
```cpp

} // extern "C"

#endif // ROMS_KOKKOS && KK_STAGE_PRE3D

/*======================================================================
** step3d_uv.F
**
** k_step3d_uv_solve covers the first row loop of step3d_uv_tile (rows
** Jstr..Jend): the right-hand-side step, the implicit vertical
** viscosity and the interior vertical-mean replacement, for U then V.
** k_step3d_uv_couple covers the second row loop (rows JstrT..JendT),
** which runs after u3dbc/v3dbc and the point sources.  Both are column
** local: a column reads its own levels plus read-only neighbours (Hz,
** Akv, z_r, pm, pn of i-1 or j-1), so one work item per row is free of
** races.  U and V share one code path; the neighbour offset (di,dj) and
** the arrays select the direction, and every expression keeps the
** Fortran operand order for both.
**====================================================================*/

#if defined ROMS_KOKKOS && defined KK_STAGE_STEP3DUV

#if defined MASKING || defined WET_DRY || defined WEC || defined WEC_VF || \
    defined OMEGA_IMPLICIT || defined BODYFORCE ||                         \
    defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3 ||               \
    defined SINGLE_PRECISION
#error "Kokkos step3d_uv does not support this physics configuration"
#endif

namespace {

using roms::Uv3dConfig;

struct UvDir {
    int di = 0, dj = 0;         /* neighbour offset: U (1,0), V (0,1) */
    Array2d<double> metric;     /* on_u or om_v */
    Array2d<double> avg1, avg2; /* DU_avg1/2 or DV_avg1/2 */
    Array4d<double> rhs;        /* ru or rv, (.., 0:N, 2) */
    Array4d<double> vel;        /* u or v, (.., N, 2) */
    Array3d<double> bar;        /* ubar or vbar, (.., 3) */
    Array3d<double> flux;       /* Huon or Hvom */
#if defined DIAGNOSTICS_UV
    Array3d<double> d2wrk, d2int; /* DiaU2wrk/DiaU2int, (.., NDM2d) */
    Array4d<double> d3wrk;        /* DiaU3wrk, (.., N, NDM3d) */
    Array5d<double> dRU;          /* DiaRU, (.., N, 2, NDrhs) */
#endif
};

struct UvFields {
    Uv3dConfig c;
    Array2d<double> pm, pn;
    Array3d<double> Hz, zr, Akv;
    UvDir d[2];

    KOKKOS_INLINE_FUNCTION
    static int solve_scratch(int N, int NDM2d) {
        return (8 * (N + 1) + NDM2d + 2) * KB;
    }

```

</details>

<a id="src-cpp-uv-solve-row"></a>

**source-block ID：`cpp-uv-solve-row`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.cpp`；闭区间：**674–921**；行数：**248**；UTF-8 payload SHA256：`c6329a2d49e64df9cc1d71ec5d956f1f2d4462a48dee59ca00dd945d124e181b`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.cpp，674–921（完整原文，可展开）</summary>

<!-- source-block id=cpp-uv-solve-row commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.cpp lines=674-921 count=248 sha256=c6329a2d49e64df9cc1d71ec5d956f1f2d4462a48dee59ca00dd945d124e181b -->
```cpp
    /* First row loop of step3d_uv_tile for one direction, columns
    ** [i0, i1] of row j, in chunks of KB columns.  X[k*KB+b] holds the
    ** Fortran X(i,k) for i = ib+b. */
    KOKKOS_INLINE_FUNCTION
    void solve_row(const UvDir &d, int i0, int i1, int j, double* s) const {
        const int N = c.N, nn = c.nnew, nr = c.nrhs;
        double* AK = s;
        double* Hzk = AK + (N + 1) * KB;
        double* oHz = Hzk + (N + 1) * KB;
        double* FC = oHz + (N + 1) * KB;
        double* CF = FC + (N + 1) * KB;
        double* BC = CF + (N + 1) * KB;
        double* DC = BC + (N + 1) * KB;
        double* wrk = DC + (N + 1) * KB;
        double* cf1 = wrk + (N + 1) * KB;
#if defined DIAGNOSTICS_UV
        double* Dwrk = cf1 + KB; /* Dwrk[id*KB+b], id = 2-D term */
#endif
        (void)wrk;
        const int di = d.di, dj = d.dj;

        for (int ib = i0; ib <= i1; ib += KB) {
            const int nb = (i1 - ib + 1 < KB) ? (i1 - ib + 1) : KB;
#define I_LOOP for (int b = 0, i = ib; b < nb; ++b, ++i)

            I_LOOP { AK[b] = 0.5 * (Akv(i - di, j - dj, 0) + Akv(i, j, 0)); }
            for (int k = 1; k <= N; ++k) {
                I_LOOP {
                    AK[k * KB + b] = 0.5 * (Akv(i - di, j - dj, k) + Akv(i, j, k));
                    Hzk[k * KB + b] = 0.5 * (Hz(i - di, j - dj, k) + Hz(i, j, k));
                    oHz[k * KB + b] = 1.0 / Hzk[k * KB + b];
                }
            }

            /* Time step right-hand-side terms. */
            double cff;
            if (c.step == 0) {
                cff = 0.25 * c.dt;
            } else if (c.step == 1) {
                cff = 0.25 * c.dt * 3.0 / 2.0;
            } else {
                cff = 0.25 * c.dt * 23.0 / 12.0;
            }
            I_LOOP {
                DC[b] = cff * (pm(i, j) + pm(i - di, j - dj)) *
                        (pn(i, j) + pn(i - di, j - dj));
            }
            for (int k = 1; k <= N; ++k) {
                I_LOOP {
                    d.vel(i, j, k, nn) = d.vel(i, j, k, nn) + DC[b] * d.rhs(i, j, k, nr);
#if defined SPLINES_VVISC
                    d.vel(i, j, k, nn) = d.vel(i, j, k, nn) * oHz[k * KB + b];
#endif
                }
#if defined DIAGNOSTICS_UV
                for (int id = 1; id <= c.M3pgrd; ++id) {
                    I_LOOP {
                        d.d3wrk(i, j, k, id) =
                            (d.d3wrk(i, j, k, id) + DC[b] * d.dRU(i, j, k, nr, id)) *
                            oHz[k * KB + b];
                    }
                }
                I_LOOP {
                    const double o = oHz[k * KB + b];
#if defined UV_VIS2 || defined UV_VIS4
                    d.d3wrk(i, j, k, c.M3xvis) = d.d3wrk(i, j, k, c.M3xvis) * o;
                    d.d3wrk(i, j, k, c.M3yvis) = d.d3wrk(i, j, k, c.M3yvis) * o;
                    d.d3wrk(i, j, k, c.M3hvis) = d.d3wrk(i, j, k, c.M3hvis) * o;
#endif
                    d.d3wrk(i, j, k, c.M3vvis) = d.d3wrk(i, j, k, c.M3vvis) * o;
                    d.d3wrk(i, j, k, c.M3rate) = d.d3wrk(i, j, k, c.M3rate) * o;
                }
#endif
            }

#if defined SPLINES_VVISC
            /* Parabolic splines; implicit tridiagonal vertical viscosity. */
            {
                const double cff1 = 1.0 / 6.0;
                for (int k = 1; k <= N - 1; ++k) {
                    I_LOOP {
                        FC[k * KB + b] = cff1 * Hzk[k * KB + b] -
                                         c.dt * AK[(k - 1) * KB + b] * oHz[k * KB + b];
                        CF[k * KB + b] = cff1 * Hzk[(k + 1) * KB + b] -
                                         c.dt * AK[(k + 1) * KB + b] * oHz[(k + 1) * KB + b];
                    }
                }
            }
            I_LOOP {
                CF[b] = 0.0;
                DC[b] = 0.0;
            }
            {
                const double cff1 = 1.0 / 3.0;
                for (int k = 1; k <= N - 1; ++k) {
                    I_LOOP {
                        const int p = k * KB + b;
                        BC[p] = cff1 * (Hzk[p] + Hzk[p + KB]) +
                                c.dt * AK[p] * (oHz[p] + oHz[p + KB]);
                        const double cffk = 1.0 / (BC[p] - FC[p] * CF[p - KB]);
                        CF[p] = cffk * CF[p];
                        DC[p] = cffk * (d.vel(i, j, k + 1, nn) - d.vel(i, j, k, nn) -
                                        FC[p] * DC[p - KB]);
                    }
                }
            }
            I_LOOP { DC[N * KB + b] = 0.0; }
            for (int k = N - 1; k >= 1; --k) {
                I_LOOP {
                    const int p = k * KB + b;
                    DC[p] = DC[p] - CF[p] * DC[p + KB];
                }
            }
            for (int k = 1; k <= N; ++k) {
                I_LOOP {
                    const int p = k * KB + b;
                    DC[p] = DC[p] * AK[p];
                    const double cffk = c.dt * oHz[p] * (DC[p] - DC[p - KB]);
                    d.vel(i, j, k, nn) = d.vel(i, j, k, nn) + cffk;
#if defined DIAGNOSTICS_UV
                    d.d3wrk(i, j, k, c.M3vvis) = d.d3wrk(i, j, k, c.M3vvis) + cffk;
#endif
                }
            }
#else
            /* Implicit vertical viscosity, [lambda*dt*Akv/Hz] off-diagonals. */
            {
                const double cffl = -c.lambda * c.dt / 0.5;
                for (int k = 1; k <= N - 1; ++k) {
                    I_LOOP {
                        const double cff1 =
                            1.0 / (zr(i, j, k + 1) + zr(i - di, j - dj, k + 1) -
                                   zr(i, j, k) - zr(i - di, j - dj, k));
                        FC[k * KB + b] = cffl * cff1 * AK[k * KB + b];
                    }
                }
            }
            I_LOOP {
                FC[b] = 0.0;
                FC[N * KB + b] = 0.0;
            }
            for (int k = 1; k <= N; ++k) {
                I_LOOP {
                    const int p = k * KB + b;
                    DC[p] = d.vel(i, j, k, nn);
                    BC[p] = Hzk[p] - FC[p] - FC[p - KB];
                }
            }
            I_LOOP {
                const double cffk = 1.0 / BC[KB + b];
                CF[KB + b] = cffk * FC[KB + b];
                DC[KB + b] = cffk * DC[KB + b];
            }
            for (int k = 2; k <= N - 1; ++k) {
                I_LOOP {
                    const int p = k * KB + b;
                    const double cffk = 1.0 / (BC[p] - FC[p - KB] * CF[p - KB]);
                    CF[p] = cffk * FC[p];
                    DC[p] = cffk * (DC[p] - FC[p - KB] * DC[p - KB]);
                }
            }
            I_LOOP {
                const int p = N * KB + b;
#if defined DIAGNOSTICS_UV
                wrk[p] = d.vel(i, j, N, nn) * oHz[p];
#endif
                DC[p] = (DC[p] - FC[p - KB] * DC[p - KB]) /
                        (BC[p] - FC[p - KB] * CF[p - KB]);
                d.vel(i, j, N, nn) = DC[p];
#if defined DIAGNOSTICS_UV
                d.d3wrk(i, j, N, c.M3vvis) =
                    d.d3wrk(i, j, N, c.M3vvis) + d.vel(i, j, N, nn) - wrk[p];
#endif
            }
            for (int k = N - 1; k >= 1; --k) {
                I_LOOP {
                    const int p = k * KB + b;
#if defined DIAGNOSTICS_UV
                    wrk[p] = d.vel(i, j, k, nn) * oHz[p];
#endif
                    DC[p] = DC[p] - CF[p] * DC[p + KB];
                    d.vel(i, j, k, nn) = DC[p];
#if defined DIAGNOSTICS_UV
                    d.d3wrk(i, j, k, c.M3vvis) =
                        d.d3wrk(i, j, k, c.M3vvis) + d.vel(i, j, k, nn) - wrk[p];
#endif
                }
            }
#endif

            /* Replace the interior incorrect vertical mean with the barotropic
            ** component, ubar=DU_avg1/(D*on_u).  CF(:,0) and DC(:,0) are
            ** CF[b] and DC[b]. */
            I_LOOP {
                CF[b] = Hzk[KB + b];
                DC[b] = d.vel(i, j, 1, nn) * Hzk[KB + b];
            }
#if defined DIAGNOSTICS_UV
            for (int q = 0; q < c.npairs; ++q) {
                double* D = Dwrk + c.m2[q] * KB;
                I_LOOP { D[b] = d.d3wrk(i, j, 1, c.m3[q]) * Hzk[KB + b]; }
            }
#endif
            for (int k = 2; k <= N; ++k) {
                I_LOOP {
                    CF[b] = CF[b] + Hzk[k * KB + b];
                    DC[b] = DC[b] + d.vel(i, j, k, nn) * Hzk[k * KB + b];
                }
#if defined DIAGNOSTICS_UV
                for (int q = 0; q < c.npairs; ++q) {
                    double* D = Dwrk + c.m2[q] * KB;
                    I_LOOP { D[b] = D[b] + d.d3wrk(i, j, k, c.m3[q]) * Hzk[k * KB + b]; }
                }
#endif
            }
            I_LOOP {
                cf1[b] = 1.0 / (CF[b] * d.metric(i, j));
                DC[b] = (DC[b] * d.metric(i, j) - d.avg1(i, j)) * cf1[b];
            }
#if defined DIAGNOSTICS_UV
            for (int id = 1; id <= c.M2pgrd; ++id) {
                double* D = Dwrk + id * KB;
                I_LOOP { D[b] = (D[b] * d.metric(i, j) - d.d2wrk(i, j, id)) * cf1[b]; }
            }
            {
                double* D = Dwrk + c.M2bstr * KB;
                I_LOOP {
                    D[b] = (D[b] * d.metric(i, j) - d.d2wrk(i, j, c.M2bstr) -
                            d.d2wrk(i, j, c.M2sstr)) *
                           cf1[b];
                }
            }
#endif
            for (int k = 1; k <= N; ++k) {
                I_LOOP { d.vel(i, j, k, nn) = d.vel(i, j, k, nn) - DC[b]; }
#if defined DIAGNOSTICS_UV
                for (int q = 0; q < c.npairs; ++q) {
                    const double* D = Dwrk + c.m2[q] * KB;
                    I_LOOP {
                        d.d3wrk(i, j, k, c.m3[q]) = d.d3wrk(i, j, k, c.m3[q]) - D[b];
                    }
                }
#endif
            }
#undef I_LOOP
        }
    }

```

</details>

<a id="src-cpp-uv-couple-row"></a>

**source-block ID：`cpp-uv-couple-row`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.cpp`；闭区间：**922–1013**；行数：**92**；UTF-8 payload SHA256：`306dad65e115812c111b4b226a5de7638f96113b61b7cd4845308b835747d479`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.cpp，922–1013（完整原文，可展开）</summary>

<!-- source-block id=cpp-uv-couple-row commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.cpp lines=922-1013 count=92 sha256=306dad65e115812c111b4b226a5de7638f96113b61b7cd4845308b835747d479 -->
```cpp
    KOKKOS_INLINE_FUNCTION
    static int couple_scratch(int N) { return (N + 5) * KB; }

    /* Second row loop for one direction.  U uses west point i=Istr and
    ** south row j=0, V uses i=Istr-1 and j=1: the first u/v boundary
    ** points.  Order of the boundary replacements: west, east, south,
    ** north, as in the Fortran. */
    KOKKOS_INLINE_FUNCTION
    void couple_row(const UvDir &d, int i0, int i1, int j, int west_i,
                    int south_j, int ilo, double* s) const {
        const int N = c.N, nn = c.nnew;
        const int di = d.di, dj = d.dj;
        double* DCk = s; /* DC(i,k), k = 1..N */
        double* DC0 = DCk + (N + 1) * KB;
        double* CF0 = DC0 + KB;
        double* FC0 = CF0 + KB;
        double* cf1 = FC0 + KB;
        for (int ib = i0; ib <= i1; ib += KB) {
            const int nb = (i1 - ib + 1 < KB) ? (i1 - ib + 1) : KB;
#define I_LOOP for (int b = 0, i = ib; b < nb; ++b, ++i)
            I_LOOP {
                DC0[b] = 0.0;
                CF0[b] = 0.0;
                FC0[b] = 0.0;
            }
            for (int k = 1; k <= N; ++k) {
                I_LOOP {
                    const double cff = 0.5 * d.metric(i, j);
                    DCk[k * KB + b] = cff * (Hz(i, j, k) + Hz(i - di, j - dj, k));
                    DC0[b] = DC0[b] + DCk[k * KB + b];
                    CF0[b] = CF0[b] + DCk[k * KB + b] * d.vel(i, j, k, nn);
                }
            }
            I_LOOP {
                cf1[b] = DC0[b];
                DC0[b] = 1.0 / DC0[b];
                CF0[b] = DC0[b] * (CF0[b] - d.avg1(i, j));
                d.bar(i, j, 1) = DC0[b] * d.avg1(i, j);
                d.bar(i, j, 2) = d.bar(i, j, 1);
#if defined DIAGNOSTICS_UV
                d.d2wrk(i, j, c.M2rate) = d.bar(i, j, 1) - d.d2int(i, j, c.M2rate) * DC0[b];
                d.d2int(i, j, c.M2rate) = d.bar(i, j, 1) * cf1[b];
#endif
            }
#if defined DIAGNOSTICS_UV
            for (int id = 1; id <= c.NDM2d - 1; ++id) {
                I_LOOP { d.d2wrk(i, j, id) = DC0[b] * d.d2wrk(i, j, id); }
            }
#endif
            I_LOOP {
                if (c.rep_w && i == west_i) {
                    for (int k = 1; k <= N; ++k)
                        d.vel(i, j, k, nn) = d.vel(i, j, k, nn) - CF0[b];
                }
                if (c.rep_e && i == c.Iend + 1) {
                    for (int k = 1; k <= N; ++k)
                        d.vel(i, j, k, nn) = d.vel(i, j, k, nn) - CF0[b];
                }
                if (c.rep_s && j == south_j && i >= ilo && i <= c.Iend) {
                    for (int k = 1; k <= N; ++k)
                        d.vel(i, j, k, nn) = d.vel(i, j, k, nn) - CF0[b];
                }
                if (c.rep_n && j == c.Mm + 1 && i >= ilo && i <= c.Iend) {
                    for (int k = 1; k <= N; ++k)
                        d.vel(i, j, k, nn) = d.vel(i, j, k, nn) - CF0[b];
                }
            }
            /* Correct mass flux, Hz*u/n. */
            for (int k = N; k >= 1; --k) {
                I_LOOP {
                    d.flux(i, j, k) =
                        0.5 * (d.flux(i, j, k) + d.vel(i, j, k, nn) * DCk[k * KB + b]);
                    FC0[b] = FC0[b] + d.flux(i, j, k);
#if defined DIAGNOSTICS_UV
                    d.d3wrk(i, j, k, c.M3rate) =
                        d.vel(i, j, k, nn) - d.d3wrk(i, j, k, c.M3rate);
#endif
                }
            }
            I_LOOP { FC0[b] = DC0[b] * (FC0[b] - d.avg2(i, j)); }
            for (int k = 1; k <= N; ++k) {
                I_LOOP { d.flux(i, j, k) = d.flux(i, j, k) - DCk[k * KB + b] * FC0[b]; }
            }
#undef I_LOOP
        }
    }
};

} // namespace

extern "C" {

```

</details>

<a id="src-cpp-step3d-tail"></a>

**source-block ID：`cpp-step3d-tail`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.cpp`；闭区间：**1141–1144**；行数：**4**；UTF-8 payload SHA256：`3e7a994e16b45e66e0e71c9cacf7f119e9fc579be38c198aa4089bf96a6e0716`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.cpp，1141–1144（完整原文，可展开）</summary>

<!-- source-block id=cpp-step3d-tail commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.cpp lines=1141-1144 count=4 sha256=3e7a994e16b45e66e0e71c9cacf7f119e9fc579be38c198aa4089bf96a6e0716 -->
```cpp

} // extern "C"

#endif // ROMS_KOKKOS && KK_STAGE_STEP3DUV
```

</details>

### 12.2 kokkos_step3d_t.cpp 无缺行重组

文件按 `cpp-t-fields:1–148` → 正文 `current-t-chunks:149–239` → `cpp-t-tail:240–241` 重组，完整 **241 行**。Fields 的 difference/curvature、两方向 horizontal slope、TVD correction、水平/垂向通量和全部顶底分支均在第一块，不是仅列函数名。

<a id="src-cpp-t-fields"></a>

**source-block ID：`cpp-t-fields`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d_t.cpp`；闭区间：**1–148**；行数：**148**；UTF-8 payload SHA256：`be27db0ed13a817a246ce9cf4579b0b7153d1230984b9b918ff8f20ea706e53c`。

<details>
<summary>ROMS/Cpp/kokkos_step3d_t.cpp，1–148（完整原文，可展开）</summary>

<!-- source-block id=cpp-t-fields commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d_t.cpp lines=1-148 count=148 sha256=be27db0ed13a817a246ce9cf4579b0b7153d1230984b9b918ff8f20ea706e53c -->
```cpp
/* U3/C4 and HSIMT tracer advection followed by spline vertical diffusion.
** Predictor slot 3 is read-only. Boundary exchange and epilogue stay in
** Fortran; this entry is called only after whole-tile eligibility checks
** in ROMS/Nonlinear/step3d_t.F. */
#include "kokkos_step3d_t.hpp"
#if defined ROMS_KOKKOS
#include <cmath>
#ifndef __sw_slave__
#include "kokkos_workspace.hpp"
#endif

namespace {
using roms::TracerConfig;
struct Slope {
    double gradient, ka;
};

#ifndef __sw_slave__
/* Raw host ownership needs no Kokkos service at thread/process teardown.
** The SW64 HostSpace and borrowed Fortran allocations share this memory.
** Each host thread has independent scratch; all supported launches join. */
thread_local roms::HostWorkspace workspace;
#endif

struct Fields {
    TracerConfig p;
    roms::Array2d<double> pm, pn;
    roms::Array3d<double> hz, huon, hvom, w, zr;
    roms::Array4d<double> akt;
    roms::Array5d<double> t, diag;

    KOKKOS_INLINE_FUNCTION
    double difference(int i, int j, int k, int it, bool xi) const {
        if (xi) {
            if (!p.periodic_ew && !p.composite_w && p.west && i == p.Istr - 1)
                ++i;
            else if (!p.periodic_ew && !p.composite_e && p.east && i == p.Iend + 2)
                --i;
            return t(i, j, k, 3, it) - t(i - 1, j, k, 3, it);
        }
        if (!p.periodic_ns && !p.composite_s && p.south && j == p.Jstr - 1)
            ++j;
        else if (!p.periodic_ns && !p.composite_n && p.north && j == p.Jend + 2)
            --j;
        return t(i, j, k, 3, it) - t(i, j - 1, k, 3, it);
    }

    KOKKOS_INLINE_FUNCTION
    double curvature(int i, int j, int k, int it, bool xi) const {
        return difference(i + (xi ? 1 : 0), j + (xi ? 0 : 1), k, it, xi) - difference(i, j, k, it, xi);
    }

    KOKKOS_INLINE_FUNCTION
    Slope horizontal_slope(int i, int j, int k, int it, bool xi) const {
        if (xi) {
            if (!p.periodic_ew && p.west && i == p.Istr - 1 && huon(p.Istr, j, k) >= 0)
                return {0, 0};
            if (!p.periodic_ew && p.east && i == p.Iend + 2 && huon(p.Iend + 1, j, k) < 0)
                return {0, 0};
            const double c = 0.125 * (pm(i - 1, j) + pm(i, j)) * (pn(i - 1, j) + pn(i, j)) * p.dt;
            const double c1 = c * (1.0 / hz(i - 1, j, k) + 1.0 / hz(i, j, k));
            return {t(i, j, k, 3, it) - t(i - 1, j, k, 3, it), 1.0 - std::abs(huon(i, j, k) * c1)};
        }
        if (!p.periodic_ns && p.south && j == p.Jstr - 1 && hvom(i, p.Jstr, k) >= 0)
            return {0, 0};
        if (!p.periodic_ns && p.north && j == p.Jend + 2 && hvom(i, p.Jend + 1, k) < 0)
            return {0, 0};
        const double c = 0.125 * (pn(i, j) + pn(i, j - 1)) * (pm(i, j) + pm(i, j - 1)) * p.dt;
        const double c1 = c * (1.0 / hz(i, j, k) + 1.0 / hz(i, j - 1, k));
        return {t(i, j, k, 3, it) - t(i, j - 1, k, 3, it), 1.0 - std::abs(hvom(i, j, k) * c1)};
    }

    KOKKOS_INLINE_FUNCTION
    double correction(Slope s, Slope neighbor, double inverse) const {
        double r = 0, rka = 0;
        if (std::abs(s.gradient) > 1.0e-12) {
            r = neighbor.gradient / s.gradient;
            rka = neighbor.ka * inverse;
        }
        const double a = p.cc1 * s.ka + p.cc2 - p.cc3 * inverse;
        const double b = -p.cc1 * s.ka + p.cc2 + p.cc3 * inverse;
        const double beta = a + b * r;
        const double limit = Kokkos::max(0.0, Kokkos::min(2.0, Kokkos::min(2.0 * r * rka, beta)));
        return 0.5 * limit * s.gradient * s.ka;
    }

    KOKKOS_INLINE_FUNCTION
    double horizontal(int i, int j, int k, int it, bool xi, int scheme) const {
        const int di = xi ? 1 : 0, dj = xi ? 0 : 1;
        const double transport = xi ? huon(i, j, k) : hvom(i, j, k);
        if (scheme == 0) {
            return transport * 0.5 * (t(i - di, j - dj, k, 3, it) + t(i, j, k, 3, it)) -
                   (1.0 / 6.0) * (curvature(i - di, j - dj, k, it, xi) * Kokkos::max(transport, 0.0) +
                                  curvature(i, j, k, it, xi) * Kokkos::min(transport, 0.0));
        }
        const Slope s = horizontal_slope(i, j, k, it, xi);
        const double inverse = s.ka <= 1.0e-12 ? 0.0 : 1.0 / Kokkos::max(s.ka, 1.0e-12);
        const bool positive = transport >= 0.0;
        const Slope neighbor = horizontal_slope(i + (positive ? -di : di), j + (positive ? -dj : dj), k, it, xi);
        const double delta = correction(s, neighbor, inverse);
        const double value = positive ? t(i - di, j - dj, k, 3, it) + delta : t(i, j, k, 3, it) - delta;
        return value * transport;
    }

    KOKKOS_INLINE_FUNCTION
    Slope vertical_slope(int i, int j, int k, int it) const {
        if (k == 0 || k == p.N)
            return {0, 0};
        const double c = pm(i, j) * pn(i, j) * p.dt;
        return {t(i, j, k + 1, 3, it) - t(i, j, k, 3, it),
                1.0 - std::abs(c * w(i, j, k) / (zr(i, j, k + 1) - zr(i, j, k)))};
    }

    KOKKOS_INLINE_FUNCTION
    double vertical(int i, int j, int k, int it, int scheme) const {
        if (k == 0 || k == p.N)
            return 0.0;
        const double velocity = w(i, j, k);
        if (scheme == 0) {
            if (k == 1)
                return velocity * (0.5 * t(i, j, 1, 3, it) + (7.0 / 12.0) * t(i, j, 2, 3, it) -
                                   (1.0 / 12.0) * t(i, j, 3, 3, it));
            if (k == p.N - 1)
                return velocity * (0.5 * t(i, j, p.N, 3, it) + (7.0 / 12.0) * t(i, j, p.N - 1, 3, it) -
                                   (1.0 / 12.0) * t(i, j, p.N - 2, 3, it));
            return velocity * ((7.0 / 12.0) * (t(i, j, k, 3, it) + t(i, j, k + 1, 3, it)) -
                               (1.0 / 12.0) * (t(i, j, k - 1, 3, it) + t(i, j, k + 2, 3, it)));
        }
        if (k == 1 && velocity >= 0)
            return velocity * t(i, j, k, 3, it);
        if (k == p.N - 1 && velocity < 0)
            return velocity * t(i, j, k + 1, 3, it);
        const Slope s = vertical_slope(i, j, k, it);
        const Slope neighbor = vertical_slope(i, j, k + (velocity >= 0 ? -1 : 1), it);
        const double delta = correction(s, neighbor, 1.0 / s.ka);
        return velocity * (velocity >= 0 ? t(i, j, k, 3, it) + delta : t(i, j, k + 1, 3, it) - delta);
    }
};
} // namespace

/*
** A work item owns one (j, tracer) row of the tile and walks it in chunks
** of KB columns, level by level with i innermost: the Fortran loop order,
** which keeps the working set of a chunk in CPE cache.  Each xi-face flux
** is evaluated once per chunk and shared by the two columns beside it.
** Per point the operations and their order are those of step3d_t.F, so
** the Serial result equals the Fortran exactly.
*/
```

</details>

<a id="src-cpp-t-tail"></a>

**source-block ID：`cpp-t-tail`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d_t.cpp`；闭区间：**240–241**；行数：**2**；UTF-8 payload SHA256：`626de321eb397f6600939892a0009f31bc378f7e886f9acc5ec0af18db6a99a7`。

<details>
<summary>ROMS/Cpp/kokkos_step3d_t.cpp，240–241（完整原文，可展开）</summary>

<!-- source-block id=cpp-t-tail commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d_t.cpp lines=240-241 count=2 sha256=626de321eb397f6600939892a0009f31bc378f7e886f9acc5ec0af18db6a99a7 -->
```cpp

#endif
```

</details>

<a id="appendix-abi"></a>

## 13. 附录 C：完整 C ABI、配置类型与当前调用端

### 13.1 完整 C++ 声明与 Fortran BIND(C) 模块

以下四个文件全文保留字段类型/顺序、scheme 枚举、所有参数、VALUE 属性，以及 rank 2–5 的连续性检查定义。`m2/m3` 固定 12 项；Fortran 1 起的诊断索引在 C++ 仍按其原数值使用，不统一减 1。只有 scheme 表和 pair 数组的 C 容器下标按 0 起访问。

<a id="src-abi-step3d-hpp"></a>

**source-block ID：`abi-step3d-hpp`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d.hpp`；闭区间：**1–120**；行数：**120**；UTF-8 payload SHA256：`e0a28b1698cf91a9761db2bddbfc9e2578bd53a6588de21f8dc042ef45a207ad`。

<details>
<summary>ROMS/Cpp/kokkos_step3d.hpp，1–120（完整原文，可展开）</summary>

<!-- source-block id=abi-step3d-hpp commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d.hpp lines=1-120 count=120 sha256=e0a28b1698cf91a9761db2bddbfc9e2578bd53a6588de21f8dc042ef45a207ad -->
```cpp
/*
** Kokkos kernels for the 3-D time-stepping routines:
**
**   pre_step3d.F   tracer predictor (horizontal advection, vertical
**                  advection with the artificial continuity equation,
**                  explicit vertical diffusion) and the 3-D momentum
**                  predictor.
**   step3d_uv.F    3-D momentum corrector: implicit vertical viscosity,
**                  vertical-mean replacement, 2-D/3-D coupling and the
**                  Huon/Hvom mass fluxes.
**
** Each entry is compiled only when its KK_STAGE_* flag is set, in which
** case the Fortran caller skips the loops it replaces.  Lateral boundary
** conditions, halo exchanges and point sources stay in Fortran.
*/

#ifndef ROMS_KOKKOS_STEP3D_HPP
#define ROMS_KOKKOS_STEP3D_HPP

#if defined ROMS_KOKKOS

#include "kokkos_config.hpp"

namespace roms {

/* Mirror of TYPE(kokkos_pre3d_config) in kokkos_step3d_mod.F; the field
** order must match. */
struct Pre3dConfig {
    int LBi, UBi, LBj, UBj;
    int Istr, Iend, Jstr, Jend, IstrU, JstrV;
    int N, NT, NAT, NDT;
    int nrhs, nstp, nnew;
    int step; /* 0: iic==ntfirst, 1: ntfirst+1, 2: later */
    int iTrate, iTvdif;
    int NDM3d, NDrhs, M3pgrd, M3vvis, M3rate;
    int fold_w, fold_e, fold_s, fold_n; /* closed-edge flux folds */
    double dt, lambda;
};

/* Horizontal / vertical tracer advection schemes, as resolved from
** Hadvection(itrc,ng) / Vadvection(itrc,ng) by the caller. */
enum Pre3dScheme {
    kSchemeNone = 0,
    kCentered2 = 1,
    kUpstream1 = 2, /* MPDATA or HSIMT: first-order upstream here */
    kUpstream3 = 3,
    kAkima4 = 4,
    kCentered4 = 5, /* CENTERED4 or SPLIT_U3 */
    kSplines = 6    /* vertical only */
};

/* Mirror of TYPE(kokkos_uv3d_config) in kokkos_step3d_mod.F. */
struct Uv3dConfig {
    int LBi, UBi, LBj, UBj;
    int Istr, Iend, Jstr, Jend, IstrU, JstrV;
    int IstrP, IstrT, IendT, JstrT, JendT;
    int Mm, N, nrhs, nnew;
    int step; /* 0: iic==ntfirst, 1: ntfirst+1, 2: later */
    int NDM2d, NDM3d, NDrhs;
    int M2pgrd, M2bstr, M2sstr, M2rate, M3pgrd, M3vvis, M3rate;
    int M3xvis, M3yvis, M3hvis; /* 0 without UV_VIS2/UV_VIS4 */
    /* (M2, M3) index pairs integrated over the column for the interior
    ** mean replacement: pgrd, vvis->bstr, then Coriolis, viscosity and
    ** advection terms as compiled. */
    int npairs, m2[12], m3[12];
    /* Boundary-point replacement switches (step3d_uv.F, coupling). */
    int rep_w, rep_e, rep_s, rep_n;
    double dt, lambda;
};

} // namespace roms

extern "C" {

/* pre_step3d: tracer predictor t(:,:,:,3,:), explicit vertical diffusion
** into t(:,:,:,nnew,:) and the DiaTwrk rate/vdif terms. */
void k_pre_step3d_t(const roms::Pre3dConfig* c, const int* hadv,
                    const int* vadv,
                    void* pm_p, void* pn_p, void* Hz_p,
                    void* Huon_p, void* Hvom_p, void* z_r_p, void* W_p,
                    void* Akt_p, void* btflx_p, void* stflx_p,
                    void* t_p, void* DiaTwrk_p);

/* pre_step3d: u/v predictor into time level nnew and DiaU3wrk/DiaV3wrk. */
void k_pre_step3d_uv(const roms::Pre3dConfig* c,
                     void* pm_p, void* pn_p, void* Hz_p, void* z_r_p,
                     void* Akv_p, void* bustr_p, void* bvstr_p,
                     void* sustr_p, void* svstr_p,
                     void* ru_p, void* rv_p, void* u_p, void* v_p,
                     void* DiaU3wrk_p, void* DiaV3wrk_p,
                     void* DiaRU_p, void* DiaRV_p);

/* step3d_uv: right-hand-side step, implicit vertical viscosity and the
** interior vertical-mean replacement, rows Jstr..Jend. */
void k_step3d_uv_solve(const roms::Uv3dConfig* c,
                       void* pm_p, void* pn_p, void* on_u_p, void* om_v_p,
                       void* Hz_p, void* z_r_p, void* Akv_p,
                       void* DU_avg1_p, void* DV_avg1_p,
                       void* ru_p, void* rv_p, void* u_p, void* v_p,
                       void* DiaU2wrk_p, void* DiaV2wrk_p,
                       void* DiaU3wrk_p, void* DiaV3wrk_p,
                       void* DiaRU_p, void* DiaRV_p);

/* step3d_uv: 2-D/3-D coupling (ubar/vbar, boundary-point mean
** replacement, Huon/Hvom), rows JstrT..JendT, after u3dbc/v3dbc. */
void k_step3d_uv_couple(const roms::Uv3dConfig* c,
                        void* on_u_p, void* om_v_p, void* Hz_p,
                        void* DU_avg1_p, void* DU_avg2_p,
                        void* DV_avg1_p, void* DV_avg2_p,
                        void* u_p, void* v_p, void* ubar_p, void* vbar_p,
                        void* Huon_p, void* Hvom_p,
                        void* DiaU2wrk_p, void* DiaV2wrk_p,
                        void* DiaU2int_p, void* DiaV2int_p,
                        void* DiaU3wrk_p, void* DiaV3wrk_p);

} // extern "C"

#endif // ROMS_KOKKOS

#endif // ROMS_KOKKOS_STEP3D_HPP
```

</details>

<a id="src-abi-tracer-hpp"></a>

**source-block ID：`abi-tracer-hpp`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d_t.hpp`；闭区间：**1–24**；行数：**24**；UTF-8 payload SHA256：`7aeeea786a89e741828ab6ff76e5966b1abe6253d7b3da5ba1f3cea8344e3b89`。

<details>
<summary>ROMS/Cpp/kokkos_step3d_t.hpp，1–24（完整原文，可展开）</summary>

<!-- source-block id=abi-tracer-hpp commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d_t.hpp lines=1-24 count=24 sha256=7aeeea786a89e741828ab6ff76e5966b1abe6253d7b3da5ba1f3cea8344e3b89 -->
```cpp
#ifndef ROMS_KOKKOS_STEP3D_T_HPP
#define ROMS_KOKKOS_STEP3D_T_HPP
#include "kokkos_config.hpp"

#if defined ROMS_KOKKOS
namespace roms {
/* Mirrored exactly by kokkos_tracer_config, including C alignment. */
struct TracerConfig {
    int LBi, UBi, LBj, UBj;
    int Istr, Iend, Jstr, Jend;
    int N, NT, NAT, NDT, nnew;
    int xadv, yadv, hadv, vadv, vdif;
    int west, east, south, north, periodic_ew, periodic_ns;
    int composite_w, composite_e, composite_s, composite_n;
    double dt, cc1, cc2, cc3;
};
} // namespace roms
/* Tracer advection and spline vertical diffusion for one tile, with the
** Fortran loop order inside each chunk of columns (i innermost). */
extern "C" void k_step3d_t_chunks(const roms::TracerConfig* config,
                                  const int* schemes, void* pm, void* pn, void* Hz, void* Huon, void* Hvom,
                                  void* W, void* zr, void* Akt, void* tracer, void* diagnostics);
#endif
#endif
```

</details>

<a id="src-abi-step3d-fortran"></a>

**source-block ID：`abi-step3d-fortran`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d_mod.F`；闭区间：**1–117**；行数：**117**；UTF-8 payload SHA256：`c8b813ad32accf3a17dfc2858349370c015cb85f7ce7945a40b3b5a4c0eadad2`。

<details>
<summary>ROMS/Cpp/kokkos_step3d_mod.F，1–117（完整原文，可展开）</summary>

<!-- source-block id=abi-step3d-fortran commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d_mod.F lines=1-117 count=117 sha256=c8b813ad32accf3a17dfc2858349370c015cb85f7ce7945a40b3b5a4c0eadad2 -->
```fortran
      MODULE kokkos_step3d_mod
!
!  Fortran interfaces to the Kokkos 3-D time-stepping kernels in
!  ROMS/Cpp/kokkos_step3d.cpp.  Arrays cross as C_LOC pointers, so no
!  data is copied; the caller's dummies carry TARGET for that.
!
      USE, INTRINSIC :: iso_c_binding, ONLY : C_PTR, C_INT, C_DOUBLE
      implicit none
      PRIVATE
      PUBLIC :: kokkos_pre3d_config, kokkos_uv3d_config
      PUBLIC :: k_pre_step3d_t, k_pre_step3d_uv
      PUBLIC :: k_step3d_uv_solve, k_step3d_uv_couple
!
!  Mirror of roms::Pre3dConfig in ROMS/Cpp/kokkos_step3d.hpp; the field
!  order must match exactly.
!
      TYPE, BIND(C) :: kokkos_pre3d_config
        integer(C_INT) :: LBi, UBi, LBj, UBj
        integer(C_INT) :: Istr, Iend, Jstr, Jend, IstrU, JstrV
        integer(C_INT) :: N, NT, NAT, NDT
        integer(C_INT) :: nrhs, nstp, nnew
        integer(C_INT) :: step
        integer(C_INT) :: iTrate, iTvdif
        integer(C_INT) :: NDM3d, NDrhs, M3pgrd, M3vvis, M3rate
        integer(C_INT) :: fold_w, fold_e, fold_s, fold_n
        real(C_DOUBLE) :: dt, lambda
      END TYPE kokkos_pre3d_config
!
!  Mirror of roms::Uv3dConfig.
!
      TYPE, BIND(C) :: kokkos_uv3d_config
        integer(C_INT) :: LBi, UBi, LBj, UBj
        integer(C_INT) :: Istr, Iend, Jstr, Jend, IstrU, JstrV
        integer(C_INT) :: IstrP, IstrT, IendT, JstrT, JendT
        integer(C_INT) :: Mm, N, nrhs, nnew
        integer(C_INT) :: step
        integer(C_INT) :: NDM2d, NDM3d, NDrhs
        integer(C_INT) :: M2pgrd, M2bstr, M2sstr, M2rate
        integer(C_INT) :: M3pgrd, M3vvis, M3rate
        integer(C_INT) :: M3xvis, M3yvis, M3hvis
        integer(C_INT) :: npairs, m2(12), m3(12)
        integer(C_INT) :: rep_w, rep_e, rep_s, rep_n
        real(C_DOUBLE) :: dt, lambda
      END TYPE kokkos_uv3d_config
!
      INTERFACE
!
        SUBROUTINE k_pre_step3d_t (cfg, hadv, vadv,                     &
     &                             pm_p, pn_p, Hz_p, Huon_p, Hvom_p,    &
     &                             z_r_p, W_p, Akt_p, btflx_p,          &
     &                             stflx_p, t_p, DiaTwrk_p)             &
     &           BIND(C, NAME='k_pre_step3d_t')
          IMPORT :: C_PTR, C_INT, kokkos_pre3d_config
          TYPE(kokkos_pre3d_config), INTENT(IN) :: cfg
          integer(C_INT), INTENT(IN) :: hadv(*), vadv(*)
          TYPE(C_PTR), VALUE :: pm_p, pn_p, Hz_p, Huon_p, Hvom_p
          TYPE(C_PTR), VALUE :: z_r_p, W_p, Akt_p, btflx_p, stflx_p
          TYPE(C_PTR), VALUE :: t_p, DiaTwrk_p
        END SUBROUTINE k_pre_step3d_t
!
        SUBROUTINE k_pre_step3d_uv (cfg, pm_p, pn_p, Hz_p, z_r_p,       &
     &                              Akv_p, bustr_p, bvstr_p,            &
     &                              sustr_p, svstr_p, ru_p, rv_p,       &
     &                              u_p, v_p, DiaU3wrk_p, DiaV3wrk_p,   &
     &                              DiaRU_p, DiaRV_p)                   &
     &           BIND(C, NAME='k_pre_step3d_uv')
          IMPORT :: C_PTR, kokkos_pre3d_config
          TYPE(kokkos_pre3d_config), INTENT(IN) :: cfg
          TYPE(C_PTR), VALUE :: pm_p, pn_p, Hz_p, z_r_p, Akv_p
          TYPE(C_PTR), VALUE :: bustr_p, bvstr_p, sustr_p, svstr_p
          TYPE(C_PTR), VALUE :: ru_p, rv_p, u_p, v_p
          TYPE(C_PTR), VALUE :: DiaU3wrk_p, DiaV3wrk_p, DiaRU_p, DiaRV_p
        END SUBROUTINE k_pre_step3d_uv
!
        SUBROUTINE k_step3d_uv_solve (cfg, pm_p, pn_p, on_u_p, om_v_p,  &
     &                                Hz_p, z_r_p, Akv_p,               &
     &                                DU_avg1_p, DV_avg1_p,             &
     &                                ru_p, rv_p, u_p, v_p,             &
     &                                DiaU2wrk_p, DiaV2wrk_p,           &
     &                                DiaU3wrk_p, DiaV3wrk_p,           &
     &                                DiaRU_p, DiaRV_p)                 &
     &           BIND(C, NAME='k_step3d_uv_solve')
          IMPORT :: C_PTR, kokkos_uv3d_config
          TYPE(kokkos_uv3d_config), INTENT(IN) :: cfg
          TYPE(C_PTR), VALUE :: pm_p, pn_p, on_u_p, om_v_p
          TYPE(C_PTR), VALUE :: Hz_p, z_r_p, Akv_p
          TYPE(C_PTR), VALUE :: DU_avg1_p, DV_avg1_p
          TYPE(C_PTR), VALUE :: ru_p, rv_p, u_p, v_p
          TYPE(C_PTR), VALUE :: DiaU2wrk_p, DiaV2wrk_p
          TYPE(C_PTR), VALUE :: DiaU3wrk_p, DiaV3wrk_p
          TYPE(C_PTR), VALUE :: DiaRU_p, DiaRV_p
        END SUBROUTINE k_step3d_uv_solve
!
        SUBROUTINE k_step3d_uv_couple (cfg, on_u_p, om_v_p, Hz_p,       &
     &                                 DU_avg1_p, DU_avg2_p,            &
     &                                 DV_avg1_p, DV_avg2_p,            &
     &                                 u_p, v_p, ubar_p, vbar_p,        &
     &                                 Huon_p, Hvom_p,                  &
     &                                 DiaU2wrk_p, DiaV2wrk_p,          &
     &                                 DiaU2int_p, DiaV2int_p,          &
     &                                 DiaU3wrk_p, DiaV3wrk_p)          &
     &           BIND(C, NAME='k_step3d_uv_couple')
          IMPORT :: C_PTR, kokkos_uv3d_config
          TYPE(kokkos_uv3d_config), INTENT(IN) :: cfg
          TYPE(C_PTR), VALUE :: on_u_p, om_v_p, Hz_p
          TYPE(C_PTR), VALUE :: DU_avg1_p, DU_avg2_p
          TYPE(C_PTR), VALUE :: DV_avg1_p, DV_avg2_p
          TYPE(C_PTR), VALUE :: u_p, v_p, ubar_p, vbar_p
          TYPE(C_PTR), VALUE :: Huon_p, Hvom_p
          TYPE(C_PTR), VALUE :: DiaU2wrk_p, DiaV2wrk_p
          TYPE(C_PTR), VALUE :: DiaU2int_p, DiaV2int_p
          TYPE(C_PTR), VALUE :: DiaU3wrk_p, DiaV3wrk_p
        END SUBROUTINE k_step3d_uv_couple
!
      END INTERFACE
!
      END MODULE kokkos_step3d_mod
```

</details>

<a id="src-abi-tracer-fortran"></a>

**source-block ID：`abi-tracer-fortran`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_step3d_t_mod.F`；闭区间：**1–141**；行数：**141**；UTF-8 payload SHA256：`836447cdefafb18fd1ea9e8416370d38c180ddb462265f801e123aec8218e82d`。

<details>
<summary>ROMS/Cpp/kokkos_step3d_t_mod.F，1–141（完整原文，可展开）</summary>

<!-- source-block id=abi-tracer-fortran commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_step3d_t_mod.F lines=1-141 count=141 sha256=836447cdefafb18fd1ea9e8416370d38c180ddb462265f801e123aec8218e82d -->
```fortran
      MODULE kokkos_step3d_t_mod
!
!  Zero-copy interface for the diagnostic U3/C4 and HSIMT tracer path.
!
      USE, INTRINSIC :: iso_c_binding, ONLY : C_INT, C_DOUBLE, C_PTR, &
     &                                      C_INTPTR_T, C_LOC
      implicit none
      PRIVATE
      PUBLIC :: kokkos_tracer_config, k_step3d_t_chunks, kk_is_contiguous
!
      TYPE, BIND(C) :: kokkos_tracer_config
        integer(C_INT) :: LBi, UBi, LBj, UBj
        integer(C_INT) :: Istr, Iend, Jstr, Jend
        integer(C_INT) :: N, NT, NAT, NDT, nnew
        integer(C_INT) :: xadv, yadv, hadv, vadv, vdif
        integer(C_INT) :: west, east, south, north
        integer(C_INT) :: periodic_ew, periodic_ns
        integer(C_INT) :: composite_w, composite_e
        integer(C_INT) :: composite_s, composite_n
        real(C_DOUBLE) :: dt, cc1, cc2, cc3
      END TYPE kokkos_tracer_config
!
!  The SW GCC 7 frontend lacks the IS_CONTIGUOUS intrinsic. Verify
!  every nontrivial array stride instead, using the actual descriptor.
      INTERFACE kk_is_contiguous
        MODULE PROCEDURE kk_contiguous2, kk_contiguous3
        MODULE PROCEDURE kk_contiguous4, kk_contiguous5
      END INTERFACE
!
      INTERFACE
        SUBROUTINE k_step3d_t_chunks (config, schemes, pm, pn, Hz,      &
     &                        Huon, Hvom, W, zr, Akt, tracer,           &
     &                        diagnostics)                              &
     &                        BIND(C, NAME='k_step3d_t_chunks')
          IMPORT :: kokkos_tracer_config, C_INT, C_PTR
          TYPE(kokkos_tracer_config), INTENT(IN) :: config
          integer(C_INT), INTENT(IN) :: schemes(*)
          TYPE(C_PTR), VALUE :: pm, pn, Hz, Huon, Hvom, W, zr, Akt
          TYPE(C_PTR), VALUE :: tracer, diagnostics
        END SUBROUTINE k_step3d_t_chunks
      END INTERFACE
      CONTAINS
      logical FUNCTION kk_contiguous2(a)
      real(C_DOUBLE), TARGET, INTENT(IN) :: a(:,:)
      integer(C_INTPTR_T) :: base, stride
      base=TRANSFER(C_LOC(a(1,1)),0_C_INTPTR_T)
      stride=8_C_INTPTR_T
      kk_contiguous2=.TRUE.
      IF (SIZE(a,1).gt.1) THEN
        kk_contiguous2=kk_contiguous2.and. &
     &    (TRANSFER(C_LOC(a(2,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,1),C_INTPTR_T)
      IF (SIZE(a,2).gt.1) THEN
        kk_contiguous2=kk_contiguous2.and. &
     &    (TRANSFER(C_LOC(a(1,2)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      END FUNCTION kk_contiguous2
!
      logical FUNCTION kk_contiguous3(a)
      real(C_DOUBLE), TARGET, INTENT(IN) :: a(:,:,:)
      integer(C_INTPTR_T) :: base, stride
      base=TRANSFER(C_LOC(a(1,1,1)),0_C_INTPTR_T)
      stride=8_C_INTPTR_T
      kk_contiguous3=.TRUE.
      IF (SIZE(a,1).gt.1) THEN
        kk_contiguous3=kk_contiguous3.and. &
     &    (TRANSFER(C_LOC(a(2,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,1),C_INTPTR_T)
      IF (SIZE(a,2).gt.1) THEN
        kk_contiguous3=kk_contiguous3.and. &
     &    (TRANSFER(C_LOC(a(1,2,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,2),C_INTPTR_T)
      IF (SIZE(a,3).gt.1) THEN
        kk_contiguous3=kk_contiguous3.and. &
     &    (TRANSFER(C_LOC(a(1,1,2)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      END FUNCTION kk_contiguous3
!
      logical FUNCTION kk_contiguous4(a)
      real(C_DOUBLE), TARGET, INTENT(IN) :: a(:,:,:,:)
      integer(C_INTPTR_T) :: base, stride
      base=TRANSFER(C_LOC(a(1,1,1,1)),0_C_INTPTR_T)
      stride=8_C_INTPTR_T
      kk_contiguous4=.TRUE.
      IF (SIZE(a,1).gt.1) THEN
        kk_contiguous4=kk_contiguous4.and. &
     &    (TRANSFER(C_LOC(a(2,1,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,1),C_INTPTR_T)
      IF (SIZE(a,2).gt.1) THEN
        kk_contiguous4=kk_contiguous4.and. &
     &    (TRANSFER(C_LOC(a(1,2,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,2),C_INTPTR_T)
      IF (SIZE(a,3).gt.1) THEN
        kk_contiguous4=kk_contiguous4.and. &
     &    (TRANSFER(C_LOC(a(1,1,2,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,3),C_INTPTR_T)
      IF (SIZE(a,4).gt.1) THEN
        kk_contiguous4=kk_contiguous4.and. &
     &    (TRANSFER(C_LOC(a(1,1,1,2)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      END FUNCTION kk_contiguous4
!
      logical FUNCTION kk_contiguous5(a)
      real(C_DOUBLE), TARGET, INTENT(IN) :: a(:,:,:,:,:)
      integer(C_INTPTR_T) :: base, stride
      base=TRANSFER(C_LOC(a(1,1,1,1,1)),0_C_INTPTR_T)
      stride=8_C_INTPTR_T
      kk_contiguous5=.TRUE.
      IF (SIZE(a,1).gt.1) THEN
        kk_contiguous5=kk_contiguous5.and. &
     &    (TRANSFER(C_LOC(a(2,1,1,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,1),C_INTPTR_T)
      IF (SIZE(a,2).gt.1) THEN
        kk_contiguous5=kk_contiguous5.and. &
     &    (TRANSFER(C_LOC(a(1,2,1,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,2),C_INTPTR_T)
      IF (SIZE(a,3).gt.1) THEN
        kk_contiguous5=kk_contiguous5.and. &
     &    (TRANSFER(C_LOC(a(1,1,2,1,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,3),C_INTPTR_T)
      IF (SIZE(a,4).gt.1) THEN
        kk_contiguous5=kk_contiguous5.and. &
     &    (TRANSFER(C_LOC(a(1,1,1,2,1)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      stride=stride*INT(SIZE(a,4),C_INTPTR_T)
      IF (SIZE(a,5).gt.1) THEN
        kk_contiguous5=kk_contiguous5.and. &
     &    (TRANSFER(C_LOC(a(1,1,1,1,2)),0_C_INTPTR_T)-base.eq.stride)
      END IF
      END FUNCTION kk_contiguous5
!
      END MODULE kokkos_step3d_t_mod
```

</details>

### 13.2 当前 PRE3D 调用端

第一个连续块含当前模块/子程序完整形参、两类数组声明（含 TARGET）、局部类型、whole-tile scheme/尺寸/源项判定、配置填充、两个调用与 ELSE 回退起点。第二块保留该 ELSE 的结束、共同侧边界及交换和例程结束；两块之间的当前 Fortran 回退计算内容在原始全文可对照，但这里不谎称这两个区段可直接连成当前完整 Fortran 文件。

<a id="src-caller-pre-declarations-hook"></a>

**source-block ID：`caller-pre-declarations-hook`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Nonlinear/pre_step3d.F`；闭区间：**1–462**；行数：**462**；UTF-8 payload SHA256：`22e148a878db645649cc935fca0fc0dac64021adc19520a07e6429631f7d8c21`。

<details>
<summary>ROMS/Nonlinear/pre_step3d.F，1–462（完整原文，可展开）</summary>

<!-- source-block id=caller-pre-declarations-hook commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Nonlinear/pre_step3d.F lines=1-462 count=462 sha256=22e148a878db645649cc935fca0fc0dac64021adc19520a07e6429631f7d8c21 -->
```fortran
#include "cppdefs.h"
      MODULE pre_step3d_mod
#if defined NONLINEAR && defined SOLVE3D
# define NEUMANN
!
!git $Id$
!=======================================================================
!  Copyright (c) 2002-2025 The ROMS Group                              !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                            Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  This subroutine initialize computations for new time step of the    !
!  3D primitive variables.                                             !
!                                                                      !
!  Both n-1 and n-2 time-step contributions of the  Adams/Bashforth    !
!  scheme are added here to u and v at time index "nnew", since the    !
!  right-hand-side  arrays ru and rv at  n-2  will be overwriten in    !
!  subsequent calls to routines within the 3D engine.                  !
!                                                                      !
!  It also computes the time  "n"  vertical viscosity and diffusion    !
!  contributions of the Crank-Nicholson implicit scheme because the    !
!  thicknesses "Hz" will be overwriten at the end of the  2D engine    !
!  (barotropic mode) computations.                                     !
!                                                                      !
!  The actual time step will be carried out in routines "step3d_uv"    !
!  and "step3d_t".                                                     !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: pre_step3d
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE pre_step3d (ng, tile)
!***********************************************************************
!
      USE mod_param
# ifdef DIAGNOSTICS
      USE mod_diags
# endif
      USE mod_forces
      USE mod_grid
      USE mod_mixing
      USE mod_ocean
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
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 22, __LINE__, MyFile)
# endif
      CALL pre_step3d_tile (ng, tile,                                   &
     &                      LBi, UBi, LBj, UBj,                         &
     &                      IminS, ImaxS, JminS, JmaxS,                 &
     &                      nrhs(ng), nstp(ng), nnew(ng),               &
# ifdef MASKING
     &                      GRID(ng) % rmask,                           &
     &                      GRID(ng) % umask,                           &
     &                      GRID(ng) % vmask,                           &
#  if defined SOLAR_SOURCE && defined WET_DRY
     &                      GRID(ng) % rmask_wet,                       &
#  endif
# endif

     &                      GRID(ng) % pm,                              &
     &                      GRID(ng) % pn,                              &
     &                      GRID(ng) % Hz,                              &
     &                      GRID(ng) % Huon,                            &
     &                      GRID(ng) % Hvom,                            &
     &                      GRID(ng) % z_r,                             &
     &                      GRID(ng) % z_w,                             &
     &                      FORCES(ng) % btflx,                         &
     &                      FORCES(ng) % bustr,                         &
     &                      FORCES(ng) % bvstr,                         &
     &                      FORCES(ng) % stflx,                         &
     &                      FORCES(ng) % sustr,                         &
     &                      FORCES(ng) % svstr,                         &
# ifdef SOLAR_SOURCE
     &                      FORCES(ng) % srflx,                         &
# endif
     &                      MIXING(ng) % Akt,                           &
     &                      MIXING(ng) % Akv,                           &
# ifdef LMD_NONLOCAL
     &                      MIXING(ng) % ghats,                         &
# endif
     &                      OCEAN(ng) % W,                              &
# ifdef WEC_VF
     &                      OCEAN(ng) % W_stokes,                       &
# endif
     &                      OCEAN(ng) % ru,                             &
     &                      OCEAN(ng) % rv,                             &
# ifdef DIAGNOSTICS_TS
     &                      DIAGS(ng) % DiaTwrk,                        &
# endif
# ifdef DIAGNOSTICS_UV
     &                      DIAGS(ng) % DiaU3wrk,                       &
     &                      DIAGS(ng) % DiaV3wrk,                       &
     &                      DIAGS(ng) % DiaRU,                          &
     &                      DIAGS(ng) % DiaRV,                          &
# endif
     &                      OCEAN(ng) % t,                              &
     &                      OCEAN(ng) % u,                              &
     &                      OCEAN(ng) % v)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 22, __LINE__, MyFile)
# endif
!
      RETURN
      END SUBROUTINE pre_step3d
!
!***********************************************************************
      SUBROUTINE pre_step3d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            IminS, ImaxS, JminS, JmaxS,           &
     &                            nrhs, nstp, nnew,                     &
# ifdef MASKING
     &                            rmask, umask, vmask,                  &
#  if defined SOLAR_SOURCE && defined WET_DRY
     &                            rmask_wet,                            &
#  endif
# endif
     &                            pm, pn,                               &
     &                            Hz, Huon, Hvom,                       &
     &                            z_r, z_w,                             &
     &                            btflx, bustr, bvstr,                  &
     &                            stflx, sustr, svstr,                  &
# ifdef SOLAR_SOURCE
     &                            srflx,                                &
# endif
     &                            Akt, Akv,                             &
# ifdef LMD_NONLOCAL
     &                            ghats,                                &
# endif
     &                            W,                                    &
# ifdef WEC_VF
     &                            W_stokes,                             &
# endif
     &                            ru, rv,                               &
# ifdef DIAGNOSTICS_TS
     &                            DiaTwrk,                              &
# endif
# ifdef DIAGNOSTICS_UV
     &                            DiaU3wrk, DiaV3wrk,                   &
     &                            DiaRU, DiaRV,                         &
# endif
     &                            t, u, v)
!***********************************************************************
!
      USE mod_param
      USE mod_scalars
      USE mod_sources
# if defined ROMS_KOKKOS && defined KK_STAGE_PRE3D
      USE, INTRINSIC :: iso_c_binding, ONLY : C_LOC, C_INT
      USE kokkos_step3d_mod, ONLY : kokkos_pre3d_config,                &
     &                              k_pre_step3d_t, k_pre_step3d_uv
# endif
!
      USE exchange_3d_mod, ONLY : exchange_r3d_tile
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange4d
# endif
      USE t3dbc_mod, ONLY : t3dbc_tile
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nrhs, nstp, nnew
!
# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
#   if defined SOLAR_SOURCE && defined WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:,LBj:)
#   endif
#  endif
      real(r8), intent(in), TARGET :: pm(LBi:,LBj:)
      real(r8), intent(in), TARGET :: pn(LBi:,LBj:)
      real(r8), intent(in), TARGET :: Hz(LBi:,LBj:,:)
      real(r8), intent(in), TARGET :: Huon(LBi:,LBj:,:)
      real(r8), intent(in), TARGET :: Hvom(LBi:,LBj:,:)
      real(r8), intent(in), TARGET :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in), TARGET :: btflx(LBi:,LBj:,:)
      real(r8), intent(in), TARGET :: bustr(LBi:,LBj:)
      real(r8), intent(in), TARGET :: bvstr(LBi:,LBj:)
      real(r8), intent(in), TARGET :: stflx(LBi:,LBj:,:)
      real(r8), intent(in), TARGET :: sustr(LBi:,LBj:)
      real(r8), intent(in), TARGET :: svstr(LBi:,LBj:)
#  ifdef SOLAR_SOURCE
      real(r8), intent(in) :: srflx(LBi:,LBj:)
#  endif
#  ifdef SUN
      real(r8), intent(in), TARGET :: Akt(LBi:UBi,LBj:UBj,0:N(ng),NAT)
#  else
      real(r8), intent(in), TARGET :: Akt(LBi:,LBj:,0:,:)
#  endif
      real(r8), intent(in), TARGET :: Akv(LBi:,LBj:,0:)
#  ifdef LMD_NONLOCAL
      real(r8), intent(in) :: ghats(LBi:,LBj:,0:,:)
#  endif
      real(r8), intent(in), TARGET :: W(LBi:,LBj:,0:)
#  ifdef WEC_VF
      real(r8), intent(in) :: W_stokes(LBi:,LBj:,0:)
#  endif
      real(r8), intent(in), TARGET :: ru(LBi:,LBj:,0:,:)
      real(r8), intent(in), TARGET :: rv(LBi:,LBj:,0:,:)
#  ifdef DIAGNOSTICS_TS
      real(r8), intent(inout), TARGET :: DiaTwrk(LBi:,LBj:,:,:,:)
#  endif
#  ifdef DIAGNOSTICS_UV
      real(r8), intent(inout), TARGET :: DiaU3wrk(LBi:,LBj:,:,:)
      real(r8), intent(inout), TARGET :: DiaV3wrk(LBi:,LBj:,:,:)
      real(r8), intent(inout), TARGET :: DiaRU(LBi:,LBj:,:,:,:)
      real(r8), intent(inout), TARGET :: DiaRV(LBi:,LBj:,:,:,:)
#  endif
#  ifdef SUN
      real(r8), intent(inout), TARGET :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
#  else
      real(r8), intent(inout), TARGET :: t(LBi:,LBj:,:,:,:)
#  endif
      real(r8), intent(inout), TARGET :: u(LBi:,LBj:,:,:)
      real(r8), intent(inout), TARGET :: v(LBi:,LBj:,:,:)

# else

#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask(LBi:UBi,LBj:UBj)
#   if defined SOLAR_SOURCE && defined WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:UBi,LBj:UBj)
#   endif
#  endif
      real(r8), intent(in), TARGET :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in), TARGET :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in), TARGET :: Hz(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in), TARGET :: Huon(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in), TARGET :: Hvom(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in), TARGET :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:N(ng))
      real(r8), intent(in), TARGET :: btflx(LBi:UBi,LBj:UBj,NT(ng))
      real(r8), intent(in), TARGET :: bustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in), TARGET :: bvstr(LBi:UBi,LBj:UBj)
      real(r8), intent(in), TARGET :: stflx(LBi:UBi,LBj:UBj,NT(ng))
      real(r8), intent(in), TARGET :: sustr(LBi:UBi,LBj:UBj)
      real(r8), intent(in), TARGET :: svstr(LBi:UBi,LBj:UBj)
#  ifdef SOLAR_SOURCE
      real(r8), intent(in) :: srflx(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in), TARGET :: Akt(LBi:UBi,LBj:UBj,0:N(ng),NAT)
      real(r8), intent(in), TARGET :: Akv(LBi:UBi,LBj:UBj,0:N(ng))
#  ifdef LMD_NONLOCAL
      real(r8), intent(in) :: ghats(LBi:UBi,LBj:UBj,0:N(ng),NAT)
#  endif
      real(r8), intent(in), TARGET :: W(LBi:UBi,LBj:UBj,0:N(ng))
#  ifdef WEC_VF
      real(r8), intent(in) :: W_stokes(LBi:UBi,LBj:UBj,0:N(ng))
#  endif
      real(r8), intent(in), TARGET :: ru(LBi:UBi,LBj:UBj,0:N(ng),2)
      real(r8), intent(in), TARGET :: rv(LBi:UBi,LBj:UBj,0:N(ng),2)
#  ifdef DIAGNOSTICS_TS
      real(r8), intent(inout), TARGET :: DiaTwrk(LBi:UBi,LBj:UBj,N(ng),NT(ng),  &
     &                                   NDT)
#  endif
#  ifdef DIAGNOSTICS_UV
      real(r8), intent(inout), TARGET :: DiaU3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d)
      real(r8), intent(inout), TARGET :: DiaV3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d)
      real(r8), intent(inout), TARGET :: DiaRU(LBi:UBi,LBj:UBj,N(ng),2,NDrhs)
      real(r8), intent(inout), TARGET :: DiaRV(LBi:UBi,LBj:UBj,N(ng),2,NDrhs)
#  endif
      real(r8), intent(inout), TARGET :: t(LBi:UBi,LBj:UBj,N(ng),3,NT(ng))
      real(r8), intent(inout), TARGET :: u(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(inout), TARGET :: v(LBi:UBi,LBj:UBj,N(ng),2)
# endif
!
!  Local variable declarations.
!
      integer :: Isrc, Jsrc
      integer :: i, ic, indx, is, itrc, j, k, ltrc
# if defined AGE_MEAN && defined T_PASSIVE
      integer :: iage
# endif
# if defined DIAGNOSTICS_TS || defined DIAGNOSTICS_UV
      integer :: idiag
# endif
      real(r8), parameter :: eps = 1.0E-16_r8

      real(r8) :: cff, cff1, cff2, cff3, cff4
      real(r8) :: Gamma

      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: CF
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: DC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC

# ifdef SOLAR_SOURCE
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,0:N(ng)) :: swdk
# endif
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: FE
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: FX
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: curv
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: grad
# if defined ROMS_KOKKOS && defined KK_STAGE_PRE3D
      TYPE(kokkos_pre3d_config) :: kk_cfg
      integer(C_INT) :: kk_hadv(NT(ng)), kk_vadv(NT(ng))
      integer, save :: kk_fallback_calls=0
      logical :: kk_fast
# endif

# include "set_bounds.h"
# if defined ROMS_KOKKOS && defined KK_STAGE_PRE3D
!
!-----------------------------------------------------------------------
!  Kokkos path: the tracer and 3D momentum predictor loops below live in
!  ROMS/Cpp/kokkos_step3d.cpp.  It is taken for the whole tile when
!  every tracer uses a scheme the kernel implements, there are no
!  tracer point sources and at most 32 tracers (the kernel's scheme
!  table); otherwise the Fortran loops run.  The lateral boundary
!  conditions and exchanges at the end run in both cases.
!-----------------------------------------------------------------------
!
      kk_fast=(.not.LuvSrc(ng)).and.(N(ng).ge.3).and.(NT(ng).le.32)
      DO itrc=1,NT(ng)
        kk_hadv(itrc)=0
        IF (Hadvection(itrc,ng)%CENTERED2) THEN
          kk_hadv(itrc)=1
        ELSE IF ((Hadvection(itrc,ng)%MPDATA).or.                       &
     &           (Hadvection(itrc,ng)%HSIMT)) THEN
          kk_hadv(itrc)=2
        ELSE IF (Hadvection(itrc,ng)%UPSTREAM3) THEN
          kk_hadv(itrc)=3
        ELSE IF (Hadvection(itrc,ng)%AKIMA4) THEN
          kk_hadv(itrc)=4
        ELSE IF ((Hadvection(itrc,ng)%CENTERED4).or.                    &
     &           (Hadvection(itrc,ng)%SPLIT_U3)) THEN
          kk_hadv(itrc)=5
        END IF
        kk_vadv(itrc)=0
        IF (Vadvection(itrc,ng)%SPLINES) THEN
          kk_vadv(itrc)=6
        ELSE IF (Vadvection(itrc,ng)%AKIMA4) THEN
          kk_vadv(itrc)=4
        ELSE IF (Vadvection(itrc,ng)%CENTERED2) THEN
          kk_vadv(itrc)=1
        ELSE IF ((Vadvection(itrc,ng)%MPDATA).or.                       &
     &           (Vadvection(itrc,ng)%HSIMT)) THEN
          kk_vadv(itrc)=2
        ELSE IF ((Vadvection(itrc,ng)%CENTERED4).or.                    &
     &           (Vadvection(itrc,ng)%SPLIT_U3)) THEN
          kk_vadv(itrc)=5
        END IF
        IF ((kk_hadv(itrc).eq.0).or.(kk_vadv(itrc).eq.0)) THEN
          kk_fast=.FALSE.
        END IF
      END DO
      IF (kk_fast) THEN
        kk_cfg%LBi=LBi
        kk_cfg%UBi=UBi
        kk_cfg%LBj=LBj
        kk_cfg%UBj=UBj
        kk_cfg%Istr=Istr
        kk_cfg%Iend=Iend
        kk_cfg%Jstr=Jstr
        kk_cfg%Jend=Jend
        kk_cfg%IstrU=IstrU
        kk_cfg%JstrV=JstrV
        kk_cfg%N=N(ng)
        kk_cfg%NT=NT(ng)
        kk_cfg%NAT=NAT
        kk_cfg%nrhs=nrhs
        kk_cfg%nstp=nstp
        kk_cfg%nnew=nnew
        IF (iic(ng).eq.ntfirst(ng)) THEN
          kk_cfg%step=0
        ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
          kk_cfg%step=1
        ELSE
          kk_cfg%step=2
        END IF
#  ifdef DIAGNOSTICS_TS
        kk_cfg%NDT=NDT
        kk_cfg%iTrate=iTrate
        kk_cfg%iTvdif=iTvdif
#  else
        kk_cfg%NDT=0
        kk_cfg%iTrate=0
        kk_cfg%iTvdif=0
#  endif
#  ifdef DIAGNOSTICS_UV
        kk_cfg%NDM3d=NDM3d
        kk_cfg%NDrhs=NDrhs
        kk_cfg%M3pgrd=M3pgrd
        kk_cfg%M3vvis=M3vvis
        kk_cfg%M3rate=M3rate
#  else
        kk_cfg%NDM3d=0
        kk_cfg%NDrhs=0
        kk_cfg%M3pgrd=0
        kk_cfg%M3vvis=0
        kk_cfg%M3rate=0
#  endif
        kk_cfg%fold_w=MERGE(1,0,                                        &
     &    (.not.(CompositeGrid(iwest ,ng).or.EWperiodic(ng))).and.      &
     &    DOMAIN(ng)%Western_Edge (tile))
        kk_cfg%fold_e=MERGE(1,0,                                        &
     &    (.not.(CompositeGrid(ieast ,ng).or.EWperiodic(ng))).and.      &
     &    DOMAIN(ng)%Eastern_Edge (tile))
        kk_cfg%fold_s=MERGE(1,0,                                        &
     &    (.not.(CompositeGrid(isouth,ng).or.NSperiodic(ng))).and.      &
     &    DOMAIN(ng)%Southern_Edge(tile))
        kk_cfg%fold_n=MERGE(1,0,                                        &
     &    (.not.(CompositeGrid(inorth,ng).or.NSperiodic(ng))).and.      &
     &    DOMAIN(ng)%Northern_Edge(tile))
        kk_cfg%dt=dt(ng)
        kk_cfg%lambda=lambda
#  ifndef TS_FIXED
        CALL k_pre_step3d_t (kk_cfg, kk_hadv, kk_vadv,                  &
     &                       C_LOC(pm), C_LOC(pn), C_LOC(Hz),           &
     &                       C_LOC(Huon), C_LOC(Hvom), C_LOC(z_r),      &
     &                       C_LOC(W), C_LOC(Akt), C_LOC(btflx),        &
     &                       C_LOC(stflx), C_LOC(t),                    &
#   ifdef DIAGNOSTICS_TS
     &                       C_LOC(DiaTwrk))
#   else
     &                       C_LOC(t))
#   endif
#  endif
        CALL k_pre_step3d_uv (kk_cfg, C_LOC(pm), C_LOC(pn), C_LOC(Hz),  &
     &                        C_LOC(z_r), C_LOC(Akv),                   &
     &                        C_LOC(bustr), C_LOC(bvstr),               &
     &                        C_LOC(sustr), C_LOC(svstr),               &
     &                        C_LOC(ru), C_LOC(rv), C_LOC(u), C_LOC(v), &
#  ifdef DIAGNOSTICS_UV
     &                        C_LOC(DiaU3wrk), C_LOC(DiaV3wrk),         &
     &                        C_LOC(DiaRU), C_LOC(DiaRV))
#  else
     &                        C_LOC(u), C_LOC(v), C_LOC(u), C_LOC(v))
#  endif
      ELSE
        kk_fallback_calls=kk_fallback_calls+1
        IF (kk_fallback_calls.eq.1) THEN
          WRITE (*,'(a)') 'pre_step3d: whole-tile Fortran fallback '//  &
     &      '(point sources, scheme, levels or tracer count)'
        END IF
# endif /* ROMS_KOKKOS && KK_STAGE_PRE3D */
```

</details>

<a id="src-caller-pre-common-epilogue"></a>

**source-block ID：`caller-pre-common-epilogue`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Nonlinear/pre_step3d.F`；闭区间：**1291–1331**；行数：**41**；UTF-8 payload SHA256：`df2ebcfd7c843289b6e2e0ee71cb2a5fd8e25fa9f410e10dd84d124a22d8302e`。

<details>
<summary>ROMS/Nonlinear/pre_step3d.F，1291–1331（完整原文，可展开）</summary>

<!-- source-block id=caller-pre-common-epilogue commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Nonlinear/pre_step3d.F lines=1291-1331 count=41 sha256=df2ebcfd7c843289b6e2e0ee71cb2a5fd8e25fa9f410e10dd84d124a22d8302e -->
```fortran
# if defined ROMS_KOKKOS && defined KK_STAGE_PRE3D
      END IF
# endif

# ifndef TS_FIXED
!
!=======================================================================
!  Apply intermediate tracers lateral boundary conditions.
!=======================================================================
!
      ic=0
      DO itrc=1,NT(ng)
        IF (LtracerCLM(itrc,ng).and.LnudgeTCLM(itrc,ng)) THEN
          ic=ic+1
        END IF
        CALL t3dbc_tile (ng, tile, itrc, ic,                            &
     &                   LBi, UBi, LBj, UBj, N(ng), NT(ng),             &
     &                   IminS, ImaxS, JminS, JmaxS,                    &
     &                   nstp, 3,                                       &
     &                   t)

        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_r3d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj, 1, N(ng),         &
     &                            t(:,:,:,3,itrc))
        END IF
      END DO

#  ifdef DISTRIBUTE
      CALL mp_exchange4d (ng, tile, iNLM, 1,                            &
     &                    LBi, UBi, LBj, UBj, 1, N(ng), 1, NT(ng),      &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    t(:,:,:,3,:))
#  endif
# endif
!
      RETURN
      END SUBROUTINE pre_step3d_tile
#endif
      END MODULE pre_step3d_mod
```

</details>

### 13.3 当前 STEP3DUV 调用端与两入口之间的真实状态变更

第一个块含完整声明、诊断 pair 与 rep 开关、solve 调用及 `#else`；第二块从原循环门禁收尾开始，完整保留 BC、动量点源和 couple 调用；第三块保留 couple 门禁结束后的所有 exchange/uv_C2A_grid。未改写通信次序，也未假定 solve 后先做 halo exchange。

<a id="src-caller-uv-declarations-solve"></a>

**source-block ID：`caller-uv-declarations-solve`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Nonlinear/step3d_uv.F`；闭区间：**1–463**；行数：**463**；UTF-8 payload SHA256：`b39ac635e1c3592adcacb9778cc95b0d0a558e7525a5583b469e6419415acedd`。

<details>
<summary>ROMS/Nonlinear/step3d_uv.F，1–463（完整原文，可展开）</summary>

<!-- source-block id=caller-uv-declarations-solve commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Nonlinear/step3d_uv.F lines=1-463 count=463 sha256=b39ac635e1c3592adcacb9778cc95b0d0a558e7525a5583b469e6419415acedd -->
```fortran
#include "cppdefs.h"
      MODULE step3d_uv_mod

#if defined NONLINEAR && defined SOLVE3D
!
!git $Id$
!=======================================================================
!  Copyright (c) 2002-2025 The ROMS Group                              !
!    Licensed under a MIT/X style license           Hernan G. Arango   !
!    See License_ROMS.md                    Alexander F. Shchepetkin   !
!==================================================== John C. Warner ===
!                                                                      !
!  This subroutine time-steps the nonlinear  horizontal  momentum      !
!  equations. The vertical viscosity terms are time-stepped using      !
!  an implicit algorithm.                                              !
!                                                                      !
!=======================================================================
!
      USE mod_param
      USE mod_coupling
# ifdef DIAGNOSTICS_UV
      USE mod_diags
# endif
      USE mod_forces
      USE mod_grid
      USE mod_mixing
      USE mod_ncparam
      USE mod_ocean
      USE mod_scalars
      USE mod_sources
!
      USE exchange_2d_mod
      USE exchange_3d_mod
# ifdef DISTRIBUTE
      USE mp_exchange_mod,   ONLY : mp_exchange2d, mp_exchange3d
# endif
      USE uv_var_change_mod, ONLY : uv_C2A_grid
      USE u3dbc_mod,         ONLY : u3dbc_tile
      USE v3dbc_mod,         ONLY : v3dbc_tile
# if defined ROMS_KOKKOS && defined KK_STAGE_STEP3DUV
      USE, INTRINSIC :: iso_c_binding, ONLY : C_LOC
      USE kokkos_step3d_mod, ONLY : kokkos_uv3d_config,                 &
     &                              k_step3d_uv_solve,                  &
     &                              k_step3d_uv_couple
# endif
!
      implicit none
!
      PRIVATE
      PUBLIC  :: step3d_uv
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE step3d_uv (ng, tile)
!***********************************************************************
!
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
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 34, __LINE__, MyFile)
# endif
      CALL step3d_uv_tile (ng, tile,                                    &
     &                     LBi, UBi, LBj, UBj,                          &
     &                     IminS, ImaxS, JminS, JmaxS,                  &
     &                     nrhs(ng), nstp(ng), nnew(ng),                &
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
     &                     knew(ng),                                    &
# endif
# ifdef MASKING
     &                     GRID(ng) % umask,                            &
     &                     GRID(ng) % vmask,                            &
# endif
# ifdef WET_DRY
     &                     GRID(ng) % umask_wet,                        &
     &                     GRID(ng) % vmask_wet,                        &
# endif
     &                     GRID(ng) % om_v,                             &
     &                     GRID(ng) % on_u,                             &
# ifdef OMEGA_IMPLICIT
     &                     GRID(ng) % om_u,                             &
     &                     GRID(ng) % on_v,                             &
# endif
     &                     GRID(ng) % pm,                               &
     &                     GRID(ng) % pn,                               &
     &                     GRID(ng) % Hz,                               &
     &                     GRID(ng) % z_r,                              &
     &                     GRID(ng) % z_w,                              &
     &                     MIXING(ng) % Akv,                            &
     &                     COUPLING(ng) % DU_avg1,                      &
     &                     COUPLING(ng) % DV_avg1,                      &
     &                     COUPLING(ng) % DU_avg2,                      &
     &                     COUPLING(ng) % DV_avg2,                      &
     &                     OCEAN(ng) % ru,                              &
     &                     OCEAN(ng) % rv,                              &
# ifdef DIAGNOSTICS_UV
     &                     DIAGS(ng) % DiaU2wrk,                        &
     &                     DIAGS(ng) % DiaV2wrk,                        &
     &                     DIAGS(ng) % DiaU2int,                        &
     &                     DIAGS(ng) % DiaV2int,                        &
     &                     DIAGS(ng) % DiaU3wrk,                        &
     &                     DIAGS(ng) % DiaV3wrk,                        &
     &                     DIAGS(ng) % DiaRU,                           &
     &                     DIAGS(ng) % DiaRV,                           &
# endif
     &                     OCEAN(ng) % u,                               &
     &                     OCEAN(ng) % v,                               &
     &                     OCEAN(ng) % ubar,                            &
     &                     OCEAN(ng) % vbar,                            &
# ifdef WEC
     &                     OCEAN(ng) % ubar_stokes,                     &
     &                     OCEAN(ng) % vbar_stokes,                     &
     &                     OCEAN(ng) % u_stokes,                        &
     &                     OCEAN(ng) % v_stokes,                        &
# endif
# ifdef OMEGA_IMPLICIT
     &                     OCEAN(ng) % Wi,                              &
# endif
     &                     GRID(ng) % Huon,                             &
     &                     GRID(ng) % Hvom)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 34, __LINE__, MyFile)
# endif
!
      RETURN
      END SUBROUTINE step3d_uv
!
!***********************************************************************
      SUBROUTINE step3d_uv_tile (ng, tile,                              &
     &                           LBi, UBi, LBj, UBj,                    &
     &                           IminS, ImaxS, JminS, JmaxS,            &
     &                           nrhs, nstp, nnew,                      &
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
     &                           knew,                                  &
# endif
# ifdef MASKING
     &                           umask, vmask,                          &
# endif
# ifdef WET_DRY
     &                           umask_wet, vmask_wet,                  &
# endif
     &                           om_v, on_u,                            &
# ifdef OMEGA_IMPLICIT
     &                           om_u, on_v,                            &
# endif
     &                           pm, pn,                                &
     &                           Hz, z_r, z_w,                          &
     &                           Akv,                                   &
     &                           DU_avg1, DV_avg1,                      &
     &                           DU_avg2, DV_avg2,                      &
     &                           ru, rv,                                &
# ifdef DIAGNOSTICS_UV
     &                           DiaU2wrk, DiaV2wrk,                    &
     &                           DiaU2int, DiaV2int,                    &
     &                           DiaU3wrk, DiaV3wrk,                    &
     &                           DiaRU,    DiaRV,                       &
# endif
     &                           u, v,                                  &
     &                           ubar, vbar,                            &
# ifdef WEC
     &                           ubar_stokes, vbar_stokes,              &
     &                           u_stokes, v_stokes,                    &
# endif
# ifdef OMEGA_IMPLICIT
     &                           Wi,                                    &
# endif
     &                           Huon, Hvom)
!***********************************************************************
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nrhs, nstp, nnew
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
      integer, intent(in) :: knew
# endif
!
# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: umask_wet(LBi:,LBj:)
      real(r8), intent(in) :: vmask_wet(LBi:,LBj:)
#  endif
      real(r8), intent(in), TARGET :: om_v(LBi:,LBj:)
      real(r8), intent(in), TARGET :: on_u(LBi:,LBj:)
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: om_u(LBi:,LBj:)
      real(r8), intent(in) :: on_v(LBi:,LBj:)
#  endif
      real(r8), intent(in), TARGET :: pm(LBi:,LBj:)
      real(r8), intent(in), TARGET :: pn(LBi:,LBj:)
      real(r8), intent(in), TARGET :: Hz(LBi:,LBj:,:)
      real(r8), intent(in), TARGET :: z_r(LBi:,LBj:,:)
      real(r8), intent(in) :: z_w(LBi:,LBj:,0:)
      real(r8), intent(in), TARGET :: Akv(LBi:,LBj:,0:)
      real(r8), intent(in), TARGET :: DU_avg1(LBi:,LBj:)
      real(r8), intent(in), TARGET :: DV_avg1(LBi:,LBj:)
      real(r8), intent(in), TARGET :: DU_avg2(LBi:,LBj:)
      real(r8), intent(in), TARGET :: DV_avg2(LBi:,LBj:)
#  ifdef WEC
      real(r8), intent(in) :: ubar_stokes(LBi:,LBj:)
      real(r8), intent(in) :: vbar_stokes(LBi:,LBj:)
#  endif
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: Wi(LBi:,LBj:,0:)
#  endif
      real(r8), intent(inout), TARGET :: ru(LBi:,LBj:,0:,:)
      real(r8), intent(inout), TARGET :: rv(LBi:,LBj:,0:,:)
#  ifdef DIAGNOSTICS_UV
      real(r8), intent(inout), TARGET :: DiaU2wrk(LBi:,LBj:,:)
      real(r8), intent(inout), TARGET :: DiaV2wrk(LBi:,LBj:,:)
      real(r8), intent(inout), TARGET :: DiaU2int(LBi:,LBj:,:)
      real(r8), intent(inout), TARGET :: DiaV2int(LBi:,LBj:,:)
      real(r8), intent(inout), TARGET :: DiaU3wrk(LBi:,LBj:,:,:)
      real(r8), intent(inout), TARGET :: DiaV3wrk(LBi:,LBj:,:,:)
      real(r8), intent(inout), TARGET :: DiaRU(LBi:,LBj:,:,:,:)
      real(r8), intent(inout), TARGET :: DiaRV(LBi:,LBj:,:,:,:)
#  endif
      real(r8), intent(inout), TARGET :: u(LBi:,LBj:,:,:)
      real(r8), intent(inout), TARGET :: v(LBi:,LBj:,:,:)
#  ifdef WEC
      real(r8), intent(inout) :: u_stokes(LBi:,LBj:,:)
      real(r8), intent(inout) :: v_stokes(LBi:,LBj:,:)
#  endif
      real(r8), intent(out), TARGET :: ubar(LBi:,LBj:,:)
      real(r8), intent(out), TARGET :: vbar(LBi:,LBj:,:)
      real(r8), intent(out), TARGET :: Huon(LBi:,LBj:,:)
      real(r8), intent(out), TARGET :: Hvom(LBi:,LBj:,:)

# else

#  ifdef MASKING
      real(r8), intent(in) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask(LBi:UBi,LBj:UBj)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: umask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask_wet(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in), TARGET :: om_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in), TARGET :: on_u(LBi:UBi,LBj:UBj)
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: om_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_v(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in), TARGET :: pm(LBi:UBi,LBj:UBj)
      real(r8), intent(in), TARGET :: pn(LBi:UBi,LBj:UBj)
      real(r8), intent(in), TARGET :: Hz(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in), TARGET :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(in) :: z_w(LBi:UBi,LBj:UBj,0:N(ng))
      real(r8), intent(in), TARGET :: Akv(LBi:UBi,LBj:UBj,0:N(ng))
      real(r8), intent(in), TARGET :: DU_avg1(LBi:UBi,LBj:UBj)
      real(r8), intent(in), TARGET :: DV_avg1(LBi:UBi,LBj:UBj)
      real(r8), intent(in), TARGET :: DU_avg2(LBi:UBi,LBj:UBj)
      real(r8), intent(in), TARGET :: DV_avg2(LBi:UBi,LBj:UBj)
#  ifdef WEC
      real(r8), intent(in) :: ubar_stokes(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vbar_stokes(LBi:UBi,LBj:UBj)
#  endif
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: Wi(LBi:UBi,LBj:UBj,0:N(ng))
#  endif
      real(r8), intent(inout), TARGET :: ru(LBi:UBi,LBj:UBj,0:N(ng),2)
      real(r8), intent(inout), TARGET :: rv(LBi:UBi,LBj:UBj,0:N(ng),2)
#  ifdef DIAGNOSTICS_UV
      real(r8), intent(inout), TARGET :: DiaU2wrk(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout), TARGET :: DiaV2wrk(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout), TARGET :: DiaU2int(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout), TARGET :: DiaV2int(LBi:UBi,LBj:UBj,NDM2d)
      real(r8), intent(inout), TARGET :: DiaU3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d)
      real(r8), intent(inout), TARGET :: DiaV3wrk(LBi:UBi,LBj:UBj,N(ng),NDM3d)
      real(r8), intent(inout), TARGET :: DiaRU(LBi:UBi,LBj:UBj,N(ng),2,NDrhs)
      real(r8), intent(inout), TARGET :: DiaRV(LBi:UBi,LBj:UBj,N(ng),2,NDrhs)
#  endif
      real(r8), intent(inout), TARGET :: u(LBi:UBi,LBj:UBj,N(ng),2)
      real(r8), intent(inout), TARGET :: v(LBi:UBi,LBj:UBj,N(ng),2)
#  ifdef WEC
      real(r8), intent(inout) :: u_stokes(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(inout) :: v_stokes(LBi:UBi,LBj:UBj,N(ng))
#  endif
      real(r8), intent(out), TARGET :: ubar(LBi:UBi,LBj:UBj,:)
      real(r8), intent(out), TARGET :: vbar(LBi:UBi,LBj:UBj,:)
      real(r8), intent(out), TARGET :: Huon(LBi:UBi,LBj:UBj,N(ng))
      real(r8), intent(out), TARGET :: Hvom(LBi:UBi,LBj:UBj,N(ng))
# endif
!
!  Local variable declarations.
!
      integer :: i, idiag, is, j, k
!
      real(r8) :: cff, cff1, cff2
!
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: AK
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: BC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: CF
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: DC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC
# ifdef OMEGA_IMPLICIT
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FCmin
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FCmax
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: WK
# endif
# ifdef WEC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: CFs
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: DCs
# endif
      real(r8), dimension(IminS:ImaxS,N(ng)) :: Hzk
      real(r8), dimension(IminS:ImaxS,N(ng)) :: oHz
# ifdef DIAGNOSTICS_UV
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: wrk
      real(r8), dimension(IminS:ImaxS,1:NDM2d-1) :: Dwrk
# endif
# if defined ROMS_KOKKOS && defined KK_STAGE_STEP3DUV
      TYPE(kokkos_uv3d_config) :: kk_cfg
      integer :: ic
# endif

# include "set_bounds.h"
# if defined ROMS_KOKKOS && defined KK_STAGE_STEP3DUV
!
!  Kokkos path: the two row loops of this routine (time step plus
!  implicit vertical viscosity, and the 2D/3D coupling with the mass
!  fluxes) live in ROMS/Cpp/kokkos_step3d.cpp.  The lateral boundary
!  conditions, point sources and exchanges between and after them stay
!  here.
!
      kk_cfg%LBi=LBi
      kk_cfg%UBi=UBi
      kk_cfg%LBj=LBj
      kk_cfg%UBj=UBj
      kk_cfg%Istr=Istr
      kk_cfg%Iend=Iend
      kk_cfg%Jstr=Jstr
      kk_cfg%Jend=Jend
      kk_cfg%IstrU=IstrU
      kk_cfg%JstrV=JstrV
      kk_cfg%IstrP=IstrP
      kk_cfg%IstrT=IstrT
      kk_cfg%IendT=IendT
      kk_cfg%JstrT=JstrT
      kk_cfg%JendT=JendT
      kk_cfg%Mm=Mm(ng)
      kk_cfg%N=N(ng)
      kk_cfg%nrhs=nrhs
      kk_cfg%nnew=nnew
      IF (iic(ng).eq.ntfirst(ng)) THEN
        kk_cfg%step=0
      ELSE IF (iic(ng).eq.(ntfirst(ng)+1)) THEN
        kk_cfg%step=1
      ELSE
        kk_cfg%step=2
      END IF
      kk_cfg%NDM2d=0
      kk_cfg%NDM3d=0
      kk_cfg%NDrhs=0
      kk_cfg%M2pgrd=0
      kk_cfg%M2bstr=0
      kk_cfg%M2sstr=0
      kk_cfg%M2rate=0
      kk_cfg%M3pgrd=0
      kk_cfg%M3vvis=0
      kk_cfg%M3rate=0
      kk_cfg%M3xvis=0
      kk_cfg%M3yvis=0
      kk_cfg%M3hvis=0
      kk_cfg%m2=0
      kk_cfg%m3=0
      ic=0
#  ifdef DIAGNOSTICS_UV
      kk_cfg%NDM2d=NDM2d
      kk_cfg%NDM3d=NDM3d
      kk_cfg%NDrhs=NDrhs
      kk_cfg%M2pgrd=M2pgrd
      kk_cfg%M2bstr=M2bstr
      kk_cfg%M2sstr=M2sstr
      kk_cfg%M2rate=M2rate
      kk_cfg%M3pgrd=M3pgrd
      kk_cfg%M3vvis=M3vvis
      kk_cfg%M3rate=M3rate
!
!  Terms integrated over the column for the interior mean replacement,
!  as (2-D index, 3-D index) pairs.
!
      ic=ic+1
      kk_cfg%m2(ic)=M2pgrd
      kk_cfg%m3(ic)=M3pgrd
      ic=ic+1
      kk_cfg%m2(ic)=M2bstr
      kk_cfg%m3(ic)=M3vvis
#   ifdef UV_COR
      ic=ic+1
      kk_cfg%m2(ic)=M2fcor
      kk_cfg%m3(ic)=M3fcor
#   endif
#   if defined UV_VIS2 || defined UV_VIS4
      kk_cfg%M3xvis=M3xvis
      kk_cfg%M3yvis=M3yvis
      kk_cfg%M3hvis=M3hvis
      ic=ic+1
      kk_cfg%m2(ic)=M2xvis
      kk_cfg%m3(ic)=M3xvis
      ic=ic+1
      kk_cfg%m2(ic)=M2yvis
      kk_cfg%m3(ic)=M3yvis
      ic=ic+1
      kk_cfg%m2(ic)=M2hvis
      kk_cfg%m3(ic)=M3hvis
#   endif
#   ifdef UV_ADV
      ic=ic+1
      kk_cfg%m2(ic)=M2xadv
      kk_cfg%m3(ic)=M3xadv
      ic=ic+1
      kk_cfg%m2(ic)=M2yadv
      kk_cfg%m3(ic)=M3yadv
      ic=ic+1
      kk_cfg%m2(ic)=M2hadv
      kk_cfg%m3(ic)=M3hadv
#   endif
#  endif
      kk_cfg%npairs=ic
      kk_cfg%rep_w=MERGE(1,0,                                           &
     &  (.not.(CompositeGrid(iwest ,ng).or.EWperiodic(ng))).and.        &
     &  DOMAIN(ng)%Western_Edge(tile))
      kk_cfg%rep_e=MERGE(1,0,                                           &
     &  (.not.(CompositeGrid(ieast ,ng).or.EWperiodic(ng))).and.        &
     &  DOMAIN(ng)%Eastern_Edge(tile))
      kk_cfg%rep_s=MERGE(1,0,                                           &
     &  .not.(CompositeGrid(isouth,ng).or.NSperiodic(ng)))
      kk_cfg%rep_n=MERGE(1,0,                                           &
     &  .not.(CompositeGrid(inorth,ng).or.NSperiodic(ng)))
      kk_cfg%dt=dt(ng)
      kk_cfg%lambda=lambda
      CALL k_step3d_uv_solve (kk_cfg, C_LOC(pm), C_LOC(pn),             &
     &                        C_LOC(on_u), C_LOC(om_v),                 &
     &                        C_LOC(Hz), C_LOC(z_r), C_LOC(Akv),        &
     &                        C_LOC(DU_avg1), C_LOC(DV_avg1),           &
     &                        C_LOC(ru), C_LOC(rv), C_LOC(u), C_LOC(v), &
#  ifdef DIAGNOSTICS_UV
     &                        C_LOC(DiaU2wrk), C_LOC(DiaV2wrk),         &
     &                        C_LOC(DiaU3wrk), C_LOC(DiaV3wrk),         &
     &                        C_LOC(DiaRU), C_LOC(DiaRV))
#  else
     &                        C_LOC(u), C_LOC(v), C_LOC(u), C_LOC(v),   &
     &                        C_LOC(u), C_LOC(v))
#  endif
# else
```

</details>

<a id="src-caller-uv-bc-sources-couple"></a>

**source-block ID：`caller-uv-bc-sources-couple`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Nonlinear/step3d_uv.F`；闭区间：**1400–1468**；行数：**69**；UTF-8 payload SHA256：`cb2ea50ce18091c0c41b613bf94f2200cad9e782158752d2416f1b60f84be320`。

<details>
<summary>ROMS/Nonlinear/step3d_uv.F，1400–1468（完整原文，可展开）</summary>

<!-- source-block id=caller-uv-bc-sources-couple commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Nonlinear/step3d_uv.F lines=1400-1468 count=69 sha256=cb2ea50ce18091c0c41b613bf94f2200cad9e782158752d2416f1b60f84be320 -->
```fortran
# endif /* ROMS_KOKKOS && KK_STAGE_STEP3DUV */
!
!-----------------------------------------------------------------------
!  Set lateral boundary conditions.
!-----------------------------------------------------------------------
!
      CALL u3dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj, N(ng),                       &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 nstp, nnew,                                      &
     &                 u)
      CALL v3dbc_tile (ng, tile,                                        &
     &                 LBi, UBi, LBj, UBj, N(ng),                       &
     &                 IminS, ImaxS, JminS, JmaxS,                      &
     &                 nstp, nnew,                                      &
     &                 v)
!
!-----------------------------------------------------------------------
!  Apply momentum transport point sources (like river runoff), if any.
!-----------------------------------------------------------------------
!
      IF (LuvSrc(ng)) THEN
        DO is=1,Nsrc(ng)
          i=SOURCES(ng)%Isrc(is)
          j=SOURCES(ng)%Jsrc(is)
          IF (((IstrR.le.i).and.(i.le.IendR)).and.                      &
     &        ((JstrR.le.j).and.(j.le.JendR))) THEN
            IF (INT(SOURCES(ng)%Dsrc(is)).eq.0) THEN
              DO k=1,N(ng)
                cff1=1.0_r8/(on_u(i,j)*                                 &
     &                       0.5_r8*(z_w(i-1,j,k)-z_w(i-1,j,k-1)+       &
     &                               z_w(i  ,j,k)-z_w(i  ,j,k-1)))
                u(i,j,k,nnew)=SOURCES(ng)%Qsrc(is,k)*cff1
              END DO
            ELSE IF (INT(SOURCES(ng)%Dsrc(is)).eq.1) THEN
              DO k=1,N(ng)
                cff1=1.0_r8/(om_v(i,j)*                                 &
     &                       0.5_r8*(z_w(i,j-1,k)-z_w(i,j-1,k-1)+       &
     &                               z_w(i,j  ,k)-z_w(i,j  ,k-1)))
                v(i,j,k,nnew)=SOURCES(ng)%Qsrc(is,k)*cff1
              END DO
            END IF
          END IF
        END DO
      END IF
!
!-----------------------------------------------------------------------
!  Couple 2D and 3D momentum equations.
!-----------------------------------------------------------------------
!
!  Couple velocity component in the XI-direction.
!
# if defined ROMS_KOKKOS && defined KK_STAGE_STEP3DUV
      CALL k_step3d_uv_couple (kk_cfg, C_LOC(on_u), C_LOC(om_v),        &
     &                         C_LOC(Hz),                               &
     &                         C_LOC(DU_avg1), C_LOC(DU_avg2),          &
     &                         C_LOC(DV_avg1), C_LOC(DV_avg2),          &
     &                         C_LOC(u), C_LOC(v),                      &
     &                         C_LOC(ubar), C_LOC(vbar),                &
     &                         C_LOC(Huon), C_LOC(Hvom),                &
#  ifdef DIAGNOSTICS_UV
     &                         C_LOC(DiaU2wrk), C_LOC(DiaV2wrk),        &
     &                         C_LOC(DiaU2int), C_LOC(DiaV2int),        &
     &                         C_LOC(DiaU3wrk), C_LOC(DiaV3wrk))
#  else
     &                         C_LOC(u), C_LOC(v), C_LOC(u), C_LOC(v),  &
     &                         C_LOC(u), C_LOC(v))
#  endif
# else
```

</details>

<a id="src-caller-uv-common-exchange"></a>

**source-block ID：`caller-uv-common-exchange`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Nonlinear/step3d_uv.F`；闭区间：**1914–2004**；行数：**91**；UTF-8 payload SHA256：`1b0944949c61a4024402eea20ce8a5554f17dbb031bf030851b124e1dec75faf`。

<details>
<summary>ROMS/Nonlinear/step3d_uv.F，1914–2004（完整原文，可展开）</summary>

<!-- source-block id=caller-uv-common-exchange commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Nonlinear/step3d_uv.F lines=1914-2004 count=91 sha256=1b0944949c61a4024402eea20ce8a5554f17dbb031bf030851b124e1dec75faf -->
```fortran
# endif /* ROMS_KOKKOS && KK_STAGE_STEP3DUV */
!
!-----------------------------------------------------------------------
!  Exchange boundary data.
!-----------------------------------------------------------------------
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_u3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          u(:,:,:,nnew))
        CALL exchange_v3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          v(:,:,:,nnew))
#  ifdef WEC
        CALL exchange_u3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          u_stokes(:,:,:))
        CALL exchange_v3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          v_stokes(:,:,:))
#  endif
        CALL exchange_u3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          Huon)
        CALL exchange_v3d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),           &
     &                          Hvom)
!
# if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
        CALL exchange_u2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          ubar(:,:,knew))
        CALL exchange_v2d_tile (ng, tile,                               &
     &                          LBi, UBi, LBj, UBj,                     &
     &                          vbar(:,:,knew))
# else
        DO k=1,2
          CALL exchange_u2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            ubar(:,:,k))
          CALL exchange_v2d_tile (ng, tile,                             &
     &                            LBi, UBi, LBj, UBj,                   &
     &                            vbar(:,:,k))
        END DO
# endif
      END IF

# ifdef DISTRIBUTE
!
      CALL mp_exchange3d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    u(:,:,:,nnew), v(:,:,:,nnew),                 &
     &                    Huon, Hvom)
!
#  if defined STEP2D_FB_AB3_AM4 || defined STEP2D_FB_LF_AM3
      CALL mp_exchange2d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    ubar(:,:,knew), vbar(:,:,knew))
#  else
      CALL mp_exchange2d (ng, tile, iNLM, 4,                            &
     &                    LBi, UBi, LBj, UBj,                           &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    ubar(:,:,1), vbar(:,:,1),                     &
     &                    ubar(:,:,2), vbar(:,:,2))
#  endif
#  ifdef WEC
      CALL mp_exchange3d (ng, tile, iNLM, 2,                            &
     &                    LBi, UBi, LBj, UBj, 1, N(ng),                 &
     &                    NghostPoints,                                 &
     &                    EWperiodic(ng), NSperiodic(ng),               &
     &                    u_stokes(:,:,:), v_stokes(:,:,:))
#  endif
# endif
!
!-----------------------------------------------------------------------
!  Compute 3D momentum (ua, va) at RHO-points (A-Grid) for output
!  purposes and data assimilation where the observations and state
!  vector is located at the cell-center.
!-----------------------------------------------------------------------
!
      CALL uv_C2A_grid (ng, tile, iNLM, nnew)
!
      RETURN
      END SUBROUTINE step3d_uv_tile
#endif
      END MODULE step3d_uv_mod
```

</details>

### 13.4 当前 tracer 调用端与共同 epilogue

第一个连续块含当前完整模块/形参/数组声明、此前 prologue、条件编译、完整资格检查、scheme 配对、halo 与连续性检查、所有配置字段、HSIMT 前置 exchange、chunks 调用和日志以及 ELSE；第二块收录计算回退的 END IF 及共同尾段，确保年龄源、侧边界、nudging、最终诊断和通信没有被加速路径绕过。

<a id="src-caller-t-declarations-eligibility"></a>

**source-block ID：`caller-t-declarations-eligibility`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Nonlinear/step3d_t.F`；闭区间：**1–507**；行数：**507**；UTF-8 payload SHA256：`e3578d03fe22698687be8a5520e751f6b0d6fd0dffd5a56cfbacd408b65f4dd2`。

<details>
<summary>ROMS/Nonlinear/step3d_t.F，1–507（完整原文，可展开）</summary>

<!-- source-block id=caller-t-declarations-eligibility commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Nonlinear/step3d_t.F lines=1-507 count=507 sha256=e3578d03fe22698687be8a5520e751f6b0d6fd0dffd5a56cfbacd408b65f4dd2 -->
```fortran
#include "cppdefs.h"
      MODULE step3d_t_mod
#if !defined TS_FIXED && (defined NONLINEAR && defined SOLVE3D)
!
!git $Id$
!=======================================================================
!  Copyright (c) 2002-2025 The ROMS Group                              !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.md                            Hernan G. Arango   !
!========================================== Alexander F. Shchepetkin ===
!                                                                      !
!  This routine time-steps tracer equations.  Notice that advective    !
!  and diffusive terms are time-stepped differently. It applies the    !
!  corrector time-step for horizontal/vertical advection,  vertical    !
!  diffusion, nudging if necessary, and lateral boundary conditions.   !
!                                                                      !
!  A different horizontal/vertical advection scheme is allowed for     !
!  each tracer. If the MPDATA or HSIMT monotonic scheme, it is applied !
!  to both horizontal and vertical advective fluxes.                   !
!                                                                      !
!  Notice that at input the tracer arrays have:                        !
!                                                                      !
!    t(:,:,:,nnew,:)   m Tunits  n+1     horizontal/vertical diffusion !
!                                        terms plus source/sink terms  !
!                                        (biology, sediment), if any   !
!                                                                      !
!    t(:,:,:,3   ,:)   Tunits    n+1/2   advective terms and vertical  !
!                                        diffusion predictor step      !
!                                                                      !
!=======================================================================
!
      implicit none
!
      PRIVATE
      PUBLIC  :: step3d_t
!
      CONTAINS
!
!***********************************************************************
      SUBROUTINE step3d_t (ng, tile)
!***********************************************************************
!
      USE mod_param
# ifdef DIAGNOSTICS_TS
      USE mod_diags
# endif
      USE mod_grid
      USE mod_mixing
      USE mod_ocean
# if defined SEDIMENT && defined SED_MORPH
      USE mod_sedbed
# endif
      USE mod_stepping
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
!
!  Local variable declarations.
!
      character (len=*), parameter :: MyFile = &
     &  __FILE__
!
# include "tile.h"
!
# ifdef PROFILE
      CALL wclock_on (ng, iNLM, 35, __LINE__, MyFile)
# endif
      CALL step3d_t_tile (ng, tile, &
     &                    LBi, UBi, LBj, UBj, &
     &                    IminS, ImaxS, JminS, JmaxS, &
     &                    nrhs(ng), nstp(ng), nnew(ng), &
# ifdef MASKING
     &                    GRID(ng) % rmask, &
     &                    GRID(ng) % umask, &
     &                    GRID(ng) % vmask, &
# endif
# ifdef WET_DRY
     &                    GRID(ng) % rmask_wet, &
     &                    GRID(ng) % umask_wet, &
     &                    GRID(ng) % vmask_wet, &
# endif
     &                    GRID(ng) % omn, &
     &                    GRID(ng) % om_u, &
     &                    GRID(ng) % om_v, &
     &                    GRID(ng) % on_u, &
     &                    GRID(ng) % on_v, &
     &                    GRID(ng) % pm, &
     &                    GRID(ng) % pn, &
     &                    GRID(ng) % Hz, &
     &                    GRID(ng) % Huon, &
     &                    GRID(ng) % Hvom, &
     &                    GRID(ng) % z_r, &
     &                    MIXING(ng) % Akt, &
     &                    OCEAN(ng) % W, &
# ifdef OMEGA_IMPLICIT
     &                    OCEAN(ng) % Wi, &
# endif
# ifdef WEC_VF
     &                    OCEAN(ng) % W_stokes, &
# endif
# if defined SEDIMENT && defined SED_MORPH
     &                    SEDBED(ng) % bed_thick, &
# endif
# if defined FLOATS && defined FLOAT_VWALK
     &                    MIXING(ng) % dAktdz, &
# endif
# ifdef DIAGNOSTICS_TS
     &                    DIAGS(ng) % DiaTwrk, &
# endif
     &                    OCEAN(ng) % t)
# ifdef PROFILE
      CALL wclock_off (ng, iNLM, 35, __LINE__, MyFile)
# endif
!
      RETURN
      END SUBROUTINE step3d_t
!
!***********************************************************************
      SUBROUTINE step3d_t_tile (ng, tile, &
     &                          LBi, UBi, LBj, UBj, &
     &                          IminS, ImaxS, JminS, JmaxS, &
     &                          nrhs, nstp, nnew, &
# ifdef MASKING
     &                          rmask, umask, vmask, &
# endif
# ifdef WET_DRY
     &                          rmask_wet, umask_wet, vmask_wet, &
# endif
     &                          omn, om_u, om_v, on_u, on_v, &
     &                          pm, pn, &
     &                          Hz, Huon, Hvom, &
     &                          z_r, &
     &                          Akt, &
     &                          W, &
# ifdef OMEGA_IMPLICIT
     &                          Wi, &
# endif
# ifdef WEC_VF
     &                          W_stokes, &
# endif
# if defined SEDIMENT && defined SED_MORPH
     &                          bed_thick, &
# endif
# if defined FLOATS && defined FLOAT_VWALK
     &                          dAktdz, &
# endif
# ifdef DIAGNOSTICS_TS
     &                          DiaTwrk, &
# endif
     &                          t)
!***********************************************************************
!
      USE mod_param
      USE mod_clima
      USE mod_ncparam
# if defined NESTING && !defined ONE_WAY
      USE mod_nesting
# endif
      USE mod_scalars
      USE mod_sources
# if defined ROMS_KOKKOS && defined KK_STAGE_STEP3D_T && \
     defined SPLINES_VDIFF && defined DIAGNOSTICS_TS
      USE kokkos_step3d_t_mod
      USE, INTRINSIC :: iso_c_binding, ONLY : C_LOC, C_INT
# endif
!
      USE exchange_3d_mod, ONLY : exchange_r3d_tile
# ifdef DISTRIBUTE
      USE mp_exchange_mod, ONLY : mp_exchange3d
      USE mp_exchange_mod, ONLY : mp_exchange4d
# endif
      USE mpdata_adiff_mod
# ifdef NESTING
      USE nesting_mod, ONLY : bry_fluxes
# endif
      USE t3dbc_mod, ONLY : t3dbc_tile
!
!  Imported variable declarations.
!
      integer, intent(in) :: ng, tile
      integer, intent(in) :: LBi, UBi, LBj, UBj
      integer, intent(in) :: IminS, ImaxS, JminS, JmaxS
      integer, intent(in) :: nrhs, nstp, nnew
!
# ifdef ASSUMED_SHAPE
#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:,LBj:)
      real(r8), intent(in) :: umask(LBi:,LBj:)
      real(r8), intent(in) :: vmask(LBi:,LBj:)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:,LBj:)
      real(r8), intent(in) :: umask_wet(LBi:,LBj:)
      real(r8), intent(in) :: vmask_wet(LBi:,LBj:)
#  endif
      real(r8), intent(in) :: omn(LBi:,LBj:)
      real(r8), intent(in) :: om_u(LBi:,LBj:)
      real(r8), intent(in) :: om_v(LBi:,LBj:)
      real(r8), intent(in) :: on_u(LBi:,LBj:)
      real(r8), intent(in) :: on_v(LBi:,LBj:)
      real(r8), TARGET, intent(in) :: pm(LBi:,LBj:)
      real(r8), TARGET, intent(in) :: pn(LBi:,LBj:)
      real(r8), TARGET, intent(in) :: Hz(LBi:,LBj:,:)
      real(r8), TARGET, intent(in) :: Huon(LBi:,LBj:,:)
      real(r8), TARGET, intent(in) :: Hvom(LBi:,LBj:,:)
      real(r8), TARGET, intent(in) :: z_r(LBi:,LBj:,:)
#  ifdef SUN
      real(r8), TARGET, intent(in) :: Akt(LBi:UBi,LBj:UBj,0:N(ng),NAT)
#  else
      real(r8), TARGET, intent(in) :: Akt(LBi:,LBj:,0:,:)
#  endif
      real(r8), TARGET, intent(in) :: W(LBi:,LBj:,0:)
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: Wi(LBi:,LBj:,0:)
#  endif
#  ifdef WEC_VF
      real(r8), intent(in) :: W_stokes(LBi:,LBj:,0:)
#  endif
#  if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(in) :: bed_thick(LBi:,LBj:,:)
#  endif
#  ifdef DIAGNOSTICS_TS
      real(r8), TARGET, intent(inout) :: DiaTwrk(LBi:,LBj:,:,:,:)
#  endif
#  ifdef SUN
      real(r8), TARGET, intent(inout) :: t(LBi:UBi,LBj:UBj,N(ng),3, &
     &  NT(ng))
#  else
      real(r8), TARGET, intent(inout) :: t(LBi:,LBj:,:,:,:)
#  endif
#  if defined FLOATS && defined FLOAT_VWALK
      real(r8), intent(out) :: dAktdz(LBi:,LBj:,:)
#  endif

# else

#  ifdef MASKING
      real(r8), intent(in) :: rmask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask(LBi:UBi,LBj:UBj)
#  endif
#  ifdef WET_DRY
      real(r8), intent(in) :: rmask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: umask_wet(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: vmask_wet(LBi:UBi,LBj:UBj)
#  endif
      real(r8), intent(in) :: omn(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: om_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: om_v(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_u(LBi:UBi,LBj:UBj)
      real(r8), intent(in) :: on_v(LBi:UBi,LBj:UBj)
      real(r8), TARGET, intent(in) :: pm(LBi:UBi,LBj:UBj)
      real(r8), TARGET, intent(in) :: pn(LBi:UBi,LBj:UBj)
      real(r8), TARGET, intent(in) :: Hz(LBi:UBi,LBj:UBj,N(ng))
      real(r8), TARGET, intent(in) :: Huon(LBi:UBi,LBj:UBj,N(ng))
      real(r8), TARGET, intent(in) :: Hvom(LBi:UBi,LBj:UBj,N(ng))
      real(r8), TARGET, intent(in) :: z_r(LBi:UBi,LBj:UBj,N(ng))
      real(r8), TARGET, intent(in) :: Akt(LBi:UBi,LBj:UBj,0:N(ng),NAT)
      real(r8), TARGET, intent(in) :: W(LBi:UBi,LBj:UBj,0:N(ng))
#  ifdef OMEGA_IMPLICIT
      real(r8), intent(in) :: Wi(LBi:UBi,LBj:UBj,0:N(ng))
#  endif
#  ifdef WEC_VF
      real(r8), intent(in) :: W_stokes(LBi:UBi,LBj:UBj,0:N(ng))
#  endif
#  if defined SEDIMENT && defined SED_MORPH
      real(r8), intent(in) :: bed_thick(LBi:UBi,LBj:UBj,3)
#  endif
#  ifdef DIAGNOSTICS_TS
      real(r8), TARGET, intent(inout) :: DiaTwrk(LBi:UBi,LBj:UBj, &
     &  N(ng),NT(ng), &
     &                                   NDT)
#  endif
      real(r8), TARGET, intent(inout) :: t(LBi:UBi,LBj:UBj,N(ng),3, &
     &  NT(ng))

#  if defined FLOATS && defined FLOAT_VWALK
      real(r8), intent(out) :: dAktdz(LBi:UBi,LBj:UBj,N(ng))
#  endif
# endif
!
!  Local variable declarations.
!
      logical :: LapplySrc, Lhsimt, Lmpdata
# if defined ROMS_KOKKOS && defined KK_STAGE_STEP3D_T && \
     defined SPLINES_VDIFF && defined DIAGNOSTICS_TS
      TYPE(kokkos_tracer_config) :: kk_config
      integer(C_INT) :: kk_schemes(NT(ng))
      logical :: kk_eligible
      integer, save :: kk_fast_calls=0, kk_fallback_calls=0
# endif
!
# ifdef NESTING
      integer :: ILB, IUB, JLB, JUB
      integer :: dg, cr, rg
# endif
      integer :: IminT, ImaxT, JminT, JmaxT
      integer :: Isrc, Jsrc
      integer :: i, ic, ii, is, itrc, j, jj, k, ltrc
# if defined AGE_MEAN && defined T_PASSIVE
      integer :: iage
# endif
# ifdef DIAGNOSTICS_TS
      integer :: idiag
# endif
      real(r8) :: eps = 1.0E-16_r8
      real(r8) :: eps1 = 1.0E-12_r8

      real(r8) :: cff, cff1, cff2, cff3
      real(r8) :: betaL, betaR, betaD, betaU
      real(r8) :: rL, rR, rD, rU, rkaL, rkaR, rkaD, rkaU
      real(r8) :: a1, b1, sw, sw_eta, sw_xi

      real(r8), dimension(IminS:ImaxS) :: gradX, KaX, oKaX
      real(r8), dimension(JminS:JmaxS) :: gradE, KaE, oKaE
      real(r8), dimension(0:N(ng))     :: gradZ, KaZ, oKaZ

      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: CF
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: BC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: DC
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FC
# ifdef OMEGA_IMPLICIT
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FCmin
      real(r8), dimension(IminS:ImaxS,0:N(ng)) :: FCmax
# endif

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: FE
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: FX
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: curv
      real(r8), dimension(IminS:ImaxS,JminS:JmaxS) :: grad

      real(r8), dimension(IminS:ImaxS,JminS:JmaxS,N(ng)) :: oHz

# ifdef DIAGNOSTICS_TS
      real(r8), allocatable :: Dhadv(:,:,:)
      real(r8), allocatable :: Dvadv(:,:,:,:)
# endif
      real(r8), allocatable :: Ta(:,:,:,:)
      real(r8), allocatable :: Ua(:,:,:)
      real(r8), allocatable :: Va(:,:,:)
      real(r8), allocatable :: Wa(:,:,:)

# include "set_bounds.h"

# ifdef NESTING
!
!  Notice that the trace flux boundary arrays are dimensioned with the
!  global dimensions of grid to facilitate processing.
!
      ILB=BOUNDS(ng)%LBi(-1)
      IUB=BOUNDS(ng)%UBi(-1)
      JLB=BOUNDS(ng)%LBj(-1)
      JUB=BOUNDS(ng)%UBj(-1)
# endif
!
!-----------------------------------------------------------------------
!  Time-step horizontal advection term.
!-----------------------------------------------------------------------
!
      Lhsimt =ANY(Hadvection(:,ng)%HSIMT).and. &
     &        ANY(Vadvection(:,ng)%HSIMT)
      Lmpdata=ANY(Hadvection(:,ng)%MPDATA).and. &
     &        ANY(Vadvection(:,ng)%MPDATA)
!
!  Allocate local arrays for MPDATA.
!
      IF (Lmpdata) THEN
# ifdef DIAGNOSTICS_TS
        IF (.not.allocated(Dhadv)) THEN
          allocate ( Dhadv(IminS:ImaxS,JminS:JmaxS,3) )
          Dhadv=0.0_r8
        END IF
        IF (.not.allocated(Dvadv)) THEN
          allocate ( Dvadv(IminS:ImaxS,JminS:JmaxS,N(ng),NT(ng)) )
          Dvadv=0.0_r8
        END IF
# endif
        IF (.not.allocated(Ta)) THEN
          allocate ( Ta(IminS:ImaxS,JminS:JmaxS,N(ng),NT(ng)) )
          Ta=0.0_r8
        END IF
        IF (.not.allocated(Ua)) THEN
          allocate ( Ua(IminS:ImaxS,JminS:JmaxS,N(ng)) )
          Ua=0.0_r8
        END IF
        IF (.not.allocated(Va)) THEN
          allocate ( Va(IminS:ImaxS,JminS:JmaxS,N(ng)) )
          Va=0.0_r8
        END IF
        IF (.not.allocated(Wa)) THEN
          allocate ( Wa(IminS:ImaxS,JminS:JmaxS,0:N(ng)) )
          Wa=0.0_r8
        END IF
      END IF
!
!  Compute reciprocal thickness, 1/Hz.
!
# if defined ROMS_KOKKOS && defined KK_STAGE_STEP3D_T && \
     defined SPLINES_VDIFF && defined DIAGNOSTICS_TS
      kk_eligible=.FALSE.
#  if !defined OMEGA_IMPLICIT && !defined WEC_VF && !defined NESTING && \
     !defined MASKING && !defined WET_DRY && \
     !(defined SEDIMENT && defined SED_MORPH)
      kk_eligible=(nnew.ge.1).and.(nnew.le.2).and.(N(ng).ge.3).and. &
     &            (.not.LuvSrc(ng)).and.(.not.LwSrc(ng))
      kk_eligible=kk_eligible.and.kk_is_contiguous(t).and. &
     &            kk_is_contiguous(DiaTwrk).and. &
     &  kk_is_contiguous(Hz).and. &
     &            kk_is_contiguous(Huon).and. &
     &  kk_is_contiguous(Hvom).and. &
     &            kk_is_contiguous(Akt).and.kk_is_contiguous(W).and. &
     &            kk_is_contiguous(z_r).and.kk_is_contiguous(pm).and. &
     &            kk_is_contiguous(pn)
!  Physical closed-edge folds need one cell; interior/periodic faces
!  need the full two-cell stencil.
      kk_eligible=kk_eligible.and.(LBi.le.Istr-2+MERGE(1,0,           &
     &  DOMAIN(ng)%Western_Edge(tile).and..not.EWperiodic(ng).and.     &
     &  .not.CompositeGrid(iwest,ng))).and.                          &
     &  (UBi.ge.Iend+2-MERGE(1,0,                                   &
     &  DOMAIN(ng)%Eastern_Edge(tile).and..not.EWperiodic(ng).and.     &
     &  .not.CompositeGrid(ieast,ng))).and.                          &
     &  (LBj.le.Jstr-2+MERGE(1,0,                                    &
     &  DOMAIN(ng)%Southern_Edge(tile).and..not.NSperiodic(ng).and.    &
     &  .not.CompositeGrid(isouth,ng))).and.                         &
     &  (UBj.ge.Jend+2-MERGE(1,0,                                    &
     &  DOMAIN(ng)%Northern_Edge(tile).and..not.NSperiodic(ng).and.    &
     &  .not.CompositeGrid(inorth,ng)))
      DO itrc=1,NT(ng)
        kk_schemes(itrc)=-1
        IF (Hadvection(itrc,ng)%UPSTREAM3.and.                         &
     &      Vadvection(itrc,ng)%CENTERED4) kk_schemes(itrc)=0
        IF (Hadvection(itrc,ng)%HSIMT.and.                             &
     &      Vadvection(itrc,ng)%HSIMT) kk_schemes(itrc)=1
        IF (kk_schemes(itrc).lt.0) kk_eligible=.FALSE.
      END DO
#  endif
      IF (kk_eligible) THEN
        kk_config%LBi=LBi
        kk_config%UBi=UBi
        kk_config%LBj=LBj
        kk_config%UBj=UBj
        kk_config%Istr=Istr
        kk_config%Iend=Iend
        kk_config%Jstr=Jstr
        kk_config%Jend=Jend
        kk_config%N=N(ng)
        kk_config%NT=NT(ng)
        kk_config%NAT=NAT
        kk_config%NDT=NDT
        kk_config%nnew=nnew
        kk_config%xadv=iTxadv
        kk_config%yadv=iTyadv
        kk_config%hadv=iThadv
        kk_config%vadv=iTvadv
        kk_config%vdif=iTvdif
        kk_config%west=MERGE(1,0,DOMAIN(ng)%Western_Edge(tile))
        kk_config%east=MERGE(1,0,DOMAIN(ng)%Eastern_Edge(tile))
        kk_config%south=MERGE(1,0,DOMAIN(ng)%Southern_Edge(tile))
        kk_config%north=MERGE(1,0,DOMAIN(ng)%Northern_Edge(tile))
        kk_config%periodic_ew=MERGE(1,0,EWperiodic(ng))
        kk_config%periodic_ns=MERGE(1,0,NSperiodic(ng))
        kk_config%composite_w=MERGE(1,0,CompositeGrid(iwest,ng))
        kk_config%composite_e=MERGE(1,0,CompositeGrid(ieast,ng))
        kk_config%composite_s=MERGE(1,0,CompositeGrid(isouth,ng))
        kk_config%composite_n=MERGE(1,0,CompositeGrid(inorth,ng))
        kk_config%dt=dt(ng)
        kk_config%cc1=cc1
        kk_config%cc2=cc2
        kk_config%cc3=cc3
!
!  Preserve the HSIMT prologue exchanges before any accelerated writes.
!
        DO itrc=1,NT(ng)
          IF (kk_schemes(itrc).eq.1) THEN
            IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
              CALL exchange_r3d_tile (ng, tile,                       &
     &                                LBi, UBi, LBj, UBj, 1, N(ng),   &
     &                                t(:,:,:,nnew,itrc))
            END IF
#  ifdef DISTRIBUTE
            CALL mp_exchange3d (ng, tile, iNLM, 1,                    &
     &                          LBi, UBi, LBj, UBj, 1, N(ng),          &
     &                          NghostPoints,                         &
     &                          EWperiodic(ng), NSperiodic(ng),        &
     &                          t(:,:,:,nnew,itrc))
#  endif
          END IF
        END DO
        CALL k_step3d_t_chunks (kk_config, kk_schemes, C_LOC(pm),     &
     &                   C_LOC(pn),                                     &
     &                   C_LOC(Hz), C_LOC(Huon), C_LOC(Hvom),          &
     &                   C_LOC(W), C_LOC(z_r), C_LOC(Akt),            &
     &                   C_LOC(t), C_LOC(DiaTwrk))
        kk_fast_calls=kk_fast_calls+1
!  Report the path once; a line per step would flood long-run logs.
        IF (kk_fast_calls.eq.1) THEN
          WRITE (*,'(a,i0)') 'step3d_t accelerated (first call) tile=', &
     &                       tile
        END IF
      ELSE
        kk_fallback_calls=kk_fallback_calls+1
        IF (kk_fallback_calls.eq.1) THEN
          WRITE (*,'(a)') 'step3d_t: whole-tile Fortran fallback '// &
     &      '(scheme, physics, layout, halo or time-level guard)'
        END IF
# endif
```

</details>

<a id="src-caller-t-common-epilogue"></a>

**source-block ID：`caller-t-common-epilogue`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Nonlinear/step3d_t.F`；闭区间：**1941–2121**；行数：**181**；UTF-8 payload SHA256：`474e91287f651dc172e82a972d398bdf633f53b9e512bfe358374e55688931cc`。

<details>
<summary>ROMS/Nonlinear/step3d_t.F，1941–2121（完整原文，可展开）</summary>

<!-- source-block id=caller-t-common-epilogue commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Nonlinear/step3d_t.F lines=1941-2121 count=181 sha256=474e91287f651dc172e82a972d398bdf633f53b9e512bfe358374e55688931cc -->
```fortran
# if defined ROMS_KOKKOS && defined KK_STAGE_STEP3D_T && \
     defined SPLINES_VDIFF && defined DIAGNOSTICS_TS
      END IF
# endif

# if defined AGE_MEAN && defined T_PASSIVE
!
!-----------------------------------------------------------------------
!  If inert passive tracer and Mean Age, compute age concentration (even
!  inert index) forced by the right-hand-side term that is concentration
!  of an associated conservative passive tracer (odd inert index). Mean
!  Age is age concentration divided by conservative passive tracer
!  concentration. Code implements NPT/2 mean age tracer pairs.
!
!  Implemented and tested by W.G. Zhang and J. Wilkin. See following
!  reference for details.
!
!   Zhang et al. (2010): Simulation of water age and residence time in
!      the New York Bight, JPO, 40,965-982, doi:10.1175/2009JPO4249.1
!-----------------------------------------------------------------------
!
      DO itrc=1,NPT,2
        iage=inert(itrc+1)                     ! even inert tracer index
        DO k=1,N(ng)
          DO j=Jstr,Jend
            DO i=Istr,Iend
              IF ((Hadvection(inert(itrc),ng)%MPDATA).and. &
     &            (Vadvection(inert(itrc),ng)%MPDATA)) THEN
                t(i,j,k,nnew,iage)=t(i,j,k,nnew,iage)+ &
     &                             dt(ng)*t(i,j,k,nnew,inert(itrc))
              ELSE
                t(i,j,k,nnew,iage)=t(i,j,k,nnew,iage)+ &
     &                             dt(ng)*t(i,j,k,3,inert(itrc))
              END IF
            END DO
          END DO
        END DO
      END DO
# endif
!
!-----------------------------------------------------------------------
!  Apply lateral boundary conditions and, if appropriate, nudge
!  to tracer data and apply Land/Sea mask.
!-----------------------------------------------------------------------
!
!  Initialize tracer counter index. The "tclm" array is only allocated
!  to the NTCLM fields that need to be processed. This is done to
!  reduce memory.
!
      ic=0
!
      DO itrc=1,NT(ng)
!
!  Set compact reduced memory tracer index for nudging coefficients and
!  climatology arrays.
!
        IF (LtracerCLM(itrc,ng).and.LnudgeTCLM(itrc,ng)) THEN
          ic=ic+1
        END IF
!
!  Set lateral boundary conditions.
!
        CALL t3dbc_tile (ng, tile, itrc, ic, &
     &                   LBi, UBi, LBj, UBj, N(ng), NT(ng), &
     &                   IminS, ImaxS, JminS, JmaxS, &
     &                   nstp, nnew, &
     &                   t)
!
!  Nudge towards tracer climatology.
!
        IF (LtracerCLM(itrc,ng).and.LnudgeTCLM(itrc,ng)) THEN
          DO k=1,N(ng)
            DO j=JstrR,JendR
              DO i=IstrR,IendR
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+ &
     &                             dt(ng)* &
     &                             CLIMA(ng)%Tnudgcof(i,j,k,ic)* &
     &                             (CLIMA(ng)%tclm(i,j,k,ic)- &
     &                              t(i,j,k,nnew,itrc))
              END DO
            END DO
          END DO
        END IF

# ifdef MASKING
!
!  Apply Land/Sea mask.
!
        DO k=1,N(ng)
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)*rmask(i,j)
            END DO
          END DO
        END DO
# endif
# ifdef DIAGNOSTICS_TS
!
!  Compute time-rate-of-change diagnostic term.
!
        DO k=1,N(ng)
          DO j=JstrR,JendR
            DO i=IstrR,IendR
              DiaTwrk(i,j,k,itrc,iTrate)=t(i,j,k,nnew,itrc)- &
     &                                   DiaTwrk(i,j,k,itrc,iTrate)
!!            DiaTwrk(i,j,k,itrc,iTrate)=t(i,j,k,nnew,itrc)-            &
!!   &                                   t(i,j,k,nstp,itrc)
            END DO
          END DO
        END DO
# endif
!
!  Apply periodic boundary conditions.
!
        IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
          CALL exchange_r3d_tile (ng, tile, &
     &                            LBi, UBi, LBj, UBj, 1, N(ng), &
     &                            t(:,:,:,nnew,itrc))
        END IF
      END DO
# ifdef DISTRIBUTE
!
!  Exchange boundary data.
!
      CALL mp_exchange4d (ng, tile, iNLM, 1, &
     &                    LBi, UBi, LBj, UBj, 1, N(ng), 1, NT(ng), &
     &                    NghostPoints, &
     &                    EWperiodic(ng), NSperiodic(ng), &
     &                    t(:,:,:,nnew,:))
# endif
# if defined FLOATS && defined FLOAT_VWALK
!
!-----------------------------------------------------------------------
!  Compute vertical gradient in vertical T-diffusion coefficient for
!  floats random walk.
!-----------------------------------------------------------------------
!
      DO j=JstrR,JendR
        DO i=IstrR,IendR
          DO k=1,N(ng)
            dAktdz(i,j,k)=(Akt(i,j,k,1)-Akt(i,j,k-1,1))/Hz(i,j,k)
          END DO
        END DO
      END DO
!
!  Apply periodic boundary conditions.
!
      IF (EWperiodic(ng).or.NSperiodic(ng)) THEN
        CALL exchange_r3d_tile (ng, tile, &
     &                          LBi, UBi, LBj, UBj, 1, N(ng), &
     &                          dAktdz)
      END IF

#  ifdef DISTRIBUTE
      CALL mp_exchange3d (ng, tile, iNLM, 1, &
     &                    LBi, UBi, LBj, UBj, 1, N(ng), &
     &                    NghostPoints, &
     &                    EWperiodic(ng), NSperiodic(ng), &
     &                    dAktdz)
#  endif
# endif
!
!-----------------------------------------------------------------------
!  If applicable, deallocate local arrays.
!-----------------------------------------------------------------------
!
      IF (Lmpdata) THEN
# ifdef DIAGNOSTICS_TS
        IF (allocated(Dhadv)) deallocate (Dhadv)
        IF (allocated(Dvadv)) deallocate (Dvadv)
# endif
        IF (allocated(Ta))    deallocate (Ta)
        IF (allocated(Ua))    deallocate (Ua)
        IF (allocated(Va))    deallocate (Va)
        IF (allocated(Wa))    deallocate (Wa)
      END IF
!
      RETURN
      END SUBROUTINE step3d_t_tile
#endif
      END MODULE step3d_t_mod
```

</details>

<a id="appendix-runtime"></a>

## 14. 附录 D：数组、HostWorkspace、运行时与严格构建来源

### 14.1 数组与 allocator / 运行时的真实定义

完整 config 头含 `Array2d/3d/4d/5d`、`as_2d/3d/4d/5d`、后端选择、宏同步约束与计时类；完整 config 实现含 initialize/finalize、call_once、计时表与 mutex。其“scratch 由 Fortran 拥有”的旧共用说明针对 step2d 外传工作数组，不能套用来否认本文两个 `.cpp` 自己的 HostWorkspace。以下 allocator 是本文三维入口真实使用的 host scratch 定义。

<a id="src-runtime-config-hpp"></a>

**source-block ID：`runtime-config-hpp`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_config.hpp`；闭区间：**1–346**；行数：**346**；UTF-8 payload SHA256：`59a3be05fa17305fe9778535ae99bcb1afd01ed709bc5766bd4600c1eaaa8c8e`。

<details>
<summary>ROMS/Cpp/kokkos_config.hpp，1–346（完整原文，可展开）</summary>

<!-- source-block id=runtime-config-hpp commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_config.hpp lines=1-346 count=346 sha256=59a3be05fa17305fe9778535ae99bcb1afd01ed709bc5766bd4600c1eaaa8c8e -->
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

<a id="src-runtime-workspace-hpp"></a>

**source-block ID：`runtime-workspace-hpp`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_workspace.hpp`；闭区间：**1–31**；行数：**31**；UTF-8 payload SHA256：`96ce40f7d433e8b021f2c51ea21911af17acaec7d70d80373f81559fff55c4b0`。

<details>
<summary>ROMS/Cpp/kokkos_workspace.hpp，1–31（完整原文，可展开）</summary>

<!-- source-block id=runtime-workspace-hpp commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_workspace.hpp lines=1-31 count=31 sha256=96ce40f7d433e8b021f2c51ea21911af17acaec7d70d80373f81559fff55c4b0 -->
```cpp
/* Host scratch shared by the Kokkos kernel implementations.
** Each translation unit keeps its own thread_local instance. Allocation
** and teardown use no Kokkos services, so storage can outlive finalize(). */
#ifndef ROMS_KOKKOS_WORKSPACE_HPP
#define ROMS_KOKKOS_WORKSPACE_HPP

#ifndef __sw_slave__
#include <cstddef>
#include <memory>

namespace roms {

class HostWorkspace {
public:
    double* reserve(std::size_t count) {
        if (count > capacity_) {
            storage_.reset(new double[count]);
            capacity_ = count;
        }
        return storage_.get();
    }

private:
    std::unique_ptr<double[]> storage_;
    std::size_t capacity_ = 0;
};

} // namespace roms
#endif // !__sw_slave__

#endif // ROMS_KOKKOS_WORKSPACE_HPP
```

</details>

<a id="src-runtime-config-cpp"></a>

**source-block ID：`runtime-config-cpp`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/kokkos_config.cpp`；闭区间：**1–130**；行数：**130**；UTF-8 payload SHA256：`bae8e44d7e161134b1edb90bfd4f56a41d530ea07c0bb79e91bf1de927a1aa58`。

<details>
<summary>ROMS/Cpp/kokkos_config.cpp，1–130（完整原文，可展开）</summary>

<!-- source-block id=runtime-config-cpp commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/kokkos_config.cpp lines=1-130 count=130 sha256=bae8e44d7e161134b1edb90bfd4f56a41d530ea07c0bb79e91bf1de927a1aa58 -->
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

### 14.2 SW64 必要执行与同步依赖

这里给出真实 `ParallelFor` 专门化完整类（文件此前 include/guard/namespace 一并保留）、实际 launch/join 定义、execution/memory_space 类型区段及完整 SW64 setup。它们是定位到本端口的必要依赖，不宣称展开整个 Kokkos 传递 include 树。`ParallelFor` 源块在原文件的 namespace/guard 尚未收尾处结束，因为后续是本五入口未使用的 ParallelReduce 类；前 74 行的 ParallelFor 类本身完整。

<a id="src-runtime-sw64-range"></a>

**source-block ID：`runtime-sw64-range`**；commit：`6e40ad34a8968a44386e01a5a757e1e37bfcacea`；路径：`Kokkos/core/src/SW64/Kokkos_SW64_Parallel_Range.hpp`；闭区间：**1–74**；行数：**74**；UTF-8 payload SHA256：`7c9f181fd558498dbc85f6ade8e528f43f6bb35c0613f934f7e1b53b38b8f6ba`。

<details>
<summary>Kokkos/core/src/SW64/Kokkos_SW64_Parallel_Range.hpp，1–74（完整原文，可展开）</summary>

<!-- source-block id=runtime-sw64-range commit=6e40ad34a8968a44386e01a5a757e1e37bfcacea path=Kokkos/core/src/SW64/Kokkos_SW64_Parallel_Range.hpp lines=1-74 count=74 sha256=7c9f181fd558498dbc85f6ade8e528f43f6bb35c0613f934f7e1b53b38b8f6ba -->
```cpp
#ifndef KOKKOS_SW64_PARALLEL_RANGE_HPP
#define KOKKOS_SW64_PARALLEL_RANGE_HPP

#include <Kokkos_Core.hpp>
#include <Kokkos_Parallel.hpp>
#include <SW64/Kokkos_SW64.hpp>
#include <SW64/Kokkos_SW64_KernelLaunch.hpp>
#include <SW64/Kokkos_SW64_Instance.hpp>
#include <SW64/Kokkos_SW64_RMAReducer.hpp>

#if defined(KOKKOS_ENABLE_SW64) && defined(__sw_hybrid_coding__)

namespace Kokkos {
namespace Impl {

template <class FunctorType, class... Traits>
class ParallelFor<FunctorType, Kokkos::RangePolicy<Traits...>, Kokkos::SW64> {
 public:
  using Policy = Kokkos::RangePolicy<Traits...>;

 private:
  using Member       = typename Policy::member_type;
  using WorkTag      = typename Policy::work_tag;
  using LaunchBounds = typename Policy::launch_bounds;

  const FunctorType m_functor;
  const Policy m_policy;

  template <class TagType>
  inline __attribute__((slave)) std::enable_if_t<std::is_void<TagType>::value>
  exec_range(const Member i) const {
    m_functor(i);
  }

  template <class TagType>
  inline __attribute__((slave)) std::enable_if_t<!std::is_void<TagType>::value>
  exec_range(const Member i) const {
    m_functor(TagType(), i);
  }

 public:
  using functor_type = FunctorType;

  ParallelFor()                              = delete;
  ParallelFor(ParallelFor const&)            = default;
  ParallelFor& operator=(const ParallelFor&) = delete;

  Policy const& get_policy() const { return m_policy; }

  inline __attribute__((slave)) void operator()() const {
    const Member work_stride = (m_policy.end() - m_policy.begin() + 63) / 64;
    const Member work_start  = m_policy.begin() + threadIdx() * work_stride;
    const Member work_end    = (work_start + work_stride < m_policy.end())
                                   ? work_start + work_stride
                                   : m_policy.end();

    for (Member iwork = work_start; iwork < work_end; ++iwork) {
      this->template exec_range<WorkTag>(iwork);
    }
  }

  inline void execute() const {
    const typename Policy::index_type nwork = m_policy.end() - m_policy.begin();
    // printf("execute from range [%d, %d] nwork: %d, chunk_size: %d\n",
    //        m_policy.begin(), m_policy.end(), nwork, m_policy.chunk_size());
    if (nwork == 0) return;
    using DriverType = ParallelFor<FunctorType, Policy, Kokkos::SW64>;

    Kokkos::Impl::sw64_parallel_launch<DriverType>(*this);
  }

  ParallelFor(const FunctorType& arg_functor, const Policy& arg_policy)
      : m_functor(arg_functor), m_policy(arg_policy) {}
};
```

</details>

<a id="src-runtime-sw64-launch"></a>

**source-block ID：`runtime-sw64-launch`**；commit：`6e40ad34a8968a44386e01a5a757e1e37bfcacea`；路径：`Kokkos/core/src/SW64/Kokkos_SW64_KernelLaunch.hpp`；闭区间：**1–45**；行数：**45**；UTF-8 payload SHA256：`9fd84474493df3581f99203449b2de183acbc0fec8f83a78525bfdb9f73d5e62`。

<details>
<summary>Kokkos/core/src/SW64/Kokkos_SW64_KernelLaunch.hpp，1–45（完整原文，可展开）</summary>

<!-- source-block id=runtime-sw64-launch commit=6e40ad34a8968a44386e01a5a757e1e37bfcacea path=Kokkos/core/src/SW64/Kokkos_SW64_KernelLaunch.hpp lines=1-45 count=45 sha256=9fd84474493df3581f99203449b2de183acbc0fec8f83a78525bfdb9f73d5e62 -->
```cpp
#ifndef KOKKOS_SW64_KERNEL_LAUNCH_HPP
#define KOKKOS_SW64_KERNEL_LAUNCH_HPP

#include <Kokkos_Macros.hpp>

#if defined(__sw_hybrid_coding__)

// #include <SW64/Kokkos_SW64_Instance.hpp>
// #include <SW64/Kokkos_SW64_Space.hpp>
#include <crts.h>

namespace Kokkos {
namespace Impl {

template <typename DriverType>
[[gnu::kernel]]
static void sw64_kernel_launch(const DriverType& driver) {
#ifdef __sw_slave__
  driver.operator()();
#endif
}

//---------------------------------------------------------------//
// Helper function                                               //
//---------------------------------------------------------------//
inline int threadIdx() {
#ifdef __sw_slave__
  return _PEN;
#endif
}

// convenience method to launch the correct kernel given the launch bounds et
// al.
template <typename DriverType>
[[gnu::host]] void sw64_parallel_launch(const DriverType& driver) {
  sw64_kernel_launch<DriverType>(driver);
  athread_join_cgs();
}

}  // namespace Impl
}  // namespace Kokkos

#endif

#endif
```

</details>

<a id="src-runtime-sw64-space"></a>

**source-block ID：`runtime-sw64-space`**；commit：`6e40ad34a8968a44386e01a5a757e1e37bfcacea`；路径：`Kokkos/core/src/SW64/Kokkos_SW64.hpp`；闭区间：**38–55**；行数：**18**；UTF-8 payload SHA256：`fe5fa44a40f0df2a864f63d323f66995cc10d26ee70acbe494e15443256e1e31`。

<details>
<summary>Kokkos/core/src/SW64/Kokkos_SW64.hpp，38–55（完整原文，可展开）</summary>

<!-- source-block id=runtime-sw64-space commit=6e40ad34a8968a44386e01a5a757e1e37bfcacea path=Kokkos/core/src/SW64/Kokkos_SW64.hpp lines=38-55 count=18 sha256=fe5fa44a40f0df2a864f63d323f66995cc10d26ee70acbe494e15443256e1e31 -->
```cpp
namespace Kokkos {

/// \class SW64
/// \brief Kokkos Execution Space that uses SW64 to run on Sunway Manycore.
class SW64 {
 public:
  //! \name Type declarations that all Kokkos devices must provide.
  //@{

  //! Tag this class as an execution space:
  using execution_space = SW64;
  using memory_space    = Kokkos::HostSpace;
  using device_type     = Kokkos::Device<execution_space, memory_space>;

  using array_layout = LayoutLeft;  // what's LayoutLeft?
  using size_type    = memory_space::size_type;

  using scratch_memory_space = ScratchMemorySpace<SW64>;
```

</details>

<a id="src-runtime-sw64-setup"></a>

**source-block ID：`runtime-sw64-setup`**；commit：`6e40ad34a8968a44386e01a5a757e1e37bfcacea`；路径：`Kokkos/core/src/setup/Kokkos_Setup_SW64.hpp`；闭区间：**1–58**；行数：**58**；UTF-8 payload SHA256：`3911f02a9e7b7c19958409244ab1a601955dad704568b1e0d01c277b3460d3ea`。

<details>
<summary>Kokkos/core/src/setup/Kokkos_Setup_SW64.hpp，1–58（完整原文，可展开）</summary>

<!-- source-block id=runtime-sw64-setup commit=6e40ad34a8968a44386e01a5a757e1e37bfcacea path=Kokkos/core/src/setup/Kokkos_Setup_SW64.hpp lines=1-58 count=58 sha256=3911f02a9e7b7c19958409244ab1a601955dad704568b1e0d01c277b3460d3ea -->
```cpp
#ifndef KOKKOS_SW64_SETUP_HPP_
#define KOKKOS_SW64_SETUP_HPP_

#if defined(KOKKOS_ENABLE_SW64)

#include <crts.h>

#ifndef __forceinline__
#define __forceinline__ __attribute__((always_inline))
#endif

#ifdef __sw_hybrid_coding__

#define KOKKOS_LAMBDA [&]
#define KOKKOS_CLASS_LAMBDA [&, *this ]

#define KOKKOS_DEDUCTION_GUIDE

#define KOKKOS_IMPL_FORCEINLINE_FUNCTION \
  __attribute__((hostslave, always_inline)) inline
#define KOKKOS_IMPL_INLINE_FUNCTION __attribute__((hostslave)) inline
#define KOKKOS_IMPL_FORCEINLINE __forceinline__
#define KOKKOS_IMPL_FUNCTION __attribute__((hostslave))
#define KOKKOS_IMPL_HOST_FUNCTION __attribute__((host))
#define KOKKOS_IMPL_DEVICE_FUNCTION __attribute__((slave))

#endif  // __sw_hybrid_coding__

// dummy std::mutex for SW64 CPE
#ifdef __sw_slave__
#include <pthread.h>
namespace std {
class mutex {
 protected:
  typedef pthread_mutex_t __native_type;
  __native_type _M_mutex;

 public:
  typedef __native_type* native_handle_type;

  mutex() noexcept = default;
  ~mutex()         = default;

  mutex(const mutex&)            = delete;
  mutex& operator=(const mutex&) = delete;

  void lock() {}
  void try_lock() {}
  void unlock() {}

  native_handle_type native_handle() noexcept { return &_M_mutex; }
};
}  // namespace std
#endif

#endif  // KOKKOS_ENABLE_SW64

#endif /* KOKKOS_SW64_SETUP_HPP_ */
```

</details>

### 14.3 编译与测试接线的原始定义

下列源码仅用于解释既有构建选项，不是本次执行命令。CMake 片段保留实际 SW64 数值选项与直接测试接线，tracer chunks 是独立选项；阶段宏仍通过 MY_CPP_FLAGS 等现有机制传递。

<a id="src-build-cmake-tracer-option"></a>

**source-block ID：`build-cmake-tracer-option`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`CMakeLists.txt`；闭区间：**55–102**；行数：**48**；UTF-8 payload SHA256：`5e76572ea7910a42f8d6295dfda80722cdc4b932083f99cf920a8fc3521db737`。

<details>
<summary>CMakeLists.txt，55–102（完整原文，可展开）</summary>

<!-- source-block id=build-cmake-tracer-option commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=CMakeLists.txt lines=55-102 count=48 sha256=5e76572ea7910a42f8d6295dfda80722cdc4b932083f99cf920a8fc3521db737 -->
```cmake
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Kokkos (optional)
#
# When ROMS_KOKKOS is ON the ROMS/Cpp kernels are built and linked, and
# the ROMS_KOKKOS define reaches both Fortran and C++ so the two sides
# agree on which path is active.  C++ is enabled only for these builds.
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

option( ROMS_KOKKOS "Build ROMS with Kokkos compute kernels" OFF )
option( ROMS_KOKKOS_STEP3D_T "Enable the U3/C4 and HSIMT tracer chunk kernel" OFF )
if( ROMS_KOKKOS AND ROMS_KOKKOS_STEP3D_T )
  add_compile_definitions( KK_STAGE_STEP3D_T )
endif()
set( ROMS_KOKKOS_BACKEND "serial" CACHE STRING
     "Kokkos execution space for the ROMS kernels: serial | openmp | sw64" )
set_property( CACHE ROMS_KOKKOS_BACKEND PROPERTY STRINGS serial openmp sw64 )

if( ROMS_KOKKOS )
  enable_language( CXX )
  # Backend.  "serial" keeps ROMS as the only source of parallelism (it
  # threads over tiles) and the kernels are plain loops; "openmp" lets
  # Kokkos thread inside each kernel, which requires running ROMS itself
  # with OMP_NUM_THREADS=1 so the two do not nest; "sw64" targets the
  # Sunway manycore backend of the Kokkos fork.
  string( TOLOWER "${ROMS_KOKKOS_BACKEND}" _kk_backend )
  if( _kk_backend STREQUAL "openmp" )
    set( Kokkos_ENABLE_SERIAL ON  CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_OPENMP ON  CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_SW64   OFF CACHE BOOL "" FORCE )
    add_compile_definitions( ROMS_KOKKOS_OPENMP )
  elseif( _kk_backend STREQUAL "sw64" )
    set( Kokkos_ENABLE_SERIAL ON  CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_OPENMP OFF CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_SW64   ON  CACHE BOOL "" FORCE )
    # The SW64 backend needs C++17: Compilers/toolchain_SW64.cmake sets
    # it and Kokkos' cmake/kokkos_enable_devices.cmake enforces it.
    add_compile_definitions( ROMS_KOKKOS_SW64 )
  elseif( _kk_backend STREQUAL "serial" )
    set( Kokkos_ENABLE_SERIAL ON  CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_OPENMP OFF CACHE BOOL "" FORCE )
    set( Kokkos_ENABLE_SW64   OFF CACHE BOOL "" FORCE )
    add_compile_definitions( ROMS_KOKKOS_SERIAL )
  else()
    message( FATAL_ERROR
             "ROMS_KOKKOS_BACKEND='${ROMS_KOKKOS_BACKEND}' is not one of "
             "serial, openmp, sw64" )
  endif()
  set( Kokkos_ENABLE_TESTS    OFF CACHE BOOL "" FORCE )
```

</details>

<a id="src-build-cmake-kernel-flags-tests"></a>

**source-block ID：`build-cmake-kernel-flags-tests`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`CMakeLists.txt`；闭区间：**394–470**；行数：**77**；UTF-8 payload SHA256：`802b3b2795a92f21f531c159ee611ed7ad84942f98380a46df2a094b0cf80893`。

<details>
<summary>CMakeLists.txt，394–470（完整原文，可展开）</summary>

<!-- source-block id=build-cmake-kernel-flags-tests commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=CMakeLists.txt lines=394-470 count=77 sha256=802b3b2795a92f21f531c159ee611ed7ad84942f98380a46df2a094b0cf80893 -->
```cmake
if( ROMS_KOKKOS )
  # Options for every translation unit that includes the kernels.
  # -mhybrid-coding is required by the SW64 backend (kokkos_config.hpp).
  # The alias-check limit only matters where GCC vectorises
  # (ROMS_FAST_MATH=ON): the fused 2-D RHS and coupling loops touch more
  # arrays than the default limit of 20 versioning checks.
  #
  # swgcc-1473 changes rounding in auto-vectorized CPE arithmetic even
  # without fast-math (an identical-input momentum replay differs by one
  # ULP; its scalar CPE form is exact). KPP amplifies such seeds through
  # density cancellation and convective mixing. Conservative math must
  # therefore also retain scalar evaluation within each CPE. This does
  # not disable Kokkos/CPE parallelism or change the Fortran reference.
  function( roms_kokkos_cxx_options target )
    if( _kk_backend STREQUAL "sw64" )
      target_compile_options( ${target} PRIVATE
        "$<$<COMPILE_LANGUAGE:CXX>:-mhybrid-coding;-fpermissive>" )
      if( ROMS_FAST_MATH )
        target_compile_options( ${target} PRIVATE
          "$<$<COMPILE_LANGUAGE:CXX>:--param;vect-max-version-for-alias-checks=64>" )
      else()
        target_compile_options( ${target} PRIVATE
          "$<$<COMPILE_LANGUAGE:CXX>:-fno-tree-vectorize>" )
      endif()
    endif()
  endfunction()

  target_link_libraries( Objects Kokkos::kokkos )
  roms_kokkos_cxx_options( Objects )

  option( ROMS_KOKKOS_TESTS "Build the direct kernel regression tests" OFF )
  if( ROMS_KOKKOS_TESTS )
    function( roms_kokkos_test target )
      add_executable( ${target} ${ARGN} )
      target_link_libraries( ${target} Kokkos::kokkos )
      set_target_properties( ${target} PROPERTIES LINKER_LANGUAGE Fortran )
      roms_kokkos_cxx_options( ${target} )
    endfunction()

    # kokkos_step2d.cpp only defines the kernels when one of the 2-D stages
    # is enabled, so the direct test follows the same condition.
    set( step2d_stages KK_STAGE_FUSED KK_STAGE_TAVG KK_STAGE_COUP
                       KK_STAGE_MOMT KK_STAGE_DIAG KK_STAGE_STOR )
    set( have_step2d FALSE )
    foreach( stage IN LISTS step2d_stages )
      if( stage IN_LIST MY_CPP_FLAGS )
        set( have_step2d TRUE )
      endif()
    endforeach()
    if( have_step2d )
      roms_kokkos_test( test_step2d_kokkos ROMS/Cpp/test_step2d_kokkos.cpp
        ROMS/Cpp/test_step2d_reference.f90
        ROMS/Cpp/kokkos_config.cpp ROMS/Cpp/kokkos_step2d.cpp )
    endif()

    if( ROMS_KOKKOS_STEP3D_T )
      preprocess_fortran( ROMS/Cpp/test_step3d_t_reference.F )
      roms_kokkos_test( test_step3d_t_kokkos ${f90srcs}
        ROMS/Cpp/test_step3d_t_kokkos.cpp ROMS/Cpp/kokkos_config.cpp
        ROMS/Cpp/kokkos_step3d_t.cpp )
      # It recompiles the ROMS modules; serialise their .mod writes.
      add_dependencies( test_step3d_t_kokkos Objects )
    endif()

    # Fortran only.  The two small modules are the preprocessed copies that
    # the Objects library generates (a .F source would be compiled as fixed
    # form); their own module directory keeps them from overwriting the ones
    # Objects writes.
    add_executable( test_row_guards
      "${CMAKE_CURRENT_BINARY_DIR}/f90/kokkos_step3d_t_mod.f90"
      "${CMAKE_CURRENT_BINARY_DIR}/f90/kokkos_loops_mod.f90"
      ROMS/Cpp/test_row_guards.f90 )
    set_target_properties( test_row_guards PROPERTIES
      Fortran_MODULE_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/module_test_row_guards" )
    add_dependencies( test_row_guards Objects )
  endif()
endif()
```

</details>

<a id="src-build-sw64-toolchain"></a>

**source-block ID：`build-sw64-toolchain`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`Compilers/toolchain_SW64.cmake`；闭区间：**1–15**；行数：**15**；UTF-8 payload SHA256：`10b94655b6339c52251ef73add8ff885aeb707dea59e558d34553986519e44b4`。

<details>
<summary>Compilers/toolchain_SW64.cmake，1–15（完整原文，可展开）</summary>

<!-- source-block id=build-sw64-toolchain commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=Compilers/toolchain_SW64.cmake lines=1-15 count=15 sha256=10b94655b6339c52251ef73add8ff885aeb707dea59e558d34553986519e44b4 -->
```cmake
# Cross-compile on the SW login node after loading swgcc and mpi/swuc.
set(CMAKE_SYSTEM_NAME Linux)
set(CMAKE_SYSTEM_PROCESSOR sw64)
set(SW64 TRUE CACHE BOOL "Sunway platform (also for the Serial control)")

set(CMAKE_CXX_COMPILER swucxx CACHE FILEPATH "Sunway C++ compiler")
# The available NetCDF/HDF5 archive depends on MPI, even when ROMS itself
# is built without distributed-memory support.  The wrapper supplies that
# link dependency; MPI=OFF still compiles a single-process, MPE-only ROMS.
set(CMAKE_Fortran_COMPILER mpif90 CACHE FILEPATH "Sunway Fortran link wrapper")
set(CMAKE_EXE_LINKER_FLAGS_INIT "-mhybrid")
set(CMAKE_CXX_FLAGS_INIT "-mieee")
set(CMAKE_Fortran_FLAGS_INIT "-mieee")
set(CMAKE_CXX_STANDARD 17 CACHE STRING "C++ standard")
set(CMAKE_TRY_COMPILE_TARGET_TYPE STATIC_LIBRARY)
```

</details>

<a id="src-build-gnu-math-flags"></a>

**source-block ID：`build-gnu-math-flags`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`Compilers/compiler_flags_GNU_Fortran.cmake`；闭区间：**29–68**；行数：**40**；UTF-8 payload SHA256：`c893d078c77bd87dc6ebfc4ef5fd26f39f11130b2c7885bc1149d8aa365f5b80`。

<details>
<summary>Compilers/compiler_flags_GNU_Fortran.cmake，29–68（完整原文，可展开）</summary>

<!-- source-block id=build-gnu-math-flags commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=Compilers/compiler_flags_GNU_Fortran.cmake lines=29-68 count=40 sha256=c893d078c77bd87dc6ebfc4ef5fd26f39f11130b2c7885bc1149d8aa365f5b80 -->
```cmake
###########################################################################
# FLAGS COMMON TO ALL BUILD TYPES
###########################################################################

string( APPEND CMAKE_Fortran_FLAGS " -frepack-arrays" )
if( CMAKE_Fortran_COMPILER_VERSION VERSION_GREATER_EQUAL 10 )
  string( APPEND CMAKE_Fortran_FLAGS " -fallow-argument-mismatch" )
endif()

option( ROMS_FAST_MATH "Enable non-IEEE floating-point optimizations" ON )

###########################################################################
# RELEASE FLAGS
###########################################################################

set( CMAKE_Fortran_FLAGS_RELEASE "-O3" )
if( ROMS_FAST_MATH )
  string( APPEND CMAKE_Fortran_FLAGS_RELEASE " -ffast-math" )
endif()

###########################################################################
# RELEASE WITH DEBUG INFORMATION FLAGS
###########################################################################

set( CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-O3 -g -fbounds-check -fbacktrace -fcheck=all" )
if( ROMS_FAST_MATH )
  string( APPEND CMAKE_Fortran_FLAGS_RELWITHDEBINFO " -ffast-math" )
endif()

###########################################################################
# DEBUG FLAGS
###########################################################################

set( CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g -fbounds-check -fbacktrace -fcheck=all -finit-real=nan -ffpe-trap=invalid,zero,overflow" )

###########################################################################
# BIT REPRODUCIBLE FLAGS
###########################################################################

set( CMAKE_Fortran_FLAGS_BIT     "-O2 -fbounds-check" )
```

</details>

<a id="src-build-app-stage-strict"></a>

**source-block ID：`build-app-stage-strict`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/tools/sw64/build_app.sh`；闭区间：**48–78**；行数：**31**；UTF-8 payload SHA256：`1f0da14a06421b357128bb63eb496969e08b5370d3b4f72d360883396f596699`。

<details>
<summary>ROMS/Cpp/tools/sw64/build_app.sh，48–78（完整原文，可展开）</summary>

<!-- source-block id=build-app-stage-strict commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/tools/sw64/build_app.sh lines=48-78 count=31 sha256=1f0da14a06421b357128bb63eb496969e08b5370d3b4f72d360883396f596699 -->
```bash
BASE="NLM_DRIVER;OUT_DOUBLE;VAR_RHO_2D;DIAGNOSTICS_UV;DIAGNOSTICS_TS"
[[ -n "$EXTRA" ]] && BASE="$BASE;$EXTRA"
[[ ";$BASE;" == *";ROMS_WALL_PROFILE;"* ]] || BASE="$BASE;ROMS_WALL_PROFILE"
DEFAULT_STAGES="KK_STAGE_FUSED;KK_STAGE_TAVG;KK_STAGE_COUP;KK_STAGE_MOMT;KK_STAGE_DIAG;KK_STAGE_STOR;"
DEFAULT_STAGES+="KK_STAGE_UV3DMIX;KK_STAGE_T3DMIX;KK_STAGE_PRSGRD;KK_STAGE_OMEGA;KK_STAGE_SETZETA;"
DEFAULT_STAGES+="KK_STAGE_STEP3DUV;KK_STAGE_GLS;KK_STAGE_PRE3D;KK_STAGE_REMAINDER"
STAGES=${KK_STAGES:-$DEFAULT_STAGES}
MPI=${MPI:-OFF}
SUFFIX=""
[[ "$MPI" == ON ]] && SUFFIX="-mpi"
mkdir -p logs build
for backend in ${BACKENDS:-reference serial sw64}; do
  KOKKOS=OFF
  TRACER=OFF
  FLAGS="$BASE"
  if [[ "$backend" != reference ]]; then
    KOKKOS=ON
    TRACER=${STEP3D_T:-ON}
    FLAGS="$BASE;$STAGES"
  fi
  tag="${APP}-${backend}${SUFFIX}${TAG_SUFFIX:-}"
  echo "=== configuring $tag"
  # The toolchain file selects mpif90 and swucxx.
  cmake -S "$ROOT" -B "$ROOT/build/$tag" \
    -DCMAKE_TOOLCHAIN_FILE="$ROOT/Compilers/toolchain_SW64.cmake" \
    -DROMS_KOKKOS_STEP3D_T="$TRACER" \
    -DCMAKE_BUILD_TYPE=Release -DROMS_FAST_MATH=OFF \
    -DROMS_KOKKOS_TESTS="${TESTS:-OFF}" \
    -DROMS_APP="$APP" -DMY_HEADER_DIR="$HEADER_DIR" \
    -DROMS_KOKKOS="$KOKKOS" -DROMS_KOKKOS_BACKEND="$backend" \
    -DMPI="$MPI" -DLIBTYPE=STATIC -DMY_CPP_FLAGS="$FLAGS" \
```

</details>

<a id="src-build-cpp-files"></a>

**source-block ID：`build-cpp-files`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/CMakeLists.txt`；闭区间：**1–32**；行数：**32**；UTF-8 payload SHA256：`453a8dd8e9159b1d5d08e99f61d7f6d69c34b61d7dfc532e60f4069491f7341c`。

<details>
<summary>ROMS/Cpp/CMakeLists.txt，1–32（完整原文，可展开）</summary>

<!-- source-block id=build-cpp-files commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/CMakeLists.txt lines=1-32 count=32 sha256=453a8dd8e9159b1d5d08e99f61d7f6d69c34b61d7dfc532e60f4069491f7341c -->
```cmake
# git $Id$
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Copyright (c) 2002-2025 The ROMS Group                                :::
#   Licensed under a MIT/X style license                                :::
#   See License_ROMS.md                                                 :::
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#
# Source code list for sub-directory "ROMS/Cpp"
#
# These files are only built when ROMS_KOKKOS is ON; the top-level
# CMakeLists.txt guards the add_subdirectory() call accordingly.

list( APPEND _files
      ROMS/Cpp/kokkos_step2d_mod.F
      ROMS/Cpp/kokkos_loops_mod.F
      ROMS/Cpp/kokkos_config.cpp
      ROMS/Cpp/kokkos_step2d.cpp
      ROMS/Cpp/kokkos_step3d_t_mod.F
      ROMS/Cpp/kokkos_step3d_t.cpp
      ROMS/Cpp/kokkos_rhs3d_mod.F
      ROMS/Cpp/kokkos_rhs3d.cpp
      ROMS/Cpp/kokkos_step3d_mod.F
      ROMS/Cpp/kokkos_step3d.cpp
    )

set ( ROMS_Cpp_files
      ${_files}
      PARENT_SCOPE
    )

# SW64 kernel flags are attached to the parent Objects target, which
# consumes these sources.  Source properties set here are directory-local.
```

</details>

### 14.4 直接 tracer 测试的真实比较与参数

以下完整当前测试定义保留比较阈值、非输出槽不变断言、独立 spline 稠密消元 oracle、37 列跨块参数与最终返回码；Fortran oracle 见仓库 `ROMS/Cpp/test_step3d_t_reference.F`。这里只展示静态内容，没有执行测试或生成编译产物。

<a id="src-test-tracer-current"></a>

**source-block ID：`test-tracer-current`**；commit：`7eb30805f51ff3b56e4020cba3a304ad7cd0006e`；路径：`ROMS/Cpp/test_step3d_t_kokkos.cpp`；闭区间：**1–165**；行数：**165**；UTF-8 payload SHA256：`520816b47aad269ab16c302d266bb5b3a0e309546984384ed57e6757fb1dd35a`。

<details>
<summary>ROMS/Cpp/test_step3d_t_kokkos.cpp，1–165（完整原文，可展开）</summary>

<!-- source-block id=test-tracer-current commit=7eb30805f51ff3b56e4020cba3a304ad7cd0006e path=ROMS/Cpp/test_step3d_t_kokkos.cpp lines=1-165 count=165 sha256=520816b47aad269ab16c302d266bb5b3a0e309546984384ed57e6757fb1dd35a -->
```cpp
/* Direct regression for the production tracer kernel k_step3d_t_chunks
** against a Fortran copy of the step3d_t loops (test_step3d_t_reference.F).
** The 37-column tile crosses the 32-column chunk boundary. */
#include "kokkos_step3d_t.hpp"
#include <algorithm>
#include <cmath>
#include <cstdio>
#include <vector>
extern "C" void reference_step3d_t(const roms::TracerConfig*, const int*,
                                   void*, void*, void*, void*, void*, void*, void*, void*, void*, void*);

bool run_case(int nx, int levels, int output, int periodic, int mode) {
    roms::TracerConfig p{};
    p.LBi = p.LBj = -3;
    p.UBi = nx + 4;
    p.UBj = 11;
    p.Istr = p.Jstr = 1;
    p.Iend = nx;
    p.Jend = 7;
    p.N = levels;
    p.NT = p.NAT = 2;
    p.NDT = 8;
    p.nnew = output;
    p.xadv = 1;
    p.yadv = 2;
    p.hadv = 3;
    p.vadv = 4;
    p.vdif = 5;
    p.west = p.east = p.south = p.north = 1;
    p.periodic_ew = periodic & 1;
    p.periodic_ns = (periodic >> 1) & 1;
    p.dt = 300;
    p.cc1 = 0.25;
    p.cc2 = 0.5;
    p.cc3 = 1.0 / 12.0;
    const int ni = p.UBi - p.LBi + 1, nj = p.UBj - p.LBj + 1;
    const std::size_t cells = ni * nj;
    std::vector<double> pm(cells), pn(cells), hz(cells * levels), hu = hz, hv = hz, zr = hz;
    std::vector<double> w(cells * (levels + 1)), akt(cells * (levels + 1) * 2);
    std::vector<double> t(cells * levels * 3 * 2), diag(cells * levels * 2 * p.NDT);
    auto a2 = [&](auto &a) { return roms::as_2d<double>(a.data(), p.LBi, p.UBi, p.LBj, p.UBj); };
    auto a3 = [&](auto &a, int low) { return roms::as_3d<double>(a.data(), p.LBi, p.UBi, p.LBj, p.UBj, low, levels); };
    auto pmv = a2(pm), pnv = a2(pn);
    auto h = a3(hz, 1), u = a3(hu, 1), v = a3(hv, 1), z = a3(zr, 1), wv = a3(w, 0);
    auto av = roms::as_4d<double>(akt.data(), p.LBi, p.UBi, p.LBj, p.UBj, 0, levels, 1, 2);
    auto tv = roms::as_5d<double>(t.data(), p.LBi, p.UBi, p.LBj, p.UBj, 1, levels, 1, 3, 1, 2);
    auto dv = roms::as_5d<double>(diag.data(), p.LBi, p.UBi, p.LBj, p.UBj, 1, levels, 1, 2, 1, p.NDT);
    for (int j = p.LBj; j <= p.UBj; ++j)
        for (int i = p.LBi; i <= p.UBi; ++i) {
            pmv(i, j) = 0.001 * (1 + 0.01 * i);
            pnv(i, j) = 0.0009 * (1 + 0.01 * j);
            wv(i, j, 0) = 0;
            for (int k = 1; k <= levels; ++k) {
                h(i, j, k) = 4 + 0.2 * k + 0.01 * i + 0.02 * j;
                z(i, j, k) = 5.0 * k + 0.01 * i + 0.02 * j;
                u(i, j, k) = (mode == 0 || mode == 3) ? 0 : mode == 2 ? 1300
                                                                      : 5000 * std::sin(0.3 * i + 0.2 * j + k);
                v(i, j, k) = (mode == 0 || mode == 3) ? 0 : mode == 2 ? -2000
                                                                      : 4000 * std::cos(0.2 * i - 0.3 * j + k);
                wv(i, j, k) = mode == 1 ? 100 * std::sin(0.3 * i + j + k) : 0;
                for (int it = 1; it <= 2; ++it) {
                    for (int state = 1; state <= 3; ++state) {
                        const double q = mode == 2 ? 10.0 * it : 10.0 * it + 0.03 * i * i + 0.02 * j * j + 0.1 * std::sin(0.5 * i + j + k) + 0.07 * k;
                        tv(i, j, k, state, it) = state == output ? h(i, j, k) * q : q + 0.1 * (state - 3);
                    }
                    for (int id = 1; id <= p.NDT; ++id)
                        dv(i, j, k, it, id) = 0.001 * id + 0.00001 * (i + j + k + it);
                }
            }
            for (int k = 0; k <= levels; ++k)
                for (int it = 1; it <= 2; ++it)
                    av(i, j, k, it) = mode == 0 ? 0 : 1e-4 * (1 + 0.01 * k + 0.001 * i + 0.002 * j + 0.1 * it);
        }
    int schemes[2] = {0, 1};
    const auto original = t;
    auto expected = t, expected_diag = diag;
    reference_step3d_t(&p, schemes, pm.data(), pn.data(), hz.data(), hu.data(), hv.data(),
                       w.data(), zr.data(), akt.data(), expected.data(), expected_diag.data());
    k_step3d_t_chunks(&p, schemes, pm.data(), pn.data(), hz.data(), hu.data(), hv.data(),
                      w.data(), zr.data(), akt.data(), t.data(), diag.data());
    bool ok = true;
    double error = 0, derr = 0, peak = 0, dpeak = 0;
    for (std::size_t k = 0; k < t.size(); ++k) {
        ok &= std::isfinite(t[k]) && std::isfinite(expected[k]);
        error = std::max(error, std::abs(t[k] - expected[k]));
        peak = std::max(peak, std::abs(expected[k]));
        const int state = int(k / (cells * levels)) % 3 + 1;
        if (state != output)
            ok &= t[k] == original[k];
    }
    for (std::size_t k = 0; k < diag.size(); ++k) {
        ok &= std::isfinite(diag[k]) && std::isfinite(expected_diag[k]);
        derr = std::max(derr, std::abs(diag[k] - expected_diag[k]));
        dpeak = std::max(dpeak, std::abs(expected_diag[k]));
    }
    ok &= error <= 1e-12 * std::max(1.0, peak) && derr <= 1e-12 * std::max(1.0, dpeak);
    if (mode == 3) {
        // Independently assemble and solve the spline system with dense pivoted
        // elimination. This is deliberately not the kernel's Thomas recurrence.
        const int i = 3, j = 3, m = levels - 1;
        auto old = roms::as_5d<double>(const_cast<double*>(original.data()), p.LBi, p.UBi,
                                       p.LBj, p.UBj, 1, levels, 1, 3, 1, 2);
        for (int it = 1; it <= 2; ++it) {
            std::vector<double> matrix(m * m, 0), rhs(m), concentration(levels + 1), solution(levels + 1, 0);
            for (int k = 1; k <= levels; ++k)
                concentration[k] = old(i, j, k, output, it) / h(i, j, k);
            for (int k = 1; k < levels; ++k) {
                const int row = k - 1;
                matrix[row * m + row] = (h(i, j, k) + h(i, j, k + 1)) / 3.0 +
                                        p.dt * av(i, j, k, it) * (1.0 / h(i, j, k) + 1.0 / h(i, j, k + 1));
                if (k > 1)
                    matrix[row * m + row - 1] = h(i, j, k) / 6.0 - p.dt * av(i, j, k - 1, it) / h(i, j, k);
                if (k < levels - 1)
                    matrix[row * m + row + 1] = h(i, j, k + 1) / 6.0 - p.dt * av(i, j, k + 1, it) / h(i, j, k + 1);
                rhs[row] = concentration[k + 1] - concentration[k];
            }
            for (int col = 0; col < m; ++col) {
                int pivot = col;
                for (int row = col + 1; row < m; ++row)
                    if (std::abs(matrix[row * m + col]) > std::abs(matrix[pivot * m + col]))
                        pivot = row;
                for (int q = col; q < m; ++q)
                    std::swap(matrix[col * m + q], matrix[pivot * m + q]);
                std::swap(rhs[col], rhs[pivot]);
                for (int row = col + 1; row < m; ++row) {
                    const double ratio = matrix[row * m + col] / matrix[col * m + col];
                    for (int q = col; q < m; ++q)
                        matrix[row * m + q] -= ratio * matrix[col * m + q];
                    rhs[row] -= ratio * rhs[col];
                }
            }
            for (int row = m - 1; row >= 0; --row) {
                double value = rhs[row];
                for (int q = row + 1; q < m; ++q)
                    value -= matrix[row * m + q] * solution[q + 1];
                solution[row + 1] = value / matrix[row * m + row];
            }
            for (int k = 1; k <= levels; ++k) {
                const double value = concentration[k] + p.dt / h(i, j, k) *
                                                            (av(i, j, k, it) * solution[k] - av(i, j, k - 1, it) * solution[k - 1]);
                ok &= std::abs(tv(i, j, k, output, it) - value) <= 1e-12 * std::max(1.0, std::abs(value));
            }
        }
    }

    std::printf("nx=%d N=%d nnew=%d periodic=%d mode=%d tracer=%.4e diag=%.4e %s\n",
                nx, levels, output, periodic, mode, error, derr, ok ? "PASS" : "FAIL");
    return ok;
}
int main() {
    roms::initialize();
    bool ok = true;
    for (int n : {4, 7, 16, 33})
        for (int output = 1; output <= 2; ++output)
            for (int periodic = 0; periodic < 4; ++periodic)
                for (int mode = 0; mode < 4; ++mode)
                    ok &= run_case(9, n, output, periodic, mode);
    for (int n : {4, 33})
        for (int output = 1; output <= 2; ++output)
            for (int periodic : {0, 3})
                for (int mode = 1; mode <= 2; ++mode)
                    ok &= run_case(37, n, output, periodic, mode);
    roms::finalize();
    return ok ? 0 : 1;
}
```

</details>

<a id="source-manifest"></a>

## 15. 来源清单与文本完整性核验

### 15.1 核验口径

本文件通过临时目录中的轻量 Python 脚本，以 `git show <完整commit>:<path>` 提取主仓库源码。Kokkos 依赖块的 commit 属于开头列出的子模块，表中路径仍用主仓库相对定位 `Kokkos/core/...`，复核时在 Kokkos 仓库执行 git show 并去掉该路径的 `Kokkos/` 前缀；不是把 gitlink 当成主仓库内的普通 blob。Python 使用 `-I`，不向仓库写入脚本或 __pycache__。每个 fenced code block 都对应下表唯一的 source-block，逐字节对照相应 Git blob 的闭区间原文，而不是只校验一个预先填入的哈希字符串。

本次 **PASS** 的文本/静态检查范围：

- 当前分支、完整 HEAD、共同未移植基线及三个文件与各自移植前父提交的一致性；当前主仓库文件与对应 HEAD blob 一致，Kokkos 文件与 HEAD gitlink 所指子模块 blob 一致。
- 每块的原 UTF-8 payload、首尾行号、行数、SHA256、块内元数据及来源清单一致；没有人工改写或新增代码占位。
- 五入口完整函数与全部 helper 覆盖；两个 C++ 文件按行号重组后无重复、无缺行且全文件 SHA256 等于 Git blob。
- 三个原始 Fortran 全文完整保留；正文相关计算范围和所有 CPP 分支逐字一致。完整相关 C++ 声明、BIND(C) 模块、Array*、HostWorkspace 和所列运行时依赖存在。
- Pre3dConfig、Uv3dConfig、TracerConfig 的 C++/BIND(C) 字段序列、类型和固定数组长度静态对应；没有将其误称编译后 ABI 布局测量。
- 围栏与 details 成对、source-block ID/显式锚点唯一、全部内部链接目标存在、UTF-8 解码成功；原始源段中出现的省略号没有被删改，也没有加入新占位源码。
- Git tracked/staged 内容未改；仅新增本文，不提交、不 push。

**NOT RUN**：当前 HEAD 的编译、预处理配置矩阵、C ABI sizeof/offsetof 实测、直接 kernel 数值测试、模型短/长积分、数值等价与性能实测、硬件缓存/带宽/向量化分析。历史 PASS 和近零诊断归一化未过的记录已在第 10 节注明，不计入本次 PASS。

核验能确认“文档与指定源码一致”和所列静态覆盖，不能由此证明所有原始 CPP 组合已经移植、所有 tile 都会调用 hook、或任意模型配置逐位等价。

### 15.2 源码块来源清单

闭区间和哈希以原文件为准，表中每项均可点击跳回对应源码块。

| source-block ID | 完整 commit | 路径 | 闭区间行号 | 行数 | 原 UTF-8 payload SHA256 |
|---|---|---|---:|---:|---|
| [flow-main3d](#src-flow-main3d) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/main3d.F` | 515–1062 | 548 | `d2f5faab528133399e4ebd2306d0600092db124ff52c208f76ab24cfb98580e1` |
| [flow-rhs3d](#src-flow-rhs3d) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/rhs3d.F` | 73–195 | 123 | `243a53ae736de7c57e2a3d9451d4ccfc69bcb3a6fbf702a2afd62b59a3437fa4` |
| [flow-dtfast](#src-flow-dtfast) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Utility/inp_par.F` | 621–632 | 12 | `2a989e2d2ff600c3e6f71d9935b1b42bbcc63bf83c4b5b865df57b414a0a556b` |
| [flow-fast-definitions](#src-flow-fast-definitions) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Modules/mod_scalars.F` | 204–216 | 13 | `e4856eb36eea48e5095ea33b38d82dc3a28f8df49396926a8e1d016f79bad75d` |
| [flow-filter-weights](#src-flow-filter-weights) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Utility/set_weights.F` | 35–196 | 162 | `8d4598d417c2575672dfc94c33f712fc0f492795b1f7618caa6ad1ba98d77c0d` |
| [timelevel-definitions](#src-timelevel-definitions) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Modules/mod_stepping.F` | 25–45 | 21 | `91a177dd8872de098bc140f9d03c5a9988706ce4a9d35667b246f2f244ad8ab6` |
| [original-pre-t](#src-original-pre-t) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/pre_step3d.F` | 318–933 | 616 | `ec06949f19ece4dc9a9385fb43e3346d3ceed17fd5a79e1eb8a0548667dcd8af` |
| [current-pre-t](#src-current-pre-t) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.cpp` | 404–552 | 149 | `234d01bcd29399951eb0ba41615e85e53b00daa46b8f2376a137cab6a7da5c6d` |
| [original-pre-uv](#src-original-pre-uv) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/pre_step3d.F` | 934–1144 | 211 | `f90b0962dd7d9856c245b0309ad2d8ab01e57ae3b93d10cc3c4e26e6b4398f2f` |
| [current-pre-uv](#src-current-pre-uv) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.cpp` | 553–614 | 62 | `d9a34f7e52ea2059772f6706a722efc0dd084bb8313bbe2fd8a4580a5ee5adcf` |
| [original-uv-solve](#src-original-uv-solve) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step3d_uv.F` | 325–1260 | 936 | `548c7719c60e68fbfb6ad3201a3afe00ee10ea9a8daab873bc501dc7393a7401` |
| [current-uv-solve](#src-current-uv-solve) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.cpp` | 1014–1075 | 62 | `da490fdcec66600a32d13adfbe8b618cb754ca25a79ec5603fe4ecdf6fae07c4` |
| [original-uv-couple](#src-original-uv-couple) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step3d_uv.F` | 1305–1756 | 452 | `0bdce07eab992b11a1022870ff8b2ea6bd675bacc6c76d0a57fb6b7d6d78b5c2` |
| [current-uv-couple](#src-current-uv-couple) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.cpp` | 1076–1140 | 65 | `ee153acf861cf30a14c6a01a554f2c32025872af853df051585ef0ede8e828bd` |
| [original-t-corrector](#src-original-t-corrector) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step3d_t.F` | 341–1799 | 1459 | `ed85d15e771bf55d79508b7561e112f7bc9ad8591cd679afd94818fd5f124397` |
| [current-t-chunks](#src-current-t-chunks) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d_t.cpp` | 149–239 | 91 | `0b86075d8ad7dde7ea2b8d84cf23754a509a34c4467925081f21225c41eae7e5` |
| [original-pre-full](#src-original-pre-full) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/pre_step3d.F` | 1–1182 | 1182 | `dae760b92740155c9cec5f086ae502e6638583af96e6bf6f5e09fbc254fc201a` |
| [original-uv-full](#src-original-uv-full) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step3d_uv.F` | 1–1846 | 1846 | `94a7107047fc94c351a9c874673d1092cfd46be836c2f4634cc0c6fc085a8316` |
| [original-t-full](#src-original-t-full) | `cf10c0f7cc270a7075ad19ee3b8950be352a7cd0` | `ROMS/Nonlinear/step3d_t.F` | 1–1976 | 1976 | `f59d48307a773e3cdf094371081e016bd2fe3caf37c76384ce137bc80f905e73` |
| [cpp-step3d-common](#src-cpp-step3d-common) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.cpp` | 1–82 | 82 | `ac9cf421a7e3c587573b9ab86d1d3f718033115dacca14a5ec8b9290ccc3d051` |
| [cpp-pre-t-fields](#src-cpp-pre-t-fields) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.cpp` | 83–297 | 215 | `8f7279c01e395002901bd6e9969a071278fea0efd9e5c662d50aa69cec2f1510` |
| [cpp-pre-uv-fields](#src-cpp-pre-uv-fields) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.cpp` | 298–403 | 106 | `9f5b7dd5f16ee1b7f7698c3f4973817f4d859cee992a9300b4ce34bfd9600eab` |
| [cpp-uv-fields-prefix](#src-cpp-uv-fields-prefix) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.cpp` | 615–673 | 59 | `a90fb38e9733541e419266102260a46fdef783d6d7323d53ddaefce4e147f9e4` |
| [cpp-uv-solve-row](#src-cpp-uv-solve-row) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.cpp` | 674–921 | 248 | `c6329a2d49e64df9cc1d71ec5d956f1f2d4462a48dee59ca00dd945d124e181b` |
| [cpp-uv-couple-row](#src-cpp-uv-couple-row) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.cpp` | 922–1013 | 92 | `306dad65e115812c111b4b226a5de7638f96113b61b7cd4845308b835747d479` |
| [cpp-step3d-tail](#src-cpp-step3d-tail) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.cpp` | 1141–1144 | 4 | `3e7a994e16b45e66e0e71c9cacf7f119e9fc579be38c198aa4089bf96a6e0716` |
| [cpp-t-fields](#src-cpp-t-fields) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d_t.cpp` | 1–148 | 148 | `be27db0ed13a817a246ce9cf4579b0b7153d1230984b9b918ff8f20ea706e53c` |
| [cpp-t-tail](#src-cpp-t-tail) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d_t.cpp` | 240–241 | 2 | `626de321eb397f6600939892a0009f31bc378f7e886f9acc5ec0af18db6a99a7` |
| [abi-step3d-hpp](#src-abi-step3d-hpp) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d.hpp` | 1–120 | 120 | `e0a28b1698cf91a9761db2bddbfc9e2578bd53a6588de21f8dc042ef45a207ad` |
| [abi-tracer-hpp](#src-abi-tracer-hpp) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d_t.hpp` | 1–24 | 24 | `7aeeea786a89e741828ab6ff76e5966b1abe6253d7b3da5ba1f3cea8344e3b89` |
| [abi-step3d-fortran](#src-abi-step3d-fortran) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d_mod.F` | 1–117 | 117 | `c8b813ad32accf3a17dfc2858349370c015cb85f7ce7945a40b3b5a4c0eadad2` |
| [abi-tracer-fortran](#src-abi-tracer-fortran) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_step3d_t_mod.F` | 1–141 | 141 | `836447cdefafb18fd1ea9e8416370d38c180ddb462265f801e123aec8218e82d` |
| [caller-pre-declarations-hook](#src-caller-pre-declarations-hook) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/pre_step3d.F` | 1–462 | 462 | `22e148a878db645649cc935fca0fc0dac64021adc19520a07e6429631f7d8c21` |
| [caller-pre-common-epilogue](#src-caller-pre-common-epilogue) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/pre_step3d.F` | 1291–1331 | 41 | `df2ebcfd7c843289b6e2e0ee71cb2a5fd8e25fa9f410e10dd84d124a22d8302e` |
| [caller-uv-declarations-solve](#src-caller-uv-declarations-solve) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step3d_uv.F` | 1–463 | 463 | `b39ac635e1c3592adcacb9778cc95b0d0a558e7525a5583b469e6419415acedd` |
| [caller-uv-bc-sources-couple](#src-caller-uv-bc-sources-couple) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step3d_uv.F` | 1400–1468 | 69 | `cb2ea50ce18091c0c41b613bf94f2200cad9e782158752d2416f1b60f84be320` |
| [caller-uv-common-exchange](#src-caller-uv-common-exchange) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step3d_uv.F` | 1914–2004 | 91 | `1b0944949c61a4024402eea20ce8a5554f17dbb031bf030851b124e1dec75faf` |
| [caller-t-declarations-eligibility](#src-caller-t-declarations-eligibility) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step3d_t.F` | 1–507 | 507 | `e3578d03fe22698687be8a5520e751f6b0d6fd0dffd5a56cfbacd408b65f4dd2` |
| [caller-t-common-epilogue](#src-caller-t-common-epilogue) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Nonlinear/step3d_t.F` | 1941–2121 | 181 | `474e91287f651dc172e82a972d398bdf633f53b9e512bfe358374e55688931cc` |
| [runtime-config-hpp](#src-runtime-config-hpp) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_config.hpp` | 1–346 | 346 | `59a3be05fa17305fe9778535ae99bcb1afd01ed709bc5766bd4600c1eaaa8c8e` |
| [runtime-workspace-hpp](#src-runtime-workspace-hpp) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_workspace.hpp` | 1–31 | 31 | `96ce40f7d433e8b021f2c51ea21911af17acaec7d70d80373f81559fff55c4b0` |
| [runtime-config-cpp](#src-runtime-config-cpp) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/kokkos_config.cpp` | 1–130 | 130 | `bae8e44d7e161134b1edb90bfd4f56a41d530ea07c0bb79e91bf1de927a1aa58` |
| [runtime-sw64-range](#src-runtime-sw64-range) | `6e40ad34a8968a44386e01a5a757e1e37bfcacea` | `Kokkos/core/src/SW64/Kokkos_SW64_Parallel_Range.hpp` | 1–74 | 74 | `7c9f181fd558498dbc85f6ade8e528f43f6bb35c0613f934f7e1b53b38b8f6ba` |
| [runtime-sw64-launch](#src-runtime-sw64-launch) | `6e40ad34a8968a44386e01a5a757e1e37bfcacea` | `Kokkos/core/src/SW64/Kokkos_SW64_KernelLaunch.hpp` | 1–45 | 45 | `9fd84474493df3581f99203449b2de183acbc0fec8f83a78525bfdb9f73d5e62` |
| [runtime-sw64-space](#src-runtime-sw64-space) | `6e40ad34a8968a44386e01a5a757e1e37bfcacea` | `Kokkos/core/src/SW64/Kokkos_SW64.hpp` | 38–55 | 18 | `fe5fa44a40f0df2a864f63d323f66995cc10d26ee70acbe494e15443256e1e31` |
| [runtime-sw64-setup](#src-runtime-sw64-setup) | `6e40ad34a8968a44386e01a5a757e1e37bfcacea` | `Kokkos/core/src/setup/Kokkos_Setup_SW64.hpp` | 1–58 | 58 | `3911f02a9e7b7c19958409244ab1a601955dad704568b1e0d01c277b3460d3ea` |
| [build-cmake-tracer-option](#src-build-cmake-tracer-option) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `CMakeLists.txt` | 55–102 | 48 | `5e76572ea7910a42f8d6295dfda80722cdc4b932083f99cf920a8fc3521db737` |
| [build-cmake-kernel-flags-tests](#src-build-cmake-kernel-flags-tests) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `CMakeLists.txt` | 394–470 | 77 | `802b3b2795a92f21f531c159ee611ed7ad84942f98380a46df2a094b0cf80893` |
| [build-sw64-toolchain](#src-build-sw64-toolchain) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `Compilers/toolchain_SW64.cmake` | 1–15 | 15 | `10b94655b6339c52251ef73add8ff885aeb707dea59e558d34553986519e44b4` |
| [build-gnu-math-flags](#src-build-gnu-math-flags) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `Compilers/compiler_flags_GNU_Fortran.cmake` | 29–68 | 40 | `c893d078c77bd87dc6ebfc4ef5fd26f39f11130b2c7885bc1149d8aa365f5b80` |
| [build-app-stage-strict](#src-build-app-stage-strict) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/tools/sw64/build_app.sh` | 48–78 | 31 | `1f0da14a06421b357128bb63eb496969e08b5370d3b4f72d360883396f596699` |
| [build-cpp-files](#src-build-cpp-files) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/CMakeLists.txt` | 1–32 | 32 | `453a8dd8e9159b1d5d08e99f61d7f6d69c34b61d7dfc532e60f4069491f7341c` |
| [test-tracer-current](#src-test-tracer-current) | `7eb30805f51ff3b56e4020cba3a304ad7cd0006e` | `ROMS/Cpp/test_step3d_t_kokkos.cpp` | 1–165 | 165 | `520816b47aad269ab16c302d266bb5b3a0e309546984384ed57e6757fb1dd35a` |

### 15.3 当前 C++ 全文件重组指纹

| 文件 | 总行数 | 原 Git blob UTF-8 SHA256 |
|---|---:|---|
| `ROMS/Cpp/kokkos_step3d.cpp` | 1144 | `b3ffd1bb6141a09aa22e1b4a5ce97ff99bcd52575e2420af80dc15f8b3dbe925` |
| `ROMS/Cpp/kokkos_step3d_t.cpp` | 241 | `7f1624a6ccfaeca8190c409bb9bb1fe3c8eeee31ae0f192f028a67c5b2466b17` |

