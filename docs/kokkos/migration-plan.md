# Kokkos 优化迁移方案：`feat/kokkos.share` → `feat/kokkos`

状态：草案，2026-11-13。本文档不修改任何代码。

配套记录：`feat/kokkos.share` 上的 `docs/kokkos-experiments-2026-11-06.md`，
以及申威上的 `~/voyage/roms/work/rhs-opt-20261113/`。

## 1. 摘要

- `feat/kokkos`（`81fcff65`）是 `feat/kokkos.share`（`822e0160`）的严格祖先。
  优化工作是当前分支之上的 8 个线性提交，迁移不需要解决任何冲突：直接快进
  即可得到与已验证树完全一致的提交。
- 默认生产构建下，迁移只改变一条算术路径：`k_step2d_rhs_fused` 中的 xi 面
  通量复用（`0bb237a7`）。另外两处代码改动在构造上就是不生效的：tracer 行块
  只在定义 `ROMS_KK_TEAM_STEP3D_T` 时编译（默认构建不定义），
  `k_step2d_diag_int_blocked` 是一个模型永远不会调用的新入口。
- 推荐路径：快进（路径 A），然后在合并后的树上重跑内核门禁和四步整模门禁。
  现有端到端门禁只做到 `0bb237a7`；合并树（多了 diag 死代码和条件编译的
  step3d_t 代码块）还没有过这道门禁。
- 如果实验性代码不应进入该分支，路径 B 只迁移 xi 提交。已核实该补丁能干净
  应用到 `feat/kokkos`。
- 尚未 push。`feat/kokkos.share` 领先其 origin 7 个提交（`9e432654` 已发布）；
  `feat/kokkos` 与 `origin/feat/kokkos` 齐平，除未跟踪的 `.claude/` 和
  `.worktrees/` 外工作区干净。

## 2. 分支状态（2026-11-13 核实）

| 分支 | HEAD | 跟踪关系 | 备注 |
|---|---|---|---|
| `feat/kokkos` | `81fcff65` docs: add the strong-scaling and size-sweep figures | 与 `origin/feat/kokkos` 齐平 | 干净；未跟踪 `.claude/`、`.worktrees/` |
| `feat/kokkos.share` | `822e0160` docs: record the scheduling and diag_int experiments | 领先 `origin/feat/kokkos.share` 7 个提交（origin 为 `9e432654`） | `81fcff65` 的线性后代 |

范围 `81fcff65..822e0160`：8 个提交，10 个文件，+2901 / −36。

| 提交 | 标题 | 涉及文件 | 行数 |
|---|---|---|---|
| `9e432654` | feat(kokkos): row-block sharing on the tracer corrector and the 2-D rhs | CMake、2 个 bench 源文件、`kokkos_step2d.{cpp,hpp}`、`kokkos_step3d_t.{cpp,hpp}`、README、docs | +1717 / −5 |
| `7b816d3b` | test(kokkos): enforce bitwise checks in the 2-D rhs benchmark | `bench_step2d_rhs_kokkos.cpp` | +194 / −27 |
| `0bb237a7` | perf(kokkos): carry complete xi advection faces in the 2-D rhs | `kokkos_step2d.cpp`、docs | +126 / −31 |
| `1444b66f` | docs: record the negative U/V viscous-sharing experiment | docs | 仅文档 |
| `47dfc0f4` | docs: record the end-to-end Fortran check of the xi carry | docs | 仅文档 |
| `bd840e73` | docs: record the aliasing diagnosis and the vectorization probe | docs | 仅文档 |
| `cf7bd663` | perf(kokkos): add an i-blocked diag_int variant and its benchmark | CMake、新 bench、`kokkos_step2d.{cpp,hpp}` | +601 |
| `822e0160` | docs: record the scheduling and diag_int experiments | docs | 仅文档 |

`9e432654` 已经存在于 `origin/feat/kokkos.share`；其余 7 个提交只存在于本地仓库。
两个未跟踪的 worktree（`.claude/worktrees/`、`.worktrees/`）和申威上的证据
目录都是实验记录，不是迁移内容。

## 3. 各项改动的内容与处置

### 3.1 已采纳的生产改动：xi 面通量复用（`0bb237a7`）

`k_step2d_rhs_fused`（share 分支上位于 `ROMS/Cpp/kokkos_step2d.cpp:1363`）现在
在 U 行和 V 行内滚动携带完整的 xi 平流通量：`gxm/gx0/dxm/dx0/ufxm/uRm` 在每行
开头播种一次，随后用 `gx0 = gx1; dx0 = dx1; ...` 滚动，使每个内部面只计算一次，
而不是两到三次。只有生产路径 `rows = 0` 改变；R=2/4 的行块变体（仅 bench）不受
影响。该改动没有运行期开关，回滚只能 `git revert 0bb237a7`。

证据：

- 单进程、单次 Kokkos 初始化的内核 A/B，job `9198626` 与 `9198728`，节点
  `vn033098`，严格使用 `-mieee -O3 -fno-fast-math -fno-tree-vectorize`。
  每个 job 245 对快照，四组输出（含 halo）全部 `memcmp` 相等且有限，
  覆盖 ±0 符号零用例。比值（旧/新，越大越好）：UPWELLING 533x1040
  `1.00635/1.00613`，BL_TEST 1599x164 `1.01196/1.01115`，canyon 440x1000
  `1.00531/1.00533`，240x352 tile `1.01045/1.01031`（首次/重复）。
- 对原始 Fortran 的端到端对照：`UPWELLING` 41x80x16，单 rank，DT 300，
  NDTFAST 30，HIS/AVG/DIA 全开。两步 job `9226298`/`9226324`（另有重复
  `9226689`/`9226701`），四步 job `9227546`/`9227580`；所有 job 退出码 0 并输出
  `ROMS:DONE`。`nccmp --all-time --rtol 1e-10 --zero-tol 1e-20` 覆盖 101 个
  字段：`maxabs = 0`、`maxnormrel = 0`，四步输出有 5 个时间记录；336 次
  `rhs_fused` 调用，无整块 Fortran 回退。
- 收益上限：`rhs_fused` 约占基准步的 19%，0.5–1.2% 的内核增益最多相当于
  整步的约 0.1–0.2%。该改动没有做过模型计时，因此不给出整步加速声明。

### 3.2 条件生效的生产改动：tracer 行块（`9e432654`）

只有在构建定义了 `ROMS_KK_TEAM_STEP3D_T` 时，`k_step3d_t_chunks` 才会把工作
分派给 `k_step3d_t_rows`（share 分支上见 `ROMS/Cpp/kokkos_step3d_t.cpp:152-172`）。
默认构建不定义该宏：CMake 里它只作为 `PRIVATE` 定义加在
`bench_step3d_t_kokkos` 目标上。要为模型启用，需要把该宏加入应用的 CPP 选项
（例如 `build_app.sh` 的 `EXTRA_CPP_FLAGS` 参数）；这样该构建默认走行块，
作业环境里 `ROMS_KK_T_ROWS=0` 可恢复 chunk 内核，`ROMS_KK_T_ROWS=2|4` 覆盖
块大小。

内核 bench 结果（全部逐位相等）：UPWELLING 533x1040x16 为 1.23x，
BL_TEST 1599x164x30 为 1.21x，240x352 tile 为 1.07x，canyon 形状的
440x1000x25 为 0.64x。同节点模型 A/B：`step3d_t` 0.5356 s → 0.4798 s
（1.12x），内核总和 8.9303 s → 8.8847 s（−0.51%），历史文件所有字段逐位
一致，仅 1.1 GB 文件中 netCDF 头部时间戳处相差 1 字节。

只有 UPWELLING 和 BL_TEST 定义了 `SPLINES_VDIFF` 并调用该内核；该区域分别
占 9.33 s 步中的 0.578 s 和 9.19 s 步中的 0.389 s，因此内核 1.2x 约等于
整步的 1.0% 和 0.7%。

同一提交还以仅 bench 实验的形式带入了 `k_step3d_t_team`、
`k_step3d_t_team_pipe` 和 `k_step2d_rhs_fused_rows`（位于
`ROMS_KK_RHS_ROWS` 之后）。它们都没有接入生产路径。TeamPolicy 相关路径
需要打过补丁的 Kokkos 副本以及运行时的
`-ldm_share_mode 5 -ldm_share_size 64`；仓库内固定的 Kokkos 子树没有改动。

### 3.3 实验性、未被调用：i-blocked diag_int（`cf7bd663`）

`k_step2d_diag_int_blocked`（share 分支上 `kokkos_step2d.cpp:1109`）保持每个
单元表达式和写入顺序不变，只把 `pm(i-1,j)+pm(i,j)` 与
`pn(i,j)+pn(i,j-1)` 提出诊断索引循环，在 `NDM2d-1` 个诊断项之间复用。
块宽为 `ROMS_KK_DIAG_BLOCK`（默认 16；`<= 1` 选择逐单元回退）。模型仍然调用
`k_step2d_diag_int`，新入口在所有构建中都是死代码。

证据：5 种块宽（1、8、16、32、64），13 个正确性用例，四组输出全部逐位相等，
无非有限值（job `9229432`–`9229436`）。内核比值在四种形状上为 1.025–1.073
（块宽 8–64）。该入口的模型级比较 **NOT RUN**，不给出整步收益声明。

### 3.4 测试与文档提交

- `7b816d3b` 让 `bench_step2d_rhs_kokkos` 变严格：对每个输出做 `memcmp`
  （而不是容差比较）、按指数位扫描非有限值、失败返回错误码、非法 CLI 输出
  用法错误、`--self-test` 含 20 项检查，并强制设置 `ROMS_KK_RHS_ROWS=0`，
  保证 bench 始终对比生产内核。
- `1444b66f`、`47dfc0f4`、`bd840e73`、`822e0160`，以及 `0bb237a7` 和
  `9e432654` 中的文档部分，记录了全部实验，包括负结果和收益不足的结果。
- `ROMS/Cpp/tools/sw64/README.md` 第 8 节（Decisions）更新了大 tile /
  TeamPolicy 的证据，属于 `9e432654`。

### 3.5 不在分支内：已否决方向

以下方向只以文档或证据形式存在，迁移时不得作为代码带入：

| 方向 | 结果 | 记录位置 |
|---|---|---|
| U/V 黏性应力共享（`rhs_fused`） | 逐位相等，但 440x1000 上慢 4.5x；否决 | 2026-11-13 文档（提交 `1444b66f`），job `9226934` |
| 条带主序调度（strip-major） | 宽度 128 时 1.13–1.17x，canyon 退化到 0.94；运行期分派修好 canyon，但双函数代码生成损失 4–5%（1.08–1.12），收益不足 | 文档“Work-item scheduling”一节，job `9228813`/`9228906`/`9228989`/`9229069`/`9230585` |
| 向量化开关 | 另外 6 个内核被向量化，`rhs_fused` 没有；1.0004x | 文档对应小节，job `9228421` |
| `rhs_fused` 行块变体作为生产路径 | 逐位相等，R=2 时 0.92–1.12x，高瘦 tile 上 R=4 为 0.54x；未接入 | 文档“The same sharing … does not pay”一节 |
| stress 原型（独立 worktree） | UP 1.068x、BL 0.999x、canyon 0.222x；否决 | `ab-stress-v1-single/result.json` |
| step2d 上的 TeamPolicy | 早期实验 0.58–0.86x；此前已移除 | README 第 8 节 |

## 4. 迁移后默认生产构建的影响

| 范围 | 改动 | 默认构建影响 |
|---|---|---|
| `k_step2d_rhs_fused`（`rows = 0`） | xi 面通量复用 | **算术变化**；所有测试形状与旧内核逐位一致，端到端 2 步/4 步与 Fortran 一致 |
| `k_step3d_t_chunks` | `ROMS_KK_TEAM_STEP3D_T` 下的分派块 | 无；默认不定义该宏 |
| `k_step3d_t_team` / `_team_pipe` / `_rows` | 同一宏下的新入口 | 无；默认不编译 |
| `k_step2d_rhs_fused_rows` | `ROMS_KK_RHS_ROWS` 下的新入口 | 无；不请求就不编译 |
| `k_step2d_diag_int_blocked` | 新的全局入口 | 运行期无影响；每个构建多一份目标代码 |
| `bench_step2d_rhs_kokkos.cpp`、`bench_step2d_diag_kokkos.cpp`、`bench_step3d_t_kokkos.cpp` | 新源文件 | 仅在 `ROMS_KOKKOS_TESTS=ON`（及对应 bench 选项）下构建 |
| CMake | `ROMS_KOKKOS_RHS_BENCH`、`ROMS_KOKKOS_DIAG_BENCH`、`bench_step3d_t_kokkos` 目标 | 默认 OFF |
| `docs/`、README | 文本 | 无 |

迁移不包含任何 Fortran、MPI、Kokkos 子模块或依赖改动。预期 diff 只涉及
`ROMS/Cpp/kokkos_*.cpp`、`ROMS/Cpp/bench_*.cpp`、`CMakeLists.txt`、
`ROMS/Cpp/tools/sw64/README.md` 和 `docs/kokkos-experiments-2026-11-06.md`。

## 5. 迁移路径

### 路径 A（推荐）：快进合并

因为 `feat/kokkos` 是 `feat/kokkos.share` 的祖先，快进会保留提交身份，且不可能
产生冲突。

```sh
git switch feat/kokkos
git merge-base --is-ancestor HEAD feat/kokkos.share && echo OK
git merge --ff-only feat/kokkos.share
git log --oneline -9
git diff --stat 81fcff65..HEAD
```

预期：`HEAD == 822e0160`；`git diff --stat` 显示 10 个文件、+2901/−36。
合并在本地完成；此后 `feat/kokkos` 领先 `origin/feat/kokkos` 8 个提交。

如果 bench 代码、实验性 diag 入口和条件编译的 tracer 行块可以留在主 Kokkos
分支上，选这条路径。代价是历史噪音和约 1900 行实验/bench 代码；好处是所有
已验证产物都能从分支直接复现。

### 路径 B：只 cherry-pick 生产改动

如果分支只应携带已验证的算术改动：

```sh
git switch feat/kokkos
git cherry-pick -n 0bb237a7          # 两个 hunk 均可干净应用（已检查）
git commit -c 0bb237a7               # 修改提交信息：仓库基线是 81fcff65
```

今天对 `feat/kokkos` 执行 `git apply --check`，内核补丁和文档补丁都能通过，
因此预计不需要手工解决冲突。文档 hunk 引用了后续提交（`1444b66f` 等）记录的
实验；要么一并带入那些文档提交，要么把该节裁剪为只保留内核证据。

限制：`bench_step2d_rhs_kokkos.cpp` 由 `9e432654` 引入，因此严格的 bench
（`7b816d3b`）无法在不带 `9e432654` 的情况下 cherry-pick。路径 B 迁移了代码
改动和文档记录，但不包含可复现的 bench 语料。

### 路径 C：快进后删除 diag 入口

如果不想让实验性 diag 入口留在树里：

```sh
git merge --ff-only feat/kokkos.share
git revert --no-commit cf7bd663
# 编辑 docs/kokkos-experiments-2026-11-06.md：822e0160 的那一节描述了一个
# 已经不存在的入口
git commit -m "revert: drop the unvalidated diag_int experimental entry"
```

这保留了已验证语料，但多出一个 revert 提交且需要改文档；除非生产目标文件里的
死代码被视为真实成本，否则不推荐。

## 6. 执行计划（路径 A）

### 阶段 0 — 预检（本地）

1. 确认 `git status` 只有 `.claude/` 和 `.worktrees/` 未跟踪；不要
   `git add -A`。
2. 记录 `git rev-parse HEAD`（`81fcff65`）作为回滚点，
   `git rev-parse feat/kokkos.share`（`822e0160`）作为目标。
3. 确认 `.claude/worktrees/` 和 `.worktrees/` 的工作副本不参与构建；
   它们只是证据。

### 阶段 1 — 快进

执行路径 A 的命令。不要 push。

### 阶段 2 — 申威构建

1. 创建 `~/voyage/roms/work/migrate-20261113/`，按文档约定制作快照
   （分支快照，`Kokkos` 符号链接到固定版本；不要触碰 `~/voyage/kokkos`、
   共享安装或固定的子模块）。
2. 用严格算术选项构建 `UPWELLING`（41x80x16），`BACKENDS=sw64 MPI=OFF`：
   `-mieee -O3 -DNDEBUG -std=gnu++1z -mhybrid-coding -fpermissive
   -fno-fast-math -fno-tree-vectorize`，`ROMS_FAST_MATH=OFF`，提交时
   `-cache_size 128`。
3. 用 `ROMS_KOKKOS_TESTS=ON`、`ROMS_KOKKOS_RHS_BENCH=ON`、
   `ROMS_KOKKOS_DIAG_BENCH=ON` 构建 bench。开启 `ROMS_KOKKOS_STEP3D_T` 时，
   step3d_t bench 会自动构建。
4. 运行工具测试：
   `python3 -m unittest discover -s ROMS/Cpp/tools -p 'test_*.py'`
   （预期 82 通过、2 跳过，跳过原因是缺少 cmake）。

### 阶段 3 — 内核门禁（job ID 记入新的 `result.json`）

| 门禁 | 命令 | 判据 |
|---|---|---|
| Bench 自测 | `bench_step2d_rhs_kokkos --self-test` | 20/20 检查通过，退出码 0 |
| RHS A/B | `bench_step2d_rhs_kokkos`，形状 533x1040、1599x164、440x1000、240x352 | 245 对 `memcmp` 相等且有限；四种形状比值均 ≥ 1.0；与记录的 1.005–1.012 对比 |
| Diag bench | `bench_step2d_diag_kokkos`，块宽 1/8/16/32/64 | 13 用例 × 5 块宽逐位相等；计时仅作参考（1.02–1.07） |
| step3d_t bench | `bench_step3d_t_kokkos`，chunks 对 rows | 逐位相等；UP/BL 上行块约 1.2x，canyon 约 0.64x；team 路径只有在打过补丁的 fork 和 LDM 选项下才运行 |

440x1000 必须保留在每一套 RHS 套件中：它是别名病理用例，条带和行块实验都在
这里出现过退化。

### 阶段 4 — 合并树的四步整模门禁

这是现有记录缺失的门禁：端到端对照是在 `0bb237a7` 上做的，早于
`bd840e73`/`cf7bd663`/`822e0160`。

1. 使用 `UPWELLING` 41x80x16，单 rank，1x1 tile，`DT=300`，`NDTFAST=30`，
   `NTIMES=4`，HIS/AVG/DIA 全开，输入 SHA `aa219a...`（已验收的四步输入）。
2. 在同一队列和节点类型上运行原始 Fortran 参考和迁移后的 SW64 模型：
   `q_share1`，1 个 MPE、64 个 CPE、128 KB cache，`OMP_NUM_THREADS=1`，
   `ROMS_KK_RHS_ROWS=0`，`ROMS_KK_TIMING=1`。直接提交原生二进制
   （`bsub -i <input> <binary>`），不要 shell 或 time 包装。
3. 用 `cmp_app.sh` → `nccmp.py --all-time --rtol 1e-10 --zero-tol 1e-20`
   比较：101 个字段，期望 `maxabs = 0`、`maxnormrel = 0`。
4. 检查两侧日志：`ROMS:DONE`、退出码 0、336 次 `rhs_fused` 调用、无整块
   Fortran 回退。

### 阶段 5 — 可选：按应用启用 tracer 行块

只针对 `UPWELLING` 和 `BL_TEST`（两个定义了 `SPLINES_VDIFF` 的头文件）：

1. 在应用的 CPP 选项中加 `ROMS_KK_TEAM_STEP3D_T` 后重新构建
   （`build_app.sh UPWELLING ROMS_KK_TEAM_STEP3D_T`）；确认生成的
   `cppdefs.h` 含该宏，且模型走行块（`ROMS_KK_T_ROWS=0` 必须能恢复
   chunk 内核）。
2. 重跑上面的四步 Fortran 门禁。预期字段逐位一致（已记录的模型 A/B 中只有
   netCDF 头部时间戳 1 字节不同）。
3. 门禁通过后，为这两个应用启用该宏并记录决定；否则保持关闭，把内核作为
   可选特性保留。

### 阶段 6 — 收尾

1. 如果需要修复，用常规提交信息提交
   （`Co-Authored-By: Claude Code <noreply@anthropic.com>`）。
2. 不要 push。汇报新的 HEAD 和相对 `81fcff65` 的 `git diff --stat`。
3. 除非被要求，本方案文档和申威证据保持未提交。

## 7. 验收标准

只有合并树上以下各项全部成立，迁移才算通过：

- [ ] `git merge --ff-only` 完成；`HEAD == 822e0160`；无冲突、未 push。
- [ ] 严格 SW64 构建 `UPWELLING` 成功；没有为了通过构建而修改源码。
- [ ] `bench_step2d_rhs_kokkos --self-test` 通过 20/20。
- [ ] RHS A/B：245 对快照全部逐位相等且有限；没有形状跌破 1.0；比值落在
      记录的 1.005–1.012 区间（换节点只应改变绝对时间，不改变方向）。
- [ ] Diag bench：13 用例 × 5 块宽逐位相等。
- [ ] 四步整模门禁：101 个字段，`maxabs = 0`、`maxnormrel = 0`，5 个时间
      记录，`ROMS:DONE`，无回退，336 次 `rhs_fused` 调用。
- [ ] 工具测试保持 82 通过 / 2 跳过。
- [ ] `git diff --stat 81fcff65..HEAD` 只涉及预期的 C++/CMake/文档文件，
      没有 Fortran、MPI 或 `Kokkos/` 子模块改动。

## 8. 风险与缓解

1. **合并树没有端到端验证。** 四步门禁早于 `cf7bd663` 和文档/bench 提交。
   缓解：阶段 4 为强制项。
2. **条件生效的行块。** 如果构建加入 `ROMS_KK_TEAM_STEP3D_T`，`step3d_t`
   的生产算术路径会改变。bench 和模型 A/B 都逐位相等，但没有记录它自己的
   四步 Fortran 门禁。缓解：默认关闭；启用前执行阶段 5。
3. **TeamPolicy 可复现性。** `k_step3d_t_team`/`_team_pipe` 需要打过补丁的
   Kokkos fork 副本和 `-ldm_share_mode 5 -ldm_share_size 64`；固定的子模块
   没有 3.75 MB 池宏。缓解：把这些测量视为工作目录证据，不声称能从分支
   复现，也不要升级子模块。
4. **死代码。** `k_step2d_diag_int_blocked` 被编译进每个模型却从不调用，
   存在腐化风险。缓解：路径 C（删除），或保留并用 bench 和文档锁定其契约。
5. **链接行注意事项。** 文档中的有效 `-Wl,-zmuldefs` 说明针对的是单进程
   A/B 驱动（链接了两份内核）。模型构建不使用该驱动，也不存在重复定义。
   不要脱离上下文引用该说明。
6. **运行期环境变量缓存。** `ROMS_KK_T_ROWS` 在进程内只读一次到静态变量；
   在第一次 `step3d_t` 调用之后设置不会生效。bench 驱动在初始化前设置。
   启用时要记录这一点。
7. **未跟踪产物。** `.claude/`、`.worktrees/` 和申威工作目录不得暂存；
   一条 `git add -A` 会把 worktree 内部文件提交进去。
8. **不做模型级性能声明。** xi 复用只有内核证据（约 0.5–1.2%），tracer 行块
   在两类应用上的上限是内核 1.12x / 整步约 1.0%。已验收门禁没有测模型墙钟，
   不要把迁移描述成模型加速。
9. **历史整洁性。** `feat/kokkos` 会增加 8 个提交（含实验记录），并与
   `origin/feat/kokkos` 分叉，在获批前保持未 push。

## 9. 回滚

- 合并前：目标树是 `81fcff65`，无需回滚。
- 快进后、产生新提交前：`git reset --hard 81fcff65` 可恢复分支。这是破坏性
  操作且只作用于本地；分支一旦发布就不得使用。
- 按改动回滚：`git revert 0bb237a7` 恢复生产 xi 路径；
  `git revert cf7bd663` 删除 diag 入口及其 bench；
  `git revert 9e432654` 删除行块语料和 README 决策更新。
- 运行期：带行块的构建里 `ROMS_KK_T_ROWS=0` 选择 chunk 内核。xi 复用没有
  运行期开关。

## 10. 待决策项

1. **路径 A 还是 B**：完整语料（实验可复现、历史更多）还是只迁 xi
   （生产改动最小、无 bench 语料）。
2. **Diag 入口**：保留 `k_step2d_diag_int_blocked` 作为实验入口，还是删除
   （路径 C）。
3. **Tracer 行块**：保持可选，还是在阶段 5 的四步门禁后为 UPWELLING/BL_TEST
   启用。
4. **方案文档位置**：本文件在仓库根目录且未跟踪；最终提交前移到 `docs/`
   或丢弃。

## 11. 附录

### `feat/kokkos.share` 上的文件/行号索引

- `ROMS/Cpp/kokkos_step2d.cpp`：`k_step2d_diag_int` 1002、
  `k_step2d_diag_int_blocked` 1109、`k_step2d_rhs_fused` 1363（xi 复用）、
  `k_step2d_rhs_fused_rows` 2131（位于 `ROMS_KK_RHS_ROWS` 之后）。
- `ROMS/Cpp/kokkos_step3d_t.cpp`：分派块 152–172（位于
  `ROMS_KK_TEAM_STEP3D_T` 之后）。
- `ROMS/Cpp/kokkos_step2d.hpp`：diag 分块声明 128–141；行块声明 183 起。
- `ROMS/Cpp/kokkos_step3d_t.hpp`：宏控制下的 team/pipe/rows 声明。
- `CMakeLists.txt`：bench 选项 451–490。

### 申威上的证据（`~/voyage/roms/work/`）

- `rhs-opt-20261113/ab-xi-v2-single/result.json`（job `9198626`、`9198728`）
- `rhs-opt-20261113/model-xi-v1/` 及 `four-step/`（job `9226298`、
  `9226324`、`9226689`、`9226701`、`9227546`、`9227580`）
- `rhs-opt-20261113/diag-v1/`（job `9229432`–`9229436`）
- `rhs-opt-20261113/strip-v1/`、`strip-v3/`（job `9228813`、`9228906`、
  `9228989`、`9229069`、`9230585`）
- `rhs-opt-20261113/vec-probe/`（job `9228421`）
- `rhs-opt-20261113/diagnose-stress-v1/`（job `9226934`）
- `team-share-20261109/results/team-2026-11-09.md`

### 已验收门禁记录的身份标识

- xi 内核源码 SHA256 `602b53f0...`（`0bb237a7` 的 `kokkos_step2d.cpp`）
- SW64 `romsM` SHA256 `9d68f9de...`
- 参考 `romsM` SHA256 `a9e7e7b2...`（原始版本，318 个纯 Fortran 单元）
- 被替换的归档成员 SHA256 `fefce6ef...`
- 四步输入 SHA256 `aa219a...`
