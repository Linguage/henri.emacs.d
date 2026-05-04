---
title: "Emacs 视觉系统模块化：lisp/visual 分层与兼容入口迁移"
date: "2026-05-02"
created: "2026-05-02 21:45:00 +0800"
category: "worknotes"
tags:
  ["emacs", "elisp", "visual", "doom-themes", "fonts", "org-mode", "refactoring", "documentation"]
---

# Emacs 视觉系统模块化：lisp/visual 分层与兼容入口迁移

本轮工作按既定计划「视觉相关配置分层收敛」，把原先分散在 `init-styling.el`、`lib-fonts.el` 与 `org-base.el` 前段的字体/主题/UI 逻辑，收拢为 `lisp/visual/` 下的 **字体、主题、组件** 三条线，并保留两层薄兼容入口以降低启动 `require` 链风险。本文记录动机、结构决策、已落地改动、验证与尚未做的可选清理。

## 1. 背景与动机

- **耦合过重**：`init-styling.el` 同时承载基础 UI、`doom-themes`、modeline、图标、字体命令与 centaur-tabs，主题与字体的先后关系也不易一眼看清。
- **职责交叉**：通用字体与 Org 专用 CJK 候选、fontset、诊断混在 `org-base.el` 开头，与「Org 行为/配色/插件」纠缠，后续维护与文档容易误指文件。
- **演进策略**：不一次性移动 `init-custom.el` 里的主题策略（`henri/apply-current-theme` 等），避免与 `defcustom`、定时刷新主题三处同时重排；短期用 **hook** 把「换主题后重跑字体」钉在稳定位置。

## 2. 阶段目标（本系列实际范围）

1. 建立 `lisp/visual/` 与总入口 `init-visual.el`（仅编排 `require`）。
2. 字体层：合并原 `lib-fonts` 与 Org CJK 相关函数；提供 `henri/apply-fonts`；Org 正文字体 hook 在字体层注册。
3. 主题层：迁入 `doom-themes`；在首次应用主题前注册 `henri-theme-changed-hook` → `henri/apply-fonts`。
4. 组件层：基础 UI、nerd-icons、doom-modeline、all-the-icons、centaur-tabs；GUI 初次 `henri/apply-fonts` 与字号快捷键。
5. `rainbow-delimiters` 迁到 `init-programming.el`（`prog-mode`），与「代码阅读」语义一致。
6. `init-styling.el`、`lib-fonts.el` 保留为兼容 shim；`init.el` 仍 `require` 前者，并仅为 `lisp/visual` 补 `load-path`。
7. 文档：`README`、`docs/specs/ARCHITECTURE.md`、`docs/jobs/emacs-align-doom-patterns.md` 反映新分层。

## 3. 主要改动（按工程主题）

### 3.1 新目录与加载顺序

- [`lisp/visual/init-visual.el`](../../lisp/visual/init-visual.el)：`visual-fonts` → `visual-themes` → `visual-components`。保证主题包加载后再挂依赖主题的 UI（例如 centaur-tabs 与 doom 主题 hook）。
- [`init.el`](../../init.el)：`load-path` 增加 `lisp/visual`；维持 `(require 'init-styling)`。

### 3.2 字体层 `visual-fonts.el`

- 承接原 **`henri/set-font`**、缩放、`henri-big-font-mode` 及 Org CJK：**候选常量**、`henri--org-first-available-font`、`henri/org-apply-cjk-fontset`、`henri/org-font-diagnose`、`henri/org-setup-body-font` 与 remap cookie 逻辑。
- 新增 **`henri/apply-fonts`**：`henri/set-font` + 若已定义则 `henri/apply-org-faces`（写作模块晚加载时由 `fboundp` 自然跳过）。
- Org 正文相关 **`org-mode-hook`** 在本层通过 **`with-eval-after-load 'org`** 注册，避免与 `org-base` 的职责混淆。
- **Batch 兼容性**：在非图形/精简环境下 `set-fontset-font` 可能未定义，`henri/set-font` 中对 `set-fontset-font` 的调用加装 **`fboundp`**，避免 `--batch` 加载时 `use-package`/主题回调里触发 void function（该点为后续验证中确认的必要补丁）。

### 3.3 主题层 `visual-themes.el`

- `doom-themes` **`use-package`** 自 `init-styling` 迁入；config 内在可用时调用 `doom-themes-org-config` 与 **`henri/apply-current-theme`**（后者仍在 `init-custom.el`）。
- 定义 **`henri-theme-changed-hook`**（若此前未统一声明则在本文件兜底），并在 **`use-package :config`** 运行前 **`add-hook` → `henri/apply-fonts`**，以满足「策略函数里 `(run-hooks 'henri-theme-changed-hook)` 之后字体能跟上」的约定。

### 3.4 组件层 `visual-components.el`

- 迁入原 `init-styling` 中的 **default-frame-alist**、行号/列号、`hl-line`、启动屏与 bell、auto-revert、electric-pair、subword。
- **`doom-modeline`**、`nerd-icons`、`all-the-icons`、可选 **`centaur-tabs`** 整块（含鼠标、图标、buffer 分组、与 `henri-buffer-real-p` 的隐藏规则等）。

### 3.5 兼容与写作侧

- [`lisp/ops/lib-fonts.el`](../../lisp/ops/lib-fonts.el)：`(require 'visual-fonts)` + `(provide 'lib-fonts)`，doctor 等仍可按旧名 `require`。
- [`lisp/init-styling.el`](../../lisp/init-styling.el)：`(require 'init-visual)` + `(provide 'init-styling)`。
- [`lisp/writing/org/org-base.el`](../../lisp/writing/org/org-base.el)：删除已迁走的字体候选与正文字体实现；**(require 'visual-fonts)**；保留 **`henri/apply-org-faces`**、`custom-theme-set-faces`、表格/链接等微调 hook。
- [`lisp/init-programming.el`](../../lisp/init-programming.el)：补上 **`rainbow-delimiters`**（与原 `prog-mode` hook 行为一致）。
- [`lisp/init-custom.el`](../../lisp/init-custom.el)：与 Org 字体相关的 **defcustom** 文档字符串中，候选列表出处由 org-base 改为指向 **`visual-fonts.el`**（避免误导）。

## 4. 文档与运维说明

- **README**：「界面与视觉层」小节改为描述 `lisp/visual/`、`init-styling`/ `lib-fonts` 兼容层与 rainbow 迁居。
- **新建或补全**：`docs/specs/ARCHITECTURE.md`（分层表与加载顺序约束）、`docs/jobs/emacs-align-doom-patterns.md`（记录「视觉拆分」进度与可选后续：直连 `init-visual`、是否去掉 shim）。

## 5. 验证（已知事实）

- **已运行**：在无图形参数的批次加载下，`user-emacs-directory` 指向本仓库、`emacs --batch … -l init.el`：**通过**，未再出现前述 `set-fontset-font` 未定义一类错误。
- **未在本笔记来源中逐项确认**：GUI 下 `henri/org-font-diagnose`、`henri/select-theme` 后 Org 观感、快捷键与 centaur-tabs 图标等——计划中列为 **交互自检项**，若在读者环境中尚未复测，应视为 **待手动确认**，不可等同「已实测通过」。
- **`lisp/**/*.elc`**：当时检查为未见 stale `.elc`（若本地曾单独 byte-compile，应自行再扫一遍）。

## 6. 当前状态与后续可选项

- **已达成**：视觉配置在代码层面三分层 + 双兼容入口 + 文档补位；Org 面色仍留在 org-base，`henri/apply-org-faces` 由字体统一入口在适当时机重跑。
- **刻意未做**：`init.el` 尚未改为直接 `(require 'init-visual)`；`init-styling`/`lib-fonts` shim 仍存在——与计划一致，留给稳定期后再做小步删除。
- **与 `02_*` 笔记的关系**：上一份笔记里 Org 正文/标题字体策略的实现细节若在读者记忆中仍写在 org-base，**现已迁至 `visual-fonts.el`**；若交叉引用旧段落，请以当前仓库路径为准。

## 7. 结论边界（事实 vs 假设）

- **可视为已在本轮落实**：目录与文件拆分、兼容层形态、rainbow 迁移位点、主题后字体重跑 hook、`set-fontset-font` 的运行时守卫、README 与架构/任务活页文档更新、批次启动无报错（在所述命令条件下）。
- **不宜写死为结论**：每位用户 GUI 字体安装、Org 缓冲实际 `actual=` 输出、centaur-tabs 开关与图标是否全部正常——依赖本机环境与人工点验。

## 8. 补充：与 Doom 对齐的高 ROI 工作（同日继续）

视觉分层完成后，紧接着横向对照 `~/.config/doom`（私人配置）+ `~/.config/emacs.doom-backup-20260502-154555`（Doom 内核）做了一次模式抽取，把 7 项「不动整体架构、ROI 高」的做法落到本仓库。本节按"做了什么 → 怎么验 → 已识别的 quirks"记录，不重复 1-7 节内容。

### 8.1 新增模块（按依赖顺序）

| 模块 | 角色 | 关键出口 |
|---|---|---|
| [`lisp/ops/lib-system.el`](../../lisp/ops/lib-system.el) | OS / 可执行检测 | `henri/get-os-type`、`henri/executable-p` |
| [`lisp/ops/lib-hooks.el`](../../lisp/ops/lib-hooks.el) | 三段式启动 hook | `henri-first-input-hook` / `-buffer-hook` / `-file-hook`、`henri-add-transient-hook!` |
| [`lisp/ops/lib-files.el`](../../lisp/ops/lib-files.el) | 大文件防御 + 真假 buffer | `henri/large-file-optimizations`、`henri-buffer-real-p`、`abort-if-file-too-large` advice |
| [`lisp/ops/doctor.el`](../../lisp/ops/doctor.el) | 依赖/字体/目录/特性自检 | `henri/doctor`（绑 `C-c h d`） |
| [`lisp/ops/profiles.el`](../../lisp/ops/profiles.el) | 机器特定覆盖 | `henri/load-profile`，按 `HENRI_PROFILE` 或 `system-name` 加载 `profile-<name>.el` |
| [`lisp/init-dashboard.el`](../../lisp/init-dashboard.el) | 启动页 | 从 `init.el` 中抽出，加多套随机 ASCII logo |

`lib-fonts.el` 在 03 主体里已经存在（属于视觉迁移过渡），本轮在其内追加：

- `henri-big-font-mode` 全局 minor mode（演示/投屏用）
- `henri/font-size-adjust` / `henri/font-size-reset`（运行时缩放）
- `henri--current-font-size` 内部状态 + `henri--sync-current-font-from-face` 同步函数

### 8.2 入口编排（`init.el`）

`require` 顺序固化为：

```
fix-warnings → init-custom → paths → profiles (+ load-profile) →
lib-system → lib-hooks → lib-fonts → lib-files → doctor →
init-dashboard → init-managing → init-styling → init-programming →
init-writing → status → backup
```

`profiles` 必须在 `init-custom` 之后、其余功能模块之前 load，确保覆盖文件能影响后续 `defcustom` 的实际值。

### 8.3 路径与 shell 收敛

- `henri-shell` defcustom 替换原来散落的 `/bin/zsh`：`init-managing.el` 的 `shell-file-name` / `explicit-shell-file-name` 与 `init-programming.el` 的 `quickrun-shell` 均已切换。
- `init-managing.el` 的 `exec-path-from-shell` 块去掉 `:config` 中的 `(let ((inhibit-message t)) ...)` 自动初始化，改为依赖 `henri-first-input-hook` 触发的 `henri/initialize-shell-env`。

### 8.4 真假 buffer 抽象

- `henri-buffer-blacklist-prefixes` defcustom 收编原 `centaur-tabs-hide-tab` 内 30+ 项的 `string-prefix-p` 列表。
- `henri-buffer-real-p` 在 `lib-files.el` 暴露；`init-styling.el` 中 `centaur-tabs-hide-tab` 改为 `(not (henri-buffer-real-p x))`。
- 副作用：未来 `consult-buffer-filter` / `ibuffer-filter-groups` 可复用同一谓词。

### 8.5 大文件双层防御

- 第一层：`advice-add 'abort-if-file-too-large :before #'henri--prepare-for-large-files-a`，在文件**读入前**比较大小与 `henri-large-file-hard-threshold`（默认 10 MB），打 message 并打 buffer-local 标志。
- 第二层：`(use-package so-long :hook (henri-first-file-hook . global-so-long-mode))`，等首次 `find-file` 后启用 `global-so-long-mode`。
- **注意**：本层另定义了 `henri/so-long-after-large-file-detected`（挂 `find-file-hook`，按 buffer 个例启用 `so-long-minor-mode`），与 `global-so-long-mode` 在覆盖面上有重叠——已记入"待二选一"。

### 8.6 which-key leader 分组

`init-managing.el` 在 `which-key` 的 `:config` 加了 12 条 `which-key-add-key-based-replacements`：

```
C-c f → henri/find       C-c F → henri/font
C-c g → henri/git        C-c h → henri/html-themes
C-c j → henri/journal    C-c m → henri/markdown
C-c o → henri/org        C-c v → henri/vc-diff
C-c w → henri/window     C-c ^ → henri/smerge
C-c e → henri/eglot      C-c n → henri/neotree
```

`C-c` 弹出菜单从平铺单字母变为带语义分组。

### 8.7 治理：忽略与可移植

- `.gitignore` 追加 `*.elc` / `**/*.elc` 与 `profile-*.el`：编译产物与机器覆盖均不入库。
- `henri-active-profile` defcustom 默认值 `(or (getenv "HENRI_PROFILE") (system-name))`，新机直接按主机名挑覆盖文件，0 配置启动即可。

### 8.8 验证

- `emacs -Q --batch -l early-init.el -l init.el` 加载成功，仅打印主题应用、Org LaTeX 加载与 custom 加载消息，无 backtrace。
- 未在本笔记来源中逐项确认：`C-=` / `C--` 缩放、`C-c F b` big-font、`C-c h d` doctor、新机器 profile 覆盖——属于交互项，应人工点验。

### 8.9 已识别但本轮未修的 quirks

| 严重度 | 现象 | 位置 |
|---|---|---|
| 🔴 高 | `lib-files.el` 中 `henri/so-long-after-large-file-detected`（per-buffer）与 `global-so-long-mode` 重叠 | [`lib-files.el#L24-L32` & `#L93-L95`](../../lisp/ops/lib-files.el) |
| 🔴 高 | `henri/initialize-shell-env` 同时挂 `prog-mode-hook` 与 `henri-first-input-hook`，前者基本被后者抢先触发 | [`init-managing.el#L141-L146`](../../lisp/init-managing.el) |
| 🟡 中 | `henri--run-first-buffer-hooks` 用 `window-buffer-change-functions`，dashboard 一显示就触发，对 `henri-first-buffer-hook` 语义偏早 | [`lib-hooks.el#L35-L37`](../../lisp/ops/lib-hooks.el) |
| 🟡 中 | `C-=` / `C--` 绑定 `(lambda () (interactive) (henri/font-size-adjust 1))` 把 `(interactive "p")` 前缀计数吞掉，无法 `C-u 5 C-=` 一次跳 5 档 | [`init-styling.el#L130-L131`](../../lisp/init-styling.el) |
| 🟡 中 | `henri/doctor` 对延迟加载包（magit / org / which-key 等）一律 `featurep` 报 MISSING，缺"已可用未触发"三态 | [`doctor.el#L20-L41`](../../lisp/ops/doctor.el) |
| 🟢 低 | `lib-fonts.el` / `lib-files.el` / `init-dashboard.el` 引用 `init-custom` 的 defcustom 但未 `(eval-when-compile (require 'init-custom))`，依赖 `init.el` 加载顺序 | 多处 |
| 🟢 低 | `init-managing.el` commentary 第 29-32 行注释重复两遍 | [`init-managing.el#L29-L32`](../../lisp/init-managing.el) |

### 8.10 与 03 主体的关系

- 03 主体只覆盖 `lisp/visual/` 三分层 + 兼容 shim，**不涉及** `lisp/ops/lib-*` 与 `init-dashboard` 抽离；本节追加这部分，保持工作笔记的时间连续性。
- 视觉层的 `henri/apply-fonts` 与本节 `henri/font-size-adjust` / `henri-big-font-mode` 互不冲突：前者负责"按 OS 选字体族"，后者负责"运行时高度缩放"，状态变量 `henri--current-font-size` 在两路径中均通过 `henri--sync-current-font-from-face` 同步。

### 8.11 结论边界（本节）

- **可视为已落实**：7 个新模块加入仓库、入口 require 顺序固化、字体缩放与 big-font 命令、which-key leader 分组、`/bin/zsh` → `henri-shell`、大文件 advice + so-long 双层、真假 buffer 抽象、profile 覆盖机制、批次启动无报错。
- **不宜写死**：上述 quirks 表中除非用户已在本机点验否则均按"待修"；`henri/doctor` 的输出在不同机器上会因可执行/字体安装情况差异较大，应配合机器特定 README 解释而非直接当成"系统坏了"。
