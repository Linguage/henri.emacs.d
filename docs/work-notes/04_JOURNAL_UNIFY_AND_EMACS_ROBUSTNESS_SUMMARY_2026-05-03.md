---
title: "Journal 三类合流入月度文件与 Emacs 配置健壮性收敛"
date: "2026-05-03"
created: "2026-05-03 01:45:00 +0800"
category: "worknotes"
tags:
  [
    "emacs",
    "elisp",
    "org-mode",
    "org-capture",
    "org-agenda",
    "journal",
    "eglot",
    "startup",
    "refactoring",
    "session-summary",
  ]
---

# Journal 三类合流入月度文件与 Emacs 配置健壮性收敛

本轮横跨若干子主题：先有「work/study capture 轻量化」的工程实现，随后在配置审查后落地了一批启动与 LSP 相关健壮性补丁，最后在写作流层面将 **日记 / 工作 / 学习** 三者重新 **合流至同一份月度 `journal-YYYY-MM.org` 与同一天的日期子树**。本文面向后续回看，交代动机、结构决策与当前状态；**不**复述聊天过程。

## 1. 背景与动机

- **Journal**：一度将 work/study 拆到子目录「按日命名的 plain 流水文件」，以降低月文件内 outline 的摩擦；实际使用后更希望在 **同一月历视图** 里对照「仪式化日记」与「工作/学习结构化条目」，并保留 tag 维度的 agenda 切片。
- **启动与 LSP**：冷启动时对空包索引无条件 `package-refresh-contents`、以及 `prog-mode` 大范围自动 `eglot-ensure`，在离线或非典型 buffer 上会放大卡顿与噪声；大文件路径里还曾用内部变量绕过 Eglot 关闭逻辑，属于可维护性与状态一致性风险。
- **工程节奏**：单次会话内出现过「计划已写好、工具中断、待办未闭环」的中间态；最终以 **批量加载自检** 与 **单模块（`org-journal.el`）集中改动** 降低回归面。

## 2. 阶段目标（本会话实际覆盖）

1. **Journal**：`C-c c d` / `w` / `s` 共享 `henri-journal-current-diary-file` + `henri-journal-goto-month-day`；`w`/`s` 使用与 `d` 相同的 `entry` 落点，仅模板与 `:journal:work:` / `:journal:study:` 标签不同。
2. **Agenda**：`org-agenda-files` 只聚合月度 `journal-*.org`，**不再**扫描 `worklog/`、`studylog/` 子目录中的历史日文件（磁盘上旧文件可保留，但不进入 agenda 集合）。
3. **查看**：`my/view-journal-by-date` 三种视角均打开同一月文件并 reveal 当日 `** YYYY-MM-DD …` 子树；未找到日期时提示「未找到 %s 的 journal day」。
4. **健壮性**：包索引刷新收紧到「安装 `use-package` 引导路径」；Eglot 改为 **白名单主模式** + server-program 与 tree-sitter 主模式对齐；大文件场景用 **`eglot-shutdown`** 替代写 `eglot--managed-mode`；`status` 中 LSP buffer 统计使用 **buffer-local 安全读取**；Julia 在缺少 grammar 时回退 `julia-mode`；`org-base` 去掉重复的 `with-eval-after-load`（低风险去重，未做整文件 face 策略大手术）。

## 3. 主要改动（按主题）

### 3.1 Journal 合流（[`lisp/writing/org/org-journal.el`](../../lisp/writing/org/org-journal.el)）

- 删除 work/study **日文件** 工具链（子目录、ensure-daily、plain + 文件尾插入等）。
- **`org-capture-templates`**：`w` 为「工作记录」模板（任务描述、要点、TODO 列表）；`s` 为「学习卡片」模板（主题、概念、解读、类别）；`d` 保持原有花销表等结构。
- **`henri-journal-agenda-files`**：仅 `journal-YYYY-MM.org` 与目录内历史月文件 glob。
- **Agenda / 日历** 文案与「工作记录」「学习卡片」语义对齐；**PDF 自动 LaTeX 类** 的路径匹配去掉对 `worklog|studylog` 的依赖，避免旧目录名继续触发期刊导出假设。

### 3.2 启动与包管理（[`init.el`](../../init.el)）

- 去掉「`package-archive-contents` 为空即刷新」的泛化路径；仅在需要 **首次安装 `use-package`** 时 `package-refresh-contents`，减少日常冷启动对网络的硬依赖。

### 3.3 Eglot 与编程栈（[`lisp/init-custom.el`](../../lisp/init-custom.el)、[`lisp/init-programming.el`](../../lisp/init-programming.el)）

- 新增 `henri-eglot-auto-major-modes`（`defcustom`），`prog-mode-hook` 仅在白名单内 `eglot-ensure`。
- `eglot-server-programs` 增补与 tree-sitter 主模式一致的条目（如 `python-ts-mode`、`c-ts-mode` 等），避免「主模式已换 ts、server 仍只注册旧 mode」的错配。

### 3.4 大文件与诊断（[`lisp/ops/lib-files.el`](../../lisp/ops/lib-files.el)、[`lisp/ops/status.el`](../../lisp/ops/status.el)）

- 大文件优化分支：在已管理时用 `eglot-managed-p` + `eglot-shutdown`；`henri/restore-from-large-file` 用 `buffer-local-boundp` 判定后再 `eglot-ensure`。
- 模块状态报告里枚举 Eglot buffer 时避免对未绑定 buffer-local 变量直接 `buffer-local-value`。

### 3.5 其它低风险整理

- **Julia**：`.jl` 在 `treesit-language-available-p` 为真时用 `julia-ts-mode`，否则 `julia-mode`。
- **Org base**：去除重复的 `with-eval-after-load 'org` 块（**未**在本轮合并多套 face 策略，该大块重构仍属未来独立 job）。

## 4. 验证与未在环境中确认的部分

- **已执行**：多次 `emacs --batch -Q --init-directory=<本仓库> --load early-init.el --load init.el`，退出码为 0，用于确认配置链无加载期错误。
- **未在此环境自动验证**：图形界面下 `C-c c d/w/s` 的实际 capture 落点、`C-c a` 自定义 agenda 在真实 `~/Documents/.../Journal` 下的交互表现、以及旧 `worklog/` / `studylog/` 日文件是否仍被用户手动引用。这些需在本机笔记目录上做一次人工烟测。

## 5. 当前状态与后续可选工作

- **当前**：三类 capture 在数据模型上已对齐为「**一月一文件、一天一子树、多条目为三级标题**」；agenda 按 tag 切 `diary` / `work` / `study` 仍成立，且文件集合更单纯。
- **文档**：[`docs/jobs/2026-05-03-journal-capture-lightweight.md`](../jobs/2026-05-03-journal-capture-lightweight.md) 描述的是「日文件分流」阶段的 job，与**现已合流**的实现可能不一致；若要以文档为单一真相源，建议后续 **更新或迁入 legacy** 并指向本 work note 或新的简短「Journal 约定」段落。
- **可选**：`org-base.el` 中多套主题/face 与 timer hook 的收敛（此前审查已标为中长期项）；若需要历史 work/study 日文件进入 agenda，应单独设计「只读导入」或显式 `org-agenda-files` 附加项，而不是恢复默认扫描。

## 6. 假设与边界

- **已确认**：仓库内 `org-journal.el` 与 `init.el` 等路径在 batch 加载下可协同工作。
- **未证实**：所有用户机器上 `henri-notes-directory` 下是否已存在符合新结构的月文件；首次 capture 会由现有 ensure 逻辑创建月文件头（与此前 diary 行为一致）。
