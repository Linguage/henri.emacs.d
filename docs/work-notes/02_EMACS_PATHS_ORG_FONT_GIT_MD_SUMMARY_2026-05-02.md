---
title: "Emacs 配置后续迭代：运行时路径、启动页与工具栏、Org 中英文字体、本地 Git 与 Markdown"
date: "2026-05-02"
created: "2026-05-02 19:00:00 +0800"
category: "worknotes"
tags: ["emacs", "elisp", "org-mode", "fonts", "magit", "diff-hl", "markdown", "paths", "dashboard", "documentation"]
---

# Emacs 配置后续迭代：运行时路径、启动页与工具栏、Org 中英文字体、本地 Git 与 Markdown

本笔记回顾在同一阶段内连贯推进的多项工作：在早前「Dashboard / Notes / Journal」方向（见 `01_*`）之后，补齐审查整改收口、日常交互修复、正文字体策略，以及本地 Git 与 Markdown 预览工具链，使配置在「可维护」与「每天用得上」之间更一致。

## 1. 背景与动机

- 配置仓库需明确 **本机运行时目录** 与 **版本控制边界**，避免把 `elpa`、`rime`、`var` 等误提交，同时不把个人绝对路径写死到各模块。
- 启动页与 **GUI 工具栏** 在 macOS 上出现与预期不符的行为（例如未出现 Dashboard、或工具栏仍显示），需要区分「逻辑条件」与「early-init 时机」两类原因。
- Org 日记/正文中 **中文衬线 vs 标题无衬线** 的观感要与主题、variable-pitch 的继承关系一致，避免整缓冲 `buffer-face-mode` 与 Doom 体系互相打架。
- 写作与协作上，**Git** 与 **Markdown 预览** 长期只靠「装个 Magit / markdown-mode」偏薄；希望在 **不接 Forge** 的前提下补足本地日常能力与可预期的依赖提示。

## 2. 阶段目标（本系列实际覆盖范围）

1. 审查整改 job 收口：运行时路径模块、文档与 legacy 归档（计划内事项）。
2. 启动体验：Dashboard 不被「笔记目录是否存在」静默关闭；工具栏在图形界面可靠关闭。
3. Org：**正文衬线**（思源宋体 / Noto Serif CJK / 宋体等候选）与 **标题无衬线**（如苹方）可同时成立。
4. Git：Magit 常用入口、`diff-hl` 改动提示、合并冲突时 `smerge-mode` 与快捷键。
5. Markdown：离线预览（pandoc，优先 EWW）与 GitHub 风格（grip）双路径，以及依赖自检与文档同步。

## 3. 主要改动（按主题）

### 3.1 运行时路径与仓库策略

- 新增或集中使用 [`lisp/ops/paths.el`](../../lisp/ops/paths.el) 约定 `henri-var-directory`、缓存、Rime、tree-sitter、`transient` 等目录；启动时创建必要子目录。
- 备份与 Rime 配置消费上述变量，避免在各处重复拼路径。
- `.gitignore`、README、`c.md`、架构说明中明确 **runtime / custom / rime** 与「未采用 no-littering」的决策。

### 3.2 启动页、default-directory、工具栏

- `initial-buffer-choice` 绑定 **始终** 使用自建 Dashboard，避免笔记根不存在时默默退回 Dired 被误读为「取消启动页」。
- `early-init.el`：通过 **`tool-bar-lines`** 与 **`window-setup-hook`** 再次关闭工具栏/菜单栏等，规避 `display-graphic-p` 在极早阶段为假导致跳过分支的问题。
- 与早前相比，**已移除** 大量针对启动期 Dired 的 advice 与定时清理，改由更小范围的行为约定（计划内简化）。

### 3.3 Org 中英文字体（衬线正文 + 无衬线标题）

- 在 [`lisp/writing/org/org-base.el`](../../lisp/writing/org/org-base.el) 使用 **`face-remap-add-relative`**：对 `default` 与 `variable-pitch` 施加衬线族；对 `org-document-title`、`org-level-1..8` 施加无衬线族；并用 cookie 列表避免重复 remap。
- [`lisp/init-custom.el`](../../lisp/init-custom.el) 增加 `henri-org-cjk-serif-family`、`henri-org-cjk-sans-family` 可选强制字族。

### 3.4 本地 Git 与 Markdown

- [`lisp/init-managing.el`](../../lisp/init-managing.el)：扩展 Magit 绑定（如 `C-c g` 前缀）、`diff-hl` 全局与 Dired、Magit 刷新后同步、冲突文件自动 `smerge-mode` 与 `C-c ^` 辅助键。
- [`lisp/init-writing.el`](../../lisp/init-writing.el)：`henri/markdown-preview-offline`、`henri/markdown-preview-github-style`、`henri/markdown-check-preview-deps`，以及 `C-c m p/g/c`；保留原有 `C-c C-v` 等入口。
- [`README.md`](../../README.md)、[`c.md`](../../c.md)、[`docs/specs/ARCHITECTURE.md`](../specs/ARCHITECTURE.md) 同步快捷键与外部依赖（pandoc、grip）。

## 4. 验证与诚实边界

**已执行（自动化 / 静态）**

- 多次 `emacs --batch -Q` 加载 `early-init.el` 与 `init.el`，确认无加载期 Elisp 错误。
- 使用 `rg` 对关键符号与文档交叉检索；对修改过的 Elisp 使用编辑器侧 linter / 诊断，未见新增问题报告。

**未在本系列中系统验证（留待本机 GUI）**

- 不同 macOS / Emacs 构建下 Dashboard 首帧、Magit 全窗口流、diff-hl 在特定主题下的 fringe/margin 观感。
- `grip` / `pandoc` 在 **GUI Emacs 的 PATH** 是否与终端一致（虽配置侧已导向 exec-path-from-shell，仍依赖用户环境）。
- 长文件或极多冲突块下 `find-file-hook` 自动启用 `smerge-mode` 的性能感受。

## 5. 文档与索引

- 审查整改 job 文档归档至 `docs/legacy/`（具体以仓库内文件名为准）；README 导航已指向 legacy 条目。
- 本目录 [`INDEX.md`](INDEX.md) 已（或应）登记本摘要，便于按日期检索。

## 6. 当前状态与后续可选项

**已收口**

- 审查计划中的路径模块、启动简化、文档与验证条目（对应当时计划）；Git/Markdown 增强按计划落地且不引入 Forge。

**仍可选的后续（非本系列承诺）**

- Forge / `gh` 与线上 PR、Issue 一体化。
- Markdown 数学公式、Mermaid、Obsidian 风格链接等重型笔记能力。
- 进一步压缩 `org-mode-hook` 中多处延迟定时器，统一字体与主题刷新顺序（属于整洁度优化，而非功能缺口）。

---

*本记录依据多轮会话中的实际改动与自述验证整理，不宣称已做全量交互回归；若与仓库后续提交不一致，以 Git 历史为准。*
