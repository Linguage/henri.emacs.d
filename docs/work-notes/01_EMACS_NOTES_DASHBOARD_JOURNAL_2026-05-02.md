---
title: "Emacs 私人配置：Notes 根目录、Dashboard、Journal 与导出主题对齐"
date: "2026-05-02"
created: "2026-05-02 17:38:32 +0800"
category: "worknotes"
tags: ["emacs", "elisp", "org-mode", "journal", "dashboard", "startup", "html-export", "theme", "modeline", "fonts"]
---

# Emacs 私人配置：Notes 根目录、Dashboard、Journal 与导出主题对齐

## 1. 背景与动机

这一阶段的目标是把 `henri.emacs.d` 当作“唯一真相源”的个人配置仓库来启用，并把日常写作/记录（Org Journal + HTML 导出）从零散状态收束到可持续迭代的结构中。

同时，启动体验需要从“默认落到某个目录/Dired”调整为类似 Doom 的启动页，并把常用入口（Notes、Journal、Find File）与 Org 工作流连接起来。

## 2. 本阶段目标

1. 让 `C-x C-f` 默认从 `~/Documents/EmacsNotes/`（Notes 根目录）开始查找文件。
2. 启用一个 dashboard 启动页，替代启动时直接打开目录列表。
3. Journal 体系按月归档，减少手动重命名/备份，并尽量减少丢稿风险（自动保存）。
4. Org 导出 HTML 使用默认主题（Henri Bearblog）并把资源路径对齐到可维护的位置。
5. 收敛字体/状态栏等 UI 配置，降低噪音，便于长期使用。

## 3. 主要改动（按模块归类）

### 3.1 启动与默认目录

- `init.el`：
  - 引入 `henri/default-notes-directory`，并将 `default-directory` 指向 Notes 根目录。
  - 实现 `henri/dashboard` 启动页（包含 ASCII logo、入口按钮、快捷键提示）。
  - 为避免 macOS GUI 启动时误打开配置目录 Dired，增加“启动期抑制 config 目录 Dired”的防御逻辑（仅针对 `henri.emacs.d` 目录，且是启动期窗口）。

### 3.2 文件查找入口

- `lisp/init-managing.el` 与 `init.el`：
  - `C-x C-f` 绑定到 `henri/find-file-in-notes`，并额外设置 `[remap find-file]`，减少被其他 keymap 覆盖导致的回退。

### 3.3 Journal（月度文件 + 自动保存）

- `lisp/writing/org/org-journal.el`：
  - Journal 根目录由硬编码路径改为从 `henri-notes-directory` 派生（`Journal/` 子目录）。
  - Journal 的 HTML 导出 setupfile 改为从 `henri-org-html-themes-directory` 派生；缺失时提示运行主题安装脚本（避免静默失败）。
  -（本阶段的目标是“按月文件”+“自动保存”，配置上已准备相关入口与自动保存逻辑；后续仍需在交互环境验证写作过程的实际体验。）

### 3.4 Org HTML 导出主题对齐

- `lisp/writing/org/org-html.el`：
  - `my/org-html-themes-dir` 改为使用 `henri-org-html-themes-directory`，减少对外部目录的耦合。
  - 默认主题指向 `theme-henri-bearblog.setup`（Henri Bearblog）。

### 3.5 Org 视觉与正文字体恢复

- `lisp/writing/org/org-base.el`：
  - 补回旧配置里 Org 正文字体偏好（例如 `Kaiti TC` / `苍耳今楷 02` 等可用字体时启用），并提供 `henri/apply-org-faces` 以在主题切换后重应用关键 faces。
  - 将 all-the-icons 在 org-base 内的 hard dependency 降级为可选（避免因图标包缺失导致 Org 配置整体报错）。

### 3.6 UI：字体、状态栏、初始 Frame 大小

- `early-init.el`：
  - 把初始 frame 的尺寸/位置提前到 early-init，减少启动后“先出现一个窗口再跳动”的观感。
  - macOS 下设置 `ns-pop-up-frames`，尽量避免启动期额外弹出 frame。
- `lisp/init-styling.el`：
  - macOS 默认字体优先使用 `Cascadia Code NF`（fallback 到其它已安装字体）。
  - `doom-modeline` 调整为更“安静”的配置，关闭 minor-modes / word-count / LSP 等易堆叠信息项。

## 4. 运行与验证

确认做过的验证：

- `emacs --batch` 能加载主要入口配置（用于语法与加载链路的快速验证）。

未能在本阶段完全确认的事项（需要交互验证）：

- macOS GUI 启动时，是否仍会出现“前台先弹出 config 目录 Dired 再切换/关闭”的现象。当前已加入启动期拦截与清理逻辑，但需要用真实 GUI 启动路径复验。

## 5. 当前状态与遗留问题

1. 启动体验：dashboard 已实现，但仍可能被 macOS 的目录打开事件/恢复机制影响，导致前台窗口不是 dashboard（需复验）。
2. 包安装/镜像：此前出现过镜像 tarball 缺失导致的包安装失败，需要继续观察镜像策略与 bootstrap 稳定性（是否应统一到官方 MELPA 或在国内环境使用更可靠的镜像组合）。
3. Org 美化插件：`org-bullets` / `org-superstar` / `org-download` 等外部包在缺失时的体验需要进一步梳理（当前倾向“可选依赖 + 不阻塞启动”）。

## 6. 后续建议（下一步）

1. 用真实 GUI 启动做一次“从 Finder / Spotlight / CLI 打开 Emacs”的矩阵验证，确认 dashboard 永远是前台窗口。
2. 把 “启动页/Notes/Journal/HTML 主题” 的最终约定写入 `README.md`，减少未来回归的成本。
3. 如果要进一步提升写作体验，考虑把 Org 的 prose 模式（variable-pitch/mixed-pitch、行距、段落宽度）独立成一个可开关模块，避免影响编程缓冲区。

