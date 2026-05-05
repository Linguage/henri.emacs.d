---
title: "Roam 配置审查、代码修正与 HTML 主题可移植化"
date: "2026-05-07"
created: "2026-05-07 00:00:00 +0800"
category: "worknotes"
tags:
  [
    "emacs",
    "elisp",
    "org-roam",
    "org-html",
    "journal",
    "academic",
    "citar",
    "code-review",
    "portability",
    "session-summary",
  ]
---

# Roam 配置审查、代码修正与 HTML 主题可移植化

本轮工作分为三个阶段：先对 Org-roam 写作系统做了一次结构性审查，随后根据审查结论对已落地代码做质量核查并修复剩余问题，最后解决了 HTML 主题路径硬编码导致的跨设备不可移植问题。

## 1. 背景与动机

上一阶段（07 号 work-note）完成了知识系统的整体收敛，包括 Roam / Journal / Academic / Citar 的边界对齐、跨线流转命令、生命周期标签、obsolete 兼容层清理等。代码已由另一个 AI 实施并暂存。

本系列会话的目标是：

1. 从代码和理念两个层面对写作系统做一次独立审查，找出残留问题；
2. 核查已落地代码的质量，确认审查建议是否被正确实施；
3. 解决 HTML 主题路径硬编码问题——`C-c m h t` 设置主题时会把 `.setup` 文件的绝对路径写入 `#+SETUPFILE:`，换一台设备后路径失效。

## 2. 结构性审查

审查范围覆盖 `org-roam-henri.el`、`org-journal.el`、`org-academic.el`、`org-base.el`、`org-html.el`、`init-org.el`、`init-custom.el` 以及 `docs/specs/`、`docs/tutorials/` 下的所有相关文档。

### 2.1 理念层

审查确认四线分工（Agenda / Journal / Roam / Academic）的边界已经清楚，但指出三处需要继续收敛的地方：

1. **流转只停留在文档约定**：Roam daily 的「待抽取节点」→ `notes/`、Reading card → Roam 图谱、Journal ↔ Roam daily 都没有命令支撑。建议至少做 `extract-pending`、`today-summary` 和 Journal warn-from-roam-daily 三个最小命令（后两个在已落地代码中已实现）。
2. **三个 references 存在歧义风险**：`Academic/Reading/`、`Roam/references/` 和 `henri-org-roam-enable-citar-integration` 开关打开后的行为可能分裂。建议把开关改成真正的 toggle，同时切换 `citar-notes-paths`。
3. **缺少节点生命周期**：目录是内容池但没有成熟度概念。建议引入 `seedling / budding / evergreen` 标签（已落地代码中已实现）。

### 2.2 代码层

审查发现 10 个具体问题，按优先级排列：

| # | 问题 | 严重度 |
|---|------|--------|
| 1 | Roam 模板手写 `:ID: %(org-id-new)` 与 org-roam 自动 ID 冲突 | 高 |
| 2 | `before-save-hook` 全局注册应改为 buffer-local | 高 |
| 3 | `citar-notes-paths` 在开关打开时被两个模块各设各的 | 高 |
| 4 | obsolete 兼容符号应直接删除 | 中 |
| 5 | `ensure-directories` 启动时无条件创建应改为按需 | 中 |
| 6 | Reading card 不写 `:ID:`，无法进 Roam 图谱 | 中 |
| 7 | `org-roam-extra-files` 用 `setq` 覆盖会吞掉其他来源 | 中 |
| 8 | `doom-themes-org-config` 与 `henri/apply-org-faces` 双重设 face | 低 |
| 9 | capture template `append` 不幂等 | 低 |
| 10 | `init-org.el` 缩进混用 tab/space | 低 |

## 3. 已落地代码的质量核查

对已暂存的全部 diff 逐文件核对后，确认审查中的核心建议已正确落地：

- 模板 `:ID:` 移除、`before-save-hook` 改 buffer-local、citar 真源单点化、obsolete 符号删除、Reading card 写 `:ID:` 并纳入 `org-roam-extra-files`、`ensure-directories` 改按需、doom-themes-org-config 移除、capture template 幂等注册、缩进统一——全部通过。

发现两个需要修正的问题：

1. **`org-roam-extra-files` 用 `setq` 覆盖**：如果其他模块也往 `org-roam-extra-files` 加文件，会直接吞掉。修正为 `delete-dups (append new-files existing)`。
2. **`:seedling:` 全局硬编码到 file-head**：所有 capture 模板（包括 references、projects、maps、people）都会打上 `:seedling:`，但 map 通常是"已经想清楚的主题入口"，reference 是外部资料，不应该从 seedling 开始。修正为只给 `notes/` 和 `inbox/` 模板加 `:seedling:`，新增 `henri-org-roam--file-head-seedling` 常量。

## 4. HTML 主题路径可移植化

这是本系列会话中最大的改动。

### 4.1 问题分析

原先的主题系统有两层路径问题：

- **Journal 文件**：`henri-journal-ensure-monthly-file` 在创建月度文件时写入 `#+SETUPFILE: /Users/.../theme-henri-journal.setup`，绝对路径硬编码到源文件。
- **用户手动设置主题**：`henri/org-html-set-theme`（`C-c m h t`）把 `.setup` 文件的绝对路径写入 `#+SETUPFILE:`。

两者在换设备后都会失效。`.setup` 文件内的 `@@henri-org-html-themes-root@@` 占位符已经在导出时被动态替换，但 `#+SETUPFILE:` 本身写入源文件的路径仍然是设备相关的。

### 4.2 解决方案：`#+HENRI_HTML_THEME:` 关键字

核心思路：源文件只存主题名称，不存路径。导出时由 hook 动态解析。

**新增机制**：

- `#+HENRI_HTML_THEME: Henri Journal` — 源文件只写入这个可移植关键字。
- `henri/org-html--read-theme-keyword` — 读取关键字值。
- `henri/org-html-ensure-default-theme-for-export` — 导出 hook 分三阶段工作：
  1. 如果有 `#+HENRI_HTML_THEME:`，在导出 buffer 中替换为当前设备上 `.setup` 文件的实际路径；
  2. 修复已有的但路径失效的旧 `#+SETUPFILE:` 行（向后兼容）；
  3. 如果没有任何主题声明，注入默认主题（Journal 文件自动用 `Henri Journal`，其他用 `Henri Notes`）。

**改造的命令**：

| 命令 | 改动 |
|------|------|
| `henri/org-html-set-theme` | 写入 `#+HENRI_HTML_THEME:` 而非 `#+SETUPFILE:`；同时清理旧 SETUPFILE 行 |
| `henri/org-html-remove-theme` | 同时清理 `#+HENRI_HTML_THEME:` 和旧 `#+SETUPFILE:` |
| `henri/org-html-export-with-theme` | 临时 buffer 中也清理关键字，直接注入 `.setup` 路径 |
| `henri/org-html-theme-present-p` | 先查关键字，再查旧 SETUPFILE |

**Journal 文件处理**：

`henri-journal-html-setupfile` 函数被完全移除。新建的 Journal 月度文件不再包含 `#+SETUPFILE:` 行。导出时由 `henri/org-html--journal-file-p` 检测文件名（`journal-YYYY-MM.org`），自动注入 `Henri Journal` 主题。

**向后兼容**：

已有文件中写死的 `#+SETUPFILE: /旧设备路径/...` 仍然有效——导出 hook 会检测路径是否存在，不存在则自动修复为本地等效路径。

### 4.3 改动范围

| 文件 | 改动 |
|------|------|
| `org-html.el` | 新增 `henri/org-html--read-theme-keyword`、`henri/org-html--journal-file-p`；改造 `set-theme`、`remove-theme`、`export-with-theme`、`theme-present-p`、`ensure-default-theme-for-export` |
| `org-journal.el` | 移除 `henri-journal-html-setupfile`；`ensure-monthly-file` 不再写入 `#+SETUPFILE:` |
| `writing-system-guide.md` | 更新 HTML 主题机制说明 |
| `org-roam-guide.md` | 更新 HTML 主题机制说明 |
| `html-theme-guide.md` | 更新主题设置机制说明 |

## 5. 验证

通过 Python 脚本验证了所有关键改动：

- `org-roam-extra-files` 使用 `append + delete-dups` 而非 `setq` 覆盖；
- `seedling` 只加到 `notes/` 和 `inbox/` 模板，其他类型不加；
- `henri/org-html-set-theme` 不再向源文件写入 `#+SETUPFILE:`；
- 导出 hook 能读取 `#+HENRI_HTML_THEME:` 并动态解析路径；
- `remove-theme` 同时处理关键字和旧 SETUPFILE；
- `henri-journal-html-setupfile` 已从 `org-journal.el` 中完全移除。

由于环境限制无法启动 Emacs 进行交互测试，但所有逻辑验证均通过。

## 6. 文档同步

以下文档已更新以反映本轮改动：

- `writing-system-guide.md`：HTML 主题部分重写，强调 `#+HENRI_HTML_THEME:` 可移植关键字和导出时动态注入。
- `org-roam-guide.md`：两处提及 HTML 主题的地方更新，说明 Journal 文件自动使用 Henri Journal 主题。
- `html-theme-guide.md`：主题设置机制说明更新。

## 7. 剩余事项

1. `henri/today-summary` 的窗口布局尚未在真实图形界面中做长期交互打磨。
2. 生命周期标签是人工切换，没有引入自动统计反链或复用次数的机制。
3. Reading card 作为 `org-roam-extra-files` 进入图谱已打通，但"Reading card 自动插入到某个 Roam map / project"的更高层流转仍无自动化。
4. HTML 主题可移植化的改动需要在真实 Emacs 环境中做交互验证，确认 `C-c m h t`、`C-c m h w`、`C-c m h e`、Journal HTML 导出等命令在更换 `user-emacs-directory` 后仍能正常工作。
