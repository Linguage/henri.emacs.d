---
title: "Writing 系统治理、快捷键重构与 Markdown 预览链接修复"
date: "2026-05-05"
created: "2026-05-05 00:00:00 +0800"
category: "worknotes"
tags:
  [
    "emacs",
    "elisp",
    "org-mode",
    "org-roam",
    "citar",
    "markdown",
    "keybindings",
    "governance",
    "documentation",
    "session-summary",
  ]
---

# Writing 系统治理、快捷键重构与 Markdown 预览链接修复

本轮工作围绕 `henri.emacs.d` 的写作系统继续收敛：先把 Agenda、Journal、Academic、Org-roam 的边界从“能用”推进到“可解释、可维护”，随后修复治理文档和快捷键系统里的几处错位，最后处理 Markdown 预览在 EWW 中无法打开相对文件链接的问题。阶段目标不是再增加一批功能入口，而是让已有入口有清晰归属，并让文档、代码、实际交互行为互相对得上。

## 1. 背景与动机

此前写作系统已经有 Journal、Agenda、Academic、Markdown、LaTeX/PDF 等模块，但用户实际使用时开始暴露出三个层面的摩擦：

1. **信息边界不清**：Journal 附带 agenda、Academic 和 Org-roam 同时触碰文献笔记、project 概念在 agenda 与 roam 中重名，导致“该把内容放哪里”不够直观。
2. **快捷键语义漂移**：`C-c a` 同时承担 agenda 与 academic，`C-c h` 同时是 doctor 和 Org HTML，`C-c l` 抢占 Org link 语义，F-key 调试也在全局和 mode-local 之间重叠。
3. **治理文档不可完全复盘**：根目录治理文件曾被 `.gitignore` 隐藏，文档中承诺的 batch side-effect 边界也需要和代码实际行为对齐。

因此这一阶段的核心判断是：Writing 系统需要先有稳定的“信息架构”和“操作架构”，否则继续堆模板或命令只会放大混乱。

## 2. Writing 数据边界与 Org-roam / Citar 决策

Agenda / GTD 被收敛为任务系统：`agenda/inbox.org`、`tasks.org`、`projects.org`、`someday.org` 成为主 agenda 文件集合，Journal 不再默认混进主 agenda。Journal 保持月度流水，用于经历记录、工作记录和学习卡片。

Academic 模块被重分为 `idea`、`reading`、`project`、`paper` 四类模板，避免所有学术内容都落到过重的 research note 结构中。正式文献阅读卡和 Citar notes 的唯一真源被确定为 `Academic/Reading/`；`Roam/references/` 保留给书、网页、视频等通用资料节点。为避免 `citar-create-note` 和 `citar-open-notes` 分裂，`henri-org-roam-enable-citar-integration` 默认改为 `nil`，`citar-org-roam` 只作为高级可选增强保留。

Org-roam 作为通用知识库独立落在 `Roam/`，下设 `inbox/`、`daily/`、`notes/`、`references/`、`projects/`、`people/`、`maps/`。它不替代 Journal、Agenda 或 Academic；其中 roam daily 更像知识入口和待抽取节点，Journal daily 则承担经历流水。

## 3. 治理文档与 batch 副作用边界

治理文档系统也做了一轮收口：

- `AGENTS.md` 被标准化为根目录约定文件名，并与 `c.md` 一起从 `.gitignore` 隐藏状态中释放出来，便于版本控制和审查。
- `docs/specs/`、`docs/tutorials/`、`docs/jobs/`、`docs/legacy/`、`docs/reports/`、`docs/work-notes/` 的职责被重新写清楚。
- 文档中的启动副作用承诺收敛为更具体的规则：`--batch -l init.el` 不应创建个人写作数据目录或 agenda 文件；运行时目录仍可由 runtime 层按需创建。
- `org-roam-henri.el`、`org-journal.el`、`org-academic.el` 中涉及个人笔记目录创建的路径改为交互式启动或显式 ensure 命令触发，避免 batch 加载污染用户笔记根。

这一部分的价值在于让“AI 可以怎么动这个仓库”变得可审计：治理文件不再是本地隐藏状态，文档承诺也不再和代码加载行为打架。

## 4. 快捷键系统重构

快捷键治理是本轮最集中的结构性改动。新增 `lisp/init-keys.el`，把 which-key 前缀标题和少量跨模块入口集中声明；具体 mode-local 绑定仍留在各自模块，避免为了“集中”而牺牲所有权清晰度。

最终形成的核心决策包括：

- `C-c a` 只给 `org-agenda`，Academic 统一迁到大写 `C-c A ...`。
- `C-c n` 独占 Org-roam，不再混入 NeoTree 或其它 notes 入口。
- `C-c h` 归 help / doctor，Org HTML 迁到 `C-c m h ...`。
- `C-c l` 释放给 `org-store-link`，Org LaTeX / PDF 迁到 `C-c m l ...`。
- Org 视图迁到 `C-c m v ...`，`C-c C-b` 不再覆盖 Org 默认行为。
- diff-hl 和 smerge 归入 `C-c g ...`。
- quickrun / realgud / Octave 调试归入 `C-c d ...`，F5/F6/F9/F10/F11/F12 不再作为主力入口。
- `C-c w e` 改为 eshell，`C-c w v` 保持 vterm。

文档同步更新了 `README.md`、`c.md`、`docs/tutorials/keybindings.md`、`docs/tutorials/writing-system-guide.md`、`docs/specs/ARCHITECTURE.md`，并新增 `docs/jobs/keybindings-refactor.md` 记录迁移依据与完成状态。

## 5. Markdown 预览相对链接修复

本阶段最后发现一个真实使用问题：在 `C-c m p` 的 EWW 预览中，Markdown 文档里的相对文件链接无法打开。其它 Markdown 阅读器可以正常打开，说明 Markdown 源文件本身没有问题。

排查后确认根因是：当前预览流程用 Pandoc 把 Markdown 渲染为临时 HTML，HTML 位于 `/var/folders/.../henri-md-preview-.../`。EWW 点击 `../../lisp/init-writing.el` 这类链接时，会按临时 HTML 所在目录解析，于是跳到不存在的 `/var/folders/.../docs/...` 路径。

修复策略分两层：

1. 渲染时为 Pandoc HTML 注入 `<base href="file:///源文件目录/">`。
2. 渲染后把 HTML 中相对 `href` / `src` 改写为基于原 Markdown 文件目录的 `file://` 绝对链接。

这样即使 EWW 对 `<base>` 支持存在差异，最终链接仍能落回仓库真实路径。该修复目前集中在 `lisp/writing/markdown/markdown-base.el`，属于本篇生成时仍在工作区中的最新改动。

## 6. 文档系统治理

快捷键和模块边界稳定后，文档系统本身也暴露出积累问题：README 从 455 行膨胀成"项目说明 + 快捷键手册 + 架构文档"三合一，`docs/specs/` 三件仍停留在 Phase 1 的状态（只覆盖视觉系统），`docs/jobs/` 堆了 3 份已完成但未归档的任务文档，`Agents.md` 缺少对文档维护规则的明确约束。具体改动：

### 6.1 README 瘦身

README 从 455 行压缩到约 124 行。移除了全部快捷键详表（原 § 5 全部快捷键、§ 3.x 配置开关表、§ 3.x 新增命令表），改为链接指向 `docs/tutorials/keybindings.md` 和 `docs/tutorials/writing-system-guide.md`。保留的内容：文档导航表、路径/开关一览、核心前缀速记表、项目结构树、写作模块划分表、系统要求、安装步骤。原则：README 是"了解这个项目"的入口，不是"查一个键位"的地方。

### 6.2 specs 三件更新

- **ARCHITECTURE.md**：从只覆盖视觉系统的短文档（约 40 行）重写为覆盖全部 8 个层级、写作系统 6 个 Org 子模块 + 6 个 Markdown 子模块、运维层 9 个模块的完整架构文档（约 100 行），含模块边界说明和 5 条加载顺序要点。所有文件链接验证通过。
- **ROADMAP.md**：新增 Phase 2（写作环境深化，全部标记完成）、Phase 3（文档系统对齐，当前阶段）、Phase 4（可选增强）。
- **BLUEPRINT.md**：更新日期，补充 `docs/tutorials/` 到文档边界表和主文档分工，修正"文档一致"策略描述。

### 6.3 Jobs 归档

3 份已完成任务从 `docs/jobs/` 移入 `docs/legacy/`：
- `2026-05-03-journal-capture-lightweight.md`（Journal 合流 + GTD 轻量化）
- `2026-05-04-markdown-module-enhancement-plan.md`（Markdown 子模块分层）
- `org-roam-settings.md`（Org-roam 通用知识库配置 + Citar 决策）

`docs/jobs/` 现在只剩 `README.md`。`legacy/README.md` 更新了归档索引，每条记录指向活文档位置。

### 6.4 其它文档清理

- `tutorials/README.md`：补充"新机器初次使用"三步引导。
- `reports/INDEX.md`：移除空的"当前优先阅读"占位，精简使用建议。
- `work-notes/INDEX.md`：更新为当前文件列表。
- `Agents.md`：精简为约 55 行。核心变更：新增"文档维护规则"小节（快捷键不进 README、jobs 完成立即归档、一问一主文档、改代码后同步文档）；移除写作系统模块边界细节（这些由代码中的 `make-obsolete-variable`、docstring 和 tutorial 承载）；项目结构改为简洁文字列表。

### 6.5 链接完整性验证

对所有文档（README、c.md、specs 三件、tutorials/README、legacy/README、reports/INDEX、work-notes/INDEX、jobs/README）做了内部链接批量检查，修复了 ARCHITECTURE.md 中 9 个相对路径错误（`../lisp/` 应为 `../../lisp/`），最终全项目 0 broken link。

## 7. 验证与已确认结果

本轮实际执行过的验证包括：

- `check-parens`：覆盖快捷键重构涉及的 Elisp 文件，以及最后修改的 `markdown-base.el`。
- `git diff --check`：多轮通过。
- `emacs --batch -l init.el --eval '(princ "ok\n")'`：配置能 batch 加载。
- 快捷键 batch 检查：
  - `C-c a` → `org-agenda`
  - `C-c A n` → `org-academic-create-idea-note`
  - `C-c n f` → `org-roam-node-find`
  - `C-c f p` → `henri/neotree-project-dir`
  - `C-c w e` → `eshell`
  - `C-c w v` → `henri/vterm`
  - `C-c l` → `org-store-link`
  - `C-c d r` → `quickrun`
  - Org buffer 中 `C-c m h/l/v` 分别命中 HTML、LaTeX、view 命令
  - F5 / F9 不再有全局主力绑定，Octave 中 `C-c d b` 命中 `octave-set-breakpoint`
- batch 副作用验证：临时 `henri-notes-directory` 下加载配置不创建 `agenda/`、`Roam/`、`Academic/`；显式 ensure 命令仍能创建目标目录。
- Markdown 预览验证：用 `writing-system-guide.md` 渲染临时 HTML，确认 `../../lisp/init-writing.el` 被改写为仓库内正确的 `file://` 绝对链接。

## 8. 当前状态与剩余事项

当前已经稳定的部分：

- Writing 系统四条主线边界更清楚：任务走 Agenda，经历走 Journal，长期知识走 Org-roam，正式学术产出走 Academic。
- Citar notes 默认归 `Academic/Reading/`，不再与 `Roam/references/` 分裂。
- 快捷键前缀已经有明确治理规则，并通过 `init-keys.el` 与 which-key 标题体现出来。
- Markdown EWW 预览的相对文件链接已经在代码层修复。
- README 瘦身到约 124 行，快捷键详情集中在 `keybindings.md`。
- specs 三件已同步为当前模块结构。
- 已完成 jobs 全部归档到 legacy，jobs 目录干净。
- Agents.md 精简为仓库工作规则，不含模块边界细节。
- 全项目文档内部链接 0 broken。

仍需后续关注的部分：

- `docs/jobs/keybindings-refactor.md` 若一段时间内没有新的冲突报告，可迁入 `docs/legacy/`。
- `henri-keybindings-enable-legacy-aliases` 未来可以在确认肌肉记忆迁移完成后删除。
- Org-roam、Academic、Agenda 的真实交互使用仍可能暴露更细的文案或模板摩擦。
- Markdown 预览链接修复建议在图形 Emacs 的 EWW 中做一次人工点验。

## 9. 结论边界

本轮可以视为写作系统从“功能堆叠”进入“治理收敛”的阶段：模块边界、快捷键前缀、文档体系、项目治理文件和 Markdown 预览链路都被重新对齐。文档系统从“README 承载一切”转变为分层结构：README 作为入口索引，快捷键集中在 tutorials，架构细节在 specs，完成的工作归入 legacy。已经确认的是配置可加载、核心快捷键能按新语义解析、batch 不再创建个人写作数据目录、Markdown 相对文件链接可被改写到真实仓库路径、全项目文档链接完整。尚未声称的是所有图形交互路径都已人工点验完毕，尤其是 EWW 中不同类型链接的点击行为还应在日常使用中继续观察。
