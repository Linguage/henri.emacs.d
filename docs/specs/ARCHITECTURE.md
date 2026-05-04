# 架构说明

> 最近更新: 2026-05-04

## 配置分层

| 层级 | 入口 | 说明 |
|------|------|------|
| early-init | `early-init.el` | GC / UI / file-name-handler 优化 |
| core | `init.el` | 包初始化；加载 `init-custom` / `init-managing` / `init-styling` / `init-programming` / `init-writing` / ops |
| customization | [`lisp/init-custom.el`](../../lisp/init-custom.el) | 全部 `defgroup` / `defcustom`：主题策略、模块开关、路径、性能阈值 |
| visual | [`lisp/visual/`](../../lisp/visual/) | 字体 / 主题包 / UI 组件三分离；`init-visual.el` 统一加载 |
| managing | [`lisp/init-managing.el`](../../lisp/init-managing.el) | ivy / counsel / swiper / neotree / which-key / Magit / diff-hl |
| programming | [`lisp/init-programming.el`](../../lisp/init-programming.el) | company / eglot / tree-sitter / flycheck；语言桥接 |
| writing | [`lisp/init-writing.el`](../../lisp/init-writing.el) | Markdown / Org / LaTeX / PDF 总线 |
| ops | [`lisp/ops/`](../../lisp/ops/) | paths / backup / status / profiles / doctor / lib-* |

## 视觉系统 (`lisp/visual/`)

| 模块 | 职责 |
|------|------|
| `visual-fonts.el` | 全局字号/字体、`henri-big-font-mode`、Org CJK 字体候选与诊断 |
| `visual-themes.el` | `doom-themes`；`henri-theme-changed-hook` 触发 `henri/apply-fonts` |
| `visual-components.el` | frame 默认值、行号、`doom-modeline`、图标、可选 `centaur-tabs`、字号缩放 |

兼容入口：[`lisp/init-styling.el`](../../lisp/init-styling.el) 仅做 `(require 'init-visual)`。

兼容层：[`lisp/ops/lib-fonts.el`](../../lisp/ops/lib-fonts.el) 内部 `(require 'visual-fonts)`，供 doctor 等保持旧 `require`。

## 写作系统 (`lisp/writing/`)

### Org 子模块 (`lisp/writing/org/`)

| 模块 | 文件 | 角色 |
|------|------|------|
| Base | `org-base.el` | Org 行为、美化、face、视图控制 |
| LaTeX | `org-latex.el` | PDF 导出、`ctexart` 文档类、LaTeX 主题、字体诊断 |
| Journal | `org-journal.el` | GTD agenda（`agenda/*.org`）+ 月度日记（`Journal/journal-YYYY-MM.org`） |
| HTML | `org-html.el` | Org → HTML 主题系统 |
| Roam | `org-roam-henri.el` | 通用 Org-roam 知识库（`Roam/` 下按用途分目录） |
| Academic | `org-academic.el` | 学术写作模板（idea / reading / project / paper / abstract）+ Citar 引用 |

加载入口 [`lisp/writing/init-org.el`](../../lisp/writing/init-org.el) 按 `henri-org-enable-*` 开关条件加载各子模块。

模块边界：Academic 不覆盖 `org-roam-directory`；文献笔记默认归 `Academic/Reading/`（Citar notes 真源）；`Roam/references/` 只放通用资料节点。详见 [Writing 系统使用指南](../tutorials/writing-system-guide.md)。

### Markdown 子模块 (`lisp/writing/markdown/`)

| 模块 | 文件 | 角色 |
|------|------|------|
| Base | `markdown-base.el` | `markdown-mode` 基础配置与预览 |
| Export | `markdown-export.el` | PDF / HTML / docx 导出 |
| Nav | `markdown-nav.el` | TOC / outline / consult-imenu |
| Notes | `markdown-notes.el` | 截图、拖拽图片、字数统计 |
| Lint | `markdown-lint.el` | markdownlint 集成（可选） |
| Template | `markdown-template.el` | 博客 front-matter 模板（可选） |

### LaTeX 与 PDF

| 目录 | 说明 |
|------|------|
| `lisp/writing/LaTeX/` | AUCTeX 配置（`latexmk + XeLaTeX`）、主题库 |
| `lisp/writing/pdf/` | pdf-tools 集成 |

## 运维层 (`lisp/ops/`)

| 模块 | 说明 |
|------|------|
| `paths.el` | `henri-runtime`：`var/`、`.local/`、`tree-sitter/`、`rime/`；`after-init-hook` 中按需创建 |
| `backup.el` | 集中备份/自动保存 |
| `status.el` | `henri/show-module-status` |
| `profiles.el` | 多机器 profile（`profile-<name>.el`） |
| `doctor.el` | `M-x henri/doctor`（`C-c h d`）：依赖自检 |
| `lib-hooks.el` | `henri-first-{input,buffer,file}-hook` |
| `lib-system.el` | OS / 可执行文件检测 |
| `lib-fonts.el` | 视觉系统兼容层 |
| `lib-files.el` | 大文件 / `so-long` 策略、`henri-buffer-real-p` |

## 加载顺序要点

1. `init-custom` 必须早于所有使用 `defcustom` 的模块。
2. `visual-fonts` 会先于 `init-visual` 被加载一次（`init.el` 先 `require 'lib-fonts`）；`init-visual` 再次 `require` 为幂等。
3. `paths.el` 的运行时目录创建在 `after-init-hook`；个人写作数据目录/文件只在交互式启动或显式命令中创建，避免 `--batch` 加载污染笔记根目录。
4. `init-org.el` 中 roam 加载先于 academic，确保 `org-roam-directory` 不被 academic 覆盖。
5. Org academic 的 `org-academic-init` 通过 `after-init-hook` 延迟执行，避免与 roam 初始化竞争。
