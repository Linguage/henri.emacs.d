# henri.emacs.d

基于 Emacs 29.1 的个人配置，专注于提供现代化的编程和写作环境。

> 本配置已引入分层架构（early-init / core / programming / writing / ops），并通过 `defcustom` 提供可配置主题策略、模块开关、LSP 自动格式化与大文件优化等功能。详见下文“架构 & 配置开关”。

## 文档导航

| 文档 | 说明 |
|------|------|
| [操作清单](c.md) | 常用命令与路径索引 |
| [使用指南（tutorials）](docs/tutorials/README.md) | 基于本配置的专题指南，首篇为快捷键速查 |
| [蓝图](docs/specs/BLUEPRINT.md) | 宏观规划与文档边界 |
| [架构](docs/specs/ARCHITECTURE.md) | 模块与目录语义 |
| [路线图](docs/specs/ROADMAP.md) | 阶段任务 |
| [在制任务 jobs](docs/jobs/README.md) | 当前专题活页（如对齐全 Doom 分期能力） |

个人路径（笔记根、项目目录、Conda、LeetCode、Org HTML 主题等）集中在 [`lisp/init-custom.el`](lisp/init-custom.el) 的 `henri-*` `defcustom`，切换机器时优先改此处或通过 `M-x customize-group RET henri-paths`。

- **`henri-paths`**：`henri-notes-directory`、`henri-org-html-themes-directory`、`henri-shell`（子进程 / quickrun / vterm / `exec-path-from-shell` 使用的 shell，默认优先 `zsh` 否则 `/bin/sh`）等机器相关路径。
- **`henri-runtime`**（[`lisp/ops/paths.el`](lisp/ops/paths.el)）：`henri-var-directory`、备份/自动保存子目录、`tree-sitter`、`rime`、`transient` 等；目录在 **`after-init-hook`** 中按需创建，避免纯 `--batch` 加载配置时在仓库里 mkdir。

### 运行时目录与本机文件（Git 策略）

本仓库常用作 `user-emacs-directory`，根目录会出现 **仅本机需要** 的内容，默认 **不纳入版本控制**（见根目录 [`.gitignore`](.gitignore) 顶部的说明）：

| 路径/文件 | 说明 |
|-----------|------|
| `custom.el` | `custom-file` 目标；由 Emacs 写入，`init.el` 在存在时 `load`。 |
| `elpa/` | 已安装包 |
| `var/`、`var/backups/`、`var/autosave/` | 备份与自动保存（`henri-var-directory` 等见 [`lisp/ops/paths.el`](lisp/ops/paths.el)；首次创建发生在 `after-init-hook`） |
| `.local/cache/`、`.local/etc/` | 预留命名空间（同上，启动完成后创建） |
| `rime/` | Rime 用户数据（`henri-rime-directory`） |
| `tree-sitter/`、`transient/` 等 | 常见运行时目录 |
| `profile-*.el` | 可选「多机器 profile」叠加（见下文 `henri/load-profile`）；根目录 `.gitignore` 中有显式条目 |

未引入 **`no-littering`**：第三方包仍可能把状态写在默认位置；需要时可在后续单独评估。

## 1. 核心特性

- 优化的启动速度和性能表现
- 现代化的编程语言支持 (LSP + Tree-sitter)
- 专业的写作环境 (Org + Markdown + LaTeX)
- 美观的界面设计
- 可配置主题策略（固定/时间/随机）
- 模块化 Org 子功能按需加载
- 大文件自动降级优化（行号/语法检查等）
- 集中化备份与自动保存目录
- 分阶段启动钩子（`henri-first-input-hook` / `henri-first-buffer-hook` / `henri-first-file-hook`，见 [`lisp/ops/lib-hooks.el`](lisp/ops/lib-hooks.el)）与大文件 / so-long 策略（[`lisp/ops/lib-files.el`](lisp/ops/lib-files.el)）

## 2. 系统要求

- Emacs 29.1 或更高版本
- 外部依赖:
  - git
  - clangd (C/C++)
  - pylsp（Python，随 Eglot 使用；另有 elpy + company-jedi 见 `init-python.el`）
  - fortls (Fortran)
  - pandoc（Markdown 离线预览 / `markdown-mode` 转换）
  - grip（可选，GitHub 风格 Markdown 预览；`henri-enable-grip` 为 t 时使用）
  - BasicTeX/MacTeX 或 texlive (LaTeX)，需提供 `xelatex`、`latexmk`、`kpsewhich`
  - 实验 LaTeX-Diary 主题额外需要 `tikz`、`tikzpagenodes`、`eso-pic`、`ifoddpage`、`xargs`、`xstring`，但它不再是 Journal PDF 默认依赖
    - BasicTeX 缺包时优先使用 user-mode：`tlmgr init-usertree`，然后 `tlmgr --usermode install tikzpagenodes ifoddpage xargs xstring`

## 3. 主要模块

### 3.1 包管理 (init.el)

- use-package - 声明式包管理
- ELPA 镜像源配置 (清华源)

### 3.2 基础管理 (init-managing.el)

- ivy + counsel + swiper - 搜索与补全框架
- neotree - 文件树侧边栏
- which-key - 快捷键提示
- **Git（本地）**：Magit（`C-x g` / `C-c g …`）、`diff-hl` 行内/边距显示改动、`smerge-mode` 检测冲突文件并辅以 `C-c ^ …` 快速取舍片段

### 3.3 界面与视觉层 (`lisp/visual/` + `init-styling.el`)

视觉相关配置拆为 `lisp/visual/` 三层，由 [`lisp/visual/init-visual.el`](lisp/visual/init-visual.el) 统一加载；[`lisp/init-styling.el`](lisp/init-styling.el) **仅作为兼容入口**（`require 'init-visual`），`init.el` 仍可 `(require 'init-styling)`。

| 模块 | 职责 |
|------|------|
| `visual-fonts.el` | 全局字号/字体、`henri-big-font-mode`；Org CJK 候选、`henri/org-setup-body-font`、`M-x henri/org-font-diagnose`。 |
| `visual-themes.el` | `doom-themes`；`henri-theme-changed-hook` 触发 `henri/apply-fonts`（主题策略函数仍在 [`lisp/init-custom.el`](lisp/init-custom.el)）。 |
| `visual-components.el` | 默认 frame、行号/高亮、`doom-modeline`、`nerd-icons` / `all-the-icons`、可选 `centaur-tabs`；GUI 初次 `henri/apply-fonts`；`C-=` / `C--` / `C-c F r` / `C-c F b`。 |

- **`lisp/ops/lib-fonts.el`**：薄兼容层，内部 `(require 'visual-fonts)`，供 [`lisp/ops/doctor.el`](lisp/ops/doctor.el) 等保持原有 `require`。
- **彩虹括号** `rainbow-delimiters`：已迁至 [`lisp/init-programming.el`](lisp/init-programming.el)（`prog-mode` hook）。
- 编程字体（如 JetBrains Mono 等候选）仍在 `visual-fonts` 的 `henri/set-font` 中按 OS 选择。

### 3.4 编程环境 (init-programming.el)

- company-mode - 代码补全
- eglot - LSP 客户端
- tree-sitter - 语法分析
- flycheck - 语法检查
- 语言支持:
  - Python
  - C/C++
  - Common Lisp
  - Fortran
  - Julia

#### 3.4.1 Python 环境 (init_python.el)

- **环境管理**
  - conda - Conda 环境管理（默认环境名由 `henri-conda-default-env` 控制，默认 `Henri_env`）
  - pyvenv - 虚拟环境支持
  
- **开发工具**
  - elpy - Python IDE 功能集成
  - company-jedi - 智能代码补全
  - flycheck - 实时语法检查
  
- **交互式开发**
  - jupyter - Jupyter Notebook 支持
  - ein - 集成 Notebook 环境
  - dap-mode - 交互式调试支持

- **代码质量**
  - blacken - Black 格式化工具
  - py-isort - Import 语句排序
  - pyflakes - 语法检查工具

#### 3.4.2 Lisp 环境 (init-lisp.el)

- **SLIME 集成**
  - slime-fancy - 核心功能扩展
  - slime-asdf - ASDF 构建系统支持
  - slime-quicklisp - Quicklisp 包管理
  - slime-repl - 增强的交互环境
  
- **开发辅助**
  - paredit - 结构化编辑
  - rainbow-delimiters - 彩虹括号匹配
  - macrostep - 宏展开支持
  
- **文档与补全**
  - slime-autodoc - 自动文档显示
  - company-quickhelp - 文档快速查看
  - lisp-extra-font-lock - 增强的语法高亮

- **调试工具**
  - sly - 替代 SLIME 的现代开发环境
  - realgud - 统一调试器界面

### 3.5 写作环境 (init-writing.el)

- **Markdown**：`markdown-mode`；离线预览（`pandoc` + 优先 EWW，见 `C-c m p`）；GitHub 风格预览（`grip` + `C-c m g` / 原 `C-c C-g`）；`C-c m c` 检查依赖是否就绪
- **Org → HTML**：主题树在 [`lisp/writing/org/org-html-themes`](lisp/writing/org/org-html-themes)（含 fniessen 上游 `src/` 与自定义 `henri-bearblog/`），由 `henri-org-html-themes-directory` 指向；本地 setup 使用占位符 `@@henri-org-html-themes-root@@`，在 HTML 导出时替换为实际路径。旧路径 `~/Documents/EmacsNotes/org-html-themes` 可弃用。无主题目录时可用 [`lisp/writing/org/install-themes.sh`](lisp/writing/org/install-themes.sh) 仅克隆上游（不含 Bearblog，需以仓库内版本为准）。
- Org Mode
- LaTeX：`.tex` 编辑配置集中在 `lisp/writing/LaTeX/`；AUCTeX 默认使用 `latexmk + XeLaTeX`，macOS 会兜底加入 `/Library/TeX/texbin`；主题库在 `lisp/writing/LaTeX/themes/`。Journal PDF 默认使用常规 `ctex + geometry` 模板，LaTeX-Diary 暂作为实验主题保留。

### 3.6 运维层与通用库 (`lisp/ops/`)

**治理（本机路径 / 诊断）**

- [`paths.el`](lisp/ops/paths.el)、[`backup.el`](lisp/ops/backup.el)、[`status.el`](lisp/ops/status.el) — 同上节「henri-runtime」与状态
- [`profiles.el`](lisp/ops/profiles.el) — 若存在可读文件 `profile-<name>.el`（`name` = 环境变量 `HENRI_PROFILE` 或 `(system-name)`）则 `henri/load-profile` 加载
- [`doctor.el`](lisp/ops/doctor.el) — `M-x henri/doctor`（`C-c h d`）

**通用库**

- [`lib-hooks.el`](lisp/ops/lib-hooks.el) — `henri-first-{input,buffer,file}-hook`
- [`lib-system.el`](lisp/ops/lib-system.el)、[`lib-fonts.el`](lisp/ops/lib-fonts.el)、[`lib-files.el`](lisp/ops/lib-files.el) — 可执行检测、字体、大文件与 `henri-buffer-real-p`

**脚本**：`scripts/profile-startup.el`、`scripts/generate-health.el`

### 3.7 本阶段常用快捷键（字体 / 自检）

| 按键 | 说明 |
|------|------|
| `C-=` / `C--` | `henri/font-size-adjust`；`C-u N` 前缀一次 N 档 |
| `C-c F r` / `C-c F b` | 字号重置 / 演示大字号模式 |
| `C-c h d` | `henri/doctor` |

## 3.x 架构 & 层次

| 层级 | 说明 | 入口 |
|------|------|------|
| early-init | 启动前 GC / UI / file-name-handler 优化 | `early-init.el` |
| core | 包初始化 + `init-custom` / `paths` / `profiles` / `lib-*` / `doctor` / `init-dashboard` 等 | `init.el` |
| customization | 所有 defgroup/defcustom | `lisp/init-custom.el` |
| startup dashboard | 启动页、`initial-buffer-choice`、笔记快捷命令 | `lisp/init-dashboard.el` |
| managing/styling | 导航/补全/界面主题/标签 | `lisp/init-managing.el` / `lisp/init-styling.el` |
| programming | LSP / 运行 / 调试 / 语言桥接 | `lisp/init-programming.el` |
| writing | Markdown / Org / LaTeX | `lisp/init-writing.el` |
| ops | 运维与通用库：`paths` / `backup` / `status` / `profiles` / `doctor` / `lib-*` | `lisp/ops/*` |

## 3.x 配置开关（部分）

可通过 `M-x customize-group RET henri-core` 等分组修改：

| 变量 | 功能 |
|------|------|
| `henri-theme-mode` | 主题策略: `time` / `random` / `fixed` |
| `henri-theme-day-theme` / `night-theme` / `fixed-theme` | 对应主题符号 |
| `henri-enable-centaur-tabs` | 启用标签页 |
| `henri-enable-magit` / `henri-enable-leetcode` / `henri-enable-grip` | 可选重型组件 |
| `henri-org-enable-base` / `-latex` / `-journal` / `-html` / `-academic` | Org 子模块开关 |
| `henri-lsp-auto-format` | 保存时自动格式化 LSP buffer |
| `henri-lsp-format-size-threshold` | 超过阈值不自动格式化 |
| `henri-large-file-threshold` | 大文件优化触发阈值（字节）|
| `henri-backup-enable` | 集中备份/自动保存开关 |
| `henri-health-report-on-startup` | 启动后输出健康信息 |
| `henri-active-profile` | 当前 profile 名；可选文件 `profile-<name>.el` 见 [`profiles.el`](lisp/ops/profiles.el) |

## 3.x 新增交互命令

| 命令 | 说明 |
|------|------|
| `henri/apply-current-theme` | 按策略应用主题 |
| `henri/select-theme` | 交互选择主题并切换为 fixed 模式 |
| `henri/report-health` | 输出健康摘要（可 `C-u` 详细）|
| `henri/show-module-status` | 查看各模块启用/加载状态 |
| `henri/profile-startup-report` | 启动性能快照（用于脚本或 M-x）|
| `henri/generate-health-json` | 批处理输出健康 JSON（脚本调用）|
| `henri/doctor` | `C-c h d`：依赖自检（含特性 loaded/available/MISSING）|
| `henri/font-size-adjust` | `C-=` / `C--`（支持数字前缀）；`henri/font-size-reset`、`henri-big-font-mode` 见上表 |

## 4. 性能优化

- 垃圾回收优化
- 启动加载优化
- 显示渲染优化
- LSP 格式化大小阈值控制
- 大文件自动降级（关闭行号、flycheck、降级高亮）
- 主题策略避免重复加载（禁用旧主题再启用新主题）
- 部分重型包按需懒加载 (magit / leetcode / grip / centaur-tabs)

## 5. 快捷键绑定

### 5.1 全局快捷键

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `M-x` | counsel-M-x | 增强的命令执行 |
| `C-x C-f` | find-file | Emacs 默认；未在全局 remap |
| `C-c f n` | henri/find-file-in-notes | 自笔记根起的 counsel-find-file（若已安装 counsel） |
| `C-s` | swiper | 交互式搜索 |
| `C-x g` | magit-status | Git 状态管理 |
| `<f8>` | neotree-toggle | 切换文件树 |
| `<f5>` | quickrun | 快速运行代码 |

#### Git（Magit / diff-hl）

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-x g` / `C-c g g` | magit-status | 仓库状态 |
| `C-c g d` | magit-dispatch | 命令面板 |
| `C-c g f` | magit-file-dispatch | 当前文件相关 Git 操作 |
| `C-c g b` | magit-blame | 逐行追溯 |
| `C-c g l` | magit-log-buffer-file | 当前文件历史 |
| `C-c v n` / `C-c v p` | diff-hl-next/previous-hunk | 跳转下/上一处改动 |
| `C-c v r` | diff-hl-revert-hunk | 还原当前 hunk（慎用） |

合并冲突缓冲区内（`smerge-mode`）：`C-c ^ n` / `C-c ^ p` 下一处冲突；`C-c ^ u` / `C-c ^ l` 保留上/下版本；`C-c ^ b` 保留基线；`C-c ^ a` 保留全部。

### 5.2 编程相关快捷键

#### 通用编程

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c e f` | eglot-format | LSP 代码格式化 |
| `C-' C-'` | imenu-list-smart-toggle | 代码导航栏 |
| `<f6>` | realgud:pdb | Python 调试器 |

#### Lisp 开发

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c C-z` | slime | 启动 SLIME REPL |
| `C-c C-c` | slime-compile-defun | 编译当前函数 |
| `C-c C-l` | slime-load-file | 加载文件到 REPL |
| `C-c i` | slime-inspect | 检查对象 |

#### Emacs Lisp

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c C-c` | eval-defun | 求值当前函数 |
| `C-c C-b` | eval-buffer | 求值整个缓冲区 |
| `C-c C-r` | eval-region | 求值选中区域 |

### 5.3 写作相关快捷键

#### Markdown

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c C-v` | markdown-preview | 内置预览（pandoc） |
| `C-c C-c p` | markdown-preview-mode | 预览模式 |
| `C-c m p` | henri/markdown-preview-offline | 离线：优先 EWW，需 pandoc |
| `C-c m g` | henri/markdown-preview-github-style | GitHub 风格：需 grip 且 `henri-enable-grip` |
| `C-c m c` | henri/markdown-check-preview-deps | 检查 pandoc / grip 是否在 PATH |
| `C-c C-g` | grip-mode | 直接开关 grip（与 `C-c m g` 等价入口之一） |

#### Org Mode - 基础操作

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c C-e` | org-export-dispatch | 导出菜单 |
| `C-c C-l` | org-insert-link | 插入链接 |
| `C-c C-t` | org-todo | 切换 TODO 状态 |
| `C-c a` | org-agenda | 打开议程 |
| `C-c c` | org-capture | 快速创建日志 |

#### Org Mode - 视图控制

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c C-b` | henri/toggle-org-bullets | 切换 bullets 样式 |
| `C-c v s` | henri/org-show-all | 展开所有内容（org-fold-show-all） |
| `C-c v o` | henri/org-overview | 仅显示大纲 |
| `C-c v c` | henri/org-content | 显示内容标题 |
| `C-c o v` | henri/cycle-org-startup-folded | 循环切换启动折叠状态 |

#### Org Journal - 日志管理

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c j s` | henri/search-journal | 搜索日志内容 |
| `C-c j d` | henri/view-diary-by-date | 按日期查看日记 |

#### Org HTML - 主题与导出

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c h t` | henri/org-html-set-theme | 选择并设置 HTML 主题（写入 SETUPFILE） |
| `C-c h d` | henri/org-html-apply-default-theme | 应用默认主题（Henri Journal） |
| `C-c h e` | henri/org-html-export-and-open | 导出并在浏览器打开 |
| `C-c h w` | henri/org-html-export-with-theme | 用指定主题导出（不修改源 buffer） |
| `C-c h s` | henri/org-html-apply-theme-by-shortcut | 缩写应用主题（0/1/2/rto/bb/...） |
| `C-c h ?` | henri/org-html-show-theme-shortcuts | 列出所有缩写映射 |
| `C-c h r` | henri/org-html-remove-theme | 移除主题 SETUPFILE |
| `C-c h k` | henri/org-html-check-local-themes | 检查本地主题文件 |
| `C-c h i` | henri/org-html-install-themes | 下载/安装主题 |

#### Org LaTeX - PDF 导出

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c l t` | org-latex-apply-theme | 选择 LaTeX 主题 |
| `C-c l p` | org-export-pdf-with-theme | 用指定主题导出 PDF |
| `C-c l q` | org-export-pdf-quick | 快速导出 PDF |
| `C-c l d` | org-latex-diagnose-fonts | 诊断字体、TeX 命令、`ctex.sty` 与实验 LaTeX-Diary 主题资源 |
| `C-c l r` | org-latex-reload-config | 重新加载 latexmk + XeLaTeX 导出配置 |

#### Org Academic - 学术写作

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c a p` | org-academic-create-paper | 创建学术论文 |
| `C-c a n` | org-academic-create-research-note | 创建研究笔记 |
| `C-c a c` | org-academic-create-conference-abstract | 创建会议摘要 |
| `C-c a P` | org-academic-quick-paper | 快速创建论文 |
| `C-c a N` | org-academic-quick-note | 快速创建笔记 |
| `C-c a b` | org-academic-setup-bibliography | 设置 BibTeX、PDF 与文献笔记目录 |
| `C-c a d` | org-academic-dashboard | 打开学术写作仪表板 |
| `C-c a i` | org-academic-insert-citation | 通过 citar 插入引用（未加载时手动回退） |

### 5.4 界面操作快捷键

#### 标签页管理

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-<prior>` / `C-<next>` | centaur-tabs-backward / forward | 上一/下一标签（`init-styling.el`） |
| `鼠标滚轮` | 标签页切换 | 在启用 centaur-tabs 后由配置绑定 |

#### 文件树操作

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c n p` | henri/neotree-project-dir | 打开项目目录 |
| `Enter` | 打开文件/目录 | 在 neotree 中 |
| `g` | 刷新目录树 | 在 neotree 中 |
| `A` | 最大化/还原 neotree | 在 neotree 中 |

### 5.5 帮助和导航

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-h f` | helpful-callable | 查看函数帮助 |
| `C-h v` | helpful-variable | 查看变量帮助 |
| `C-h k` | helpful-key | 查看快捷键帮助 |
| `C-h x` | helpful-command | 查看命令帮助 |

### 5.6 自定义快捷键说明

- 所有快捷键都经过精心设计，避免与 Emacs 默认绑定冲突
- 使用 `C-c` 前缀的快捷键都是用户自定义的安全绑定
- 功能键 `<f5>` - `<f8>` 用于常用开发工具的快速访问
- 写作相关快捷键遵循各模式的标准约定

## 6. 安装说明

1. 备份现有配置:

```bash
mv ~/.emacs.d ~/.emacs.d.bak
```

1. 克隆仓库:

```bash
git clone https://github.com/Linguage/henri.emacs.d.git ~/.emacs.d
```

1. 启动 Emacs，系统将自动安装所需包。

### 6.1 可选：采集启动基线

```bash
emacs -Q --load early-init.el --load init.el --eval '(henri/profile-startup-report)' --kill
```

### 6.2 生成健康快照 JSON

```bash
emacs --quick --load early-init.el --load init.el \
  --load scripts/generate-health.el \
  --eval '(henri/generate-health-json "health.json")' --kill
cat ~/\.emacs.d/health.json
```

## 6.3 主题策略快速切换

```elisp
(setq henri-theme-mode 'random)
(henri/apply-current-theme)
```

## 6.4 临时禁用某 Org 功能（示例）

```elisp
(setq henri-org-enable-academic nil)
;; 重载写作模块或下次启动生效
```


## 7. 贡献

欢迎提交 Issue 和 Pull Request。

## 8. 许可

MIT License
