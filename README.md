# henri.emacs.d

基于 Emacs 29.1 的个人配置，专注于提供现代化的编程和写作环境。

## 文档导航

| 文档 | 说明 |
|------|------|
| [操作清单 c.md](c.md) | 常用命令、路径与诊断入口 |
| [快捷键速查](docs/tutorials/keybindings.md) | 本仓库**所有显式绑定**的快捷键一览表 |
| [Writing 系统使用指南](docs/tutorials/writing-system-guide.md) | Agenda / Journal / Org-roam / Academic / Markdown / Org 导出 |
| [Org-roam 专题指南](docs/tutorials/org-roam-guide.md) | Roam 分类、节点、链接、Map 与复盘操作手册 |
| [Org HTML 主题指南](docs/tutorials/html-theme-guide.md) | HTML 主题目录约定与维护 |
| [蓝图](docs/specs/BLUEPRINT.md) | 项目定位、核心挑战与验收标准 |
| [架构](docs/specs/ARCHITECTURE.md) | 配置分层、模块职责与加载顺序 |
| [路线图](docs/specs/ROADMAP.md) | 阶段任务与进度 |
| [在制任务](docs/jobs/README.md) | 当前正在推进的专题 |

## 个人路径与配置开关

路径集中化管理：机器相关目录在 [`lisp/init-custom.el`](lisp/init-custom.el) 的 `defcustom` 中定义，通过 `M-x customize-group RET henri-paths` 调整。

| 分组 | 关键变量 | 说明 |
|------|----------|------|
| `henri-paths` | `henri-notes-directory` | 笔记/Journal/Roam 等写作根目录 |
| `henri-paths` | `henri-projects-directory` | Projectile 搜索根 |
| `henri-paths` | `henri-shell` | 子进程使用的 shell（默认 zsh，回退 /bin/sh） |
| `henri-writing` | `henri-org-enable-*` | Org 各子模块开关（base / latex / journal / html / roam / academic） |
| `henri-writing` | `henri-org-roam-directory` | Org-roam 知识库根目录 |
| `henri-writing` | `henri-md-enable-*` | Markdown 子模块开关 |
| `henri-theme` | `henri-theme-mode` | 主题策略：`time` / `random` / `fixed` |
| `henri-runtime` | — | 运行时目录（`var/`、`.local/`、`tree-sitter/` 等），[`lisp/ops/paths.el`](lisp/ops/paths.el) 在 `after-init-hook` 中创建 |
| `henri-programming` | `henri-lsp-auto-format` | 保存时自动格式化 LSP buffer |

运行时目录与本机文件（`custom.el`、`elpa/`、`var/` 等）默认不纳入版本控制，见根目录 [`.gitignore`](.gitignore)。

## 快捷键总览

完整快捷键表见 [docs/tutorials/keybindings.md](docs/tutorials/keybindings.md)。

常用前缀速记：

| 前缀 | 域 | 典型入口 |
|------|----|----------|
| `C-c c` | capture | `C-c c t` 快速 TODO、`C-c c d` 个人日记 |
| `C-c a` | agenda | 打开 Org Agenda 分发 |
| `C-c A` | academic | `C-c A n` 想法卡、`C-c A p` 论文 |
| `C-c f` | find / file tree | `C-c f n` 查找 Notes 文件、`C-c f p` 打开项目文件树 |
| `C-c n` | Org-roam | `C-c n f` 查找节点、`C-c n i` 插入链接、`C-c n j` daily |
| `C-c o` | Org 视图 | `C-c o a` 今日 Dashboard、`C-c o s` 今日三栏、`C-c o i` Inbox |
| `C-c h` | help / doctor | `C-c h d` 自检 |
| `C-c l` | store-link | 保留给 Org link |
| `C-c m` | mode-local | Markdown 预览；Org 中 `C-c m h/l/v` |
| `C-c g` | Git | `C-c g g` 状态、`C-c g n/p` hunk 跳转 |
| `C-c w` | window / terminal | `C-c w e` eshell、`C-c w v` vterm |
| `C-c d` | debug/run | `C-c d r` quickrun |
| `C-c e` | Eglot | `C-c e f` 格式化 |

## 系统要求

- Emacs 29.1+
- 外部依赖：git、clangd (C/C++)、pylsp (Python)、fortls (Fortran)、pandoc (Markdown)、BasicTeX/MacTeX 或 texlive（`xelatex` + `latexmk`）
- 可选：grip（GitHub 风格 Markdown 预览，需 `henri-enable-grip`）

## 项目结构

```
henri.emacs.d/
├── early-init.el / init.el          # 启动入口
├── lisp/
│   ├── init-custom.el               # defcustom 集中定义
│   ├── init-managing.el             # 导航/补全/Magit
│   ├── init-styling.el              # 视觉兼容入口 → lisp/visual/
│   ├── init-programming.el          # LSP/Tree-sitter/Flycheck/语言
│   ├── init-writing.el              # Markdown/Org/LaTeX/PDF 总线
│   ├── visual/                      # 字体/主题/UI 三层分离
│   ├── writing/
│   │   ├── org/                     # Org 子模块：base/journal/html/latex/roam/academic
│   │   ├── markdown/                # Markdown 子模块：export/nav/notes/lint/template
│   │   ├── LaTeX/                   # AUCTeX 配置与主题
│   │   └── pdf/                     # pdf-tools
│   ├── programming_languages/       # 语言专属配置
│   └── ops/                         # 运维：paths/backup/doctor/profiles/lib-*
├── docs/
│   ├── specs/                       # 蓝图/架构/路线图
│   ├── tutorials/                   # 使用指南与快捷键速查
│   ├── jobs/                        # 在制任务
│   ├── legacy/                      # 已归档任务
│   ├── reports/                     # 经验沉淀
│   └── work-notes/                  # 研发经历记录
└── c.md                             # 操作清单与路径索引
```

## 写作系统模块划分

| 模块 | 入口文件 | 角色 | 数据落点 |
|------|----------|------|----------|
| Agenda / GTD | [`org-journal.el`](lisp/writing/org/org-journal.el) | 任务管理 | `EmacsNotes/agenda/*.org` |
| Journal | [`org-journal.el`](lisp/writing/org/org-journal.el) | 经历流水 | `EmacsNotes/Journal/journal-YYYY-MM.org` |
| Org-roam | [`org-roam-henri.el`](lisp/writing/org/org-roam-henri.el) | 通用知识库 | `EmacsNotes/Roam/{inbox,daily,notes,references,projects,people,maps}` |
| Academic | [`org-academic.el`](lisp/writing/org/org-academic.el) | 学术写作 | `EmacsNotes/Academic/{Ideas,Reading,Projects,Papers,PDFs}` |
| Markdown | [`lisp/writing/markdown/`](lisp/writing/markdown/) | 普通写作 | 任意 `*.md` |
| Org 导出 | [`org-html.el`](lisp/writing/org/org-html.el) / [`org-latex.el`](lisp/writing/org/org-latex.el) | HTML / PDF 导出 | 同源目录 |

详细使用方法见 [Writing 系统使用指南](docs/tutorials/writing-system-guide.md)。

## 安装

```bash
git clone https://github.com/Linguage/henri.emacs.d.git ~/.emacs.d
# 启动 Emacs，自动安装依赖包
```

可选操作：

```bash
# 采集启动基线
emacs -Q --load early-init.el --load init.el --eval '(henri/profile-startup-report)' --kill

# 生成健康快照
emacs --quick --load early-init.el --load init.el \
  --load scripts/generate-health.el \
  --eval '(henri/generate-health-json "health.json")' --kill
```

## 贡献与许可

欢迎提交 Issue 和 Pull Request。MIT License。
