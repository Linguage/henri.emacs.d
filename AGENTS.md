# henri.emacs.d 项目约定

## 工具链

- **运行时**：GNU Emacs 29.1+
- **语言**：Emacs Lisp（`lexical-binding` 按需使用）
- **包管理**：`package.el`、`use-package`，ELPA 镜像（如清华源）
- **可移植路径**：个人机相关目录集中在 `lisp/init-custom.el` 的 `henri-paths` / `henri-*` `defcustom`；模块内用 `expand-file-name` 派生，禁止硬编码 `~/...`。
- **本机运行时目录**：`lisp/ops/paths.el`（`henri-runtime`）约定 `var/`、`.local/` 等，在 `after-init-hook` 中按需创建。
- **编辑与语言服务**：Eglot（LSP）、Tree-sitter、Flycheck
- **写作**：Org、Markdown、LaTeX/Pandoc（以 `lisp/writing/` 为准）
- **外部工具**：git、clangd、pylsp、fortls、pandoc、texlive 等

## 项目结构

- **入口**：`early-init.el`、`init.el`
- **实现**：`lisp/`（管理、样式、编程、写作、运维子目录）
- **文档**：`docs/specs/`（核心三件）、`docs/tutorials/`（使用指南）、`docs/jobs/`、`docs/legacy/`、`docs/reports/`、`docs/work-notes/`
- **操作清单**：本仓库根目录 `c.md`

## 文档系统协同（人 + AI）

### 核心三件（`docs/specs/`）

1. `BLUEPRINT.md`：宏观规划与理念。
2. `ARCHITECTURE.md`：模块职责、加载顺序、边界说明。
3. `ROADMAP.md`：阶段路线与 todo-list。

### 文档维护规则

1. **快捷键不进 README**：详情只在 `docs/tutorials/keybindings.md`，README 只放前缀速记和链接。
2. **jobs 保持干净**：`docs/jobs/` 除 `README.md` 外最多 3 份在制任务；完成的立即移入 `docs/legacy/`。
3. **一问一主文档**：同一问题只保留一个主文档，避免多真相源。
4. **work-notes vs reports**：经历归 work-notes，可复用结论归 reports。
5. **改代码后同步文档**：改了模块结构或加载顺序应更新 `ARCHITECTURE.md`；改了快捷键应更新 `keybindings.md`。

### Day 1 可归档工作流

当 AI 新增或扩展一条任务线时，应尽早补齐：`lifecycle`、`entrypoint`、`outputs`、`summary`、`smoke`、`freeze rule`。

## AI 开发约束

1. 新增或修改代码时，优先保证需求落地、可读性和回归稳定。
2. 路径一律用 `expand-file-name` 派生，禁止硬编码绝对路径。
3. 运行时目录创建必须在 `after-init-hook` 或更晚；个人写作数据目录/文件不得在 `noninteractive` batch 加载中自动创建。

若某个子目录下存在更贴近上下文的 `AGENTS.md`，则优先遵循该子目录的局部规则。
