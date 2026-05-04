# Markdown 子模块丰富化实施计划

- `状态`: done
- `最近更新`: 2026-05-04
- `lifecycle`: active patch
- `entrypoint`: `lisp/writing/markdown/` 子模块群 + `lisp/init-writing.el` + `lisp/init-custom.el`
- `outputs`:
  - **Phase 1 核心交付**: `lisp/writing/markdown/markdown-export.el`（PDF / HTML / docx 导出）
  - **Phase 2 导航交付**: `lisp/writing/markdown/markdown-nav.el`（TOC / outline / consult-imenu）
  - **Phase 3 笔记增强交付**: `lisp/writing/markdown/markdown-notes.el`（截图贴入 / 拖拽图片 / 字数统计）
  - **Phase 4（可选）**: `lisp/writing/markdown/markdown-lint.el`
  - **Phase 5（可选）**: `lisp/writing/markdown/markdown-template.el`
  - **加载入口更新**: `lisp/init-writing.el` 由单文件 `(require 'markdown-base)` 改为子模块总线模式
  - **自定义开关**: `lisp/init-custom.el` 新增 `henri-md-enable-*` 系列 toggles
- `summary`: Markdown 子模块由单文件 `markdown-base.el` 扩展为分层子模块，补齐导出（尤其 PDF，与 LaTeX 模块联动）、导航、笔记型增强三大核心能力；保持"按需开关 + 离线优先"价值观；键位沿用 `C-c m` which-key 前缀。
- `smoke`:
  1. `emacs --batch -Q --load early-init.el --load init.el` 启动通过；
  2. 打开任意 `.md` 文件，`C-c m e p` 调用 `henri/md-export-pdf` → 同目录生成同名 `.pdf`（需 pandoc + xelatex）；
  3. `C-c m e h` / `C-c m e d` 分别生成 `.html` / `.docx`；
  4. `C-c m t i` 在当前光标处插入自动 TOC，`C-c m t r` 刷新已有 TOC；
  5. `C-c h d` 的 doctor 报告中出现 `xelatex` / `tectonic` / `markdown-toc` 等检查项；
  6. `C-c m i s` 在 macOS 下调用 `pngpaste` 将截图落地到 `<filename>.assets/YYYY-MM-DD_HH-MM-SS.png` 并插入相对路径。
- `freeze rule`: Phase 1～3 全部 smoke 验证通过；`init-writing.el` 加载结构稳定；`henri-md-enable-export` / `henri-md-enable-toc` / `henri-md-enable-notes` 三个开关各自独立生效；本文档迁入 `docs/legacy/`。

---

## 1. 背景

当前 `lisp/writing/markdown/` 仅含单文件 [`markdown-base.el`](../../lisp/writing/markdown/markdown-base.el)，负责：

- `markdown-mode` 语法支持（pandoc 作为 `markdown-command`）
- 三种预览：`markdown-preview-mode`（自带）、`markdown-preview-eww`（离线）、`grip-mode`（GitHub 风格）
- 预览依赖体检命令 `henri/markdown-check-preview-deps`

由 [`init-writing.el`](../../lisp/init-writing.el) 直接 `(require 'markdown-base)` 加载，无子模块分层，无 `henri-md-enable-*` 类开关。与之对比，Org 子模块已有 `init-org.el` 总线 + `henri-org-enable-base/html/latex/journal/academic` 完整开关体系。

本次盘点发现**导出能力是明显缺口**（仅 `C-c C-c e` 原生 HTML），而预览已饱和；导航、笔记型增强（截图贴入）、YAML front-matter 等视场景有补充价值。

---

## 2. 目标

1. **补齐导出管线**（Phase 1）：通过 pandoc 实现 Markdown → PDF / HTML / docx，PDF 路径复用 [`lisp/writing/LaTeX/`](../../lisp/writing/LaTeX/) 的中文字体与主题能力。
2. **增强导航与结构**（Phase 2）：自动 TOC 维护、consult-imenu 集成、outline-minor-mode 默认开启。
3. **笔记型工作流**（Phase 3）：截图贴入、拖拽图片归档、中文字数统计。
4. **开关同构**：新增 `henri-md-enable-export` / `toc` / `notes` / `lint` 系列 `defcustom`，与 Org 子模块开关风格一致，全部默认 `t`（lint 默认 `nil`）。
5. **加载结构升级**：`lisp/writing/markdown/` 下新增 `init-markdown.el` 作为子模块总线（或直接在 `init-writing.el` 中条件加载，视改动最小化原则决定，见第 5 节）。

---

## 3. 非目标

1. 不替换 `markdown-base.el` 中已有的预览体系（`markdown-preview-eww` / `grip-mode` 保持不动）；
2. 不引入 `markdown-live-preview-mode`（与现有 EWW / grip 重复）；
3. 不引入 mermaid / pandoc-filter 等图表管线（仅少数人需要，按需自行扩展）；
4. `markdownlint` 默认关闭（需 node 依赖，与"离线优先"冲突），Phase 4 只做可选封装；
5. 不创建 hugo/jekyll 静态站点工作流（Phase 5 仅提供 front-matter 模板，不集成站点生成）。

---

## 4. 现状盘点（已确认）

| 维度 | 现状 | 评价 |
|------|------|------|
| 语法 / 编辑 | `markdown-mode` + tree-sitter grammar 已注册 | 够用 |
| 预览 | ① `markdown-preview-mode` ② `markdown-preview-eww`（pandoc → EWW）③ `grip-mode` | 三种齐备，已覆盖离线 + GitHub 双通道 |
| 依赖体检 | `henri/markdown-check-preview-deps` + `doctor.el` 已检查 pandoc / grip | 与"离线优先 + 体检"约定一致 |
| 导出 | **仅有** `C-c C-c e`（`markdown-mode` 自带 HTML） | **明显缺口** |
| 目录 / 导航 | 仅 `imenu`，无自动 TOC | 缺口 |
| YAML front-matter | 无专用支持 | 视场景，按需 |
| Lint / 写作辅助 | 无 markdownlint / proselint | 视场景，按需 |
| 表格 / 图片 | `markdown-mode` 自带；无拖拽贴图 | 写笔记时常缺 |
| 与 LaTeX 协作 | 未打通（pandoc → XeLaTeX 模板可复用 `latex-themes-simple.el`） | 可联动收益最高 |

---

## 5. 必要性评估

| 功能 | 必要性 | 理由 |
|------|--------|------|
| **Markdown → PDF（pandoc + XeLaTeX）** | **高** | 与 Org PDF 流派对齐；接收外部 md 时可直接出 PDF；可复用 `latex-themes-simple.el` 的 CJK 字体方案 |
| **Markdown → docx / HTML（pandoc）** | 中-高 | 共享同一套 `henri/md-export` 命令族，零增量 |
| **目录（TOC）自动生成与刷新** | 中 | 长文档刚需，`markdown-toc` 包很轻 |
| **outline / consult-imenu 集成** | 中 | 已用 consult，做 1 行胶水即得 |
| **YAML front-matter 模板 + 跳转** | 中 | 若写博客 / Hugo 站才有意义 |
| **markdownlint（flymake）** | 低-中 | 需要 node 依赖，与"离线优先"冲突；只对发布管线有用 |
| **proselint / 中文字数统计** | 低 | 中文 proselint 不适用；字数统计 `markdown-mode` 自带 `M-x count-words` |
| **图片拖拽 / 截图贴入** | 中 | 笔记型 md 高频；需要小段 dnd 钩子 + 文件命名规则 |
| **markdown-live-preview-mode** | 低 | 已经有 EWW / grip，再加价值不高 |
| **mermaid / pandoc filter** | 低 | 只有渲染图表的人需要，按需 |

**结论**：值得做的核心是 **导出（尤其 PDF）+ 目录与导航 + 截图贴入**；其它视使用场景按需启用即可。

---

## 6. 设计

### 6.1 目录结构

```
lisp/writing/markdown/
├── markdown-base.el      ; 已存在：语法、预览、体检
├── markdown-export.el    ; Phase 1：导出（PDF/HTML/docx）
├── markdown-nav.el       ; Phase 2：TOC、outline、consult-imenu
├── markdown-notes.el     ; Phase 3：截图、拖拽、字数统计
├── markdown-lint.el      ; Phase 4（可选）：markdownlint flymake 后端
└── markdown-template.el  ; Phase 5（可选）：YAML front-matter 模板
```

`init-writing.el` 当前直接 `(require 'markdown-base)`。为最小化改动，有两种加载策略可选：

- **策略 A（保守）**：在 `init-writing.el` 中保留 `(require 'markdown-base)`，再逐条条件加载后续文件：
  ```elisp
  (require 'markdown-base)
  (when henri-md-enable-export (require 'markdown-export))
  (when henri-md-enable-toc   (require 'markdown-nav))
  ...
  ```
- **策略 B（对齐 Org）**：新增 `lisp/writing/markdown/init-markdown.el` 作为子模块总线，`init-writing.el` 改为 `(require 'init-markdown)`；总线内部按开关条件加载各文件。

**推荐策略 A**：Markdown 当前规模小于 Org，无需额外一层总线；待文件数超过 5 个且开关复杂度增加时，再迁移到策略 B。

### 6.2 自定义开关（写入 `init-custom.el`）

新增到 `:group 'henri-writing`：

```elisp
;; Markdown sub-module toggles ------------------------------------------------
(defcustom henri-md-enable-export t
  "启用 Markdown → PDF/HTML/docx 导出模块 `markdown-export'."
  :type 'boolean :group 'henri-writing)

(defcustom henri-md-enable-toc t
  "启用 Markdown 目录/导航模块 `markdown-nav'（含 markdown-toc、outline）。"
  :type 'boolean :group 'henri-writing)

(defcustom henri-md-enable-notes t
  "启用 Markdown 笔记增强模块 `markdown-notes'（截图、拖拽、字数统计）。"
  :type 'boolean :group 'henri-writing)

(defcustom henri-md-enable-lint nil
  "启用 Markdown lint 模块 `markdown-lint'（需 markdownlint CLI）。"
  :type 'boolean :group 'henri-writing)

(defcustom henri-md-pdf-engine "xelatex"
  "Markdown → PDF 使用的 pandoc LaTeX 引擎。
可选 \"xelatex\" 或 \"tectonic\"。"
  :type '(choice (const "xelatex") (const "tectonic"))
  :group 'henri-writing)

(defcustom henri-md-export-output-dir nil
  "导出输出目录；nil 表示与源文件同目录。"
  :type '(choice (const :tag "与源文件同目录" nil)
                 (directory :tag "指定目录"))
  :group 'henri-writing)
```

### 6.3 键位设计

沿用 `C-c m` 前缀（已在 [`init-managing.el`](../../lisp/init-managing.el) 注册 which-key `"henri/markdown"`），在 `markdown-mode-map` 下追加：

| 键位 | 命令 | 来源 |
|------|------|------|
| `C-c m e p` | `henri/md-export-pdf` | Phase 1 |
| `C-c m e h` | `henri/md-export-html` | Phase 1 |
| `C-c m e d` | `henri/md-export-docx` | Phase 1 |
| `C-c m e e` | `henri/md-export-dispatch` | Phase 1（transient） |
| `C-c m t i` | `henri/md-toc-insert` | Phase 2 |
| `C-c m t r` | `henri/md-toc-refresh` | Phase 2 |
| `C-c m t d` | `henri/md-toc-delete` | Phase 2 |
| `C-c m o` | `consult-imenu` / `consult-outline` | Phase 2 |
| `C-c m i s` | `henri/md-insert-screenshot` | Phase 3 |
| `C-c m w` | `henri/md-word-count` | Phase 3 |

已有键位（`C-c m p` 离线预览、`C-c m g` GitHub 预览、`C-c m c` 体检）**保持不变**。

### 6.4 PDF 导出与 LaTeX 模块联动设计

`markdown-export.el` 的 PDF 路径：

1. 调用 `pandoc --pdf-engine=xelatex -o output.pdf input.md`；
2. 通过 `--include-in-header=` 传入由 `latex-themes-simple.el` 生成的 `.sty` 主题文件（或内联 LaTeX header），复用 CJK 字体设置；
3. 字体方案优先读取 `henri-org-cjk-serif-family` / `henri-org-cjk-sans-family`（若已定义），否则回退到 `latex-themes-simple.el` 的默认字体命令；
4. 输出路径受 `henri-md-export-output-dir` 控制。

---

## 7. 分阶段实施计划

### Phase 1 · 导出核心（`markdown-export.el`）

**优先级：P0**

- [ ] 创建 `lisp/writing/markdown/markdown-export.el`
- [ ] 实现 `henri/md-export-pdf`：调用 pandoc + XeLaTeX，复用 LaTeX 模块字体/主题
- [ ] 实现 `henri/md-export-html`：pandoc → 独立 HTML（可含 GFM 样式内联）
- [ ] 实现 `henri/md-export-docx`：pandoc → docx
- [ ] 实现 `henri/md-export-dispatch`：基于 `transient`（已安装）弹出格式选择面板
- [ ] 在 `markdown-base.el` 或本文件内绑定 Phase 1 键位
- [ ] 在 `init-custom.el` 新增 `henri-md-enable-export`、`henri-md-pdf-engine`、`henri-md-export-output-dir`
- [ ] 在 `init-writing.el` 追加 `(when henri-md-enable-export (require 'markdown-export))`
- [ ] 更新 `doctor.el`：增加 `xelatex` / `tectonic` 检查项，在 Markdown PDF 路径给出专属提示

### Phase 2 · 导航与结构（`markdown-nav.el`）

**优先级：P1**

- [ ] 创建 `lisp/writing/markdown/markdown-nav.el`
- [ ] 集成 `markdown-toc`（MELPA）：`henri/md-toc-insert` / `refresh` / `delete`
- [ ] `outline-minor-mode` 默认加入 `markdown-mode-hook`
- [ ] `consult-imenu` 或 `consult-outline` 胶水命令，绑 `C-c m o`
- [ ] 在 `init-custom.el` 新增 `henri-md-enable-toc`
- [ ] 在 `init-writing.el` 追加条件加载
- [ ] `doctor.el` 增加 `markdown-toc` feature 检查

### Phase 3 · 笔记型增强（`markdown-notes.el`）

**优先级：P1**

- [ ] 创建 `lisp/writing/markdown/markdown-notes.el`
- [ ] `henri/md-insert-screenshot`：
  - macOS 探测 `pngpaste`，Linux 探测 `xclip` / `wl-paste`
  - 落地到 `<filename>.assets/<timestamp>.png`
  - 插入 Markdown 相对路径 `![...](<filename>.assets/<timestamp>.png)`
- [ ] 拖拽插入图片：覆写 `markdown-mode` 的 `dnd-protocol-alist`，按同样规则归档到 `.assets/`
- [ ] `henri/md-word-count`：区分中文字符数与英文词数（`count-words` 的增强版）
- [ ] 在 `init-custom.el` 新增 `henri-md-enable-notes`
- [ ] 在 `init-writing.el` 追加条件加载

### Phase 4（可选）· 发布质量（`markdown-lint.el`）

**优先级：P2**

- [ ] 创建 `lisp/writing/markdown/markdown-lint.el`
- [ ] 仅当 `henri-md-enable-lint` 为 `t` 且 `markdownlint` CLI 可执行：注册 `flymake` 后端
- [ ] 默认 `nil`，不阻塞离线启动
- [ ] `doctor.el` 增加 `markdownlint` 可执行文件检查

### Phase 5（可选）· YAML / 模板（`markdown-template.el`）

**优先级：P2**

- [ ] 创建 `lisp/writing/markdown/markdown-template.el`
- [ ] 通过 `tempel`（若已安装）或 Emacs 自带 `skeleton`，提供 Hugo / Jekyll front-matter 模板
- [ ] `henri/md-new-post`：在 `henri-notes-directory` 下指定子目录创建带元信息的新文档
- [ ] `doctor.el` 增加 `tempel` feature 检查（若采用 tempel）

---

## 8. 与现有模块的协作点

| 协作方 | 协作内容 |
|--------|----------|
| `latex-themes-simple.el` | PDF 导出时通过 `--include-in-header` 注入主题 `.sty` 或内联 CJK 字体命令 |
| `init-custom.el` | 新增 `henri-md-*` 系列 `defcustom`，与 `henri-org-enable-*` 同构 |
| `doctor.el` | 新增 `xelatex` / `tectonic` / `markdown-toc` / `markdownlint` 检查项 |
| `init-writing.el` | 由直接 `(require 'markdown-base)` 扩展为条件加载 Phase 1～3（策略 A） |
| `init-managing.el` | `C-c m` which-key 前缀已注册，新增键位自动归入 `"henri/markdown"` 组 |
| `lib-system.el` | 复用 `henri/executable-p` 做 pandoc / xelatex / pngpaste 等可执行文件探测 |

---

## 9. 风险与回退

| 风险 | 缓解 |
|------|------|
| `markdown-toc` 未安装时 `require` 失败 | 用 `featurep` / `locate-library` 前置探测，缺失则 `message` 提示并静默跳过 |
| `transient` 版本过旧不支持某些 API | 使用最基础的 `transient-define-prefix` + `transient-insert-suffix`，Emacs 29 内置 transient 已足够 |
| `pngpaste` 未安装时截图命令报错 | `henri/md-insert-screenshot` 内部先 `(henri/executable-p "pngpaste")`，缺失则 `user-error` 提示安装方式 |
| pandoc → xelatex 中文渲染失败 | 提供 `henri-md-pdf-engine` 切换为 `tectonic`；字体回退逻辑兜底 |
| 键位冲突 | `C-c m e/t/i/o/w` 在现有 `markdown-base.el` 中均未使用；实施前用 `C-h m` 复核 |

---

## 10. 验收标准（各 Phase 冻结条件）

### Phase 1 冻结
- [ ] `henri/md-export-pdf` 在含中文的 `.md` 文件上成功生成正确显示中文的 PDF
- [ ] `henri/md-export-html` 生成独立 `.html`，样式可用
- [ ] `henri/md-export-docx` 生成 `.docx`，Word / Pages 可打开
- [ ] `henri/md-export-dispatch` 弹出 transient 面板，三项均可正常触发
- [ ] `C-c h d` 中 pandoc 与 xelatex/tectonic 状态正确显示

### Phase 2 冻结
- [ ] `C-c m t i` 在光标处插入 `<!-- markdown-toc -->` 围栏及目录
- [ ] `C-c m t r` 刷新已有目录，不重复插入
- [ ] `C-c m t d` 删除当前文档中的 toc 围栏及内容
- [ ] `C-c m o` 调用 consult 类导航，heading 列表正确
- [ ] `outline-minor-mode` 在 markdown-mode 下默认生效，`S-TAB` 可折叠 heading

### Phase 3 冻结
- [ ] `C-c m i s` 成功将截图保存到 `.assets/` 并插入相对路径 Markdown 图片语法
- [ ] 从 Finder 拖拽图片到 Emacs markdown 缓冲区，图片被复制到 `.assets/` 并插入相对路径
- [ ] `henri/md-word-count` 在 minibuffer 中分别显示中文字符数与英文词数

### 整体冻结（本文档归档条件）
- [ ] Phase 1～3 全部验收通过
- [ ] `init-writing.el` 加载结构稳定，`emacs --batch` 启动无报错
- [ ] `henri-md-enable-export/toc/notes/lint` 各自设为 `nil` 后，对应功能不加载且不影响 markdown-mode 基本编辑
- [ ] 本文档更新为 `状态: done` 后迁入 `docs/legacy/`

---

*创建日期: 2026-05-04*
