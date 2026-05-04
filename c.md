# henri.emacs.d c.md

## 主题与导出资源

- `lisp/writing/org/org-html-themes/`：Org HTML 主题（默认与 `install-themes.sh` 一致；若缺失请运行 `lisp/writing/org/install-themes.sh`）
- `lisp/writing/LaTeX/`：专门的 LaTeX 工作台配置；AUCTeX 默认走 `latexmk + XeLaTeX`，macOS BasicTeX/MacTeX 兜底目录为 `/Library/TeX/texbin`。
- `lisp/writing/LaTeX/themes/latex-diary/`：vendored LaTeX-Diary 日记本 PDF 实验主题；当前不作为 Journal 默认导出模板。

## 个人路径（`henri-paths`）

在 `M-x customize-group RET henri-paths` 中可调：

- `henri-notes-directory` — 笔记/Journal 等根目录  
- `henri-projects-directory` — Projectile 搜索根  
- `henri-leetcode-directory` — LeetCode 存放目录  
- `henri-conda-home` / `henri-conda-envs-directory` / `henri-conda-default-env` — Conda 与默认环境  
- `henri-org-html-themes-directory` — 本地 org-html-themes（默认在配置仓库内）

## LaTeX / 学术写作

- **检查**：`M-x org-latex-diagnose-fonts` 检查 `xelatex`、`latexmk`、`kpsewhich`、`ctex.sty` 与实验 LaTeX-Diary 主题资源。
- **编译**：`.tex` 文件使用 AUCTeX 的 `LatexMk XeLaTeX`；Org PDF 导出使用 `latexmk -xelatex`。
- **Journal PDF**：`#+LATEX_CLASS: journal` 使用常规 `ctex + geometry` 模板，默认保留目录、不编号、导出到 5 级标题。
- **Journal 依赖**：若 BasicTeX 缺少主题包，可按诊断提示运行 `tlmgr init-usertree`，然后 `tlmgr --usermode install tikzpagenodes ifoddpage xargs xstring`。
- **文献工作流**：`C-c a b` 初始化 BibTeX、PDF 与文献笔记目录；`C-c a i` 优先通过 `citar` 插入引用。
- **文献笔记真源**：正式文献阅读卡与 Citar notes 默认落在 `org-academic-literature-notes-dir`（默认 `Academic/Reading/`）；`Roam/references/` 只放通用资料节点。
- **学术可调路径**：`M-x customize-group RET org-academic` 中维护 `org-academic-bibliography-file`、`org-academic-library-dir`、`org-academic-literature-notes-dir`。
- **Org-roam 可调路径**：`M-x customize-group RET henri-writing` 中维护 `henri-org-roam-directory`；`henri-org-roam-enable-citar-integration` 默认为 `nil`，只有主动希望 Citar 创建 Roam reference 节点时才开启。

## 常用入口

- **终端**：`C-c w e` / `C-c w v` 打开主力 `vterm`；`C-c w E` 保留 `eshell` 备用入口。
- **文件与项目树**：`C-c f n` 查找 Notes 文件；`C-c f p` 打开当前项目的 NeoTree；`<f8>` 切换 NeoTree。
- **自检与 HTML**：全局 `C-c h d` 是 `henri/doctor`；在 Org buffer 中，`C-c h d` 被局部绑定为应用默认 HTML 主题。

## 运行时目录（`henri-runtime`）

`[lisp/ops/paths.el](lisp/ops/paths.el)` 在启动时创建常用本机目录，并定义：

- `henri-var-directory` → 默认 `var/`，其下 `backups/`、`autosave/` 由 `[lisp/ops/backup.el](lisp/ops/backup.el)` 使用  
- `henri-local-cache-directory` / `henri-local-etc-directory` → `.local/cache`、`.local/etc`  
- `henri-tree-sitter-directory`、`henri-rime-directory`

可调：`M-x customize-group RET henri-runtime`。

## Git（Magit / diff-hl / 合并）

- **Magit**：`C-x g` 或 `C-c g g` 打开状态；`C-c g d` 总菜单；`C-c g f` 当前文件；`C-c g b` blame；`C-c g l` 当前文件 log。可通过 `henri-enable-magit` 关闭 Magit 块（仍保留 `diff-hl`）。
- **diff-hl**：有改动的行在 fringe（图形）或 margin（终端）提示；`C-c v n` / `C-c v p` 跳转 hunk，`C-c v r` 还原当前 hunk。
- **合并冲突**：打开含 `<<<<<<<` 的文件会自动 `smerge-mode`；冲突块内 `C-c ^ n/p` 导航，`C-c ^ u/l/b/a` 取舍。

## Markdown 预览

- **依赖**：`pandoc`（离线，`C-c m p` 优先走 EWW）；可选 `grip`（GitHub 渲染，`C-c m g` / `C-c C-g`，需 `henri-enable-grip` 为 t）。
- **检查**：在 `markdown-mode` 下 `C-c m c` 或 `M-x henri/markdown-check-preview-deps`。

## `custom.el` 与 Rime

- `**custom.el`**：位于配置根（`locate-user-emacs-file`），自动生成；是否提交 Git 由你决定，默认策略是不跟踪。  
- `**rime/`**：启用 `henri-enable-rime` 时使用 `henri-rime-directory`（默认配置根下 `rime/`）。

## 常用运行命令

```bash
# 使用本目录作为配置目录启动 Emacs（示例）
emacs --init-directory="/path/to/henri.emacs.d"

# 或设置环境变量后启动（依个人 shell 配置而定）
# export EMACSLOADPATH=...
```

## 结果路径

- `elpa/`：通过 `package.el` 安装的扩展包
- `.local/`、`var/`：缓存、备份、自动保存等（以仓库 `.gitignore` 为准）
- `tree-sitter/`：Tree-sitter 相关文件

## 常看目录

- `early-init.el`、`init.el`：配置入口
- `lisp/`：模块化 Elisp
- `docs/specs/`：蓝图、架构、路线图
- 根目录 `README.md`：功能概览、系统要求与模块索引
