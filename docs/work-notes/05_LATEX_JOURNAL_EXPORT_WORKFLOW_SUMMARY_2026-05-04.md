---
title: "LaTeX 写作工作台试装、Journal PDF 主题试验与导出链路回退"
date: "2026-05-04"
created: "2026-05-04 18:07:14 +0800"
category: "worknotes"
tags:
  [
    "emacs",
    "elisp",
    "latex",
    "auctex",
    "org-mode",
    "journal",
    "pdf",
    "citar",
    "workflow",
    "session-summary",
  ]
---

# LaTeX 写作工作台试装、Journal PDF 主题试验与导出链路回退

本轮工作围绕 Emacs 写作栈中的 LaTeX / Org / Journal / PDF 交界面展开：先把 `.tex` 编辑与 Org PDF 导出升级为更完整的 `latexmk + XeLaTeX` 工作流，再尝试将 vendored `LaTeX-Diary` 主题接入 Journal PDF；随后在真实 journal 文件上暴露出 capture、主题耦合、目录与 PDF outline 等问题，最终又将 Journal 默认模板回退为常规 `ctex + geometry` 路线，并把实验主题保留为后续资源。本文记录这个阶段为何启动、做了什么、验证到什么程度，以及哪些问题只是暂时止血而非彻底定型。

## 1. 背景与动机

- 原有配置已经能完成基础 Org 导出，但 `.tex` 编辑、SyncTeX、PDF 查看、中文 XeLaTeX、文献工作流之间仍是“能用但不成体系”的状态。
- 用户希望把 Emacs 的 LaTeX 写作体验从“Org 导出可用”推进到“完整论文工作台”：AUCTeX、`latexmk`、`pdf-tools`、`citar`、`org-roam`、Journal PDF 导出等模块需要进入同一条链路。
- Journal PDF 进一步提出了视觉诉求：希望尝试将第三方日记本样式主题接入 Org Journal 导出，而不是只停留在常规学术 PDF 版式。
- 真实使用中很快暴露出两个现实约束：
  1. BasicTeX 缺少大量主题依赖，实验主题不是“开箱即用”；
  2. Org HTML setup、Org capture、Journal 月文件结构、LaTeX class 与 PDF outline 会彼此干涉，稍有耦合就会反噬稳定性。

## 2. 阶段目标（本系列会话实际覆盖）

1. 让 `.tex` 与 Org PDF 导出统一走 `latexmk + XeLaTeX`，并为 BasicTeX 提供 `/Library/TeX/texbin` 的兜底。
2. 在 Emacs 内补齐 `pdf-tools`、字体/工具链诊断、以及更稳定的 PDF 打开路径。
3. 为学术写作接入 `citar` / `org-cite` / `org-roam` 方向的基础配置，使 BibTeX、PDF 库与文献笔记目录有明确落点。
4. 试验性接入 `LaTeX-Diary` 作为 Journal PDF 主题，包括 vendoring 上游资源、定制 `henri-diary.cls`、打通 Org 导出。
5. 在真实 `journal-2026-05.org` 上验证 capture、PDF 导出、目录与阅读器侧边栏 outline，并在主题不稳定时及时回退到常规模板。

## 3. 主要改动（按工作主题）

### 3.1 LaTeX 工作台与工具链收敛

- `.tex` 编辑配置集中在 `lisp/writing/LaTeX/`，AUCTeX 默认命令统一为 `latexmk + XeLaTeX`。
- 为 macOS BasicTeX/MacTeX 增加 `/Library/TeX/texbin` 的 PATH / `exec-path` 兜底，降低 `xelatex`、`latexmk`、`kpsewhich` 在 GUI Emacs 内找不到的概率。
- Org PDF 导出链路同步改到 `latexmk`，并最终收敛为 `latexmk -g -xelatex -synctex=1 -interaction=nonstopmode -outdir=%o %f`：
  这不是“手写跑两次 xelatex”，而是让 `latexmk` 在 Org 导出内部自动重跑到 `.aux/.toc` 稳定。
- `pdf-tools` 的主体配置迁出到单独的 `lisp/writing/pdf/pdf-base.el`，而 `org-latex.el` 只保留导出后打开 PDF 的集成钩子，避免 PDF 查看配置和 Org 导出逻辑继续缠在一起。

### 3.2 学术写作与文献工作流补齐

- `org-academic.el` 增加了 bibliography、PDF library、文献笔记、`org-roam` 研究目录的可配置路径。
- `org-cite` 被接到 `citar`，插入、跟随与激活处理器统一指向 `citar`，`org-academic-insert-citation` 在可用时走选择器，缺失时再回退手动输入。
- 这一步的意义不是“已经把论文流完全做完”，而是给后续学术写作固定了最基本的数据模型：BibTeX 文件、PDF 库、文献笔记与 Org-roam 目录已经有约定，不再散落在多个局部配置里。

### 3.3 Journal PDF 主题试验：接入、踩坑与回退

- 仓库中 vendored 了上游 `LaTeX-Diary` 资源到 `lisp/writing/LaTeX/themes/latex-diary/`，并增加 `SOURCE.md` 记录来源与 commit。
- 基于上游 `diary.cls` 派生出 `henri-diary.cls`，尝试为 Org Journal 提供适配层：包括 Org 标题树映射、字体路径、checkbox 符号、日头样式等。
- 真实导出过程中先后遇到几类问题：
  - BasicTeX 缺失 `tikzpagenodes`、`xargs`、`xstring`、`ifoddpage` 等依赖；
  - 导出目录看不到 vendored `icofont.ttf`；
  - HTML `SETUPFILE` 与 PDF 导出选项相互覆盖；
  - Org 深层标题、checkbox 与 TOC / outline 的生成方式和第三方 class 的结构假设不一致。
- 针对这些问题，一度加入过 user-mode `tlmgr` 指引、字体 symlink、Org export filter、metadata 注入等修补层，并成功让实验主题在真实 journal 上产出 PDF。
- 但在继续打磨过程中，暴露出主题样式、capture 行为、Org setupfile、导出后目录/书签之间存在明显耦合；因此这一轮没有继续把 `LaTeX-Diary` 硬推成默认方案，而是明确回退：
  `#+LATEX_CLASS: journal` 重新语义化为“常规 Journal PDF 模板”，实验主题保留但不再占据默认导出链。

### 3.4 Journal capture 与 PDF 导出链的稳定化

- `C-c c d` 的 diary capture 曾因模板最前方插入 metadata prompt 函数而失效；那段逻辑在 capture 展开阶段调用 `org-back-to-heading`，很容易与 target 进入时机冲突。
- 本轮已把该 prompt / metadata / export hook 整体撤回，恢复 diary/work/study 三类 capture 的月文件共享结构，其中 `d` 模板再次能直接插入 `:journal:diary:` 条目。
- Journal 默认 PDF 模板回退为 `ctexart + geometry + hyperref` 的常规组合，并保留 `toc:t`、`num:nil`、`H:5` 这组三项 Org 侧约束。
- 目录问题后续又暴露出一个更细的事实：`num:nil` 会让 Org 生成 `\section*` 一类星号标题，LaTeX 不会把它们写进 TOC 和 PDF outline。
  最终修法不是“再手动编一次”，而是：
  - Org 侧允许 Journal 生成普通 `\section` / `\subsection`；
  - LaTeX 侧通过 `\setcounter{secnumdepth}{0}` 隐藏打印编号；
  - 通过 `\setcounter{tocdepth}{5}` 与 `bookmark` 包生成正文目录与 PDF 侧边栏 outline。

## 4. 验证与已确认结果

- 多次执行 `emacs --batch -Q --eval '(setq user-emacs-directory default-directory)' --load init.el --eval '(message "loaded")'`，用于确认配置链无加载期错误。
- 对 `org-latex.el`、`org-journal.el`、`latex-minimal.el` 做过 batch byte-compile，确认没有明显加载顺序或未定义符号问题。
- 多次执行 `git diff --check`，确认相关改动没有空白或 patch 级格式错误。
- 运行过 `org-latex-diagnose-fonts`，确认 `xelatex`、`latexmk`、`kpsewhich`、`ctex.sty` 可见；实验 `LaTeX-Diary` 依赖也被明确区分为“存在，但不再是 Journal 默认依赖”。
- 对真实 `journal-2026-05.org` 多次执行 `org-latex-export-to-pdf`，分别验证过：
  - PDF 是否成功生成；
  - `.tex` 是否包含 `\tableofcontents`；
  - 标题是否使用普通 section 命令而不是星号标题；
  - PDF 文本里是否存在目录项与页码；
  - PDF outline 是否存在（最终用 PyMuPDF `get_toc()` 确认真实 PDF 中已有 21 条目录树）。
- 对 capture 也做过临时目录烟测：`org-capture-string` 使用 diary 模板能成功写入新的月度 journal 文件，不再触发地点/天气 prompt。

## 5. 文档与配置说明更新

- `README.md` 与 `c.md` 都补上了 BasicTeX / `latexmk` / `kpsewhich` 依赖说明，以及“实验 LaTeX-Diary 主题不再是 Journal 默认依赖”的状态说明。
- Journal PDF 的当前约定已经切回常规模板，而不是继续把实验主题描述成稳定功能。
- 写作层模块说明也补充了 `citar` 与学术目录配置的路径语义，便于之后继续扩展论文工作流。

## 6. 当前状态与未完成项

- 当前可视为已经稳定的部分：
  - `.tex` 与 Org PDF 导出统一走 `latexmk + XeLaTeX`；
  - `C-c c d` 的 diary capture 恢复可用；
  - `#+LATEX_CLASS: journal` 默认导出常规中文 PDF；
  - Journal PDF 的正文目录与 PDF outline 均已生成。
- 仍然保留但不应当被误认为“稳定完成”的部分：
  - `lisp/writing/LaTeX/themes/latex-diary/` 中的 vendored 实验主题；
  - `henri-diary.cls` 这类适配层；
  - BasicTeX user-mode 主题依赖安装指引。
- 后续若要继续推动日记本视觉主题，最好将其做成**显式可选主题**，而不是再次接回 `journal` 默认 class；否则 Org HTML setup、Journal capture、LaTeX class 与 PDF outline 之间还会重新缠在一起。

## 7. 结论边界

- **已确认**：本轮不是单纯“把 LaTeX 编译次数调对”，而是把 Org → LaTeX → PDF 的整条导出语义重新梳理了一遍，尤其是 TOC / outline 与无编号标题之间的关系已经被验证清楚。
- **未在本文中声称**：实验 `LaTeX-Diary` 已经成熟到可作为默认日记模板，也未声称学术写作流（`citar` / `org-roam` / PDF 库）已经在所有机器和所有图形界面路径上完整交互实测。当前更准确的表述是：基础工作流已被搭起来，Journal 主题试验已知会干扰稳定性，因此本阶段以回退和解耦收尾。
