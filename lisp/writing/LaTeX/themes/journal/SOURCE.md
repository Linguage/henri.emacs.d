# Journal LaTeX 模板

## 来源

2026-05-04 从 `lisp/writing/org/org-latex.el` 中 `journal` 文档类定义解耦而来。

## 用途

为 Org Journal（`henri-journal-*.org`）月度日志文件提供 PDF 导出模板。
Org 文件头使用 `#+LATEX_CLASS: journal` 即可激活。

## 设计

- **纸张**：B5（176mm x 250mm）
- **字体**：京華老宋体（KingHwa_OldSong），与 HTML Journal 主题保持一致
- **页面边距**：上下左右各 2cm

## 依赖

- 编译器：XeLaTeX（`org-latex-compiler` 设为 `xelatex`）
- 系统字体：KingHwa_OldSong（京華老宋体）
- TeX 包：ctex、fontspec、xeCJK、hyperref、bookmark、geometry、amsmath、graphicx

## 文件

| 文件 | 说明 |
|------|------|
| `journal.cls` | LaTeX 文档类，基于 ctexart |

## 注册方式

由 `org-journal.el` 在 `with-eval-after-load 'ox-latex` 中通过
`add-to-list 'org-latex-classes` 注册。本目录不由 `org-latex.el` 管理。

## 书签说明

PDF 侧边栏书签由以下配置保证：
- `bookmark` 包（`open=true`）自动生成书签
- `hypersetup{bookmarks=true, bookmarksopen=true}` 启用并展开书签
- `tocdepth=3` 控制书签到 subsection 层级
- `secnumdepth=0` 仅隐藏打印编号，不影响书签生成
