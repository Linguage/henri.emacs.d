% journal.cls --- Org Journal PDF 导出文档类
% 从 org-latex.el 解耦而来，由 org-journal.el 加载注册。
%
% 用法：Org 文件头写 #+LATEX_CLASS: journal
% 本类基于 ctexart，适用于 XeLaTeX 编译。
% B5 页面 + 京華老宋体，与 HTML Journal 主题保持一致。

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{journal}[2026/05/04 Henri Journal class]

% 继承 ctexart（不设 fontset，由下方 fontspec 手动指定）
\LoadClass[UTF8,11pt]{ctexart}

% ---- 数学与图形 ----
\RequirePackage{amsmath}
\RequirePackage{amssymb}
\RequirePackage{graphicx}

% ---- 超链接与书签 ----
\RequirePackage{hyperref}
\RequirePackage[open=true]{bookmark}

% ---- 字体：京華老宋体（与 HTML Journal 主题一致） ----
\RequirePackage{fontspec}
\RequirePackage{xeCJK}

\setCJKmainfont{KingHwa_OldSong}[
  BoldFont       = KingHwa_OldSong,
  ItalicFont     = KingHwa_OldSong,
  AutoFakeBold   = 2,
  AutoFakeSlant  = 0.15
]
\setCJKsansfont{KingHwa_OldSong}
\setCJKmonofont{KingHwa_OldSong}

% ---- 页面布局：B5 ----
\RequirePackage[paperwidth=176mm,paperheight=250mm,
                top=2cm,bottom=2cm,left=2cm,right=2cm]{geometry}
\linespread{1.15}
\setlength{\parskip}{0.4em}

% ---- 章节编号控制 ----
% secnumdepth=0 隐藏打印编号，但书签仍由 tocdepth 控制
\setcounter{secnumdepth}{0}
\setcounter{tocdepth}{3}

% ---- PDF 书签（侧边栏大纲） ----
\hypersetup{
  bookmarks=true,
  bookmarksopen=true,
  bookmarksnumbered=false,
  bookmarkstype=toc,
  pdfcreator={Henri Journal},
  colorlinks=true,
  linkcolor=blue!60!black,
  urlcolor=blue!80!black
}

\endinput
