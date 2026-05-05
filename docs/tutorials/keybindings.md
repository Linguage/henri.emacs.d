# 快捷键速查（本配置显式绑定）

> 仅收录本仓库显式绑定的主力入口。Emacs / Org / Magit 包自身默认键不展开；具体实现以源文件为准。

## 1. 全局前缀

| 前缀 | 领域 | 常用入口 |
|------|------|----------|
| `C-c c` | capture | `org-capture` |
| `C-c a` | agenda | `org-agenda` |
| `C-c A` | academic | `n/r/j/p/c/P/N/b/d/i` |
| `C-c f` | file / find | `n` notes、`p` project tree |
| `C-c b` | buffer / bookmark | 预留治理前缀 |
| `C-c w` | window / terminal | `l` layout、`e` eshell、`v` vterm、`E` eshell legacy |
| `C-c g` | git | `g/d/f/b/l` Magit、`n/p/r` hunk、`m ...` smerge |
| `C-c s` | search | `C-s` swiper；前缀预留 |
| `C-c t` | toggle / theme | `t` load theme、`b` big font、`l` line numbers |
| `C-c h` | help / doctor | `d` doctor、`k` keybindings |
| `C-c o` | Org ops | `a` dashboard、`i/t/p` agenda files、`v` startup folded |
| `C-c n` | Org-roam | `f/i/b/c/g/j/t/y/m/d/x/?` |
| `C-c j` | Journal | `s` search、`d` view by date |
| `C-c r` | Rime | 由 Rime 模块提供 |
| `C-c l` | store link | 释放给 Org link 语义 |
| `C-c m` | mode-local | Markdown / Org / PDF / LaTeX 当前缓冲区能力 |
| `C-c d` | debug / run | `r` quickrun、`p/b/o/s/n` realgud |
| `C-c e` | Eglot | `f` format |

## 2. 基础管理

| 快捷键 | 命令 | 位置 |
|--------|------|------|
| `M-x` | `counsel-M-x` | `lisp/init-managing.el` |
| `C-s` | `swiper` | `lisp/init-managing.el` |
| `C-c f n` | `henri/find-file-in-notes` | `lisp/init-managing.el` |
| `C-c f p` | `henri/neotree-project-dir` | `lisp/init-managing.el` |
| `<f8>` | `neotree-toggle` | `lisp/init-managing.el` |
| `C-c w l` | `henri/setup-window-layout` | `lisp/init-managing.el` |
| `C-c w e` | `eshell` | `lisp/init-managing.el` |
| `C-c w v` | `henri/vterm` | `lisp/init-managing.el` |
| `C-c w E` | `eshell` | `lisp/init-managing.el` |
| `C-c h k` | `henri/open-keybindings-doc` | `lisp/init-keys.el` |
| `C-c h d` | `henri/doctor` | `lisp/ops/doctor.el` |

## 3. Git

| 快捷键 | 命令 | 位置 |
|--------|------|------|
| `C-x g` | `magit-status` | `lisp/init-managing.el` |
| `C-c g g` | `magit-status` | `lisp/init-managing.el` |
| `C-c g d` | `magit-dispatch` | `lisp/init-managing.el` |
| `C-c g f` | `magit-file-dispatch` | `lisp/init-managing.el` |
| `C-c g b` | `magit-blame` | `lisp/init-managing.el` |
| `C-c g l` | `magit-log-buffer-file` | `lisp/init-managing.el` |
| `C-c g n` | `diff-hl-next-hunk` | `lisp/init-managing.el` |
| `C-c g p` | `diff-hl-previous-hunk` | `lisp/init-managing.el` |
| `C-c g r` | `diff-hl-revert-hunk` | `lisp/init-managing.el` |
| `C-c g m n/p` | `smerge-next` / `smerge-prev` | `lisp/init-managing.el` |
| `C-c g m u/l/b/a` | keep upper/lower/base/all | `lisp/init-managing.el` |

`C-c ^ ...` 在 `henri-keybindings-enable-legacy-aliases` 为 non-nil 时仍作为 smerge 过渡别名保留。

## 4. Toggle / Visual

| 快捷键 | 命令 | 位置 |
|--------|------|------|
| `C-c t t` | `henri/load-theme-interactively` | `lisp/init-keys.el` |
| `C-c t b` | `henri-big-font-mode` | `lisp/init-keys.el` |
| `C-c t l` | `henri/toggle-line-numbers` | `lisp/init-keys.el` |
| `C-=` | `henri/font-size-adjust` | `lisp/visual/visual-components.el` |
| `C--` | decrease font size | `lisp/visual/visual-components.el` |
| `C-c F r` | `henri/font-size-reset` | `lisp/visual/visual-components.el` |
| `C-c F b` | `henri-big-font-mode` | `lisp/visual/visual-components.el` |

## 5. Programming

| 快捷键 | 命令 | 位置 |
|--------|------|------|
| `C-c e f` | `eglot-format` | `lisp/init-programming.el` |
| `C-c e f` | `my/format-lisp-buffer` in `lisp-mode` | `lisp/programming_languages/init-lisp.el` |
| `C-' C-'` | `imenu-list-smart-toggle` | `lisp/init-programming.el` |
| `C-c d r` | `quickrun` | `lisp/init-programming.el` |
| `C-c d p` | `realgud:pdb` | `lisp/init-programming.el` |
| `C-c d b` | `realgud:cmd-break` | `lisp/init-programming.el` |
| `C-c d o` | `realgud:cmd-step-over` | `lisp/init-programming.el` |
| `C-c d s` | `realgud:cmd-step` | `lisp/init-programming.el` |
| `C-c d n` | `realgud:cmd-next` | `lisp/init-programming.el` |

F5/F6/F9/F10/F11/F12 不再作为本配置的主力调试入口。

## 6. Writing / Org

| 快捷键 | 命令 | 位置 |
|--------|------|------|
| `C-c c` | `org-capture` | `lisp/writing/org/org-journal.el` |
| `C-c a` | `org-agenda` | `lisp/writing/org/org-journal.el` |
| `C-c o a` | `henri/org-agenda-dashboard` | `lisp/writing/org/org-journal.el` |
| `C-c o i/t/p` | open inbox/tasks/projects | `lisp/writing/org/org-journal.el` |
| `C-c o v` | `henri/cycle-org-startup-folded` | `lisp/writing/org/org-base.el` |
| `C-c j s` | `henri/search-journal` | `lisp/writing/org/org-journal.el` |
| `C-c j d` | `henri/view-diary-by-date` | `lisp/writing/org/org-journal.el` |

### 6.1 Org Mode Local

| 快捷键 | 命令 | 位置 |
|--------|------|------|
| `C-c m v b` | `henri/toggle-org-bullets` | `lisp/writing/org/org-base.el` |
| `C-c m v s` | `henri/org-show-all` | `lisp/writing/org/org-base.el` |
| `C-c m v o` | `henri/org-overview` | `lisp/writing/org/org-base.el` |
| `C-c m v c` | `henri/org-content` | `lisp/writing/org/org-base.el` |
| `C-c m h t/d/e/w/c/r/i/k` | Org HTML theme/export commands | `lisp/writing/org/org-html.el` |
| `C-c m h s/?/1/2/0` | Org HTML shortcut themes | `lisp/writing/org/org-html.el` |
| `C-c m l t/p/q/d/r` | Org LaTeX/PDF commands | `lisp/writing/org/org-latex.el` |

`C-c C-b` 不再被本配置覆盖，回到 Org 默认的 `org-tree-to-indirect-buffer`。

### 6.2 Academic

| 快捷键 | 命令 | 位置 |
|--------|------|------|
| `C-c A n` | `org-academic-create-idea-note` | `lisp/writing/org/org-academic.el` |
| `C-c A r` | `org-academic-create-reading-note` | `lisp/writing/org/org-academic.el` |
| `C-c A j` | `org-academic-create-project-note` | `lisp/writing/org/org-academic.el` |
| `C-c A p` | `org-academic-create-paper` | `lisp/writing/org/org-academic.el` |
| `C-c A c` | `org-academic-create-conference-abstract` | `lisp/writing/org/org-academic.el` |
| `C-c A P` | `org-academic-quick-paper` | `lisp/writing/org/org-academic.el` |
| `C-c A N` | `org-academic-quick-note` | `lisp/writing/org/org-academic.el` |
| `C-c A b` | `org-academic-setup-bibliography` | `lisp/writing/org/org-academic.el` |
| `C-c A d` | `org-academic-dashboard` | `lisp/writing/org/org-academic.el` |
| `C-c A i` | `org-academic-insert-citation` | `lisp/writing/org/org-academic.el` |

`C-c a` 只用于 agenda；Academic 不保留 `C-c a ...` 旧别名。

### 6.3 Org-roam

| 快捷键 | 命令 | 位置 |
|--------|------|------|
| `C-c n f` | `org-roam-node-find` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n i` | `org-roam-node-insert` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n b` | `org-roam-buffer-toggle` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n c` | `org-roam-capture` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n g` | `org-roam-graph` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n j/t/y/m` | dailies capture/today/yesterday/tomorrow | `lisp/writing/org/org-roam-henri.el` |
| `C-c n d` | `henri-org-roam-open-directory` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n x` | `henri-org-roam-open-inbox` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n ?` | `henri-org-roam-show-template-keys` | `lisp/writing/org/org-roam-henri.el` |

Org-roam capture template key:

| Key | 模板 | 落点 |
|-----|------|------|
| `n` | note / 概念笔记 | `Roam/notes/${slug}.org` |
| `i` | inbox / 临时收集 | `Roam/inbox/${slug}.org` |
| `r` | reference / 资料笔记 | `Roam/references/${slug}.org` |
| `p` | project / 项目笔记 | `Roam/projects/${slug}.org` |
| `m` | map / 索引地图 | `Roam/maps/${slug}.org` |
| `e` | person / 人物笔记 | `Roam/people/${slug}.org` |
| `d` | daily / 每日笔记 | `Roam/daily/YYYY-MM-DD.org`，通过 `C-c n j` 使用 |

若安装 `consult-org-roam`，还会启用 `C-c n s/r/l`。

## 7. Markdown / PDF / LaTeX

| 快捷键 | 命令 | 位置 |
|--------|------|------|
| `C-c m p/v/l/g/c/s` | Markdown preview/live/GitHub/check/theme | `lisp/writing/markdown/markdown-base.el` |
| `C-c m e p/h/d/e` | Markdown export PDF/HTML/docx/dispatch | `lisp/writing/markdown/markdown-export.el` |
| `C-c m t i/r/d` | Markdown TOC insert/refresh/delete | `lisp/writing/markdown/markdown-nav.el` |
| `C-c m o` | Markdown outline | `lisp/writing/markdown/markdown-nav.el` |
| `C-c m i s` | insert screenshot | `lisp/writing/markdown/markdown-notes.el` |
| `C-c m w` | word count | `lisp/writing/markdown/markdown-notes.el` |
| `C-c m T i/n` | front matter / new post | `lisp/writing/markdown/markdown-template.el` |
| `C-c m d c` | `henri/pdf-check-deps` | `lisp/writing/pdf/pdf-base.el` |
| `C-c m d n` | `pdf-view-midnight-mode` | `lisp/writing/pdf/pdf-base.el` |
| `C-c m i/c/e/p/l` | LaTeX theme insert/create/edit/preview/list | `lisp/writing/LaTeX/latex-themes.el` |

## 8. Octave

| 快捷键 | 命令 | 位置 |
|--------|------|------|
| `C-c C-i/r/b/f/l/h/d/k` | Octave REPL/send/help/process commands | `lisp/programming_languages/init-octave.el` |
| `M-.` / `M-,` | definition / pop tag mark | `lisp/programming_languages/init-octave.el` |
| `C-c d b` | `octave-set-breakpoint` | `lisp/programming_languages/init-octave.el` |
| `C-c d B` | `octave-clear-breakpoints` | `lisp/programming_languages/init-octave.el` |
| `C-c d s` | `octave-step-debug` | `lisp/programming_languages/init-octave.el` |
| `C-c d c` | `octave-continue-debug` | `lisp/programming_languages/init-octave.el` |
| `C-c d r` | `octave-debug-file` | `lisp/programming_languages/init-octave.el` |
| `C-c C-p s/c` | project setup / clear workspace | `lisp/programming_languages/init-octave.el` |

## 9. Legacy Aliases

`henri-keybindings-enable-legacy-aliases` 默认为 `t`，目前只保留不破坏治理边界的过渡入口，例如 `C-c ^ ...` 到 smerge。以下旧键已立即释放，不保留别名：

| 旧键 | 新键 / 归属 |
|------|-------------|
| `C-c a ...` Academic | `C-c A ...` |
| `C-c h ...` Org HTML | `C-c m h ...` |
| `C-c l ...` Org LaTeX | `C-c m l ...`；`C-c l` 释放给 Org link |
| `C-c v ...` Org view | `C-c m v ...` |
| `C-c C-b` Org bullets | `C-c m v b` |
| F5/F6/F9/F10/F11/F12 | `C-c d ...` |

## 10. 按键不生效时

- 用 `C-h k` 后按目标快捷键，查看实际命令与 keymap。
- 查看 `C-h v henri-keybindings-enable-legacy-aliases`，确认是否启用了过渡别名。
- mode-local 键只在对应 major mode 中生效；例如 Org 的 `C-c m h` 不在 Markdown buffer 中出现。
