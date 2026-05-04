# 快捷键速查（本配置显式绑定）

> 仅收录**本仓库 Elisp 中显式写出**的 `:bind` / `define-key` / `global-set-key` / `local-set-key`。  
> **不收录**：Emacs 内置键、Org / Magit 等包的默认键、以及未绑定的命令。  
> 行号以仓库内文件为准，便于对照；若你本地改过配置，以实际文件为准。

---

## 1. 基础管理（`lisp/init-managing.el`）

### 1.1 搜索与补全


| 快捷键       | 命令                         | 位置                         |
| --------- | -------------------------- | -------------------------- |
| `M-x`     | `counsel-M-x`              | `lisp/init-managing.el:55` |
| `C-c f n` | `henri/find-file-in-notes` | `lisp/init-managing.el:56` |
| `C-s`     | `swiper`                   | `lisp/init-managing.el:62` |


### 1.2 文件树（NeoTree）


| 快捷键       | 命令                          | 位置                         |
| --------- | --------------------------- | -------------------------- |
| `<f8>`    | `neotree-toggle`            | `lisp/init-managing.el:72` |
| `C-c f p` | `henri/neotree-project-dir` | `lisp/init-managing.el` |


### 1.3 布局与 Shell（全局）


| 快捷键       | 命令                          | 位置                          |
| --------- | --------------------------- | --------------------------- |
| `C-c w l` | `henri/setup-window-layout` | `lisp/init-managing.el` |
| `C-c w e` | `henri/vterm`              | `lisp/init-managing.el` |
| `C-c w v` | `henri/vterm`              | `lisp/init-managing.el` |
| `C-c w E` | `eshell`                   | `lisp/init-managing.el` |


### 1.4 自检（全局）

| 快捷键       | 命令             | 位置                 |
| --------- | -------------- | ------------------ |
| `C-c h d` | `henri/doctor` | `lisp/ops/doctor.el` |

在 Org buffer 中，`C-c h d` 会被 `org-mode-map` 局部绑定覆盖为 `henri/org-html-apply-default-theme`。


### 1.5 Git（Magit）

仅在 `**henri-enable-magit` 非 nil** 时安装绑定。


| 快捷键       | 命令                      | 位置                          |
| --------- | ----------------------- | --------------------------- |
| `C-x g`   | `magit-status`          | `lisp/init-managing.el:206` |
| `C-c g g` | `magit-status`          | `lisp/init-managing.el:207` |
| `C-c g d` | `magit-dispatch`        | `lisp/init-managing.el:208` |
| `C-c g f` | `magit-file-dispatch`   | `lisp/init-managing.el:209` |
| `C-c g b` | `magit-blame`           | `lisp/init-managing.el:210` |
| `C-c g l` | `magit-log-buffer-file` | `lisp/init-managing.el:211` |


### 1.6 diff-hl（在 `diff-hl-mode` 作用 buffer）


| 快捷键       | 命令                      | 位置                          |
| --------- | ----------------------- | --------------------------- |
| `C-c v p` | `diff-hl-previous-hunk` | `lisp/init-managing.el:222` |
| `C-c v n` | `diff-hl-next-hunk`     | `lisp/init-managing.el:223` |
| `C-c v r` | `diff-hl-revert-hunk`   | `lisp/init-managing.el:224` |


### 1.7 smerge（合并冲突：`smerge-mode` 激活时）


| 快捷键       | 命令                  | 位置                          |
| --------- | ------------------- | --------------------------- |
| `C-c ^ n` | `smerge-next`       | `lisp/init-managing.el:247` |
| `C-c ^ p` | `smerge-prev`       | `lisp/init-managing.el:248` |
| `C-c ^ u` | `smerge-keep-upper` | `lisp/init-managing.el:249` |
| `C-c ^ l` | `smerge-keep-lower` | `lisp/init-managing.el:250` |
| `C-c ^ b` | `smerge-keep-base`  | `lisp/init-managing.el:251` |
| `C-c ^ a` | `smerge-keep-all`   | `lisp/init-managing.el:252` |


---

## 2. 写作 · Markdown（`lisp/init-writing.el`）

均在 `**markdown-mode`** 下生效。


| 快捷键         | 命令                                    | 位置                         |
| ----------- | ------------------------------------- | -------------------------- |
| `C-c C-v`   | `markdown-preview`                    | `lisp/init-writing.el:89`  |
| `C-c C-c p` | `markdown-preview-mode`               | `lisp/init-writing.el:90`  |
| `C-c m p`   | `henri/markdown-preview-offline`      | `lisp/init-writing.el:91`  |
| `C-c m g`   | `henri/markdown-preview-github-style` | `lisp/init-writing.el:92`  |
| `C-c m c`   | `henri/markdown-check-preview-deps`   | `lisp/init-writing.el:93`  |
| `C-c C-g`   | `grip-mode`                           | `lisp/init-writing.el:101` |


`C-c C-g` 仅在 `**henri-enable-grip` 非 nil** 且安装了 `grip-mode` 包时配置。

---

## 3. 界面 · Centaur Tabs（`lisp/init-styling.el`）

仅在 `**henri-enable-centaur-tabs` 非 nil** 且**图形界面**下加载。


| 快捷键                   | 命令                      | 位置                         |
| --------------------- | ----------------------- | -------------------------- |
| `C-<prior>`（Page Up）  | `centaur-tabs-backward` | `lisp/init-styling.el:211` |
| `C-<next>`（Page Down） | `centaur-tabs-forward`  | `lisp/init-styling.el:212` |


以下在 `**henri/setup-tabs-mouse-support`** 中绑定（启用 centaur-tabs 后执行）：


| 快捷键            | 命令                      | 位置                         |
| -------------- | ----------------------- | -------------------------- |
| `mouse-4`（滚轮上） | `centaur-tabs-backward` | `lisp/init-styling.el:303` |
| `mouse-5`（滚轮下） | `centaur-tabs-forward`  | `lisp/init-styling.el:304` |


---

## 4. 编程（`lisp/init-programming.el`）


| 快捷键                     | 命令                        | 位置                             |
| ----------------------- | ------------------------- | ------------------------------ |
| `C-c e f`               | `eglot-format`            | `lisp/init-programming.el:63`  |
| `C-' C-'`（连续按 `C-'` 两次） | `imenu-list-smart-toggle` | `lisp/init-programming.el:236` |
| `<f5>`                  | `quickrun`                | `lisp/init-programming.el:260` |
| `<f6>`                  | `realgud:pdb`             | `lisp/init-programming.el:247` |
| `<f9>`                  | `realgud:cmd-break`       | `lisp/init-programming.el:248` |
| `<f10>`                 | `realgud:cmd-step-over`   | `lisp/init-programming.el:249` |
| `<f11>`                 | `realgud:cmd-step`        | `lisp/init-programming.el:250` |
| `<f12>`                 | `realgud:cmd-next`        | `lisp/init-programming.el:251` |


说明：`realgud` 与 `**octave-mode`** 下部分功能键（如 `<f9>`–`<f11>`）可能重叠；在 Octave buffer 中通常以 **major mode 本地绑定** 为准。

---

## 5. Lisp / Emacs Lisp / Helpful（`lisp/programming_languages/init-lisp.el`）

### 5.1 Common Lisp（`lisp-mode-map` / SLIME）


| 快捷键       | 命令                      | 位置                                            |
| --------- | ----------------------- | --------------------------------------------- |
| `C-c C-z` | `slime`                 | `lisp/programming_languages/init-lisp.el:53`  |
| `C-c C-c` | `slime-compile-defun`   | `lisp/programming_languages/init-lisp.el:54`  |
| `C-c C-l` | `slime-load-file`       | `lisp/programming_languages/init-lisp.el:55`  |
| `C-c f`   | `my/format-lisp-buffer` | `lisp/programming_languages/init-lisp.el:122` |


### 5.2 SLIME 缓冲（`slime-mode-map`）


| 快捷键     | 命令              | 位置                                           |
| ------- | --------------- | -------------------------------------------- |
| `C-c i` | `slime-inspect` | `lisp/programming_languages/init-lisp.el:57` |


### 5.3 Emacs Lisp（`emacs-lisp-mode-map`）


| 快捷键       | 命令            | 位置                                           |
| --------- | ------------- | -------------------------------------------- |
| `C-c C-c` | `eval-defun`  | `lisp/programming_languages/init-lisp.el:95` |
| `C-c C-b` | `eval-buffer` | `lisp/programming_languages/init-lisp.el:96` |
| `C-c C-r` | `eval-region` | `lisp/programming_languages/init-lisp.el:97` |


### 5.4 Helpful（全局）


| 快捷键     | 命令                 | 位置                                            |
| ------- | ------------------ | --------------------------------------------- |
| `C-h f` | `helpful-callable` | `lisp/programming_languages/init-lisp.el:128` |
| `C-h v` | `helpful-variable` | `lisp/programming_languages/init-lisp.el:129` |
| `C-h k` | `helpful-key`      | `lisp/programming_languages/init-lisp.el:130` |
| `C-h F` | `helpful-function` | `lisp/programming_languages/init-lisp.el:131` |
| `C-h C` | `helpful-command`  | `lisp/programming_languages/init-lisp.el:132` |


---

## 6. Org 相关

### 6.1 全局（`lisp/writing/org/org-base.el` / `org-journal.el` / `org-latex.el`）


| 快捷键       | 命令                               | 位置                                    |
| --------- | -------------------------------- | ------------------------------------- |
| `C-c o f` | `henri/emergency-org-fix`        | `lisp/writing/org/org-base.el:506`    |
| `C-c o v` | `henri/cycle-org-startup-folded` | `lisp/writing/org/org-base.el:507`    |
| `C-c c`   | `org-capture`                    | `lisp/writing/org/org-journal.el:313` |
| `C-c a`   | `org-agenda`                     | `lisp/writing/org/org-journal.el:314` |
| `C-c j s` | `my/search-journal`              | `lisp/writing/org/org-journal.el:315` |
| `C-c j d` | `my/view-diary-by-date`          | `lisp/writing/org/org-journal.el:316` |
| `C-c l d` | `org-latex-diagnose-fonts`       | `lisp/writing/org/org-latex.el:344`   |
| `C-c l r` | `org-latex-reload-config`        | `lisp/writing/org/org-latex.el:345`   |


### 6.2 `org-mode` 缓冲（`lisp/writing/org/org-base.el`）


| 快捷键       | 命令                         | 位置                                 |
| --------- | -------------------------- | ---------------------------------- |
| `C-c C-r` | `henri/reload-org-theme`   | `lisp/writing/org/org-base.el:511` |
| `C-c C-b` | `henri/toggle-org-bullets` | `lisp/writing/org/org-base.el:512` |
| `C-c v s` | `henri/org-show-all`       | `lisp/writing/org/org-base.el:513` |
| `C-c v o` | `henri/org-overview`       | `lisp/writing/org/org-base.el:514` |
| `C-c v c` | `henri/org-content`        | `lisp/writing/org/org-base.el:515` |


### 6.3 Org HTML 主题（`org-mode-map`，`lisp/writing/org/org-html.el`）


| 快捷键       | 命令                                    | 位置                                 |
| --------- | ------------------------------------- | ---------------------------------- |
| `C-c h t` | `my/org-html-set-theme`               | `lisp/writing/org/org-html.el:389` |
| `C-c h d` | `my/org-html-apply-default-theme`     | `lisp/writing/org/org-html.el:390` |
| `C-c h e` | `my/org-html-export-and-open`         | `lisp/writing/org/org-html.el:391` |
| `C-c h w` | `my/org-html-export-with-theme`       | `lisp/writing/org/org-html.el:392` |
| `C-c h c` | `my/org-html-add-custom-css`          | `lisp/writing/org/org-html.el:393` |
| `C-c h r` | `my/org-html-remove-theme`            | `lisp/writing/org/org-html.el:394` |
| `C-c h i` | `my/org-html-install-themes`          | `lisp/writing/org/org-html.el:395` |
| `C-c h k` | `my/org-html-check-local-themes`      | `lisp/writing/org/org-html.el:396` |
| `C-c h s` | `my/org-html-apply-theme-by-shortcut` | `lisp/writing/org/org-html.el:399` |
| `C-c h ?` | `my/org-html-show-theme-shortcuts`    | `lisp/writing/org/org-html.el:400` |
| `C-c h 1` | `my/org-html-theme-1`                 | `lisp/writing/org/org-html.el:401` |
| `C-c h 2` | `my/org-html-theme-2`                 | `lisp/writing/org/org-html.el:402` |
| `C-c h 0` | `my/org-html-theme-default`           | `lisp/writing/org/org-html.el:403` |


### 6.4 学术写作 minor mode（`org-academic-mode-map`，`lisp/writing/org/org-academic.el`）

**全局 minor mode `org-academic-mode` 开启时**，下列键可用：


| 快捷键       | 命令                                        | 位置                                     |
| --------- | ----------------------------------------- | -------------------------------------- |
| `C-c a n` | `org-academic-create-idea-note`           | `lisp/writing/org/org-academic.el` |
| `C-c a r` | `org-academic-create-reading-note`        | `lisp/writing/org/org-academic.el` |
| `C-c a j` | `org-academic-create-project-note`        | `lisp/writing/org/org-academic.el` |
| `C-c a p` | `org-academic-create-paper`               | `lisp/writing/org/org-academic.el` |
| `C-c a c` | `org-academic-create-conference-abstract` | `lisp/writing/org/org-academic.el` |
| `C-c a P` | `org-academic-quick-paper`                | `lisp/writing/org/org-academic.el` |
| `C-c a N` | `org-academic-quick-note`                 | `lisp/writing/org/org-academic.el` |
| `C-c a b` | `org-academic-setup-bibliography`         | `lisp/writing/org/org-academic.el` |
| `C-c a d` | `org-academic-dashboard`                  | `lisp/writing/org/org-academic.el` |
| `C-c a i` | `org-academic-insert-citation`            | `lisp/writing/org/org-academic.el` |


学术 Dashboard 缓冲区内另有 `**q`** → `kill-this-buffer`（`local-set-key`，`lisp/writing/org/org-academic.el:808`）。

---

### 6.5 Org-roam 通用知识库（`lisp/writing/org/org-roam-henri.el`）

| 快捷键 | 命令 | 位置 |
| ------ | ---- | ---- |
| `C-c n f` | `org-roam-node-find` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n i` | `org-roam-node-insert` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n b` | `org-roam-buffer-toggle` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n c` | `org-roam-capture` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n g` | `org-roam-graph` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n j` | `org-roam-dailies-capture-today` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n t` | `org-roam-dailies-goto-today` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n y` | `org-roam-dailies-goto-yesterday` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n m` | `org-roam-dailies-goto-tomorrow` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n d` | `henri-org-roam-open-directory` | `lisp/writing/org/org-roam-henri.el` |
| `C-c n x` | `henri-org-roam-open-inbox` | `lisp/writing/org/org-roam-henri.el` |

若本机已安装 `consult-org-roam`，还会启用 `C-c n s/r/l` 用于全文搜索、反链和前向链接。

`C-c n` 前缀保留给 Org-roam；项目文件树使用 `C-c f p`。

---

## 7. Octave（`lisp/programming_languages/init-octave.el`）

均在 `**octave-mode`** 下。前缀 `**C-c C-p`** → `octave-project-prefix-map`（`lisp/programming_languages/init-octave.el:324`），其下：


| 快捷键 | 命令                       | 位置                                              |
| --- | ------------------------ | ----------------------------------------------- |
| `s` | `octave-setup-project`   | `lisp/programming_languages/init-octave.el:322` |
| `c` | `octave-clear-workspace` | `lisp/programming_languages/init-octave.el:323` |


### 7.1 `use-package` 中绑定


| 快捷键       | 命令                         | 位置                                             |
| --------- | -------------------------- | ---------------------------------------------- |
| `C-c C-i` | `octave-inferior-buffer`   | `lisp/programming_languages/init-octave.el:56` |
| `C-c C-r` | `octave-send-region`       | `lisp/programming_languages/init-octave.el:57` |
| `C-c C-b` | `octave-send-buffer`       | `lisp/programming_languages/init-octave.el:58` |
| `C-c C-f` | `octave-send-defun`        | `lisp/programming_languages/init-octave.el:59` |
| `C-c C-l` | `octave-send-line`         | `lisp/programming_languages/init-octave.el:60` |
| `C-c C-h` | `octave-help`              | `lisp/programming_languages/init-octave.el:61` |
| `C-c C-d` | `octave-describe-function` | `lisp/programming_languages/init-octave.el:62` |
| `M-.`     | `octave-find-definition`   | `lisp/programming_languages/init-octave.el:63` |
| `M-,`     | `pop-tag-mark`             | `lisp/programming_languages/init-octave.el:64` |
| `C-c C-k` | `octave-kill-process`      | `lisp/programming_languages/init-octave.el:65` |


### 7.2 调试与格式化等


| 快捷键         | 命令                                | 位置                                              |
| ----------- | --------------------------------- | ----------------------------------------------- |
| `<f9>`      | `octave-set-breakpoint`           | `lisp/programming_languages/init-octave.el:196` |
| `S-<f9>`    | `octave-clear-breakpoints`        | `lisp/programming_languages/init-octave.el:197` |
| `<f10>`     | `octave-step-debug`               | `lisp/programming_languages/init-octave.el:198` |
| `<f11>`     | `octave-continue-debug`           | `lisp/programming_languages/init-octave.el:199` |
| `C-<f5>`    | `octave-debug-file`               | `lisp/programming_languages/init-octave.el:200` |
| `C-c C-q`   | `octave-format-buffer`            | `lisp/programming_languages/init-octave.el:228` |
| `C-c C-u`   | `octave-format-region`            | `lisp/programming_languages/init-octave.el:229` |
| `C-c C-t f` | `octave-insert-function-template` | `lisp/programming_languages/init-octave.el:273` |
| `C-c C-t s` | `octave-insert-script-template`   | `lisp/programming_languages/init-octave.el:274` |
| `C-c C-?`   | `octave-show-function-help`       | `lisp/programming_languages/init-octave.el:296` |
| `C-c C-t t` | `octave-run-tests`                | `lisp/programming_languages/init-octave.el:297` |


---

## 8. LaTeX 主题（`lisp/writing/LaTeX/latex-themes.el`）

在 `**LaTeX-mode` 缓冲**中由 `latex-themes-setup-keybindings` 设置：


| 快捷键       | 命令                                 | 位置                                       |
| --------- | ---------------------------------- | ---------------------------------------- |
| `C-c t i` | `latex-themes-insert-theme`        | `lisp/writing/LaTeX/latex-themes.el:152` |
| `C-c t c` | `latex-themes-create-custom-theme` | `lisp/writing/LaTeX/latex-themes.el:153` |
| `C-c t e` | `latex-themes-edit-custom-theme`   | `lisp/writing/LaTeX/latex-themes.el:154` |
| `C-c t p` | `latex-themes-preview-theme`       | `lisp/writing/LaTeX/latex-themes.el:155` |
| `C-c t l` | `latex-themes-list-available`      | `lisp/writing/LaTeX/latex-themes.el:156` |


---

## 9. 按键不生效时

- 在 Emacs 内 `**C-h k`**（`describe-key`），再按疑问组合键，查看实际绑定的命令与所属 keymap。
- macOS **系统快捷键**可能先于 Emacs 截获；可在「系统设置 → 键盘 → 键盘快捷键」中排查或改用其它前缀。
- 部分绑定依赖 `**defcustom`**（如 `henri-enable-magit`）或 **major/minor mode**；以本文「位置」列源文件为准。
