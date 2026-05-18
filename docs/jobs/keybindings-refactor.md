# 快捷键系统重构 (jobs)

> lifecycle: in-progress
> entrypoint: 本文档 + `lisp/init-keys.el`（待新增）
> outputs: `lisp/init-keys.el`、各模块 `:bind` 调整、`docs/tutorials/keybindings.md` 重写
> summary: 全局快捷键经过两年增量演进，前缀语义重叠（`C-c a` / `C-c h` / `C-c m` / `C-c v` / `C-c o` / F-key），并覆盖 Org 默认键（`C-c l`, `C-c C-b`）。本次按「一前缀一领域 + mode 内动作收进 mode prefix-map」原则重构。
> smoke: `M-x henri/doctor`、Org buffer 中 `C-c C-b` 应回到 Org 默认、`C-c l` 应是 `org-store-link`、`which-key` 弹窗每个 `C-c X` 都有标题。
> freeze rule: 当 `keybindings.md` 与代码完全对齐、且不再出现 prefix 冲突报告 / 用户记不住的键，归档到 `docs/legacy/`。

---

## 1. 现状审查

### 1.1 主要冲突

| # | 冲突 | 涉及文件 |
|---|------|----------|
| 1 | `C-c h d` 全局 = doctor，Org buffer 被 `henri/org-html-apply-default-theme` 覆盖 | `lisp/ops/doctor.el:61`, `lisp/writing/org/org-html.el:413` |
| 2 | `C-c C-b` 在 org-mode 被改写为 `henri/toggle-org-bullets`，破坏 `org-tree-to-indirect-buffer` | `lisp/writing/org/org-base.el:247` |
| 3 | `C-c l` 全局被 LaTeX 主题占用，与 `org-store-link` 冲突 | `lisp/writing/org/org-latex.el:340-344` |
| 4 | `<f5> <f9> <f10> <f11>` 全局 realgud / 局部 octave 双重含义 | `init-programming.el:227-240` vs `init-octave.el:196-200` |
| 5 | `C-c w e` 与 `C-c w v` 都绑 `henri/vterm` | `init-managing.el:217-218` |
| 6 | `C-c m c` 同时给 markdown / pdf 自检 | `markdown-base.el`, `pdf-base.el:93` |
| 7 | `C-c v` 既是 diff-hl（vc）又是 org 视图（view） | `init-managing.el`, `org-base.el` |
| 8 | `C-c o` 混合 org-base 与 agenda dashboard | `org-base.el`, `org-journal.el:496-503` |
| 9 | 主题入口分散：org-html=`C-c h`、latex=`C-c t`、org-latex=`C-c l`、视图=`C-c v` | 多处 |
| 10 | 缺 `which-key-add-key-based-replacements`，所有 prefix 弹窗无标题 | 全仓库 |
| 11 | `C-c a` 同时承担 Org Agenda 与 Academic，开启 academic minor mode 后心智模型分裂 | `org-journal.el`, `org-academic.el` |

### 1.2 设计原则

1. 一前缀对应一领域。
2. 不覆盖 Org / Emacs 已有约定键（`C-c a / c / l / C-c C-b / C-c C-c`）。
3. 全局一级前缀 ≤ 12 个；每个 `C-c X` 都有 which-key 标题。
4. mode 内动作收进 mode prefix-map。
5. 全局禁用 `<f5>-<f12>` 绑定，避免与 mode-local 冲突。
6. 动词字母统一：`s` start/search、`n/p` next/prev、`e` export、`l` list、`r` reload/rename、`t` toggle、`d` delete/diagnose。

---

## 2. 全局前缀地图（目标态）

| 前缀 | 领域 | which-key 标题 | 二级键 |
|---|---|---|---|
| `C-c f` | file / find | `+file` | `f` find-in-notes、`p` project-tree、`r` recent |
| `C-c b` | buffer / bookmark | `+buffer` | `b` switch、`k` kill |
| `C-c w` | window / workspace | `+window` | `l` layout、`v` vterm、`e` eshell |
| `C-c g` | git (Magit + diff-hl + smerge) | `+git` | `g/d/f/b/l` Magit；`n/p/r` hunk；`m {n/p/u/l/b/a}` smerge |
| `C-c s` | search | `+search` | `l` consult-line、`r` consult-ripgrep、`o` consult-outline |
| `C-c t` | toggle / theme | `+toggle` | `t` doom-theme、`b` big-font、`l` line-num |
| `C-c h` | help / doctor | `+help` | `d` doctor、`f/v/k/F/C` helpful（保留） |
| `C-c o` | org ops（跨 buffer） | `+org` | `a` agenda-dashboard、`i/t/p` inbox/tasks/projects、`f` emergency-fix、`v` startup-fold |
| `C-c n` | notes / org-roam | `+roam` | 现状保留 |
| `C-c j` | journal | `+journal` | `s` search、`d` view-by-date |
| `C-c r` | rime | `+rime` | 现状 |
| `C-c a` | org-agenda（保留） | `agenda` | — |
| `C-c A` | academic | `+academic` | `n/r/j/p/c/P/N/b/d/i` |
| `C-c c` | org-capture（保留） | `capture` | — |
| `C-c l` | org-store-link（**释放**） | `store-link` | — |

### Mode-local 收口

| Mode | 前缀 | 内容 |
|---|---|---|
| `markdown-mode-map` | `C-c m` | preview / export / toc / outline / 截图 / 字数 / 模板 |
| `org-mode-map` | `C-c m h` | Org HTML 主题（原 `C-c h …`） |
| `org-mode-map` | `C-c m l` | Org LaTeX 导出/主题（原 `C-c l …`） |
| `org-mode-map` | `C-c m v` | Org 视图 show-all/overview/content（原 `C-c v …`） |
| `LaTeX-mode-map` | `C-c m` | latex-themes 5 键 |
| `pdf-view-mode-map` | `C-c m` | pdf 自检/midnight |
| `octave-mode-map` | `C-c C-p` | 已存在 prefix-map，保留 |
| `prog-mode` (eglot) | `C-c e` | `f` format、`r` rename、`a` action、`d` def、`s` symbol |
| `prog-mode` (debug) | `C-c d` | `r` run、`b` break、`s` step、`n` next、`o` step-out（替 F-key） |

---

## 3. 迁移清单

决策记录：Academic 改用大写 `C-c A`，`C-c a` 不保留旧别名，确保它始终是 `org-agenda`。

### 3.1 释放 / 还原

- [x] `org-base.el`：删除 `C-c C-b` 覆盖；`henri/toggle-org-bullets` 改为 `C-c m v b`
- [x] `org-base.el`：`C-c v {s/o/c}` → `C-c m v {s/o/c}`
- [x] `org-html.el`：`C-c h …` 全部 → `C-c m h …`
- [x] `org-latex.el`：`C-c l …` 全部 → `C-c m l …`
- [x] `init-managing.el`：删 `C-c w e` 的 vterm 别名（保留 `C-c w v` = vterm，`C-c w e` = eshell）
- [x] `init-programming.el`：移除 `<f5>/<f6>` 全局；新增 `C-c d {r/b/s/n/o}`
- [x] `init-octave.el`：`<f9>-<f11>` 改为 `octave-mode-map` 内 `C-c d {b/B/s/c}`
- [x] `pdf-base.el`：`C-c m c` → `C-c m d c`（避免与 markdown `C-c m c` 重名）
- [x] `org-academic.el`：`C-c a ...` → `C-c A ...`，释放 `C-c a` 给 agenda

### 3.2 新增

- [x] 新建 `lisp/init-keys.el`：
  - 定义 `which-key-add-key-based-replacements` 标签
  - 集中托管全局 prefix 命名（不接管单键绑定）
  - `init.el` 中在 `init-managing` 之后 `require`
- [x] `lisp/init-styling.el` 或 `init-keys.el`：`C-c t {t/b/l}` toggle 组（theme / big-font / display-line-numbers）

### 3.3 同步文档

- [x] 重写 `docs/tutorials/keybindings.md`，按上表分章
- [x] `docs/specs/ARCHITECTURE.md` 加「键位治理」段，链接本 jobs
- [ ] 完成后将本文件移入 `docs/legacy/`，保留迁移说明

---

## 4. 验证 (smoke)

1. `emacs -Q --batch -l init.el` 不报 `which-key` 标签缺失 / prefix 冲突警告。
2. 启动后在 Org buffer：
   - `C-h k C-c C-b` 应是 `org-tree-to-indirect-buffer`
   - `C-h k C-c l` 应是 `org-store-link`
   - `C-h k C-c h d` 应是 `henri/doctor`
3. `C-c` 后等待 which-key 弹窗，所有一级前缀都有中文/英文标题。
4. `<f5>` 在普通 buffer 不再触发 `quickrun`；编程 buffer 中 `C-c d r` 可触发。
5. Octave buffer 中 `C-c d {b/B/s/c}` 行为与文档一致。

---

## 5. 风险与回滚

- 风险：肌肉记忆中断（尤其 `C-c h …` Org HTML 主题用户）。
- 缓解：在 `init-keys.el` 内提供过渡 alias（旧键发出 `message` 提示新键），跑 1-2 周后删除。
- 回滚：本次重构集中在 `init-keys.el` + 几个 mode-local 文件；`git revert` 可整体撤回。
