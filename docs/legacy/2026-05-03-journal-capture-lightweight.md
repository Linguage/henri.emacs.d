# Journal Capture 轻量化与同日合流

- `状态`: doing
- `最近更新`: 2026-05-03
- `lifecycle`: active patch
- `entrypoint`: `lisp/writing/org/org-journal.el`（capture 模板、文件路径与 goto 函数、agenda 收集）
- `outputs`:
  - **diary（不变）**: `~/Documents/EmacsNotes/Journal/journal-YYYY-MM.org`（**月文件**）
  - **work（新）**: `~/Documents/EmacsNotes/Journal/worklog/YYYY-MM-DD.org`（**日文件**，子目录）
  - **study（新）**: `~/Documents/EmacsNotes/Journal/studylog/YYYY-MM-DD.org`（**日文件**，子目录）
- `summary`: diary 维持月度仪式化模板不动；work / study 改为**按日命名**的 plain 流水文件，存入 Journal 各自子目录；同日多次 capture 自然汇入同一日文件，跨日自动新建文件。
- `smoke`:
  1. `emacs --batch -Q --load early-init.el --load init.el` 启动通过；
  2. `C-c c w` 输入一句话 → `Journal/worklog/2026-05-03.org` 出现 `[HH:MM] ...`；同日再 `C-c c w` → 同一文件追加第二条；
  3. `C-c c s` 同上落到 `Journal/studylog/2026-05-03.org`；
  4. `C-c c d` 行为不变（仍写入月度 `journal-2026-05.org`，含"今日要点 / 花销表"）；
  5. `C-c a w` / `C-c a s` / `C-c a j` 三个 agenda 视图能扫到新子目录里的日文件。
- `freeze rule`: 三种 capture 各自落到正确路径、同日合流验证通过、agenda 视图覆盖到子目录、`henri-journal-refresh-agenda-files` 被新规则替换；本文档迁入 `docs/legacy/`。

---

## 1. 背景

[org-journal.el](../../lisp/writing/org/org-journal.el) 当前的统一前提是"三种 kind 都用月文件 + 月/日 heading 嵌套"：

```
Journal/
├── journal-2026-05.org    ← diary（月）
├── worklog-2026-05.org    ← work（月，文件内才分日）
└── studylog-2026-05.org   ← study（月，文件内才分日）
```

文件内层级是 `* 月 / ** 日 / *** 时间戳条目 / **** 子标题`，每次 capture 在当日 subtree 下新建 level-3 entry。两个错位：

1. 同日多次 capture 横向并列多个 level-3 子树，"今天"被切碎；
2. work / study 的预设子标题（"完成任务 / 明日计划 / 资源链接"）把 capture 推向"计划与进度管理"，与"随手捕捉想法"不符。

用户已在前序对话中明确：

- diary 当前模板（含**花销记录表**）有用，**保持不动**；
- work / study 应**弱化进度管理**，强化随手捕捉；
- work / study 应**按日期命名**，不再共用月文件；
- work / study 应**存到 Journal 子目录**，不再与 diary 平铺在 Journal 根目录。

经查 `worklog-2026-05.org` / `studylog-2026-05.org` 各仅 4 行（仅 header），**当前没有需要迁移的内容**，可直接弃用旧月文件。

### 1.1 顺手修掉的 diary 现存 bug（P0 前置）

排查过程中发现 `henri-journal-goto-month-day` 末尾的 `(org-end-of-subtree t t)` + `(insert "\n")` 会把 point 推离 day heading：

- 当日**首次** capture（day 刚被创建在 EOB）→ point 落到 EOB → org-capture `target-entry-p = nil` → 落入 "顶级 entry" 分支 → 生成 `* [...]`（level 1，错）；
- 当日**第二次** capture（day 已有 buggy level-1 兄弟节点）→ `org-end-of-subtree t t` 跳到那条 level-1 → org-capture 把它当父级 → 生成 `** [...]`（level 2，仍错）。

实证：[`journal-2026-05.org`](../../../Documents/EmacsNotes/Journal/journal-2026-05.org) L42 是 `* [00:43]`、L56 是 `** [00:44]`，分别对应上述两种错落级别；而 L8 的 `*** [16:18]`（5/2）刚好处在"day 新建但不在 EOB"的特殊情形里，巧合避开 bug。

修复策略：删除那两行尾巴，让 goto 函数返回时 point **停在 day heading 上**，由 org-capture 自己派生子级 level 3。修复已落在本仓库 [`org-journal.el`](../../lisp/writing/org/org-journal.el) 里（commit 待提交），不影响其它流程。

**遗留**：上面 L42 与 L56 那两条**已写入文件的旧条目**仍是错落级别，属于用户数据，不在本 job 自动处理范围；建议手动调整为 `*** ...` 后置于 `** 2026-05-03 Sunday` 之下。

## 2. 目标

1. **diary 路径与模板完全不变**：仍走 `journal-YYYY-MM.org`（月）+ 现有模板字段。
2. **work / study 改为日文件 + 子目录**：
   - 路径：`Journal/worklog/YYYY-MM-DD.org`、`Journal/studylog/YYYY-MM-DD.org`；
   - 文件首次创建时写入 `#+TITLE` 与 `#+FILETAGS:`；
   - 不再有 `* 月 / ** 日` 嵌套，**整个文件就是"今天"**。
3. **同日合流自然成立**：因为同一天 capture 都打开同一个日文件并追加到末尾，无需额外结构。
4. **跨日自动新建**：换一天 capture，目标路径里的 `YYYY-MM-DD` 变化，自动落到新文件。
5. **agenda 视图保持可用**：`C-c a w` `C-c a s` `C-c a j` 仍能聚合对应 tag 的内容（依赖 FILETAGS 继承）。

## 3. 非目标

1. 不合并三种 kind 为单一文件 + tag 模式；
2. 不动 diary 的任何路径、模板、子标题、花销表；
3. 不删除 [org-academic.el](../../lisp/writing/org/org-academic.el) 与其它 org 子模块；
4. 不调整 LaTeX/HTML 导出 hook（`org-journal-auto-setup-pdf` 等）；
5. 不引入 org-roam / denote。

## 4. 设计

### 4.1 三个 kind 的角色与存储策略

```diagram
                      ╭───────────────╮
                      │   今天 (day)   │
                      ╰───────┬───────╯
                              │
        ╭─────────────────────┼─────────────────────╮
        │                     │                     │
   ╭────┴─────╮         ╭─────┴─────╮         ╭─────┴─────╮
   │ :diary:  │         │  :work:   │         │  :study:  │
   │ 仪式化记录 │         │ 流水想法   │         │ 流水想法   │
   │ 模板不动  │         │ 日文件     │         │ 日文件     │
   ╰────┬─────╯         ╰─────┬─────╯         ╰─────┬─────╯
        │                     │                     │
        ▼                     ▼                     ▼
  Journal/journal-       Journal/worklog/      Journal/studylog/
  YYYY-MM.org            YYYY-MM-DD.org        YYYY-MM-DD.org
   (月，含子标题)          (日，纯流水)            (日，纯流水)
```

### 4.2 期望的文件形态

**diary 月文件（保持原状）**

```org
#+TITLE: 个人日记 2026-05
#+LATEX_CLASS: journal
#+STARTUP: content

* 2026-05 May
** 2026-05-03 Sunday
*** [2026-05-03 Sun 10:30] :journal:diary:
**** 今日要点
**** 花销记录
| 项目 | 金额 | 类别 |
...
```

**work / study 日文件（新形态）**

```org
#+TITLE: 工作日志 2026-05-03
#+FILETAGS: :journal:work:
#+STARTUP: content

[10:30] 调通了 module Z，关键是 …
[14:15] 想到 X 这件事其实是 Y 的反面 …
[20:00] 又一个想法 …
```

要点：
- **没有 `* 月` heading，没有 `** 日` heading**；
- 整个文件就是"今天"，标题已经在 `#+TITLE` 里；
- 想法之间以空行分隔，每条以 `[HH:MM]` 时间戳起头；
- `#+FILETAGS:` 让文件内所有 entry/段落继承 `:journal:work:` 或 `:journal:study:`，agenda tag 检索仍然命中。

### 4.3 实现切片

修改集中在 [org-journal.el](../../lisp/writing/org/org-journal.el)，分四个切片：

#### S1. 路径辅助函数

新增（或修改现有）以下函数，**不改 diary 相关函数**：

```elisp
;; 子目录路径
(defun henri-journal-subdir (kind)
  "Return absolute subdir for KIND ('work or 'study)."
  (expand-file-name
   (pcase kind ('work "worklog") ('study "studylog"))
   henri-journal-directory))

;; 日文件名（不带前缀，目录已说明 kind）
(defun henri-journal-day-string (&optional time)
  (format-time-string "%Y-%m-%d" (or time (current-time))))

;; 日文件绝对路径
(defun henri-journal-daily-file (kind &optional time)
  (expand-file-name
   (concat (henri-journal-day-string time) ".org")
   (henri-journal-subdir kind)))

;; 确保日文件存在（首次创建写入 TITLE / FILETAGS）
(defun henri-journal-ensure-daily-file (kind &optional time)
  ...)

;; 用于 capture target 的 wrapper
(defun henri-journal-current-worklog-daily-file ()
  (henri-journal-ensure-daily-file 'work))
(defun henri-journal-current-studylog-daily-file ()
  (henri-journal-ensure-daily-file 'study))
```

文件首次创建时写入：

```
#+TITLE: 工作日志 YYYY-MM-DD     ← study 替换为"学习日志"
#+FILETAGS: :journal:work:        ← study 替换为 :journal:study:
#+STARTUP: content

```

#### S2. capture 模板替换

`org-capture-templates` 改为：

```elisp
(setq org-capture-templates
      '(;; diary：保持原状（月文件 + 完整模板 + 花销表）
        ("d" "个人日记" entry
         (file+function henri-journal-current-diary-file
                        henri-journal-goto-month-day)
         "*** %U %? :journal:diary:\n%i\n**** 今日要点\n\n**** 花销记录\n| 项目 | 金额 | 类别 |\n|------+------+------|\n|      |      |      |\n"
         :empty-lines 1)

        ;; work：日文件 + plain 流水
        ("w" "工作随想" plain
         (file+function henri-journal-current-worklog-daily-file
                        henri-journal-goto-file-end)
         "[%<%H:%M>] %?\n"
         :empty-lines 1
         :unnarrowed nil)

        ;; study：日文件 + plain 流水
        ("s" "学习随想" plain
         (file+function henri-journal-current-studylog-daily-file
                        henri-journal-goto-file-end)
         "[%<%H:%M>] %?\n"
         :empty-lines 1
         :unnarrowed nil)))
```

新增极简 goto：

```elisp
(defun henri-journal-goto-file-end ()
  "Move point to end of buffer; ensure newline before insertion."
  (goto-char (point-max))
  (unless (bolp) (insert "\n")))
```

注意：work / study 的描述从"工作日志/学习日志"改为"工作随想/学习随想"，呼应"弱化进度管理"。

#### S3. agenda 文件收集改造

[org-journal.el#L182-L193](../../lisp/writing/org/org-journal.el#L182) 的 `henri-journal-agenda-files` 当前只扫 Journal 根目录的月度文件。需要扩展为：

```elisp
(defun henri-journal-agenda-files ()
  "Diary 月文件 + work/study 子目录所有日文件。"
  (let ((diary
         (cons (henri-journal-ensure-monthly-file 'diary)
               (directory-files henri-journal-directory t
                                "\\`journal-[0-9]\\{4\\}-[0-9]\\{2\\}\\.org\\'")))
        (daily
         (cl-loop for kind in '(work study)
                  for dir = (henri-journal-subdir kind)
                  when (file-directory-p dir)
                  append (directory-files dir t
                                          "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org\\'"))))
    (delete-dups (append diary daily))))
```

`henri-journal-refresh-agenda-files`、`org-capture-after-finalize-hook` 中的调用点不变（接口稳定）。

#### S4. 月度 worklog/studylog 的去除

- `henri-journal-current-worklog-file`、`henri-journal-current-studylog-file`、`henri-journal-monthly-file` 的 `'work` / `'study` 分支：保留函数但改为内部不再使用（向后兼容若干外部引用），或删除并在文件头加迁移注释。
  - **优选：删除三个 `'work` / `'study` 相关分支**，以"单一真相源"原则避免误用；
  - `henri-journal-ensure-monthly-file` 改为仅接受 `'diary`，或保留泛型但只 diary 真正调用。
- `henri-journal-kind-prefix` 的 `'work` / `'study` 分支去除（不再用 `worklog-` / `studylog-` 文件名前缀）。

#### S5. 旧月度文件清理（一次性）

`Journal/worklog-2026-05.org` 与 `Journal/studylog-2026-05.org` 各 4 行，**仅 header 无内容**。处理方式：

- 直接删除（推荐）；或
- 重命名加 `.bak` 后缀放到 `Journal/_archive/`（保守）。

本 job 选**直接删除**，并在 step 清单中作为 P1 一项。

### 4.4 风险与回滚

| 风险 | 缓解 |
|---|---|
| 子目录 `worklog/` `studylog/` 不存在导致 capture 失败 | `henri-journal-ensure-daily-file` 内 `make-directory ... t` 兜底 |
| `plain` 模板对 `org-adapt-indentation` 敏感产生前导空格 | 模板字符串显式不带前导空格；smoke 用例 W1 直接 grep 原文确认 |
| FILETAGS 写入文件时机不当（若文件已被打开但未保存） | `ensure-daily-file` 用 `write-region ... 'silent` 一次性写入完整 header；首次 capture 才触发；不会和 buffer 写冲突 |
| `org-agenda-files` 引用一堆不存在的历史日文件路径 | 新 `henri-journal-agenda-files` 用 `directory-files` 即时扫描，不缓存 |
| 旧的 `worklog-2026-05.org` 没删，agenda 仍被扫到 | S5 步骤明确删除；并且新正则不再匹配根目录的 `worklog-*` |
| 未来想把 diary 也改为日文件 | 不在本 job 范围；本 job 的 `daily-file` / `monthly-file` 函数边界清晰，未来可独立扩展 |

## 5. 步骤清单

- [ ] **P0** 新增 `henri-journal-subdir` / `henri-journal-day-string` / `henri-journal-daily-file` / `henri-journal-ensure-daily-file` / `henri-journal-current-worklog-daily-file` / `henri-journal-current-studylog-daily-file` / `henri-journal-goto-file-end`；
- [ ] **P0** 改写 `("w" ...)` 与 `("s" ...)` capture 为 plain + 日文件 target；
- [ ] **P0** 改写 `henri-journal-agenda-files`，覆盖 diary 月文件 + work/study 子目录日文件；
- [ ] **P0** 删除/收敛 `henri-journal-monthly-file` 等函数对 `'work` / `'study` 的分支；
- [ ] **P1** 删除 `Journal/worklog-2026-05.org`、`Journal/studylog-2026-05.org` 两个空 header 文件；
- [ ] **P1** smoke 1 / 2 / 3 / 4 / 5 全部跑通；
- [ ] **P2** 在 [c.md](../../c.md) 或 README 的"日常使用"段落更新一段说明：work / study 现在是按日存到 Journal 子目录的流水文件；
- [ ] **结案** 本文件迁入 `docs/legacy/`，并按需在 [docs/specs/ROADMAP.md](../specs/ROADMAP.md) 追加一句"Journal capture 轻量化已落地"。

## 6. 验收用例

1. **W1 当日首次 work capture**
   - 操作：`C-c c w` → 输入 `调通了 X 模块` → `C-c C-c`
   - 期望：`Journal/worklog/` 子目录被创建；`worklog/2026-05-03.org` 形如：
     ```
     #+TITLE: 工作随想 2026-05-03
     #+FILETAGS: :journal:work:
     #+STARTUP: content

     [HH:MM] 调通了 X 模块
     ```
2. **W2 当日二次 work capture**
   - 操作：再 `C-c c w` → 输入 `又想到 Y`
   - 期望：**同一文件**末尾追加：
     ```
     [HH:MM] 调通了 X 模块
     [HH:MM] 又想到 Y
     ```
   - 不新建 day heading、不新建 level-3。
3. **S1 study 同上**，落到 `Journal/studylog/2026-05-03.org`。
4. **D1 diary 不受影响**
   - 操作：`C-c c d` → 写日记 → `C-c C-c`
   - 期望：`Journal/journal-2026-05.org` 出现带"今日要点 / 花销记录表"的 level-3 entry。
5. **A1 agenda 视图保持**
   - 操作：`C-c a w` / `C-c a s` / `C-c a j`
   - 期望：分别命中子目录里的 work / study 日文件、根目录的 diary 月文件。
6. **X1 跨日**（可在第二天验证或临时改系统时间）
   - 操作：`C-c c w`
   - 期望：自动新建 `worklog/YYYY-MM-DD.org`（新一天的文件）。

## 7. 后续可能的扩展（非本 job）

- 若 diary 也想换成日文件，单独起 job；本 job 的路径函数已经为 diary 走日文件留了对称扩展位（`(henri-journal-daily-file 'diary ...)`）；
- 给日文件加自动周/月汇总命令：把一周内所有 `worklog/*.org` 流水拼成周报草稿；
- 当 work / study 流水里某条想法被反复回看，提供"晋升为 level-3 标题"的命令，把段落抽成可被 agenda TODO 化的条目；
- `worklog/` 与 `studylog/` 子目录命名是否改为更短的 `work/` `study/`：留给未来收敛，目前与 capture key 形成的"复数前缀"保持一致。
