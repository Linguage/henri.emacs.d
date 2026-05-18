# mu4e Newsletter Intake (jobs)

> 状态: doing
> 最近更新: 2026-05-07
> 本次完成: 完成调研与方案设计；未写入代码
> 下一步: Phase 1 把 `henri-enable-mu4e` defcustom 与 `lisp/managing/init-mu4e.el` 骨架落地
> 非目标: 不接管主邮箱（工作邮、个人收信、日历邀请、企业协作）；不在 Emacs 内做 LLM 摘要
>
> lifecycle: experimental attempt
> entrypoint: 本文档 + `henri-enable-mu4e`（默认 `nil`）+ `M-x mu4e`
> outputs: `lisp/init-custom.el`（新增 5 个 `defcustom`）、`lisp/managing/init-mu4e.el`（新增模块）、`lisp/ops/doctor.el`（新增 mu/mbsync 检查项）、`docs/specs/ARCHITECTURE.md`（managing 模块条目）、`docs/tutorials/keybindings.md`（`C-c m e` / mu4e 入口）；运行时副产物：`~/Mail/newsletter/**`（Maildir）、`/Users/henripogatrain/Documents/TolariaHub/MiaoYan-Notes/Clippings/inbox/<date>-<slug>.md`
> summary: 把 mu4e 定位为 **newsletter intake system**，不是通用邮件客户端。Gmail label `Newsletters` → `mbsync` → 本地 Maildir → mu4e 阅读/筛选 → 一键导出为带 frontmatter 的 markdown 到 MiaoYan-Notes 的 `Clippings/inbox/` → 由 MiaoYan-Notes 自有 `workflows/scripts/summarize_inbox.py` 完成 LLM 摘要与归档。两个仓库通过文件系统解耦，Emacs 端不引入任何 LLM 依赖。
> smoke: ① `M-x henri/doctor` 报 mu / mbsync / MiaoYan-Notes 路径全绿；② `mbsync -a` 拉到首批邮件；③ `M-x mu4e` 进入主界面，按 `b n` 弹出 "Newsletter · Unread" 结果；④ 在 headers buffer 上按 `C-c m e`，目标文件出现在 `MiaoYan-Notes/Clippings/inbox/`，frontmatter 至少含 `title` 与 `source`；⑤ 在 MiaoYan-Notes 仓库内运行 `python workflows/scripts/summarize_inbox.py 1 --provider claude` 能成功消费该文件。
> freeze rule: 当 (a) 模块在新机重装 < 10 分钟可走通；(b) 连续两周稳定每日跑通"拉信 → 阅读 → 导出 → 摘要"链路；(c) `ARCHITECTURE.md` 与 `keybindings.md` 与代码完全对齐 — 归档到 `docs/legacy/`。

---

## 1. 背景与定位

### 1.1 为什么不是"全能邮件客户端"

mu4e 的强项是：本地 Maildir、`mu` 索引、bookmark 形式的 saved-search、文本工作流。短板是：HTML newsletter 渲染脏；日历邀请、富文本回信、企业邮箱协作较弱；移动端同步状态需自行维护。

工作邮 / 私人主邮箱继续走原生客户端（macOS Mail / Gmail Web），mu4e **只**承担 newsletter 这一条信息流。

### 1.2 三层数据流

```diagram
╭─────────────╮   ╭────────╮   ╭──────────────────╮   ╭──────╮
│ Gmail label │──▶│ mbsync │──▶│ ~/Mail/newsletter│──▶│ mu4e │
│ Newsletters │   │(isync) │   │   /{Inbox,AI,…}  │   │      │
╰─────────────╯   ╰────────╯   ╰──────────────────╯   ╰──┬───╯
                                                          │ C-c m e
                                                          ▼
                              ╭────────────────────────────────────╮
                              │ MiaoYan-Notes/Clippings/inbox/     │
                              │   YYYY-MM-DD-<slug>.md             │
                              │   (frontmatter: title + source)    │
                              ╰──────────────┬─────────────────────╯
                                             ▼
                              workflows/scripts/summarize_inbox.py
                              （MiaoYan-Notes 自有 LLM 流水线）
```

### 1.3 与 MiaoYan-Notes 的契约

经检查 [`MiaoYan-Notes/workflows/scripts/summarize_inbox.py`](file:///Users/henripogatrain/Documents/TolariaHub/MiaoYan-Notes/workflows/scripts/summarize_inbox.py) 与 `Clippings/inbox/*.md` 的现有样本，**最小 frontmatter 仅需两个字段**：

```yaml
---
title: "<邮件主题>"
source: "<原文链接 or mailto:from>"
---
```

可选追加：`author`、`date`、`tags`、`description`。Emacs 端导出时全部填上即可。MiaoYan-Notes 入口路径可由 `c.md:23` 命令直接消费：

```bash
python workflows/scripts/summarize_inbox.py --provider claude
```

**两仓库通过文件系统握手，Emacs 仓库 0 个 Python 依赖。**

---

## 2. 系统前置（仓库外）

### 2.1 macOS 工具链

```bash
brew install mu isync          # mu 自带 mu4e.el；isync 提供 mbsync
mu --version                   # ≥ 1.10
mbsync --version               # ≥ 1.5
```

### 2.2 Gmail 侧准备

1. **启用 IMAP**：Gmail 设置 → "转发和 POP/IMAP" → 启用 IMAP。
2. **生成应用专用密码**：账户 → 安全性 → 两步验证 → 应用专用密码（mu4e 不能直接走 OAuth2，最稳是 app password）。
3. **建 label `Newsletters`** + 过滤器：
   - `from:(substack.com OR mail.beehiiv.com OR ghost.io OR convertkit-mail2.com OR …)` → Apply label `Newsletters` + Skip Inbox。
   - 也可手动：可疑发件人随手 → Filter messages like these → 标 `Newsletters`。

### 2.3 `~/.authinfo.gpg`（密码存储）

```text
machine imap.gmail.com login your.address@gmail.com password <app-password> port 993
machine smtp.gmail.com login your.address@gmail.com password <app-password> port 587
```

```bash
gpg --encrypt --recipient <your-key> ~/.authinfo
```

### 2.4 `~/.mbsyncrc`（**只**同步 Newsletters label）

```text
IMAPAccount gmail-newsletter
Host imap.gmail.com
User your.address@gmail.com
PassCmd "auth-source-pass-show gmail-app-password"   # 或直接走 .authinfo.gpg
TLSType IMAPS
AuthMechs LOGIN

IMAPStore gmail-newsletter-remote
Account gmail-newsletter

MaildirStore gmail-newsletter-local
Path ~/Mail/newsletter/
Inbox ~/Mail/newsletter/Inbox/
Subfolders Verbatim

Channel gmail-newsletter
Far :gmail-newsletter-remote:
Near :gmail-newsletter-local:
Patterns "Newsletters"   # 仅同步这一个 label
Create Both
Expunge Both
SyncState *
```

```bash
mkdir -p ~/Mail/newsletter
mbsync -a                      # 首次拉取
mu init --maildir=~/Mail --my-address=your.address@gmail.com
mu index
```

> ⚠ 这一段**不入仓库**，仅在本文档内作为操作清单。

---

## 3. Emacs 仓库改动

### 3.1 [lisp/init-custom.el](file:///Users/henripogatrain/henri.emacs.d/lisp/init-custom.el) — 新增 5 个 `defcustom`

```elisp
(defcustom henri-enable-mu4e nil
  "Enable mu4e newsletter intake module."
  :type 'boolean :group 'henri-core)

(defcustom henri-mail-directory
  (expand-file-name "Mail" (getenv "HOME"))
  "Local Maildir root (mbsync 同步目标)."
  :type 'directory :group 'henri-runtime)

(defcustom henri-mail-newsletter-subdir "newsletter"
  "Maildir 下专用于 newsletter 的子目录名."
  :type 'string :group 'henri-runtime)

(defcustom henri-miaoyan-notes-directory
  "/Users/henripogatrain/Documents/TolariaHub/MiaoYan-Notes"
  "MiaoYan-Notes 本地仓库根目录（newsletter 摘要落点的母仓库）."
  :type 'directory :group 'henri-runtime)

(defcustom henri-miaoyan-clippings-inbox
  nil
  "MiaoYan-Notes 的 Clippings/inbox 绝对路径；nil 时由根目录派生."
  :type '(choice (const :tag "Derive from miaoyan root" nil) directory)
  :group 'henri-runtime)
```

> 个人路径仍允许 override，但默认值已就位，遵循 AGENTS.md 第 2 条 "用 expand-file-name 派生"。

### 3.2 新增模块 [lisp/managing/init-mu4e.el](file:///Users/henripogatrain/henri.emacs.d/lisp/managing/init-mu4e.el)

按本仓库现有 `init-managing.el` 的 `use-package` 风格：

```elisp
;;; init-mu4e.el --- Newsletter intake via mu4e -*- lexical-binding: t -*-

;;; Commentary:
;; 仅在 `henri-enable-mu4e' 为非 nil 时由 init-managing 加载。
;; 与 MiaoYan-Notes 通过文件系统解耦：导出邮件正文为 markdown
;; (含 frontmatter)，落点 `henri-miaoyan-clippings-inbox`，
;; 由 MiaoYan-Notes 仓库内的 summarize_inbox.py 接力完成 AI 摘要。

;;; Code:
(require 'init-custom)

(defun henri-mu4e--clippings-inbox ()
  (or henri-miaoyan-clippings-inbox
      (expand-file-name "Clippings/inbox" henri-miaoyan-notes-directory)))

(use-package mu4e
  :ensure nil                            ;; 由 brew install mu 提供
  :commands (mu4e mu4e-update-mail-and-index)
  :init
  (setq mail-user-agent 'mu4e-user-agent)
  :config
  (let ((nl (concat "/" henri-mail-newsletter-subdir)))
    (setq mu4e-maildir              (expand-file-name henri-mail-directory)
          mu4e-get-mail-command     "mbsync -a"
          mu4e-update-interval      nil               ;; 手动 U
          mu4e-change-filenames-when-moving t          ;; mbsync 必须
          mu4e-view-show-images     t
          mu4e-view-prefer-html     nil                ;; newsletter 默认看纯文本
          mu4e-html2text-command    'mu4e-shr2text
          mu4e-attachment-dir       (expand-file-name "attach" henri-var-directory)
          mu4e-bookmarks
          `((:name "Newsletter · Unread"    :key ?n
             :query ,(format "maildir:%s/Inbox AND flag:unread" nl))
            (:name "Newsletter · This week" :key ?w
             :query ,(format "date:7d..now AND maildir:%s" nl))
            (:name "Newsletter · AI"        :key ?a
             :query ,(format "maildir:%s/AI" nl))
            (:name "Newsletter · Read later":key ?r
             :query ,(format "maildir:%s/ReadLater" nl)))))

  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)

  ;; -- 导出当前邮件到 MiaoYan-Notes/Clippings/inbox ----------------
  (defun henri/mu4e-export-to-miaoyan ()
    "Dump current message body as a markdown file into MiaoYan inbox."
    (interactive)
    (let* ((msg     (mu4e-message-at-point))
           (subj    (or (mu4e-message-field msg :subject) "untitled"))
           (date    (format-time-string "%Y-%m-%d"
                                        (mu4e-message-field msg :date)))
           (from    (car (mu4e-message-field msg :from)))
           (sender  (or (plist-get from :name)
                        (plist-get from :email) "unknown"))
           (sender-mail (or (plist-get from :email) ""))
           (slug    (downcase
                     (replace-regexp-in-string
                      "[^[:alnum:]-]+" "-"
                      (replace-regexp-in-string "\\`-+\\|-+\\'" "" subj))))
           (dir     (henri-mu4e--clippings-inbox))
           (file    (expand-file-name (format "%s-%s.md" date slug) dir))
           (body    (or (mu4e-message-field msg :body-txt)
                        (with-temp-buffer
                          (insert (or (mu4e-message-field msg :body-html) ""))
                          (shr-render-region (point-min) (point-max))
                          (buffer-substring-no-properties
                           (point-min) (point-max))))))
      (unless (file-directory-p dir) (make-directory dir t))
      (with-temp-file file
        (insert (format
                 "---\ntitle: %S\nsource: \"mailto:%s\"\nauthor: %S\ndate: %s\ntags:\n  - newsletter\n---\n\n# %s\n\n%s\n"
                 subj sender-mail sender date subj body)))
      (message "[henri] 已导出 → %s" file)))

  (define-key mu4e-headers-mode-map (kbd "C-c m e")
              #'henri/mu4e-export-to-miaoyan)
  (define-key mu4e-view-mode-map    (kbd "C-c m e")
              #'henri/mu4e-export-to-miaoyan)

  ;; -- 远端触发 MiaoYan 摘要（可选；纯异步 make-process） ----------
  (defun henri/miaoyan-run-summarize (&optional limit)
    "在 MiaoYan-Notes 仓库内异步跑一次 summarize_inbox.py。"
    (interactive "P")
    (let* ((default-directory
            (file-name-as-directory
             (expand-file-name henri-miaoyan-notes-directory)))
           (cmd (format "python workflows/scripts/summarize_inbox.py%s --provider claude"
                        (if limit (format " %d" (prefix-numeric-value limit)) ""))))
      (compile cmd)))

  (with-eval-after-load 'which-key
    (which-key-add-key-based-replacements "C-c m" "+mail/mu4e")))

(provide 'init-mu4e)
;;; init-mu4e.el ends here
```

### 3.3 [lisp/init-managing.el](file:///Users/henripogatrain/henri.emacs.d/lisp/init-managing.el) — 条件加载

```elisp
(when henri-enable-mu4e
  (require 'init-mu4e))
```

### 3.4 [lisp/ops/doctor.el](file:///Users/henripogatrain/henri.emacs.d/lisp/ops/doctor.el) — 新增检查项

- `mu` 二进制存在且版本 ≥ 1.10
- `mbsync` 二进制存在
- `henri-mail-directory` 可写
- `henri-miaoyan-notes-directory` 存在且其下有 `Clippings/inbox`
- `henri-miaoyan-notes-directory/workflows/scripts/summarize_inbox.py` 可见

未启用 `henri-enable-mu4e` 时全部跳过。

### 3.5 文档同步（AGENTS.md 第 5 条）

- [docs/specs/ARCHITECTURE.md](file:///Users/henripogatrain/henri.emacs.d/docs/specs/ARCHITECTURE.md)：在 managing 段加 "`init-mu4e.el` — newsletter intake (条件加载，`henri-enable-mu4e`)；与 MiaoYan-Notes 通过 `Clippings/inbox/*.md` 解耦"。
- [docs/tutorials/keybindings.md](file:///Users/henripogatrain/henri.emacs.d/docs/tutorials/keybindings.md)：新增 `C-c m e` = 导出到 MiaoYan、`C-c m s` = 触发 summarize。
- README **不**改（继续只放前缀速记）。
- `c.md`：新增一行 `H mu4e/newsletter intake — 见 docs/jobs/mu4e-newsletter-intake.md`。

---

## 4. Phase 计划

| Phase | 任务 | 验收 |
|-------|------|------|
| **P1 配置骨架** | §3.1 + §3.2 + §3.3 | `henri-enable-mu4e nil` 时启动无警告；`t` + 缺 `mu` 时 doctor 报红但 init 不崩 |
| **P2 系统拉通** | §2 完成；`mbsync -a` + `mu init` + `mu index` 全跑通 | `M-x mu4e` 见到 Newsletter · Unread 有数 |
| **P3 导出契约** | §3.2 `henri/mu4e-export-to-miaoyan` 落地 | 导出 1 封测试邮件，`summarize_inbox.py 1` 成功消费 |
| **P4 doctor + 文档** | §3.4 + §3.5 | `M-x henri/doctor` 全绿；keybindings/architecture 同步 |
| **P5 稳定期** | 连续两周日常使用 | 无前缀冲突、无丢件、Maildir 体积可接受 |

---

## 5. 决策记录

| # | 选择 | 理由 |
|---|------|------|
| 1 | mu4e 而非 notmuch（首发） | mu4e 客户端体验更熟，且 Maildir 不绑定客户端，未来零成本切 notmuch |
| 2 | 不在 Emacs 内做 LLM 摘要 | MiaoYan-Notes 已有成熟 `summarize_inbox.py` 流水线；避免双源真相 |
| 3 | 文件系统握手而非 IPC/HTTP | MiaoYan-Notes 入口本就是 `Clippings/inbox/`；最小契约 = `title + source` 即可被消费 |
| 4 | 默认 `mu4e-view-prefer-html nil` | newsletter HTML 太脏；需要漂亮排版时一键 `View in browser` |
| 5 | `henri-enable-mu4e` 默认 `nil` | 仓库其它机器无 `mu` 时启动不报错；遵循本仓库"按需启用"惯例 |
| 6 | 不接管 SMTP 发信 | newsletter 几乎不回信；保持 mu4e 模块只读，降低复杂度 |

---

## 6. 风险与回退

- **app password 失效**：Gmail 侧重新生成；`.authinfo.gpg` 内更新。
- **mbsync UID validity 错乱**：`rm ~/.mbsync/* && rm -rf ~/Mail/newsletter && mbsync -a` 全量重拉；不影响远端。
- **mu index 损坏**：`mu reset && mu index`。
- **完全回退**：`(setq henri-enable-mu4e nil)` 即可移除全部行为；MiaoYan-Notes 端不受影响。

---

## 7. 后续可拓展（**非本 job 目标**）

- 导出时附带"邮件原文 HTML"作为 `_raw.html` 兄弟文件，给 MiaoYan-Notes 做更高保真摘要。
- 第二条信息流（GitHub 通知、邮件列表）共用同一管道，bookmark 增加 `maildir:/notifications`。
- 评估切换到 notmuch 作为"邮件资料库后端"，mu4e 仅留作阅读 UI。
