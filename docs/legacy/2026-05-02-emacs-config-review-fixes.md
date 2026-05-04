# Emacs 配置审查整改任务

- `状态`: done
- `最近更新`: 2026-05-02
- `lifecycle`: archived
- `entrypoint`: `init.el`
- `outputs`: 本仓库内的 Elisp 配置、文档与资源路径约定
- `summary`: 根据系统审查报告，将当前配置中的硬编码路径、重复定义、外部散落资源、加载机制与文档不一致问题拆成可执行修复任务。
- `smoke`: `emacs --batch -Q --load early-init.el --load init.el`（如实际环境需要联网安装包，可先使用本机交互启动验证）
- `freeze rule`: P0/P1 项完成且能稳定启动；README 与核心文档反映新的路径和模块边界；剩余低风险整理项可迁入后续 job 或 legacy。

**结案摘要（第二轮）**：已移除 `init.el` 中对 `dired` 的 advice 与启动清理 timer；新增 [`lisp/ops/paths.el`](../../lisp/ops/paths.el)（`henri-runtime`）与 `.gitignore` 顶部说明；`backup`/`rime` 与运行时目录对齐；**未引入 `no-littering`**。验证：`emacs --batch` 加载通过。本文档已迁入 `docs/legacy/`。

## 1. 背景

本任务来自一次系统性审查。整体判断是：当前配置的分层方向正确，`early-init.el` / `init.el` / `lisp/` 模块层次清晰，启动优化、大文件降级、LSP 自动格式化阈值与 `defcustom` 集中开关都有良好基础。

主要风险集中在五类：

1. 个人路径硬编码较多，迁移到新机器或 CI 时容易失效。
2. 部分配置资产散落在 `~/Documents/EmacsNotes/`，与仓库内资源目录并存且不一致。
3. 多处重复定义、重复 hook 和重复性能参数设置，实际生效值依赖加载顺序。
4. `load-file` 与自加载模式削弱模块边界、字节编译收益和可维护性。
5. `fix-warnings.el` 与 dashboard/dired 相关防御性补丁过重，可能掩盖真实问题。

## 2. 目标

本 job 的目标是把个人 Emacs 配置整理成更可迁移、更可解释、更容易 debug 的仓库内系统。

验收时应满足：

1. 所有明确点名的个人路径通过 `init-custom.el` 中的 `defcustom` 或仓库内相对路径派生。
2. `org-html-themes` 等配置资产有唯一真相源，不再同时指向仓库内目录和 `~/Documents/EmacsNotes/`。
3. 重复 `use-package`、重复 hook、重复 GC/exec-path 配置已收敛到单一位置。
4. 跨模块加载以 `load-path` + `require` 为主，不再依赖散落的 `load-file`。
5. `fix-warnings.el` 不再大范围屏蔽真实警告。
6. README、`c.md`、`docs/specs/ARCHITECTURE.md` 与实际结构一致。

## 3. 非目标

本 job 暂不追求：

1. 重写整个 Emacs 配置架构。
2. 更换补全框架、主题框架或 LSP 客户端。
3. 迁移到 Doom Emacs、Spacemacs 或其他发行版。
4. 一次性引入复杂测试框架。
5. 清理第三方包目录 `elpa/` 的版本策略，除非它阻塞路径收口。

## 4. 优先级总览

### P0: 先做，风险低且收益高

1. 路径集中化：新增并使用核心 `defcustom`。
2. 重复定义清理：删除明显重复的 package/hook/GC/exec-path 配置。
3. 加载机制收敛：统一 `load-path`，把 `load-file` 改为 `require`。
4. 删除 `org-base.el` 自加载自身的硬编码。

### P1: 第二阶段，影响面中等

1. 统一 `org-html-themes` 位置。
2. 精简 `fix-warnings.el`。
3. 处理 dashboard/dired 防御性补丁。
4. 清理大段注释死代码。

### P2: 收尾与文档

1. 引入或评估 `no-littering`。
2. 明确 `rime/`、`custom.el`、`var/`、`.local/`、`tree-sitter/` 等目录策略。
3. 更新 README 与文档系统。

## 5. 任务拆解

### 进度快照（2026-05-02）

- **5.1 路径集中化**：已在 `lisp/init-custom.el` 的 `henri-paths` 组落地；笔记、Projectile、LeetCode、Conda、Org HTML 主题目录已改为派生路径。
- **5.2 org-html-themes**：默认目录与 `install-themes.sh` 一致（仓库内 `lisp/writing/org/org-html-themes/`）；缺失时 Journal/HTML 会 `message` 提示。
- **5.3 重复配置**：centaur-tabs 合并且修复 `use-package` 结构；`all-the-icons` / `company` / `rainbow-delimiters` / GC / `find-file` 绑定已收敛（C-c f n）；`init-managing` 增加 `(provide 'init-managing)`。
- **5.4 require**：`init.el` 与 `init-programming` / `init-writing` / `org-base` 应急路径已改为 `require`。
- **5.5 fix-warnings**：已瘦身。
- **5.6 dashboard**：已删除 `dired` advice、`dired-mode-hook` 替换与多次 `run-at-time` 清理；保留 `henri/dashboard` + `initial-buffer-choice`；`C-x C-f` 默认、`C-c f n` 进笔记。
- **5.7 注释死代码**：此前已删部分；其余零散注释可在日常编辑中清理。
- **5.8**：`no-littering` **未采用**；由 `paths.el` + 文档说明替代。
- **5.9**：`rime/`、`custom.el`、`var/`、`.local/` 等策略写入 README / c.md / `.gitignore` 注释。
- **5.10 文档**：README / c.md / ARCHITECTURE / ROADMAP 已更新；本 job 归档至 `docs/legacy/`。

### 5.1 路径集中化

状态：done

涉及文件：

- `lisp/init-custom.el`
- `init.el`
- `lisp/init-managing.el`
- `lisp/init-programming.el`
- `lisp/programming_languages/init-python.el`
- `lisp/writing/org/org-journal.el`
- `lisp/writing/org/org-html.el`
- `lisp/writing/org/org-academic.el`

新增建议：

```elisp
(defcustom henri-notes-directory "~/Documents/EmacsNotes/"
  "Root directory for personal notes."
  :type 'directory
  :group 'henri)

(defcustom henri-projects-directory "~/projects/"
  "Root directory for personal projects."
  :type 'directory
  :group 'henri)

(defcustom henri-leetcode-directory "~/leetcode/"
  "Directory used by leetcode.el."
  :type 'directory
  :group 'henri)

(defcustom henri-conda-home "~/miniconda3/"
  "Root directory of the local Conda installation."
  :type 'directory
  :group 'henri)

(defcustom henri-conda-default-env "Henri_env"
  "Default Conda environment name."
  :type 'string
  :group 'henri)
```

执行步骤：

1. 在 `lisp/init-custom.el` 中补齐上述变量。
2. 把 `~/Documents/EmacsNotes/` 改为从 `henri-notes-directory` 派生。
3. 把 `~/projects/` 改为 `henri-projects-directory`。
4. 把 `~/leetcode/` 改为 `henri-leetcode-directory`。
5. 把 `~/miniconda3/`、`~/.conda/envs`、`Henri_env` 改为从 `henri-conda-home` 与 `henri-conda-default-env` 派生。
6. 检查是否还有 `~/Documents/`、`~/projects/`、`~/leetcode/`、`~/miniconda3/`、`Henri_env` 的直接引用。

验收：

```bash
rg 'Documents/EmacsNotes|~/projects|~/leetcode|~/miniconda3|Henri_env' .
```

只允许在文档、注释或默认值定义处出现。

### 5.2 统一 org-html-themes

状态：done

涉及文件：

- `lisp/writing/org/org-html.el`
- `lisp/writing/org/org-journal.el`
- `lisp/writing/org/install-themes.sh`
- `.gitignore`
- `docs/specs/ARCHITECTURE.md`

问题：

当前审查指出主题路径至少有三套来源：

1. 仓库内 `lisp/writing/org/org-html-themes/`
2. 外部 `~/Documents/EmacsNotes/org-html-themes/`
3. `install-themes.sh` 的下载目标

执行步骤：

1. 选择仓库内 `lisp/writing/org/org-html-themes/` 作为唯一安装位置。
2. 调整 `org-html.el` 与 `org-journal.el`，用仓库内路径派生。
3. 调整 `install-themes.sh` 默认目标。
4. 检查 `.gitignore` 对该目录的规则。如果使用 submodule，保留 submodule 入口；如果使用脚本下载，明确忽略下载产物并在 README/c.md 中说明。
5. 删除对 `~/Documents/EmacsNotes/org-html-themes` 的所有代码引用。

验收：

```bash
rg 'org-html-themes|theme-henri-bearblog' lisp .gitignore docs README.md c.md
```

结果应能清楚说明唯一资源位置。

### 5.3 清理重复定义与冲突

状态：done

涉及文件：

- `lisp/init-styling.el`
- `lisp/init-managing.el`
- `lisp/init-programming.el`
- `lisp/programming_languages/init-python.el`
- `lisp/programming_languages/init-lisp.el`
- `init.el`
- `early-init.el`
- `lisp/fix-warnings.el`

待处理清单：

1. `centaur-tabs` 在 `init-styling.el` 中保留一份实现，删除重复的 `centaur-tabs-buffer-groups` / `centaur-tabs-hide-tab`。
2. `all-the-icons` 只保留一处 `use-package`。
3. `company` 全局策略只保留在 `init-programming.el`，Python 模块只做 Python 特化。
4. `rainbow-delimiters` 只保留全局或 Lisp 专用中的一种；若 Lisp 专用更合理，则从 styling 中删除。
5. GC 设置保留 `early-init.el` 的启动期设置与 `init.el` 启动后恢复，删除其他运行期重复设置。
6. `exec-path-from-shell` 只在 `init-managing.el` 配置一份。
7. `menu-bar-mode`、`tool-bar-mode`、`scroll-bar-mode` 只保留一个阶段负责。
8. `eglot` 的 `before-save-hook` 只添加一次，且必须是 buffer-local hook。
9. `byte-compile-warnings` 只保留一处最终策略。

验收：

```bash
rg 'centaur-tabs|all-the-icons|company|rainbow-delimiters|gc-cons-threshold|exec-path-from-shell|before-save-hook|byte-compile-warnings' lisp init.el early-init.el
```

逐项确认每个职责只有一个主位置。

### 5.4 改 `load-file` 为 `require`

状态：done

涉及文件：

- `init.el`
- `lisp/writing/org/org-base.el`
- 所有包含 `(load-file ...)` 的模块

建议入口结构：

```elisp
(dolist (sub '("lisp"
               "lisp/ops"
               "lisp/programming_languages"
               "lisp/writing"
               "lisp/writing/org"
               "lisp/writing/LaTeX"))
  (add-to-list 'load-path (expand-file-name sub user-emacs-directory)))
```

执行步骤：

1. 在 `init.el` 顶部集中设置 `load-path`。
2. 确认每个模块文件末尾有正确 `(provide 'feature-name)`。
3. 将跨模块加载从 `(load-file ...)` 改为 `(require 'feature-name)`。
4. 删除 `org-base.el` 中自加载自身的硬编码。
5. 对语言模块使用 `require`，避免重复读磁盘。

验收：

```bash
rg '\\(load-file|lisp/programming_languages|lisp/writing/org/org-base.el' .
```

除文档或明确的脚本说明外，不应再有配置运行路径中的 `load-file`。

### 5.5 精简 fix-warnings.el

状态：done

涉及文件：

- `lisp/fix-warnings.el`

保留方向：

1. `byte-compile-warnings` 的最终策略。
2. 必要的 `warning-suppress-types`。

删除方向：

1. `display-warning` advice。
2. `display-startup-echo-area-message` 覆盖。
3. 对 `defadvice` obsolete 的屏蔽。
4. 其他会隐藏真实警告的补丁。

验收：

```bash
rg 'advice-add|display-warning|display-startup-echo-area-message|defadvice|warning-suppress-types|byte-compile-warnings' lisp/fix-warnings.el
```

文件应只承担“降低噪音”的有限职责，而不是改写警告系统。

### 5.6 Dashboard 与 find-file 行为整理

状态：done

涉及文件：

- `init.el`
- `lisp/init-managing.el`

问题：

当前 dashboard/dired 相关逻辑偏防御式：多处 `advice-add`、`run-at-time` 与启动 buffer 清理在和默认行为打架。另有 `find-file` remap 重复，且把 `C-x C-f` 永久导向 notes 根目录不符合常规预期。

执行步骤：

1. 决定是否引入 `dashboard` 包；如果不引入，则用更小的 `initial-buffer-choice` 实现。
2. 删除专门“消灭意外 dired 启动 buffer”的复杂 advice 与 timer。
3. `C-x C-f` 恢复常规 `find-file`。
4. 将 notes 专用入口改为 `C-c f n` 或同类显式快捷键。
5. 去重 `henri/find-file-in-notes` 的绑定位置。

验收：

1. 交互启动 Emacs 后首页行为稳定。
2. `C-x C-f` 打开普通路径。
3. notes 快捷键仍能进入 `henri-notes-directory`。

### 5.7 清理注释死代码

状态：done

涉及文件：

- `init.el`
- `lisp/programming_languages/init-python.el`
- `lisp/init-styling.el`

执行步骤：

1. 删除已废弃的大段注释代码块。
2. 保留必要的解释性注释。
3. 如果某段历史逻辑仍有价值，迁入 `docs/legacy/` 或 `docs/reports/`，不要留在运行配置中。

验收：

代码中不再存在大块被注释掉的旧实现。

### 5.8 评估并引入 no-littering

状态：done

涉及文件：

- `init.el`
- `lisp/ops/backup.el`
- 可选新增 `lisp/ops/paths.el`

执行步骤：

1. 先记录当前已收口路径：`var/`、`elpa/`、`tree-sitter/`、`transient/`、`.local/cache`、`.local/etc`。
2. 评估是否引入 `no-littering`。
3. 若引入，将 `recentf`、`projectile`、`url`、`request` 等状态文件统一收进 `var/` 或 `.local/`。
4. 若不引入，则新增 `lisp/ops/paths.el` 统一说明路径策略。

验收：

1. 新增运行时文件不会散落到仓库根目录。
2. `c.md` 与 `ARCHITECTURE.md` 说明路径落点。

### 5.9 Rime 与 custom.el 策略

状态：done

涉及文件：

- `lisp/init-rime.el`
- `.gitignore`
- `README.md`
- `c.md`

执行步骤：

1. 明确 `rime/` 是否应由用户本地生成并忽略。
2. 在 `.gitignore` 中说明 `rime/` 策略（如适用）。
3. 明确 `custom.el` 是否跟踪。如果跟踪，文件内写清“自动生成、可被覆盖”；如果不跟踪，加入忽略并确保配置能处理文件不存在。

验收：

新用户能从 README/c.md 理解 Rime 数据与 custom 配置的位置和版本策略。

### 5.10 README 与文档同步

状态：done

涉及文件：

- `README.md`
- `c.md`
- `docs/specs/ARCHITECTURE.md`
- `docs/specs/ROADMAP.md`

执行步骤：

1. 修正 README 中过期仓库地址。
2. 校对外部依赖列表与实际配置一致性。
3. 加入文档导航，指向 `docs/specs/` 与 `c.md`。
4. 将本 job 完成后的架构变化同步到 `ARCHITECTURE.md`。
5. 将剩余任务更新到 `ROADMAP.md` 或拆成新的 job。

验收：

README 不再与代码明显冲突，新读者能通过 README -> c.md -> docs/specs 找到主路径。

## 6. 建议执行顺序

1. 先做 `5.1 路径集中化`。
2. 再做 `5.3 清理重复定义与冲突`。
3. 再做 `5.4 改 load-file 为 require`。
4. 完成一次交互启动 smoke test。
5. 处理 `5.2 org-html-themes`。
6. 精简 `5.5 fix-warnings.el`。
7. 处理 `5.6 Dashboard 与 find-file 行为整理`。
8. 清理注释死代码。
9. 最后更新 README、`c.md` 与核心 docs。

这个顺序优先降低迁移风险与加载不确定性，并避免在 dashboard/UI 行为仍不稳定时同时改太多用户体验逻辑。

## 7. 验证清单

每个阶段至少执行：

```bash
rg 'Documents/EmacsNotes|~/projects|~/leetcode|~/miniconda3|Henri_env' .
rg '\\(load-file' .
rg 'advice-add|display-warning|before-save-hook|gc-cons-threshold|exec-path-from-shell' init.el early-init.el lisp
```

如本机环境可用，再执行：

```bash
emacs --batch -Q --load early-init.el --load init.el
```

最后进行一次交互启动，确认：

1. 启动页或初始 buffer 正常。
2. `C-x C-f` 行为符合预期。
3. notes 专用入口可用。
4. Org HTML 导出相关主题路径可解析。
5. Python/Conda 配置在默认环境不存在时不会硬失败。

## 8. 结案标准

满足以下条件后，本 job 可移动到 `docs/legacy/`：

1. P0 与 P1 全部完成，P2 至少完成 README/c.md/ARCHITECTURE 同步。
2. 审查报告中明确点名的硬编码路径、重复定义、`load-file`、警告屏蔽问题已逐项处理或有记录说明暂不处理原因。
3. 交互启动与最小 batch smoke 均通过，或记录了受本机环境限制的已知原因。
4. 剩余事项已迁入新的 job 或 ROADMAP，不再混在本任务中。
