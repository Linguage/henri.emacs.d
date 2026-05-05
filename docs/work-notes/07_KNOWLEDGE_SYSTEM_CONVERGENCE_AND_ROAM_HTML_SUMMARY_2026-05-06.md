---
title: "知识系统收敛、Roam HTML 默认主题与跨线流转入口完善"
date: "2026-05-06"
created: "2026-05-06 00:00:00 +0800"
category: "worknotes"
tags:
  [
    "emacs",
    "elisp",
    "org-mode",
    "org-roam",
    "org-html",
    "journal",
    "academic",
    "citar",
    "documentation",
    "workflow",
    "session-summary",
  ]
---

# 知识系统收敛、Roam HTML 默认主题与跨线流转入口完善

本轮工作继续围绕 `henri.emacs.d` 的 Writing 系统推进，但重点已经从“继续加模块”转向“让已有系统互相对得上”。这一系列会话先处理了 Org HTML 默认主题与 Roam 导出的外观一致性问题，随后补齐 Org-roam 的模板提示、教程与时间元数据，最后根据一份结构性评审，对 Roam / Journal / Academic / Citar 的边界、联动方式、初始化副作用和过期兼容层做了一轮系统收敛。

## 1. 背景与动机

在上一阶段完成快捷键治理、Writing 文档分层和 Markdown 预览修复之后，真正开始使用知识系统时又暴露出另一组更细的摩擦：

1. Org HTML 的默认主题虽然已经能工作，但 Journal 专用主题过重，不适合作为普通 Org 导出的默认外观。
2. Roam 节点虽然已有用途目录、capture 模板和 daily 工作流，但知识入口、正式节点、Academic 阅读卡、Journal 月度文件之间仍然缺少可操作的流转通道。
3. 配置层还有几处双真源与历史兼容残留：例如 Citar notes 的真源会在 `Academic/Reading/` 与 `Roam/references/` 之间分裂，Roam 启动时会主动建目录，Academic 里保留了已经过期的旧 Org-roam 兼容变量和命令。

因此本阶段的实际目标不再是再加一批新模板，而是让“默认行为、导出外观、知识流转、文档说明、代码副作用”这几条线重新对齐。

## 2. Henri Notes 默认主题与 Roam HTML 导出修正

本系列先完成了普通 Org HTML 默认主题的拆分与落地。原先默认主题沿用 `Henri Journal`，但它的页头、日期拆分、Activity History 和浮窗日历都紧耦合 Journal 月度页面，不适合作为普通 Org 文档和 Roam 节点的通用 HTML 外观。

为此新增了 `Henri Notes` 主题，保留 Journal 主题里的阅读排版、代码块、表格、明暗切换等基础视觉语言，但移除了 Journal 专属结构，并改为：

- 通用页头：`Org Notes` kicker、标题、作者/日期元信息、主题切换按钮；
- 右下角 `Contents` 按钮和浮窗 TOC；
- 普通 Org 文档默认走 `Henri Notes`，Journal 文件若显式声明 setupfile，仍继续使用 `Henri Journal`。

这一部分还顺手解决了一个路径治理问题：`lisp/writing/org/org-html-themes` 原本在父仓库中是 gitlink 形式，导致新主题文件放进去后不受父仓库版本控制。后续已将该目录转换为由当前仓库统一管理的普通文件树，并把 `Henri Notes` 一并纳管。

在 Roam HTML 导出层面，又补了一次关键修正。最初 `hello_org_roam_note.org` 这类节点导出后，会把 `:ID:`、`:CREATED:`、`:UPDATED:` 当成正文显示。排查后确认原因不是单一模板问题，而是两层叠加：

1. 旧 Roam 文件的 property drawer 位于 `#+title` 之前，Org 会把这类内容导出成正文段落；
2. `Henri Notes` 前端 JS 最初遇到 TOC 后就提前停止扫描，导致即使已经写了“隐藏 metadata”逻辑，也命不中真实 DOM。

修复策略最终分成三层：

- Roam 新模板把 `#+title` / `#+filetags` 放到前面，property drawer 放后面；
- HTML 导出前，在临时 export buffer 中移除文件级 metadata，不改源文件；
- `Henri Notes` 的前端兜底逻辑改为跳过 TOC 后继续扫描 metadata 段落。

这样既处理了新节点，也兼容了旧节点的导出行为。

## 3. Org-roam 使用语义与节点工作流补强

在进入更大的系统收敛之前，Roam 本身也做了一轮可用性补强。

首先是模板体系。原本普通节点模板只有 `n/r/p/m/e`，`daily` 属于另一套 dailies 模板，`inbox` 目录虽然存在但没有独立 template key，导致用户从文档上看得到 `inbox/` 和 `daily/`，在实际 `template key` 输入时却找不到对应入口。现在已经补齐为：

- 普通节点：`n/i/r/p/m/e`
- daily：`d`

并且 `C-c n ?` 不再只展示普通节点模板，而是会分开显示 regular templates 与 daily templates。

其次是 Roam 教程与工作流文档。新增并扩写了 `docs/tutorials/org-roam-guide.md`，把“什么内容放哪里、什么时候抽成节点、如何建立关联、Map 怎么用、daily 与 Journal 的边界是什么”这类原本分散在对话里的经验，整理成可以独立阅读的操作手册。与之配套，`writing-system-guide.md`、`keybindings.md` 和 README 的文档导航也一并更新。

最后是 Roam 文件时间元数据。新建节点时会写入 `CREATED/UPDATED`，后续保存时只刷新 `UPDATED`。在本阶段末尾，这一逻辑又被进一步重写为更标准的 Org API 路径：不再手写裸 `:KEY:` 行扫描，而是使用标准 property drawer 与 `org-id-get-create` / `org-entry-put`，同时把 `before-save-hook` 从全局挂钩改为 Org buffer-local。

## 4. 依据评审做的系统收敛

本系列的后半段，工作重点从“让 Roam 好用”转向“让四条写作主线互相看得见，但不互相接管”。

### 4.1 Roam 内部收口

Roam capture 模板去掉了手写 `:ID:`。原先模板里显式写 `%(org-id-new)`，这与 org-roam 自身的文件 ID 生成路径重叠，容易造成将来解析与上下文不一致。现在新建节点仍然有 ID，但由 org-roam / org-id 自己生成。

同时又引入了轻量生命周期标签：普通节点默认加 `:seedling:`，并新增 `henri-org-roam-cycle-lifecycle-tag`（`C-c n v`）在 `seedling -> budding -> evergreen` 间循环切换。这不是自动知识评估器，而是把“三次出现原则”和节点成熟度变成一个明确、可操作的状态标记。

Roam daily 模板里的 `* 想法` 也被改成了 `* 想法 (会沉淀为 notes)`，用文案直接提醒它是知识入口，而不是 Journal 式的人生流水。

### 4.2 跨线流转命令

评审中最有价值的一点，是指出四条线的边界虽然写清楚了，但“流转”仍然只存在于文档说明里。本阶段落地了两个最小命令：

- `henri-org-roam-extract-pending`（`C-c n E`）：
  在 Roam daily 或 inbox 的当前 heading 上调用，输入目标 note 标题后，会创建 `Roam/notes/${slug}.org`，复制当前 subtree 的正文内容，给新 note 加来源链接，并在原 heading 下留下指向新 note 的回链。
- `henri/today-summary`（`C-c o s`）：
  一次打开当天 Journal day subtree、当天 Roam daily 与今日 Agenda dashboard，形成经历 / 知识入口 / 任务的三栏复盘入口。

此外，Journal capture 在从 Roam daily buffer 发起时会给出提示，提醒当前内容如果是知识线索，优先继续写在 Roam daily 中，减少 Journal 与 Roam daily 的无意识重叠。

### 4.3 Academic / Citar / Roam 真源单点化

Academic 侧的最大收口是把 Citar notes 的真源变成一个真正单点决策。

此前虽然文档里说“默认 Academic/Reading，开启 `henri-org-roam-enable-citar-integration` 时切换到 `Roam/references/`”，但代码里 `citar-notes-paths` 与 `citar-org-roam-subdir` 实际上各管各的。现在已经改成：

- 默认：`Academic/Reading/` 是正式文献阅读卡与 Citar notes 的唯一真源；
- 开启 `henri-org-roam-enable-citar-integration`：`citar-notes-paths` 与 literature notes 目录同时切换到 `Roam/references/`。

与此同时，Reading card 现在会写文件级 `:ID:`，并新增 `* 相关节点` 小节提示使用 `C-c n i`。Academic 不再试图覆盖 `org-roam-directory`，而是只把 `Academic/Reading/*.org` 作为 `org-roam-extra-files` 纳入图谱。这样正式文献阅读卡默认能被 Roam 看到，但不会把整个 Academic 目录都变成 Roam 内容池。

### 4.4 配置卫生与历史兼容清理

Academic 中已经过期的兼容层被正式移除，包括：

- `org-academic-org-roam-dir`
- `org-academic-enable-org-roam`
- `org-academic-create-research-note`

并新增一份 `docs/legacy/2026-05-05-academic-roam-compat-removal.md` 记录这次兼容层收束的边界。

与此同时：

- `org-academic-literature-notes-dir` 从容易快照失真的 `defcustom` 值改为动态函数；
- `org-journal.el` 的 capture templates 不再用裸 `append`，而是按 key 覆盖注册，避免重复 require 后累积同 key 模板；
- `init-org.el` 的缩进统一为空格，保持 Roam 先于 Academic 的加载顺序；
- Org face 唯一真源重新收口为 `henri/apply-org-faces`，移除了 `doom-themes-org-config` 在 `org-base.el` / `visual-themes.el` 中的重复参与。

## 5. 验证与已确认结果

本系列已实际运行并确认过的检查包括：

- `org-html.el`、`org-roam-henri.el`、`org-academic.el`、`org-journal.el`、`org-base.el`、`init-org.el` 的 batch 加载。
- `Henri Notes` 主题的 JS 语法检查、默认导出、显式 Journal/Bearblog/ReadTheOrg 导出不被覆盖。
- `hello_org_roam_note.org` 的实际 HTML 导出检查：正文不再泄漏 `:ID:` / `:CREATED:` / `:UPDATED:`。
- Roam template key 检查：regular 模板为 `n/i/r/p/m/e`，daily 为 `d`。
- Roam metadata 维护：新模板不再手写 `:ID:`，已有 `CREATED` 保持不变，`UPDATED` 正常刷新。
- `henri-org-roam-extract-pending`：可从 daily heading 生成 note，并在源 heading 与目标 note 之间留下双向链接。
- 生命周期标签切换：`C-c n v` 可从 `seedling` 切到 `budding` 与 `evergreen`。
- Academic / Citar：默认 notes path 指向 `Academic/Reading/`；开启 Roam integration 后切到 `Roam/references/`；Reading card 模板包含 `:ID:` 与 `* 相关节点`。
- Journal / Agenda：重复加载 `org-journal.el` 不会累积重复 capture key；`henri-roam-as-agenda-files` 关闭时不纳入 Roam projects，打开后会纳入。
- `henri/today-summary` 的命令与快捷键绑定存在。
- `git diff --check` 通过。

尚未在本轮中完成的验证主要是交互层点验：例如 `henri/today-summary` 的三栏布局虽然已能打开对应命令入口，但还没有在真实图形界面里做长期使用级别的窗口体验调整。

## 6. 文档与当前状态

文档层已经与当前实现重新对齐：

- `README.md`：补入 `C-c o s` 的入口提示；
- `docs/tutorials/keybindings.md`：加入 `C-c n E`、`C-c n v`、`C-c o s`；
- `docs/tutorials/writing-system-guide.md`：补充生命周期标签、Roam project 可选进入 agenda、Reading card 进入 Roam 图谱、Citar toggle 语义；
- `docs/tutorials/org-roam-guide.md`：加入 `daily/inbox -> note` 抽取命令与成熟度标签说明；
- `docs/specs/ARCHITECTURE.md`、`ROADMAP.md`：把 Reading card 进 Roam 图谱、Citar 真源切换、Journal / Roam daily 交叉入口标成已落地；
- `docs/legacy/`：新增兼容符号移除记录，并更新 legacy 索引。

到这一阶段，Writing 系统的整体状态已经从“模块各自能工作”推进到“默认行为、文档说法、快捷键入口、跨线流转和数据真源基本一致”。这并不意味着系统已经完全定型，但它至少已经脱离了“继续用之前先回忆一遍历史上下文”的状态。

## 7. 剩余事项与边界

本系列还留着几件明确未完成的事：

1. `henri/today-summary` 的窗口布局还没有做长期交互打磨，例如是否要固定三栏宽度、是否自动聚焦 Journal 或 Roam daily。
2. 生命周期标签目前是轻量人工切换，没有引入自动统计反链、map 引用或复用次数的机制。
3. Reading card 作为 `org-roam-extra-files` 进入图谱的路径已打通，但“Reading card 自动插入到某个 Roam map / project”的更高层流转仍没有做自动化。
4. work-notes 与 reports 的进一步沉淀还需要后续分流，尤其是这轮关于“真源单点化”和“跨线流转”的经验，未来可能值得抽成一篇 report。

因此更准确的结论不是“知识系统已经最终完成”，而是：这一系列工作把默认主题、Roam 节点、Academic 阅读卡、Journal 入口、Agenda 可见性、文档说明和历史兼容层拉回到了同一套治理叙事里，为后续更细的使用优化打好了底。
