---
title: "Journal 花销账单、每日状态与大额收支管理"
date: "2026-05-23"
created: "2026-05-23 16:08:15 +0800"
category: "worknotes"
tags:
  [
    "emacs",
    "elisp",
    "org-mode",
    "journal",
    "expense",
    "pdf-export",
    "documentation",
    "session-summary",
  ]
---

# Journal 花销账单、每日状态与大额收支管理

本轮工作围绕 Journal 的日常记录继续向“可结算、可回看”的方向推进。起点是确认默认目录中的 `cost.org` 是否已经承担花销汇总职责，最终落地为三组功能：月度花销账单、每日状态前置模板，以及大额收支管理。

这次改动的边界比较清楚：原始 Journal 仍然保持普通 Org 表和日记文本，不改写历史月记；账单和大额收支作为派生结果生成到 `Journal/bills/` 下；旧的 `cost.org` 保留为历史草稿，不再作为新流程入口。

## 1. 背景与动机

最初的问题是：Journal 中的每日消费是否已有整理汇总能力。代码检查发现，`lisp/writing/org/org-journal.el` 已经在 capture 模板中提供了 `花销记录` 表，但没有现成的月度汇总命令。

默认 Journal 目录中的 `cost.org` 确实存在，不过它更像早期的 Org columnview 草稿，依赖 `%ITEM / %AMOUNT / %CATEGORY` 这类 property/column 格式。当前真实日记使用的是普通 Org 表，表头是 `项目 / 金额 / 类别`，两者并不兼容。因此本轮选择以 `journal-YYYY-MM.org` 为唯一源数据，生成独立账单文件。

这个判断保留了现有写作习惯：每天照常在 Journal 里记账，汇总能力作为一个“重建结果”存在，随时可以覆盖生成。

## 2. 月度花销账单

新增的主命令是 `henri/journal-expense-generate-current-month`，绑定到 `C-c j e`。它默认读取当前月的 `journal-YYYY-MM.org`，生成 `Journal/bills/bill-YYYY-MM.org`。另有 `henri/journal-expense-generate-month` 用于选择月份，以及 `henri/journal-expense-regenerate-all` 用于批量重建所有月度账单。

账单相关路径和识别规则也做成了配置项：

| 配置 | 作用 |
|------|------|
| `henri-journal-expense-bills-directory` | 账单输出目录，默认是 Journal 下的 `bills/` |
| `henri-journal-expense-heading-regexp` | 花销 heading 匹配规则，默认匹配包含“花销记录”的标题 |
| `henri-journal-large-transaction-threshold` | 大额收支阈值，默认单笔金额大于 300 |
| `henri-journal-large-transactions-file` | 全局大额收支文件 |

解析规则尽量贴近日常记录方式：只读取 Org 表格，按 `项目 / 金额 / 类别` 表头识别列；空行、分隔线、模板空白行会被跳过；整数和小数金额都能计入；无法解析的金额会跳过并在生成结果中汇报数量。

日期来自所在 day heading，例如 `** 2026-05-15 Friday`。如果花销表位于 `昨日花销记录` heading 下，则归属到前一天。这一点保留了“今天补记昨天账”的使用习惯。

生成文件包含以下部分：

1. `* 明细`：日期、项目、金额、类别、来源。
2. `* 按项目汇总`：按早餐、午餐、购物等项目汇总。
3. `* 按类别汇总`：按 KFC、乡村基等类别或商家汇总。
4. `* 每日汇总`：按日期统计当天总额。
5. `* 大额收支`：当月超过阈值或手动录入的大额记录。
6. `* 月总计`：本月花销总金额。

## 3. PDF 导出修复

月度账单生成后，Org 和 HTML 都能正常输出，但第一次导出 PDF 失败。错误不是账单数据本身，而是 LaTeX 环境与 Org 默认导出依赖不匹配。

修复过程分两步：

1. 将账单导出类切到 `ctexart`，避免默认 `article` 路径中缺失 `capt-of.sty` 时直接失败。
2. 为长表格显式加入 `longtable` 支持，并在账单表格前生成 `#+ATTR_LATEX: :environment longtable ...`。

最终账单文件头部会生成适合中文和长表格的导出配置，包括 `#+LATEX_CLASS: ctexart`、`#+LATEX_HEADER: \usepackage{longtable}` 和必要的 Org export options。

真实的 `2026-05` 账单已成功导出 PDF，文件位于 `Journal/bills/bill-2026-05.pdf`。

## 4. 每日状态前置模板

在创建当天第一条 Journal 记录时，现在会自动插入一组每日状态字段。模板只在新的 day heading 第一次创建时插入，之后同一天继续 capture 不会重复。

当前模板包括：

```org
- 天气：温度  ℃；状况
  - [ ] 晴
  - [ ] 阴
  - [ ] 雨
- 睡眠：
  - 质量：
  - 时间长度：
- 体重： kg
- 活动场所：
- 大额收支
  - 收入：项目 金额 类别/备注
  - 支出：项目 金额 类别/备注
```

这里也顺手解决了一个交互问题：原先天气选项写成行内 `[ ] 晴 [ ] 阴`，看起来像 checkbox，但不是 Org mode 能用 `C-c C-c` 切换的 checkbox。改成标准列表项 `- [ ] 晴` 后，`C-c C-c` 可以正常切换为 `- [X] 晴`。

## 5. 大额收支管理

新增的大额收支管理同时支持两类来源。

第一类来自 `花销记录` 表：只要单笔金额超过 `henri-journal-large-transaction-threshold`，就会被识别为 `支出`，来源标记为 `花销记录`。

第二类来自每日状态模板中的手动补充：

```org
- 大额收支
  - 收入：奖金 1000 项目结算
  - 支出：维修 500 电脑
```

解析时，金额前面的文本作为 `项目`，金额后面的文本作为 `类别/备注`。因此上面的两条记录会分别整理为：

| 类型 | 项目 | 金额 | 类别/备注 |
|------|------|------|-----------|
| 收入 | 奖金 | 1000 | 项目结算 |
| 支出 | 维修 | 500 | 电脑 |

当月账单中会出现对应的大额收支表。同时还会维护一个全局文件 `Journal/bills/large-transactions.org`，用于跨月查看所有大额记录。

全局文件不是按月份拆分的多文件方案，而是一张总表。为了避免重建本月账单时破坏历史月份，写入逻辑改成“只替换当前月”：更新时会删除全局表中日期属于本月的旧记录，再追加当前月重新扫描得到的新记录；其他月份保持不变。

## 6. 验证

本轮验证主要分为真实数据验证和临时数据验证。

真实数据方面，运行 `emacs --batch -l init.el` 并调用 `henri-journal-expense-generate-bill`，成功从 `journal-2026-05.org` 生成 `bill-2026-05.org`。之后通过 Org LaTeX export 成功生成 `bill-2026-05.pdf`。

临时数据方面，构造了小型 Journal 文件验证以下行为：

1. 新建 day heading 时每日状态模板只插入一次。
2. 标准天气 checkbox 能通过 `org-ctrl-c-ctrl-c` 切换状态。
3. `昨日花销记录` 下的表格会归属到前一天。
4. 小数金额能正常计入汇总。
5. 空模板行不会进入账单明细。
6. 表格中的大额支出和每日模板中的大额收入、支出都能被收集。
7. 全局大额收支文件更新本月时，会保留其他月份已有记录。

模块级加载也通过了 batch 检查。没有新增正式 ERT 测试文件，本轮主要依靠 batch smoke test 和临时文件验证核心路径。

## 7. 文档同步

同步更新了写作系统文档，补充了以下内容：

1. `docs/tutorials/keybindings.md`：记录 `C-c j e` 生成当前月花销账单。
2. `docs/tutorials/writing-system-guide.md`：说明月度账单路径、生成命令、每日状态模板、大额收支输入格式，以及全局大额收支文件。

README 没有写入具体快捷键，继续遵守“快捷键不进 README”的项目约定。

## 8. 当前状态与剩余事项

当前 Journal 财务功能已经形成了一个可用闭环：每天用普通 Org 表和前置模板记录，月底或随时用命令重建账单，PDF 可导出，大额收支同时进入当月账单和全局总表。

仍然需要注意的地方：

1. 旧日期不会自动回填每日状态模板；模板只作用于新创建的 day heading。
2. 全局大额收支文件的读取假设它保持当前规范表头，手动改乱表结构可能导致旧记录无法正确合并。
3. 目前没有把解析逻辑固化为 ERT 测试，后续如果继续扩展财务规则，应该补上表格解析、昨日日期归属、汇总计算和全局合并的单元测试。
4. 生成出的 `Journal/bills/` 文件位于个人 Notes/Journal 目录，不属于本 Emacs 配置仓库本身。
