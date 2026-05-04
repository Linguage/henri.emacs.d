
# org-roam配置指南

## 实施状态

- `lifecycle`: active patch
- `entrypoint`: `lisp/writing/org/org-roam-henri.el`，由 `lisp/writing/init-org.el` 在 `henri-org-enable-roam` 为 `t` 时加载。
- `outputs`: `henri-notes-directory/Roam/`，包含 `inbox/`、`daily/`、`notes/`、`references/`、`projects/`、`people/`、`maps/`。
- `summary`: 通用 Org-roam 知识库独立于 Agenda、Journal、Academic；Academic 只保留 Citar 文献引用轻联动，不再覆盖 `org-roam-directory`。
- `decision`: 2026-05-04 选择 Academic 作为 Citar 文献笔记唯一真源；正式文献阅读卡与 `citar-notes-paths` 默认指向 `Academic/Reading/`，`citar-org-roam` 默认关闭，`Roam/references/` 保留为通用资料节点。
- `smoke`: `emacs --batch -l init.el --eval "(require 'org-roam-henri)"`；`emacs --batch -l init.el --eval "(princ henri-org-roam-directory)"`。
- `freeze rule`: Org-roam batch 加载、目录创建、capture 模板、`C-c n` 快捷键、Academic 不覆盖 roam 根目录、Citar notes 默认指向 `Academic/Reading/` 均验证通过后，本文档可迁入 `docs/legacy/`。

给你拟一套 **“工程师 + 写作 + 读书 + 论文/项目”型 Org-roam 方案**。目标不是把 Emacs 配成知识管理展览馆，而是让你每天能顺手写、顺手找、顺手沉淀。

Org-roam 官方的基本用法就是：指定 `org-roam-directory`，用 `org-roam-node-find` 创建/打开节点，用 `org-roam-node-insert` 插入链接，再靠数据库维护节点、链接和反链；官方示例也建议启用 `org-roam-db-autosync-mode`。([OrgRoam][1])

## 1. 总体原则

你的 Org-roam 不要按学科分目录。不要这样：

```text
roam/
  math/
  history/
  ai/
  emacs/
  railway/
  philosophy/
```

建议按“笔记用途”分：

```text
~/org/roam/
  inbox/
  daily/
  notes/
  references/
  projects/
  people/
  maps/
```

其中：

`inbox/` 放临时想法、语音转写、网页摘录，乱一点没关系。

`daily/` 放每日笔记，承接流水、done list、当天想法。

`notes/` 放长期概念节点，比如“规格设计”“验证机制”“轮轨动力输入”“格罗滕迪克的抽象化”。

`references/` 放书、论文、文章、视频、网页。

`projects/` 放有明确产出的东西，比如博士论文、博客系统、Claude export 静态查看器、Emacs 写作系统。

`people/` 放人物节点，比如费曼、格罗滕迪克、乔布斯、李存勖。

`maps/` 放索引页，也就是 MOC / Map of Contents，比如“AI 编程地图”“博士论文地图”“Emacs 系统地图”。

这里的关键是：**目录只管用途，主题靠链接和标签解决。**

## 2. 推荐文件命名

我建议普通节点用：

```text
${slug}.org
```

比如：

```text
notes/specification-design.org
notes/validation-mechanism.org
references/recoltes-et-semailles.org
projects/thesis-first-draft.org
```

不一定非要时间戳。Org-roam 真正稳定的是节点里的 `ID`，不是文件名。这样文件名可读，Git diff 也舒服。

每个文件开头统一这样：

```org
:PROPERTIES:
:ID:       自动生成
:END:
#+title: 规格设计
#+filetags: :software-engineering:ai-coding:
```

注意：**链接尽量用 `id:` 链接**，不要只靠标题。标题以后会改，ID 不该改。

## 3. 最小可用配置

下面这套可以直接放进你的 Emacs 配置里。假设你用 `use-package`。

```elisp
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/roam/"))

  ;; dailies 单独放 daily/
  (org-roam-dailies-directory "daily/")

  ;; 显示节点时，带上标签和层级，方便查找
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:20}" 'face 'org-tag)))

  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph)

         ;; daily notes
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         ("C-c n m" . org-roam-dailies-goto-tomorrow))

  :config
  (org-roam-db-autosync-mode))
```

几个快捷键你可以先记这四个：

```text
C-c n f   找/建节点
C-c n i   插入节点链接
C-c n b   看反链 buffer
C-c n j   写今天的 daily
```

Org-roam 普通 capture 和 dailies capture 是两套模板，别混在一起；普通节点走 `org-roam-capture-templates`，每日笔记走 `org-roam-dailies-capture-templates`。社区讨论里也常把普通笔记对应 permanent notes，把 dailies 对应 fleeting notes，也就是临时/流动笔记。([Org-roam][2])

## 4. Capture 模板：先用 5 个

我给你设五类，够你现在用：

```elisp
(setq org-roam-capture-templates
      '(("n" "note / 概念笔记" plain
         "%?"
         :target (file+head "notes/${slug}.org"
                            ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :note:\n\n")
         :unnarrowed t)

        ("r" "reference / 资料笔记" plain
         "* Metadata\n- Source:\n- Author:\n- Date:\n- Type:\n\n* 摘要\n%?\n\n* 我的理解\n\n* 相关节点\n"
         :target (file+head "references/${slug}.org"
                            ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :reference:\n\n")
         :unnarrowed t)

        ("p" "project / 项目笔记" plain
         "* 目标\n%?\n\n* 当前状态\n\n* 下一步\n\n* 相关节点\n"
         :target (file+head "projects/${slug}.org"
                            ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :project:\n\n")
         :unnarrowed t)

        ("m" "map / 索引地图" plain
         "* 核心节点\n%?\n\n* 相关项目\n\n* 参考资料\n"
         :target (file+head "maps/${slug}.org"
                            ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :map:\n\n")
         :unnarrowed t)

        ("e" "person / 人物笔记" plain
         "* 简介\n%?\n\n* 关键思想/事件\n\n* 相关节点\n"
         :target (file+head "people/${slug}.org"
                            ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :person:\n\n")
         :unnarrowed t)))
```

我的建议是：**先别加更多模板**。模板一多，写笔记前先想“这条该放哪种模板”，脑子就被打断了。

## 5. Daily 模板：承接流水，但不长期堆垃圾

每日笔记建议这样：

```elisp
(setq org-roam-dailies-capture-templates
      '(("d" "daily" entry
         "* %<%H:%M> %?\n"
         :target (file+head "%<%Y-%m-%d>.org"
                            ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n* 今日记录\n\n* Done\n\n* 想法\n\n* 待抽取节点\n"))))
```

每天可以这样写：

```org
* 今日记录

* Done
- 配置了 org-roam 的基础目录
- 读了《收获与播种》一段

* 想法
- [[id:xxx][规格设计]] 本质上是在定义“什么算做对”
- [[id:yyy][Org-roam]] 更像知识工程底座，不是漂亮笔记软件

* 待抽取节点
- 验证机制
- Emacs 写作系统
```

`待抽取节点` 很重要。它是从 Logseq 式流水，过渡到 Org-roam 长期节点的桥。

## 6. 标签规则：粗一点，别太细

建议你只用粗标签：

```text
:ai:
:software:
:emacs:
:latex:
:writing:
:reading:
:math:
:railway:
:thesis:
:health:
:history:
:project:
:reference:
:daily:
:map:
```

不要搞这种：

```text
:ai-coding-agent-workflow-context-compression:
```

标签太细会变成另一套目录系统，麻烦。

一条笔记 1 到 3 个标签就够了，比如：

```org
#+filetags: :ai:software:writing:
```

## 7. MOC / 索引页：你真正的“主页”

你可以先建 5 个 `maps/`：

```text
maps/emacs-system.org
maps/ai-coding.org
maps/thesis.org
maps/reading.org
maps/life-system.org
```

比如 `maps/ai-coding.org`：

```org
#+title: AI 编程地图
#+filetags: :map:ai:software:

* 核心概念
- [[id:xxx][规格设计]]
- [[id:yyy][验证机制]]
- [[id:zzz][架构判断]]
- [[id:aaa][vibe coding]]

* 工具
- [[id:bbb][Cursor]]
- [[id:ccc][GitHub Copilot]]
- [[id:ddd][Claude Code]]

* 项目
- [[id:eee][Claude Export 静态查看器]]
- [[id:fff][VPN 使用分析 dashboard]]
```

MOC 不是目录，它是你的工作台。
你以后不知道从哪开始，就打开对应地图页。

## 8. 推荐工作流

日常就四步。

第一步，想到什么先写 daily。

```text
C-c n j
```

第二步，遇到重要概念，用 `C-c n f` 创建节点。

比如：

```text
规格设计
验证机制
Org-roam
Emacs 写作系统
铁路扣件系统退化
```

第三步，在节点之间插链接。

```text
C-c n i
```

第四步，每周抽一次 daily，把反复出现的内容沉淀到 `notes/` 或 `projects/`。

你可以用一个很粗的判断：
**出现三次的想法，就该变成节点。**

## 9. 和你现有工作结合

你比较适合先建这些节点。

Emacs / 写作类：

```text
Emacs 写作系统
Org mode
Org-roam
LaTeX 论文写作
BibTeX / citar
博客发布流程
```

AI 编程类：

```text
AI 编程工作流
规格设计
验证机制
架构判断
vibe coding
上下文管理
Claude Export 静态查看器
```

博士/工程类：

```text
铁路扣件系统退化
轮轨动力输入
温度荷载
扣件疲劳
Dahl 摩擦模型
P2 共振
S-N 曲线
Miner 损伤
```

读书类：

```text
格罗滕迪克
收获与播种
梦之钥匙
费曼
乔布斯
科学家传记
抽象化
个人自由
```

生活系统类：

```text
活下去2.0
作息调整
身体状态记录
朗读
深度对话
```

注意，健康相关记录别搞成自我诊断数据库。可以记录体验和行动，但医学判断还是要谨慎。

## 10. Git 备份方案

Org-roam 是纯文本，很适合 Git。

```bash
cd ~/org/roam
git init
git add .
git commit -m "Initial org-roam notes"
```

`.gitignore` 可以先这样：

```gitignore
.DS_Store
*.elc
.#*
#*#
*~
```

数据库文件是否进 Git，看你习惯。通常我建议 **不提交数据库**，因为数据库可以重建：

```gitignore
org-roam.db
```

如果数据库出问题，直接：

```text
M-x org-roam-db-sync
```

有些节点不显示时，常见原因是数据库没同步、文件不在 `org-roam-directory` 下、子目录被 Git ignore 影响，或者模板没有正确生成 ID。社区里也有人遇到“子目录节点不显示”，最后发现和 gitignore 有关；排查时先看目录、同步、ID 这三件事。([Doom Emacs Discourse][3])

## 11. 可选增强包

第一阶段先别装太多。等基础稳定后，可以加这些：

```elisp
(use-package consult-org-roam
  :ensure t
  :after org-roam
  :custom
  (consult-org-roam-grep-func #'consult-ripgrep)
  :config
  (consult-org-roam-mode 1)
  :bind (("C-c n s" . consult-org-roam-search)
         ("C-c n r" . consult-org-roam-backlinks)
         ("C-c n l" . consult-org-roam-forward-links)))
```

这个适合搜索全文、查反链、查前向链接。

如果你后面要接论文，可以再考虑：

```text
citar
citar-org-roam
org-noter
pdf-tools
```

但现在别急。先把 Org-roam 本身跑顺。

## 12. 你的第一周启动计划

第一天：只配置目录、快捷键、模板。

第二天：建 5 个 maps。

```text
Emacs 系统地图
AI 编程地图
博士论文地图
读书地图
生活系统地图
```

第三到第五天：每天只写 daily，不迁移旧资料。

第六天：从 daily 里抽 10 个节点。

第七天：打开每个 map，把这 10 个节点挂进去。

一周后，你的知识库应该长这样：

```text
roam/
  daily/
    2026-05-04.org
    2026-05-05.org
  notes/
    specification-design.org
    validation-mechanism.org
    org-roam.org
    emacs-writing-system.org
  references/
    recoltes-et-semailles.org
  projects/
    thesis-first-draft.org
    claude-export-static-viewer.org
  maps/
    emacs-system.org
    ai-coding.org
    thesis.org
    reading.org
    life-system.org
```

这就已经能用了。

## 13. 最后给你一版完整配置块

你可以先直接复制这一版：

```elisp
;;; org-roam setup

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename "~/org/roam/"))
  (org-roam-dailies-directory "daily/")
  (org-roam-node-display-template
   (concat "${title:*} "
           (propertize "${tags:20}" 'face 'org-tag)))
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n b" . org-roam-buffer-toggle)
         ("C-c n c" . org-roam-capture)
         ("C-c n g" . org-roam-graph)
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n t" . org-roam-dailies-goto-today)
         ("C-c n y" . org-roam-dailies-goto-yesterday)
         ("C-c n m" . org-roam-dailies-goto-tomorrow))
  :config
  (org-roam-db-autosync-mode)

  (setq org-roam-capture-templates
        '(("n" "note / 概念笔记" plain
           "%?"
           :target (file+head "notes/${slug}.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :note:\n\n")
           :unnarrowed t)

          ("r" "reference / 资料笔记" plain
           "* Metadata\n- Source:\n- Author:\n- Date:\n- Type:\n\n* 摘要\n%?\n\n* 我的理解\n\n* 相关节点\n"
           :target (file+head "references/${slug}.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :reference:\n\n")
           :unnarrowed t)

          ("p" "project / 项目笔记" plain
           "* 目标\n%?\n\n* 当前状态\n\n* 下一步\n\n* 相关节点\n"
           :target (file+head "projects/${slug}.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :project:\n\n")
           :unnarrowed t)

          ("m" "map / 索引地图" plain
           "* 核心节点\n%?\n\n* 相关项目\n\n* 参考资料\n"
           :target (file+head "maps/${slug}.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :map:\n\n")
           :unnarrowed t)

          ("e" "person / 人物笔记" plain
           "* 简介\n%?\n\n* 关键思想/事件\n\n* 相关节点\n"
           :target (file+head "people/${slug}.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: ${title}\n#+filetags: :person:\n\n")
           :unnarrowed t)))

  (setq org-roam-dailies-capture-templates
        '(("d" "daily" entry
           "* %<%H:%M> %?\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              ":PROPERTIES:\n:ID:       %(org-id-new)\n:END:\n#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n* 今日记录\n\n* Done\n\n* 想法\n\n* 待抽取节点\n")))))
```

我对这套方案的判断很明确：**先求稳定、低摩擦、可输出。**
Org-roam 配置的第一性原理不是“知识图谱多漂亮”，而是你今天晚上愿不愿意按 `C-c n j` 写两句。

[1]: https://www.orgroam.com/manual.html?utm_source=chatgpt.com "Org-roam User Manual"
[2]: https://org-roam.discourse.group/t/org-roam-capture-templates-vs-org-roam-dailies-capture-templates/2452?utm_source=chatgpt.com "Org-roam-capture-templates vs org-roam-dailies ... - Discourse"
[3]: https://discourse.doomemacs.org/t/org-roam-doesnt-show-nodes-that-are-in-subdirectories/2589?utm_source=chatgpt.com "Org-Roam doesn't show nodes that are in subdirectories"
