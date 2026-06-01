# Writing 系统使用指南

本指南面向日常使用：如何在当前 `henri.emacs.d` 中写任务、写日志、写 Markdown、写 Org，并导出 HTML / PDF / docx。实现入口是 [`lisp/init-writing.el`](../../lisp/init-writing.el)，它装配 Markdown、Org、LaTeX 和 PDF 子模块。

---

## 1. 心智模型

Writing 系统分成五条线：

| 线索 | 主要用途 | 入口 |
|------|----------|------|
| Agenda / GTD | 今日任务、Inbox、项目任务、Someday | `C-c a`、`C-c c`、`C-c o a` |
| Journal | 个人日记、工作记录、学习卡片 | `C-c c d/w/l`、`C-c j d` |
| Org-roam | 长期知识节点、daily、资料、项目地图 | `C-c n f`、`C-c n i`、`C-c n j` |
| Markdown | 普通文章、笔记、预览、导出 | `*.md` 文件内 `C-c m ...` |
| Org 导出 | Org 写作、HTML 主题、PDF / LaTeX | `*.org` 文件内 `C-c m h ...`、`C-c m l ...` |
| Academic | 学术 idea、reading、project、paper | `C-c A n/r/j/p` |

推荐原则：**任务进 agenda，经历进 Journal，长期知识进 Org-roam，成文内容用 Markdown 或 Org**。不要把所有东西都塞进 Journal，否则 agenda 会重新变吵。

快捷键前缀约定：`C-c n` 只服务 Org-roam；`C-c a` 只打开 agenda；Academic 使用大写 `C-c A`；项目文件树走 `C-c f p`；终端走 `C-c w v` 打开 vterm，`C-c w e` / `C-c w E` 打开 eshell。

---

## 2. 目录约定

个人写作根目录由 `henri-notes-directory` 决定，默认是：

```text
~/Documents/EmacsNotes/
```

当前约定目录：

```text
EmacsNotes/
├── agenda/
│   ├── inbox.org
│   ├── tasks.org
│   ├── projects.org
│   └── someday.org
├── Roam/
│   ├── inbox/
│   ├── daily/
│   ├── notes/
│   ├── references/
│   ├── projects/
│   ├── people/
│   └── maps/
└── Journal/
    └── journal-YYYY-MM.org
```

`agenda/*.org` 是主 agenda 的数据源；`Journal/*.org` 默认不混入主 agenda，只在 Journal 专用视图中查看。
`Roam/` 是通用知识库，按用途分目录，主题关系靠链接和标签表达。

---

## 3. Agenda / GTD 工作流

### 3.1 捕获任务

按 `C-c c` 打开 `org-capture`，常用模板：

| 键 | 模板 | 落点 |
|----|------|------|
| `t` | 快速 TODO | `agenda/inbox.org` |
| `p` | 项目 TODO | `agenda/projects.org` |
| `s` | Someday | `agenda/someday.org` |

任务必须写成 Org TODO headline，才会被 agenda 识别：

```org
* TODO 写实验记录
```

Checkbox 只适合局部清单，不是 agenda 任务：

```org
- [ ] 这不会作为 TODO 出现在 agenda
```

### 3.2 查看任务

| 快捷键 | 用途 |
|--------|------|
| `C-c a` | 打开 Org Agenda 分发入口 |
| `C-c o a` | 直接打开今日 GTD Dashboard |
| `C-c o s` | 打开 Journal + Roam daily + Agenda 今日三栏 |
| `C-c o i` | 打开 `inbox.org` |
| `C-c o t` | 打开 `tasks.org` |
| `C-c o p` | 打开 `projects.org` |

`C-c a` 后常用视图：

| 键 | 视图 |
|----|------|
| `d` | 今日 Dashboard：今日安排、DOING、WAITING、Inbox |
| `w` | 本周计划 |
| `p` | 项目任务 |
| `i` | Inbox 清理 |
| `j` | Journal 概览 |

### 3.3 推荐节奏

1. 临时想法先进 `C-c c t`。
2. 每天打开 `C-c o a` 看今日 Dashboard。
3. 定期用 `C-c a i` 清理 Inbox，把任务移到 `tasks.org` 或 `projects.org`。
4. 当前正在做的任务切到 `DOING`，卡住的任务切到 `WAITING`。

项目相关内容有两类入口：具体行动项写进 agenda，例如 `C-c c p` 捕获到 `agenda/projects.org`；项目目标、背景、路径、关键节点写成 Roam project node，例如 `C-c n c p` 捕获到 `Roam/projects/`。

---

## 4. Journal 工作流

Journal 用来记录“经历”，不是任务系统本身。按 `C-c c` 后使用：

| 键 | 模板 | 内容 |
|----|------|------|
| `d` | 个人日记 | 今日要点、花销记录 |
| `w` | 工作记录 | 工作任务描述、要点、待办记录 |
| `l` | 学习卡片 | 主题、概念、解读、类别 |

所有 Journal 模板写入当月文件：

```text
Journal/journal-YYYY-MM.org
```

当天第一次捕获 Journal 时，会在日期标题下自动插入每日状态列表，包含天气、睡眠质量/时长、体重、活动场所和大额收支补充；同一天后续捕获不会重复插入。

查看与搜索：

| 快捷键 | 用途 |
|--------|------|
| `C-c j d` | 按日期打开当天 Journal |
| `C-c j s` | 搜索 Journal 内容 |
| `C-c j e` | 选择月份生成花销账单；在 `journal-YYYY-MM.org` 中默认选中当前 buffer 月份 |
| `C-c a j` | Journal 概览 |

个人日记里的 `花销记录` 表可以生成独立月账单。默认表格包含 `项目 / 金额 / 类别 / 详情` 四列；账单汇总只读取前三列，`详情` 仅作为日记里的补充说明保留：

```text
Journal/bills/bill-YYYY-MM.org
```

账单包含逐笔明细、按项目汇总、按类别汇总、每日汇总（末行月总计）和当月大额收支；源 journal 文件保持不变。单笔超过 300 的花销会自动进入大额收支，日期前置区的“大额收支”收入/支出也会同步进入：

交互命令会列出已有 `journal-YYYY-MM.org` 月份供选择；如果当前 buffer 是某个月的 Journal 或账单文件，默认月份会优先使用当前文件名中的 `YYYY-MM`。

```text
Journal/bills/large-transactions.org
```

大额收支补充格式示例：

```org
- 大额收支
  - 收入：奖金 1000 项目结算
  - 支出：维修 500 电脑
```

Calendar 中按 `RET` 也可以打开选中日期的 Journal。

---

## 5. Org-roam 知识库

Org-roam 用来承载“会反复回看、会和别的内容发生关系”的长期知识。它不替代 agenda，也不替代 Journal：临时任务仍进 agenda，经历记录仍进 Journal，成熟概念再沉淀为 roam 节点。

Journal 和 Roam daily 的区别很简单：**今天发生了什么、做了什么、感受如何**写 Journal；**今天冒出的概念、线索、待抽取节点**写 Roam daily。前者是经历档案，后者是知识入口。

详细分类、节点建立、链接方法和每周复盘流程见 [Org-roam 日常使用手册](org-roam-guide.md)。本节只保留 Writing 系统里的边界和入口速查。

### 5.1 目录定位

| 目录 | 用途 |
|------|------|
| `Roam/inbox/` | 临时想法、网页摘录、待整理材料 |
| `Roam/daily/` | 每日笔记，承接当天流水和待抽取节点 |
| `Roam/notes/` | 长期概念节点 |
| `Roam/references/` | 书、论文、文章、视频、网页等资料节点 |
| `Roam/projects/` | 有明确产出的项目节点 |
| `Roam/people/` | 人物节点 |
| `Roam/maps/` | MOC / Map of Contents 索引页 |

正式文献阅读卡和 Citar notes 默认不放在 `Roam/references/`，而是放在 `Academic/Reading/`。`Roam/references/` 更适合通用资料节点，比如网页、视频、书目线索、非正式读书摘录。

### 5.2 常用入口

| 快捷键 | 用途 |
|--------|------|
| `C-c n f` | 查找或创建节点 |
| `C-c n i` | 插入节点链接 |
| `C-c n b` | 打开反链 buffer |
| `C-c n c` | 使用 roam capture |
| `C-c n g` | 查看 roam graph |
| `C-c n j` | 捕获今天的 daily note |
| `C-c n t` | 打开今天的 daily note |
| `C-c n y` | 打开昨天的 daily note |
| `C-c n m` | 打开明天的 daily note |
| `C-c n E` | 从 Roam daily / inbox 当前 heading 抽取为 `notes/` 节点 |
| `C-c n v` | 切换当前 Roam 文件生命周期标签 |
| `C-c n d` | 打开 Roam 根目录 |
| `C-c n x` | 打开 Roam inbox 目录 |
| `C-c n ?` | 查看 capture template key |

模块开关在 `M-x customize-group RET henri-writing` 中维护：`henri-org-enable-roam` 控制是否加载 Org-roam；`henri-org-roam-directory` 控制 Roam 根目录；`henri-org-roam-enable-citar-integration` 默认为 `nil`，只有主动希望 Citar 创建 Roam reference 节点时才开启；`henri-roam-as-agenda-files` 默认为 `nil`，打开后会把 `Roam/projects/*.org` 纳入 agenda。

`M-x henri-org-roam-ensure-directories` 可重新创建 `Roam/` 下的用途目录。若本机已安装 `consult-org-roam`，配置会额外启用 `C-c n s/r/l` 用于全文搜索、反链和前向链接。

### 5.3 Capture 模板

`C-c n c` 后可选：

| 键 | 模板 | 落点 |
|----|------|------|
| `n` | 概念笔记 | `Roam/notes/${slug}.org` |
| `i` | 临时收集 | `Roam/inbox/${slug}.org` |
| `r` | 资料笔记 | `Roam/references/${slug}.org` |
| `p` | 项目笔记 | `Roam/projects/${slug}.org` |
| `m` | 索引地图 | `Roam/maps/${slug}.org` |
| `e` | 人物笔记 | `Roam/people/${slug}.org` |

每日笔记走独立模板，template key 是 `d`：`C-c n j` 会写入 `Roam/daily/YYYY-MM-DD.org`，包含“今日记录 / Done / 想法 (会沉淀为 notes) / 待抽取节点”。

创建新节点进入 template key 输入时，配置会在 echo area 显示一行 `n/i/r/p/m/e` 的模板提示；忘记时也可以随时按 `C-c n ?` 查看普通节点模板和 daily 模板。

Roam 文件顶部的 `:PROPERTIES:` 会维护 `:CREATED:` 和 `:UPDATED:`。新建节点时两者都会写入；之后保存 Roam 文件时只刷新 `:UPDATED:`，保留原始创建时间。

普通 Roam 节点默认带 `:seedling:` 标签。用 `C-c n v` 可在 `seedling -> budding -> evergreen` 之间切换，用来标记节点成熟度。

推荐节奏：日常先写 daily；出现三次以上、值得长期复用的想法，在当前 heading 按 `C-c n E` 抽成 `notes/` 节点，并用 `C-c n i` 连接到相关概念或 map。

---

## 6. Markdown 写作

打开 `*.md` 自动进入 `markdown-mode`。核心能力是预览、导出、目录、图片和字数统计。

### 6.1 预览

| 快捷键 | 用途 | 依赖 |
|--------|------|------|
| `C-c m p` | Pandoc 渲染后在 EWW 预览 | `pandoc` |
| `C-c m v` | Pandoc 渲染后在浏览器打开 | `pandoc` |
| `C-c m l` | markdown-mode 实时预览 | markdown-mode |
| `C-c m g` | GitHub 风格预览 | `grip` + `henri-enable-grip` |
| `C-c m c` | 检查 pandoc / grip 依赖 | 无 |
| `C-c m s` | 选择 Markdown HTML 主题 | 本仓库 CSS |

### 6.2 导出

| 快捷键 | 导出 |
|--------|------|
| `C-c m e p` | PDF |
| `C-c m e h` | HTML |
| `C-c m e d` | docx |
| `C-c m e e` | 导出分发面板 |

PDF 导出依赖 `pandoc` 与 `xelatex` 或 `tectonic`；HTML / docx 主要依赖 `pandoc`。

### 6.3 结构与素材

| 快捷键 | 用途 |
|--------|------|
| `C-c m t i` | 插入 Markdown TOC |
| `C-c m t r` | 刷新 TOC |
| `C-c m t d` | 删除 TOC |
| `C-c m o` | 大纲跳转 |
| `C-c m i s` | 插入剪贴板截图 |
| `C-c m w` | 字数统计 |
| `C-c m T i` | 插入 Hugo / Jekyll front-matter |
| `C-c m T n` | 新建带 front-matter 的文章 |

截图会保存到当前 Markdown 文件旁边的 `文件名.assets/` 目录。

---

## 7. Org 写作与导出

Org 基础写作使用 Emacs / Org 默认键即可；本配置额外提供视图、HTML 主题和 PDF 导出。

### 7.1 视图控制

| 快捷键 | 用途 |
|--------|------|
| `C-c o v` | 循环切换 Org 启动折叠级别 |
| `C-c m v b` | 切换 org-bullets |
| `C-c m v s` | 展开所有内容 |
| `C-c m v o` | 只显示大纲 |
| `C-c m v c` | 显示标题结构 |
| `C-c m x` | 插入 checkbox list item |

`M-S-RET` 保留 Org 默认行为：在 plain list 中会新建 checkbox；如果终端或 macOS 没有把这个组合键传给 Emacs，用 `C-c m x`。

### 7.2 HTML 主题

| 快捷键 | 用途 |
|--------|------|
| `C-c m h t` | 选择并写入 HTML 主题 |
| `C-c m h d` | 应用默认主题 |
| `C-c m h e` | 导出并打开 HTML |
| `C-c m h w` | 用指定主题导出但不修改源 buffer |
| `C-c m h s` | 用缩写选择主题 |
| `C-c m h ?` | 查看主题缩写 |
| `C-c m h r` | 移除主题 SETUPFILE |
| `C-c m h k` | 检查本地主题文件 |

主题资源在 [`lisp/writing/org/org-html-themes`](../../lisp/writing/org/org-html-themes)。更多细节见 [html-theme-guide.md](html-theme-guide.md)。

用 `C-c m h t` 设置主题时，源文件只写入 `#+HENRI_HTML_THEME: <主题名>` 关键字，不含绝对路径，可跨设备使用。未指定主题的 Org 文件导出 HTML 时，默认采用 `Henri Notes` 主题；Journal 月度文件（`journal-YYYY-MM.org`）自动使用 `Henri Journal` 主题。两者均在导出时动态解析为当前设备上的 `.setup` 路径。需要临时换主题可用 `C-c m h w`。

### 7.3 PDF / LaTeX

| 快捷键 | 用途 |
|--------|------|
| `C-c m l q` | 快速导出 PDF |
| `C-c m l p` | 用指定主题导出 PDF |
| `C-c m l t` | 应用 LaTeX 主题 |
| `C-c m l d` | 诊断 LaTeX 字体和依赖 |
| `C-c m l r` | 重新加载 LaTeX 配置 |

Journal 月度文件默认使用 `#+LATEX_CLASS: journal`，适合导出带目录的月度记录。

---

## 8. 学术写作

`org-academic-mode` 现在按写作对象区分四类模板，避免所有内容都落进一个过重的 research note：

| 类型 | 定位 | 落点 |
|------|------|------|
| Idea | 灵感、问题、假设，一页轻量卡片 | `Academic/Ideas/` |
| Reading | 文献阅读卡，围绕 citation、方法、结论和可引用片段 | `Academic/Reading/` |
| Project | 长期研究项目页，承载问题、路径、进展和产出 | `Academic/Projects/` |
| Paper | 成文论文草稿，保留完整论文结构和 bibliography | `Academic/Papers/` |

模板默认使用稳定的 `#+LATEX_CLASS: ctexart`，不再内联 `fancyhdr`、`titlesec`、`titletoc` 等重型 LaTeX header。

| 快捷键 | 用途 |
|--------|------|
| `C-c A n` | 创建轻量想法卡 |
| `C-c A r` | 创建文献阅读卡 |
| `C-c A j` | 创建研究项目页 |
| `C-c A p` | 创建学术论文草稿 |
| `C-c A c` | 创建会议摘要 |
| `C-c A P` | 快速创建论文草稿 |
| `C-c A N` | 快速创建想法卡 |
| `C-c A b` | 设置 bibliography |
| `C-c A d` | 学术写作 dashboard |
| `C-c A i` | 插入引用 |

相关路径可通过 `M-x customize-group RET org-academic` 调整。正式文献阅读卡与 Citar notes 默认使用 `Academic/Reading/`，这是文献笔记的唯一真源；Reading card 会写入 `:ID:` 并作为 `org-roam-extra-files` 进入 Roam 图谱。只有将 `henri-org-roam-enable-citar-integration` 打开时，Citar notes 才整体切换到 `Roam/references/`。

---

## 9. 常见检查

| 问题 | 检查方式 |
|------|----------|
| Markdown 不能预览 | `C-c m c`，确认 `pandoc` 在 PATH |
| GitHub 风格预览不可用 | 确认 `henri-enable-grip` 为 `t`，并安装 `grip` |
| Org HTML 主题丢失 | `C-c m h k` 检查主题目录 |
| Org PDF 导出失败 | `C-c m l d` 检查字体、XeLaTeX、latexmk |
| Agenda 太吵 | 确认任务放在 `agenda/*.org`，Journal 只走 `C-c a j` |
| 新任务不出现在 agenda | 确认使用 `* TODO ...`，不是 checkbox |
| Org-roam 节点不显示 | 先执行 `M-x org-roam-db-sync`，再确认文件在 `Roam/` 下且包含 `:ID:` |

推荐第一次配置新机器后执行：

```text
C-c m c      # Markdown 依赖
C-c m h k    # Org HTML 主题
C-c m l d    # Org PDF / LaTeX
C-c o a      # Agenda Dashboard
C-c n f      # Org-roam 节点查找/创建
```
