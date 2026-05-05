# Org Mode HTML 导出主题指南

本说明梳理 **Org → HTML** 多主题共存的目录约定、分层思路与选择器清单，并给出落地顺序建议。主题资源统一位于仓库 [`lisp/writing/org/org-html-themes`](../../lisp/writing/org/org-html-themes)，路径变量为 `henri-org-html-themes-directory`（见 [`lisp/init-custom.el`](../../lisp/init-custom.el)）。

---

## 1. 通用 Org → HTML 主题框架

把现状梳理如下。


| 主题                     | 来源          | 风格                 | 关键能力                            |
| ---------------------- | ----------- | ------------------ | ------------------------------- |
| `theme-readtheorg`     | fniessen 上游 | 类 Read-the-Docs    | 左侧 sticky TOC、Bootstrap、搜索、代码复制 |
| `theme-bigblow`        | fniessen 上游 | 横版宽幅               | 浮动 TOC、jQuery 动效                |
| `theme-henri`          | 本地          | 简单 setup           | 仅引入 CSS                         |
| `theme-henri-bearblog` | 本地          | Bearblog + Monokai | 顶栏、浅/深切换、章节折叠、当前小节、移动端 TOC      |
| `theme-henri-notes`    | 本地          | 默认 Org 导出       | 通用阅读版式、明暗切换、浮窗 TOC                  |
| `theme-henri-journal`  | 本地          | Journal 专用        | Journal 页头、Activity History 日历             |

未显式指定主题的 Org 文件导出 HTML 时，会自动使用 `Henri Notes` 主题（Journal 月度文件自动使用 `Henri Journal`）。主题在导出时动态注入，不会在源文件中写入绝对路径。用 `C-c m h t` 设置主题时，源文件只写入 `#+HENRI_HTML_THEME: <主题名>` 这个可移植关键字；导出时由 hook 解析为当前设备上 `.setup` 文件的实际路径。若需要为某次导出换主题而不改源文件，用 `C-c m h w`。


各主题往往各自覆盖**字号、布局、TOC、代码块、表格、链接**等一整套样式，重复度高、命名又不统一，会带来：

- 改一项共性能力（例如全主题「代码复制」）需多处修改；
- 新主题常靠「整文件复制 + 改色」，行为脚本与样式耦合，难维护；
- 若 `.setup` 写死本机绝对路径，换环境即失效。

### 1.1 框架分层

建议按「**变量 / 重置 / 布局 / Org 元素 / 行为 / 主题包**」六层组织，职责分离：

```text
org-html-themes/
├── core/                       # 共享基座（所有主题必引）
│   ├── tokens.css              # 仅定义 :root 默认变量（颜色/字号/间距/容器宽）
│   ├── reset.css               # body / 排版 / 链接 / hr 的浏览器默认整平
│   ├── layout.css              # #content grid、#table-of-contents、#postamble、媒体查询
│   ├── org-elements.css        # .outline-N、.org-ul、.org-src-container、table、figure、tag、todo、timestamp…
│   ├── behavior.js             # 折叠、当前小节、顶栏紧凑、主题切换、移动端 TOC
│   └── README.org              # 变量与可用 hook 类名清单
│
├── themes/
│   ├── henri-bearblog/
│   │   ├── tokens.css          # 只覆盖 :root 变量（颜色/字体/容器宽）
│   │   ├── overrides.css       # 必要时再覆盖少量选择器（如 logo 字体、卡片化 h1.title）
│   │   └── theme.js            # 仅当主题需要额外行为时存在；否则不创建
│   ├── henri/
│   │   └── tokens.css
│   └── readtheorg-mirror/        # 若将上游 ReadTheOrg 对齐到本框架，可在此做适配层
│
├── setup/                      # Org 入口文件，路径可移植
│   ├── theme-henri-bearblog.setup
│   └── theme-henri.setup
│
└── examples/                   # 每主题一份最小示例 .org（及可选对照 HTML），便于回归
```

**变量层 `core/tokens.css`** 只声明 token，不在此写具体组件样式：

```css
:root {
    /* 容器与节奏 */
    --width: 760px;
    --toc-width: 200px;
    --col-gap: 2.2rem;
    --line-height: 1.65;

    /* 字体 */
    --font-body: system-ui, -apple-system, "PingFang SC", sans-serif;
    --font-heading: var(--font-body);
    --font-mono: ui-monospace, "JetBrains Mono", Menlo, monospace;

    /* 字号阶 */
    --fs-h1: 1.85em;
    --fs-h2: 1.35em;
    --fs-h3: 1.18em;
    --fs-h4: 1.06em;
    --fs-h5: 1.00em;

    /* 颜色（语义名，由主题包赋色） */
    --color-bg: #ffffff;
    --color-bg-alt: #f5f5f5;
    --color-text: #1a1a1a;
    --color-muted: #6b7280;
    --color-heading: #111;
    --color-link: #2563eb;
    --color-link-hover-bg: #2563eb;
    --color-link-hover-text: #fff;
    --color-border: #e5e7eb;
    --color-code-bg: #f6f8fa;
    --color-code-text: #24292f;
    --color-accent: #fd971f;

    /* 顶栏等扩展 */
    --topbar-bg: var(--color-bg-alt);
}
```

**主题包**（例如 `themes/henri-bearblog/tokens.css`）以覆盖变量为主：

```css
:root {
    --font-body: 'Bree Serif', serif;
    --color-bg: #272822;
    --color-text: #f8f8f2;
    --color-link: #66d9ef;
    /* … */
}
html[data-theme='light'] { /* 浅色覆盖 */ }
html[data-theme='dark']  { /* 深色覆盖 */ }
```

新增主题时优先只增加一份 `tokens.css`，其余由 `core` 承担。

### 1.2 `core/org-elements.css` 应覆盖的 Org 选择器

新主题最容易漏掉的节点如下（按常见程度排列）。


| 选择器                                                                                   | 含义                         |
| ------------------------------------------------------------------------------------- | -------------------------- |
| `.title`, `h1.title`                                                                  | 文档标题（导出可能同时出现 div 与 h1）    |
| `.subtitle`                                                                           | 副标题                        |
| `#table-of-contents`, `#text-table-of-contents`, `#text-table-of-contents ul ul`      | 目录及嵌套                      |
| `.outline-2` … `.outline-6`, `.outline-text-2` … `.outline-text-6`                    | 大纲容器（与列表缩进叠加时易「挤到右侧」）      |
| `#content ul`, `#content ol`, `#content li > ul`                                      | **嵌套列表缩进**（建议显式写，勿依赖浏览器默认） |
| `.org-src-container`, `pre.src`, `pre.example`, `code`                                | 源码块与行内代码                   |
| `.todo`, `.done`, `.timestamp`, `.timestamp-wrapper`, `.timestamp-kwd`, `.tag > span` | TODO、时间戳、标签                |
| `.figure`, `.figure-number`, `.org-svg`, `img`                                        | 图与图号                       |
| `table`, `table th`, `table td`, `caption`                                            | 表格                         |
| `.footdef`, `.footnum`, `.footpara`, `#footnotes`                                     | 脚注                         |
| `.org-ul`, `.org-ol`, `.org-dl`                                                       | Org 列表类                    |
| `#postamble`, `#preamble`                                                             | 文后元信息、文头                   |


合入 `themes/` 前，应用示例文档对上述选择器至少做一次导出目视检查（`examples/` 宜覆盖全表）。

### 1.3 行为层（`core/behavior.js`）约定

可将现有折叠脚本抽成可复用模块，并用 **data 属性**控制是否启用各功能，例如：

```html
<body data-hb-features="topbar,fold,theme-toggle,current-section,mobile-toc">
```

对外约定建议包括：

- **类名**：如 `.hb-collapsible-heading`、`.hb-section-body`、`#hb-topbar`、`#hb-current-heading`（主题 CSS 仅负责为这些类赋色与间距）；
- **配置**：在脚本前定义 `window.HB_CONFIG = { ... }`，控制默认折叠深度、顶栏紧凑阈值、是否记忆主题、当前小节跟踪层级等。

后续若做偏「学术、静态」的主题，可关闭 `fold` 与 `theme-toggle`，而不改核心 HTML 结构。

### 1.4 `.setup` 入口模板与路径

**当前仓库做法**（已实现）：在 `#+HTML_HEAD` 中使用占位符 `@@henri-org-html-themes-root@@`，与 `file://` 拼接为资源 URL；HTML 导出结束后由 `[lisp/writing/org/org-html.el](../../lisp/writing/org/org-html.el)` 挂入的 `org-export-filter-final-output-functions` 将占位符替换为 `henri-org-html-themes-directory` 的绝对路径（`SETUPFILE` 合并时机晚于 `org-export-before-processing-hook`，故不宜仅在后者中替换）。

**目标形态示例**（待统一拆 `core` / `themes` 后可改为下列多文件引用；`{{THEMES_ROOT}}` 表示由工具或占位符统一替换的 themes 根目录）：

```org
# -*- mode: org; -*-
#+OPTIONS: html-style:nil toc:t num:3
#+HTML_HEAD_EXTRA: <link rel="stylesheet" href="{{THEMES_ROOT}}/core/tokens.css"/>
#+HTML_HEAD_EXTRA: <link rel="stylesheet" href="{{THEMES_ROOT}}/core/reset.css"/>
#+HTML_HEAD_EXTRA: <link rel="stylesheet" href="{{THEMES_ROOT}}/core/layout.css"/>
#+HTML_HEAD_EXTRA: <link rel="stylesheet" href="{{THEMES_ROOT}}/core/org-elements.css"/>
#+HTML_HEAD_EXTRA: <link rel="stylesheet" href="{{THEMES_ROOT}}/themes/henri-bearblog/tokens.css"/>
#+HTML_HEAD_EXTRA: <link rel="stylesheet" href="{{THEMES_ROOT}}/themes/henri-bearblog/overrides.css"/>
#+HTML_HEAD_EXTRA: <script src="{{THEMES_ROOT}}/core/behavior.js" defer></script>
```

实际写本地资源时，可将 `{{THEMES_ROOT}}` 换为 `file://@@henri-org-html-themes-root@@`，与现有 Emacs 侧替换逻辑一致。

### 1.5 Emacs 侧可扩展能力

[`lisp/writing/org/org-html.el`](../../lisp/writing/org/org-html.el) 中 `henri/org-html-themes-list` 维护「展示名 → setup 路径」。可进一步增加例如：

1. `**my/org-html-theme-validate**`：对固定 `examples/<theme>/sample.org` 导出并比对 DOM 或快照（便于 CI）；
2. `**my/org-html-theme-scaffold**`：交互生成主题骨架（`tokens.css`、`setup`、`sample.org`），减少从 Bearblog 整文件复制。

---

## 2. 推荐执行顺序

与仓库内「小步可归档」工作流一致时，可按下面顺序推进：

1. **先打补丁**：在既有单文件主题（如 `henri-bearblog-theme.css`）上收敛列表与 `outline` 缩进、调整版心，风险低；必要时在 `lisp/writing/org/` 下用 work-note 记一笔设计原因。
2. **再抽 `core`**：按 §1.1 将 Bearblog 大文件拆成 `tokens` / `reset` / `layout` / `org-elements`，先在同一主题上验证导出像素级或结构级无回归。
3. **最后定型**：补齐 `themes/henri/`、`setup/` 模板与脚手架命令；在架构说明（如 `docs/specs/ARCHITECTURE.md`）中增加「Org HTML 主题分层」一节，并在 `docs/jobs/` 登记后续任务。

上述第 2、3 步为中长期重构；是否与第 1 步合并排期取决于当前迭代负担。
