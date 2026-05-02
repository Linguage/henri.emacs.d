# 架构说明

## 配置分层（摘要）

| 层级 | 路径 / 入口 | 说明 |
|------|-------------|------|
| 入口 | `init.el`、`early-init.el` | `load-path` 含 `lisp`、`lisp/visual`、`lisp/ops` 等 |
| 核心可调项 | `lisp/init-custom.el` | `defcustom`、主题策略（`henri/apply-current-theme` 等） |
| 视觉系统 | `lisp/visual/` | **字体 / 主题包 / UI 组件** 三分离；总入口 `init-visual.el` |
| 兼容入口 | `lisp/init-styling.el` | `(require 'init-visual)`；短期保留以降低启动链改动风险 |
| 字体兼容 | `lisp/ops/lib-fonts.el` | `(require 'visual-fonts)`，避免 doctor 等大改 `require` |
| Org 写作 | `lisp/writing/org/org-base.el` | Org 行为、颜色与插件；**正文字体策略**委托 `visual-fonts` |

## `lisp/visual/` 职责

1. **`visual-fonts.el`**：`henri/set-font`、`henri-big-font-mode`、Org CJK、`henri/org-setup-body-font`（通过 `eval-after-load 'org` 注册 hook）、`henri/apply-fonts`（`henri/set-font` + 若已定义则 `henri/apply-org-faces`）。
2. **`visual-themes.el`**：`doom-themes`；定义 `henri-theme-changed-hook` 并在此挂上 `henri/apply-fonts`，保证主题应用后字体重载。
3. **`visual-components.el`**：基础 UI、modeline、图标、可选 centaur-tabs；GUI 下首次调用 `henri/apply-fonts`；字体缩放快捷键。

## 加载顺序注意

- `init.el` 在 `init-styling` 之前已 `(require 'lib-fonts)`，因此 **`visual-fonts` 会先于 `init-visual` 被加载一次**（函数可用）；`init-visual` 再次 `require` 为幂等。
- 主题策略仍依赖 `init-custom` 早于 `lib-fonts` / `init-styling` 载入。
