# 对齐全 Doom 模式 —— 分期活页

## 已完成 / 进行中

### 视觉系统拆分（2026）

- **现状**：界面相关逻辑已从臃肿的 `init-styling.el` 收敛到 **`lisp/visual/`**（`visual-fonts` / `visual-themes` / `visual-components`），`init-styling.el` 与 `lib-fonts.el` 仅为兼容入口。
- **主题**：`henri/apply-current-theme` 与 `defcustom` 主题策略仍在 `init-custom.el`；切换主题后经 `henri-theme-changed-hook` 调用 `henri/apply-fonts`。
- **后续清理（可选）**：将 `init.el` 改为直接 `(require 'init-visual)` 并删除 `init-styling` 兼容层；评估是否合并 `lib-fonts` shim。
