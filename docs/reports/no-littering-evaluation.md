# no-littering 引入评估（决策记录）

> 日期：2026-05-02  
> 状态：**暂不引入**，保留现有 `lisp/ops/paths.el`（`henri-runtime`）方案。

## 背景

本配置已通过 `paths.el` 在 `after-init-hook` 中集中创建 `var/`、`.local/*`、`tree-sitter/`、`rime/`、`transient/` 等目录，并与 `backup.el` 对齐时序。审查建议可选评估 [no-littering](https://github.com/emacsmirror/no-littering)，以进一步把 `recentf-save-file`、`url-cache-directory`、`projectile` 等状态重定向到统一命名空间。

## 预期收益

- 更少「散落在家目录或默认 `user-emacs-directory`」的包状态文件。
- 与「把一切写入 `var/` / `.local/`」的心智模型一致，便于备份与 `.gitignore`。

## 成本与风险

- **与 `henri-runtime` 的职责重叠**：`no-littering` 会改写大量路径变量；需逐项核对是否与手写的 `henri-var-directory` / `custom-file` 等冲突或重复。
- **迁移与习惯**：已有机器上的历史文件仍在旧路径；需要一次性迁移或接受双轨，否则「以为集中了其实还在读旧文件」。
- **批处理/CI**：虽与 `after-init-hook` 无关，但需确认 no-littering 在仅加载部分文件时的行为（通常安全，但变量顺序依赖 `require` 顺序）。
- **维护面**：上游变量列表随包更新而变化，升级 Emacs/第三方包后需回归。

## 结论

- **当前不启用 `no-littering`**。现有分层（`henri-paths` 用户内容根 + `henri-runtime` 机械落盘）已覆盖主要痛点，且边界清晰。
- **若要再评估**：建议单独开一个 job，列出要纳入的包（`recentf`、`url`、`projectile`、`anaconda-mode` 等），做最小 `use-package no-littering :init` PoC，并在健康脚本里检查关键路径是否指向 `var/`。

## 回滚策略（若未来试点）

- 移除 `no-littering` 的 `require` 与 `:init`，将相关 `defcustom` 恢复默认或改回手写路径；删除或迁移回标准位置的数据文件（按需）。
