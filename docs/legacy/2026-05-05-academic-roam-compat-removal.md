# Academic / Roam 兼容符号移除记录

2026-05-05 起，Academic 不再保留旧版 Org-roam 兼容入口：

- `org-academic-org-roam-dir`
- `org-academic-enable-org-roam`
- `org-academic-create-research-note`

新的边界是：Org-roam 统一由 `org-roam-henri.el` 管理；Academic 的正式文献阅读卡默认仍在 `Academic/Reading/`，并通过 `org-roam-extra-files` 进入 Roam 图谱。若显式开启 `henri-org-roam-enable-citar-integration`，Citar notes 真源会整体切换到 `Roam/references/`。
