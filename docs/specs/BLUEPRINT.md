# henri.emacs.d 项目蓝图 (Blueprint)

> 战略性文档：宏观规划与理念阐述。
> 最近更新: 2026-05-04

---

## 1. 项目定位

基于 Emacs 29.1 的个人配置，专注于提供现代化的编程和写作环境；采用 early-init / 核心分层与模块化 Org 等能力，并通过 `defcustom` 暴露可配置开关。

## 2. 核心挑战与技术路线

1. **可移植性**：机器相关路径与 shell 一律经 `defcustom`（`henri-paths`、`henri-runtime`），避免硬编码绝对路径；子路径用 `expand-file-name` 拼接。
2. **启动卫生**：减少重复钩子与 advice；运行时目录延迟到 `after-init-hook` 创建，便于批处理/CI 加载配置不产生副作用。
3. **模块边界**：分层 `early-init` → `init.el` → `lisp/init-*.el` / `ops` / `writing`；重型包按需 `use-package` 延迟加载。
4. **文档一致**：README 作为项目入口，快捷键详情集中在 [`docs/tutorials/keybindings.md`](../../docs/tutorials/keybindings.md)，架构与路线图在 `docs/specs/` 同步。

## 3. 验收标准

- 新机器仅通过 Customize 修改 `henri-paths`（及必要时 `henri-runtime`）即可启动，无崩溃与路径相关的 `user-error`。
- `--batch` 下 `require` 核心模块不因「启动阶段 mkdir」污染工作区（运行时目录在 init 完成后创建）。
- 审查项中已列的重复配置（GC、shell 初始化、tabs 声明耦合等）保持收敛状态；README 快捷键与 `init-*.el` 一致。

## 4. 战略治理原则

1. **价值优先**：默认先交付业务价值，再按需做结构优化。
2. **证据驱动**：关键结论应有测试或可复现工况支撑。
3. **文档分层**：战略（Blueprint）、架构（Architecture）、路线（Roadmap）各司其职。
4. **一问一主文档**：同一个高频问题只保留一个主文档负责回答，避免并行维护多个真相源。
5. **Day 1 可归档**：明显会形成任务线、专题线或实验线的工作，从第一天开始保留入口、输出、摘要、验证和冻结条件。

## 5. 文档系统边界

| 文档 | 职责 |
|------|------|
| `docs/specs/BLUEPRINT.md` | 战略层：宏观规划、理念、技术路线与验收标准 |
| `docs/specs/ARCHITECTURE.md` | 架构层：功能实现框架与关键技术细节 |
| `docs/specs/ROADMAP.md` | 执行层：阶段路线与 todo-list |
| `docs/tutorials/` | **使用指南**：快捷键速查、写作系统、HTML 主题等操作说明 |
| `docs/jobs/` | 在制专题文档 |
| `docs/legacy/` | 已完成任务归档 |
| `docs/reports/` | **经验沉淀**：阶段分析、专项复盘、可复用结论与决策证据 |
| `docs/work-notes/` | **经历记录**：按时间推进的研发工作记录与 session 索引 |

`reports/` 与 `work-notes/` 的硬性边界：

- 只描述“当时做了什么”→ `work-notes/`
- 已经提炼为可复用结论、原则或专题决策证据 → `reports/`
- 在 work-notes 中发现某份记录被反复回看且实际承担了经验文档的职责，应主动迁入 reports，并保留迁移说明。

默认主文档分工：

- “怎么运行/结果去哪”优先看 `c.md` 或对应子项目 `c.md`
- “当前结构边界与目录语义”优先看 `docs/specs/ARCHITECTURE.md`
- “快捷键怎么按”优先看 `docs/tutorials/keybindings.md`
- “写作系统怎么用”优先看 `docs/tutorials/writing-system-guide.md`
- “当前做什么/下一步做什么”优先看 `docs/specs/ROADMAP.md` 与 active `docs/jobs/`
- “历史材料如何回捞”优先看 `docs/legacy/`
- “经验沉淀与复盘结论”优先看 `docs/reports/INDEX.md`
- “研发过程的时间线记录”优先看 `docs/work-notes/INDEX.md`
