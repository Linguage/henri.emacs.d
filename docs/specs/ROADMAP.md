# henri.emacs.d 路线图 (Roadmap)

> 路线图文档：阶段路线与 todo-list。
> 最近更新: 2026-05-04

---

## Phase 0: 脚手架搭建

- [x] 建立 `docs/specs/`、`docs/jobs/`、`docs/legacy/`、`docs/reports/`、`docs/work-notes/` 与根目录 `c.md`、`AGENTS.md` 文档骨架

## Phase 1: 可移植性与模块卫生（已收官）

- [x] 路径集中化（`henri-paths`）与核心模块 `require` 加载
- [x] 重复包/钩子/GC 部分收敛；`fix-warnings.el` 精简
- [x] Org HTML 主题目录默认与仓库内 `org-html-themes` 一致
- [x] 启动 dashboard 简化：移除 `dired` advice 与定时清理
- [x] 运行时目录：`lisp/ops/paths.el` + 文档；**不采用** `no-littering`（见 [reports 评估](../reports/no-littering-evaluation.md)）
- [x] `henri-shell` 去硬编码；`exec-path-from-shell` 一次性初始化
- [x] centaur-tabs 辅助函数并入 `use-package`
- [x] `pyvenv-mode` 由 `python-mode` hook 启用；`company` `:defer 1`

## Phase 2: 写作环境深化（已收官）

- [x] Journal 统一月度文件 + GTD agenda 轻量化（见 [legacy](../legacy/2026-05-03-journal-capture-lightweight.md)）
- [x] `lisp/visual/` 三层分离：fonts / themes / components
- [x] Markdown 子模块分层：export / nav / notes / lint / template
- [x] Org-roam 通用知识库独立模块 `org-roam-henri.el`
- [x] Org Academic 重构为轻量模板系统（idea / reading / project / paper / abstract）
- [x] Citar 文献笔记落点决策：Academic/Reading/ 为唯一真源，`citar-org-roam` 默认关闭
- [x] Journal PDF 使用 `ctex + geometry` 常规模板；LaTeX-Diary 保留为实验主题
- [x] LaTeX 诊断增强：逐包检查 + kpsewhich 不可用提示

## Phase 3: 文档系统对齐（当前）

- [x] README 瘦身：移除冗余快捷键表，改为链接 [`tutorials/keybindings.md`](../tutorials/keybindings.md)
- [x] `docs/specs/ARCHITECTURE.md` 更新为当前模块结构
- [x] `docs/specs/ROADMAP.md` 反映 Phase 2 完成状态
- [ ] 已完成 jobs 归档到 `docs/legacy/`（markdown-enhancement、journal-capture、org-roam-settings）
- [ ] `docs/reports/INDEX.md` 补充 Phase 2 阶段经验沉淀

## Phase 4: 可选增强

- [ ] `BLUEPRINT.md` 验收标准加入度量项（启动秒数、健康 JSON 字段）
- [ ] Org-roam + Academic 联动：reading card 中自动插入 `id:` 链接到 Roam 节点
- [ ] `citar-org-roam` 开启后的完整工作流验证与文档
- [ ] Journal 与 Roam daily 的交叉引用约定

---

## 相关导航

- 在制任务：[`docs/jobs/`](../jobs/README.md)
- 历史归档：[`docs/legacy/`](../legacy/README.md)
- 经验沉淀：[`docs/reports/`](../reports/INDEX.md)
- 工作日志：[`docs/work-notes/`](../work-notes/INDEX.md)
- 快捷键速查：[`docs/tutorials/keybindings.md`](../tutorials/keybindings.md)
