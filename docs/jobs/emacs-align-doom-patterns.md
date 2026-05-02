# Emacs：对齐 Doom 模式（staging hooks / lib-* / doctor / profile）

- **状态**: doing
- **最近更新**: 2026-05-02

## 范围

对标 Doom Emacs 中高 ROI、可独立于 `straight`/`elpaca`/`map!` 的部分：

- `henri-first-{input,buffer,file}-hook`（[`lisp/ops/lib-hooks.el`](../../lisp/ops/lib-hooks.el)）
- 通用工具层 `lib-{hooks,system,fonts,files}.el`、`doctor`、`profiles`（[`lisp/ops/`](../../lisp/ops/)）
- `henri-buffer-real-p`、`henri/doctor`、多机器 `profile-*.el`
- so-long：首个文件后 `global-so-long-mode`；`abort-if-file-too-large` 前置仅 `message`，避免与本模式重复的第二套 so-long-minor-mode 钩子

## 本次完成（审计整改）

- 去掉 so-long「本地 hook」与全局 `global-so-long-mode` 的重复路径
- 移除 `prog-mode-hook` 上已无用的 PATH 初始化，统一 `henri-first-input-hook`
- `henri-first-buffer-hook` 忽略 `*Henri Dashboard*` 与 minibuffer（centaur-tabs 延后到首张「工作」缓冲）
- 字体缩放快捷键保留 `universal-arg`/`C-u N`
- doctor 特性行：`loaded` / `available` / `MISSING`（延迟安装包不报 MISSING）
- 文档：根目录 `README.md`、`docs/specs/ARCHITECTURE.md`、`docs/jobs` 本文

## 下一步（可选 / 对照审查「可选打磨」）

- `consult-buffer` / ibuffer：复用 `henri-buffer-real-p`
- `henri-add-transient-hook!`：在某一延迟任务上写一个示例注释或单次 use
- doctor：`JSON`/`report-emacs-bug` 式一键复制缓冲区
- 字体缩放：可选联动 `variable-pitch` / `mode-line`

## 非目标（本 job）

- 引入 `straight`/`elpaca`、`doom sandbox`/`doom doctor` CLI
- 全流程 `general.el` / leader 重构

## 维护

结案后移动到 `docs/legacy/`。
