# Org Mode 模块化配置

## 概述

此配置将原来的 `init-org.el` 拆分为多个独立的模块，每个模块负责特定的功能，便于管理和维护。

## 目录结构

```
lisp/writing/
├── init-org.el          # 主入口文件
├── init-org-backup.el   # 原文件备份
└── org/                 # 子模块目录
    ├── org-base.el      # 基础配置
    ├── org-pdf.el       # PDF 工具
    ├── org-journal.el   # 日志系统
    ├── org-latex.el     # LaTeX 导出
    └── org-academic.el  # 学术写作（可选）
```

## 模块说明

### 1. org-base.el - 基础配置
- Org Mode 基本设置
- 美化插件 (org-bullets, org-superstar)
- 图片下载和管理 (org-download)

### 2. org-pdf.el - PDF 工具
- pdf-tools 配置
- PDF 查看模式
- 自动打开导出的 PDF

### 3. org-journal.el - 日志系统
- 日志模板配置（个人日记、工作日志、学习日志）
- 快捷键设置
- 日志查看和搜索功能
- Agenda 自定义视图
- 日历集成

### 4. org-latex.el - LaTeX 导出
- XeLaTeX 编译器设置
- 中文支持配置
- 文档类定义 (ctexart)
- 导出处理钩子

### 5. org-html.el - HTML 导出
- org-html-themes 集成（ReadTheOrg 默认主题）
- 可选主题切换（ReadTheOrg, Bigblow）
- HTML 导出优化和自定义样式
- 本地/在线主题支持

### 6. org-academic.el - 学术写作（可选）
- 引用管理 (org-ref) - 已注释
- 参考文献配置 - 已注释
- 脚注功能 - 已注释
- 内部链接增强 - 已注释
- 学术文档模板 - 已注释

## 快捷键

### 日志系统快捷键
- `C-c c` - 快速创建日志 (org-capture)
- `C-c a` - 打开议程视图 (org-agenda)
- `C-c s` - 搜索日志 (my/search-journal)
- `C-c d` - 查看个人日记 (my/view-diary-by-date)

### HTML 导出快捷键
- `C-c h t` - 设置 HTML 主题 (my/org-html-set-theme)
- `C-c h d` - 应用默认主题 (my/org-html-apply-default-theme)
- `C-c h e` - 导出并在浏览器打开 (my/org-html-export-and-open)
- `C-c h w` - 使用指定主题导出 (my/org-html-export-with-theme)
- `C-c h c` - 添加自定义 CSS (my/org-html-add-custom-css)
- `C-c h r` - 移除主题设置 (my/org-html-remove-theme)

### HTML 主题快捷调用（新增）
- `C-c h 1` - 应用主题1 (ReadTheOrg)
- `C-c h 2` - 应用主题2 (Bigblow)
- `C-c h 0` - 应用默认主题
- `C-c h s` - 交互式选择主题（输入编号或缩写）
- `C-c h ?` - 显示主题快捷方式帮助

### 全局命令（可在任何地方使用 M-x 调用）
- `org-html-theme-1` - 应用主题1
- `org-html-theme-2` - 应用主题2  
- `org-html-theme-default` - 应用默认主题
- `org-html-theme` - 交互式选择主题
- `org-html-export-quick` - 快速导出并打开

### 学术写作快捷键（需要启用 org-academic.el）
- `C-c ]` - 插入引用链接
- `C-c f` - 插入脚注
- `C-c l` - 插入内部链接
- `C-c t a` - 创建学术文档模板

## 使用方法

### 基本使用
只需要在你的 Emacs 配置中加载主入口文件：

```elisp
(require 'init-org)
```

### 启用学术写作功能
如果需要使用学术写作功能，请：

1. 编辑 `init-org.el`，取消注释以下行：
   ```elisp
   (require 'org-academic)
   ```

2. 编辑 `org-academic.el`，取消注释需要的功能配置

### 自定义配置
每个模块都可以独立配置。如果只需要某些功能，可以：

1. 编辑 `init-org.el`，注释掉不需要的模块
2. 或者直接只加载需要的模块：
   ```elisp
   (add-to-list 'load-path "~/.emacs.d/lisp/writing/org")
   (require 'org-base)
   (require 'org-journal)
   ```

## 日志系统使用

### 创建日志
使用 `C-c c` 然后选择：
- `d` - 个人日记
- `w` - 工作日志  
- `s` - 学习日志

### 查看日志
- `C-c d` - 按日期查看个人日记
- `C-c a j` - 查看日志概览
- `C-c a d` - 查看个人日记条目
- `C-c a w` - 查看工作日志条目
- `C-c a s` - 查看学习日志条目

### 搜索日志
使用 `C-c s` 在所有日志中搜索关键词

## HTML 导出使用

### 首次设置

1. 运行安装脚本下载主题：
   ```bash
   cd ~/.emacs.d/lisp/writing/org
   ./install-themes.sh
   ```
   或在 Emacs 中使用 `C-c h i` 安装主题

2. 检查主题状态：使用 `C-c h k` 检查本地主题是否可用

### 主题设置

- `C-c h d` - 为当前文件应用默认主题（ReadTheOrg）
- `C-c h t` - 选择并设置其他主题（ReadTheOrg, Bigblow）
- `C-c h r` - 移除主题设置
- `C-c h i` - 安装/检查主题
- `C-c h k` - 检查本地主题状态

### 导出文档

- `C-c h e` - 导出为 HTML 并在浏览器中打开
- `C-c h w` - 使用指定主题导出（不修改当前文件）
- 或使用标准的 `C-c C-e h h` 导出

### 自定义样式

- `C-c h c` - 添加自定义 CSS 样式
- 使用 `my/org-html-add-dark-code-style` - 添加深色代码块
- 使用 `my/org-html-expand-content-width` - 扩展内容宽度

### 主题文件位置

- **本地主题**：`~/.emacs.d/lisp/writing/org/org-html-themes/org/`
- **自动回退**：本地主题不可用时自动使用在线版本
- **离线使用**：本地主题安装后无需网络连接

## 注意事项

1. 确保已安装所需的包：
   - org
   - org-bullets
   - org-superstar
   - org-download
   - pdf-tools

2. 日志文件会存储在：
   - `~/Documents/EmacsNotes/Journal/diary.org`
   - `~/Documents/EmacsNotes/Journal/worklog.org`
   - `~/Documents/EmacsNotes/Journal/studylog.org`

3. 如果路径不存在，请手动创建目录或修改配置中的路径

4. 学术写作功能需要额外安装 org-ref 等包

## 升级和维护

模块化配置的优势：
- 易于维护：每个模块功能明确
- 按需加载：可以选择性加载模块
- 便于扩展：新功能可以独立添加为新模块
- 减少冲突：模块间相对独立

如需添加新功能，建议：
1. 创建新的模块文件
2. 在 `init-org.el` 中添加对应的 `require` 语句
3. 更新此 README 文档
