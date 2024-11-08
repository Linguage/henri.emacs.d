# henri.emacs.d

基于 Emacs 29.1 的个人配置，专注于提供现代化的编程和写作环境。

## 1. 核心特性

- 优化的启动速度和性能表现
- 现代化的编程语言支持 (LSP + Tree-sitter)
- 专业的写作环境 (Org + Markdown + LaTeX)
- 美观的界面设计

## 2. 系统要求

- Emacs 29.1 或更高版本
- 外部依赖:
  - git
  - clangd (C/C++)
  - pylsp (Python)
  - fortls (Fortran)
  - pandoc (Markdown)
  - texlive (LaTeX)

## 3. 主要模块

### 3.1 包管理 (init.el)
- use-package - 声明式包管理
- ELPA 镜像源配置 (清华源)

### 3.2 基础管理 (init-managing.el)
- ivy + counsel + swiper - 搜索与补全框架
- neotree - 文件树侧边栏
- which-key - 快捷键提示

### 3.3 界面美化 (init-styling.el)
- doom-themes - 主题方案
- doom-modeline - 状态栏美化
- rainbow-delimiters - 彩虹括号
- JetBrains Mono - 编程字体

### 3.4 编程环境 (init-programming.el)
- company-mode - 代码补全
- eglot - LSP 客户端
- tree-sitter - 语法分析
- flycheck - 语法检查
- 语言支持:
  - Python
  - C/C++
  - Common Lisp
  - Fortran
  - Julia

#### 3.4.1 Python 环境 (init_python.el)
- **环境管理**
  - conda - Conda 环境管理（默认激活 Henri_env）
  - pyvenv - 虚拟环境支持
  
- **开发工具**
  - elpy - Python IDE 功能集成
  - company-jedi - 智能代码补全
  - flycheck - 实时语法检查
  
- **交互式开发**
  - jupyter - Jupyter Notebook 支持
  - ein - 集成 Notebook 环境
  - dap-mode - 交互式调试支持

- **代码质量**
  - blacken - Black 格式化工具
  - py-isort - Import 语句排序
  - pyflakes - 语法检查工具

#### 3.4.2 Lisp 环境 (init-lisp.el)
- **SLIME 集成**
  - slime-fancy - 核心功能扩展
  - slime-asdf - ASDF 构建系统支持
  - slime-quicklisp - Quicklisp 包管理
  - slime-repl - 增强的交互环境
  
- **开发辅助**
  - paredit - 结构化编辑
  - rainbow-delimiters - 彩虹括号匹配
  - macrostep - 宏展开支持
  
- **文档与补全**
  - slime-autodoc - 自动文档显示
  - company-quickhelp - 文档快速查看
  - lisp-extra-font-lock - 增强的语法高亮

- **调试工具**
  - sly - 替代 SLIME 的现代开发环境
  - realgud - 统一调试器界面

### 3.5 写作环境 (init-writing.el)
- Markdown
- Org Mode
- LaTeX

## 4. 性能优化

- 垃圾回收优化
- 启动加载优化
- 显示渲染优化

## 5. 快捷键绑定

TODO: 添加常用快捷键列表

## 6. 安装说明

1. 备份现有配置:
```bash
mv ~/.emacs.d ~/.emacs.d.bak
```
2. 克隆仓库:
```bash
git clone https://github.com/PeakHan618/henri.emacs.d.git ~/.emacs.d

```

3. 启动 Emacs，系统将自动安装所需包。


## 7. 贡献
欢迎提交 Issue 和 Pull Request。

## 8. 许可
MIT License ```
