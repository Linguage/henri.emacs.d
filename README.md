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

### 5.1 全局快捷键

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `M-x` | counsel-M-x | 增强的命令执行 |
| `C-x C-f` | counsel-find-file | 智能文件查找 |
| `C-s` | swiper | 交互式搜索 |
| `C-x g` | magit-status | Git 状态管理 |
| `<f8>` | neotree-toggle | 切换文件树 |
| `<f5>` | quickrun | 快速运行代码 |

### 5.2 编程相关快捷键

#### 通用编程
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c e f` | eglot-format | LSP 代码格式化 |
| `C-' C-'` | imenu-list-smart-toggle | 代码导航栏 |
| `<f6>` | realgud:pdb | Python 调试器 |

#### Lisp 开发
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c C-z` | slime | 启动 SLIME REPL |
| `C-c C-c` | slime-compile-defun | 编译当前函数 |
| `C-c C-l` | slime-load-file | 加载文件到 REPL |
| `C-c i` | slime-inspect | 检查对象 |

#### Emacs Lisp
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c C-c` | eval-defun | 求值当前函数 |
| `C-c C-b` | eval-buffer | 求值整个缓冲区 |
| `C-c C-r` | eval-region | 求值选中区域 |

### 5.3 写作相关快捷键

#### Markdown
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c C-v` | markdown-preview | 内置预览 |
| `C-c C-c p` | markdown-preview-mode | 预览模式 |
| `C-c C-g` | grip-mode | GitHub 风格预览 |

#### Org Mode - 基础操作
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c C-e` | org-export-dispatch | 导出菜单 |
| `C-c C-l` | org-insert-link | 插入链接 |
| `C-c C-t` | org-todo | 切换 TODO 状态 |
| `C-c a` | org-agenda | 打开议程 |
| `C-c c` | org-capture | 快速创建日志 |

#### Org Mode - 视图控制
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c C-r` | henri/reload-org-theme | 重新加载主题 |
| `C-c C-b` | henri/toggle-org-bullets | 切换 bullets 样式 |
| `C-c v s` | henri/org-show-all | 展开所有内容 |
| `C-c v o` | henri/org-overview | 仅显示大纲 |
| `C-c v c` | henri/org-content | 显示内容标题 |

#### Org Mode - 修复和维护
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c o f` | henri/emergency-org-fix | 紧急修复 Org 配置 |
| `C-c o v` | henri/cycle-org-startup-folded | 循环切换启动折叠状态 |

#### Org Journal - 日志管理
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c j s` | my/search-journal | 搜索日志内容 |
| `C-c j d` | my/view-diary-by-date | 按日期查看日记 |

#### Org LaTeX - PDF 导出
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c l d` | org-latex-diagnose-fonts | 诊断字体配置 |
| `C-c l r` | org-latex-reload-config | 重新加载 LaTeX 配置 |

#### Org Academic - 学术写作
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c a p` | org-academic-create-paper | 创建学术论文 |
| `C-c a n` | org-academic-create-research-note | 创建研究笔记 |
| `C-c a c` | org-academic-create-conference-abstract | 创建会议摘要 |
| `C-c a P` | org-academic-quick-paper | 快速创建论文 |
| `C-c a N` | org-academic-quick-note | 快速创建笔记 |
| `C-c a b` | org-academic-setup-bibliography | 设置参考文献 |
| `C-c a d` | org-academic-dashboard | 打开学术写作仪表板 |
| `C-c a i` | org-academic-insert-citation | 插入引用 |

### 5.4 界面操作快捷键

#### 标签页管理
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c t n` | centaur-tabs-forward | 下一个标签页 |
| `C-c t p` | centaur-tabs-backward | 上一个标签页 |
| `C-c t c` | centaur-tabs-local-mode | 切换标签页模式 |
| `鼠标滚轮` | 标签页切换 | 在标签栏上滚动切换 |

#### 文件树操作
| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-c n p` | henri/neotree-project-dir | 打开项目目录 |
| `Enter` | 打开文件/目录 | 在 neotree 中 |
| `g` | 刷新目录树 | 在 neotree 中 |
| `A` | 最大化/还原 neotree | 在 neotree 中 |

### 5.5 帮助和导航

| 快捷键 | 功能 | 说明 |
|--------|------|------|
| `C-h f` | helpful-callable | 查看函数帮助 |
| `C-h v` | helpful-variable | 查看变量帮助 |
| `C-h k` | helpful-key | 查看快捷键帮助 |
| `C-h x` | helpful-command | 查看命令帮助 |

### 5.6 自定义快捷键说明

- 所有快捷键都经过精心设计，避免与 Emacs 默认绑定冲突
- 使用 `C-c` 前缀的快捷键都是用户自定义的安全绑定
- 功能键 `<f5>` - `<f8>` 用于常用开发工具的快速访问
- 写作相关快捷键遵循各模式的标准约定

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
