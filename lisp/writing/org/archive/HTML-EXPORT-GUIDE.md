# HTML 导出快速使用指南

## 快速开始

1. **检查主题状态**
   - 在任意 .org 文件中按 `C-c h k`
   - 如果显示"本地主题文件已就绪"，则可以直接使用
   - 如果显示缺失，请继续第2步

2. **安装主题（如果需要）**
   - 在 Emacs 中按 `C-c h i` 自动安装
   - 或在终端中运行：
     ```bash
     cd ~/.emacs.d/lisp/writing/org
     ./install-themes.sh
     ```

3. **使用主题**
   - 打开任意 .org 文件
   - 按 `C-c h d` 应用默认主题（ReadTheOrg）
   - 按 `C-c h e` 导出并在浏览器中查看

## 快捷键参考

| 快捷键    | 功能                           |
|-----------|--------------------------------|
| `C-c h d` | 应用默认主题                   |
| `C-c h t` | 选择其他主题                   |
| `C-c h e` | 导出为 HTML 并打开浏览器       |
| `C-c h w` | 使用指定主题导出（临时）       |
| `C-c h c` | 添加自定义 CSS                 |
| `C-c h r` | 移除主题设置                   |
| `C-c h i` | 安装/检查主题                  |
| `C-c h k` | 检查本地主题状态               |

## 主题选择

- **ReadTheOrg**：现代、清洁的设计，类似 GitBook
- **Bigblow**：经典的文档主题，适合技术文档

## 故障排除

### 主题不生效
1. 检查文件顶部是否有 `#+SETUPFILE:` 行
2. 确认主题文件路径是否正确
3. 使用 `C-c h k` 检查本地主题状态

### 导出失败
1. 确保已加载 org-html 模块
2. 检查 Emacs Messages 缓冲区的错误信息
3. 尝试标准导出：`C-c C-e h h`

### 网络问题
- 使用本地主题无需网络连接
- 如果本地主题安装失败，检查网络连接
- 可以手动下载主题文件

## 自定义样式

### 深色代码块
```elisp
(my/org-html-add-dark-code-style)
```

### 扩展内容宽度
```elisp
(my/org-html-expand-content-width)
```

### 自定义 CSS
使用 `C-c h c` 或直接在文件中添加：
```org
#+HTML_HEAD: <style>your-css-here</style>
```
