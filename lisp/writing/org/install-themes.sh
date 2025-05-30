#!/bin/bash

# org-html-themes 安装脚本
# 用于下载和设置 org-html-themes 到本地 org 目录

echo "=== Org HTML Themes 安装脚本 ==="

# 设置目标目录为当前 org 目录下
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
THEMES_DIR="$SCRIPT_DIR/org-html-themes"

echo "📁 目标目录: $THEMES_DIR"

# 检查是否已经存在
if [ -d "$THEMES_DIR" ]; then
    echo "❌ 主题目录已存在: $THEMES_DIR"
    read -p "是否要重新下载？(y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "🗑️  删除现有目录..."
        rm -rf "$THEMES_DIR"
    else
        echo "✅ 使用现有主题目录"
        exit 0
    fi
fi

# 检查 git 是否可用
if ! command -v git &> /dev/null; then
    echo "❌ Git 未安装，请先安装 Git"
    exit 1
fi

# 下载主题到本地 org 目录
echo "📥 正在下载 org-html-themes 到本地..."
if git clone https://github.com/fniessen/org-html-themes.git "$THEMES_DIR"; then
    echo "✅ org-html-themes 下载完成!"
    echo "📁 安装位置: $THEMES_DIR"
    
    # 显示可用的主题文件
    echo ""
    echo "📋 可用的主题设置文件："
    ls -la "$THEMES_DIR/org/"*.setup | while read line; do
        echo "   $line"
    done
    
    echo ""
    echo "🎉 安装完成！"
    echo ""
    echo "使用方法："
    echo "1. 在 Emacs 中重新加载配置: M-x eval-buffer (在 org-html.el 中)"
    echo "2. 打开任意 .org 文件"
    echo "3. 使用 C-c h d 应用默认主题"
    echo "4. 使用 C-c h t 选择其他主题"
    echo "5. 使用 C-c h e 导出并在浏览器中查看"
    echo ""
    echo "注意: 主题已安装到本地，无需网络连接即可使用"
    
else
    echo "❌ 下载失败，请检查网络连接"
    exit 1
fi
