#!/bin/bash

# org-html-themes 安装脚本
# 若主题已随本仓库位于 lisp/writing/org/org-html-themes/（含上游 src/ 与 henri-bearblog/），无需运行；
# 仅在新环境缺少上游文件时用于从 GitHub 克隆补齐。

echo "=== Org HTML Themes 安装脚本 ==="

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
THEMES_DIR="$SCRIPT_DIR/org-html-themes"

echo "目标目录: $THEMES_DIR"

# 已由 Git 携带或先前装全：含 ReadTheOrg 资源与自定义 Bearblog
if [ -d "$THEMES_DIR/src/readtheorg_theme" ] && [ -d "$THEMES_DIR/henri-bearblog" ]; then
    echo "主题目录已完整（通常随 henri.emacs.d 仓库）。无需 clone。"
    exit 0
fi

if [ -d "$THEMES_DIR" ]; then
    echo "目录存在但可能不完整（缺少 src/readtheorg_theme 或 henri-bearblog）。"
    read -p "是否删除后重新从 GitHub 克隆？(y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo "删除现有目录..."
        rm -rf "$THEMES_DIR"
    else
        echo "已取消。请补全缺失子目录或手动从仓库拉取。"
        exit 0
    fi
fi

if ! command -v git &> /dev/null; then
    echo "未找到 git，请先安装 Git"
    exit 1
fi

echo "正在从 GitHub 克隆 org-html-themes..."
if git clone https://github.com/fniessen/org-html-themes.git "$THEMES_DIR"; then
    echo "下载完成。安装位置: $THEMES_DIR"
    echo ""
    echo "注意: 上游仓库不含 Henri Bearblog；若需 Bearblog 请使用本仓库内的 org-html-themes 或自行合并。"
    echo "用法: Emacs 中 org-html 主题由 henri-org-html-themes-directory 指向该目录；"
    echo "      C-c h d / C-c h t 切换主题，C-c h e 导出 HTML。"
else
    echo "克隆失败，请检查网络"
    exit 1
fi
