#!/bin/bash

# 设置颜色变量
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# 获取脚本所在的目录的绝对路径
SCRIPT_DIR=$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" &> /dev/null && pwd)
HOME_DIR=~
GIT_PROXY="https://gh-proxy.com/"

# 函数：创建符号链接，如果目标文件已存在则备份
# 参数1: 源文件
# 参数2: 目标文件
create_symlink() {
    local source_file="$1"
    local target_file="$2"

    # 如果目标文件已存在
    if [ -e "$target_file" ] || [ -L "$target_file" ]; then
        # 如果它不是一个指向我们源文件的链接
        if [ "$(readlink "$target_file")" != "$source_file" ]; then
            echo -e "${YELLOW}Backing up existing $target_file to ${target_file}.bak${NC}"
            mv "$target_file" "${target_file}.bak"
        else
            echo -e "${GREEN}Symlink $target_file already exists and is correct. Skipping.${NC}"
            return
        fi
    fi

    echo -e "Creating symlink: ${GREEN}$target_file -> $source_file${NC}"
    # 创建符号链接
    ln -s "$source_file" "$target_file"
}

echo "Starting dotfiles installation..."

# 0. git clone 对应的仓库
echo ""
echo "Clone tmux plugin Manager.."
git clone $GIT_PROXY"https://github.com/tmux-plugins/tpm" "$HOME_DIR/.tmux/plugins/tpm"

# 1. 安装 tmux.conf
echo ""
echo "Installing tmux configuration..."
create_symlink "$SCRIPT_DIR/tmux.conf" "$HOME_DIR/.tmux.conf"

# 2. 将 scripts 目录加入 PATH（无需符号链接）
echo ""
echo "Installing scripts to PATH..."
chmod +x "$SCRIPT_DIR"/scripts/*
line="export PATH=\"$SCRIPT_DIR/scripts:\$PATH\""
for rc in "$HOME_DIR/.zshrc" "$HOME_DIR/.bashrc"; do
  if [ -f "$rc" ] && grep -qxF "$line" "$rc"; then
    echo -e "${GREEN}Already in $rc. Skipping.${NC}"
  elif [ -f "$rc" ]; then
    echo "$line" >> "$rc"
    echo -e "${GREEN}Appended to $rc${NC}"
  fi
done

echo ""
echo -e "${GREEN}Installation complete!${NC}"
echo -e "重新打开终端或执行 ${YELLOW}source ~/.zshrc${NC} 即可直接使用脚本。"
echo ""
echo "可用脚本："
ls "$SCRIPT_DIR/scripts/" | grep -v '^__pycache__$' | grep -v '^mylib$'
