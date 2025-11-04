## 安装依赖
```bash
sudo apt install -y python3 fzf graphviz clangd curl
```

# 配置

## 添加到zshrc,bashrc

```bash
export PATH=$PATH:<path-to-rbook_nunjucks>/dotfiles/scripts
```

脚本列表


```
dotfiles/scripts
├── b          快速编译
├── dot2png.py graphviz dot 转 png 
├── duipai.py 对拍脚本
├── input2dot.py  `cat in | tail -n +2 | input2dot.py` in.txt 转dot图 
├── lldb.sh 调用lldb调试
├── luogu.py 下载luogu样例数据
├── mylib  
├── nvimsizer.sh
├── one-duipai.py 展示输入文件INPUT 的内容 并输出 程序1 说程序2 的运行结果
├── r-list-all-scripts.py  列出所有的脚本
├── randint.py  随机数生成
├── test-data.py 测试数据 `data/` 目录下
├── template.py 根据 template 创建 cpp
└── transfer.sh 在线剪切板
```

## tmux

使用了 [Tmux Plugin Manager](https://github.com/tmux-plugins/tpm)

安装

```bash
git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
```


Installing plugins

1.  Add new plugin to `~/.tmux.conf` with `set -g @plugin '...'`
2.  Press `prefix` + I (capital i, as in **I**nstall) to fetch the plugin.

- [Tmux 快捷键速查表](https://blog.roj.ac.cn/blog/tmux%E5%B8%B8%E7%94%A8%E5%BF%AB%E6%8D%B7%E9%94%AE.html)

## 脚本使用

### transfer.sh

通过命令行上传分享代码[github](https://github.com/dutchcoders/transfer.sh)

编辑配置文件 (以 `~/.bashrc` 为例):

```bash
echo 'export TRANSFER_URL="http:// your-transfer-sh-url"' >> ~/.bashrc
```