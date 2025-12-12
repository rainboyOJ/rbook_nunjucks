import os
import sys
import subprocess
import glob

def find_dot_files(root_dir):
    """使用 glob 递归查找目录中的所有 .dot 文件。"""
    return glob.glob(os.path.join(root_dir, '**', '*.dot'), recursive=True)

def compile_dot_to_svg(dot_file):
    """
    将 .dot 文件编译为同目录下的 .svg 文件。
    此函数还会检查修改时间以避免不必要的重新编译。
    """
    # 为输出的 SVG 文件构建完整路径
    svg_path = os.path.splitext(dot_file)[0] + '.svg'

    # 检查 SVG 文件是否存在且比 dot 文件新
    if os.path.exists(svg_path) and os.path.getmtime(svg_path) > os.path.getmtime(dot_file):
        # print(f"跳过 {dot_file}, {svg_path} 是最新的。")
        return

    print(f"正在编译 {dot_file} 到 {svg_path}")
    try:
        # 执行 dot 命令
        subprocess.run(['dot', '-Tsvg', dot_file, '-o', svg_path], check=True)
    except subprocess.CalledProcessError as e:
        print(f"编译 {dot_file} 时出错: {e}")
    except FileNotFoundError:
        print("错误: 'dot' 命令未找到。是否已安装 Graphviz 并将其添加至 PATH？")
        sys.exit(1)

def main():
    """主函数，用于解析参数和处理文件。"""
    if len(sys.argv) < 2:
        print(f"用法: python {sys.argv[0]} <要扫描的目录>")
        sys.exit(1)

    scan_dir = sys.argv[1]

    if not os.path.isdir(scan_dir):
        print(f"错误: 目录 '{scan_dir}' 未找到。")
        sys.exit(1)

    dot_files = find_dot_files(scan_dir)

    for dot_file in dot_files:
        compile_dot_to_svg(dot_file)

if __name__ == "__main__":
    main()
