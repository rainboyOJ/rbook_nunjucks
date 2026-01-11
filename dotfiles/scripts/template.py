#!/usr/bin/env python3
"""
根据 code/template/ 目录下的模板文件，生成对应的代码文件

功能说明:
    template.py 选择对应的模板文件，生成对应的代码文件
    
"""

import os
import re
import sys
from datetime import datetime
from pathlib import Path
from mylib.gum import choose as gum_choose, filter as gum_filter, input as gum_input ,confirm as gum_confirm


class TemplateEngine:
    """简单的模板引擎，用于处理模板文件"""
    
    def __init__(self):
        # 模板变量定义
        self.template_vars = {
            'author': 'Rainboy',
            'blog': 'https://rainboylv.com',
            'github': 'https://github.com/rainboylvx',
            'date': datetime.now().strftime('%Y-%m-%d %H:%M:%S'),
        }
        
        # 获取项目根目录
        self.project_root = Path(__file__).parent.parent.parent
        self.code_path = self.project_root / 'code'
    
    def set_var(self, key: str, value: str):
        """设置模板变量"""
        self.template_vars[key] = value
    
    def set_vars(self, vars_dict: dict):
        """批量设置模板变量"""
        self.template_vars.update(vars_dict)
    
    def get_vars(self) -> dict:
        """获取当前所有模板变量"""
        return self.template_vars.copy()
    
    def include_file(self, file_path: str) -> str:
        """包含文件内容"""
        # 支持相对路径和绝对路径
        if file_path.startswith('/'):
            # 绝对路径
            full_path = Path(file_path)
        elif file_path.startswith('./') or file_path.startswith('../'):
            # 相对路径，相对于当前工作目录
            full_path = Path.cwd() / file_path
        else:
            # 相对于项目根目录
            full_path = self.project_root / file_path
        
        if not full_path.exists():
            return f"// 包含文件未找到: {file_path} (查找路径: {full_path})"
        
        try:
            content = full_path.read_text(encoding='utf-8')
            
            # 移除可能的include语句，避免重复
            content = re.sub(r'#include\s*<[^>]+>', '', content)
            content = re.sub(r'#include\s*"[^"]+"', '', content)
            
            return f"\n//oisnip_begin {file_path} 内容开始\n{content}\n//oisnip_end {file_path} 内容结束\n"
        except Exception as e:
            return f"// 读取文件失败: {file_path} - {str(e)}"
    
    def parse_template(self, template_str: str) -> str:
        """解析模板字符串"""
        result = template_str
        
        # 处理文件包含 {{include "path/to/file"}}
        def replace_include(match):
            file_path = match.group(1)
            return self.include_file(file_path)
        
        result = re.sub(r'\{\{\s*include\s*"([^"]+)"\s*\}\}', replace_include, result)
        
        # 替换 {{variable}} 格式的变量
        def replace_var(match):
            var_name = match.group(1).strip()
            
            # 跳过include语法
            if var_name.startswith('include '):
                return match.group(0)

            if self.template_vars.get(var_name) is None:
               new_var = gum_input(value="", header=f"输入变量 {var_name} 的值:")
               return new_var
            
            return str(self.template_vars.get(var_name, match.group(0)))
        
        result = re.sub(r'\{\{([^}]+)\}\}', replace_var, result)
        
        # 处理条件块 {{#if condition}} ... {{/if}}
        def replace_if(match):
            condition = match.group(1).strip()
            content = match.group(2)
            
            var_name = condition.strip()
            if self.template_vars.get(var_name) and self.template_vars[var_name] != '':
                return content
            else:
                return ''
        
        result = re.sub(r'\{\{#if\s+([^}]+)\}\}(.*?)\{\{/if\}\}', replace_if, result, flags=re.DOTALL)
        
        return result
    
    def parse_template_file(self, file_path: str) -> str:
        """解析模板文件"""
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            return self.parse_template(content)
        except Exception as e:
            raise Exception(f"无法打开文件: {file_path} - {str(e)}")
    
    def apply_template_to_buffer(self, template_path: str, output_path: str = None):
        """应用模板并输出到文件或标准输出"""
        try:
            # 更新日期变量
            self.template_vars['date'] = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
            
            parsed_content = self.parse_template_file(template_path)
            
            if output_path:
                # 写入文件
                with open(output_path, 'w', encoding='utf-8') as f:
                    f.write(parsed_content)
                print(f"模板应用成功: {template_path} -> {output_path}")
            else:
                # 输出到标准输出
                print(parsed_content)
                
            return True
        except Exception as e:
            print(f"模板解析错误: {str(e)}", file=sys.stderr)
            return False


def apply_temp_cpp():
    """应用C++模板"""
    template_engine = TemplateEngine()
    template_path = template_engine.code_path / 'template' / 'template.cpp'
    
    if not template_path.exists():
        print(f"模板文件不存在: {template_path}", file=sys.stderr)
        return
    
    # 应用模板到标准输出
    template_engine.apply_template_to_buffer(str(template_path))


def list_templates():
    """列出所有可用的模板文件"""
    template_engine = TemplateEngine()
    template_dir = template_engine.code_path / 'template'
    
    if not template_dir.exists():
        print(f"模板目录不存在: {template_dir}", file=sys.stderr)
        return []
    
    templates = []
    for file_path in template_dir.rglob('*'):
        relative_path = file_path.relative_to(template_dir)
        templates.append(str(relative_path))
    
    return sorted(templates)


def select_and_apply_template():
    """选择并应用模板"""
    templates = list_templates()
    
    if not templates:
        print("没有找到模板文件", file=sys.stderr)
        return
    
    # 使用 gum.filter 选择模板
    try:
        selected = gum_filter(templates, header="选择模板文件:", placeholder="搜索模板...")
        if selected:
            template_engine = TemplateEngine()
            template_path = template_engine.code_path / 'template' / selected
            
            if template_path.exists():
                # 应用模板到标准输出
                output_cpp_file = gum_input(value="1", header="输入输出文件名(默认1.cpp):")
                ## 后缀不是.cpp 则添加.cpp
                if not output_cpp_file.endswith('.cpp'):
                    output_cpp_file += '.cpp'
                template_engine.apply_template_to_buffer(str(template_path), output_cpp_file)
            else:
                print(f"模板文件不存在: {template_path}", file=sys.stderr)
        else:
            print("取消选择")
    except Exception as e:
        print(f"选择模板时出错: {str(e)}", file=sys.stderr)


def main():
    """主函数"""
    if len(sys.argv) < 2:
        print("用法:")
        print("  template.py apply-cpp          # 应用C++模板")
        print("  template.py select             # 选择并应用模板")
        print("  template.py list               # 列出所有模板")
        print("  template.py apply <template>   # 应用指定模板")
        print("默认选择 select")
    
    command = 'select' if len(sys.argv) <  2 else sys.argv[1]
    
    if command == 'apply-cpp':
        apply_temp_cpp()
    elif command == 'select':
        select_and_apply_template()
    elif command == 'list':
        templates = list_templates()
        if templates:
            print("可用的模板文件:")
            for template in templates:
                print(f"  {template}")
        else:
            print("没有找到模板文件")
    elif command == 'apply':
        if len(sys.argv) < 3:
            print("用法: template.py apply <template>")
            return
        template_name = sys.argv[2]
        template_engine = TemplateEngine()
        template_path = template_engine.code_path / 'template' / template_name
        
        if not template_path.exists():
            print(f"模板文件不存在: {template_path}", file=sys.stderr)
            return
        
        template_engine.apply_template_to_buffer(str(template_path))
    else:
        print(f"未知命令: {command}")


if __name__ == '__main__':
    main()