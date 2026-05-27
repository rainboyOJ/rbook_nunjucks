import pug from 'pug';
import fs from 'fs';
import path from 'path';

/**
 * 使用 Pug 渲染模板
 * @param {string} theme_dir - 主题目录路径
 * @param {string} layout_name - 布局名称（不包含 .pug 扩展名）
 * @param {*} data  { content: html_content, front_matter: front_matter, config: config, assets: assets }
 * @return {string} html - 渲染后的HTML
 */
export const renderTemplate = function (theme_dir, layout_name, data) {

    // 检查主题目录是否存在
    if (!fs.existsSync(theme_dir)) {
        throw new Error(`主题目录不存在: ${theme_dir}`);
    }

    // 构建模板路径
    const templatePath = `${layout_name}.pug`;
    const templateFullPath = path.join(theme_dir, templatePath);
    
    // 检查模板文件是否存在
    if (!fs.existsSync(templateFullPath)) {
        throw new Error(`模板文件不存在: ${templatePath}`);
    }
    
    try {
        return pug.renderFile(templateFullPath, {
            basedir: theme_dir,
            cache: false,
            pretty: true,
            menuLink(str) {
                if (!str) return str;
                if (str.endsWith('.md')) return str.replace('.md', '.html');
                if (str.endsWith('/')) return str + 'index.html';
                return str;
            },
            ...data
        });
    } catch (error) {
        throw new Error(`渲染模板失败: ${error.message}`);
    }
};

export const pugRender = renderTemplate;

export default renderTemplate;
