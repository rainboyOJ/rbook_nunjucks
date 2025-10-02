import nunjucks from 'nunjucks';
import fs from 'fs';
import path from 'path';

/**
 * 使用 Nunjucks 渲染模板
 * @param {string} theme_dir - 主题目录路径
 * @param {string} layout_name - 布局名称（不包含 .njk 扩展名）
 * @param {*} data  { content: html_content, front_matter: front_matter, config: config, assets: assets }
 * @return {string} html - 渲染后的HTML
 */
export const nunjucksRender = function (theme_dir, layout_name, data) {
    // 检查主题目录是否存在
    if (!fs.existsSync(theme_dir)) {
        throw new Error(`主题目录不存在: ${theme_dir}`);
    }
    
    // 配置 Nunjucks 环境
    const env = nunjucks.configure(theme_dir, {
        autoescape: true,
        noCache: true
    });
    
    // 构建模板路径
    const templatePath = `${layout_name}.njk`;
    const templateFullPath = path.join(theme_dir, templatePath);
    
    // 检查模板文件是否存在
    if (!fs.existsSync(templateFullPath)) {
        throw new Error(`模板文件不存在: ${templatePath}`);
    }
    
    try {
        // 准备渲染上下文
        const context = {
            site: data.config || {},
            page: {
                ...(data.front_matter || {}),
                content: data.content || ''  // 将content添加到page对象中
            },
            assets: data.assets || {}
        };
        
        // 渲染模板
        return env.render(templatePath, context);
    } catch (error) {
        throw new Error(`渲染模板失败: ${error.message}`);
    }
};

export default nunjucksRender;