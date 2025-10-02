import fs from 'fs';
import path from 'path';
import yaml from 'js-yaml';
import { fileURLToPath } from 'url';
import { dirname } from 'path';
import { nunjucksRender } from './renderEngine.js';

// 获取当前文件的目录路径
const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);
const __workdir = path.join(__dirname, '../../');
const __bookdir = path.join(__workdir, 'book');
const __themedir = path.join(__workdir, 'theme');

class rbook {
    constructor() {
        this.name = 'rbook';
        this.config = this.load_config();
    }

    load_config(configPath = 'book.yaml') {
        try {
            const configFile = path.join(__workdir, configPath);
            if (!fs.existsSync(configFile)) {
                throw new Error(`配置文件不存在: ${configPath}`);
            }
            
            const content = fs.readFileSync(configFile, 'utf8');
            return yaml.load(content);
        } catch (error) {
            throw new Error(`加载配置文件失败: ${error.message}`);
        }
    }

    /**
     * 解析FrontMatter和Markdown内容
     * @param {string} content - 文件内容
     * @returns {Object} - {data: FrontMatter数据, body: Markdown内容}
     */
    parseFrontMatter(content) {
        const frontMatterRegex = /^---\s*\n([\s\S]*?)\n---\s*\n([\s\S]*)$/;
        const match = content.match(frontMatterRegex);
        
        if (match) {
            const frontMatter = match[1];
            const body = match[2];
            try {
                const data = yaml.load(frontMatter);
                return { data, body };
            } catch (error) {
                throw new Error(`解析FrontMatter失败: ${error.message}`);
            }
        }
        
        // 如果没有FrontMatter，返回空数据和完整内容
        return { data: {}, body: content };
    }

    buildMarkdownFile (filePath, outputPath, defaultTemplateType = null) {
        const fullPath = path.join(__bookdir, filePath);
        if (fs.existsSync(fullPath)) {
            // 确保输出目录存在
            const fullOutputPath = path.join(__workdir, outputPath);
            const outputDir = path.dirname(fullOutputPath);
            if (!fs.existsSync(outputDir)) {
                fs.mkdirSync(outputDir, { recursive: true });
            }
            
            // 读取Markdown文件内容
            const content = fs.readFileSync(fullPath, 'utf8');
            
            // 解析FrontMatter
            const frontMatter = this.parseFrontMatter(content);
            
            // 确定模板类型
            const templateType = frontMatter.data.layout || defaultTemplateType || 'page';
            
            // 准备渲染数据
            const renderData = {
                content: frontMatter.body, // Markdown正文内容
                front_matter: frontMatter.data, // FrontMatter数据
                config: this.config // 站点配置
            };
            
            // 使用Nunjucks渲染模板
            const htmlContent = nunjucksRender(__themedir, templateType, renderData);
            
            // 写入HTML文件
            fs.writeFileSync(fullOutputPath, htmlContent);
            console.log(`✓ 构建完成: ${filePath}`);
            return true;
        }
        return false;
    };

    /**
     * 检查给定路径的Markdown文件是否存在
     * @param {string} basePath - 基础路径 (如 'chapter1')
     * @param {string} relativePath - 相对于基础路径的路径 (如 'multiple-knapsack')
     * @returns {string|null} - 找到的文件路径或null
     */
    checkMarkdownFile(basePath, relativePath) {
        // 构建完整路径
        const fullPath = path.join(__bookdir, basePath, relativePath);
        
        // 首先检查是否是一个目录
        if (fs.existsSync(fullPath) && fs.statSync(fullPath).isDirectory()) {
            // 如果是目录，检查目录下的index.md文件
            const indexPath = path.join(fullPath, 'index.md');
            if (fs.existsSync(indexPath)) {
                return path.join(basePath, relativePath, 'index.md');
            }
        } else {
            // 如果不是目录，检查同名的.md文件
            const mdPath = fullPath + '.md';
            if (fs.existsSync(mdPath)) {
                return path.join(basePath, relativePath + '.md');
            }
        }
        
        return null;
    }

    /**
     * 递归获取章节中的所有Markdown文件
     * @param {Array} chapters - 章节配置数组
     * @param {string} basePath - 基础路径
     * @returns {Array} - Markdown文件路径数组
     */
    getAllMarkdownFiles(chapters, basePath = '') {
        const files = [];
        
        if (!chapters || !Array.isArray(chapters)) {
            return files;
        }
        
        for (const chapter of chapters) {
            // 如果有sections，说明不是叶节点，递归处理子章节
            if (chapter.sections && Array.isArray(chapter.sections)) {
                // 递归处理子章节
                const subPath = basePath ? path.join(basePath, chapter.path) : chapter.path;
                const subFiles = this.getAllMarkdownFiles(chapter.sections, subPath);
                files.push(...subFiles);
            } else {
                // 叶节点，检查Markdown文件是否存在
                const filePath = basePath ? path.join(basePath, chapter.path) : chapter.path;
                const foundFile = this.checkMarkdownFile(basePath || '', chapter.path);
                if (foundFile) {
                    files.push(foundFile);
                }
            }
        }
        
        return files;
    }

    // 根据book.yaml chapters 获取所有Markdown文件
    get AllMarkdownFiles() {
        if (!this.config || !this.config.chapters) {
            return [];
        }
        return this.getAllMarkdownFiles(this.config.chapters);
    }

    build() {
        console.log('开始构建...');
        
        try {
            // 确保dist目录存在
            const distDir = path.join(__workdir, 'dist');
            if (!fs.existsSync(distDir)) {
                fs.mkdirSync(distDir, { recursive: true });
            }
            
            // 加载配置
            const config = this.config;
            
            // 获取所有Markdown文件
            const markdownFiles = this.AllMarkdownFiles;
            // console.log('找到的Markdown文件:', markdownFiles);
            
            // 构建所有Markdown文件
            for (const file of markdownFiles) {
                const outputPath = path.join('dist', file).replace(/\.md$/, '.html');
                this.buildMarkdownFile(file, outputPath);
            }

            // 构建首页
            if (this.buildMarkdownFile('index.md', 'dist/index.html', 'index')) {
                console.log('✓ 首页构建完成');
            }
            
            console.log('构建完成！');
        } catch (error) {
            throw new Error(`构建失败: ${error.message}`);
        }
    }

}

export default rbook;