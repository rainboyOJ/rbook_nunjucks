import fs from 'fs';
import path from 'path';
import yaml from 'js-yaml';
import { globSync } from 'glob';
import markdown from '@rbook/markdown';
import { renderTemplate } from './renderEngine.js';
import {
    bookDir,
    codeTemplateDir,
    configPath as defaultConfigPath,
    distDir,
    publicDir,
    fromApp,
    rootDir,
    themeDir
} from './paths.js';

interface BookChapter {
    path?: string;
    sections?: BookChapter[];
    [key: string]: unknown;
}

interface BookConfig {
    chapters?: BookChapter[];
    glob?: string[];
    currentYear?: number;
    last_build_time?: string;
    menuHtml?: string;
    [key: string]: unknown;
}

type RenderData = Record<string, unknown>;

export const __workdir = rootDir;
export const __bookdir = bookDir;
export const __code_template_dir = codeTemplateDir;
export const __themedir = themeDir;

function errorMessage(error: unknown) {
    return error instanceof Error ? error.message : String(error);
}

function ensureDir(dir: string) {
    if (!fs.existsSync(dir)) {
        fs.mkdirSync(dir, { recursive: true });
    }
}

class rbook {
    name: string;
    config: BookConfig;

    constructor() {
        this.name = 'rbook';
        this.config = this.load_config();
        this.config.currentYear = new Date().getFullYear();
        this.config.last_build_time = new Date().toLocaleString('zh-CN', {
            timeZone: 'Asia/Shanghai'
        });
    }

    load_config(configPath = defaultConfigPath): BookConfig {
        try {
            const configFile = path.isAbsolute(configPath) ? configPath : fromApp(configPath);
            if (!fs.existsSync(configFile)) {
                throw new Error(`配置文件不存在: ${configPath}`);
            }

            const content = fs.readFileSync(configFile, 'utf8');
            return yaml.load(content) as BookConfig;
        } catch (error) {
            throw new Error(`加载配置文件失败: ${errorMessage(error)}`);
        }
    }

    buildMarkdownFile(
        filePath: string,
        outputPath: string,
        defaultTemplateType: string | null = null,
        data: RenderData = {}
    ) {
        const fullPath = path.join(bookDir, filePath);
        if (!fs.existsSync(fullPath)) {
            console.error(`✗ 文件不存在: ${filePath}`);
            return false;
        }

        const fullOutputPath = path.isAbsolute(outputPath) ? outputPath : fromApp(outputPath);
        ensureDir(path.dirname(fullOutputPath));

        const md = new markdown(fullPath);
        const templateType = md.front_matter.layout || defaultTemplateType || 'page';
        const htmlContent = renderTemplate(themeDir, templateType, {
            ...md.toJSON(),
            site: this.config,
            ...data
        });

        fs.writeFileSync(fullOutputPath, htmlContent);
        console.log(`✓ 构建完成: ${filePath}`);
        return true;
    }

    checkMarkdownFile(basePath: string, relativePath: string) {
        const fullPath = path.join(bookDir, basePath, relativePath);

        if (fs.existsSync(fullPath) && fs.statSync(fullPath).isDirectory()) {
            const indexPath = path.join(fullPath, 'index.md');
            return fs.existsSync(indexPath)
                ? path.join(basePath, relativePath, 'index.md')
                : null;
        }

        if (fullPath.endsWith('.md') && fs.existsSync(fullPath)) {
            return path.join(basePath, relativePath);
        }

        const mdPath = `${fullPath}.md`;
        return fs.existsSync(mdPath)
            ? path.join(basePath, `${relativePath}.md`)
            : null;
    }

    getAllMarkdownFiles(chapters: BookChapter[] = [], basePath = '') {
        const files: string[] = [];

        for (const chapter of chapters) {
            if (!chapter.path) continue;

            if (Array.isArray(chapter.sections)) {
                const childBasePath = basePath ? path.join(basePath, chapter.path) : chapter.path;
                files.push(...this.getAllMarkdownFiles(chapter.sections, childBasePath));
                continue;
            }

            const foundFile = this.checkMarkdownFile(basePath, chapter.path);
            if (foundFile) files.push(foundFile);
        }

        return files;
    }

    // 只返回 book.yaml 目录树中的文章；glob 文章由 build_glob 单独处理。
    get AllMarkdownFiles() {
        return this.getAllMarkdownFiles(this.config.chapters);
    }

    build() {
        console.log('开始构建...');

        try {
            ensureDir(distDir);
            this.config.menuHtml = this.renderMenu();

            for (const file of this.AllMarkdownFiles) {
                const outputPath = path.join(distDir, file).replace(/\.md$/, '.html');
                this.buildMarkdownFile(file, outputPath);
            }

            this.renderIndex();
            this.copyAssets();

            console.log('构建完成！');
        } catch (error) {
            throw new Error(`构建失败: ${errorMessage(error)}`);
        }
    }

    build_glob() {
        if (!this.config.glob) return;

        const chapterFiles = this.AllMarkdownFiles;
        console.log('===== render glob md file =====');

        for (const pattern of this.config.glob) {
            const mdFiles = globSync(pattern, {
                cwd: bookDir,
                ignore: 'node_modules/**'
            });

            for (const mdFile of mdFiles) {
                if (chapterFiles.includes(mdFile)) continue;

                const outputPath = path.join(distDir, mdFile).replace(/\.md$/, '.html');
                this.buildMarkdownFile(mdFile, outputPath);
            }
        }
    }

    renderMenu() {
        return renderTemplate(themeDir, 'partials/menu', this.config);
    }

    renderIndex() {
        if (this.buildMarkdownFile('index.md', path.join(distDir, 'index.html'), 'index', {
            site: this.config
        })) {
            console.log('✓ 首页构建完成');
        }
    }

    copyAssets() {
        this.copyDir(publicDir, distDir);
        this.copyDir(path.join(themeDir, 'assets'), path.join(distDir, 'assets'));
    }

    // 兼容旧调用名，新的代码优先使用 copyAssets。
    deal_assets() {
        this.copyAssets();
    }

    copyDir(src: string, dest: string) {
        const fullSrc = path.isAbsolute(src) ? src : fromApp(src);
        const fullDest = path.isAbsolute(dest) ? dest : fromApp(dest);
        if (!fs.existsSync(fullSrc)) {
            throw new Error(`源目录不存在: ${src}`);
        }

        ensureDir(fullDest);
        fs.cpSync(fullSrc, fullDest, { recursive: true });
    }

    // 兼容旧调用名，新的代码优先使用 copyDir。
    copy_dir(src: string, dest: string) {
        this.copyDir(src, dest);
    }
}

export default rbook;
