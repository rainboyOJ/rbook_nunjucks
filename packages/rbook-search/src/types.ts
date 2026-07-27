export type PageSource = 'index' | 'about' | 'chapters' | 'glob' | 'all' | string;

export interface BookChapter {
  title?: string;
  path?: string;
  type?: string;
  sections?: BookChapter[];
}

export interface BookConfig {
  title?: string;
  author?: string;
  description?: string;
  github_repository?: string;
  chapters?: BookChapter[];
  glob?: string[];
  [key: string]: unknown;
}

export interface CollectPagesOptions {
  configPath?: string;
  includeAllMarkdown?: boolean;
}

export interface CollectedPage {
  source: PageSource;
  visible: boolean;
  path: string;
  title: string;
  navTrail: string[];
}

export interface PageDocument extends CollectedPage {
  url: string;
  frontMatter: Record<string, unknown>;
  sourceContent?: string;
  headings: string[];
  text: string;
  excerpt: string;
}

export interface BuildSearchIndexOptions extends CollectPagesOptions {
  outputPath?: string;
  write?: boolean;
}
