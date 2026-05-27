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
  headings: string[];
  text: string;
  excerpt: string;
  chunks: SearchChunk[];
}

export interface SearchChunk {
  id: string;
  path: string;
  url: string;
  title: string;
  heading: string;
  headingLevel: number;
  chunkIndex: number;
  splitIndex: number;
  text: string;
  navTrail?: string[];
  visible?: boolean;
  source?: PageSource;
}

export interface BuildSearchIndexOptions extends CollectPagesOptions {
  outputPath?: string;
  write?: boolean;
}

export interface SearchOptions {
  limit?: number | string;
  textLength?: number | string;
  includeText?: boolean;
}
