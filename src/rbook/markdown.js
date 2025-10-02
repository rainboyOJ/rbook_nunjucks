import * as matter from 'gray-matter';
import markdownit from './markdownit.js';
class Markdown {
    constructor() {
        this.name = 'rbook';
        this.front_matter = {};
        this.md_content = '';
        this.html_content = '';
    }

    load_config() {
        
    }

    /**
     * return { conent: markdown, data: frontmatter }
     *  */
    matter(md_content) {
    }
}

export default Markdown;