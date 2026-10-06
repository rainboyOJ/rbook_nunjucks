import container from 'markdown-it-container';

// Panels are readable without JavaScript. The browser enhances each group into
// an accessible tab widget; print CSS always restores the complete document.
export default function codeTabs(md) {
    const escape = md.utils.escapeHtml;

    md.use(container, 'code-tabs', {
        validate: (info) => info.trim() === 'code-tabs',
        render(tokens, index) {
            const token = tokens[index];
            if (token.nesting === -1) return '</div>\n';
            const tabs = token.meta.tabs;
            const buttons = tabs.map(({ id, label }) =>
                `<button type="button" id="${id}-tab" aria-controls="${id}-panel">${escape(label)}</button>`
            ).join('\n');
            return '<div class="code-tab-group" data-code-tabs>\n'
                + `<div class="code-tab-list" aria-label="代码版本" hidden>${buttons}</div>\n`;
        }
    });

    md.use(container, 'tab', {
        validate: (info) => /^tab\s+\S/.test(info.trim()),
        render(tokens, index) {
            const token = tokens[index];
            if (token.nesting === -1) return '</section>\n';
            const label = escape(token.info.trim().replace(/^tab\s+/, ''));
            const id = token.meta?.codeTabId;
            return `<section class="code-tab-panel"${id ? ` id="${id}-panel"` : ''}>\n`
                + `<div class="code-tab-title">${label}</div>\n`;
        }
    });

    // Assign IDs per document, and only collect direct children of each group.
    // Token levels keep nested containers and nested groups independent.
    md.core.ruler.after('block', 'code_tabs', (state) => {
        let sequence = 0;
        const groups = [];
        for (const token of state.tokens) {
            if (token.type === 'container_code-tabs_open') {
                token.meta = { ...token.meta, tabs: [], id: ++sequence };
                groups.push(token);
            } else if (token.type === 'container_code-tabs_close') {
                groups.pop();
            } else if (token.type === 'container_tab_open') {
                const group = groups[groups.length - 1];
                if (!group || token.level !== group.level + 1) continue;
                const id = `rbook-code-tabs-${group.meta.id}-${group.meta.tabs.length + 1}`;
                token.meta = { ...token.meta, codeTabId: id };
                group.meta.tabs.push({ id, label: token.info.trim().replace(/^tab\s+/, '') });
            }
        }
    });
}
