export interface ArticleRelationPage {
    title: string;
    url: string;
    frontMatter: Record<string, unknown>;
}

export interface ArticleRelationNode {
    id: string;
    title: string;
    url: string;
}

function supplementIds(value: unknown): string[] {
    return Array.isArray(value)
        ? [...new Set(value.filter((id): id is string => typeof id === 'string' && !!id.trim()))]
        : [];
}

// Supplement links are optional reading, independent of prerequisites and menu visibility.
export function getArticleRelations(
    pages: readonly ArticleRelationPage[],
    frontMatter: Record<string, unknown>
) {
    const id = typeof frontMatter.id === 'string' ? frontMatter.id : '';
    if (!id) return null;

    const byId = new Map<string, ArticleRelationNode>();
    for (const page of pages) {
        const pageId = page.frontMatter.id;
        if (typeof pageId !== 'string' || !pageId) continue;
        byId.set(pageId, { id: pageId, title: page.title || pageId, url: page.url });
    }
    const outgoing = supplementIds(frontMatter.supplements)
        .filter((ref) => ref !== id && byId.has(ref))
        .map((ref) => byId.get(ref)!);
    const incomingIds = pages
        .filter((page) => supplementIds(page.frontMatter.supplements).includes(id))
        .map((page) => page.frontMatter.id)
        .filter((ref): ref is string => typeof ref === 'string' && ref !== id && byId.has(ref));
    const incoming = [...new Set(incomingIds)].map((ref) => byId.get(ref)!);

    if (!incoming.length && !outgoing.length) return null;
    return {
        current: { id, title: String(frontMatter.title || byId.get(id)?.title || id) },
        incoming,
        outgoing
    };
}
