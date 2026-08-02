---
name: rbook-problem-solutions
description: Use this skill whenever the user asks to find, verify, recommend, or add an OJ problem from /home/rainboy/mycode/rbook_new_problem_solutions/problems, including requests such as "find luogu P2922", "find Trie exercises", or "use these problems to improve an rbook article". It verifies the solution catalog by front matter and generates the corresponding pcs2.roj.ac.cn solution links.
---

# RBook Problem Solutions

Use the local solution catalog to find verified OJ problems and connect them to articles in this repository. The catalog is authoritative through each `index.md` front matter, not through its directory names.

## Source Of Truth

- Problems root: `/home/rainboy/mycode/rbook_new_problem_solutions/problems`
- Catalog implementation: `/home/rainboy/mycode/rbook_new_problem_solutions/lib/problem.js`
- Public solution URL: `https://pcs2.roj.ac.cn/problems/{oj}/{problem_id}`

The catalog's `problem_url(oj, id)` is `/problems/{oj}/{id}`. It is formed from front matter `oj` and `problem_id`, so preserve that canonical metadata in all generated URLs.

## Lookup Commands

Run the bundled script from the rbook repository root. It recursively reads `index.md` files, validates their front matter, and returns JSON.

```bash
node .agents/skills/rbook-problem-solutions/scripts/problem-catalog.mjs \
  lookup --oj luogu --id P2922
```

For Luogu, `2922` and `P2922` both resolve to the canonical `P2922`. Other OJ IDs are matched against catalog metadata without inventing a platform-specific transformation.

When a lookup succeeds, use the returned `problem.oj`, `problem.problem_id`, `problem.title`, `problem.difficulty`, `problem.tags`, and `problem.solution_url`. Do not derive a URL from the file path.

When it fails, say that no verified solution was found for the canonical query. Do not generate a solution link, substitute another problem, or claim the original problem has a local solution.

## Topic Search

Only search by subject when the user explicitly asks for problems suitable for an algorithm or article. Extract one or more concrete algorithm keywords, then run:

```bash
node .agents/skills/rbook-problem-solutions/scripts/problem-catalog.mjs \
  search --query "Trie prefix" --limit 5
```

The script ranks tag matches ahead of title, description, and body matches. Return at most five candidates. For each candidate, report:

- OJ, canonical problem ID, title, and difficulty;
- the relevant tags or the specific reason it fits the requested article;
- the normal problem reference and verified solution link.

Read a candidate's `index.md` before making a detailed recommendation. Do not return code or copy a solution during discovery unless the user asks for analysis.

## Article Links

When editing `book/pages/` content, also follow the `rbook-article-writer` skill. Add verified problems only when the user explicitly asks to improve an article, or when editing its `经典例题` or `应用分类详解` section.

Keep the existing rbook problem reference and append the solution link:

```markdown
[[problem: luogu,P2922]] [题解](https://pcs2.roj.ac.cn/problems/luogu/P2922)
```

Do not replace `[[problem: oj,id]]`. Before editing, verify the exact problem through the lookup command. For an existing problem reference, parse its OJ and ID, verify it, then append a `[题解]` link only if the catalog metadata matches.

Finding candidates alone never changes an article. Make edits only when the user explicitly asks to use selected problems to improve it. Explain why each added problem exercises the article's algorithmic idea rather than adding a list of unrelated titles.

## Verification

For article edits, run the validation required by `rbook-article-writer`. At minimum, check that every added solution URL uses the returned canonical OJ and ID, and that the Markdown link renders as intended.

## Examples

User: "Find luogu 2922."

1. Run `lookup --oj luogu --id 2922`.
2. Report the verified title and `https://pcs2.roj.ac.cn/problems/luogu/P2922`.

User: "Find exercises for the Trie article."

1. Run a topic search such as `search --query "Trie prefix" --limit 5`.
2. Present verified candidates and matching reasons. Do not edit the article yet.

User: "Use P2922 to improve the Trie article."

1. Look up `luogu/P2922` and read its `index.md`.
2. Update the relevant example section with the verified problem reference and `[题解]` link.
3. Validate the article.
