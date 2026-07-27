import { parseIncludeCodeDirective, readIncludedCode } from '../../include-code.js';

/**
 * A markdown-it plugin to include code from files.
 *  options {
 *    baseDir: string, // base directory for the files to be included
 * }
 * Syntax: @include-code(path/to/file.ext, language)
 */
function includeCodePlugin(md,options = {}) {

  function includeCodeRule(state, startLine, endLine, silent) {
    const pos = state.bMarks[startLine] + state.tShift[startLine];
    const max = state.eMarks[startLine];

    const directive = parseIncludeCodeDirective(state.src.slice(pos, max));
    if (!directive) {
      return false;
    }

    // `silent` is true when the parser is just checking if the rule can be applied.
    // We should not perform any actions but just report success.
    if (silent) {
      return true;
    }

    const included = readIncludedCode(directive.reference, {
      baseDir: options.baseDir,
      currentFilePath: state.env.filePath,
      language: directive.language,
      resolveCodeId: state.env.resolveCodeId || options.resolveCodeId
    });

    if (included.error) {
      // If the file is not found, render an error message in the output.
      const errorToken = new state.Token('html_block', '', 0);
      errorToken.content = `<div style="color: red; border: 1px solid red; padding: 10px;">[include-code] Error: ${included.error}</div>`;
      state.tokens.push(errorToken);
      state.line = startLine + 1;
      return true;
    }

    // Create a 'fence' token to be rendered as a code block.
    // This reuses markdown-it's existing code block rendering.
    const token = new state.Token('fence', 'code', 0);
    token.info = included.language; // The language for syntax highlighting
    token.content = included.content.endsWith('\n') ? included.content : included.content + '\n';
    token.markup = '```'; // The fence character
    token.map = [startLine, startLine + 1];

    state.tokens.push(token);

    // Advance the parser to the next line.
    state.line = startLine + 1;

    return true;
  }

  // Register the rule. It should be checked before the default 'fence' rule.
  md.block.ruler.before('fence', 'include_code', includeCodeRule);
}

// module.exports = includeCodePlugin;
export default includeCodePlugin;
