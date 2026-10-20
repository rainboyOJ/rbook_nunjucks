// 判断 build-gates 这组昂贵用例是否需要真的运行。
//
// build-gates.test.mjs 会逐个调用 6 个构建入口（含嵌套的 build:packages），整套约 2 分钟。
// 它验证的是“构建门槛”：重复 id / 未注册 code 引用会不会被拦住、构建失败时会不会误删产物。
// 这些行为只在改动构建流程本身时才会变，所以先按改动范围做一次判断。

/** 触及即视为“可能影响构建流程”的路径。 */
export const BUILD_GATE_PATTERNS = [
  /^package\.json$/,
  /^package-lock\.json$/,
  /^site\/build\.sh$/,
  /^packages\/[^/]+\/src\//,
  /^packages\/[^/]+\/tsconfig\.json$/,
  /^tsconfig[^/]*\.json$/,
  /^\.github\/workflows\//
];

/** 该文件是否需要触发 build-gates。 */
export function touchesBuildFlow(file) {
  return BUILD_GATE_PATTERNS.some((pattern) => pattern.test(file));
}

/**
 * 决定是否运行 build-gates，返回 null 表示“要运行”，否则返回跳过原因。
 *
 * changedFiles 为 null 表示判断不出改动范围（无 git、无基线等），此时一律运行，
 * 宁可慢也不漏。
 */
export function decideBuildGates({ changedFiles, env = {} }) {
  if (env.RBOOK_FORCE_BUILD_GATES === '1') return null;
  if (env.CI) return null; // CI 没有可靠的增量上下文，全量跑
  if (changedFiles === null) return null;

  const touched = [...changedFiles].filter(touchesBuildFlow);
  if (touched.length > 0) return null;

  return (
    `本次改动（${changedFiles.size} 个文件）没有触及构建流程，跳过 build-gates；` +
    '需要强制验证时设置 RBOOK_FORCE_BUILD_GATES=1'
  );
}
