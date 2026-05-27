-- | 生成所有可能的子串
--   1. tails s 拿到所有后缀
--   2. inits 拿到每个后缀的所有前缀（即所有子串）
--   3. 过滤掉长度小于 2 的串（因为 X, Y 长度至少为 1）
allSubstrings :: String -> [String]
allSubstrings s = [sub | suffix <- tails s, sub <- inits suffix, length sub >= 2]