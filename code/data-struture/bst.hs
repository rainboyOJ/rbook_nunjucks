import Prelude hiding (succ)

-- | 定义二叉搜索树 (BST)，支持重复值
-- | 'Empty' 代表空树
-- | 'Node l k c r' 代表一个节点，k 是键值，c 是计数，l 和 r 分别是左右子树
data Tree a = Empty
            | Node (Tree a) a Int (Tree a)
            deriving (Eq) -- 移除 Show 以便自定义

-- | 自定义 Show 实例，使其更易读
instance (Show a) => Show (Tree a) where
    show = showTree 0
      where
        showTree _ Empty = "Empty"
        showTree level (Node l k c r) =
            "Node " ++ show k ++ " (count: " ++ show c ++ ")" ++ "\n" ++
            indent ++ "L: " ++ showTree (level + 1) l ++ "\n" ++
            indent ++ "R: " ++ showTree (level + 1) r
          where
            indent = replicate ((level + 1) * 4) ' '

-- | 辅助函数：从列表构建树
-- | 使用 foldl 和 insert 来构建
fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (flip insert) Empty

-- | 辅助函数：将树转为中序列表 (L-K-R)，包含重复值
toList :: Tree a -> [a]
toList = go
  where
    go Empty = []
    go (Node l k c r) = go l ++ replicate c k ++ go r

-- | ---------------------------------------------------------------------------
-- | 核心功能
-- | ---------------------------------------------------------------------------

-- | 向 BST 中插入一个值
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node Empty x 1 Empty -- 基本情况：在空树上插入，返回一个新节点，计数为1
insert x (Node l k c r)
    | x < k     = Node (insert x l) k c r -- x 小于当前键，递归插入左子树
    | x > k     = Node l k c (insert x r) -- x 大于当前键，递归插入右子树
    | otherwise = Node l k (c + 1) r      -- x 等于当前键，增加计数

-- | 在 BST 中查找一个值
find :: (Ord a) => a -> Tree a -> Bool
find _ Empty = False -- 在空树中查找，失败
find x (Node l k c r)
    | x < k     = find x l -- x 小于当前键，递归查找左子树
    | x > k     = find x r -- x 大于当前键，递归查找右子树
    | otherwise = True       -- 找到 (k == x)

-- | 查找 BST 中的最小元素
findMin :: Tree a -> Maybe a
findMin Empty = Nothing -- 空树没有最小值
findMin (Node Empty k c _) = Just k -- 没有左子树，当前节点就是最小值
findMin (Node l _ _ _) = findMin l -- 否则，最小值在左子树中

-- | 查找 BST 中的最大元素
findMax :: Tree a -> Maybe a
findMax Empty = Nothing
findMax (Node _ k _ Empty) = Just k
findMax (Node _ _ _ r) = findMax r

-- | 从 BST 中删除一个值
delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Empty = Empty -- 从空树删除，返回空树
delete x (Node l k c r)
    | x < k     = Node (delete x l) k c r -- 要删除的值在左子树
    | x > k     = Node l k c (delete x r) -- 要删除的值在右子树
    | c > 1     = Node l k (c - 1) r      -- 找到了，但计数 > 1，只减少计数
    | otherwise = del l r                 -- 找到了，计数为 1，需要删除节点
    where
        -- | 'del' 助手函数处理删除根节点的情况
        del :: Tree a -> Tree a -> Tree a
        del Empty r = r -- 没有左子树，用右子树替换
        del l Empty = l -- 没有右子树，用左子树替换
        del l r     = -- 有两个子节点
            -- 1. 找到右子树的最小值 (k') 作为新的根
            -- 2. 新树的左子树是 l
            -- 3. 新树的右子树是 *删除* 了 k' 的原右子树 (r)
            let (k', r') = deleteMin r -- 找到并删除右子树的最小值
            in Node l k' 1 r' -- 新节点的计数为 1

-- | 助手函数：找到并删除最小节点 (用于 'delete')
-- | 返回 (最小值的键, 删除了最小值的子树)
deleteMin :: Tree a -> (a, Tree a)
deleteMin Empty = error "deleteMin: called on empty tree" -- 逻辑上不应该发生
deleteMin (Node Empty k _ r) = (k, r) -- 找到了最小值 (k)，用其右子树 (r) 替换
deleteMin (Node l k c r) =
    let (minKey, newL) = deleteMin l -- 最小值在左子树
    in (minKey, Node newL k c r)

-- | 在纯函数式 BST 中查找后继
-- 需要整个树和目标键值
-- `best` 参数在向下递归时，记录“目前为止最好的后继候选”
succ :: (Ord a) => a -> Tree a -> Maybe a
succ x tree = go tree Nothing
  where
    go Empty _ = Nothing -- 树为空或未找到
    go (Node l k c r) best
        | x < k     = go l (Just k) -- k 是一个可能的后继, 继续在左子树找更接近的
        | x > k     = go r best     -- 后继在右子树, best 不变
        | otherwise = -- 找到了键 x (k == x)
            case r of
                Empty -> best      -- 没有右子树, 那么 best (最近的左拐祖先) 就是后继
                _     -> findMin r -- 有右子树, 后继是右子树的最小值

-- 纯函数式的前驱查找，逻辑对称
prev :: (Ord a) => a -> Tree a -> Maybe a
prev x tree = go tree Nothing
  where
    go Empty _ = Nothing
    go (Node l k c r) best
        | x < k     = go l best     -- 前驱在左子树, best 不变
        | x > k     = go r (Just k) -- k 是一个可能的前驱, 在右子- 树找更接近的
        | otherwise = -- 找到了键 x (k == x)
            case l of
                Empty -> best      -- 没有左子树, best (最近的右拐祖先) 就是前驱
                _     -> findMax l -- 有左子树, 前驱是左子树的最大值

-- | ---------------------------------------------------------------------------
-- | 排名操作 (Rank Operations)
-- | ---------------------------------------------------------------------------

-- | 计算 x 在树中的排名 (第几小的元素，从1开始)
-- | 如果 x 不在树中，返回 x 应该插入的位置的排名
rank :: (Ord a) => a -> Tree a -> Int
rank x Empty = 1  -- 在空树中，任何元素的排名都是1
rank x (Node l k c r)
    | x < k     = rank x l                    -- x 在左子树中
    | x > k     = size l + c + rank x r       -- x 在右子树中，跳过左子树和当前节点的所有重复值
    | otherwise = size l + 1                  -- x 等于当前键，排名是左子树大小 + 1（第一个出现的位置）

-- | 返回第 k 小的元素 (k 从1开始)
-- | 如果 k 超出范围，返回 Nothing
atRank :: Int -> Tree a -> Maybe a
atRank k Empty = Nothing
atRank k (Node l k' c r)
    | k <= 0    = Nothing                     -- k 无效
    | leftSize >= k = atRank k l              -- 在左子树中
    | leftSize + c >= k = Just k'             -- 在当前节点中
    | otherwise = atRank (k - leftSize - c) r -- 在右子树中
  where
    leftSize = size l

-- | 计算树中元素的总个数 (包括重复值)
size :: Tree a -> Int
size Empty = 0
size (Node l k c r) = size l + c + size r

-- | ---------------------------------------------------------------------------
-- | 映射和折叠 (Map & Fold)
-- | ---------------------------------------------------------------------------

-- | 对树中的每个元素应用一个函数
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node l k c r) = Node (mapTree f l) (f k) c (mapTree f r)

-- | 折叠 (fold) 树 (Catamorphism)
foldTree :: (b -> c -> b -> b) -> (a -> c) -> b -> Tree a -> b
foldTree _ _ z Empty = z
foldTree g f z (Node l k c r) =
    g (foldTree g f z l) (f k) (foldTree g f z r)

-- | 这是一个 *反向* 中序 (R-K-L, Right-Key-Left) 遍历折叠
foldRKL :: (a -> b -> b) -> b -> Tree a -> b
foldRKL _ z Empty = z
foldRKL f z (Node l k c r) = foldRKL f (k `f` (foldRKL f z r)) l

-- | 一个更有用的 *正向* 中序 (L-K-R, Left-Key-Left) 遍历折叠
foldLKR :: (b -> a -> b) -> b -> Tree a -> b
foldLKR f z = go z
  where
    go acc Empty = acc
    go acc (Node l k c r) =
        let accL = go acc l     -- 先处理左子树 (L)
            accK = accL `f` k  -- 再处理当前键 (K)
        in go accK r           -- 最后处理右子树 (R)

-- | ---------------------------------------------------------------------------
-- | 主函数：测试数据
-- | ---------------------------------------------------------------------------
main :: IO ()
main = do
    putStrLn "--- BST 测试 ---"

    let nums = [5, 3, 8, 1, 4, 7, 9, 2]
    putStrLn $ "原始数据: " ++ show nums

    let tree = fromList nums
    putStrLn $ "\n--- 构建的树 ---"
    print tree

    let orderedList = toList tree
    putStrLn $ "\n--- 中序遍历 (toList) ---"
    putStrLn $ "结果: " ++ show orderedList

    putStrLn "\n--- 查找 (find) ---"
    putStrLn $ "查找 4 (应为 True): " ++ show (find 4 tree)
    putStrLn $ "查找 6 (应为 False): " ++ show (find 6 tree)

    putStrLn "\n--- 最小值/最大值 (findMin/findMax) ---"
    putStrLn $ "最小值 (应为 1): " ++ show (findMin tree)
    putStrLn $ "最大值 (应为 9): " ++ show (findMax tree)

    putStrLn "\n--- 后继/前驱 (succ/prev) ---"
    putStrLn $ "4 的后继 (应为 5): " ++ show (succ 4 tree)
    putStrLn $ "5 的后继 (应为 7): " ++ show (succ 5 tree)
    putStrLn $ "9 的后继 (应为 Nothing): " ++ show (succ 9 tree)
    putStrLn $ "4 的前驱 (应为 3): " ++ show (prev 4 tree)
    putStrLn $ "1 的前驱 (应为 Nothing): " ++ show (prev 1 tree)

    putStrLn "\n--- 插入 (insert) ---"
    let tree' = insert 6 tree
    putStrLn $ "插入 6 后的树:"
    print tree'
    putStrLn $ "中序遍历 (应包含 6): " ++ show (toList tree')

    putStrLn "\n--- 删除 (delete) ---"
    putStrLn "删除 2 (叶节点):"
    let treeDelLeaf = delete 2 tree'
    print treeDelLeaf
    putStrLn $ "中序: " ++ show (toList treeDelLeaf)

    putStrLn "\n删除 3 (两个子节点):"
    let treeDelOne = delete 3 tree'
    print treeDelOne
    putStrLn $ "中序: " ++ show (toList treeDelOne)

    putStrLn "\n删除 8 (两个子节点, 7 和 9):"
    let treeDelTwo = delete 8 tree'
    print treeDelTwo
    putStrLn $ "中序: " ++ show (toList treeDelTwo)

    putStrLn "\n删除 5 (根节点，有两个子节点):"
    let treeDelRoot = delete 5 tree'
    print treeDelRoot
    putStrLn $ "中序: " ++ show (toList treeDelRoot)

    putStrLn "\n--- 映射 (mapTree) ---"
    let treeSquared = mapTree (*2) tree
    putStrLn $ "每个元素 * 2:"
    print treeSquared
    putStrLn $ "中序: " ++ show (toList treeSquared)

    putStrLn "\n--- 折叠 (foldRKL) ---"
    let sumRKL = foldRKL (+) 0 tree
    putStrLn $ "R-K-L 方式求和: " ++ show sumRKL
    putStrLn $ "foldr (+) 0 (toList tree): " ++ show (foldr (+) 0 (toList tree))

    putStrLn "\n--- 折叠 (foldLKR) ---"
    let sumLKR = foldLKR (\acc k -> acc + k) 0 tree
    putStrLn $ "L-K-R 方式求和: " ++ show sumLKR
    putStrLn $ "foldl (+) 0 (toList tree): " ++ show (foldl (+) 0 (toList tree))

    putStrLn "\n--- 测试重复值 ---"
    let treeWithDups = fromList [1, 1, 2, 2, 2, 3]
    putStrLn $ "插入 [1, 1, 2, 2, 2, 3] 后的树:"
    print treeWithDups
    putStrLn $ "中序遍历 (应为 [1, 1, 2, 2, 2, 3]): " ++ show (toList treeWithDups)

    putStrLn "\n--- 删除重复值 ---"
    let treeAfterDel = delete 2 treeWithDups
    putStrLn $ "删除一个 '2' 后的树:"
    print treeAfterDel
    putStrLn $ "中序遍历 (应为 [1, 1, 2, 2, 3]): " ++ show (toList treeAfterDel)

    putStrLn "\n--- 排名操作 (rank/atRank) ---"
    let testTree = fromList [5, 3, 8, 1, 4, 7, 9, 2]
    putStrLn $ "测试树: " ++ show (toList testTree)
    putStrLn $ "树的大小: " ++ show (size testTree)
    
    putStrLn "\n--- rank 测试 ---"
    putStrLn $ "rank 1 (应为 1): " ++ show (rank 1 testTree)
    putStrLn $ "rank 4 (应为 4): " ++ show (rank 4 testTree)
    putStrLn $ "rank 9 (应为 8): " ++ show (rank 9 testTree)  -- 9是第8个元素（没有6）
    putStrLn $ "rank 6 (不在树中，应为 6): " ++ show (rank 6 testTree)
    putStrLn $ "rank 10 (不在树中，应为 9): " ++ show (rank 10 testTree)  -- 10会排在最后，即第9位
    
    putStrLn "\n--- atRank 测试 ---"
    putStrLn $ "atRank 1 (应为 Just 1): " ++ show (atRank 1 testTree)
    putStrLn $ "atRank 4 (应为 Just 4): " ++ show (atRank 4 testTree)
    putStrLn $ "atRank 8 (应为 Just 9): " ++ show (atRank 8 testTree)  -- 第8个元素是9
    putStrLn $ "atRank 9 (应为 Nothing): " ++ show (atRank 9 testTree)  -- 没有第9个元素
    putStrLn $ "atRank 0 (应为 Nothing): " ++ show (atRank 0 testTree)
    putStrLn $ "atRank 10 (应为 Nothing): " ++ show (atRank 10 testTree)
    
    putStrLn "\n--- 重复值排名测试 ---"
    let dupTree = fromList [1, 1, 2, 2, 2, 3]
    putStrLn $ "重复树: " ++ show (toList dupTree)
    putStrLn $ "树的大小: " ++ show (size dupTree)
    
    putStrLn "\n--- 重复值 rank 测试 ---"
    putStrLn $ "rank 1 (应为 1): " ++ show (rank 1 dupTree)   -- 第一个1的排名是1
    putStrLn $ "rank 2 (应为 3): " ++ show (rank 2 dupTree)   -- 第一个2的排名是3
    putStrLn $ "rank 3 (不在树中，应为 6): " ++ show (rank 3 dupTree)  -- 3会排在最后，即第6位
    putStrLn $ "rank 4 (不在树中，应为 7): " ++ show (rank 4 dupTree)  -- 4会排在3后面，即第7位
    
    putStrLn "\n--- 重复值 atRank 测试 ---"
    putStrLn $ "atRank 1 (应为 Just 1): " ++ show (atRank 1 dupTree)
    putStrLn $ "atRank 2 (应为 Just 1): " ++ show (atRank 2 dupTree)
    putStrLn $ "atRank 3 (应为 Just 2): " ++ show (atRank 3 dupTree)
    putStrLn $ "atRank 4 (应为 Just 2): " ++ show (atRank 4 dupTree)
    putStrLn $ "atRank 5 (应为 Just 2): " ++ show (atRank 5 dupTree)
    putStrLn $ "atRank 6 (应为 Just 3): " ++ show (atRank 6 dupTree)
    
    putStrLn "\n--- rank/atRank 互为逆运算验证 ---"
    let elements = [1,2,3,5,7,8,9]
    let ranks = map (\x -> rank x testTree) elements
    let recovered = map (\r -> atRank r testTree) ranks
    putStrLn $ "原始元素: " ++ show elements
    putStrLn $ "对应排名: " ++ show ranks
    putStrLn $ "从排名恢复: " ++ show recovered
    putStrLn $ "是否完全恢复: " ++ show (map (\(Just x) -> x) recovered == elements)
