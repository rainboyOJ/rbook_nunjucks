import Prelude hiding (succ)

-- | 定义二叉搜索树 (BST)
-- | 'Empty' 代表空树
-- | 'Node l k r' 代表一个节点，k 是键值，l 和 r 分别是左右子树
data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Eq) -- 移除 Show 以便自定义

-- | 自定义 Show 实例，使其更易读
instance (Show a) => Show (Tree a) where
    show = showTree 0
      where
        showTree _ Empty = "Empty"
        showTree level (Node l k r) =
            "Node " ++ show k ++ "\n" ++
            indent ++ "L: " ++ showTree (level + 1) l ++ "\n" ++
            indent ++ "R: " ++ showTree (level + 1) r
          where
            indent = replicate ((level + 1) * 4) ' '

-- | 辅助函数：从列表构建树
-- | 使用 foldl 和 insert 来构建
fromList :: (Ord a) => [a] -> Tree a
fromList = foldl (flip insert) Empty

-- | 辅助函数：将树转为中序列表 (L-K-R)
toList :: Tree a -> [a]
toList = foldLKR (\acc k -> acc ++ [k]) []

-- | ---------------------------------------------------------------------------
-- | 核心功能
-- | ---------------------------------------------------------------------------

-- | 向 BST 中插入一个值
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty -- 基本情况：在空树上插入，返回一个新节点
insert x (Node l k r)
    | x < k     = Node (insert x l) k r -- x 小于当前键，递归插入左子树
    | x > k     = Node l k (insert x r) -- x 大于当前键，递归插入右子树
    | otherwise = Node l k r            -- x 等于当前键，树不变 (BST 通常不存储重复值)

-- | 在 BST 中查找一个值
find :: (Ord a) => a -> Tree a -> Bool
find _ Empty = False -- 在空树中查找，失败
find x (Node l k r)
    | x < k     = find x l -- x 小于当前键，递归查找左子树
    | x > k     = find x r -- x 大于当前键，递归查找右子树
    | otherwise = True       -- 找到 (k == x)

-- | 查找 BST 中的最小元素
findMin :: Tree a -> Maybe a
findMin Empty = Nothing -- 空树没有最小值
findMin (Node Empty k _) = Just k -- 没有左子树，当前节点就是最小值
findMin (Node l _ _) = findMin l -- 否则，最小值在左子树中

-- | 查找 BST 中的最大元素
findMax :: Tree a -> Maybe a
findMax Empty = Nothing
findMax (Node _ k Empty) = Just k
findMax (Node _ _ r) = findMax r

-- | 从 BST 中删除一个值
delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Empty = Empty -- 从空树删除，返回空树
delete x (Node l k r)
    | x < k     = Node (delete x l) k r -- 要删除的值在左子树
    | x > k     = Node l k (delete x r) -- 要删除的值在右子树
    | otherwise = del l r               -- 找到了要删除的节点 (k == x)，调用 del
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
            in Node l k' r'

-- | 助手函数：找到并删除最小节点 (用于 'delete')
-- | 返回 (最小值的键, 删除了最小值的子树)
deleteMin :: Tree a -> (a, Tree a)
deleteMin Empty = error "deleteMin: called on empty tree" -- 逻辑上不应该发生
deleteMin (Node Empty k r) = (k, r) -- 找到了最小值 (k)，用其右子树 (r) 替换
deleteMin (Node l k r) =
    let (minKey, newL) = deleteMin l -- 最小值在左子树
    in (minKey, Node newL k r)

-- | 在纯函数式 BST 中查找后继
-- 需要整个树和目标键值
-- `best` 参数在向下递归时，记录“目前为止最好的后继候选”
succ :: (Ord a) => a -> Tree a -> Maybe a
succ x tree = go tree Nothing
  where
    go Empty _ = Nothing -- 树为空或未找到
    go (Node l k r) best
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
    go (Node l k r) best
        | x < k     = go l best     -- 前驱在左子树, best 不变
        | x > k     = go r (Just k) -- k 是一个可能的前驱, 在右子- 树找更接近的
        | otherwise = -- 找到了键 x (k == x)
            case l of
                Empty -> best      -- 没有左子树, best (最近的右拐祖先) 就是前驱
                _     -> findMax l -- 有左子树, 前驱是左子树的最大值

-- | ---------------------------------------------------------------------------
-- | 映射和折叠 (Map & Fold)
-- | ---------------------------------------------------------------------------

-- | 对树中的每个元素应用一个函数
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node l k r) = Node (mapTree f l) (f k) (mapTree f r)

-- | 折叠 (fold) 树 (Catamorphism)
foldTree :: (b -> c -> b -> b) -> (a -> c) -> b -> Tree a -> b
foldTree _ _ z Empty = z
foldTree g f z (Node l k r) =
    g (foldTree g f z l) (f k) (foldTree g f z r)

-- | 这是一个 *反向* 中序 (R-K-L, Right-Key-Left) 遍历折叠
foldRKL :: (a -> b -> b) -> b -> Tree a -> b
foldRKL _ z Empty = z
foldRKL f z (Node l k r) = foldRKL f (k `f` (foldRKL f z r)) l

-- | 一个更有用的 *正向* 中序 (L-K-R, Left-Key-Left) 遍历折叠
foldLKR :: (b -> a -> b) -> b -> Tree a -> b
foldLKR f z = go z
  where
    go acc Empty = acc
    go acc (Node l k r) =
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
