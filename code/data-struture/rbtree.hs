import Data.Binary.Get (isEmpty)
import Control.Monad.Fix (fix)


-- | 定义颜色
-- | R: 红色, B: 黑色, BB: 双黑色(仅用于删除修复的临时状态)
data Color = R | B | BB deriving (Eq, Show)

-- | 定义红黑树节点
-- | 'Empty' 代表空叶子节点（NIL），在红黑树中视为黑色
-- | 'BBEmpty' 代表双黑空节点，用于删除修复
-- | 'Node c l k r' 代表一个节点，c 是颜色，k 是键值，l 和 r 分别是左右子树
data RBTree a = Empty 
              | BBEmpty 
              | Node Color (RBTree a) a (RBTree a)
              deriving (Eq)

-- | 自定义 Show 实例
instance (Show a) => Show (RBTree a) where
    show Empty = "NIL"
    show BBEmpty = "BB"  -- 双黑空节点
    show (Node c l k r) = 
        let colorStr = case c of 
              R -> "R" 
              B -> "B" 
              BB -> "BB"  -- 双黑色
        in "(" ++ colorStr ++ ":" ++ show k ++ " " ++ show l ++ " " ++ show r ++ ")"

-- | 旋转操作

-- | 左旋操作
-- | rotateLeft (Node c l k (Node rc rl rk rr)) = Node rc (Node c l k rl) rk rr
rotateLeft :: RBTree a -> RBTree a
rotateLeft Empty = Empty  -- 空树无法旋转
rotateLeft BBEmpty = BBEmpty  -- 双黑空节点无法旋转
rotateLeft (Node c l k r) =
    case r of
        Empty -> Node c l k r  -- 右子树为空，无法旋转
        BBEmpty -> Node c l k r  -- 右子树为双黑空节点，无法旋转
        Node rc rl rk rr -> 
            Node rc (Node c l k rl) rk rr  -- 新的根节点是原来的右子节点，保留其颜色rc

-- | 右旋操作
-- | rotateRight (Node c (Node lc ll lk lr) k r) = Node lc ll lk (Node c lr k r)
rotateRight :: RBTree a -> RBTree a
rotateRight Empty = Empty  -- 空树无法旋转
rotateRight BBEmpty = BBEmpty  -- 双黑空节点无法旋转
rotateRight (Node c l k r) =
    case l of
        Empty -> Node c l k r  -- 左子树为空，无法旋转
        BBEmpty -> Node c l k r  -- 左子树为双黑空节点，无法旋转
        Node lc ll lk lr -> 
            Node lc ll lk (Node c lr k r)  -- 新的根节点是原来的左子节点，保留其颜色lc

-- | 辅助函数：获取节点颜色 (Empty 视为 B, BBEmpty 视为 BB)
color :: RBTree a -> Color
color Empty = B
color BBEmpty = BB
color (Node c _ _ _) = c

-- | 辅助函数：将树的根染黑
makeBlack :: RBTree a -> RBTree a
makeBlack Empty = Empty
makeBlack BBEmpty = Empty  -- 双黑空节点变回普通空节点
makeBlack (Node _ l k r) = Node B l k r

-- ================= | 插入操作 | =================

-- | 顶层插入函数
insert :: (Ord a) => a -> RBTree a -> RBTree a
insert x t = makeBlack (ins x t)

-- | 内部递归插入函数
ins :: (Ord a) => a -> RBTree a -> RBTree a
ins x Empty = Node R Empty x Empty  -- 新节点默认为红色
ins x (Node c l k r)
    | x < k     = balance (Node c (ins x l) k r)
    | x > k     = balance (Node c l k (ins x r))
    | otherwise = Node c l k r  -- 重复值不插入

-- | 插入后的平衡函数
balance :: RBTree a -> RBTree a
-- 根据图4.6，只有四种需要修复的情况
-- Case 1: 左左情况 (LL)
balance (Node B (Node R (Node R a x b) y c) z d) = 
    Node R (Node B a x b) y (Node B c z d)
-- Case 2: 左右情况 (LR)
balance (Node B (Node R a x (Node R b y c)) z d) = 
    Node R (Node B a x b) y (Node B c z d)
-- Case 3: 右左情况 (RL)
balance (Node B a x (Node R (Node R b y c) z d)) = 
    Node R (Node B a x b) y (Node B c z d)
-- Case 4: 右右情况 (RR)
balance (Node B a x (Node R b y (Node R c z d))) = 
    Node R (Node B a x b) y (Node B c z d)
-- 其他情况保持不变
balance t = t

-- ================= | 删除操作 | =================

-- | 顶层删除函数
delete :: (Ord a) => a -> RBTree a -> RBTree a
delete x t = makeBlack (del x t)

_isEmpty :: RBTree a -> Bool
_isEmpty Empty = True
_isEmpty BBEmpty = True
_isEmpty _ = False

shiftB:: RBTree a -> RBTree a
shiftB (Node B l k r) = Node BB l k r
-- shiftB (Node R l k r) = Node B l k r
-- shiftB (Node BB l k r) = Node B l k r -- 提升黑色
shiftB (Node _ l k r) = Node B l k r -- 提升黑色
shiftB Empty = BBEmpty
shiftB BBEmpty = Empty -- 双黑空节点变回普通空节点 ?? why

-- | 找到最小值
treeMin :: (Ord a) => RBTree a -> a
treeMin Empty = error "treeMin: cannot get minimum from Empty"
treeMin BBEmpty = error "treeMin: cannot get minimum from BBEmpty"
treeMin (Node _ Empty k _) = k
treeMin (Node _ l _ _) = treeMin l

-- | 内部递归删除函数
del :: (Ord a) => a -> RBTree a -> RBTree a
del _ Empty = Empty
del x (Node c l k r)
    | x < k     = fixBB c (del x l) k r
    | x > k     = fixBB c l k (del x r)
    | _isEmpty l = if c == B then shiftB r else r -- 删除根节点, 变成双黑
    | _isEmpty r = if c == B then shiftB l else l -- 删除根节点, 变成双黑
    | otherwise = fixBB c l m (del m r) where m = treeMin r

-- | 删除当前子树的根节点
remove :: (Ord a) => RBTree a -> RBTree a
-- Case 0: 移除红色叶子
remove (Node R Empty _ Empty) = Empty
-- Case 1: 移除黑色叶子，产生双黑状态
remove (Node B Empty _ Empty) = BBEmpty
-- Case 2: 节点只有一个孩子
remove (Node B Empty _ (Node R l k r)) = Node B l k r
remove (Node B (Node R l k r) _ Empty) = Node B l k r
-- Case 3: 节点有两个孩子
remove (Node c l _ r) = 
    let (k', r') = getMin r
    in fixBB c l k' r'

-- | 找到后继节点和删除后的子树
getMin :: (Ord a) => RBTree a -> (a, RBTree a)
getMin Empty = error "getMin: cannot get minimum from Empty"
getMin BBEmpty = error "getMin: cannot get minimum from BBEmpty"
getMin (Node R Empty k r) = (k, r)
getMin (Node B Empty k r) = (k, r)
getMin (Node c l k r) = 
    let (k', l') = getMin l
    in (k', fixBB c l' k r)  -- 修复双黑状态

-- | 修复双黑状态的函数
-- 根据图片中的算法实现
fixBB :: Color -> RBTree a -> a -> RBTree a -> RBTree a
-- case 1 BB右兄弟黑,右兄弟的右孩子红 , 远点
fixBB color a@(Node BB _ _ _ ) x (Node B b y (Node R c z d)) 
  = Node color (Node B (shiftB a) x b) y (Node B c z d)
fixBB color BBEmpty x (Node B b y (Node R c z d))
  = Node color (Node B (shiftB BBEmpty) x b) y (Node B c z d)
-- case 2 BB左兄弟黑,左兄弟的左孩子红 , 远点
fixBB color (Node B (Node R a x b) y c) z d@(Node BB _ _ _)
  = Node color (Node B a x b) y (Node B c z (shiftB d))
fixBB color (Node B (Node R a x b) y c) z BBEmpty
  = Node color (Node B a x b) y (Node B c z (shiftB BBEmpty))
-- case 3 BB右兄弟黑,右兄弟的左孩子红(右孩子一定黑,不然符合远点) , 近点
fixBB color a@(Node BB _ _ _) x (Node B (Node R b y c) z d)
  = Node color (Node B (shiftB a) x b) y (Node B c z d)
fixBB color BBEmpty x (Node B (Node R b y c) z d)
  = Node color (Node B (shiftB BBEmpty) x b) y (Node B c z d)
-- case 4 BB左兄弟黑,左兄弟的右孩子红(左孩子一定黑,不然符合远点) , 近点
fixBB color (Node B a x (Node R b y c)) z d@(Node BB _ _ _)
  = Node color (Node B a x b) y (Node B c z (shiftB d))
fixBB color (Node B a x (Node R b y c)) z BBEmpty
  = Node color (Node B a x b) y (Node B c z (shiftB BBEmpty))
-- case 5 BB右兄弟红,左旋, 变成case 1, 右兄弟黑
fixBB B a@(Node BB _ _ _) x (Node R b y c)
  = fixBB B (fixBB R a x b) y c
fixBB B a@BBEmpty x (Node R b y c)
  = fixBB B (fixBB R a x b) y c
-- case 6 BB左兄弟红,右旋, 变成case 2, 左兄弟黑
fixBB B (Node R a x b) y c@(Node BB _ _ _) 
  = fixBB B a x (fixBB R b y c) 
fixBB B (Node R a x b) y c@BBEmpty 
  = fixBB B a x (fixBB R b y c) 
-- case 7 BB右兄弟黑,右兄弟的两个孩子黑,提升黑色 
fixBB color a@(Node BB _ _ _) x (Node B b y c) 
  = shiftB (Node color (shiftBlack a) x (Node R b y c)) 
fixBB color BBEmpty x (Node B b y c) 
  = shiftB (Node color Empty x (Node R b y c)) 
-- case 8 BB左兄弟黑,左兄弟的两个孩子黑,提升黑色
fixBB color (Node B a x b) y c@(Node BB _ _ _) 
  = shiftB (Node color (Node R a x b) y (shiftBlack c)) 
fixBB color (Node B a x b) y BBEmpty 
  = shiftB (Node color (Node R a x b) y Empty) 
-- case 9 其它情况,保持不变
fixBB color l k r = Node color l k r


-- | 工具函数

-- | 中序遍历
inorder :: RBTree a -> [a]
inorder Empty = []
inorder BBEmpty = []
inorder (Node _ l k r) = inorder l ++ [k] ++ inorder r

-- | 计算树的高度
height :: RBTree a -> Int
height Empty = 0
height BBEmpty = 0
height (Node _ l k r) = 1 + max (height l) (height r)

-- | 从列表构建红黑树
fromList :: (Ord a) => [a] -> RBTree a
fromList = foldr insert Empty

-- | 测试函数
testRotations :: IO ()
testRotations = do
    putStrLn "--- 旋转操作测试 ---"
    
    let tree = Node B (Node B Empty 1 Empty) 2 (Node B Empty 3 Empty)
    putStrLn $ "原始树: " ++ show tree
    
    let leftRotated = rotateLeft tree
    putStrLn $ "左旋后: " ++ show leftRotated
    
    let tree2 = Node B (Node B Empty 1 (Node B Empty 2 Empty)) 3 Empty
    putStrLn $ "原始树2: " ++ show tree2
    
    let rightRotated = rotateRight tree2
    putStrLn $ "右旋后: " ++ show rightRotated

-- | 测试插入操作
testInsert :: IO ()
testInsert = do
    putStrLn "--- 插入操作测试 ---"
    
    let values = [1,2,3,4,5,6,7]
    let tree = fromList values
    putStrLn $ "插入 " ++ show values ++ " 后的树: " ++ show tree
    putStrLn $ "中序遍历: " ++ show (inorder tree)
    putStrLn $ "树高度: " ++ show (height tree)

-- | 测试删除操作
testDelete :: IO ()
testDelete = do
    putStrLn "--- 删除操作测试 ---"
    
    -- 测试简单的删除
    let simpleTree = Node B (Node B Empty 1 Empty) 2 (Node B Empty 3 Empty)
    putStrLn $ "简单树: " ++ show simpleTree
    
    let tree1 = delete 1 simpleTree
    putStrLn $ "删除 1 后: " ++ show tree1
    putStrLn $ "中序遍历: " ++ show (inorder tree1)
    
    -- 测试删除叶子节点
    let leafTree = Node B Empty 5 Empty
    putStrLn $ "叶子树: " ++ show leafTree
    
    let tree2 = delete 5 leafTree
    putStrLn $ "删除 5 后: " ++ show tree2
    putStrLn $ "中序遍历: " ++ show (inorder tree2)

-- | 验证红黑树性质
validateRBTree :: (Show a, Ord a) => RBTree a -> Bool
validateRBTree t = 
    let (valid, _) = checkRBTree t
    in valid

-- | 检查红黑树性质，返回 (是否有效, 黑高)
checkRBTree :: (Ord a) => RBTree a -> (Bool, Int)
checkRBTree Empty = (True, 1)  -- 空树是黑色，黑高为1
checkRBTree BBEmpty = (True, 1)  -- 双黑空节点在验证时视为普通空节点
checkRBTree (Node c l k r) =
    let (leftValid, leftBlack) = checkRBTree l
        (rightValid, rightBlack) = checkRBTree r
        nodeValid = leftValid && rightValid
        blackValid = case c of
            R -> color l /= R && color r /= R  -- 红色节点的孩子必须是黑色
            _ -> True
        blackHeightValid = leftBlack == rightBlack
        nodeBlack = if c == B then leftBlack + 1 else leftBlack
    in (nodeValid && blackValid && blackHeightValid, nodeBlack)

-- | 更详细的测试
testRBTreeProperties :: IO ()
testRBTreeProperties = do
    putStrLn "--- 红黑树性质验证 ---"
    
    -- 测试四种平衡情况
    putStrLn "\n测试四种平衡情况:"
    
    -- Case 1: LL情况
    let case1 = Node B (Node R (Node R Empty 1 Empty) 2 Empty) 3 Empty
    putStrLn $ "LL情况前: " ++ show case1
    let balanced1 = balance case1
    putStrLn $ "LL情况后: " ++ show balanced1
    putStrLn $ "LL情况中序: " ++ show (inorder balanced1)
    
    -- Case 2: LR情况
    let case2 = Node B (Node R Empty 1 (Node R Empty 2 Empty)) 3 Empty
    putStrLn $ "LR情况前: " ++ show case2
    let balanced2 = balance case2
    putStrLn $ "LR情况后: " ++ show balanced2
    putStrLn $ "LR情况中序: " ++ show (inorder balanced2)
    
    -- Case 3: RL情况 - 修正测试用例
    let case3 = Node B Empty 1 (Node R (Node R Empty 2 Empty) 3 Empty)
    putStrLn $ "RL情况前: " ++ show case3
    let balanced3 = balance case3
    putStrLn $ "RL情况后: " ++ show balanced3
    putStrLn $ "RL情况中序: " ++ show (inorder balanced3)
    
    -- Case 4: RR情况 - 修正测试用例
    let case4 = Node B Empty 1 (Node R Empty 2 (Node R Empty 3 Empty))
    putStrLn $ "RR情况前: " ++ show case4
    let balanced4 = balance case4
    putStrLn $ "RR情况后: " ++ show balanced4
    putStrLn $ "RR情况中序: " ++ show (inorder balanced4)
    
    -- 验证性质
    putStrLn "\n验证性质:"
    putStrLn $ "LL情况是否满足红黑树性质: " ++ show (validateRBTree balanced1)
    putStrLn $ "LR情况是否满足红黑树性质: " ++ show (validateRBTree balanced2)
    putStrLn $ "RL情况是否满足红黑树性质: " ++ show (validateRBTree balanced3)
    putStrLn $ "RR情况是否满足红黑树性质: " ++ show (validateRBTree balanced4)
    
    -- 测试插入后的树是否满足性质
    putStrLn "\n测试插入操作的红黑树性质:"
    let insertTree = fromList [1,2,3,4,5,6,7]
    putStrLn $ "插入树是否满足红黑树性质: " ++ show (validateRBTree insertTree)

-- | 主测试函数
main :: IO ()
main = do
    testRotations
    putStrLn ""
    testInsert
    putStrLn ""
    testDelete
    putStrLn ""
    testRBTreeProperties