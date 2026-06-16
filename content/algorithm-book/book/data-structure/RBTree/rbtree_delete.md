实现红黑树的删除操作确实是它最复杂的部分，远比插入要精妙。它涉及多种情况，核心是处理“双重黑色”（Double Black）或“黑高缺陷”（Black-Deficient）的问题。

当一个**黑色**节点被删除时，它所在的路径会比其他路径少一个黑色节点，这就违反了红黑树的属性 5 (黑高)。我们必须修复这个缺陷。

下面是 `delete` 操作的完整 Haskell 实现，它依赖于你之前定义的 `Color` 和 `RBTree` 类型。

-----

### 🌳 红黑树删除操作 (Haskell 实现)

我们将需要你之前定义的 `Color` 和 `RBTree`，以及 `Show` 实例（为了方便查看）：

```haskell
-- | (已定义)
data Color = R | B deriving (Eq, Show)
data RBTree a = Empty
              | Node Color (RBTree a) a (RBTree a)
              deriving (Eq)

-- | (来自上一问)
instance (Show a) => Show (RBTree a) where
  show Empty = "E"
  show (Node c l k r) = 
    "N " ++ show c ++ " (" ++ show k ++ ") " ++ show l ++ " " ++ show r

-- | 辅助函数：获取节点颜色 (Empty 视为 B)
color :: RBTree a -> Color
color Empty = B
color (Node c _ _ _) = c

-- | 辅助函数：将树的根染黑
makeBlack :: RBTree a -> RBTree a
makeBlack Empty = Empty
makeBlack (Node _ l k r) = Node B l k r
```

#### 删除操作核心代码

```haskell
-- | 1. 顶层删除函数
delete :: (Ord a) => a -> RBTree a -> RBTree a
delete x t = makeBlack (del x t)

-- | 2. 内部递归删除函数 `del`
-- | 查找节点，并在递归返回时调用平衡函数
del :: (Ord a) => a -> RBTree a -> RBTree a
del _ Empty = Empty
del x (Node c l k r)
    -- 递归查找
    | x < k     = balanceL c (del x l) k r -- 从左删除, 之后平衡
    | x > k     = balanceR c l k (del x r) -- 从右删除, 之后平衡
    -- 找到了 (x == k)
    | otherwise = remove (Node c l k r)

-- | 3. `remove`: 删除当前子树的根节点
-- | (这是在 del 找到匹配键 x 之后调用的)
remove :: (Ord a) => RBTree a -> RBTree a
-- Case 0: 移除红色叶子, 没有影响
remove (Node R Empty _ Empty) = Empty
-- Case 1: 移除黑色叶子, 产生"黑高缺陷", 返回 Empty, 
--         由上层 (调用者) 的 `balanceL` / `balanceR` 来修复
remove (Node B Empty _ Empty) = Empty
-- Case 2: 节点只有一个孩子 (另一个必为 Empty)
--         孩子必须是红色 (否则黑高不等), 将孩子提上并染黑
remove (Node B Empty _ (Node R l k r)) = Node B l k r
remove (Node B (Node R l k r) _ Empty) = Node B l k r
-- Case 3: 节点有两个孩子 (l 和 r 都不为 Empty)
remove (Node c l _ r)   =
    let (k', r') = getMin r -- 1. 找到右子树的最小(后继)节点 k'
                           -- 2. r' 是删除了 k' 后的新右子树
    in balanceR c l k' r'  -- 3. 用 k' 替换 k, 
                           -- 4. r' 可能有黑高缺陷, `balanceR` 负责修复

-- | 4. `getMin`: 找到后继节点 (k') 和删除后继节点后的子树 (r')
-- | 返回: (最小键, 删除了最小键的树)
getMin :: (Ord a) => RBTree a -> (a, RBTree a)
getMin (Node R Empty k r) = (k, r)
getMin (Node B Empty k r) = (k, r)
getMin (Node c l k r) = 
    let (k', l') = getMin l -- 递归向左
    in (k', balanceL c l' k r) -- 回溯时平衡

-- | 5. 核心：删除-平衡函数
-- | `balanceL c l k r`
-- | 当 `l` (左子树) 刚刚删除了一个黑色节点, 导致黑高-1时, 
-- | `k` 是父节点, `r` 是兄弟子树.
balanceL :: Color -> RBTree a -> a -> RBTree a -> RBTree a

-- === `balanceL` (左子树 l 出现缺陷) ===

-- Case 1: 兄弟节点 `r` 是红色
balanceL p l k (Node R rl rk rr) =
    Node R (Node p l k rl) rk rr

-- 兄弟节点 `r` 是黑色 (以下所有情况)

-- Case 2: 兄弟 `r` 黑色, 兄弟的两个孩子 (`rl`, `rr`) 都是黑色
--         我们将缺陷"上移"
balanceL p l k (Node B rl rk rr)
    | color rl == B && color rr == B = 
        balL p l k (Node R rl rk rr) -- `balL` 是处理颜色上推的辅助函数
    
-- Case 3: 兄弟 `r` 黑色, 兄弟的左孩子 `rl` 是红色 (RL 情况)
balanceL p l k (Node B (Node R rll rlk rlr) rk rr) =
    Node p (Node B l k rll) rlk (Node B rlr rk rr)

-- Case 4: 兄弟 `r` 黑色, 兄弟的右孩子 `rr` 是红色 (RR 情况)
balanceL p l k (Node B rl rk rr@(Node R _ _ _)) =
    Node p (Node B l k rl) rk (makeBlack rr)


-- === `balanceR` (右子树 r 出现缺陷) ===
-- | (与 `balanceL` 对称)
balanceR :: Color -> RBTree a -> a -> RBTree a -> RBTree a

-- Case 1: 兄弟节点 `l` 是红色
balanceR p (Node R ll lk lr) k r = 
    Node R ll lk (Node p lr k r)

-- 兄弟节点 `l` 是黑色 (以下所有情况)

-- Case 2: 兄弟 `l` 黑色, 兄弟的两个孩子 (`ll`, `lr`) 都是黑色
balanceR p (Node B ll lk lr) k r
    | color ll == B && color lr == B = 
        balR p (Node R ll lk lr) k r

-- Case 3: 兄弟 `l` 黑色, 兄弟的右孩子 `lr` 是红色 (LR 情况)
balanceR p (Node B ll lk (Node R lrl lrk lrr)) k r =
    Node p (Node B ll lk lrl) lrk (Node B lrr k r)

-- Case 4: 兄弟 `l` 黑色, 兄弟的左孩子 `ll` 是红色 (LL 情况)
balanceR p (Node B ll@(Node R _ _ _) lk lr) k r =
    Node p (makeBlack ll) lk (Node B lr k r)


-- | 6. `balL` 和 `balR` 辅助函数 (用于 Case 2)
-- | 当兄弟和兄弟的孩子都为黑时，它们处理缺陷的"上传"
-- | `p` 是原父节点的颜色

-- `l` 是缺陷侧, `r` 是兄弟侧 (已被染红)
balL :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balL B l k r = Node B l k r -- 父B, 兄弟B->R. 缺陷上传 (整个子树仍是"双黑")
balL R l k r = Node B l k r -- 父R, 兄弟B->R. 父R->B, 缺陷被吸收

-- `r` 是缺陷侧, `l` 是兄弟侧 (已被染红)
balR :: Color -> RBTree a -> a -> RBTree a -> RBTree a
balR B l k r = Node B l k r -- 父B, 兄弟B->R. 缺陷上传
balR R l k r = Node B l k r -- 父R, 兄弟B->R. 父R->B, 缺陷被吸收
```

-----

### 💡 操作原理详解

这套代码的实现非常精妙，它没有显式地传递“我是否缺陷”这个布尔值，而是通过函数式编程的结构，在递归回溯时**强制**调用平衡函数 (`balanceL`, `balanceR`)。

如果子树（例如 `del x l` 的结果）没有缺陷，`balanceL` 会巧妙地什么都不做（匹配到默认情况，但这里我们穷尽了所有情况）；如果它**有**缺陷（即它返回了一个比原来“黑高-1”的树，比如 `Empty` 替换了 `Node B Empty _ Empty`），`balanceL` 就会被触发。

`balanceL`/`balanceR` 的 `p` 参数是**原父节点**的颜色。这是修复黑高缺陷的关键。

**`balanceL` (左子树 `l` 缺陷，兄弟 `r` 在右侧)**

1.  **Case 1: 兄弟 `r` 是红色。**

      * **操作:** 旋转 `k` 和 `r`，并将 `k` 染成红色，`r` 染成黑色（如代码中 `Node R ...` 的结构所示）。
      * **目的:** 这会将一个**黑色**节点（`rl`）变成 `l` 的新兄弟。现在问题转化为了 Case 2, 3, 或 4，然后我们再次在 `l` 上（在新的父节点 `k` 下）解决缺陷。

2.  **Case 2: 兄弟 `r` 是黑色，`r` 的两个孩子都是黑色。**

      * **操作:** 将兄弟 `r` 染成**红色**（如 `balL` 辅助函数中 `Node R rl rk rr` 所示）。
      * **目的:** 这平衡了 `l` 和 `r` 两侧的黑高（`l` 少 1，`r` 也通过变红来“少 1”）。
      * **结果:** 整个以 `k` 为根的子树（`Node p l k r`）的黑高**整体减少了 1**。
      * `balL` 辅助函数检查 `p` (父节点 `k` 的颜色)：
          * 如果 `p` 是**红色** (R): `balL R ...` 返回 `Node B ...`。我们将父节点 `p` 染黑，缺陷被完全吸收。
          * 如果 `p` 是**黑色** (B): `balL B ...` 返回 `Node B ...`。父节点 `p` 保持黑色，但整个子树仍然是缺陷的。这个“双黑”问题被**传递到上一层**，由上一层的 `balanceL` / `balanceR` 继续修复。

3.  **Case 3: 兄弟 `r` 黑色，`r` 的左孩子 `rl` 红色 (RL)。**

      * **操作:** 在 `r` 上右旋，再在 `k` 上左旋 (一次性完成)。
      * **目的:** 旋转并重新着色，将 `rl` 提升为新根，`k` 和 `r` 变为其黑色孩子。黑高被修复。

4.  **Case 4: 兄弟 `r` 黑色，`r` 的右孩子 `rr` 红色 (RR)。**

      * **操作:** 在 `k` 上左旋。
      * **目的:** 旋转并重新着色，`r` 变为新根，`k` 和 `rr`（染黑后）为其孩子。黑高被修复。

**`remove` 和 `getMin` 的协同**

`remove` 函数在有两个孩子时，需要用后继节点 `k'` 替换自己。它通过 `getMin r` 得到 `k'` 和 `r'` (删除了 `k'` 的 `r`)。

`getMin` 在递归向左查找时，返回时会调用 `balanceL`。这意味着 `getMin` 在删除（`Node B Empty k r`）时，会触发平衡，并返回一个**已经平衡好**的 `r'`。

这套逻辑是函数式红黑树最精妙但也最难理解的部分。