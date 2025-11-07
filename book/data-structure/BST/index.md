---
title: "二叉查找树"
date: 2025-11-06 14:30
toc: true
tags: [""]
draft: true
categories: [""]
log: 
  - date: 2025-11-06 14:30
    comment: "初稿"
---

[[TOC]]

## BST 定义

BST的随机生成

!!! definition 定义
二叉搜索树是一种特殊的二叉树,它的节点的值可以进行"比较"(抽象的比较),并且满足以下性质:

1. 如果一个节点的左子树不为空,那么左子树上所有节点的值都小于该节点的值
2. 如果一个节点的右子树不为空,那么右子树上所有节点的值都大于该节点的值
3. 它的左右子树也都是二叉搜索树

!!!


```cpp [g1:cpp]
struct Node {
    int key;
    Node *left, *right, *parent;
    Node(int key) : key(key), left(nullptr), right(nullptr), parent(nullptr) {}
};
```

```haskell [g1:haskell]
data Tree a = Empty | Node (Tree a) a (Tree a) deriving (Eq, Show)
```


## 插入


### 🌳 二叉搜索树操作的数学原理与证明

二叉搜索树（BST）的核心在于其**不变量（Invariant）**：对于树中的任意节点 `N`，其左子树中所有节点的值都小于 `N` 的值，其右子树中所有节点的值都大于 `N` 的值。这个不变量是所有操作正确性的基础。

树的高度 `h` 是所有操作时间复杂度的关键。
- 对于一个**完全平衡**的二叉树，其高度为 `h = O(log n)`，其中 `n` 是节点数。
- 对于一个**完全不平衡**（退化为链表）的二叉树，其高度为 `h = O(n)`。

因此，所有基本操作的性能都介于 `O(log n)` 和 `O(n)` 之间。

---

#### 1. `find(x)` - 查找操作

`find`是最简单的操作，它在树中搜索值 `x`。

`find` 操作利用了 BST 的不变量来高效地缩小搜索范围。从根节点开始，如果要查找的值 `x` 小于当前节点的值，则搜索进入左子树；如果 `x` 大于当前节点的值，则搜索进入右子树。这个过程持续进行，直到找到值 `x` 或者到达一个空的子树（表示 `x` 不在树中）。

伪代码

::: pseudocode		
\begin{algorithm}
\caption{BST FIND}
\begin{algorithmic}
\FUNCTION{Find}{Node t,var x}
  \IF{$t.val = x$}
    \RETURN TRUE
  \ELIF{ $t = \varnothing $}
    \RETURN False
  \ELIF{$x > t.val $}
    \RETURN Find(t.right,x)
  \ELSE
    \RETURN Find(t.left,x)
  \ENDIF
\ENDFUNCTION
\end{algorithmic}
\end{algorithm}
:::

**正确性证明:**
我们使用结构归纳法 (structural induction) 来证明 `find(T, x)` 的正确性。设 $R$ 为树 $T$ 的根节点，其键为 $k$，其左右子树分别为 $T_L$ 和 $T_R$。

BST 的不变量是： $\forall y \in T_L, \text{key}(y) < k$ 且 $\forall z \in T_R, \text{key}(z) > k$。

**基本情况 (Base Case):**
当树 $T$ 为空 ($T = \varnothing$) 时，`find(T, x)` 返回 `false`，这是正确的，因为空树不包含任何元素。

**归纳步骤 (Inductive Step):**
假设对于子树 $T_L$ 和 $T_R$，`find` 操作是正确的。我们需要证明 `find(T, x)` 对整个树 $T$ 也正确。

`find` 算法的逻辑如下：
1.  **若 $x = k$**: 算法返回 `true`。这显然是正确的。
2.  **若 $x < k$**: 算法递归调用 `find(T_L, x)`。根据 BST 不变量，任何在 $T_R$ 中的键都大于 $k$，所以 $x$ 不可能存在于 $T_R$ 中。因此，$x$ 存在于 $T$ 中当且仅当它存在于 $T_L$ 中。即：
    $$ (x \in T) \iff (x \in T_L) $$
    根据归纳假设，$find(T_L, x)$ 的结果是正确的，因此 $find(T, x)$ 的结果也是正确的。

3.  **若 $x > k$**: 算法递归调用 `find(T_R, x)`。同理，根据 BST 不变量，$x$ 不可能存在于 $T_L$ 中。因此，$x$ 存在于 $T$ 中当且仅当它存在于 $T_R$ 中。即：
    $$ (x \in T) \iff (x \in T_R) $$
    根据归纳假设，`find(T_R, x)` 的结果是正确的，因此 `find(T, x)` 的结果也是正确的。

综上所述，`find` 操作的正确性得以证明。其递归深度最多为树的高度 $h$，所以时间复杂度为 $O(h)$。


```cpp [g2:cpp]
struct Node {
    int key;
    Node *left, *right, *parent;
    Node(int key) : key(key), left(nullptr), right(nullptr), parent(nullptr) {}
};

//非递归实现
void find( int x, Node* t) {
    while (t != nullptr) {
      if( x < t->key)
        t = t->left;
      else if( x > t->key)
        t = t->right;
      else return true;
    }
    return false;
}

// 递归实现
bool find_dfs(int x, Node* t) {
    if (t == nullptr) {
        return false;
    }
    if (x < t->key) {
        return find_dfs(x, t->left);
    } else if (x > t->key) {
        return find_dfs(x, t->right);
    } else {
        return true;
    }
}
```

```haskell [g2:haskell]
find :: (Ord a) => a -> Tree a -> Bool
find _ Empty = False
find x (Node l k r)
    | x < k     = find x l
    | x > k     = find x r
    | otherwise = True
```

#### 2. `insert(x)` - 插入操作

**数学原理:**
插入操作首先像 `find` 一样，在树中寻找 `x` 的位置。这个搜索过程最终会到达一个 `NULL` 链接（即一个空子树），这就是 `x` 应该被插入的地方。创建一个新节点并链接到该位置。

**正确性证明 (保持 BST 不变量):**
我们通过归纳法证明 `insert` 操作会保持 BST 不变量。
- **基本情况:** 向一棵空树插入节点 `x`，会创建一个只包含 `x` 的新树。这个树显然满足 BST 不变量。
- **归纳假设:** 假设向一棵大小为 `n` 的树 `T` 中插入节点能保持 BST 不变量。
- **归纳步骤:** 考虑向大小为 `n+1` 的树 `T'` 中插入节点 `x`。`insert` 算法会沿着一条路径向下，设路径上最后一个节点为 `P`。
    - 如果 `x < P.key`，`x` 将被作为 `P` 的新左孩子插入。由于 `x < P.key`，并且路径上所有 `P` 的祖先都满足 BST 不变量，所以插入后，`x` 依然小于 `P` 和 `P` 的所有右侧祖先，且大于所有左侧祖先。
    - 如果 `x > P.key`，`x` 将被作为 `P` 的新右孩子插入，同理也保持不变量。
    - （如果 `x` 已存在，则不进行任何操作，不变量自然保持。）
由于新节点总是作为叶子节点插入，它不会改变树中任何现有节点之间的关系。它只在 `find` 路径的末端建立了一个新的、正确的父子关系。因此，BST 不变量得以保持。
时间复杂度与 `find` 相同，为 `O(h)`。



```cpp [g3:cpp]
struct Node {
    int key;
    Node *left, *right, *parent;
    Node(int key) : key(key), left(nullptr), right(nullptr), parent(nullptr) {}
};

//非递归实现
void insert( int x, Node* t) {
    Node* p = t,parent = nullptr;
    while (p != nullptr) {
      parent = p; // 记录父节点
      if( x < p->key)
        p = p->left;
      else if( x > p->key)
        p = p->right;
      else return; //do nothing
    }
    Node* newNode = new Node(x);
    newNode->parent = parent; // 设置新节点的父节点
    if (parent == nullptr) {
        t = newNode; // 树为空，新节点为根
    } else if (x < parent->key) { // 根据 BST 不变量决定插入位置
        parent->left = newNode;
    } else {
        parent->right = newNode;
    }
}

// 递归实现
void insert_dfs(int x, Node* t) {
    if (t == nullptr) {
        t = new Node(x);
        return;
    }
    if (x < t->key) {
        insert_dfs(x, t->left);
    } else if (x > t->key) {
        insert_dfs(x, t->right);
    }
    // 如果 x == t->key，不做任何操作
}
```

```haskell [g3:haskell]
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x (Node l k r)
    | x < k     = Node (insert x l) k r
    | x > k     = Node l k (insert x r)
    | otherwise = Node l k r
```
---

#### 3. `findMin()` - 查找最小值

**数学原理:**
根据 BST 不变量，对于任何节点 `N`，其左子树中的所有值都比它小。因此，树中最小的值必须位于从根开始不断向左遍历所能达到的最深节点。

**正确性证明:**
这是一个直接推论。假设最小值 `m` 不在最左侧的路径上。那么 `m` 所在的节点 `M` 必然是某个节点 `P` 的右孩子，或者它自己有一个左孩子 `L`。
- 如果 `M` 有左孩子 `L`，则 `L.key < M.key`，所以 `M` 不是最小值，矛盾。
- 如果 `M` 是 `P` 的右孩子，则 `P.key < M.key`，所以 `M` 不是最小值，矛盾。
因此，最小值必须位于没有左孩子的、最靠左的节点上。时间复杂度为 `O(h)`，即从根到最左叶子的路径长度。


```cpp
struct Node {
    int key;
    Node *left, *right, *parent;
    Node(int key) : key(key), left(nullptr), right(nullptr), parent(nullptr) {}
};

//非递归实现
int findMin(Node* t) {
    while (t->left != nullptr) {
        t = t->left;
    }
    return t->key;
}

// 递归实现
int findMin_dfs(Node* t) {
    if (t->left == nullptr) {
        return t->key;
    }
    return findMin_dfs(t->left);
}

int findMax(Node* t) {
    while (t->right != nullptr) {
        t = t->right;
    }
    return t->key;
}
```


```haskell
findMin :: Tree a -> Maybe a
findMin Empty = Nothing
findMin (Node Empty k _) = Just k
findMin (Node l _ _) = findMin l
```

---


## `succ` 后继与 `prev` 前驱

`succ` (Successor) 和 `prev` (Predecessor) 分别指 BST 中序遍历序列中一个节点的后一个节点和前一个节点。换句话说：
- **后继 (Successor)**: 树中键值**大于**当前节点键值的节点中，键值**最小**的那个。
- **前驱 (Predecessor)**: 树中键值**小于**当前节点键值的节点中，键值**最大**的那个。

这两个操作对于实现需要顺序访问的功能（如 `delete` 操作或构建有序迭代器）至关重要。`succ` 和 `prev` 的高效实现有两种思路：
1.  如果节点定义中包含 `parent` 指针，可以方便地在树中向上移动。
2.  在纯函数式、无 `parent` 指针的场景下，需要从根节点开始查找，并一路记录“候选”的后继或前驱。

---

### 1. `succ(x)` - 查找后继

**数学原理:**
对于给定的节点 `x`，其后继节点 `s` 的查找分为两种情况：

1.  **如果 `x` 存在右子树:** 那么 `x` 的后继就是其**右子树中的最小节点**。因为右子树中所有节点都比 `x` 大，而其中最小的那个自然就是紧随 `x` 之后的节点。
2.  **如果 `x` 不存在右子树:** 那么需要从 `x` 开始**向上回溯**父节点。后继是第一个“从左边分支上来的祖先节点”。换言之，我们持续向上查找，直到找到一个节点 `p`，使得当前节点是 `p` 的左子树上的点。这个 `p` 就是 `x` 的后继。如果一路向上直到根节点，当前节点都是右孩子，那么说明 `x` 是树中最大的元素，它没有后继。

> 思考🤔, 对于任意一个点x: 哪些点比x大?
> 容易想到: 在中序序列中,比x位置靠后的都比x大.
> 1. x右子树上的点
> 2. 考虑x的祖先u
>     1. 如果x在u的右子树上,那么x > u
>     1. 如果x在u的左子树上,那么x < u,我们称这些祖先点为**左祖先点**
>     2. 离x越近的左祖先点,越小
> anc1(x) anc2(x)  都是x的 左祖先结点,且 dep(anc1) < dep(anc2) ,那说明anc1 离 x比较远, 那么 anc1 是anc2的左祖先结点(比较容易证明,只要证明,x的父亲在anc1的左子树上,然后归纳)

**正确性证明:**
设要查找后继的节点为 `x`。

**情况 1: `x` 有右子树 `R_x`**
- 根据 BST 不变量，`R_x` 中的所有节点的键值都大于 `x.key`。
- 同时，任何不属于 `x` 的子树且键值大于 `x.key` 的节点（即 `x` 的某个祖先 `A`），其键值必然也大于 `R_x` 中所有节点的键值（因为 `x` 及其子树 `R_x` 都位于 `A` 的左子树中）。
- 因此，大于 `x.key` 的最小键值必然存在于 `R_x` 中。这个值就是 `R_x` 的最小值。
- 算法通过 `findMin(x.right)` 找到此节点，是正确的。

**情况 2: `x` 没有右子树**
- 此时，任何 `x` 的后代（在这里就是空）的键值都不可能大于 `x.key`。所以后继只可能在 `x` 的祖先中。
- 我们从 `x` 向上遍历，设当前节点为 `curr`，其父节点为 `p`。
    - **如果 `curr` 是 `p` 的右孩子 (`curr == p.right`)**: 这意味着 `p.key < curr.key`。由于我们是从 `x` 开始向上，所以 `p.key` 也小于 `x.key`。`p` 不可能是后继，我们必须继续向上寻找。
    - **如果 `curr` 是 `p` 的左孩子 (`curr == p.left`)**: 这意味着 `p.key > curr.key`。因为 `p` 是第一个满足此条件的祖先，所有在 `p` 和 `x` 之间的祖先节点（`curr` 的父节点、祖父节点等）的键值都小于 `x.key`。因此，`p` 就是大于 `x.key` 的最小键值节点，即 `x` 的后继。
- 算法中的 `while` 循环 (`while (p != nullptr && x == p.right)`) 正是实现了这个过程：不断向上移动，直到当前节点不再是父节点的右孩子。此时的父节点 `p` 就是后继。如果循环结束时 `p` 为 `nullptr`，说明 `x` 是树中的最大元素，没有后继。

---

### 2. `prev(x)` - 查找前驱

**数学原理:**
`prev` 的逻辑与 `succ` 完全对称。对于节点 `x`：

1.  **如果 `x` 存在左子树:** 那么 `x` 的前驱就是其**左子树中的最大节点**。
2.  **如果 `x` 不存在左子树:** 从 `x` 开始**向上回溯**，直到找到一个节点 `p`，使得当前节点是 `p` 的右孩子。这个 `p` 就是前驱。如果一路向上直到根节点，当前节点都是左孩子，那么 `x` 是树中最小的元素，它没有前驱。

> 思考🤔, 对于任意一个点x: 哪些点比x小?
> 容易想到: 在中序序列中,比x位置靠前的都比x小.序列中离x最近的就是前驱
> 1. 如果x存在左子树,左子树中的最小值离x最近
> 2. 如果x不存在左子树,显然离x最近的**右子树祖先**,就是最接近x的

**正确性证明:**
证明过程与 `succ` 对称。

**情况 1: `x` 有左子树 `L_x`**
- `L_x` 中所有节点的键值都小于 `x.key`。
- 小于 `x.key` 的最大键值必然存在于 `L_x` 中，即 `L_x` 的最大值。
- 算法通过 `findMax(x.left)` 找到此节点，是正确的。

**情况 2: `x` 没有左子树**
- 前驱只可能在 `x` 的祖先中。
- 我们向上遍历，设当前节点为 `curr`，父节点为 `p`。
    - **如果 `curr` 是 `p` 的左孩子**: `p.key > curr.key`，`p` 不是前驱，继续向上。
    - **如果 `curr` 是 `p` 的右孩子**: `p.key < curr.key`。`p` 是第一个满足此条件的祖先，因此 `p` 是小于 `x.key` 的最大键值节点，即 `x` 的前驱。
- 算法的 `while` 循环实现了此逻辑，是正确的。

---

### 3. 实现

#### C++ (带 `parent` 指针)

```cpp
struct Node {
    int key;
    Node *left, *right, *parent;
    Node(int key) : key(key), left(nullptr), right(nullptr), parent(nullptr) {}
};

// 辅助函数：在以 t 为根的子树中找到最小节点
Node* findMinNode(Node* t) {
    if (t == nullptr) return nullptr;
    while (t->left != nullptr) {
        t = t->left;
    }
    return t;
}

// 辅助函数：在以 t 为根的子树中找到最大节点
Node* findMaxNode(Node* t) {
    if (t == nullptr) return nullptr;
    while (t->right != nullptr) {
        t = t->right;
    }
    return t;
}

// 查找后继
Node* succ(Node* x) {
    if (x == nullptr) return nullptr;

    // 情况 1: 节点有右子树
    if (x->right != nullptr) {
        return findMinNode(x->right);
    }

    // 情况 2: 节点没有右子树
    Node* p = x->parent;
    while (p != nullptr && x == p->right) {
        x = p;
        p = p->parent;
    }
    return p; // p 是后继, 或者 p 是 nullptr (x是最大值)
}

// 查找前驱
Node* prev(Node* x) {
    if (x == nullptr) return nullptr;

    // 情况 1: 节点有左子树
    if (x->left != nullptr) {
        return findMaxNode(x->left);
    }

    // 情况 2: 节点没有左子树
    Node* p = x->parent;
    while (p != nullptr && x == p->left) {
        x = p;
        p = p->parent;
    }
    return p; // p 是前驱, 或者 p 是 nullptr (x是最小值)
}
```

#### Haskell (纯函数式)

在纯函数式编程中，数据结构是不可变的，通常没有父指针。因此，查找后继或前驱需要从根开始遍历。

**算法思路 (以 `succ` 为例):**
从根节点开始向下查找目标值 `x`。
- 当向**左**走时（即 `x < 当前节点k`），当前节点 `k` 是一个潜在的后继，因为 `k > x`。我们记录下这个 `k`，然后继续在左子树中寻找更精确（即更小）的后继。
- 当向**右**走时（即 `x > 当前节点k`），当前节点 `k` 小于 `x`，不可能是后继。我们继续在右子树中查找 `x`，但无需更新潜在的后继。
- 当找到 `x` 时：
    - 如果 `x` 有右子树，后继就是其右子树的最小值。
    - 如果 `x` 没有右子树，那么我们在下降过程中记录的最后一个“潜在后继”（即最近的那个我们向左拐弯时的节点）就是最终的后继。

```haskell
-- 查找 BST 中的最大元素 (findMin 已在之前定义)
findMax :: Tree a -> Maybe a
findMax Empty = Nothing
findMax (Node _ k Empty) = Just k
findMax (Node _ _ r) = findMax r

-- 在纯函数式 BST 中查找后继
-- 需要整个树和目标键值
-- `best` 参数在向下递归时，记录“目前为止最好的后继候选”
succ :: (Ord a) => a -> Tree a -> Maybe a
succ x tree = go tree Nothing
  where
    go :: Tree a -> Maybe a -> Maybe a
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
    go :: Tree a -> Maybe a -> Maybe a
    go Empty _ = Nothing
    go (Node l k r) best
        | x < k     = go l best     -- 前驱在左子树, best 不变
        | x > k     = go r (Just k) -- k 是一个可能的前驱, 在右子树找更接近的
        | otherwise = -- 找到了键 x (k == x)
            case l of
                Empty -> best      -- 没有左子树, best (最近的右拐祖先) 就是前驱
                _     -> findMax l -- 有左子树, 前驱是左子树的最大值
```

#### 4. `deleteMin()` - 删除最小值

**数学原理:**
`deleteMin` 建立在 `findMin` 的基础上。首先，找到最小节点 `M`。`M` 必没有左子树。要删除它，我们只需将其父节点（设为 `P`）的左链接指向 `M` 的右子树即可。

**正确性证明 (保持 BST 不变量):**
设 `M` 是树中的最小节点，`P` 是它的父节点（`M = P.left`）。
1. `M` 没有左子树（根据 `findMin` 的证明）。
2. `M` 可能有也可能没有右子树 `R`。
3. 根据 BST 不变量，`P.key > M.key`。并且，`R` 中所有节点的值 `r_i` 都满足 `r_i > M.key`。
4. 同时，因为 `M` 是 `P` 的左孩子，`P` 及其所有祖先节点的值都大于 `R` 中任意节点的值。即 `P.key > r_i`。

当我们用 `R` 替换 `M` 作为 `P` 的左孩子时，新的关系是 `P.left = R`。由于我们已经知道 `P.key > r_i` 对于 `R` 中所有节点都成立，所以 BST 不变量在 `P` 节点处得以保持。树的其他部分没有受到影响。因此，操作是正确的。时间复杂度为 `O(h)`。

---

#### 5. `delete(x)` - 删除操作

**数学原理:**
删除操作是最复杂的情况，因为它需要处理三种不同的场景来维持 BST 不变量。设要删除的节点为 `N`。
1.  **`N` 是叶子节点:** 直接删除即可，不影响其他节点。
2.  **`N` 只有一个子树:** 将 `N` 的父节点直接链接到 `N` 的那个孩子上，从而绕过 `N`。
3.  **`N` 有两个孩子:** 这是最关键的情况。我们不能简单地删除 `N`，因为会留下两个子树需要处理。解决方法是：
    a. 在 `N` 的右子树中找到最小值（这个值被称为 `N` 的**中序后继**，Successor）。设为 `S`。
    b. 将 `N` 节点的值替换为 `S` 的值。
    c. 递归地删除节点 `S`（此时 `S` 最多只有一个右孩子，将其转化为情况 1 或 2）。

$$delete( x, \varnothing) = \varnothing$$

$$
delete\quad x\quad (l,k,r) = \begin{cases}
x < k: (delete\ x\ l,k,r) \\
x > k: (l,k,delete\ x\ r) \\
x = k: del\ l\ r
\end{cases}
$$

其中:

$$
del\ \varnothing \ r = r \\
del\ l\ \varnothing = l \\
del\ l\ r = (l,y,delete\ y\ r), y = \min r \\
$$


**正确性证明 (保持 BST 不变量):**
- **情况 1 (叶子节点):** 删除叶子节点不会改变任何祖先节点的子树结构，显然保持不变量。
- **情况 2 (单孩子节点):** 假设 `N` 有一个左孩子 `L`（右孩子同理），其父节点为 `P`。我们有 `P.key > N.key > L.key`。将 `P` 的链接指向 `L` 后，新的关系是 `P.key > L.key`，这符合 BST 不变量。
- **情况 3 (双孩子节点):**
    - 设 `N` 的左右子树分别为 `L_N` 和 `R_N`。
    - 我们找到 `R_N` 中的最小值 `S`。根据定义，`S` 是大于 `N.key` 的所有值中最小的一个。
    - 当我们将 `N` 的值替换为 `S` 的值后，新的 `N` 节点（我们称之为 `N'`）满足：
        - `N'.key` 大于 `L_N` 中所有节点的值（因为 `S.key > N.key`）。
        - `N'.key` 小于 `R_N` 中所有其他节点的值（因为 `S` 是 `R_N` 的最小值）。
    - 此时，BST 的结构不变量在 `N'` 处得到了满足。
    - 接下来，我们需要从 `R_N` 中删除原先的 `S` 节点。根据 `findMin` 的原理，`S` 节点自身没有左孩子。因此，删除 `S` 的问题就退化为了更简单的情况 1 或情况 2。
    - 由于退化后问题的解决是正确的（已证明），因此整个删除操作也是正确的。

整个 `delete` 操作的复杂度也是由查找节点和（在情况3中）查找后继节点决定的，时间复杂度为 `O(h)`。

```haskell
-- | 从 BST 中删除一个值
-- | 这里的逻辑直接翻译自原文中的 'delete' 和 'del' 助手函数
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
            let (k', r') = deleteMinHelper r -- 找到并删除右子树的最小值
            in Node l k' r'

-- | 助手函数：找到并删除最小节点 (用于 'delete' 操作)
-- | 返回 (最小值的键, 删除了最小值的子树)
-- | 这是一个高效的函数式实现，避免了对右子树的两次遍历。
deleteMinHelper :: Tree a -> (a, Tree a)
deleteMinHelper Empty = error "deleteMinHelper: called on empty tree" -- 逻辑上不应该发生
deleteMinHelper (Node Empty k r) = (k, r) -- 找到了最小值 (k)，用其右子树 (r) 替换
deleteMinHelper (Node l k r) =
    let (minKey, newL) = deleteMinHelper l -- 最小值在左子树
    in (minKey, Node newL k r)
```

## 总结

- 增
  - insert
- 删
  - delete
  - deleteMin
  - deleteMax
- 查
  - find
  - findMin
  - findMax
  - succ
  - pred
- 遍历
  - 中序遍历会得到一个有序的序列
  - mapt
  - foldt

`insert`,`find`,`delete`都很简单,就是简单的递归

`succ`和`pred`比较复杂,需要考虑父节点
`delete`也比较复杂,需要考虑左右子树

## 等待整理

这是一个非常好的练习。您提供的伪代码混合了两种不同的编程范式：

1.  **函数式 (Functional)：** `insert`、`delete`、`mapt`、`foldt` 这些函数不修改（mutate）原有的树，而是返回一个全新的、修改后的树。
2.  **命令式 (Imperative)：** `Node` 定义中的 `parent` 指针、以及依赖它的 `succ` (后继) 函数，都依赖于能够修改数据和在数据结构中“向上”导航。


因此，在下面的 Haskell 实现中，我将：

1.  实现一个**纯函数式**的二叉搜索树，它不包含 `parent` 指针。
2.  这与您示例中的 `insert`、`delete`、`mapt` 和 `foldt` 函数的**函数式风格**相匹配。
3.  我将**无法实现**原文中的 `succ` 函数，因为它**根本上依赖**于 `parent` 指针。我会在代码中详细注释这一点。
4.  原文中的 `insert` 算法逻辑**是错误的**（它将大于键的值插入左子树，小于的插入右子树）。我将实现**正确**的 BST 插入算法。
5.  我将实现 `find` 和 `min` 的纯函数式（递归）版本，它们与原文中的迭代版本功能相同。

-----

### 🌳 Haskell 实现的二叉搜索树 (BST.hs)

```haskell
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
-- | 注意：原文中的 insert 算法逻辑是错误的 (它将大于的放左边，小于的放右边)
-- | 这里实现的是 *正确* 的 BST 插入逻辑
insert :: (Ord a) => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty -- 基本情况：在空树上插入，返回一个新节点
insert x (Node l k r)
    | x < k     = Node (insert x l) k r -- x 小于当前键，递归插入左子树
    | x > k     = Node l k (insert x r) -- x 大于当前键，递归插入右子树
    | otherwise = Node l k r            -- x 等于当前键，树不变 (BST 通常不存储重复值)

-- | 在 BST 中查找一个值
-- | 原文提供了一个迭代(while循环)算法，这在 Haskell 中不自然
-- | 这里使用等效的、惯用的递归版本
find :: (Ord a) => a -> Tree a -> Bool
find _ Empty = False -- 在空树中查找，失败
find x (Node l k r)
    | x < k     = find x l -- x 小于当前键，递归查找左子树
    | x > k     = find x r -- x 大于当前键，递归查找右子树
    | otherwise = True       -- 找到 (k == x)

-- | 查找 BST 中的最小元素
-- | 原文提供了一个迭代版本，这里是等效的递归版本
-- | 最小元素总是在最左边的节点上
findMin :: Tree a -> Maybe a
findMin Empty = Nothing -- 空树没有最小值
findMin (Node Empty k _) = Just k -- 没有左子树，当前节点就是最小值
findMin (Node l _ _) = findMin l -- 否则，最小值在左子树中

-- | 从 BST 中删除一个值
-- | 这里的逻辑直接翻译自原文中的 'delete' 和 'del' 助手函数
delete :: (Ord a) => a -> Tree a -> Tree a
delete _ Empty = Empty -- 从空树删除，返回空树
delete x (Node l k r)
    | x < k     = Node (delete x l) k r -- 要删除的值在左子树
    | x > k     = Node l k (delete x r) -- 要删除的值在右子树
    | otherwise = del l r               -- 找到了要删除的节点 (k == x)，调用 del
    where
        -- | 'del' 助手函数处理删除根节点的情况
        -- | 这对应原文中的 'del l r'
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
-- | 这是 'del l r = let k' = min r in Node l k' (delete k' r)' 这一行
-- | 的高效函数式实现。原文的实现 (先 findMin 再 delete) 会导致对右子树的两次遍历。
deleteMin :: Tree a -> (a, Tree a)
deleteMin Empty = error "deleteMin: called on empty tree" -- 逻辑上不应该发生
deleteMin (Node Empty k r) = (k, r) -- 找到了最小值 (k)，用其右子树 (r) 替换
deleteMin (Node l k r) =
    let (minKey, newL) = deleteMin l -- 最小值在左子树
    in (minKey, Node newL k r)

-- | ---------------------------------------------------------------------------
-- | 关于后继 (succ) 函数的说明
-- | ---------------------------------------------------------------------------
-- |
-- | 原文中 'succ' 函数的实现 *严重依赖于* 'parent' (父节点) 引用。
-- |
-- |     else {
-- |       p = x.parent
-- |       while (p 6= null and x == p.right) {
-- |         x = p
-- |         p = p.parent
-- |       }
-- |       return Optional.of(p);
-- |     }
-- |
-- | 在 Haskell 这样的纯函数式语言中，我们不使用可变的父节点引用。
-- | 我们的 'Tree' 数据结构是不可变的。'insert' 和 'delete' 函数会
-- | 返回一个 *新* 的树，而不是修改旧的树。
-- |
-- | 因此，无法直接翻译原文中依赖 'parent' 指针的 'succ' 算法。
-- |
-- | 在函数式 BST 中要找到一个键的后继，你需要整个树和这个键，
-- | 然后执行一次查找，同时"记住"当你向左走时可能的"后继"。
-- | (此处省略 succ 的实现)
-- |
-- | ---------------------------------------------------------------------------

-- | ---------------------------------------------------------------------------
-- | 映射和折叠 (Map & Fold)
-- | ---------------------------------------------------------------------------

-- | 对树中的每个元素应用一个函数
-- | 对应原文中的 'mapt'
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _ Empty = Empty
mapTree f (Node l k r) = Node (mapTree f l) (f k) (mapTree f r)

-- | 折叠 (fold) 树 (Catamorphism)
-- | 这是一个通用的树遍历/转换函数
-- | 对应原文中的 'foldt'
-- | g: 如何组合 (左子树结果, 转换后的键, 右子树结果)
-- | f: 如何转换键
-- | z: 空树的默认值
foldTree :: (b -> c -> b -> b) -> (a -> c) -> b -> Tree a -> b
foldTree _ _ z Empty = z
foldTree g f z (Node l k r) =
    g (foldTree g f z l) (f k) (foldTree g f z r)

-- | 演示：使用 'foldTree' 来实现 'mapTree'
-- | 对应原文中的 'maptr'
mapTree' :: (a -> b) -> Tree a -> Tree b
mapTree' f = foldTree Node f Empty

-- | 对应原文中的第二个 'fold'
-- | fold f z (Node l k r) = fold f (k `f` (fold f z r)) l
-- | 这是一个 *反向* 中序 (R-K-L, Right-Key-Left) 遍历折叠
foldRKL :: (a -> b -> b) -> b -> Tree a -> b
foldRKL _ z Empty = z
foldRKL f z (Node l k r) = foldRKL f (k `f` (foldRKL f z r)) l

-- | 一个更有用的 *正向* 中序 (L-K-R, Left-Key-Left) 遍历折叠
-- | 注意 f 的类型是 (b -> a -> b)
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

    putStrLn "\n--- 最小值 (findMin) ---"
    putStrLn $ "最小值 (应为 1): " ++ show (findMin tree)

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

    putStrLn "\n删除 3 (一个子节点, 1 和 4):"
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
```

### 🧑‍💻 如何运行代码

1.  将上述代码保存为 `BST.hs`。
2.  在您的终端中，使用 GHC (Haskell 编译器) 运行它：
    ```bash
    runghc BST.hs
    ```

您将看到所有测试用例的输出。

这个实现忠实地反映了您所提供示例中的**函数式**部分，并对命令式部分（`parent` 指针和 `succ`）无法在纯函数式 Haskell 中直接实现的原因进行了解释。

您想让我展示如何在 Haskell 中实现一个功能上等效的 `succ` (后继) 函数吗？它会比原文中的版本更复杂一些。

---
