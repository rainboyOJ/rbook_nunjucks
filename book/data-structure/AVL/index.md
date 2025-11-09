

好的，我们来详细讲解 AVL 树。这是一种非常重要的自平衡二叉搜索树（BST）。

一、核心定义：什么是 AVL 树？

AVL 树是最早被发明的自平衡二叉搜索树。它的命名来源于其发明者 G. M. Adelson-Velsky 和 E. M. Landis。

在普通的二叉搜索树中，如果插入的序列是有序的（如 1, 2, 3, 4, 5），树会退化成一条链表，搜索时间复杂度会从理想的 O(log n) 恶化到 O(n)。

AVL 树通过在插入和删除元素后立即进行旋转操作来重新平衡自身，从而确保树的高度始终保持在对数范围内，保证了搜索、插入和删除操作的时间复杂度都是 O(log n)。

AVL 树的核心特性：对于树中的每一个节点，其左子树和右子树的高度差（平衡因子）的绝对值不超过 1。

---

二、核心概念：平衡因子

要理解 AVL 树，必须先理解平衡因子。

· 平衡因子 = 左子树高度 - 右子树高度
· 高度：从一个节点到其最远叶子节点的最长路径上的边数。叶子节点的高度为 0，空节点的高度通常定义为 -1。

对于一个 AVL 树中的任意节点，其平衡因子只能是以下三个值之一：

· -1：右子树比左子树高 1。
· 0：左子树和右子树一样高。
· 1：左子树比右子树高 1。

如果任何一个节点的平衡因子不在这个范围内（即绝对值大于 1），那么这个节点就是不平衡的，需要被重新平衡。

---

三、失衡与旋转

当插入或删除一个节点后，可能会破坏 AVL 树的平衡条件。我们需要从新插入/删除的节点开始，向上回溯到根节点，检查路径上的每一个祖先节点的平衡因子。一旦发现某个节点变得不平衡（平衡因子为 2 或 -2），就需要通过旋转来修复。

旋转操作有四种基本类型，可以分为两组对称的操作：

1. 左旋

使用场景：当一个节点的右子树比左子树高太多（平衡因子 = -2），并且其右子树的右子树较高导致失衡（即“右右”情况）。

操作描述：

1. 将失衡节点（A）的右孩子（B）提升为新的子树根节点。
2. 原根节点（A）成为新根节点（B）的左孩子。
3. 新根节点（B）原来的左孩子（β）成为原根节点（A）的右孩子。

效果：就像把树向左拧了一下，降低了右子树的高度。



好的，我们来详细讲解 AVL 树的插入与删除操作，并提供完整的 C++ 实现代码。

------



## 什么是 AVL 树？



AVL 树是最早被发明的自平衡二叉搜索树（Self-Balancing Binary Search Tree）。它的核心特点是：**树中任何节点的两个子树的高度差最多为 1**。

这个高度差被称为**平衡因子（Balance Factor）**。

```
平衡因子 = 左子树高度 - 右子树高度
```

一个合法的 AVL 树中，所有节点的平衡因子只能是 **-1、0 或 1**。如果任何节点的平衡因子的绝对值大于 1（即为 -2 或 +2），则该树就失去了平衡，需要通过**旋转**操作来恢复平衡。

这个特性保证了树的高度维持在 O(logn) 级别，从而使得搜索、插入和删除操作的时间复杂度也保持在 O(logn)。



## 核心操作：旋转



旋转是 AVL 树维持平衡的关键。当插入或删除一个节点导致某个祖先节点的平衡因子变为 -2 或 +2 时，就需要进行旋转。主要有四种失衡情况，对应两种单旋转和两种双旋转。

假设失衡的节点为 `z`。



### 1. 左-左（LL）情况：右旋



- **失衡原因**：在新节点插入到 `z` 的**左子树**的**左子树**之后，`z` 的平衡因子变为 +2。
- **解决方法**：对 `z` 进行一次**右旋**。

```
      z (+2)              y (0)
     / \                 /   \
    y (+1)      ->      x     z
   / \                 / \   / \
  x   T3              T1 T2 T3 T4
 / \
T1 T2
```



### 2. 右-右（RR）情况：左旋



- **失衡原因**：在新节点插入到 `z` 的**右子树**的**右子树**之后，`z` 的平衡因子变为 -2。
- **解决方法**：对 `z` 进行一次**左旋**。

```
    z (-2)                y (0)
   / \                   /   \
  T1  y (-1)     ->     z     x
     / \                 / \   / \
    T2  x               T1 T2 T3 T4
       / \
      T3 T4
```



### 3. 左-右（LR）情况：先左旋后右旋



- **失衡原因**：在新节点插入到 `z` 的**左子树**的**右子树**之后，`z` 的平衡因子变为 +2。
- **解决方法**：
  1. 先对 `z` 的左孩子 `y` 进行**左旋**。
  2. 再对 `z` 本身进行**右旋**。

```
      z (+2)                z (+2)                  x
     / \                   / \                     /  \
    y (-1)      (1) 左旋 y   x        (2) 右旋 z     y    z
   / \          ------>   / \        ------>     / \  / \
  T1  x                  y   T3                T1 T2 T3 T4
     / \                / \
    T2 T3              T1 T2
```



### 4. 右-左（RL）情况：先右旋后左旋



- **失衡原因**：在新节点插入到 `z` 的**右子树**的**左子树**之后，`z` 的平衡因子变为 -2。
- **解决方法**：
  1. 先对 `z` 的右孩子 `y` 进行**右旋**。
  2. 再对 `z` 本身进行**左旋**。

```
    z (-2)              z (-2)                    x
   / \                 / \                       /   \
  T1  y (+1)  (1) 右旋 y x          (2) 左旋 z   z     y
     / \      ------>   / \          ------>   / \   / \
    x   T4             T2  y                  T1 T2 T3 T4
   / \                    / \
  T2 T3                  T3 T4
```

------



## AVL 树的插入操作



插入操作的步骤如下：

1. **执行标准二叉搜索树（BST）的插入**：从根节点开始，找到合适的位置插入新节点。
2. **更新高度**：从新插入的节点开始，向上回溯到根节点，更新路径上所有祖先节点的高度。
3. **检查平衡并旋转**：在回溯的每一步，计算当前节点的平衡因子。
   - 如果平衡因子的绝对值大于 1，说明树在当前节点失衡。
   - 根据上面描述的四种情况（LL, RR, LR, RL）进行相应的旋转操作来恢复平衡。
   - **注意**：对于插入操作，一旦完成一次旋转，整个树就恢复了平衡，回溯过程可以停止。



## AVL 树的删除操作



删除操作比插入更复杂一些，因为一次删除可能导致路径上多个节点失衡。

1. **执行标准二叉搜索树（BST）的删除**：
   - **情况1：被删除节点是叶子节点**：直接删除。
   - **情况2：被删除节点只有一个孩子**：用其孩子节点替换它。
   - **情况3：被删除节点有两个孩子**：找到它的**中序后继**（右子树中最小的节点），用后继节点的值替换当前节点的值，然后递归地删除那个后继节点（后继节点最多只有一个右孩子，转化为情况1或2）。
2. **更新高度**：从被删除节点的父节点开始，向上回溯到根节点，更新路径上所有祖先节点的高度。
3. **检查平衡并旋转**：在回溯的每一步，检查当前节点的平衡因子。
   - 如果发现失衡（平衡因子为 -2 或 +2），则进行相应的旋转。
   - **注意**：与插入不同，删除操作在进行一次旋转后，可能导致更高层的祖先节点也失衡。因此，**必须继续向上回溯，直到根节点**，检查并修正所有可能的失衡。

------



## C++ 代码实现



下面是一个完整的 AVL 树的 C++ 实现，包括插入、删除和打印功能。

C++

```
#include <iostream>
#include <algorithm> // for std::max

// AVL 树节点结构
struct Node {
    int key;
    Node* left;
    Node* right;
    int height;
};

// 获取节点高度的辅助函数
int height(Node* n) {
    if (n == nullptr)
        return 0;
    return n->height;
}

// 创建新节点的辅助函数
Node* newNode(int key) {
    Node* node = new Node();
    node->key = key;
    node->left = nullptr;
    node->right = nullptr;
    node->height = 1; // 新节点初始高度为1
    return node;
}

// 右旋操作 (对应 LL 情况)
// y 是失衡的节点
Node* rightRotate(Node* y) {
    Node* x = y->left;
    Node* T2 = x->right;

    // 执行旋转
    x->right = y;
    y->left = T2;

    // 更新高度
    y->height = std::max(height(y->left), height(y->right)) + 1;
    x->height = std::max(height(x->left), height(x->right)) + 1;

    // 返回新的根节点
    return x;
}

// 左旋操作 (对应 RR 情况)
// x 是失衡的节点
Node* leftRotate(Node* x) {
    Node* y = x->right;
    Node* T2 = y->left;

    // 执行旋转
    y->left = x;
    x->right = T2;

    // 更新高度
    x->height = std::max(height(x->left), height(x->right)) + 1;
    y->height = std::max(height(y->left), height(y->right)) + 1;

    // 返回新的根节点
    return y;
}

// 获取节点的平衡因子
int getBalance(Node* n) {
    if (n == nullptr)
        return 0;
    return height(n->left) - height(n->right);
}

// 插入函数
Node* insert(Node* node, int key) {
    // 1. 执行标准BST插入
    if (node == nullptr)
        return newNode(key);

    if (key < node->key)
        node->left = insert(node->left, key);
    else if (key > node->key)
        node->right = insert(node->right, key);
    else // 不允许插入重复的键
        return node;

    // 2. 更新祖先节点的高度
    node->height = 1 + std::max(height(node->left), height(node->right));

    // 3. 获取平衡因子，检查是否失衡
    int balance = getBalance(node);

    // 4. 如果失衡，有四种情况

    // 左-左 (LL) 情况
    if (balance > 1 && key < node->left->key) {
        return rightRotate(node);
    }

    // 右-右 (RR) 情况
    if (balance < -1 && key > node->right->key) {
        return leftRotate(node);
    }

    // 左-右 (LR) 情况
    if (balance > 1 && key > node->left->key) {
        node->left = leftRotate(node->left);
        return rightRotate(node);
    }

    // 右-左 (RL) 情况
    if (balance < -1 && key < node->right->key) {
        node->right = rightRotate(node->right);
        return leftRotate(node);
    }

    // 如果没有失衡，返回原节点
    return node;
}

// 寻找中序后继 (右子树中最小的节点)
Node* minValueNode(Node* node) {
    Node* current = node;
    while (current->left != nullptr)
        current = current->left;
    return current;
}

// 删除函数
Node* deleteNode(Node* root, int key) {
    // 1. 执行标准BST删除
    if (root == nullptr)
        return root;

    if (key < root->key)
        root->left = deleteNode(root->left, key);
    else if (key > root->key)
        root->right = deleteNode(root->right, key);
    else {
        // 找到了要删除的节点
        // 情况 1 & 2: 节点有一个或没有孩子
        if ((root->left == nullptr) || (root->right == nullptr)) {
            Node* temp = root->left ? root->left : root->right;
            // 没有孩子
            if (temp == nullptr) {
                temp = root;
                root = nullptr;
            } else { // 一个孩子
                *root = *temp; // 拷贝内容
            }
            delete temp;
        } else {
            // 情况 3: 节点有两个孩子
            // 获取中序后继
            Node* temp = minValueNode(root->right);
            // 拷贝后继节点的值到当前节点
            root->key = temp->key;
            // 删除那个后继节点
            root->right = deleteNode(root->right, temp->key);
        }
    }

    // 如果删除后树为空 (比如树只有一个节点)
    if (root == nullptr)
        return root;

    // 2. 更新当前节点的高度
    root->height = 1 + std::max(height(root->left), height(root->right));

    // 3. 获取平衡因子，检查是否失衡
    int balance = getBalance(root);

    // 4. 如果失衡，有四种情况 (与插入类似，但判断条件不同)

    // 左-左 (LL) 情况
    if (balance > 1 && getBalance(root->left) >= 0) {
        return rightRotate(root);
    }

    // 左-右 (LR) 情况
    if (balance > 1 && getBalance(root->left) < 0) {
        root->left = leftRotate(root->left);
        return rightRotate(root);
    }

    // 右-右 (RR) 情况
    if (balance < -1 && getBalance(root->right) <= 0) {
        return leftRotate(root);
    }

    // 右-左 (RL) 情况
    if (balance < -1 && getBalance(root->right) > 0) {
        root->right = rightRotate(root->right);
        return leftRotate(root);
    }

    return root;
}

// 打印树 (前序遍历)
void preOrder(Node* root) {
    if (root != nullptr) {
        std::cout << root->key << " ";
        preOrder(root->left);
        preOrder(root->right);
    }
}

int main() {
    Node* root = nullptr;

    // 插入操作
    std::cout << "Inserting nodes: 10, 20, 30, 40, 50, 25" << std::endl;
    root = insert(root, 10);
    root = insert(root, 20);
    root = insert(root, 30);
    root = insert(root, 40);
    root = insert(root, 50);
    root = insert(root, 25); // 这会触发一次 RL 旋转

    /*
     * 插入过程:
     * 10, 20, 30 (RR旋转) -> 20, 10, 30
     * 40, 50 (RR旋转) -> 30, 20, 40 -> ... -> 最终形态
     * 25 (RL旋转) -> ...
     *
     * 最终插入后的树 (前序遍历): 30 20 10 25 40 50
     */
    std::cout << "Preorder traversal of the constructed AVL tree is: ";
    preOrder(root);
    std::cout << std::endl << std::endl;

    // 删除操作
    std::cout << "Deleting node 10" << std::endl;
    root = deleteNode(root, 10);
    /*
     * 删除10后的树 (前序遍历): 30 20 25 40 50
     */
    std::cout << "Preorder traversal after deletion of 10: ";
    preOrder(root);
    std::cout << std::endl;

    std::cout << "Deleting node 40" << std::endl;
    root = deleteNode(root, 40);
     /*
     * 删除40后的树 (前序遍历): 30 20 25 50
     */
    std::cout << "Preorder traversal after deletion of 40: ";
    preOrder(root);
    std::cout << std::endl;

    return 0;
}
```



## 代码讲解



1. **Node 结构体**：除了常规的 `key`, `left`, `right` 指针外，多了一个 `height` 成员来存储以该节点为根的子树的高度。
2. **`height()` 函数**：一个安全的获取高度的函数，将 `nullptr` 的高度视为 0（在严格定义中，空树高度为-1，叶子为0，但为了方便计算，这里将空树高度设为0，叶子高度设为1，不影响平衡因子的计算）。
3. **旋转函数**：`rightRotate` 和 `leftRotate` 是核心。它们通过改变指针来调整子树结构，并在操作后更新受影响节点的高度。
4. **`getBalance()` 函数**：直接通过左右子树的高度差计算平衡因子。
5. **`insert()` 函数**：
   - 递归地找到插入位置。
   - 在递归返回时，更新路径上节点的高度。
   - 计算平衡因子并检查是否需要旋转。
   - 旋转的判断条件是关键：
     - `balance > 1` 表示左子树过高。如果新 `key` 小于左孩子的 `key`，说明插在了左孩子的左边（LL），否则插在了右边（LR）。
     - `balance < -1` 表示右子树过高。如果新 `key` 大于右孩子的 `key`，说明插在了右孩子的右边（RR），否则插在了左边（RL）。
6. **`deleteNode()` 函数**：
   - 同样递归地找到要删除的节点并执行标准的BST删除。
   - 在递归返回时，更新高度并检查平衡。
   - 旋转的判断条件与插入略有不同，它不依赖于被删除的 `key`，而是依赖于**当前失衡节点的子节点的平衡因子**。
     - `balance > 1` (左重): 如果左孩子的平衡因子 `>= 0`，说明左孩子的左子树更高或等高，是LL情况。如果左孩子平衡因子 `< 0`，说明左孩子的右子树更高，是LR情况。
     - `balance < -1` (右重): 逻辑与上面对称。

