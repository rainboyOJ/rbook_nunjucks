
// bst 通用操作
template <template<typename> class Node,typename T>
class BST_common_operation {
public:
    using NodeType = Node<T>;
    using NodePtr = NodeType*;

    // 左旋，让右孩子 y 上位，自己 x 下沉
    // 传入的 root 是一个指向子树根节点的指针的引用(NodePtr&), 方便直接修改
    // 口诀:  1. 查空  2. 过继 3. 调整父子关系 4. 更新根
    static void rotateLeft(NodePtr &x)
    {
        // 1. 先判空，再取右孩子：x 为空时不能先解引用 x->right（原顺序把取孩子放在判空之前）
        if (x->isEmpty())
            return; // 节点为空，无法左旋
        NodePtr y = x->right;
        if (y->isEmpty())
            return; // 右孩子为空，无法左旋

        // 2. "过继" y 的左子树
        x->right = y->left;     //y 上位后, y 原来的左子树 yl, 挂到 x 的右边
        if (!y->left->isEmpty()) y->left->parent = x; // 更新 yl 的父节点

        // 3. x 连接到 y 的左边
        y->left = x;
        y->parent = x->parent; // y 连接到 x 的原父节点 P
        x->parent = y;

        // 4. 更新子树的根由于 root 是引用, 修改它就是修改原先指向 x 的那个指针
        x = y;
    }

    // 右旋，让左孩子 x 上位，自己 y 下沉 (与左旋完全对称)
    // 口诀:  1. 查空  2. 过继 3. 调整父子关系 4. 更新根
    static void rotateRight(NodePtr &y)
    {
        // 1. 先判空，再取左孩子：y 为空时不能先解引用 y->left
        if (y->isEmpty())
            return; // 节点为空，无法右旋
        NodePtr x = y->left;
        if (x->isEmpty())
            return; // 左孩子为空，无法右旋

        // 2. "过继" x 的右子树
        y->left = x->right;     //x 上位后, x 原来的右子树 xr, 挂到 y 的左边
        if (!x->right->isEmpty()) x->right->parent = y; // 更新 xr 的父节点

        // 3. y 连接到 x 的右边
        x->right = y;
        x->parent = y->parent; // x 连接到 y 的原父节点 P
        y->parent = x;

        // 4. 更新子树的根由于 root 是引用, 修改它就是修改原先指向 y 的那个指针
        y = x;
    }

    /**
     * @brief Find the minimum value in the tree.
     * @param node The root of the tree.
     * @param NIL The NIL node.
     * @return The minimum value in the tree.
     */
    NodePtr findMin(Node<T> *node, const NodePtr NIL = nullptr)
    {
        while (node->left != NIL)
        {
            node = node->left;
        }
        return node;
    }
};