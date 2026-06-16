#include <iostream>
#include <algorithm> // 用于 std::max

using namespace std;

// 定义节点结构体
struct Node {
    int val;        // 节点值
    int h;          // 树高 (Height)，叶子节点高度为 1
    Node *left;
    Node *right;

    // 构造函数：初始化新节点
    Node(int v) : val(v), h(1), left(nullptr), right(nullptr) {}
};

// ==========================================
// 辅助函数 (Helpers)
// ==========================================

// 安全获取节点高度 (处理 nullptr 情况)
// 空节点高度为 0，非空节点返回 node->h
int getHeight(Node* node) {
    if (node == nullptr) {
        return 0;
    }
    return node->h;
}

// 计算平衡因子 (Balance Factor)
// 公式：左子树高度 - 右子树高度
// 结果 > 1 : 左边太高
// 结果 < -1: 右边太高
int getBalanceFactor(Node* node) {
    if (node == nullptr) {
        return 0;
    }
    return getHeight(node->left) - getHeight(node->right);
}

// 更新节点高度
// 公式：max(左高, 右高) + 1
void updateHeight(Node* node) {
    if (node != nullptr) {
        node->h = std::max(getHeight(node->left), getHeight(node->right)) + 1;
    }
}

// ==========================================
// 核心操作：旋转 (Rotations)
// ==========================================

// 右旋 (Right Rotation)
// 适用场景：左左 (LL) 失衡
//       y (root)
//      /
//     x
//    / \
//   T1 T2
Node* rotateRight(Node* y) {
    Node* x = y->left;      // x 是新的根
    Node* T2 = x->right;    // T2 是 x 原本的右孩子

    // 执行旋转
    x->right = y;           // y 变成 x 的右孩子
    y->left = T2;           // T2 变成 y 的左孩子

    // 更新高度 (必须先更新子节点 y，再更新父节点 x)
    updateHeight(y);
    updateHeight(x);

    return x; // 返回新的根节点
}

// 左旋 (Left Rotation)
// 适用场景：右右 (RR) 失衡
//    x (root)
//     \
//      y
//     / \
//    T2 T3
Node* rotateLeft(Node* x) {
    Node* y = x->right;     // y 是新的根
    Node* T2 = y->left;     // T2 是 y 原本的左孩子

    // 执行旋转
    y->left = x;            // x 变成 y 的左孩子
    x->right = T2;          // T2 变成 x 的右孩子

    // 更新高度 (先子后父)
    updateHeight(x);
    updateHeight(y);

    return y; // 返回新的根节点
}

// ==========================================
// 核心功能：再平衡 (Rebalance)
// ==========================================
// 对节点进行再平衡检查，并在需要时执行旋转
Node* rebalance(Node* node) {
    updateHeight(node);
    int balance = getBalanceFactor(node);

    // 左高 (Left Heavy) -> 需要右旋
    if (balance > 1) {
        // LL Case: 直接右旋
        if (getBalanceFactor(node->left) >= 0) {
            return rotateRight(node);
        }
        // LR Case: 先对左孩子左旋，再对自己右旋
        else {
            node->left = rotateLeft(node->left);
            return rotateRight(node);
        }
    }

    // 右高 (Right Heavy) -> 需要左旋
    if (balance < -1) {
        // RR Case: 直接左旋
        if (getBalanceFactor(node->right) <= 0) {
            return rotateLeft(node);
        }
        // RL Case: 先对右孩子右旋，再对自己左旋
        else {
            node->right = rotateRight(node->right);
            return rotateLeft(node);
        }
    }

    // 节点已平衡，无需旋转
    return node;
}

// ==========================================
// 核心功能：插入 (Insert)
// ==========================================
// 递归插入节点，并自动平衡
// 返回值：更新后的子树根节点指针
Node* insert(Node* node, int val) {
    // 1. 执行标准的 BST 插入
    if (node == nullptr) {
        return new Node(val);
    }

    if (val < node->val) {
        node->left = insert(node->left, val);
    } else if (val > node->val) {
        node->right = insert(node->right, val);
    } else {
        // 不允许插入重复值，直接返回
        return node;
    }

    // 2. 更新高度并重新平衡
    return rebalance(node);
}

// ==========================================
// 核心功能：删除 (Remove)
// ==========================================

// 查找树中最小值的节点 (用于删除有两个孩子的节点时找到后继)
Node* getMinValueNode(Node* node) {
    Node* current = node;
    while (current->left != nullptr) {
        current = current->left;
    }
    return current;
}

// 递归删除节点，并自动平衡
Node* remove(Node* root, int val) {
    // 1. 执行标准的 BST 删除
    if (root == nullptr) {
        return root;
    }

    if (val < root->val) {
        root->left = remove(root->left, val);
    } else if (val > root->val) {
        root->right = remove(root->right, val);
    } else {
        // 找到了要删除的节点

        // 情况 1: 只有一个孩子 或 没有孩子
        if ((root->left == nullptr) || (root->right == nullptr)) {
            Node* temp = root->left ? root->left : root->right;

            // 无孩子情况
            if (temp == nullptr) {
                temp = root;
                root = nullptr;
            } else {
                // 有一个孩子情况，用孩子复制给当前节点 (或者简单指针替换)
                *root = *temp; // 浅拷贝结构体内容
            }
            delete temp; // 释放内存
        } else {
            // 情况 2: 有两个孩子
            // 找到右子树中的最小值 (中序后继)
            Node* temp = getMinValueNode(root->right);

            // 把后继的值复制上来
            root->val = temp->val;

            // 删除那个后继节点
            root->right = remove(root->right, temp->val);
        }
    }

    // 如果树只有一个节点且被删除了，直接返回
    if (root == nullptr) {
        return root;
    }

    // 2. 更新高度并重新平衡
    return rebalance(root);
}

// ==========================================
// 测试代码 (Main)
// ==========================================

// 中序遍历 (用于验证排序正确性)
void inOrder(Node* root) {
    if (root != nullptr) {
        inOrder(root->left);
        cout << root->val << " ";
        inOrder(root->right);
    }
}

int main() {
    Node* root = nullptr;

    cout << "插入: 10, 20, 30, 40, 50, 25" << endl;
    // 依次插入，测试自动平衡
    root = insert(root, 10);
    root = insert(root, 20);
    root = insert(root, 30); // 触发旋转 (RR)
    root = insert(root, 40);
    root = insert(root, 50);
    root = insert(root, 25); // 触发旋转 (RL)

    cout << "中序遍历 (应该是排序的): ";
    inOrder(root);
    cout << endl;
    cout << "根节点是: " << root->val << " (树是平衡的)" << endl;

    cout << "\n删除: 30" << endl;
    root = remove(root, 30);

    cout << "中序遍历: ";
    inOrder(root);
    cout << endl;
    cout << "当前根节点: " << root->val << endl;

    return 0;
}