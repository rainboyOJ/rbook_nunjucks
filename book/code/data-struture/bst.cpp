#include <iostream>
#include <vector>
#include <optional>
#include <algorithm>
#include <memory>

// BST 节点定义
struct Node {
    int key;
    Node *left, *right, *parent;
    Node(int key) : key(key), left(nullptr), right(nullptr), parent(nullptr) {}
};

// 查找操作 (递归实现)
bool find(int x, Node* t) {
    if (t == nullptr) {
        return false;
    }
    if (x < t->key) {
        return find(x, t->left);
    } else if (x > t->key) {
        return find(x, t->right);
    } else {
        return true;
    }
}

// 查找操作 (非递归实现)
bool find_iterative(int x, Node* t) {
    while (t != nullptr) {
      if( x < t->key)
        t = t->left;
      else if( x > t->key)
        t = t->right;
      else return true;
    }
    return false;
}

// 插入操作 (递归实现)
// 注意：这个实现没有处理 parent 指针，非递归版本更好
void insert_dfs(int x, Node*& t) {
    if (t == nullptr) {
        t = new Node(x);
        return;
    }
    if (x < t->key) {
        insert_dfs(x, t->left);
        if(t->left) t->left->parent = t; // 设置父节点
    } else if (x > t->key) {
        insert_dfs(x, t->right);
        if(t->right) t->right->parent = t; // 设置父节点
    }
    // 如果 x == t->key，不做任何操作
}

// 插入操作 (非递归实现，推荐)
void insert(int x, Node*& t) {
    Node* p = t;
    Node* parent = nullptr;
    while (p != nullptr) {
      parent = p; // 记录父节点
      if( x < p->key)
        p = p->left;
      else if( x > p->key)
        p = p->right;
      else return; //do nothing, 元素已存在
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

// 查找最小节点 (返回指针)
Node* findMinNode(Node* t) {
    if (t == nullptr) return nullptr;
    while (t->left != nullptr) {
        t = t->left;
    }
    return t;
}

// 查找最大节点 (返回指针)
Node* findMaxNode(Node* t) {
    if (t == nullptr) return nullptr;
    while (t->right != nullptr) {
        t = t->right;
    }
    return t;
}

// 查找最小值 (返回键值)
std::optional<int> findMin(Node* t) {
    Node* minNode = findMinNode(t);
    if (minNode) {
        return minNode->key;
    }
    return std::nullopt;
}

// 查找最大值 (返回键值)
std::optional<int> findMax(Node* t) {
    Node* maxNode = findMaxNode(t);
    if (maxNode) {
        return maxNode->key;
    }
    return std::nullopt;
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

// 删除最小值
void deleteMin(Node*& root) {
    if (root == nullptr) return;
    
    Node* minNode = findMinNode(root);
    if (minNode == nullptr) return;

    // 最小节点没有左孩子
    if (minNode->parent == nullptr) { // 是根节点
        root = minNode->right;
    } else {
        minNode->parent->left = minNode->right;
    }

    if (minNode->right) {
        minNode->right->parent = minNode->parent;
    }

    delete minNode;
}


// 删除操作
void delete_node(int x, Node*& root) {
    if (root == nullptr) return;

    Node* current = root;
    // 1. 先找到要删除的节点
    while (current != nullptr && current->key != x) {
        if (x < current->key) {
            current = current->left;
        } else {
            current = current->right;
        }
    }

    if (current == nullptr) return; // 没找到要删除的节点

    Node* p = current->parent;

    // 2. 处理删除的三种情况
    // 情况 1: 删除叶子节点
    if (current->left == nullptr && current->right == nullptr) {
        if (p == nullptr) { // 删除的是根节点
            root = nullptr;
        } else if (p->left == current) {
            p->left = nullptr;
        } else {
            p->right = nullptr;
        }
        delete current;
        return;
    }

    // 情况 2: 删除只有一个孩子的节点
    if (current->left == nullptr || current->right == nullptr) {
        Node* child = (current->left != nullptr) ? current->left : current->right;
        
        if (p == nullptr) { // 删除的是根节点
            root = child;
            child->parent = nullptr;
        } else if (p->left == current) {
            p->left = child;
            child->parent = p;
        } else {
            p->right = child;
            child->parent = p;
        }
        delete current;
        return;
    }

    // 情况 3: 删除有两个孩子的节点
    // 找到后继节点 (右子树的最小值)
    Node* successor = findMinNode(current->right);
    
    // 用后继节点的值替换当前节点的值
    current->key = successor->key;

    // 删除后继节点 (后继节点最多只有一个右孩子)
    // 注意：后继节点的父节点不可能是 current
    if (successor->parent->left == successor) {
        successor->parent->left = successor->right;
    } else {
        successor->parent->right = successor->right;
    }
    
    if (successor->right) {
        successor->right->parent = successor->parent;
    }
    
    delete successor;
}

// 辅助函数：释放树内存
void freeTree(Node* node) {
    if (node == nullptr) return;
    freeTree(node->left);
    freeTree(node->right);
    delete node;
}

// 辅助函数：中序遍历，用于验证
void inOrderTraversal(Node* node, std::vector<int>& result) {
    if (node == nullptr) return;
    inOrderTraversal(node->left, result);
    result.push_back(node->key);
    inOrderTraversal(node->right, result);
}

// 辅助函数：打印树的结构（简版）
void printTree(Node* node, std::string indent = "", bool isLeft = true) {
    if (node != nullptr) {
        std::cout << indent;
        if (isLeft) {
            std::cout << "L---";
            indent += "   ";
        } else {
            std::cout << "R---";
            indent += "|  ";
        }
        std::cout << node->key << std::endl;
        printTree(node->left, indent, true);
        printTree(node->right, indent, false);
    }
}


// 测试主函数
int main() {
    std::cout << "--- BST 测试 ---" << std::endl;

    std::vector<int> nums = {5, 3, 8, 1, 4, 7, 9, 2};
    std::cout << "原始数据: ";
    for (int n : nums) std::cout << n << " ";
    std::cout << std::endl;

    Node* root = nullptr;
    for (int n : nums) {
        insert(n, root);
    }

    std::cout << "\n--- 构建的树 ---" << std::endl;
    printTree(root);

    std::vector<int> orderedList;
    inOrderTraversal(root, orderedList);
    std::cout << "\n--- 中序遍历 ---" << std::endl;
    std::cout << "结果: ";
    for (int n : orderedList) std::cout << n << " ";
    std::cout << std::endl;

    std::cout << "\n--- 查找 ---" << std::endl;
    std::cout << "查找 4 (应为 1): " << find(4, root) << std::endl;
    std::cout << "查找 6 (应为 0): " << find(6, root) << std::endl;
    
    std::cout << "\n--- 最小值/最大值 ---" << std::endl;
    auto minVal = findMin(root);
    auto maxVal = findMax(root);
    std::cout << "最小值 (应为 1): " << (minVal.has_value() ? std::to_string(minVal.value()) : "nullopt") << std::endl;
    std::cout << "最大值 (应为 9): " << (maxVal.has_value() ? std::to_string(maxVal.value()) : "nullopt") << std::endl;

    std::cout << "\n--- 后继/前驱 ---" << std::endl;
    Node* node4 = root->left->right; // 节点 4
    Node* node5 = root; // 节点 5
    Node* node9 = root->right->right; // 节点 9
    Node* node1 = root->left->left; // 节点 1

    auto succ4 = succ(node4);
    auto succ5 = succ(node5);
    auto succ9 = succ(node9);
    auto prev4 = prev(node4);
    auto prev1 = prev(node1);

    std::cout << "4 的后继 (应为 5): " << (succ4 ? std::to_string(succ4->key) : "nullptr") << std::endl;
    std::cout << "5 的后继 (应为 7): " << (succ5 ? std::to_string(succ5->key) : "nullptr") << std::endl;
    std::cout << "9 的后继 (应为 nullptr): " << (succ9 ? std::to_string(succ9->key) : "nullptr") << std::endl;
    std::cout << "4 的前驱 (应为 3): " << (prev4 ? std::to_string(prev4->key) : "nullptr") << std::endl;
    std::cout << "1 的前驱 (应为 nullptr): " << (prev1 ? std::to_string(prev1->key) : "nullptr") << std::endl;

    std::cout << "\n--- 插入 ---" << std::endl;
    insert(6, root);
    std::cout << "插入 6 后的树:" << std::endl;
    printTree(root);
    orderedList.clear();
    inOrderTraversal(root, orderedList);
    std::cout << "中序遍历 (应包含 6): ";
    for (int n : orderedList) std::cout << n << " ";
    std::cout << std::endl;

    std::cout << "\n--- 删除 ---" << std::endl;
    std::cout << "删除 2 (叶节点):" << std::endl;
    delete_node(2, root);
    printTree(root);
    orderedList.clear();
    inOrderTraversal(root, orderedList);
    std::cout << "中序: ";
    for (int n : orderedList) std::cout << n << " ";
    std::cout << std::endl;

    std::cout << "\n删除 3 (两个子节点, 1 和 4):" << std::endl;
    delete_node(3, root);
    printTree(root);
    orderedList.clear();
    inOrderTraversal(root, orderedList);
    std::cout << "中序: ";
    for (int n : orderedList) std::cout << n << " ";
    std::cout << std::endl;

    std::cout << "\n删除 8 (两个子节点, 7 和 9):" << std::endl;
    delete_node(8, root);
    printTree(root);
    orderedList.clear();
    inOrderTraversal(root, orderedList);
    std::cout << "中序: ";
    for (int n : orderedList) std::cout << n << " ";
    std::cout << std::endl;
    
    std::cout << "\n删除 5 (根节点，有两个子节点):" << std::endl;
    delete_node(5, root);
    printTree(root);
    orderedList.clear();
    inOrderTraversal(root, orderedList);
    std::cout << "中序: ";
    for (int n : orderedList) std::cout << n << " ";
    std::cout << std::endl;

    // 释放内存
    freeTree(root);

    return 0;
}
