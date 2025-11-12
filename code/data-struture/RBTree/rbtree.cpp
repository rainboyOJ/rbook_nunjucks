#include <memory_resource>
#include <iostream>
#include <string>
#include <string_view>

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
        // 1. 定义新根 y (x的右孩子)
        NodePtr y = x->right;
        if (x->isEmpty() || y->isEmpty())
            return; // 节点或右孩子为空，无法左旋

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
        // 1. 定义新根 y (x的左孩子)
        NodePtr x = y->left;
        if (x->isEmpty() || y->isEmpty())
            return; // 节点或左孩子为空，无法右旋

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

namespace RBTree
{
    using std::operator""sv;

    enum class Color
    {
        RED, // 🔴
        BLACK, // ⚫
        DOUBLE_BLACK, // ⚫⚫
        ANY // 任意颜色,主要用于[匹配]
    };
    
    // c++17 
    constexpr Color RED = Color::RED;
    constexpr Color BLACK = Color::BLACK;
    constexpr Color DOUBLE_BLACK = Color::DOUBLE_BLACK;
    constexpr Color ANY = Color::ANY;

    std::string color_to_string(const Color &c)
    {
        switch (c)
        {
        case Color::RED:
            return "R";
        case Color::BLACK:
            return "B";
        case Color::DOUBLE_BLACK:
            return "DB";
        case Color::ANY:
            return "_";
        }
        return "?";
    }

    // bool operator==(Color a, Color b) {
    //     if( a == Color::ANY || b == Color::ANY ) return true;
    //     return a == b;
    // }
    // bool operator!=(Color a, Color b) { return !(a == b); }

    template <typename T>
    class Node;


    template <typename T>
    class Node : public BST_common_operation<Node, T>
    {
    public:
        using NodeType = Node<T>;
        using NodePtr = NodeType *;

    public:
        T data;
        Color color;
        NodePtr left, right, parent;
        // NodePtr left, right;

        Node(T data) : data(data), color(RED), left(this), right(this), parent(this) {}
        Node(T data,Color color) : data(data), color(color), left(this), right(this), parent(this) {}

        Node(T data, Color color, Node<T> *left, Node<T> *right, Node<T> *parent)
            : data(data), color(color), left(left), right(right), parent(parent) {}

        // Empty Node
        // C++17 inline static member allows initialization inside the class definition.
        inline static NodeType Empty{T(), BLACK};
        inline static NodeType BBEmpty{T(), DOUBLE_BLACK};
        inline static NodeType *NIL = &Empty;
        inline static NodeType *BBNIL = &BBEmpty;

        bool isEmpty() const { return this == NIL || this == BBNIL; }
        bool isBBEmpty() const { return this == BBNIL; }
        bool isRed() const { return color == RED; }
        bool isBlack() const { return color == BLACK; }
        bool isDoubleBlack() const { return color == DOUBLE_BLACK; }


        void debug() const;
    };

    // helper
    // BST_common_operation<Node,T> COMMON_OPT;
    template <typename T>
    void printRecursive(const Node<T>* node, const std::string &prefix, bool isLeft)
    {
        if (node->isEmpty())
        {
            return;
        }

        std::cout << prefix;
        std::cout << (isLeft ? "├──" : "└──");

        std::cout << " " << node->data << " (" << color_to_string(node->color) << ")" << std::endl;

        printRecursive(node->left, prefix + (isLeft ? "│   " : "    "), true);
        printRecursive(node->right, prefix + (isLeft ? "│   " : "    "), false);
    }

    // 实现 Node::debug()
    template <typename T>
    void Node<T>::debug() const
    {
#ifdef RBTree_DEBUG
        // printRecursive(this, "", false);
        if (this->isEmpty())
            return;
        // 1. 直接打印当前节点信息
        std::cout << this->data << " (" << color_to_string(this->color) << ")" << std::endl;
        // 2. 递归打印左右子树，初始前缀为空
        printRecursive(this->left, "", true); // 如果有右子树，则左子树不是最后一个
        printRecursive(this->right, "", false);
#endif
    }

    template <typename T,int SIZE = 1000005>
    class RBTree
    {
    public:
        Node<T> *root;
        using NodeType = Node<T>;
        using NodePtr = NodeType *;

        //https://cppreference.cn/w/cpp/memory/monotonic_buffer_resource
        std::pmr::monotonic_buffer_resource pool{SIZE * sizeof(Node<T>)};
        std::pmr::polymorphic_allocator<Node<T>> alloc{&pool};

        NodePtr new_node(const T& data) {
            auto ptr = alloc.allocate(1);
            ptr->data = data;
            ptr->color = RED;
            ptr->left = ptr->right = ptr->parent = Node<T>::NIL;
            return ptr;
        }

    private:
        // 一个三层的树的描述
        // B | R B | * * B R
        // B | R B | * * B R | 1r 7r
        struct RBTree_Descriptor {
            union {
                Color desc[7];
                struct {
                    Color root;
                    Color l, r;
                    Color ll, lr, rl, rr;
                };
            };
            int rot_size = 0;
            // 旋转序列 ,一个两位数字
            // 十位数: 1 表示左旋, 2 表示右旋
            // 个位数: 表示旋转结点的编号
            // 17 表示 7 号结点左旋
            struct {
                int node_id;
                char rotate_type;
            } rot[10]; 

            RBTree_Descriptor(std::string_view str) {
                int i = 0;
                for(auto c : str) {
                    // if (isspace(c) || c == '|') continue;
                    // if (i >= 7) break;
                    if( c == 'B' ) desc[i++] = BLACK;
                    else if( c == 'R' ) desc[i++] = RED;
                    else if( c == 'D' ) desc[i++] = DOUBLE_BLACK;
                    else if( c == '*' ) desc[i++] = ANY;
                    else if ( isdigit(c) ) {
                        rot[rot_size++].node_id = c - '0';
                    }
                    else if ( c == 'l')
                        rot[rot_size-1].rotate_type = 'l';
                    else if ( c == 'r')
                        rot[rot_size-1].rotate_type = 'r';
                }
            }

            bool match(NodePtr & u) const {
                if( u->isEmpty() ) return false;
                if( root != ANY && u->color != root ) return false;
                if( l != ANY && u->left->color != l ) return false;
                if( r != ANY && u->right->color != r ) return false;
                if( ll != ANY && u->left->left->color != ll ) return false;
                if( lr != ANY && u->left->right->color != lr ) return false;
                if( rl != ANY && u->right->left->color != rl ) return false;
                if( rr != ANY && u->right->right->color != rr ) return false;
                return true; // 都匹配
            }

            NodePtr & findNode(NodePtr & root,int id) const {
                if( id == 0 ) return root;
                else if( id == 1 ) return root->left;
                else if( id == 2 ) return root->right;
                else if( id == 3 ) return root->left->left;
                else if( id == 4 ) return root->left->right;
                else if( id == 5 ) return root->right->left;
                else if( id == 6 ) return root->right->right;
                return root; // 都不匹配,无需调整
            }

            NodePtr & rotate(NodePtr & root) const {
                for(int i = 0; i < rot_size; i++) {
                    auto & node = findNode(root,rot[i].node_id);
                    if( rot[i].rotate_type == 'l' )  NodeType::rotateLeft(node);
                    else if( rot[i].rotate_type == 'r' ) NodeType::rotateRight(node);

#ifdef RBTree_DEBUG
                    cout << "after rotate: " << rot[i].node_id << " " << rot[i].rotate_type << endl;
                    root->debug();
                    cout << "-----------------------------------" << endl;
#endif
                }
                return root;
            }

            void debug()  const {
                for(int i = 0; i < 7; i++) {
                    // std::cout << (desc[i] == BLACK ? "B" : "R") ;
                    char c;
                    if( desc[i] == BLACK ) c = 'B';
                    else if( desc[i] == RED ) c = 'R';
                    else if( desc[i] == DOUBLE_BLACK ) c = 'D';
                    else if( desc[i] == ANY ) c = '*';
                    std::cout << c;
                    std::cout << (( i == 0 || i == 2  || i == 6) ? " | "  : " ");
                }
                std::cout << std::endl;
            }

        };
    
    public:

        RBTree() :root(Node<T>::NIL)  {}

        // _ 表示 anyColor
        // Color :  B black ,BB double black,  R red,
        // 设计:  使用字符串匹配
        
        // Insert a new node with the given data.
        // 核心: 把红色上移一层
        // 过程 把新节点插入到叶子节点，然后调整颜色。

        void makeBlack(NodePtr node) {
            if( node->isEmpty() ) return;
             node->color = BLACK; 
        }

        void makeRed(NodePtr node) { 
            if( node->isEmpty() ) return;
            node->color = RED; 
        }

        void insert(T data) {
            makeBlack( ins(data,root) );
        }

        // 返回: 调整后的根节点
        NodePtr &ins(T data,NodePtr & u)
        {
            if( u -> isEmpty() ) {
                u = new_node(data); // 新节点默认是红色
                // makeBlack(u);
                return u;
            }
            if( u -> data > data ) {
                ins(data,u->left);
            } else {
                ins(data,u->right);
            }
            return balance(u);
        }

        NodePtr & balance(NodePtr &node)
        {
            // 4 种情况, 为什么使用static: 避免每次调用都重新构造
            static const RBTree_Descriptor rotate_desc[4] = 
            {
                "B | R * | R * * * | 0r"sv,
                "B | * R | * * * R | 0l"sv,
                "B | R * | * R * * | 1l 0r"sv,
                "B | * R | * * R * | 2r 0l"sv
            };
            for(int i = 0; i < 4; i++) {
                if( rotate_desc[i].match(node) ) {
#ifdef RBTree_DEBUG
                    cout << " match: " << i << " node: " << node->data <<  " -> ";
                    rotate_desc[i].debug();
#endif
                    rotate_desc[i].rotate(node);
                    makeBlack(node->left); // 提升红色
                    makeBlack(node->right);
                    makeRed(node);
                    return node;
                }
            }
            return node; // 都不匹配,无需调整
        }

        

        // 核心: 上移双黑
        void del(T data)
        {
        }

    public:
        // Print the tree.
        void print() const {
            if (root->isEmpty()) {
                std::cout << "Tree is empty." << std::endl;
            } else {
                printRecursive(root, "", false);
            }
        }

    private:
        /**
         * @brief 递归地验证红黑树的属性并计算黑高。
         * @param node 当前子树的根。
         * @return 如果子树是有效的红黑树，则返回其黑高；否则返回 -1。
         */
                int validateRecursive(const Node<T>* node) const {
            // 属性3: 叶子节点(NIL)是黑色的。
            // 我们将NIL节点的黑高视为1。
            if (node->isEmpty()) {
                return 1;
            }

            // 递归检查左右子树
            int leftBlackHeight = validateRecursive(node->left);
            int rightBlackHeight = validateRecursive(node->right);

            // 如果任一子树无效，则整棵树无效
            if (leftBlackHeight == -1 || rightBlackHeight == -1) {
                return -1;
            }

            // 属性5: 从任一节点到其每个叶子的所有简单路径都包含相同数目的黑色节点。
            if (leftBlackHeight != rightBlackHeight) {
                // 黑高不匹配
                std::cout << "Validation Error: Black-height mismatch at node " << node->data << std::endl;
                return -1;
            }

            // 属性4: 如果一个节点是红色的，则它的两个子节点都是黑色的。
            if (node->isRed()) {
                if (node->left->isRed() || node->right->isRed()) {
                    // 红色节点的子节点不能是红色
                    std::cout << "Validation Error: Red node " << node->data << " has red child." << std::endl;
                    return -1;
                }
            }

            // 计算当前节点的黑高
            return leftBlackHeight + (node->isBlack() ? 1 : 0);
        }

    public:
        /**
         * @brief 验证整个红黑树是否符合所有5条属性。
         * @return 如果树是有效的红黑树，则返回 true，否则返回 false。
         *
         * 红黑树的5条属性:
         * 1. 每个节点要么是红色，要么是黑色。 (由Color枚举保证)
         * 2. 根节点是黑色的。
         * 3. 每个叶子节点（NIL）是黑色的。 (在实现中保证)
         * 4. 如果一个节点是红色的，则它的两个子节点都是黑色的。
         * 5. 对每个节点，从该节点到其所有后代叶节点的简单路径上，均包含相同数目的黑色节点。
         */
        bool isValid() const {
            // 属性2: 根节点是黑色的。
            if (root->isRed()) {
                std::cout << "Validation Error: Root is not black." << std::endl;
                return false;
            }

            // 从根节点开始递归验证，并检查黑高是否有效
            if (validateRecursive(root) == -1) {
                // 如果validateRecursive返回-1，说明树的某个属性被违反了。
                // 具体的错误会在递归函数中被检测到，这里我们只关心最终结果。
                return false;
            }

            return true;
        }
    };

} // namespace RBTree
