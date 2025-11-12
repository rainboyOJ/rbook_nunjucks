#include <memory_resource>
#include <iostream>

template <template<typename> class Node,typename T>
class BST_common_operation {
public:
    using NodeType = Node<T>;
    using NodePtr = NodeType*;

    // 左旋，让右孩子 y 上位，自己 x 下沉
    // 传入的 root 是一个指向子树根节点的指针的引用(NodePtr&), 方便直接修改
    // 口诀:  1. 查空  2. 过继 3. 调整父子关系 4. 更新根
    void rotateLeft(NodePtr &x)
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
    void rotateRight(NodePtr &y)
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

    enum class Color
    {
        RED,
        BLACK,
        DOUBLE_BLACK,
        ANY
    };
    
    // c++17 
    constexpr Color RED = Color::RED;
    constexpr Color BLACK = Color::BLACK;
    constexpr Color DOUBLE_BLACK = Color::DOUBLE_BLACK;
    constexpr Color ANY = Color::ANY;

    bool operator==(Color a, Color b) {
        if( a == Color::ANY || b == Color::ANY ) return true;
        return a == b;
    }
    bool operator!=(Color a, Color b) { return !(a == b); }



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
    };

    // helper
    // BST_common_operation<Node,T> COMMON_OPT;


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

            RBTree_Descriptor(const std::string & str) {
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
                    else if ( c == 'l') rot[rot_size].rotate_type = 'l';
                    else if ( c == 'r') rot[rot_size].rotate_type = 'r';
                }
            }

            bool match(NodePtr & root) {
                if( root->isEmpty() ) return false;
                if( root != ANY && root->color != root ) return false;
                if( l != ANY && root->left->color != l ) return false;
                if( r != ANY && root->right->color != r ) return false;
                if( ll != ANY && root->left->left->color != ll ) return false;
                if( lr != ANY && root->left->right->color != lr ) return false;
                if( rl != ANY && root->right->left->color != rl ) return false;
                if( rr != ANY && root->right->right->color != rr ) return false;
            }

            NodePtr & findNode(NodePtr & root,int id) {
                if( id == 0 ) return root;
                else if( id == 1 ) return root->left;
                else if( id == 2 ) return root->right;
                else if( id == 3 ) return root->left->left;
                else if( id == 4 ) return root->left->right;
                else if( id == 5 ) return root->right->left;
                else if( id == 6 ) return root->right->right;
            }

            NodePtr & rotate(NodePtr & root) {
                for(int i = 0; i < rot_size; i++) {
                    auto & node = findNode(root,rot[i].node_id);
                    if( rot[i].rotate_type == 'l' ) rotateLeft(node);
                    else if( rot[i].rotate_type == 'r' ) rotateRight(node);
                }
                return root;
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

        NodePtr &ins(T data,NodePtr & u)
        {
            if( u -> isEmpty() ) {
                u = new_node(data);
                makeBlack(u);
                return;
            }
            if( u -> data > data ) {
                return balance( ins(data,u->left) );
            } else {
                return balance( ins(data,u->right) );
            }
        }

        NodePtr & balance(NodePtr &node)
        {
            // 4 种情况
            const RBTree_Descriptor rotate_desc[4] = 
            {
                "B | R * | R * * * | 0r",
                "B | * R | * * R * | 0l",
                "B | R * | * R * * | 1l 0r",
                "B | * R | * * R * | 1r 0l"
            };
            for(int i = 0; i < 4; i++) {
                if( rotate_desc[i].match(node) ) {
                    auto & root = rotate_desc[i].rotate(node);
                    makeBlack(root->left);
                    makeBlack(root->right);
                    makeRed(root);
                    return root;
                }
            }
            return node; // 都不匹配,无需调整
            // // case 1 ll red-red
            // if( patternMatch(node, RBTree_Descriptor("B | R * | R * * *")) ) {
            //     // 1. 父节点变黑
            //     // 2. 祖父节点变红
            //     // 3. 祖父节点右旋
            //     rotateRight(node);
            // }
            // // case 2 rr red-red
            // else if( patternMatch(node, RBTree_Descriptor("B | * R | * * R *")) ) {
            //     // 1. 父节点变黑
            //     // 2. 祖父节点变红
            //     // 3. 祖父节点左旋
            //     rotateLeft(node);
            // }
            // // case 3 lr red-red
            // else if( patternMatch(node, RBTree_Descriptor("B | R * | * R * *")) ) {
            //     // 1. 父节点左旋
            //     // 2. 祖父节点右旋
            //     // 3. 父节点变黑
            //     // 4. 祖父节点变红
            //     rotateLeft(node->left);
            //     rotateRight(node);
            // }
            // // case 4 rl red-red
            // else if( patternMatch(node, RBTree_Descriptor("B | * R | * * R *")) ) {
            //     // 1. 父节点右旋
            //     // 2. 祖父节点左旋
            //     // 3. 父节点变黑
            //     // 4. 祖父节点变红
            //     rotateRight(node->right);
            //     rotateLeft(node);
            // }
            // else return node; // 都不匹配,无需调整

            // // 这是就证明匹配了,需要调整:统一处理
            // return node;
        }

        

        // 核心: 上移双黑
        void del(T data)
        {
        }

    private:
        std::string color_to_string(const Color& c) const {
            switch (c) {
                case Color::RED: return "R";
                case Color::BLACK: return "B";
                case Color::DOUBLE_BLACK: return "DB";
                case Color::ANY: return "_";
            }
            return "?";
        }

        void printRecursive(NodePtr node, const std::string& prefix, bool isLeft) const {
            if (node->isEmpty()) {
                return;
            }

            std::cout << prefix;
            std::cout << (isLeft ? "├──" : "└──");

            std::cout << " " << node->data << " (" << color_to_string(node->color) << ")" << std::endl;

            printRecursive(node->left, prefix + (isLeft ? "│   " : "    "), true);
            printRecursive(node->right, prefix + (isLeft ? "│   " : "    "), false);
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
    };

} // namespace RBTree
