// 泛型小根堆 struct
template <typename T>
struct MinHeap {
    // 使用 vector 省去手动管理数组大小的麻烦
    vector<T> h;

    // 构造函数：初始化塞入一个占位符，保证下标从 1 开始
    MinHeap() {
        h.push_back(T()); 
    }

    // 【核心 1：上浮】
    // 只要比爸爸小，就跟爸爸换位置
    void up(int u) {
        while (u > 1 && h[u] < h[u / 2]) {
            swap(h[u], h[u / 2]);
            u /= 2;
        }
    }

    // 【核心 2：下沉】
    // 找出 (自己, 左孩子, 右孩子) 里最小的，只要最小的不是自己，就交换下去
    void down(int u) {
        int t = u;             // t 代表“最小值的下标”
        int left = u * 2;      // 左孩子
        int right = u * 2 + 1; // 右孩子

        // 如果左孩子存在，且比当前最小值还小，更新 t
        if (left < h.size() && h[left] < h[t]) t = left;
        // 如果右孩子存在，且比当前最小值还小，更新 t
        if (right < h.size() && h[right] < h[t]) t = right;

        // 如果最小值不是自己，说明需要交换并继续下沉
        if (t != u) {
            swap(h[u], h[t]);
            down(t); // 递归下沉
        }
    }

    // --- 用户接口 ---

    // 插入 x
    void push(T x) {
        h.push_back(x);      // 1. 放到数组末尾
        up(h.size() - 1);    // 2. 上浮到正确位置
    }

    // 删除堆顶
    void pop() {
        if (size() == 0) return;
        swap(h[1], h.back()); // 1. 拿末尾元素覆盖堆顶
        h.pop_back();         // 2. 删除末尾
        if (size() > 0) {     // 3. 堆顶下沉
            down(1);
        }
    }

    // 获取堆顶
    T top() {
        return h[1];
    }

    // 获取元素个数
    int size() {
        return h.size() - 1; // 减去那个占位符
    }
    
    bool empty() {
        return size() == 0;
    }
};