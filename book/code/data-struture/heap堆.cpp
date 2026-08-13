#include <bits/stdc++.h>
using namespace std;

// 小根堆：支持 push / top / pop
// 隐式二叉堆：节点 u 的左孩子 2u，右孩子 2u+1，父节点 u/2
template <typename T>
struct MinHeap {
    // h[0] 不使用，让父子下标关系保持为 u/2、u*2、u*2+1。
    vector<T> h;

    MinHeap() {
        h.push_back(T());
    }

    int size() const {
        return (int)h.size() - 1;
    }

    bool empty() const {
        return size() == 0;
    }

    // 堆顶（最小值）
    T top() const {
        return h[1];
    }

    // 上浮：节点 u 若比父节点小则交换，恢复堆性质
    void up(int u) {
        while (u > 1 && h[u] < h[u / 2]) {
            swap(h[u], h[u / 2]);
            u /= 2;
        }
    }

    // 下沉：节点 u 与较小的孩子交换，恢复堆性质
    void down(int u) {
        while (true) {
            int best = u;
            int left = u * 2;
            int right = u * 2 + 1;

            if (left <= size() && h[left] < h[best]) best = left;
            if (right <= size() && h[right] < h[best]) best = right;
            if (best == u) break;

            swap(h[u], h[best]);
            u = best;
        }
    }

    // 插入值 x
    void push(const T &x) {
        h.push_back(x);
        up(size());
    }

    // 删除堆顶
    void pop() {
        if (empty()) return;
        h[1] = h.back();
        h.pop_back();
        if (!empty()) down(1);
    }
};

int main() {
    int n;
    cin >> n;

    MinHeap<int> heap;
    while (n--) {
        int op;
        cin >> op;
        if (op == 1) {
            int x;
            cin >> x;
            heap.push(x);
        } else if (op == 2) {
            cout << heap.top() << "\n";
        } else if (op == 3) {
            heap.pop();
        }
    }

    return 0;
}
