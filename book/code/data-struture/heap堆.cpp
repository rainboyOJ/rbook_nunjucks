#include <bits/stdc++.h>
using namespace std;

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

    T top() const {
        return h[1];
    }

    void up(int u) {
        while (u > 1 && h[u] < h[u / 2]) {
            swap(h[u], h[u / 2]);
            u /= 2;
        }
    }

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

    void push(const T &x) {
        h.push_back(x);
        up(size());
    }

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
