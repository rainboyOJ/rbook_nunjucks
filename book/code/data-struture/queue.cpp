const int maxn = 100000 + 5;

template <typename T = int, int siz = maxn>
struct MyQueue {
    T q[siz + 5];
    int head = 0, tail = 0;  // 当前队列区间为 [head, tail)

    void clear() { head = tail = 0; }

    void push(const T& x) { q[tail++] = x; }

    void pop() { ++head; }

    // 竞赛中有时需要从队尾删除元素，例如单调队列。
    void pop_back() { --tail; }

    T& front() { return q[head]; }

    const T& front() const { return q[head]; }

    T& back() { return q[tail - 1]; }

    const T& back() const { return q[tail - 1]; }

    bool empty() const { return head == tail; }

    int size() const { return tail - head; }
};
