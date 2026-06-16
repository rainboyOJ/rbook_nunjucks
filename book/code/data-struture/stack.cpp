const int maxn = 100000 + 5;

template <typename T = int, int siz = maxn>
struct MyStack {
    T sta[siz + 5];
    int top_pos = 0;  // 指向栈顶元素后面的一个位置

    void clear() { top_pos = 0; }

    void push(const T& x) { sta[top_pos++] = x; }

    void pop() { --top_pos; }

    T& top() { return sta[top_pos - 1]; }

    const T& top() const { return sta[top_pos - 1]; }

    bool empty() const { return top_pos == 0; }

    int size() const { return top_pos; }
};
