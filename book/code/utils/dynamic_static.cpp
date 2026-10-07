// 模板动态化静态（内存池）：一次性在堆上申请 N 个槽位，
// get() 按顺序发放下标，之后仍可像静态数组一样用 head[i] 访问。
// 注意：这里只负责分配槽位与发放下标，不构造/析构槽位里的元素（对 POD 类型足够）。

#include <stdexcept>

template <typename T, int N = 10000>
struct dynamic_static {
    T *head;       // 指向 N 个 T 的连续槽位
    int idx;       // 下一个待发放的下标
    int capacity;  // 槽位总数，默认为模板参数 N，也可由构造函数指定

    // 默认容量取模板参数 N；也允许构造时指定运行时容量（与 Python 版的 __init__(n) 对应）。
    explicit dynamic_static(int n = N) : head(new T[n]), idx(0), capacity(n) {}

    ~dynamic_static() { delete[] head; }

    // 返回下一个空闲下标并后移指针；池耗尽时抛异常（与 Python 版的 IndexError 对应）。
    int get() {
        if (idx >= capacity) throw std::out_of_range("dynamic_static 池已耗尽");
        return idx++;
    }
};
