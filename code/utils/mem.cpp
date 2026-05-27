#include <cstddef> // For std::byte, std::size_t
#include <new>     // For placement new, std::align_val_t
#include <utility> // For std::forward

/**
 * @brief 一个使用堆内存的线性内存池 (Arena Allocator).
 *
 * 用于在竞赛中快速分配大量相同类型的小对象, 避免多次 `new` 的开销.
 * 特点:
 * 1. O(1) 复杂度分配.
 * 2. 在构造时从堆上分配一大块内存, 允许比栈或静态内存区更大的容量.
 * 3. 缓存友好 (因为分配是线性的).
 *
 * @tparam T 要分配的对象的类型.
 * @tparam N 池中可以容纳的对象最大数量.
 */
template <typename T, int N = 100000>
struct DynamicPool {
    // 指向下一块可用内存的索引.
    int head_idx = 0;

    // 指向在堆上分配的内存块.
    std::byte* buffer = nullptr;

    // 构造函数: 在堆上申请内存
    DynamicPool() {
        // 使用 C++17 的对齐 new 来获取保证对齐的内存, 避免未定义行为.
        buffer = new (std::align_val_t(alignof(T))) std::byte[N * sizeof(T)];
    }

    // 析构函数: 释放内存
    ~DynamicPool() {
        // 必须使用与对齐 new 匹配的 delete.
        operator delete[](buffer, std::align_val_t(alignof(T)));
    }

    // 禁止拷贝和移动, 避免指针管理的复杂性 (例如重复释放).
    // 内存池通常不应该被复制或移动.
    DynamicPool(const DynamicPool&) = delete;
    DynamicPool& operator=(const DynamicPool&) = delete;
    DynamicPool(DynamicPool&&) = delete;
    DynamicPool& operator=(DynamicPool&&) = delete;

    /**
     * @brief 从池中分配一个对象, 并使用给定的参数构造它.
     *
     * @tparam Args 构造函数参数的类型.
     * @param args 传递给 T 的构造函数的参数.
     * @return 指向新构造对象的指针.
     */
    template <typename... Args>
    T* alloc(Args&&... args) {
        // 在竞赛环境下, 为追求极致性能, 通常假设不会超出内存池大小.
        // assert(head_idx < N);

        void* place = &buffer[head_idx * sizeof(T)];
        head_idx++;
        
        return new (place) T(std::forward<Args>(args)...);
    }

    int get() { return head_idx++; }

    // 重载 [] 运算符, 允许通过索引访问对象.
    T& operator[](int idx) {
        return *reinterpret_cast<T*>(&buffer[idx * sizeof(T)]);
    }

    /**
     * @brief 重置内存池, 使所有内存可重新使用.
     *
     * 注意: 这不会调用池中已分配对象的析构函数.
     * 在大多数竞赛场景中, 对象生命周期随程序结束, 不需要手动析构.
     */
    void reset() {
        head_idx = 0;
    }
};
