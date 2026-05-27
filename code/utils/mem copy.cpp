#include <cstddef> // C++17, for std::byte
#include <new>     // for placement new
#include <utility> // for std::forward

/**
 * @brief 一个简单的线性内存池 (Arena Allocator).
 *
 * 用于在竞赛中快速分配大量相同类型的小对象, 避免 `new` 的开销.
 * 特点:
 * 1. O(1) 复杂度分配.
 * 2. 内存区域在编译时确定, 避免运行时 `malloc` 失败.
 * 3. 缓存友好.
 *
 * @tparam T 要分配的对象的类型.
 * @tparam N 池中可以容纳的对象最大数量.
 */
template <typename T, int N = 100000>
struct SimplePool {
    // 指向下一块可用内存的索引.
    int head_idx = 0;

    // 预先分配的内存缓冲区.
    // alignas(T) 确保内存对齐, 这对于某些复杂类型是必须的, 能够避免未定义行为.
    // 使用 std::byte (C++17) 来表示原始内存, 比 char* 更类型安全.
    alignas(T) std::byte buffer[N * sizeof(T)];

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

        // 计算下一个可用地址
        void* place = &buffer[head_idx * sizeof(T)];
        head_idx++;

        // 在该地址上使用 placement new 构造对象
        // std::forward 实现了完美转发, 可以保持参数的左值/右值属性.
        return new (place) T(std::forward<Args>(args)...);
    }

    /**
     * @brief 重置内存池, 使所有内存可重新使用.
     *
     * 注意: 这不会调用池中已分配对象的析构函数.
     * 如果 T 类型需要析构 (例如释放其他资源), 你需要手动处理.
     * 在大多数竞赛场景中, 对象生命周期随程序结束, 不需要手动析构.
     */
    void reset() {
        head_idx = 0;
    }
};
