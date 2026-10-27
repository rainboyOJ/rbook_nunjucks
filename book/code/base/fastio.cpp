#include <cstdio>
#include <cctype>

/**
 * 竞赛极简快速输入输出模板 (Fast I/O)
 *
 * 核心特点：
 * 1. 结构对称：read() 先滤非数字、再算十进制；write() 先判负、再递归除 10 输出。
 * 2. 极度精简：无需开外层栈数组与维护指针，代码行数极少，可在 1 分钟内手工默写。
 * 3. 泛型适配：支持 int、long long、__int128 等所有整型类型，支持单行多变量读入。
 */

// 快速读入：支持所有有符号与无符号整型
template <typename T>
inline void read(T &x) {
    x = 0;
    T f = 1;
    char ch = getchar();
    // 过滤空白字符并识别负号
    while (ch < '0' || ch > '9') {
        if (ch == '-') f = -1;
        ch = getchar();
    }
    // 秦九韶算法累加各位数值
    while (ch >= '0' && ch <= '9') {
        x = x * 10 + (ch - '0');
        ch = getchar();
    }
    x *= f;
}

// 可变参数快速读入：一条语句读取多个变量，如 read(n, m, k)
template <typename T, typename... Args>
inline void read(T &x, Args &...args) {
    read(x);
    read(args...);
}

// 快速输出：利用递归栈自动倒序输出各位数字
template <typename T>
inline void write(T x) {
    if (x < 0) {
        putchar('-');
        x = -x;
    }
    if (x > 9) write(x / 10);
    putchar(x % 10 + '0');
}

// 快捷带后缀字符输出（如空格、换行符）
template <typename T>
inline void write(T x, char end) {
    write(x);
    putchar(end);
}

// 快捷带换行输出
template <typename T>
inline void writeln(T x) {
    write(x);
    putchar('\n');
}
