#include <cstdio>

// ==================== 快读快写模板 ====================

template <typename T>
inline void read(T &x) {
    x = 0;
    T f = 1;
    char ch = getchar();
    while (ch < '0' || ch > '9') {
        if (ch == '-') f = -1;
        ch = getchar();
    }
    while (ch >= '0' && ch <= '9') {
        x = x * 10 + (ch - '0');
        ch = getchar();
    }
    x *= f;
}

template <typename T, typename... Args>
inline void read(T &x, Args &...args) {
    read(x);
    read(args...);
}

template <typename T>
inline void write(T x) {
    if (x < 0) {
        putchar('-');
        x = -x;
    }
    if (x > 9) write(x / 10);
    putchar(x % 10 + '0');
}

template <typename T>
inline void write(T x, char end) {
    write(x);
    putchar(end);
}

template <typename T>
inline void writeln(T x) {
    write(x);
    putchar('\n');
}

// ==================== 业务逻辑 ====================

int main() {
    int n;
    read(n);
    long long sum = 0;
    for (int i = 0; i < n; ++i) {
        long long val;
        read(val);
        sum += val;
        write(sum, i == n - 1 ? '\n' : ' ');
    }
    return 0;
}
