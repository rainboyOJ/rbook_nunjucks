// 简化版快速IO模板
template<typename T>
inline void read(T& x) {
    x = 0;
    T f = 1;
    char c = getchar();
    while (c < '0' || c > '9') {
        if (c == '-') f = -1;
        c = getchar();
    }
    while (c >= '0' && c <= '9') {
        x = x * 10 + c - '0';
        c = getchar();
    }
    x = x * f;
}

template<typename T,typename... Args>
inline void read(T & x,Args&... args) {
    read(x); read(args...); //尾递归
}

template<typename T>
inline void write(T x) {
    if (x < 0) {
        putchar('-');
        x = -x;
    }
    if (x == 0) {
        putchar('0');
        return;
    }

    char buf[50];        // 足够存储128位整数
    int len = 0;         // 数字长度计数器
    while (x > 0) {      // 迭代提取每个数字位
        buf[len++] = x % 10 + '0';
        x /= 10;
    }
    // 反向输出得到正确顺序
    for (int i = len - 1; i >= 0; i--) {
        putchar(buf[i]);
    }
}

template<typename T,typename... Args>
inline void write(T x,Args... args) {
    write(x); write(args...); //尾递归
}
