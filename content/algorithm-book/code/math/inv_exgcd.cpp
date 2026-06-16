// !! 🧠记忆口诀 “递归反着传 (y, x)，回来减乘除 (y -= a/b*x)”
// 求解 ax + by = gcd(a, b)
// 返回值为 gcd(a, b)
template<typename T = long long>
T exgcd(T a, T b, T &x, T &y) {
    if (!b) {
        x = 1, y = 0;
        return a;
    }
    // 【核心技巧】注意这里的传参顺序：y, x
    // 这样递归回来后：x 存的是上一层的 y'，y 存的是上一层的 x'
    T d = exgcd(b, a % b, y, x);
    
    // 公式直接简化为一行：y = x' - (a/b) * y'
    // 此时当前的 y 已经是 x'，当前的 x 已经是 y'
    y -= a / b * x; 
    
    return d;
}

// 求 a 在模 m 下的逆元 (前提: gcd(a, m) = 1)
template<typename T = long long>
T inv(T a, T m) {
    T x, y;
    exgcd(a, m, x, y);
    // 使用 (x % m + m) % m 确保结果为正数
    return (x % m + m) % m;
}