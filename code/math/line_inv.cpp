
long long inv[2000005]; // 数组开到最大范围
long long p; // 模数

void init_inverse(int n) {
    inv[1] = 1; // 【奠基】1的逆元是1
    
    for (int i = 2; i <= n; ++i) {
        // 【核心公式】
        // inv[i] = (p - p/i) * inv[p % i] % p
        inv[i] = (p - p / i) * inv[p % i] % p;
    }
}