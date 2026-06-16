// ==========================================
// 模板 : 质因数分解 (Prime Factorization)
// 将 n 分解为 p1^c1 * p2^c2 ... 的形式
// 返回值: vector<pair<质数, 指数>>
// 时间复杂度: O(sqrt(n))
// ==========================================
// vector<pair<ll, int>> 
void get_prime_factors(ll n) {
    // vector<pair<ll, int>> factors;
    
    // 从 2 遍历到 sqrt(n)
    for (ll i = 2; i * i <= n; ++i) {
        if (n % i == 0) {
            int cnt = 0;
            // 除尽当前的质因子 i
            while (n % i == 0) {
                cnt++;
                n /= i;
            }
            // factors.push_back({i, cnt});
            printf("(%d,%d)", i, cnt);
        }
    }
    
    // 如果 n > 1，说明剩下的 n 本身就是一个大于 sqrt(原始n) 的质数
    if (n > 1) {
        // factors.push_back({n, 1});
        printf("(%d,%d)", n, 1);
    }
    // return factors;
}
