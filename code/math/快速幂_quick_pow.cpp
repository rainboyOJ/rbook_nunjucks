// ==========================================
// 模板 : 快速幂 (Modular Exponentiation)
// 计算 (base^exp) % mod
// 时间复杂度: O(log exp)
// ==========================================
ll quick_pow(ll base, ll exp, ll mod) {
    ll res = 1;
    base %= mod; // 防止 base 初始值就大于 mod

    for( ; exp  ; exp >= 1) {
        // 是1就乘
        if (exp & 1) res = (res * base) % mod;
        
        // base 自乘
        base = (base * base) % mod;
    }
    return res;
}