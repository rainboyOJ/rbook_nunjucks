const int MAX_N = 1000000;  // 最大范围
std::vector<long long> primes;
std::vector<bool> is_composite;
// bool is_composite[MAX_N + 1];  // true 表示是合数

void get_primes_euler(long long n) {
    // 初始化
    // for (int i = 0; i <= n; i++) is_composite[i] = false;
    is_composite.clear();
    is_composite.assign(n+5,0);
    primes.clear();
    
    for (int i = 2; i <= n; i++) {
        if (!is_composite[i]) primes.push_back(i);
        
        for (int j = 0; j < primes.size(); j++) {
            int p = primes[j];
            
            // 这个剪枝: (p * i) : 有没有超过枚举的范围
            // 1LL 防止溢出
            if (1LL *p * i > n) break;
            
            is_composite[i * p] = true;
            
            // 【核心关键】
            // 如果 i 被 p 整除，说明 i 的最小质因子就是 p。
            // 如果继续枚举下一个素数 p_next (p_next > p)，
            // 构造出的合数 X = i * p_next。
            // X 的最小质因子将会是 p (因为 i 包含因子 p)，而不是 p_next。
            // 这违背了"用最小质因子筛除"的原则，因此必须 break。
            if (i % p == 0) break;
        }
    }
}
