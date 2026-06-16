#include <iostream>
#include <vector>
#include <cmath>  // 用于 log 函数

class EulerSieve {
private:
    std::vector<int> primes;
    std::vector<bool> is_composite;
    
public:
    // 获取 n 以内的所有素数
    void get_primes(int n) {
        // 重新分配空间
        is_composite.assign(n + 1, false);
        primes.clear();
        
        // 预分配空间提高性能（素数定理：π(n) ≈ n/ln(n)）
        primes.reserve(static_cast<int>(n / std::log(n)) + 10);
        
        for (long long i = 2; i <= n; i++) {
            if (!is_composite[i]) {
                primes.push_back(i);
            }
            
            for (size_t j = 0; j < primes.size(); j++) {
                long long product = static_cast<long long>(i) * primes[j];
                if (product > n) break;
                
                is_composite[product] = true;
                
                // 【核心关键】
                // 如果 i 被 p 整除，说明 i 的最小质因子就是 p。
                // 如果继续枚举下一个素数 p_next (p_next > p)，
                // 构造出的合数 X = i * p_next。
                // X 的最小质因子将会是 p (因为 i 包含因子 p)，而不是 p_next。
                // 这违背了"用最小质因子筛除"的原则，因此必须 break。
                if (i % primes[j] == 0) break;
            }
        }
    }
    
    // 获取素数列表
    const std::vector<int>& get_primes_list() const {
        return primes;
    }
    
    // 判断一个数是否是素数
    bool is_prime(int x) {
        if (x < 2) return false;
        if (x < is_composite.size()) {
            return !is_composite[x];
        }
        // 如果 x 超出当前筛选范围，需要重新筛选
        get_primes(x);
        return !is_composite[x];
    }
};

int main() {
    EulerSieve sieve;
    int n = 100;
    
    sieve.get_primes(n);
    const auto& primes = sieve.get_primes_list();
    
    std::cout << "1 到 " << n << " 之间的素数有 " << primes.size() << " 个:\n";
    for (size_t i = 0; i < primes.size(); i++) {
        std::cout << primes[i] << " ";
        if ((i + 1) % 10 == 0) std::cout << "\n";
    }
    std::cout << "\n";
    
    // 测试素数判断
    std::cout << "\n素数测试:\n";
    std::cout << "17 是素数吗? " << (sieve.is_prime(17) ? "是" : "否") << "\n";
    std::cout << "100 是素数吗? " << (sieve.is_prime(100) ? "是" : "否") << "\n";
    
    return 0;
}
