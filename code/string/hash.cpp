#include <vector>
using namespace std;

typedef unsigned long long ull;

struct Hash {
    vector<ull> h, p;
    ull P = 131; 

    // 构造函数：传入 char 数组的头指针 s 和长度 n
    // 假设：字符串内容存储在 s[1]...s[n] (配合 scanf("%s", s+1))
    Hash(char *s, int n) {
        h.resize(n + 1);
        p.resize(n + 1);
        
        p[0] = 1;
        for (int i = 1; i <= n; ++i) {
            h[i] = h[i - 1] * P + s[i]; // 直接读取 s[i]，完全符合 1-based 习惯
            p[i] = p[i - 1] * P;
        }
    }

    // 获取区间 [l, r] 的哈希值
    ull get(int l, int r) {
        if (l > r) return 0;
        return h[r] - h[l - 1] * p[r - l + 1];
    }
};