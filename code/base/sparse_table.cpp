#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>

using namespace std;

const int MAXN = 100005;
const int LOGN = 17; // ceil(log2(100005))

int st[MAXN][LOGN];
// log_table[i] = floor(log2(i))
// 原理公式: log2(x) = floor(log(i/2)) +1
//由对数运算法则 $\log_2(x/2) = \log_2(x) - \log_2(2) = \log_2(x) - 1$，且向下取整函数满足性质 $\lfloor y - 1 \rfloor = \lfloor y \rfloor - 1$，移项即得证。
int log_table[MAXN]; 

// 预处理对数表，用于O(1)计算log2
template<typename T>
void precompute_log(int n) {
    log_table[1] = 0;
    for (int i = 2; i <= n; i++) {
        log_table[i] = log_table[i / 2] + 1;
    }
}

// 构建ST表
template<typename T>
void build_st(const vector<T>& arr) {
    int n = arr.size();
    precompute_log(n);

    // 初始化长度为 2^0 = 1 的区间
    for (int i = 0; i < n; i++) {
        st[i][0] = arr[i];
    }

    // 递推构建
    for (int j = 1; (1 << j) <= n; j++) {
        for (int i = 0; i + (1 << j) <= n; i++) {
            st[i][j] = max(st[i][j - 1], st[i + (1 << (j - 1))][j - 1]);
        }
    }
}

// 查询区间 [l, r] 的最大值 (0-indexed)
template<typename T>
T query(int l, int r) {
    int k = log_table[r - l + 1];
    return max(st[l][k], st[r - (1 << k) + 1][k]);
}

// 通用ST表类，支持最大值、最小值等
template<typename T, typename Compare = less<T>>
class SparseTable {
private:
    vector<vector<T>> st;
    vector<int> log_table;
    int n;
    Compare cmp;

public:
    SparseTable(const vector<T>& arr, Compare comp = Compare()) : n(arr.size()), cmp(comp) {
        // 预处理对数表
        log_table.resize(n + 1);
        log_table[1] = 0;
        for (int i = 2; i <= n; i++) {
            log_table[i] = log_table[i / 2] + 1;
        }

        // 初始化st表
        st.resize(n, vector<T>(log_table[n] + 1));
        for (int i = 0; i < n; i++) {
            st[i][0] = arr[i];
        }

        // 递推构建
        for (int j = 1; (1 << j) <= n; j++) {
            for (int i = 0; i + (1 << j) <= n; i++) {
                st[i][j] = cmp(st[i][j - 1], st[i + (1 << (j - 1))][j - 1]) ? st[i][j - 1] : st[i + (1 << (j - 1))][j - 1];
            }
        }
    }

    // 查询区间 [l, r] 的值 (0-indexed)
    T query(int l, int r) {
        int k = log_table[r - l + 1];
        return cmp(st[l][k], st[r - (1 << k) + 1][k]) ? st[l][k] : st[r - (1 << k) + 1][k];
    }
};

// 竞赛用简洁模板
struct ST {
    static const int MAXN = 100005;
    static const int LOGN = 17;
    int st[MAXN][LOGN];
    int log_table[MAXN];
    int n;

    void init(vector<int>& arr) {
        n = arr.size();
        // 预处理对数表
        log_table[1] = 0;
        for (int i = 2; i <= n; i++) {
            log_table[i] = log_table[i / 2] + 1;
        }

        // 初始化长度为 2^0 = 1 的区间
        for (int i = 0; i < n; i++) {
            st[i][0] = arr[i];
        }

        // 递推构建
        for (int j = 1; (1 << j) <= n; j++) {
            for (int i = 0; i + (1 << j) <= n; i++) {
                st[i][j] = max(st[i][j - 1], st[i + (1 << (j - 1))][j - 1]);
            }
        }
    }

    // 查询区间 [l, r] 的最大值 (0-indexed)
    int query(int l, int r) {
        int k = log_table[r - l + 1];
        return max(st[l][k], st[r - (1 << k) + 1][k]);
    }
};

int main() {
    // --- 测试用例 ---
    vector<int> arr = {3, 1, 4, 1, 5, 9, 2, 6};
    int n = arr.size();

    // 方法1: 使用全局函数
    build_st(arr);
    cout << "Array: ";
    for (int x : arr) {
        cout << x << " ";
    }
    cout << endl << endl;

    // 测试查询
    int l1 = 0, r1 = 4; // 区间 [3, 1, 4, 1, 5], 期望最大值: 5
    cout << "Query [" << l1 << ", " << r1 << "]: " << query(l1, r1) << endl;

    int l2 = 3, r2 = 7; // 区间 [1, 5, 9, 2, 6], 期望最大值: 9
    cout << "Query [" << l2 << ", " << r2 << "]: " << query(l2, r2) << endl;

    int l3 = 2, r3 = 2; // 区间 [4], 期望最大值: 4
    cout << "Query [" << l3 << ", " << r3 << "]: " << query(l3, r3) << endl;

    // 方法2: 使用ST类
    cout << "\n--- 使用ST类 ---" << endl;
    ST st_obj;
    st_obj.init(arr);
    cout << "Query [" << l1 << ", " << r1 << "]: " << st_obj.query(l1, r1) << endl;
    cout << "Query [" << l2 << ", " << r2 << "]: " << st_obj.query(l2, r2) << endl;
    cout << "Query [" << l3 << ", " << r3 << "]: " << st_obj.query(l3, r3) << endl;

    return 0;
}
