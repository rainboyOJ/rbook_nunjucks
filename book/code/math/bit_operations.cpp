#include <bits/stdc++.h>
using namespace std;

using ull = unsigned long long;

// 第 k 位是否为 1，k 从 0 开始。
bool has_bit(ull x, int k) {
    return (x >> k) & 1ULL;
}

// 把第 k 位置 1。
ull set_bit(ull x, int k) {
    return x | (1ULL << k);
}

// 把第 k 位置 0。
ull clear_bit(ull x, int k) {
    return x & ~(1ULL << k);
}

// 翻转第 k 位。
ull flip_bit(ull x, int k) {
    return x ^ (1ULL << k);
}

// 保留最低位的 1。
ull lowbit(ull x) {
    return x & -x;
}

// 清除最低位的 1。
ull clear_lowbit(ull x) {
    return x & (x - 1);
}

// 统计二进制中 1 的数量。
int count_bits(ull x) {
    return __builtin_popcountll(x);
}

// 最高位 1 的位置，0 没有最高位，返回 -1。
int highest_bit_pos(ull x) {
    if (x == 0) return -1;
    return 63 - __builtin_clzll(x);
}

// 只保留最高位的 1。
ull keep_highbit(ull x) {
    if (x == 0) return 0;
    return 1ULL << highest_bit_pos(x);
}

// 清除最高位的 1。
ull clear_highbit(ull x) {
    return x ^ keep_highbit(x);
}

// 判断是否是 2 的幂。0 不是 2 的幂。
bool is_power_of_two(ull x) {
    return x > 0 && (x & (x - 1)) == 0;
}

// 枚举 mask 的所有非空子集。
vector<ull> non_empty_subsets(ull mask) {
    vector<ull> res;
    for (ull sub = mask; sub; sub = (sub - 1) & mask) {
        res.push_back(sub);
    }
    return res;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    ull x;
    if (!(cin >> x)) return 0;

    cout << "lowbit=" << lowbit(x) << '\n';
    cout << "popcount=" << count_bits(x) << '\n';
    cout << "highest_bit_pos=" << highest_bit_pos(x) << '\n';
    cout << "keep_highbit=" << keep_highbit(x) << '\n';
    cout << "clear_highbit=" << clear_highbit(x) << '\n';
    cout << "is_power_of_two=" << (is_power_of_two(x) ? "yes" : "no") << '\n';

    return 0;
}
