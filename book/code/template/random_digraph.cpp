#include <bits/stdc++.h>
using namespace std;

mt19937 rng((unsigned)chrono::steady_clock::now().time_since_epoch().count());

bool hit(double probability) {
    return uniform_real_distribution<double>(0.0, 1.0)(rng) <= probability;
}

int main() {
    int n = 5;
    double p = 0.30;

    vector<pair<int, int>> edges;
    for (int u = 1; u <= n; ++u) {
        for (int v = 1; v <= n; ++v) {
            if (u != v && hit(p)) edges.push_back({u, v});
        }
    }

    cout << n << ' ' << edges.size() << '\n';
    for (auto [u, v] : edges) {
        cout << u << ' ' << v << '\n';
    }

    return 0;
}
