#include <bits/stdc++.h>
using namespace std;

double f(double x) {
    return -(x - 3.0) * (x - 3.0) + 10.0;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    double left, right;
    cin >> left >> right;

    for (int iter = 0; iter < 200; iter++) {
        double m1 = left + (right - left) / 3.0;
        double m2 = right - (right - left) / 3.0;

        if (f(m1) < f(m2)) {
            left = m1;
        } else {
            right = m2;
        }
    }

    double x = (left + right) / 2.0;
    cout << fixed << setprecision(10) << x << ' ' << f(x) << '\n';
    return 0;
}
