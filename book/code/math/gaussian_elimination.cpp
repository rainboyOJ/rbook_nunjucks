#include <bits/stdc++.h>
using namespace std;

const double EPS = 1e-9;

// 解 n 元一次方程组，增广矩阵 a 的大小为 n x (n+1)。
// 返回值：0 无解，1 唯一解，2 无穷多解。
int gaussian_elimination(vector<vector<double>> a, vector<double>& ans) {
    int n = (int)a.size();
    int row = 0;
    vector<int> where(n, -1);

    for (int col = 0; col < n && row < n; col++) {
        int pivot = row;
        for (int i = row; i < n; i++) {
            if (fabs(a[i][col]) > fabs(a[pivot][col])) pivot = i;
        }

        if (fabs(a[pivot][col]) < EPS) continue;
        swap(a[pivot], a[row]);
        where[col] = row;

        double div = a[row][col];
        for (int j = col; j <= n; j++) a[row][j] /= div;

        for (int i = 0; i < n; i++) {
            if (i == row) continue;
            double factor = a[i][col];
            for (int j = col; j <= n; j++) {
                a[i][j] -= factor * a[row][j];
            }
        }

        row++;
    }

    ans.assign(n, 0);
    for (int i = 0; i < n; i++) {
        if (where[i] != -1) ans[i] = a[where[i]][n];
    }

    for (int i = 0; i < n; i++) {
        double sum = 0;
        for (int j = 0; j < n; j++) sum += ans[j] * a[i][j];
        if (fabs(sum - a[i][n]) > EPS) return 0;
    }

    for (int i = 0; i < n; i++) {
        if (where[i] == -1) return 2;
    }
    return 1;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    vector<vector<double>> a(n, vector<double>(n + 1));
    for (int i = 0; i < n; i++) {
        for (int j = 0; j <= n; j++) cin >> a[i][j];
    }

    vector<double> ans;
    int status = gaussian_elimination(a, ans);

    if (status == 0) {
        cout << "No solution\n";
    } else if (status == 2) {
        cout << "Infinite solutions\n";
    } else {
        cout.setf(ios::fixed);
        cout << setprecision(6);
        for (double x : ans) cout << x << '\n';
    }

    return 0;
}
