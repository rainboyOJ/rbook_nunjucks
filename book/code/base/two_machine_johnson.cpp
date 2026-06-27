#include <algorithm>
#include <iostream>
#include <vector>
using namespace std;

struct Job {
    int id;
    long long a;
    long long b;
};

bool johnson_cmp(const Job& x, const Job& y) {
    const bool x_front = x.a <= x.b;
    const bool y_front = y.a <= y.b;

    if (x_front != y_front) return x_front > y_front;
    if (x_front) return x.a < y.a;
    return x.b > y.b;
}

long long finish_time(const vector<Job>& jobs) {
    long long finish_m1 = 0;
    long long finish_m2 = 0;

    for (const Job& job : jobs) {
        finish_m1 += job.a;
        finish_m2 = max(finish_m2, finish_m1) + job.b;
    }

    return finish_m2;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n;
    cin >> n;

    vector<Job> jobs(n);
    for (int i = 0; i < n; ++i) {
        cin >> jobs[i].a >> jobs[i].b;
        jobs[i].id = i + 1;
    }

    sort(jobs.begin(), jobs.end(), johnson_cmp);

    cout << finish_time(jobs) << '\n';
    for (int i = 0; i < n; ++i) {
        if (i) cout << ' ';
        cout << jobs[i].id;
    }
    cout << '\n';

    return 0;
}
