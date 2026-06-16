#include <bits/stdc++.h>
using namespace std;

struct Item {
    int weight;
    int value;
};

int main() {
    int group_count, capacity;
    cin >> group_count >> capacity;

    vector<int> dp(capacity + 1, 0);

    for (int group_id = 1; group_id <= group_count; group_id++) {
        int item_count;
        cin >> item_count;

        vector<Item> group(item_count);
        for (int i = 0; i < item_count; i++) {
            cin >> group[i].weight >> group[i].value;
        }

        vector<int> previous = dp;
        for (int c = 0; c <= capacity; c++) {
            dp[c] = previous[c];
            for (const Item &item : group) {
                if (c < item.weight) continue;
                dp[c] = max(dp[c],
                            previous[c - item.weight] + item.value);
            }
        }
    }

    cout << dp[capacity] << "\n";
    return 0;
}
