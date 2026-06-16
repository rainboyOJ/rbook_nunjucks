#include <bits/stdc++.h>
using namespace std;

struct DLX {
    int cols;
    vector<int> left_link, right_link, up_link, down_link;
    vector<int> row_id, col_id, col_size, row_head;
    vector<int> answer;

    DLX(int max_nodes, int max_rows, int column_count)
        : cols(column_count),
          left_link(max_nodes + 1),
          right_link(max_nodes + 1),
          up_link(max_nodes + 1),
          down_link(max_nodes + 1),
          row_id(max_nodes + 1),
          col_id(max_nodes + 1),
          col_size(column_count + 1, 0),
          row_head(max_rows + 1, -1) {
        init_headers();
    }

    int node_count = 0;

    void init_headers() {
        node_count = cols + 1;
        for (int i = 0; i <= cols; ++i) {
            left_link[i] = i - 1;
            right_link[i] = i + 1;
            up_link[i] = down_link[i] = i;
        }
        left_link[0] = cols;
        right_link[cols] = 0;
    }

    void add_node(int r, int c) {
        int x = node_count++;
        row_id[x] = r;
        col_id[x] = c;
        ++col_size[c];

        up_link[x] = up_link[c];
        down_link[x] = c;
        down_link[up_link[c]] = x;
        up_link[c] = x;

        if (row_head[r] == -1) {
            row_head[r] = x;
            left_link[x] = right_link[x] = x;
        } else {
            int h = row_head[r];
            left_link[x] = left_link[h];
            right_link[x] = h;
            right_link[left_link[h]] = x;
            left_link[h] = x;
        }
    }

    void cover(int c) {
        right_link[left_link[c]] = right_link[c];
        left_link[right_link[c]] = left_link[c];

        for (int i = down_link[c]; i != c; i = down_link[i]) {
            for (int j = right_link[i]; j != i; j = right_link[j]) {
                down_link[up_link[j]] = down_link[j];
                up_link[down_link[j]] = up_link[j];
                --col_size[col_id[j]];
            }
        }
    }

    void uncover(int c) {
        for (int i = up_link[c]; i != c; i = up_link[i]) {
            for (int j = left_link[i]; j != i; j = left_link[j]) {
                ++col_size[col_id[j]];
                down_link[up_link[j]] = j;
                up_link[down_link[j]] = j;
            }
        }

        right_link[left_link[c]] = c;
        left_link[right_link[c]] = c;
    }

    bool solve() {
        if (right_link[0] == 0) return true;

        int c = right_link[0];
        for (int j = right_link[c]; j != 0; j = right_link[j]) {
            if (col_size[j] < col_size[c]) c = j;
        }
        if (col_size[c] == 0) return false;

        cover(c);
        for (int i = down_link[c]; i != c; i = down_link[i]) {
            answer.push_back(row_id[i]);
            for (int j = right_link[i]; j != i; j = right_link[j]) {
                cover(col_id[j]);
            }

            if (solve()) return true;

            for (int j = left_link[i]; j != i; j = left_link[j]) {
                uncover(col_id[j]);
            }
            answer.pop_back();
        }
        uncover(c);
        return false;
    }
};

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    int n, m;
    cin >> n >> m;

    int max_nodes = n * m + m + 5;
    DLX dlx(max_nodes, n, m);

    for (int i = 1; i <= n; ++i) {
        for (int j = 1; j <= m; ++j) {
            int x;
            cin >> x;
            if (x == 1) dlx.add_node(i, j);
        }
    }

    if (!dlx.solve()) {
        cout << "No Solution!\n";
        return 0;
    }

    for (int x : dlx.answer) cout << x << ' ';
    cout << '\n';
    return 0;
}
