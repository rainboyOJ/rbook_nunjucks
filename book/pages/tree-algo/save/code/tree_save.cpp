// 完整实现：从无向边输入建一棵有根树，输出每个点的深度和子树大小
// 输入：
//   第一行 n root
//   接下来 n-1 行 u v（无向边）
// 输出：
//   第一行 depth[1..n]，第二行 size[1..n]
//
// 建的是父亲孩子表示法：fa[] 记父亲，ch[] 记孩子列表。
//   自顶向下沿 ch[] 求深度
//   自底向上沿 fa[] 累加子树大小
#include <cstdio>
#include <vector>
using namespace std;

const int maxn = 1e5 + 5;

int n, root;
vector<int> g[maxn];   // 第一步：树是特殊的图，先用邻接表存 n-1 条无向边
vector<int> ch[maxn];  // 孩子列表 ch[]
int fa[maxn];          // 父亲 fa[]，两者合起来是父亲孩子表示法
int depth[maxn];       // 深度：到根的路径上的边数
int sz[maxn];          // 子树大小：以 u 为根的子树里的点数

// 从 root 出发 DFS（用栈模拟），确定每个点的父亲和孩子，同时得到遍历顺序
void build() {
    vector<int> order;  // DFS 顺序：父亲一定排在孩子前面
    vector<int> stk = {root};
    fa[root] = 0;  // 根没有父亲
    depth[root] = 0;
    while (!stk.empty()) {
        int u = stk.back();
        stk.pop_back();
        order.push_back(u);
        for (int v : g[u]) {
            if (v == fa[u]) continue;  // 树上 u 只有父亲这一条"回头边"
            fa[v] = u;                 // 第一次走到 v 的点就是 v 的父亲
            ch[u].push_back(v);
            depth[v] = depth[u] + 1;
            stk.push_back(v);
        }
    }

    // 自底向上（沿 fa[]）：逆序遍历，把子树大小累加到父亲上
    for (int i = 1; i <= n; i++) sz[i] = 1;  // 先算上自己
    for (int i = n - 1; i >= 0; i--) {
        int u = order[i];
        if (u != root) sz[fa[u]] += sz[u];
    }
}

int main() {
    scanf("%d %d", &n, &root);
    for (int i = 1; i < n; i++) {
        int u, v;
        scanf("%d %d", &u, &v);
        g[u].push_back(v);
        g[v].push_back(u);
    }

    build();

    printf("depth:");
    for (int i = 1; i <= n; i++) printf(" %d", depth[i]);
    printf("\nsize:");
    for (int i = 1; i <= n; i++) printf(" %d", sz[i]);
    printf("\n");
    return 0;
}
