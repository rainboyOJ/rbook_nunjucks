这是一个非常严谨的数学请求。我们将使用 **反证法** 和 **不等式推导** 的方式，像做几何证明题一样，一步步证明这个结论。

## 命题

在 DFS-SPFA 算法执行过程中，如果当前节点 $u$ 试图松弛一个已经在递归栈（`instack`）中的节点 $v$，则图中必然存在一个负环。

------

## 证明过程

### 1. 定义与假设

- 设 $dist(i)$ 为源点到节点 $i$ 当前计算出的最短距离。

- 设 $w(i, j)$ 为边 $(i, j)$ 的权值。

- 松弛条件：DFS 会继续沿着边 $(u, v)$ 走的唯一条件是满足松弛不等式：

  

  $$dist(v) > dist(u) + w(u, v) \quad \text{......(不等式 A)}$$

### 2. 场景重现

假设我们在 DFS 递归过程中，当前的递归栈（instack）状态如下：



$$v \to x_1 \to x_2 \to \dots \to u$$



这意味着：

1. $v$ 是 $u$ 的祖先。
2. 存在一条从 $v$ 到 $u$ 的路径：Path $= \{ (v, x_1), (x_1, x_2), \dots, (x_k, u) \}$。

此时，算法正在处理节点 $u$，并且发现可以通过边 $(u, v)$ 回到 $v$，且满足松弛条件。

### 3. 建立不等式组

我们回溯一下 递归栈中 每一个节点被加入的时刻。

既然 $x_1$ 是由 $v$ 递归进去的，说明在那个时刻，边 $(v, x_1)$ 成功进行了松弛（或者赋值）。

这意味着当前时刻的 $dist$ 值满足以下关系（注意：由于是 DFS 一条路走下来的，后继节点的 dist 是由前驱节点推导出来的）：

$$\begin{cases} dist(x_1) \le dist(v) + w(v, x_1) \\ dist(x_2) \le dist(x_1) + w(x_1, x_2) \\ \vdots \\ dist(u) \le dist(x_k) + w(x_k, u) \end{cases}$$

*(注：通常在 DFS 中直接赋值是等号，但考虑到可能有其他路径更新，写成 $\le$ 是最严谨的三角形不等式性质)*

此时，发生了一次关键的“回边”松弛，即试图从 $u$ 走到 $v$：



$$dist(v) > dist(u) + w(u, v) \quad \text{......(关键不等式)}$$

### 4. 不等式累加（消元法）

我们将上述所有关系列在一起：

$$\begin{aligned} dist(x_1) &\le dist(v) + w(v, x_1) \\ dist(x_2) &\le dist(x_1) + w(x_1, x_2) \\ \dots & \dots \\ dist(u) &\le dist(x_k) + w(x_k, u) \\ dist(v) &> dist(u) + w(u, v)  \quad \text{(注意这里是严格大于)} \end{aligned}$$

现在，我们将这些式子的 **左边加左边，右边加右边**：

$$\begin{aligned} dist(x_1) + dist(x_2) + \dots + dist(u) + \mathbf{dist(v)} \quad &> \quad \mathbf{dist(v)} + dist(x_1) + \dots + dist(x_k) + dist(u) + \sum w_{\text{all}} \end{aligned}$$

### 5. 化简

你会发现，不等式两边都有相同的 $dist$ 项（$dist(v), dist(x_1) \dots dist(u)$）。

我们将两边相同的项全部消去：

$$0 > \sum w_{\text{all}}$$

其中 $\sum w_{\text{all}}$ 正是环 $v \to x_1 \to \dots \to u \to v$ 上所有边的权值之和。

### 6. 结论

$$\sum w_{\text{all}} < 0$$

即：该回路的总权值严格小于 0。

证毕。

------

## 💡 为什么正环不会触发？

为了加深理解，我们用同样的逻辑看看**正环**会发生什么。

假设环的权值是正的（比如总和是 10）。

当你绕了一圈回到 $v$ 时，你计算出的新距离大约是：



$$NewDist = dist(v)_{\text{old}} + 10$$

此时，算法会检查松弛条件：



$$dist(v)_{\text{old}} > NewDist \quad ?$$

即：

$$dist(v)_{\text{old}} > dist(v)_{\text{old}} + 10 \quad ?$$

显然 $0 > 10$ 是不成立的。

所以，如果是正环，当你撞到祖先 $v$ 时，松弛条件 if (dist[v] > dist[u] + w) 根本不会成立。DFS 会在这里止步，回溯，而不会误报“发现环”。

只有当环是负的（比如 -5），不等式 $0 > -5$ 成立，DFS 才会闯进去，进而触发 `instack` 的报警。