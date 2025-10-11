---
title: "树的直径"
date: 2025-10-07 14:14
toc: true
tags: [""]
categories: [""]
---

[[TOC]]

## 树的直径定义

给定一棵带权(边)树

- **距离**: 两个点路径上的边权和   
- **树的直径**: 最大距离 

## 两次dfs法求直径

1. 从任意一点 $u$ 出发，找到距离 $u$ 最远的点 $v$
2. 从点 $v$ 出发，找到距离 $v$ 最远的点 $w$，则 $v$ 到 $w$ 的路径就是树的直径


!!! theorem 定理
从任意一点 $u$ 出发，找到距离 $u$ 最远的点 $v$，则 $v$ 一定是直径的某个端点
!!!

### 证明

**引理**: 设树中距离任意点 $u$ 最远的点为 $v$，则 $v$ 一定是直径的某个端点。

**证明**: 
- 假设树的直径为 $(x,y)$，即距离最大的两点
- 如果 $v$ 不是直径的端点，那么 $v$ 到 $x$ 的距离和 $v$ 到 $y$ 的距离都小于直径长度
- 根据 $v$ 的定义（距离 $u$ 最远），有 $dist(u,v) \geq dist(u,x)$ 和 $dist(u,v) \geq dist(u,y)$
- 由三角不等式：$dist(x,y) \leq dist(x,v) + dist(v,y)$
- 但 $dist(u,v) \geq dist(u,x)$ 且 $dist(u,v) \geq dist(u,y)$
- 这与 $(x,y)$ 是直径（最大距离）矛盾
- 因此 $v$ 必须是直径的某个端点

**算法正确性**:
- 第一次 DFS 从任意点 $u$ 找到最远点 $v$，根据引理，$v$ 是直径端点
- 第二次 DFS 从 $v$ 找到最远点 $w$，则 $(v,w)$ 就是直径

### 代码实现

```cpp
int max_dist, farthest_node;

void dfs_find_farthest(int u, int fa, int dist) {
    if (dist > max_dist) {
        max_dist = dist;
        farthest_node = u;
    }
    
    for (int i = e.h[u]; i != -1; i = e[i].next) {
        int v = e[i].v;
        if (v == fa) continue;
        dfs_find_farthest(v, u, dist + e[i].w);
    }
}

int tree_diameter() {
    // 第一次DFS：从任意点(如1)找到最远点
    max_dist = 0;
    dfs_find_farthest(1, 0, 0);
    int v = farthest_node;
    
    // 第二次DFS：从v找到最远点，距离即为直径
    max_dist = 0;
    dfs_find_farthest(v, 0, 0);
    return max_dist;
}
```

**时间复杂度**: $O(n)$，两次 DFS 遍历整棵树
**空间复杂度**: $O(n)$，递归栈空间:

## DP求树的直径

我们认为(假定)树的根为$1$, 

- $tree(i)$ 表示以点 $i$ 为根的子树.  
- $pathLca(i)$ 表示路径上的两个端点的lca为点 $i$ 的路径的集合   

$$
\text{所有路径的集合} = \bigcup_{i=1}^n pathLca(i) 
$$

根据公式

$$
A = B \cup C \to \max(A) = \max(\ \max(B) , \max(C)\ )
$$

那么答案就是

$$
max\_ans = \max_{i=1}^n ( \max( pathLca(i) ) )
$$

于是问题转换成求 $P(i) = \max ( pathLca(i) )$,易想到

- $P(i)$ 表示经过点 $i$ 的最长链,且 $i$ 是链的 $lca$
- $D(i)$ 表示点 $i$ 到叶结点的 最长链
- $x_i$ 表示点 $i$ 的孩子 
- $e(i,x_i)$ 表示边 $(i , x_i)$ 的长度 

$$
D(i) = \max \{ D(x_i) + e(i,x_i) \}
$$

$$
P(i) = \max\{ D(x_i)+ e(i,x_i) +  D(y_i)  + e(i,y_i) \} , x_i < y_i
$$

- 设$Dt(i,x_i) = D(x_i) + e(i,x_i)$

那么这个问题就变成的[集合最大二值和问题](../../math/集合.md)

![](./images/dx.excalidraw.svg)


于是我们得到代码如下: 


```cpp
int ans;

template<typename T>
void upd(T& v,T t) { if( v < t) v = t; } 

void dfs_dp(int u,int fa) {
    
    int pathlca = 0;
    for(int i = e.h[u];i != -1; i = e[i].next)
    {
        int v = e[i].v;
        if( v == fa) continue;
        dfs_dp(v, u);
        int dt = d[v] + e[i].w; //得到 u到达 v 子树上的最远点
        upd(pathlca,d[u] + dt); // 得到 分类 dt 的结尾的最值,更新 pathlca
        upd(d[u],dt); // 更新d[u]
    }
    upd(ans,pathlca);
}
```


## 树的所有直径拥有相同的中点

反证法:

假设有两条不相交的直径

因为树上任意两点都有一条唯一的路径

那么假设连接两个中点的长度为len, 直径长度为max_d

那么两条直径的两端距离为max_d + len > max_d

那么直径长度不是len
 
矛盾

所以两条直径必须经过一个点

**定理**: 树的所有直径都经过相同的中点（或中点所在的边）。

### 证明

**定义**:
- 路径的中点：将路径分成两个相等长度的点（或边）
- 如果直径长度为偶数，中点是一个确定的点
- 如果直径长度为奇数，中点是一条确定的边

**反证法证明**:

假设存在两条直径 $(x_1,y_1)$ 和 $(x_2,y_2)$，它们的中点不同。

设：
- $M_1$ 为直径 $(x_1,y_1)$ 的中点
- $M_2$ 为直径 $(x_2,y_2)$ 的中点
- 直径长度为 $L$

**情况1：中点都是点（直径长度为偶数）**

由于 $M_1 \neq M_2$，考虑路径 $M_1 \to M_2$。在树中，两点间只有唯一路径。

考虑以下四个距离：
- $dist(M_1, x_1) = dist(M_1, y_1) = L/2$ （中点定义）
- $dist(M_2, x_2) = dist(M_2, y_2) = L/2$ （中点定义）

由于 $M_1 \neq M_2$，不失一般性，设 $M_2$ 在 $M_1$ 到 $x_1$ 的路径上。

则：
- $dist(M_2, x_1) < L/2$
- $dist(M_2, y_1) > L/2$

但 $dist(M_2, x_2) = L/2$，且 $(x_2,y_2)$ 是直径，所以：
$$L = dist(x_2, y_2) \leq dist(x_2, M_2) + dist(M_2, y_2) = L/2 + dist(M_2, y_2)$$

这意味着 $dist(M_2, y_2) \geq L/2$，结合 $dist(M_2, y_1) > L/2$，可以得到：
$$dist(x_2, y_1) = dist(x_2, M_2) + dist(M_2, y_1) > L/2 + L/2 = L$$

这与 $L$ 是最大距离（直径长度）矛盾！

**情况2：中点都是边（直径长度为奇数）**

设 $M_1 = (a_1,b_1)$，$M_2 = (a_2,b_2)$ 是两条边。

类似的论证可以证明，如果中点边不同，也能构造出比直径更长的路径，产生矛盾。

**结论**: 因此，树的所有直径必须有相同的中点。

### 推论与应用

1. **树的中心**: 所有直径的中点（或中点边）的集合称为树的中心
2. **中心性质**: 
   - 如果直径长度为偶数，中心是单个点
   - 如果直径长度为奇数，中心是两个相邻的点（一条边）
3. **应用**: 这个性质在很多树算法中有重要应用，如：
   - 找树的最小高度根节点
   - 网络布局优化
   - 树的重心相关问题

### 代码验证

```cpp
// 验证所有直径是否经过相同中点
vector<int> diameter_path;  // 存储直径路径
vector<int> center_points;  // 存储中心点

void find_diameter_path(int u, int fa, int target, vector<int>& path) {
    path.push_back(u);
    if (u == target) return;
    
    for (int i = e.h[u]; i != -1; i = e[i].next) {
        int v = e[i].v;
        if (v == fa) continue;
        find_diameter_path(v, u, target, path);
        if (path.back() == target) return;
    }
    path.pop_back();
}

void find_center() {
    int v = find_farthest(1, 0);
    int w = find_farthest(v, 0);
    
    vector<int> path;
    find_diameter_path(v, 0, w, path);
    
    int diameter_length = path.size() - 1;
    if (diameter_length % 2 == 0) {
        // 中心是单个点
        center_points.push_back(path[diameter_length / 2]);
    } else {
        // 中心是两个点
        center_points.push_back(path[diameter_length / 2]);
        center_points.push_back(path[diameter_length / 2 + 1]);
    }
}
```


## 树的直径的重合部分

**重要性质**: 树的所有直径有且只有一段重合的部分，这段重合可能是：
- 一个点（当直径长度为偶数时）
- 一条边（当直径长度为奇数时）

### 证明

基于前面证明的"所有直径拥有相同的中点"性质：

1. **所有直径都经过相同的中点区域**
   - 如果直径长度为偶数，所有直径都经过同一个中心点
   - 如果直径长度为奇数，所有直径都经过同一条中心边

2. **重合部分的唯一性**
   - 设树的中心区域为 $C$（点或边）
   - 任意直径都必须完整包含 $C$
   - 在 $C$ 之外，不同直径可能分叉到不同的端点

3. **重合部分的最大性**
   - 重合部分不能更长，否则会有直径不包含某个端点
   - 重合部分不能更短，否则会有直径不经过中心

### 直观理解

想象一棵树的形状：

```
        A       B       C
         \     / \     /
          \   /   \   /
            D       E
           / \     / \
          F   G   H   I
         / \     / \ / \
        J   K   L  M N  O
```

- 如果直径长度为偶数，所有直径都经过中心点 D
- 如果直径长度为奇数，所有直径都经过中心边 (D,E)

### 应用：NOI2013 快餐店

这是一个经典的应用题，需要在树上设置一个快餐店，使得所有点到快餐店的最大距离最小。

**解题思路**：
1. 找到所有直径的公共重合部分
2. 将快餐店设在重合部分的中点
3. 这样可以最大化地减少最远距离

**代码实现**：

```cpp
// 找到所有直径的重合部分
vector<int> find_common_path() {
    // 找到一条直径的端点
    int v = find_farthest(1, 0);
    int w = find_farthest(v, 0);
    
    // 找到所有直径端点
    vector<int> diameter_ends = find_all_diameter_ends(v, w);
    
    // 找到重合部分的起点和终点
    int common_start = n, common_end = 1;
    
    for (int end : diameter_ends) {
        vector<int> path1, path2;
        find_path(v, end, path1);
        find_path(w, end, path2);
        
        // 找到两条路径的公共部分
        int lca_node = lca(v, end);
        // 更新重合部分的边界
        update_common_boundary(path1, path2, common_start, common_end);
    }
    
    return get_common_path(common_start, common_end);
}

// 快店店问题的核心代码
double solve_restaurant() {
    vector<int> common_path = find_common_path();
    
    if (common_path.size() == 1) {
        // 重合部分是一个点
        return max_distance_from_point(common_path[0]);
    } else {
        // 重合部分是一条路径
        return max_distance_from_path(common_path);
    }
}
```

### 相关题目

1. **[NOI2013 快餐店]**: 在树上找一点，使得到所有点的最大距离最小
2. **[CF 1059E] Split the Tree**: 利用直径重合性质进行树的分割
3. **[POJ 1849] Two**: 两个人从树的任意两点出发，走完所有边

### 总结

这个性质告诉我们：
- 树的直径虽然可能有很多条，但它们有很强的规律性
- 所有直径都会在树的"中心区域"重合
- 这个性质是解决很多树问题的基础，特别是涉及最优化的问题
- 在实际应用中，可以利用这个性质来简化问题的复杂度


## 直径必过边

对应题目: [luogu P3304 SDOI2013 直径](https://www.luogu.com.cn/problem/P3304)

### 两次 dfs 法

### DP 法


## 参考

- 树的直径 题目单 https://www.luogu.com.cn/training/505787#problems

NOI2013有个叫快餐店