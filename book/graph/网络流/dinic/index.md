---
title: 最大流算法:Dinic
date: 2019-01-27 17:45
update: 2019-01-27 17:45
author: Rainboy
cover: https://ww1.sinaimg.cn/large/007i4MEmgy1fzla9krgiug30qg0bk0wk.gif
---

## 连续最短增广路算法：Dinic

Dinic 算法由 Yefim Dinitz 在 1970 年提出。它是一种**最短增广路算法**，通过引入“分层图”概念，解决了 Edmonds-Karp 算法在寻找增广路时可能存在的盲目性，大幅提升了效率。

### 核心概念：分层图与允许弧

为了避免在残余网络中漫无目的地寻找增广路，Dinic 算法引入了**分层图 (Level Graph)** 的概念。

1.  **顶点的层次 ($level$)**：在残余网络中，从源点 $s$ 到顶点 $u$ 的**最短路径长度**（边数），称为 $u$ 的层次，记为 $level(u)$。源点 $s$ 的层次规定为 $0$。
2.  **分层图**：由残余网络中所有满足 $level(v) = level(u) + 1$ 的边 $(u, v)$ 构成的子图。
3.  **允许弧 (Permissible Arc)**：满足 $level(v) = level(u) + 1$ 且残余容量 $cap > 0$ 的边。

**为什么要分层？**
在分层图中，任意一条从 $s$ 到 $t$ 的路径都对应原残余网络中一条从 $s$ 到 $t$ 的**最短路径**。通过在分层图上进行 DFS，我们可以一次性找到多条最短增广路，而不会走“回头路”或“冤枉路”。

## Dinic 算法流程

Dinic 算法的核心循环如下：

1.  **BFS 分层**：在残余网络上进行 BFS，计算每个点的层次 $level(u)$。如果汇点 $t$ 的层次无法计算（即 $s$ 无法到达 $t$），则算法结束。
2.  **DFS 增广**：在分层图上进行 DFS，寻找从 $s$ 到 $t$ 的增广路。利用**多路增广**和**当前弧优化**，一次性找出当前分层图下的所有增广流量，并更新残余网络。
3.  **累加流量**：将 DFS 找到的所有增广流量累加到最大流中，返回步骤 1。

```viz-dot
digraph G {
    rankdir=TB;
    node[shape=box, style="rounded,filled", fillcolor="#f9f9f9"];
    
    start [label="开始", shape=ellipse, fillcolor="#e1f5fe"];
    init  [label="初始化最大流 = 0
构造初始残余网络"];
    bfs   [label="BFS 构造分层图"];
    check [label="汇点 t 是否可达？", shape=diamond, fillcolor="#fff9c4"];
    dfs   [label="DFS 多路增广
(含当前弧优化)"];
    add   [label="累加流量到最大流"];
    end   [label="结束", shape=ellipse, fillcolor="#e1f5fe"];

    start -> init;
    init  -> bfs;
    bfs   -> check;
    check -> dfs [label="是"];
    check -> end [label="否"];
    dfs   -> add;
    add   -> bfs [style=dashed, label=" 重新分层"];
}
```

**为什么要多次建立分层图？**

建立分层图可以保证当前的增广路是最短的。但在进行一轮 DFS 增广后，残余网络发生了变化（某些边被填满，某些反向边增加了容量），原有的层次关系可能不再适用。

特别是**反向边**的引入，可能会产生更短的路径，或者使得原本不可达的点变得可达。因此，我们需要重新进行 BFS 分层，直到在残余网络中源点无法到达汇点为止。


```viz-dot
digraph G {
    rankdir= LR;
    edge[minlen=2];
    node[shape="circle"];
    { rank=same; 1,3; }
    { rank=same; 2,4; }
    { rank=same; 5,6; } 

    1[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>0</TD></TR></TABLE>>]

    2[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>1</TD></TR></TABLE>>]
    3[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>1</TD></TR></TABLE>>]

    4[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>2</TD></TR></TABLE>>]
    5[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>2</TD></TR></TABLE>>]
    6[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>3</TD></TR></TABLE>>]

    edge[label="2"]
    1->2->4->6;
    edge[label="1"]
    1->3->4;
    2->5->6;
}
```

如下图，你会发现：在找到一条增广路径 $1 	o 2 	o 4 	o 6$ 后，如果还用原来的**分层图**，就找不到下一条增广路（例如 $1 	o 3 	o 4 	o 2 	o 5 	o 6$），因为反向边 $4 	o 2$ 不满足 $level(2) = level(4) + 1$。

```viz-dot
digraph G {
    rankdir= LR;
    edge[minlen=2];
    node[shape="circle"];
    { rank=same; 1,3; }
    { rank=same; 2,4; }

    { rank=same; 5,6; } 


    1[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>0</TD></TR></TABLE>>]

    2[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>1</TD></TR></TABLE>>]
    3[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>1</TD></TR></TABLE>>]

    4[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>2</TD></TR></TABLE>>]
    5[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>2</TD></TR></TABLE>>]
    6[xlabel=< <TABLE BORDER="0" CELLBORDER="1"><TR><TD>3</TD></TR></TABLE>>]

    edge[weight=3];
    2->5[label="1"];
    3->4[label="1"];

    1->3[label="1"];
    5->6[label="1"];


    edge[weight=1];
    1->2[label="0",style="dashed"];
    2->1[label="2"];

    2->4[label="0",style="dashed"];
    4->2[label="2",constraint=false];

    4->6[label="0",style="dashed"];
    6->4[label="2"];
}
```

## 存储残余网络：成对存储技巧

在网络流算法中，更新流量时必须同时修改正向边和反向边的残余容量。为了在 $O(1)$ 时间内找到某条边对应的反向边，我们通常使用**链式前向星**并配合**成对存储**技巧。

### 核心原理

如果我们将边的计数器 `cnt` 从 `0` 开始（或从偶数开始），并且每次加边时紧接着加入其反向边，那么：
- 边 `0` 和 `1` 互为反向边
- 边 `2` 和 `3` 互为反向边
- ...
- 边 `i` 和 `i ^ 1` 互为反向边

**位运算解释：**
- `0 ^ 1 = 1`
- `1 ^ 1 = 0`
- `2 ^ 1 = 3`
- `3 ^ 1 = 2`

### 代码实现

```cpp
struct Edge {
    int to, next, cap;
} e[MAXE];

int head[MAXN], cnt = 0;

void addEdge(int u, int v, int cap) {
    // 正向边
    e[cnt] = {v, head[u], cap};
    head[u] = cnt++;
    // 反向边 (初始容量通常为 0，若是有向图)
    e[cnt] = {u, head[v], 0};
    head[v] = cnt++;
}
```

> **注意**：如果你的 `cnt` 从 `1` 开始，那么成对存储的规律会失效（`1^1=0`, `2^1=3`），此时需要从 `2` 开始或者使用其他逻辑。建议始终从 `0` 开始。

## 算法过程模拟

下面通过一个具体的例子来演示 Dinic 算法的执行过程。

### 1. 初始容量网络

每条边上的值代表该边的最大容量 $cap$。我们的目标是求从源点 $S$ 到汇点 $T$ 的最大流。

```viz-neato
digraph G {
    node[shape=circle];
    S[pos="-1,1!"];
    v1[pos="1,2!"];
    v2[pos="1,0!"];
    v3[pos="3,2!"];
    v4[pos="3,0!"];
    T[pos="5,1!"];


    edge[arrowhead=open]
    S->v1[label="12"];
    v1->v3[label="8"];
    v3->T[label="18"];


    v2->v1[label=2];

    v4->v3[label=6];
    v3->v2[label=5];

    S->v2[label=10];
    v2->v4[label=13];
    v4->T[label=4];
}
```

### 2. 初始化残余网络

根据初始容量网络建立残余网络。初始时，正向边的残余容量等于其原始容量，反向边的残余容量为 $0$（图中用虚线表示）。



```viz-neato
digraph G {
    node[shape=circle];
    S[pos="-1,1!"];
    v1[pos="1,2!"];
    v2[pos="1,0!"];
    v3[pos="3,2!"];
    v4[pos="3,0!"];
    T[pos="5,1!"];


    edge[arrowhead=open]
    S->v1[label="12"];
    v1->v3[label="8"];
    v3->T[label="18"];

    v2->v1[label=2];

    v4->v3[label=6];
    v3->v2[headlabel=5,labelangle=-5,labeldistance=8];

    S->v2[label=10];
    v2->v4[label=13];
    v4->T[label=4];


    edge[style=dashed,labeldistance=5]
    v1->S[headlabel=0,labelangle=-10];
    v3->v1[headlabel=0,labelangle=-10];
    T->v3[headlabel=0,labelangle=-10];

    v1->v2[headlabel=0,labelangle=-10];

    v3->v4[headlabel=0,labelangle=-10];
    v2->v3[headlabel=0,labelangle=-5,labeldistance=8];

    v2->S[headlabel=0,labelangle=-10];
    v4->v2[headlabel=0,labelangle=-10];
    T->v4[headlabel=0,labelangle=-10];
}
```


### 3. 第一轮 BFS 分层

从源点 $S$ 开始进行 BFS。$S$ 的层次为 $0$，其邻接点 $v1, v2$ 的层次为 $1$，依此类推。图中蓝色数字表示各点的层次 $level$。


```viz-neato
digraph G {
    node[shape=circle];
    S[pos="-1,1!"];
    v1[pos="1,2!"];
    v2[pos="1,0!"];
    v3[pos="3,2!"];
    v4[pos="3,0!"];
    T[pos="5,1!"];


    edge[arrowhead=open]
    S->v1[label="12"];
    v1->v3[label="8"];
    v3->T[label="18"];

    v2->v1[label=2];

    v4->v3[label=6];
    v3->v2[headlabel=5,labelangle=-5,labeldistance=8];

    S->v2[label=10];
    v2->v4[label=13];
    v4->T[label=4];


    edge[style=dashed,labeldistance=5]
    v1->S[headlabel=0,labelangle=-10];
    v3->v1[headlabel=0,labelangle=-10];
    T->v3[headlabel=0,labelangle=-10];

    v1->v2[headlabel=0,labelangle=-10];

    v3->v4[headlabel=0,labelangle=-10];
    v2->v3[headlabel=0,labelangle=-5,labeldistance=8];

    v2->S[headlabel=0,labelangle=-10];
    v4->v2[headlabel=0,labelangle=-10];
    T->v4[headlabel=0,labelangle=-10];
    
    
    
    S[xlabel=<<font color="blue">[0]</font>>];
    v1[xlabel=<<font color="blue">[1]</font>>];
    v2[xlabel=<<font color="blue">[1]</font>>];
    
    v3[xlabel=<<font color="blue">[2]</font>>];
    v4[xlabel=<<font color="blue">[2]</font>>];

    T[xlabel=<<font color="blue">[3]</font>>];

}
```

### 4. 第一轮 DFS 增广

在分层图中寻找增广路。注意：DFS 只能沿着 $level(v) = level(u) + 1$ 的边（允许弧）进行。

**第一条路径**：$S 	o v1 	o v3 	o T$，该路径上的最小残余容量为 $8$。

```viz-neato
digraph G {
    node[shape=circle];
    S[pos="-1,1!"];
    v1[pos="1,2!"];
    v2[pos="1,0!"];
    v3[pos="3,2!"];
    v4[pos="3,0!"];
    T[pos="5,1!"];


    edge[arrowhead=open]
    S->v1[label="12",color="limegreen",penwidth=3];
    v1->v3[label="8",color="limegreen",penwidth=3];
    v3->T[label="18",color="limegreen",penwidth=3];

    v2->v1[label=2];

    v4->v3[label=6];
    v3->v2[headlabel=5,labelangle=-5,labeldistance=8];

    S->v2[label=10];
    v2->v4[label=13];
    v4->T[label=4];


    edge[style=dashed,labeldistance=5]
    v1->S[headlabel=0,labelangle=-10];
    v3->v1[headlabel=0,labelangle=-10];
    T->v3[headlabel=0,labelangle=-10];

    v1->v2[headlabel=0,labelangle=-10];

    v3->v4[headlabel=0,labelangle=-10];
    v2->v3[headlabel=0,labelangle=-5,labeldistance=8];

    v2->S[headlabel=0,labelangle=-10];
    v4->v2[headlabel=0,labelangle=-10];
    T->v4[headlabel=0,labelangle=-10];
    
    
    
    S[xlabel=<<font color="blue">[0]</font>>];
    v1[xlabel=<<font color="blue">[1]</font>>];
    v2[xlabel=<<font color="blue">[1]</font>>];
    
    v3[xlabel=<<font color="blue">[2]</font>>];
    v4[xlabel=<<font color="blue">[2]</font>>];

    T[xlabel=<<font color="blue">[3]</font>>];

}
```

**更新残余网络**：沿途正向边容量减 $8$，反向边容量加 $8$。

```viz-neato
digraph G {
    node[shape=circle];
    S[pos="-1,1!"];
    v1[pos="1,2!"];
    v2[pos="1,0!"];
    v3[pos="3,2!"];
    v4[pos="3,0!"];
    T[pos="5,1!"];


    edge[arrowhead=open]
    S->v1[label="4"];
    v1->v3[label="0",color="grey80",penwidth=0.5];
    v3->T[label="10"];

    v2->v1[label=2];

    v4->v3[label=6];
    v3->v2[headlabel=5,labelangle=-5,labeldistance=8];

    S->v2[label=10];
    v2->v4[label=13];
    v4->T[label=4];


    edge[style=dashed,labeldistance=5]
    v1->S[headlabel=8,labelangle=-10,color="orange",penwidth=2];
    v3->v1[headlabel=8,labelangle=-10,color="orange",penwidth=2];
    T->v3[headlabel=8,labelangle=-10,color="orange",penwidth=2];

    v1->v2[headlabel=0,labelangle=-10];

    v3->v4[headlabel=0,labelangle=-10];
    v2->v3[headlabel=0,labelangle=-5,labeldistance=8];

    v2->S[headlabel=0,labelangle=-10];
    v4->v2[headlabel=0,labelangle=-10];
    T->v4[headlabel=0,labelangle=-10];
    
    
    
    S[xlabel=<<font color="blue">[0]</font>>];
    v1[xlabel=<<font color="blue">[1]</font>>];
    v2[xlabel=<<font color="blue">[1]</font>>];
    
    v3[xlabel=<<font color="blue">[2]</font>>];
    v4[xlabel=<<font color="blue">[2]</font>>];

    T[xlabel=<<font color="blue">[3]</font>>];

}
```

- 此时 $v1 	o v3$ 的残余容量变为 $0$，在该轮分层图中已无法继续增广。
- DFS 回溯到 $S$ 后，尝试其他分支。发现另一条路径：$S 	o v2 	o v4 	o T$，最小容量为 $4$。

**第一轮 DFS 全部结束后的残余网络：**

```viz-neato
digraph G {
    node[shape=circle];
    S[pos="-1,1!"];
    v1[pos="1,2!"];
    v2[pos="1,0!"];
    v3[pos="3,2!"];
    v4[pos="3,0!"];
    T[pos="5,1!"];


    edge[arrowhead=open]
    S->v1[label="4"];
    v1->v3[label="0",color="grey80",penwidth=0.5];
    v3->T[label="10"];

    v2->v1[label=2];

    v4->v3[label=6];
    v3->v2[headlabel=5,labelangle=-5,labeldistance=8];

    S->v2[label=6];
    v2->v4[label=9];
    v4->T[label="0",color="grey80",penwidth=0.5];


    edge[style=dashed,labeldistance=5]
    v1->S[headlabel=8,labelangle=-10,];
    v3->v1[headlabel=8,labelangle=-10,];
    T->v3[headlabel=8,labelangle=-10,];

    v1->v2[headlabel=0,labelangle=-10];

    v3->v4[headlabel=0,labelangle=-10];
    v2->v3[headlabel=0,labelangle=-5,labeldistance=8];

    v2->S[headlabel=4,labelangle=-10];
    v4->v2[headlabel=4,labelangle=-10];
    T->v4[headlabel=4,labelangle=-10];
    
    
    
    S[xlabel=<<font color="blue">[0]</font>>];
    v1[xlabel=<<font color="blue">[1]</font>>];
    v2[xlabel=<<font color="blue">[1]</font>>];
    
    v3[xlabel=<<font color="blue">[2]</font>>];
    v4[xlabel=<<font color="blue">[2]</font>>];

    T[xlabel=<<font color="blue">[3]</font>>];

}
```

### 5. 第二轮 BFS 分层与 DFS 增广

由于第一轮增广后，汇点 $T$ 依然可达，我们需要重新计算层次。


```viz-neato
digraph G {
    node[shape=circle];
    S[pos="-1,1!"];
    v1[pos="1,2!"];
    v2[pos="1,0!"];
    v3[pos="3,2!"];
    v4[pos="3,0!"];
    T[pos="5,1!"];


    edge[arrowhead=open]
    S->v1[label="4"];
    v1->v3[label="0",color="grey80",penwidth=0.5];
    v3->T[label="10"];

    v2->v1[label=2];

    v4->v3[label=6];
    v3->v2[headlabel=5,labelangle=-5,labeldistance=8];

    S->v2[label=6];
    v2->v4[label=9];
    v4->T[label="0",color="grey80",penwidth=0.5];


    edge[style=dashed,labeldistance=5]
    v1->S[headlabel=8,labelangle=-10,];
    v3->v1[headlabel=8,labelangle=-10,];
    T->v3[headlabel=8,labelangle=-10,];

    v1->v2[headlabel=0,labelangle=-10];

    v3->v4[headlabel=0,labelangle=-10];
    v2->v3[headlabel=0,labelangle=-5,labeldistance=8];

    v2->S[headlabel=4,labelangle=-10];
    v4->v2[headlabel=4,labelangle=-10];
    T->v4[headlabel=4,labelangle=-10];
    
    
    
    S[xlabel=<<font color="blue">[0]</font>>];
    v1[xlabel=<<font color="blue">[1]</font>>];
    v2[xlabel=<<font color="blue">[1]</font>>];
    
    v3[xlabel=<<font color="blue">[2]</font>>];
    v4[xlabel=<<font color="blue">[2]</font>>];

    T[xlabel=<<font color="blue">[3]</font>>];

}
```

在新的分层图上再次进行 DFS。发现路径 $S 	o v2 	o v4 	o v3 	o T$，最小容量为 $6$。


```viz-neato
digraph G {
    node[shape=circle];
    S[pos="-1,1!"];
    v1[pos="1,2!"];
    v2[pos="1,0!"];
    v3[pos="3,2!"];
    v4[pos="3,0!"];
    T[pos="5,1!"];


    edge[arrowhead=open]
    S->v1[label="4"];
    v1->v3[label="0",color="grey80",penwidth=0.5];
    v3->T[label="10",color="limegreen",penwidth=2];

    v2->v1[label=2];

    v4->v3[label=6,color="limegreen",penwidth=2];
    v3->v2[headlabel=5,labelangle=-5,labeldistance=8];

    S->v2[label=6,color="limegreen",penwidth=2];
    v2->v4[label=9,color="limegreen",penwidth=2];
    v4->T[label="0",color="grey80",penwidth=0.5];


    edge[style=dashed,labeldistance=5]
    v1->S[headlabel=8,labelangle=-10,];
    v3->v1[headlabel=8,labelangle=-10,];
    T->v3[headlabel=8,labelangle=-10,];

    v1->v2[headlabel=0,labelangle=-10];

    v3->v4[headlabel=0,labelangle=-10];
    v2->v3[headlabel=0,labelangle=-5,labeldistance=8];

    v2->S[headlabel=4,labelangle=-10];
    v4->v2[headlabel=4,labelangle=-10];
    T->v4[headlabel=4,labelangle=-10];
    
    
    
    S[xlabel=<<font color="blue">[0]</font>>];
    v1[xlabel=<<font color="blue">[1]</font>>];
    v2[xlabel=<<font color="blue">[1]</font>>];
    
    v3[xlabel=<<font color="blue">[2]</font>>];
    v4[xlabel=<<font color="blue">[2]</font>>];

    T[xlabel=<<font color="blue">[3]</font>>];

}
```

**更新后的残余网络：**


```viz-neato
digraph G {
    node[shape=circle];
    S[pos="-1,1!"];
    v1[pos="1,2!"];
    v2[pos="1,0!"];
    v3[pos="3,2!"];
    v4[pos="3,0!"];
    T[pos="5,1!"];


    edge[arrowhead=open]
    S->v1[label="4"];
    v1->v3[label="0",color="grey80",penwidth=0.5];
    v3->T[label="4"];

    v2->v1[label=2];

    v4->v3[label=0,color="grey80",penwidth=0.5];
    v3->v2[headlabel=5,labelangle=-5,labeldistance=8];

    S->v2[label=0,color="grey80",penwidth=0.5];
    v2->v4[label=3];
    v4->T[label="0",color="grey80",penwidth=0.5];


    edge[style=dashed,labeldistance=5]
    v1->S[headlabel=8,labelangle=-10,];
    v3->v1[headlabel=8,labelangle=-10,];
    T->v3[headlabel=14,labelangle=-10,];

    v1->v2[headlabel=0,labelangle=-10];

    v3->v4[headlabel=6,labelangle=-10];
    v2->v3[headlabel=0,labelangle=-5,labeldistance=8];

    v2->S[headlabel=10,labelangle=-10];
    v4->v2[headlabel=10,labelangle=-10];
    T->v4[headlabel=4,labelangle=-10];
    
    
    
    S[xlabel=<<font color="blue">[0]</font>>];
    v1[xlabel=<<font color="blue">[1]</font>>];
    v2[xlabel=<<font color="blue">[1]</font>>];
    
    v3[xlabel=<<font color="blue">[2]</font>>];
    v4[xlabel=<<font color="blue">[2]</font>>];

    T[xlabel=<<font color="blue">[3]</font>>];

}
```

### 6. 算法结束

再次尝试 BFS 分层，发现从 $S$ 无法到达 $T$（所有通往 $T$ 的路径在残余网络中容量均为 $0$）。算法结束，总最大流量为 $8 + 4 + 6 = 18$。

**最终的流量分配情况：**
```viz-neato
digraph G {
    node[shape=circle];
    S[pos="-1,1!"];
    v1[pos="1,2!"];
    v2[pos="1,0!"];
    v3[pos="3,2!"];
    v4[pos="3,0!"];
    T[pos="5,1!"];


    edge[arrowhead=open]
    S->v1[label="8"];
    v1->v3[label="8"];
    v3->T[label="14"];

    v2->v1[label=0];

    v4->v3[label=6];
    v3->v2[headlabel=0,labelangle=-5,labeldistance=8];

    S->v2[label=10];
    v2->v4[label=10];
    v4->T[label="4"];

}
```

## 练习

### 练习 1：手动模拟

找一张纸，使用下面的图，用笔模拟一遍 Dinic 算法。

```viz-neato
digraph G {
    node[shape=circle];
    1[label=S];
    7[label=T];

    1[pos="0,1!"];
    2[pos="1,2!"];
    3[pos="3,2!"];
    4[pos="1,0!"];
    5[pos="3,0!"];
    6[pos="2,1!"];
    7[pos="4,1!"];

    1->2[label="4"];
    2->3[label="7"];
    3->7[label="9"];
    1->4[label="10"];
    4->5[label="3"];
    5->7[label="6"];
    2->6[label="2"];
    6->5[label="1"];
    4->6[label="5"];
    6->3[label="6"];
    2->4[label="1"];
    5->3[label="5"];

}
```

**参考分层结果：**

```viz-neato
digraph G {
    node[shape=circle];


    1[pos="0,1!"];
    2[pos="1,2!"];
    3[pos="3,2!"];
    4[pos="1,0!"];
    5[pos="3,0!"];
    6[pos="2,1!"];
    7[pos="4,1!"];

    {

        node[shape="none",fontcolor="blue"];
        x1[label="[0]",pos="-0.5,1!"];
        x2[label="[1]",pos="1,2.5!"];
        x3[label="[2]",pos="3,2.5!"];
        x4[label="[1]",pos="1,-0.5!"];
        x5[label="[2]",pos="3,-0.5!"];
        x6[label="[2]",pos="2,0.5!"];
        x7[label="[3]",pos="4.5,1!"];
    }

    1->2[label="4"];
    2->3[label="7"];
    3->7[label="9"];
    1->4[label="10"];
    4->5[label="3"];
    5->7[label="6"];
    2->6[label="2"];
    6->5[label="1"];
    4->6[label="5"];
    6->3[label="6"];
    2->4[label="1"];
    5->3[label="5"];

}
```

## Dinic 算法复杂度分析

总体时间复杂度：$O(V^{2}E)$

1.  **分层阶段 (BFS)**：
    *   每次建立分层图需要遍历整张图，复杂度 $O(E)$。
    *   分层图的层数最多为 $V$（最坏情况是一条链），所以最多建立 $V$ 次分层图。
2.  **增广阶段 (DFS)**：
    *   引入**当前弧优化**后，每条边在每个分层图中最多被“榨干”一次。
    *   这一部分的均摊复杂度上限较为复杂，但在普通网络中表现非常优秀。

**特殊情况：**

*   **二分图匹配**：Dinic 的复杂度会降为 $O(E\sqrt{V})$。这使得 Dinic 也可以高效解决二分图最大匹配问题。

## 朴素Dinic算法演示

<x-gif src="Dinic1.gif"></x-gif>

## 优化策略

### 1. 多路增广 (Multi-path Augmentation)

在朴素的 DFS 中，每找到一条增广路就立即回溯并更新残余网络。而在 **多路增广** 中，如果当前点 $u$ 接收到了流量 `limit`，我们可以尝试用这股流量去填充 $u$ 连接的所有允许弧。

**核心逻辑：**

1.  记录当前点 $u$ 接收到的总流量 `limit`，以及已经流出去的流量 `flow`。
2.  遍历 $u$ 的每一条允许弧 $(u, v)$：
    -   如果 $u$ 还有剩余流量（`limit - flow > 0`），则尝试向 $v$ 增广。
    -   增广得到的流量为 $f$，更新 `flow += f`。
3.  **炸点优化 (Dead-end Elimination)**：如果遍历完 $u$ 的所有出边后，发现流出的总流量 `flow` 为 $0$，说明从 $u$ 出发无法到达汇点 $t$。为了避免在同一轮 BFS 中重复搜索这个“死胡同”，我们可以将 $level(u)$ 设为一个无效值（如 $-1$）。

### 2. 当前弧优化 (Current Arc Optimization)

在 DFS 过程中，如果我们已经尝试过从点 $u$ 的某条边 $e$ 进行增广，并且 $e$ 已经被“榨干”（容量变为 $0$）或者通过 $e$ 无法到达汇点，那么在**同一轮 BFS 分层图**中，我们没有必要再次尝试这条边。

**实现方法：**
1.  为每个点维护一个“当前弧”指针 `cur[u]`，初始指向 `head[u]`。
2.  在 DFS 遍历 $u$ 的出边时，直接从 `cur[u]` 开始。
3.  每当我们完整地探索完一条边 $i$ 且无法再从该边获得更多流量时，就更新 `cur[u] = e[i].next`。

> **注意**：`cur` 数组必须在每一轮 BFS 重新分层后重置为 `head`。

```viz-dot
<%- include("dot/dinic_current_edge.dot") %>
```

## Dinic 算法模板 (C++)

包含 **当前弧优化**、**多路增广** 和 **炸点优化** 的标准模板。

```cpp
#include <iostream>
#include <queue>
#include <algorithm>
#include <cstring>

using namespace std;

const int MAXN = 10005;
const int MAXE = 200005;
const long long INF = 1e18; // 使用 long long 防止溢出

struct Edge {
    int to, next;
    long long cap;
} e[MAXE];

int head[MAXN], cur[MAXN], dep[MAXN], cnt = 0;
int n, m, s, t;

void addEdge(int u, int v, long long cap) {
    e[cnt] = {v, head[u], cap};
    head[u] = cnt++;
    e[cnt] = {u, head[v], 0}; // 反向边
    head[v] = cnt++;
}

// BFS 构建分层图
bool bfs() {
    memset(dep, -1, sizeof(dep));
    queue<int> q;
    q.push(s);
    dep[s] = 0;
    cur[s] = head[s]; // 也可以在 dfs 前统一重置

    while (!q.empty()) {
        int u = q.front();
        q.pop();
        for (int i = head[u]; i != -1; i = e[i].next) {
            int v = e[i].to;
            if (dep[v] == -1 && e[i].cap > 0) {
                dep[v] = dep[u] + 1;
                cur[v] = head[v]; // 顺便重置当前弧
                q.push(v);
                if (v == t) return true; // 提前退出优化
            }
        }
    }
    return false;
}

// DFS 多路增广
long long dfs(int u, long long limit) {
    if (u == t || limit == 0) return limit;
    
    long long flow = 0;
    for (int i = cur[u]; i != -1; i = e[i].next) {
        cur[u] = i; // 更新当前弧
        int v = e[i].to;
        if (dep[v] == dep[u] + 1 && e[i].cap > 0) {
            long long f = dfs(v, min(limit, e[i].cap));
            if (f > 0) {
                e[i].cap -= f;
                e[i ^ 1].cap += f;
                flow += f;
                limit -= f;
                if (limit == 0) break; // 流量用尽
            }
        }
    }
    if (flow == 0) dep[u] = -1; // 炸点优化：此点在本轮 BFS 中已无用
    return flow;
}

long long dinic() {
    long long max_flow = 0;
    while (bfs()) {
        max_flow += dfs(s, INF);
    }
    return max_flow;
}

int main() {
    ios::sync_with_stdio(false);
    cin.tie(nullptr);

    memset(head, -1, sizeof(head));
    cin >> n >> m >> s >> t;
    for (int i = 0; i < m; i++) {
        int u, v;
        long long w;
        cin >> u >> v >> w;
        addEdge(u, v, w);
    }
    
    cout << dinic() << endl;
    return 0;
}
```

题目地址：[luogu P3376 【模板】网络最大流](https://www.luogu.org/problemnew/show/P3376)

### 练习 2：[USACO 4.2] 草地排水

[题目地址 (POJ 1273)](https://vjudge.net/problem/POJ-1273#author=CCOA)

### 练习 3：[USACO 2007 Open Gold] Dining

[题目地址 (POJ 3281)](https://vjudge.net/problem/POJ-3281#author=Darnt)

**解题思路：拆点**

建图如下，注意：所有的边的容量为 1。
```viz-dot
digraph { 
    rankdir = LR
    node [shape=circle,fixedsize=true,height=1];
    edge [samehead=n,sametail=n];
    S,T;

    ranksep=1.2;
    nodesep=0.8;
    n1,n2,n3,n4;
    {
        rank=same;
        f1,f2,f3;
    }

    n1[label="牛1"];
    n2[label="牛2"];
    n3[label="牛3"];
    n4[label="牛4"];
    f1[label="食物1"];
    f2[label="食物2"];
    f3[label="食物3"];

    d1[label="饮料1"];
    d2[label="饮料2"];
    d3[label="饮料3"];



    S->{f1,f2,f3};

    {f1,f2}->n1;
    {f2,f3}->n2;
    {f1,f3}->{n3,n4};
    n1->{d1,d3};
    n2->{d1,d2};
    n3->{d1,d2};
    n4->d3;

    { d1,d2,d3 }->T;
    {
        rank=same;
        d1,d2,d3;
    }
}
```
但这样产生了新的问题，如下图：

```viz-dot
digraph { 
    rankdir = LR
    node [shape=circle,fixedsize=true,height=1];
    edge [samehead=n,sametail=n];
    S,T;

    ranksep=1.2;
    nodesep=0.8;

    n1[label="牛1"];
    f1[label="食物1"];
    f2[label="食物2"];

    d1[label="饮料1"];
    d2[label="饮料2"];

    S->{f1,f2}->n1->{d1,d2}->T;
}
```
这种情况下**牛 1** 可以吃多份食物与饮料。为了解决这个问题，我们使用下面的建图方式：**牛 1 -> 牛 1'**，这可以**保证流经每头牛身上的流量最多为 1**。

```viz-dot
digraph { 
    rankdir = LR
    node [shape=circle,fixedsize=true,height=1];
    edge [samehead=n,sametail=n];
    S,T;

    ranksep=1.2;
    nodesep=0.8;
    n1,n2,n3,n4;
    {
        rank=same;
        f1,f2,f3;
    }

    n1[label="牛1"];
    n2[label="牛2"];
    n3[label="牛3"];
    n4[label="牛4"];

    n11[label="牛1'"];
    n22[label="牛2'"];
    n33[label="牛3'"];
    n44[label="牛4'"];

    n1->n11;
    n2->n22;
    n3->n33;
    n4->n44;

    f1[label="食物1"];
    f2[label="食物2"];
    f3[label="食物3"];

    d1[label="饮料1"];
    d2[label="饮料2"];
    d3[label="饮料3"];

    S->{f1,f2,f3};


    {f1,f2}->n1;
    {f2,f3}->n2;
    {f1,f3}->{n3,n4};
    n11->{d1,d3};
    n22->{d1,d2};
    n33->{d1,d2};
    n44->d3;

    { d1,d2,d3 }->T;
    {
        rank=same;
        d1,d2,d3;
    }
}
```

这种把一个点拆分成两个点，从而限制流经该点的最大流量的操作，称为**拆点**。

那么我们总共要建立 $2 + 2 	imes n + f + d$ 个点。其中，源点 $S$ 编号为 $0$，汇点 $T$ 编号为 $1 + 2 	imes n + f + d$。

## 引用/资料

- [Dinic算法详解及实现 - 博客园](https://www.cnblogs.com/LUO77/p/6115057.html)
- [Dinic算法（网络流） - 博客园](http://cnblogs.com/SYCstudio/p/7260613.html)
- [网络流之Dinic算法 - CSDN](https://blog.csdn.net/u012914220/article/details/23865829)
