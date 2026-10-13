# 并查集的 set 替代写法：小并大（启发式合并），只适合"合并 + 判同"。
# 下标约定：元素编号 1..n。
# 用法：init(n); unite(1, 3); same(1, 3) -> True; members(1) 直接拿到集合内所有元素
# 复杂度：判同 O(1)；合并 O(小集合大小)，每个元素最多被搬 log n 次，总 O(n log n)。
# 边界：维护元素间关系（带权/种类并查集）或要高效撤销时不能用，老实写并查集。
# 易错点：必须小并大，不比大小直接 s1 |= s2 最坏退化 O(n^2)。

bel: dict[int, set[int]] = {}  # 元素 -> 所在集合对象


def init(n: int) -> None:
    """每个元素各自成集合。"""
    global bel
    bel = {i: {i} for i in range(1, n + 1)}


def same(x: int, y: int) -> bool:
    """x 与 y 是否同一集合：比较集合对象即可，O(1)。"""
    return bel[x] is bel[y]


def unite(x: int, y: int) -> bool:
    """小集合的元素逐个搬进大集合，返回是否真的发生了合并。"""
    s1, s2 = bel[x], bel[y]
    if s1 is s2:
        return False
    if len(s1) < len(s2):
        s1, s2 = s2, s1
    for v in s2:
        bel[v] = s1
    s1 |= s2
    return True


def members(x: int) -> set[int]:
    """x 所在集合的所有元素；len(bel[x]) 就是集合大小——set 写法的加分项。"""
    return bel[x]
