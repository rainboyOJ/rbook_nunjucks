# 字典树 Trie：读入 n 个字符串后处理 q 次询问。
# op=1 判断字符串是否完整插入过（输出 Yes/No）；op=2 输出以该串为前缀的字符串个数。
# 字符集为小写字母 a-z（对应 C++ 的 OFFSET='a'），0 号节点为根。
import sys


def main() -> None:
    data = sys.stdin.buffer.read().split()
    pos = 0
    n = int(data[pos]); pos += 1
    q = int(data[pos]); pos += 1

    # 每个节点：ch 为 {字符: 子节点编号}，pass 为经过次数，end 为单词结尾次数。
    ch: list[dict[str, int]] = [{}]
    pass_cnt: list[int] = [0]
    end_cnt: list[int] = [0]

    for _ in range(n):
        s = data[pos].decode(); pos += 1
        u = 0
        pass_cnt[u] += 1
        for c in s:
            nxt = ch[u].get(c)
            if nxt is None:
                nxt = len(ch)
                ch[u][c] = nxt
                ch.append({})
                pass_cnt.append(0)
                end_cnt.append(0)
            u = nxt
            pass_cnt[u] += 1
        end_cnt[u] += 1

    out: list[str] = []
    for _ in range(q):
        op = int(data[pos]); pos += 1
        s = data[pos].decode(); pos += 1
        u = 0
        ok = True
        for c in s:
            nxt = ch[u].get(c)
            if nxt is None:
                ok = False
                break
            u = nxt
        if op == 1:
            out.append("Yes" if ok and end_cnt[u] > 0 else "No")
        else:
            out.append(str(pass_cnt[u] if ok else 0))

    sys.stdout.write("\n".join(out) + ("\n" if out else ""))


if __name__ == "__main__":
    main()
