import sys

def main():
    tokens = sys.stdin.read().split()
    if not tokens:
        return
    n = int(tokens[0])
    cur_sum = 0
    res = []
    for i in range(1, n + 1):
        cur_sum += int(tokens[i])
        res.append(str(cur_sum))
    sys.stdout.write(" ".join(res) + "\n")

if __name__ == "__main__":
    main()
