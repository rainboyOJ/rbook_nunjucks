from math import gcd, lcm


def main() -> None:
    samples = [(6, 8), (12, 18), (15, 28), (21, 35)]
    print("a b gcd lcm gcd*lcm a*b")
    for a, b in samples:
        g = gcd(a, b)
        l = lcm(a, b)
        print(a, b, g, l, g * l, a * b)


if __name__ == "__main__":
    main()
