def is_prime(n: int) -> bool:
    if n < 2:
        return False
    i = 2
    while i * i <= n:
        if n % i == 0:
            return False
        i += 1
    return True


def main() -> None:
    m = int(input())
    if not is_prime(m):
        print("not prime")
        return

    for a in range(1, m):
        value = pow(a, m)
        print(f"{a}^{m} = {value} mod {m} = {value % m}")


if __name__ == "__main__":
    main()
