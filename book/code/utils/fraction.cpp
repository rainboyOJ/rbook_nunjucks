#include <bits/stdc++.h>
using namespace std;

struct Fraction {
    long long num = 0;
    long long den = 1;

    Fraction(long long numerator = 0, long long denominator = 1) {
        num = numerator;
        den = denominator;
        normalize();
    }

    void normalize() {
        if (den == 0) {
            num = (num >= 0 ? 1 : -1);
            return;
        }
        if (den < 0) {
            num = -num;
            den = -den;
        }
        long long g = std::gcd(llabs(num), llabs(den));
        if (g != 0) {
            num /= g;
            den /= g;
        }
    }

    friend bool operator==(const Fraction& a, const Fraction& b) {
        return a.num == b.num && a.den == b.den;
    }

    friend bool operator<(const Fraction& a, const Fraction& b) {
        return (__int128)a.num * b.den < (__int128)b.num * a.den;
    }

    friend Fraction operator+(const Fraction& a, const Fraction& b) {
        return Fraction(a.num * b.den + b.num * a.den, a.den * b.den);
    }

    friend Fraction operator-(const Fraction& a, const Fraction& b) {
        return Fraction(a.num * b.den - b.num * a.den, a.den * b.den);
    }

    friend Fraction operator*(const Fraction& a, const Fraction& b) {
        return Fraction(a.num * b.num, a.den * b.den);
    }

    friend ostream& operator<<(ostream& out, const Fraction& x) {
        if (x.den == 0) return out << (x.num > 0 ? "inf" : "-inf");
        return out << x.num << '/' << x.den;
    }
};

int main() {
    Fraction a(1, 3), b(1, 6);
    cout << a + b << '\n';
    cout << boolalpha << (b < a) << '\n';
    return 0;
}
