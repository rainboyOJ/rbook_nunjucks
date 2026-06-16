#include <bits/stdc++.h>
using namespace std;

#define debug_line cerr << "\n=================\n"
#define debug(...)                                                             \
    do {                                                                       \
        cerr << "LINE:" << __LINE__ << " ";                                    \
        string names = #__VA_ARGS__;                                           \
        replace(names.begin(), names.end(), ',', ' ');                         \
        stringstream ss(names);                                                \
        istream_iterator<string> it(ss);                                       \
        debug_print(it, __VA_ARGS__);                                          \
    } while (false)

void debug_print(istream_iterator<string>) {}

template <typename T, typename... Args>
void debug_print(istream_iterator<string> it, const T& value,
                 const Args&... args) {
    cerr << *it << " = " << value << "\n";
    debug_print(++it, args...);
}
