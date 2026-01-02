// 手写离散化
struct DiscreteManual {
    std::vector<int> xs;

    void add(int x) {
        xs.push_back(x);
    }

    // 手写去重逻辑
    void build() {
        if (xs.empty()) return;

        std::sort(xs.begin(), xs.end());

        int p = 0; 
        for(int i = 1; i < xs.size(); i++) {
            if (xs[i] != xs[p]) {
                xs[++p] = xs[i];
            }
        }
        // 此时 xs[0...p] 是去重后的数组
        xs.resize(p + 1); // 截断 vector，扔掉后面重复的部分
    }

    int get(int x) {
        return lower_bound(xs.begin(), xs.end(), x) - xs.begin() + 1;
    }

    int origin(int k) {
        return xs[k - 1];
    }
} disc;
