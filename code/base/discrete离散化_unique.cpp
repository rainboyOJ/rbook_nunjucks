/**  
* 步骤1：加入数据
*   for (int x : nums) d.add(x);
* 步骤2：构建
*   d.build();
* 步骤3：使用
*   去重后大小: :  d.size() 
*   for (int x : nums) cout << x << " -> " << d.get(x) << endl;
*/
struct Discrete {
    std::vector<int> xs;

    inline void clear() { xs.clear(); }

    // 1. 添加元素
    void add(int x) { xs.push_back(x); }

    // 2. 构建：排序并去重
    void build() {
        std::sort(xs.begin(), xs.end());
        xs.erase(unique(xs.begin(), xs.end()), xs.end());
    }

    // 3. 查询 x 映射后的值 (默认从 1 开始)
    // 如果找不到，返回的数值可能会超出范围，具体视 lower_bound 行为而定
    int get(int x) {
        return std::lower_bound(xs.begin(), xs.end(), x) - xs.begin() + 1;
    }

    // 3.2 查询 x 映射后的最大的值 (从 1 开始)
    int get_max() {
        return xs.size();
    }
    
    // 3.1 查询 x 映射后的值 (从 0 开始)
    int get_zero(int x) {
        return std::lower_bound(xs.begin(), xs.end(), x) - xs.begin();
    }

    // 4. 反向查询：根据离散化后的值 k 找回 原值 (k 从 1 开始)
    int origin(int k) {
        return xs[k - 1];
    }
    
    // 获取去重后的元素总数
    int size() {
        return xs.size();
    }
} disc;
