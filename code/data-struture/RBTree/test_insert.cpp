#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <memory_resource>
#include <functional>
#include <random>


// Using declarations to make code cleaner
using std::cout;
using std::endl;
using std::string;
using std::vector;


#include "rbtree.cpp"


std::mt19937 rng(std::random_device{}());

void random_test() {
    // 随机生成数据,测试插入
    RBTree::RBTree<int> tree;
    int n = rand() % 100;
    cout << "insert n = " << n << endl;
    cout << ">>> Insert values : " ;
    std::vector<int> values;
    for(int i = 0; i < n; i++) {
        int val = rng() % 100;
        cout << val << " ";
        values.push_back(val);
    }
    cout << endl;

    for(int val : values) {
        tree.insert(val);
    }
    // tree.print();
    // cout << endl;
    cout << "Validation: " << (tree.isValid() ? "Valid" : "Invalid") << endl;
    cout << endl << "-----------------------------------" << endl;
}

int main()
{
    RBTree::RBTree<int> tree;

    cout << "Inserting values into the RBTree..." << endl;
    cout << "-----------------------------------" << endl;
    
    // A set of values to test various insertion cases
    // int values[] = {10, 85, 15, 70, 20, 60, 30, 50, 65, 80, 90, 40, 5, 55};
    // int values[] = {3,2,1};
    // int values[] = {1,2,3};
    // 随机生成数据,测试插入

    int T = 20;
    for(int i = 0; i < T; i++) {
        cout << "test i = " << i+1 << " | " ;
        random_test();
    }

    return 0;
}
