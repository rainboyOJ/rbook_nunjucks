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


#define RBTree_DEBUG
#include "rbtree.cpp"


std::mt19937 rng(std::random_device{}());


int main()
{
    RBTree::RBTree<int> tree;

    cout << "Inserting values into the RBTree..." << endl;
    cout << "-----------------------------------" << endl;
    
    // A set of values to test various insertion cases
    // int values[] = {10, 85, 15, 70, 20, 60, 30, 50, 65, 80, 90, 40, 5, 55};
    // int values[] = {3,2,1};
    // int values[] = {1,2,3};
    // int values[] = {1,3,2};
    int values[] = {
// 11,38,27,49,1,3,78,19,12
2, 6, 5, 7, 0, 1, 8, 4, 3
    };

    for (int val : values) {
        cout << ">>> Inserting " << val << "..." << endl;
        tree.insert(val);
        tree.print();
        cout << "Validation: " << (tree.isValid() ? "Valid" : "Invalid") << endl;
        cout << endl << "-----------------------------------" << endl;
    }

    cout << "Final RBTree structure:" << endl;
    tree.print();
    cout << endl;

    cout << "Validation: " << (tree.isValid() ? "Valid" : "Invalid") << endl;

    return 0;
}
