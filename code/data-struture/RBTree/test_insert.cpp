#include <iostream>
#include <string>
#include <vector>
#include <cctype>
#include <memory_resource>
#include <functional>


// Using declarations to make code cleaner
using std::cout;
using std::endl;
using std::string;
using std::vector;


#include "rbtree.cpp"

int main()
{
    RBTree::RBTree<int> tree;

    cout << "Inserting values into the RBTree..." << endl;
    cout << "-----------------------------------" << endl;
    
    // A set of values to test various insertion cases
    int values[] = {10, 85, 15, 70, 20, 60, 30, 50, 65, 80, 90, 40, 5, 55};

    for (int val : values) {
        cout << ">>> Inserting " << val << "..." << endl;
        tree.ins(val);
        tree.print();
        cout << endl << "-----------------------------------" << endl;
    }

    cout << "Final RBTree structure:" << endl;
    tree.print();

    return 0;
}
