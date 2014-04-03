#include "pch.h"

#include "linkedList.h"
#include "BST.h"
#include "hashTable.h"

extern void test_list();
extern void test_linkedList();
extern void test_BST();
extern void test_hashTable();

int main() {
    test_list();
    test_linkedList();
    test_BST();
    test_hashTable();
}
