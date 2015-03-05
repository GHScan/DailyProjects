#include "stdafx.h"

//------------------------------
class Solution {
public:
    vector<int> searchRange(int A[], int n, int target) {
        int first = 0, last = n - 1;
        while (first <= last) {
            int mid = (first + last) / 2;
            if (A[mid] < target) first = mid + 1;
            else if (A[mid] > target) last = mid - 1;
            else {
                return {lowerBound(A, first, mid, target), upperBound(A, mid, last, target)};
            }
        }
        return {-1, -1};
    }
private:
    int lowerBound(int A[], int first, int last, int target) {
        assert(first <= last);
        if (A[first] >= target);
        else if (A[last] < target) return -1;
        else {
            ++first;
            while (first < last) {
                int mid = (first + last) / 2;
                if (A[mid] < target) first = mid + 1;
                else last = mid;
            }
        }
        return A[first] == target ? first : -1;
    }
    int upperBound(int A[], int first, int last, int target) {
        assert(first <= last);
        if (A[first] > target) return -1;
        else if (A[last] <= target) first = last;
        else {
            --last;
            while (first < last) {
                int mid = (first + last + 1) / 2;
                if (A[mid] <= target) first = mid;
                else last = mid - 1;
            }
        }
        return A[first] == target ? first : -1;
    }
};

//------------------------------
#define countOf(a) int(sizeof(a) / sizeof(a[0]))
#include <algorithm>

static void printResult(vector<int> const &result) {
    for (auto i : result) cout << i << ',';
    cout << endl;
}

int main() {
    Solution s;
    {
        puts("------------------------");
        int a[] = {1};
        printResult(s.searchRange(a, 0, 0));
        printResult(s.searchRange(a, 0, 1));
        printResult(s.searchRange(a, 0, 2));
        printResult(s.searchRange(a, countOf(a), 0));
        printResult(s.searchRange(a, countOf(a), 1));
        printResult(s.searchRange(a, countOf(a), 2));
    }
    {
        puts("------------------------");
        int a[] = {1, 3, 5 };
        for (int i = 0; i < 7; ++i) {
            printResult(s.searchRange(a, countOf(a), i));
        }
    }
    {
        puts("------------------------");
        int a[] = { 1, 3, 3, 3, 5, 5 };
        for (int i = 0; i < 7; ++i) {
            printResult(s.searchRange(a, countOf(a), i));
        }
    }
    {
        puts("------------------------");
        int a[] = { 2, 2, 2, };
        for (int i = 0; i < 4; ++i) {
            printResult(s.searchRange(a, countOf(a), i));
        }
    }
}
