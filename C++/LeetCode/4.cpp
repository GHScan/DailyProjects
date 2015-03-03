#include "stdafx.h"

#include <assert.h>

//--------------------------------------------------

class Solution {
public:
	int findInA(int A[], int m, int B[], int n) {
		auto half = (n + m) / 2;
		auto begin = 0, end = min(m, half + 1);
		while (begin < end) {
			auto leftA = (begin + end) / 2;
			auto leftB = half - leftA;
			if (leftB > n || (leftB > 0 && B[leftB - 1] > A[leftA])) begin = leftA + 1;
			else if (leftB < n && B[leftB] < A[leftA]) end = leftA;
			else return leftA;
		}
		return -1;
	}

	double findMedianSortedArrays(int A[], int m, int B[], int n) {
		auto idx = findInA(A, m, B, n);
		if (idx == -1) {
			swap(A, B);
			swap(m, n);
			idx = findInA(A, m, B, n);
		}
		assert(idx != -1);

		int half = (m + n) / 2;
		if ((m + n) % 2 == 0) {
			if (idx == 0) return (A[idx] + B[half - idx - 1]) / 2.0;
			else if (idx == half) return (A[idx] + A[idx - 1]) / 2.0;
			else return (max(A[idx - 1], B[half - idx - 1]) + A[idx]) / 2.0;
		} else {
			return A[idx];
		}
	}
};

//--------------------------------------------------

int main() {
	Solution s;
	{
		int a[] = {3,}, b[] = {1, 2};
		cout << s.findMedianSortedArrays(a, 1, b, 2) << endl;
	}
	{
		int a[] = {2,}, b[] = {1, 3};
		cout << s.findMedianSortedArrays(a, 1, b, 2) << endl;
	}
	{
		int a[] = {2,}, b[] = {2, 3};
		cout << s.findMedianSortedArrays(a, 0, b, 2) << endl;
	}
	{
		int a[] = {3,}, b[] = {1,};
		cout << s.findMedianSortedArrays(a, 0, b, 1) << endl;
	}
	{
		int a[] = {1,2,3,4,5,7,8,9,10}, b[] = {6,};
		cout << s.findMedianSortedArrays(a, 9, b, 1) << endl;
	}	
	{
		int a[] = {1}, b[] = {2,3,4,5,6,7,};
		cout << s.findMedianSortedArrays(a, 1, b, 6) << endl;
	}	
}
