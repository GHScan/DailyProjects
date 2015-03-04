class Solution {
public:
	int removeDuplicates(int A[], int n) {
		int len = 0, i = 0;
		while (i < n) {
			int v = A[i++];
			A[len++] = v;
			for (; i < n && A[i] == v; ++i);
		}
		return len;
	}
};
