#include <string>
#include <vector>
#include <functional>
#include <algorithm>
using namespace std;

#include <time.h>

static void timeit(int n, function<void()> f)
{
	if (n > 1) f();
	clock_t start = clock();
	for (auto i = 0; i < n; ++i) f();
	printf("%.6fms\n", double(clock() - start) * 1000 / CLOCKS_PER_SEC / n);
}

static bool compare(int a, int b) { return a < b; }
static int qcompare(const void *a, const void *b) { return *(int*)a - *(int*)b; }

int main() 
{	
	vector<int> v(1024 * 1024);
	{
		int i = 0;
		generate(v.begin(), v.end(), [&i](){return i++; });
	}
	static const int LOOP = 1;
	static const int TIME = 8;

	timeit(TIME, [&v](){
		for (auto i = 0; i < LOOP; ++i) random_shuffle(v.begin(), v.end());
	});
	timeit(TIME, [&v](){
		for (auto i = 0; i < LOOP; ++i) {
			random_shuffle(v.begin(), v.end());
			sort(v.begin(), v.end());
		}
	});
	timeit(TIME, [&v](){
		for (auto i = 0; i < LOOP; ++i) {
			random_shuffle(v.begin(), v.end());
			sort(v.begin(), v.end(), [](int a, int b){return a < b; });
		}
	});
	timeit(TIME, [&v](){
		for (auto i = 0; i < LOOP; ++i) {
			random_shuffle(v.begin(), v.end());
			sort(v.begin(), v.end(), function<bool(int, int)>([](int a, int b){return a < b; }));
		}
	});
	timeit(TIME, [&v](){
		for (auto i = 0; i < LOOP; ++i) {
			random_shuffle(v.begin(), v.end());
			sort(v.begin(), v.end(), compare);
		}
	});
	timeit(TIME, [&v](){
		for (auto i = 0; i < LOOP; ++i) {
			random_shuffle(v.begin(), v.end());
			qsort(&v[0], v.size(), sizeof(v[0]), qcompare);
		}
	});
}
