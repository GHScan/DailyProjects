#include "stdafx.h"

struct Interval {
    int start;
    int end;
    Interval() : start(0), end(0) {}
    Interval(int s, int e) : start(s), end(e) {}
};
//------------------------------
#include <algorithm>

class Solution {
public:
    vector<Interval> insert(vector<Interval> &intervals, Interval newInterval) {
        auto it = lower_bound(intervals.begin(), intervals.end(), newInterval, [](Interval const &a, Interval const &b){
            return a.start < b.start;
        });
        it = intervals.insert(it, newInterval);

        int begin = it - intervals.begin();
        int end = begin + 1;
        for (; begin > 0 && intervals[begin - 1].end >= newInterval.start; --begin) {
            newInterval.start = intervals[begin - 1].start;
            newInterval.end = max(newInterval.end, intervals[begin - 1].end);
        }
        for (; end < (int)intervals.size() && intervals[end].start <= newInterval.end; ++end) {
            newInterval.end = max(newInterval.end, intervals[end].end);
        }
        intervals.erase(intervals.begin() + begin, intervals.begin() + end);
        intervals.insert(intervals.begin() + begin, newInterval);
        return intervals;
    }
};

//------------------------------
static void printResult(vector<Interval> const &result) {
    for (auto interval : result) {
        printf("[%d,%d],", interval.start, interval.end);
    }
    puts("");
}

int main() {
    Solution s;
    printResult(s.insert(vector<Interval>{}, Interval(1, 1)));
    printResult(s.insert(vector<Interval>{Interval(1, 1)}, Interval(2, 2)));
    printResult(s.insert(vector<Interval>{Interval(1, 5)}, Interval(2, 3)));
    printResult(s.insert(vector<Interval>{Interval(1, 1), Interval(2, 2)}, Interval(1, 1)));
    printResult(s.insert(vector<Interval>{ Interval(1, 1), Interval(2, 2) }, Interval(-1, 1)));
    printResult(s.insert(vector<Interval>{ Interval(1, 2), Interval(3, 3) }, Interval(2, 4)));
    printResult(s.insert(vector<Interval>{ Interval(1, 2), Interval(3, 3) }, Interval(1, 2)));
    printResult(s.insert(vector<Interval>{ Interval(1, 3), Interval(4, 4) }, Interval(2, 4)));
    printResult(s.insert(vector<Interval>{ Interval(1, 3), Interval(4, 4) }, Interval(-1, 2)));
    printResult(s.insert(vector<Interval>{ Interval(1, 3), Interval(4, 6), Interval(8, 10), Interval(15, 18) }, Interval(9, 12)));
    printResult(s.insert(vector<Interval>{ Interval(1, 3), Interval(4, 6), Interval(8, 10), Interval(15, 18) }, Interval(-1, 14)));
    printResult(s.insert(vector<Interval>{ Interval(1, 3), Interval(4, 6), Interval(8, 10), Interval(15, 18) }, Interval(-1, 100)));

    printResult(s.insert(vector<Interval>{ Interval(1, 3), Interval(6, 9) }, Interval(2, 5)));
    printResult(s.insert(vector<Interval>{ Interval(1, 2), Interval(3, 5), Interval(6, 7), Interval(8, 10), Interval(12, 16) }, Interval(4, 9)));
}
