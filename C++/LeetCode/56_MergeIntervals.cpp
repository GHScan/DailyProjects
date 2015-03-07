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
    vector<Interval> merge(vector<Interval> &intervals) {
        sort(intervals.begin(), intervals.end(), [](Interval const &a, Interval const &b){
            return a.start < b.start;
        });
        
        vector<Interval> result;
        for (int i = 0; i < (int)intervals.size(); ) {
            int start = intervals[i].start;
            int end = intervals[i].end;
            for (++i; i < (int)intervals.size() && intervals[i].start <= end; ++i) {
                end = max(end, intervals[i].end);
            }
            result.push_back(Interval(start, end));
        }
        return result;
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
    printResult(s.merge(vector<Interval>{}));
    printResult(s.merge(vector<Interval>{Interval(1, 1)}));
    printResult(s.merge(vector<Interval>{Interval(1, 1), Interval(1, 1)}));
    printResult(s.merge(vector<Interval>{ Interval(1, 1), Interval(1, 2) }));
    printResult(s.merge(vector<Interval>{ Interval(1, 2), Interval(2, 3) }));
    printResult(s.merge(vector<Interval>{ Interval(1, 2), Interval(3, 3) }));
    printResult(s.merge(vector<Interval>{ Interval(1, 3), Interval(2, 4) }));
    printResult(s.merge(vector<Interval>{ Interval(1, 3), Interval(-1, 4) }));
    printResult(s.merge(vector<Interval>{ Interval(1, 3), Interval(2, 6), Interval(8, 10), Interval(15, 18) }));
}
