
#include <cstdio>

#include <string>
#include <vector>
#include <stack>
#include <list>
#include <unordered_map>
#include <iostream>
#include <algorithm>

using namespace std;

class MinStack {
public:
    MinStack() {
    }

    void push(int x) {
        stack.push_back(x);
        if (mins.empty())
            mins.push_back(x);
        else
            mins.push_back(min(x, mins.back()));
    }

    void pop() {
        stack.pop_back();
        mins.pop_back();
    }

    int top() {
        return stack.back();
    }

    int getMin() {
        return mins.back();
    }

private:
    vector<int> stack;
    vector<int> mins;
};

int main()
{
    MinStack minStack;
    minStack.push(-2);
    minStack.push(0);
    minStack.push(-3);
    cout << minStack.getMin() << "\n"; // -- > Returns - 3.
    minStack.pop();
    cout << minStack.top() << "\n"; //     -- > Returns 0.
    cout << minStack.getMin() << "\n"; //   -- > Returns - 2.
}