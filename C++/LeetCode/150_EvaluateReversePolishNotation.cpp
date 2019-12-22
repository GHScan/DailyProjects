
#include <cstdio>

#include <string>
#include <vector>
#include <stack>
#include <list>
#include <unordered_map>
#include <iostream>

using namespace std;

class Solution {
public:
    int evalRPN(vector<string> const &tokens) {
        stack<int> nums;
        for (string const &token : tokens)
        {
            if (token.size() == 1)
            {
                switch (token[0])
                {
                case '+':
                {
                    int b = nums.top(); nums.pop();
                    int a = nums.top(); nums.pop();
                    nums.push(a + b);
                }
                break;
                case '-':
                {
                    int b = nums.top(); nums.pop();
                    int a = nums.top(); nums.pop();
                    nums.push(a - b);
                }
                break;
                case '*':
                {
                    int b = nums.top(); nums.pop();
                    int a = nums.top(); nums.pop();
                    nums.push(a * b);
                }
                break;
                case '/':
                {
                    int b = nums.top(); nums.pop();
                    int a = nums.top(); nums.pop();
                    nums.push(a / b);
                }
                break;
                default:
                    nums.push(token[0] - '0');
                    break;
                }
            }
            else
            {
                nums.push(atoi(token.c_str()));
            }
        }
        return nums.top();
    }
};

int main()
{
    cout << Solution().evalRPN({ "2", "1", "+", "3", "*" }) << "\n";
    cout << Solution().evalRPN({ "4", "13", "5", "/", "+" }) << "\n";
    cout << Solution().evalRPN({ "10", "6", "9", "3", "+", "-11", "*", "/", "*", "17", "+", "5", "+" }) << "\n";
}