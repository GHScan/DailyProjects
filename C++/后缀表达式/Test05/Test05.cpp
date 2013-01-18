// Test05.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <cmath>

#include <string>
#include <vector>

#include <windows.h>

#include "Util.h"

const int MAX_OP_PRIORITY = 256;

inline int opIndex(char c)
{
    switch (c)
    {
    case '+': return 0;
    case '-': return 1;
    case '*': return 2;
    case '/': return 3;
    case '!': return 4;
    default: break;
    }
    verify(0); return -1;
}

static int opPriority(char c)
{
    const int priorityTable[] = 
    {
        1, 1, 2, 2, 3,
    };
    return priorityTable[opIndex(c)];
}

static int opParamNum(char c)
{
    const int numTable[] = 
    {
        2, 2, 2, 2, 1,
    };
    return numTable[opIndex(c)];
}

// 是否支持交换律
static bool isOpSupportCommLaw(char c)
{
    const bool supportCommLaw[] =
    {
        true, false, true, false, false,
    };
    return supportCommLaw[opIndex(c)];
}

static bool isPrefixParam1Op(char c)
{
    verify(opParamNum(c) == 1);
    const bool prefixTable[] =
    {
        false, false, false, false, true,
    };
    return prefixTable[opIndex(c)];
}

static double add(double a, double b){ return a + b; }
static double sub(double a, double b){ return a - b; }
static double mul(double a, double b){ return a * b; }
static double div(double a, double b){ return a / b; }
static double not(double a) { return fabs(a) < 0.0001f ? 1 : 0; }

typedef double(*fParam2)(double, double);
typedef double(*fParam1)(double);

static fParam2 opFunc(char c)
{
    const fParam2 procTable[] = 
    {
        &add, &sub, &mul, &div, (fParam2)&not,
    };
    return procTable[opIndex(c)];
}

// 打印后缀表达式
static void printPostfixExpression(const std::vector<std::string> &postfix)
{
    for (int i = 0; i < (int)postfix.size(); ++i) cout << postfix[i] << ",";
    cout << "\n";
}

// 中缀到后缀
static std::vector<std::string> expressionInfixToPostfix(const std::string& infix)
{
    verify(!infix.empty());

    std::vector<std::string> result;
    std::vector<char> opStack;

    for (std::istringstream si(infix);;)
    {
        char c = si.get();
        if (!si) break;

        if (isdigit(c))
        {
            si.unget();

            double d = 0; 
            si >> d;
            result.push_back(toString(d));
        }
        else
        {
            if (c == '(')
            {
                opStack.push_back(c);
            }
            else if (c == ')')
            {
                while (
                    !opStack.empty() &&
                    opStack.back() != '(')
                {
                    result.push_back(std::string() + opStack.back());
                    opStack.pop_back();
                }
                opStack.pop_back();
            }
            else
            {
                while (
                    !opStack.empty() 
                    && opStack.back() != '(' 
                    && opPriority(opStack.back()) >= opPriority(c))
                {
                    result.push_back(std::string() + opStack.back());
                    opStack.pop_back();
                }
                opStack.push_back(c);
            }
        }
    }

    while (
        !opStack.empty() &&
        opStack.back() != '(')
    {
        result.push_back(std::string() + opStack.back());
        opStack.pop_back();
    }

    verify(opStack.empty());

    return result;
}

// 后缀到中缀
static std::string expressionPostfixToInfix(const std::vector<std::string> &_postfix)
{
    verify(!_postfix.empty());

    std::vector<std::string> postfix(_postfix);
    std::vector<int> lowestExpPriofity(_postfix.size(), MAX_OP_PRIORITY);

    for (int i = 0; i < (int)postfix.size(); ++i)
    {
        verify(!postfix[i].empty());

        char c = postfix[i][0];
        if (isdigit(c)) continue;

        verify(postfix[i].size() == 1);

        int paramNum = opParamNum(c);
        verify(i - paramNum >= 0);

        if (paramNum == 1)
        {
            if (isPrefixParam1Op(c))
            {
                if (lowestExpPriofity[i - 1] <= opPriority(c))
                {
                    postfix[i - 1] = postfix[i] + "(" + postfix[i - 1] + ")";
                }
                else postfix[i - 1] = postfix[i] + postfix[i - 1];
            }
            else
            {
                if (lowestExpPriofity[i - 1] < opPriority(c))
                {
                    postfix[i - 1] = "(" + postfix[i - 1] + ")" + postfix[i];
                }
                else postfix[i - 1] = postfix[i - 1] + postfix[i];
            }

            lowestExpPriofity[i - 1] = opPriority(c);
            postfix.erase(postfix.begin() + i);
            lowestExpPriofity.erase(lowestExpPriofity.begin() + i);
            --i;
        }
        else if (paramNum == 2)
        {
            std::string opAndNum;
            if (lowestExpPriofity[i - 2] < opPriority(c))
            {
                opAndNum += "(" + postfix[i - 2] + ")";
            }
            else opAndNum += postfix[i - 2];
            opAndNum += c;
            if (lowestExpPriofity[i - 1] <= opPriority(c))
            {
                opAndNum += "(" + postfix[i - 1] + ")";
            }
            else opAndNum += postfix[i - 1];

            postfix[i - 2] = opAndNum;
            lowestExpPriofity[i - 2] = opPriority(c);
            postfix.erase(postfix.begin() + i - 1, postfix.begin() + i + 1);
            lowestExpPriofity.erase(lowestExpPriofity.begin() + i - 1, lowestExpPriofity.begin() + i + 1);
            i -= 2;
        }
        else verify(0);
    }

    verify(postfix.size() == 1 && !postfix.empty());
    return postfix[0];
}

// 对后缀表达式求值
static double calcValOfPostfixExpression(const std::vector<std::string> &_postfix)
{
    verify(!_postfix.empty());

    std::vector<std::string> postfix(_postfix);

    for (int i = 0; i < (int)postfix.size(); ++i)
    {
        verify(!postfix[i].empty());

        char c = postfix[i][0];
        if (isdigit(c)) continue;

        verify(postfix[i].size() == 1);

        int paramNum = opParamNum(c);
        verify(i - paramNum >= 0);

        if (paramNum == 1)
        {
            bool parseOk = false;
            double d = parseString<double>(postfix[i - 1], &parseOk);
            verify(parseOk);

            postfix[i - 1] = toString((*(fParam1)opFunc(c))(d));
            postfix.erase(postfix.begin() + i);
            --i;
        }
        else if (paramNum == 2)
        {
            bool parseOk = false;
            double d1 = parseString<double>(postfix[i - 2], &parseOk);
            verify(parseOk);
            double d2 = parseString<double>(postfix[i - 1], &parseOk);
            verify(parseOk);

            postfix[i - 2] = toString((*opFunc(c))(d1, d2));
            postfix.erase(postfix.begin() + i - 1, postfix.begin() + i + 1);
            i -= 2;
        }
        else verify(0);
    }

    verify(postfix.size() == 1 && !postfix.back().empty());
    {
        bool parseOk = false;
        double d;
        d = parseString<double>(postfix.back(), &parseOk);
        return d;
    }
}

int main()
{
    for (std::string l; getline(cin, l);)
    {
		// 要求值中缀，先转化为后缀，因为转化的过程就过滤掉了优先级和括号，而转化过程又极其简单...
        std::vector<std::string> postfix = expressionInfixToPostfix(l);

        print("postfix expression:");
        printPostfixExpression(postfix);
        print(expressionPostfixToInfix(postfix));

        print(calcValOfPostfixExpression(postfix));
    }
}