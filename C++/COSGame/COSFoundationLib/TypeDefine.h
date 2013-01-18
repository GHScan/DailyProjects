#pragma once

#include <string>
#include <vector>

namespace Scan
{

typedef  std::string                    String;
typedef  std::vector<std::string>       StringVec;
typedef  std::vector<unsigned char>     BytesVec;

// 如果一个类型是不可拷贝的, 私有继承Copyable<false>, 否则继承true
template<bool copyable>
class Copyable
{
};

template<>
class Copyable<false>
{
public:
    Copyable(){}

private:
    Copyable(const Copyable&);
    Copyable& operator = (const Copyable&);
};

}