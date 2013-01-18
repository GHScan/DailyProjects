#pragma once

#include <string>
#include <vector>

namespace Scan
{

typedef  std::string                    String;
typedef  std::vector<std::string>       StringVec;
typedef  std::vector<unsigned char>     BytesVec;

// ���һ�������ǲ��ɿ�����, ˽�м̳�Copyable<false>, ����̳�true
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