// vim: fileencoding=gbk
#include "pch.h"

#include "Vector.h"

Vector2 Vector2::ZERO = Vector2(0, 0);
Vector3 Vector3::ZERO = Vector3(0, 0, 0);
Vector4 Vector4::ZERO = Vector4(0, 0, 0, 0);

std::ostream& operator << (std::ostream& so, const Vector2& o)
{
    so << '(' << o.x << ',' << o.y << ')';
    return so;
}
std::istream& operator >> (std::istream& si, Vector2& o)
{
    char c;
    c = si.get(); assert(c == '(');
    si >> o.x;
    c = si.get(); assert(c == ',');
    si >> o.y;
    c = si.get(); assert(c == ')');
    return si;
}

std::ostream& operator << (std::ostream& so, const Vector3& o)
{
    so << '(' << o.x << ',' << o.y << ',' << o.z << ')';
    return so;
}
std::istream& operator >> (std::istream& si, Vector3& o)
{
    char c;
    c = si.get(); assert(c == '(');
    si >> o.x;
    c = si.get(); assert(c == ',');
    si >> o.y;
    c = si.get(); assert(c == ',');
    si >> o.z;
    c = si.get(); assert(c == ')');
    return si;
}

std::ostream& operator << (std::ostream& so, const Vector4& o)
{
    so << '(' << o.x << ',' << o.y << ',' << o.z << ',' << o.w << ')';
    return so;
}
std::istream& operator >> (std::istream& si, Vector4& o)
{
    char c;
    c = si.get(); assert(c == '(');
    si >> o.x;
    c = si.get(); assert(c == ',');
    si >> o.y;
    c = si.get(); assert(c == ',');
    si >> o.z;
    c = si.get(); assert(c == ',');
    si >> o.w;
    c = si.get(); assert(c == ')');
    return si;
}
