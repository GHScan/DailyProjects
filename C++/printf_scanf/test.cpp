// vim:fileencoding=gbk
//
#include "pch.h"

#include <stdarg.h>
#include <math.h>
#include <assert.h>

#include <string>

template<typename T>
T* reverse(T* begin, T* end)
{
    for (T *first = begin, *last = end - 1; first < last; ++first, --last) {
        T val = *first;
        *first = *last;
        *last = val;
    }
    return begin;
}

void formatInt(std::string& buf, const char *fmt, int val)
{
    bool negative = false;
    if (val < 0) {
        negative = true;
        val = -val;
    }
    while (val > 0) {
        buf += val % 10 + '0';
        val /= 10;
    }
    if (negative) buf += '-';
    reverse(&buf[0], &buf[0] + buf.size());
}

void formatDouble(std::string& buf, const char *fmt, double d)
{
    bool negative = false;
    if (d < 0) {
        negative = true;
        d = -d;
    }
    int pointW = 0;
    if (*fmt == '.') {
        ++fmt;
        while (*fmt >= '0' && *fmt <= '9') {
            pointW *= 10;
            pointW += *fmt++ - '0';
        }
    }

    double frac = fmod(d, 1);
    bool hasPoint = false;
    if (frac > 0 || pointW > 0) {
        for (int i = pointW == 0 ? 6 : pointW; i > 0; --i) {
            frac *= 10;
            int j = (int)frac;
            buf += j + '0';
            frac -= j;
        }
        reverse(&buf[0], &buf[0] + buf.size());
        buf += '.';
        hasPoint = true;
    }

    while (d > 0.9) {
        buf += (int)fmod(d, 10) + '0';
        d /= 10;
    }

    if (negative) buf += '-';
    reverse(&buf[0], &buf[0] + buf.size());

    if (hasPoint && pointW == 0) {
        while (buf[buf.size() - 1] == '0') buf.resize(buf.size() - 1);
    }
    if (buf[buf.size() - 1] == '.') buf.resize(buf.size() - 1);
}

void formatString(std::string& buf, const char *fmt, const char *str)
{
    buf = str;
}

struct PrintFormatDesc
{
    bool isAlignLeft;
    char fill;
    int width;
    const char* parse(const char *fmt);
    PrintFormatDesc();
};
PrintFormatDesc::PrintFormatDesc():
    isAlignLeft(false), fill(' '), width(0){}
const char* PrintFormatDesc::parse(const char *fmt)
{
    isAlignLeft = false;
    fill = ' ';
    width = 0;

    if (*fmt == '-') {
        isAlignLeft = true;
        ++fmt;
    }
    if (*fmt == '0' || *fmt == ' ') {
        fill = *fmt++;
    }
    while (*fmt >= '0' && *fmt <= '9') {
        width *= 10;
        width += *fmt++ - '0';
    }
    return fmt;
}

int myfscanf(FILE *f, const char *fmt, ...)
{
    int r = 0;
    return r;
}

int myprintf(FILE *f, const char *fmt, ...)
{
    int r = 0;

    va_list arg;
    va_start(arg, fmt);

    std::string buf;
    std::string buf2;
    bool pattern = false;
    while (*fmt) {
        if (pattern) {
            PrintFormatDesc desc;
            pattern = false;
            switch (*fmt) {
                case '%': 
                    buf2 = '%';
                    break;
                case 'd':
                    formatInt(buf2, desc.parse(buf.c_str()), va_arg(arg, int));
                    break;
                case 'f':
                    formatDouble(buf2, desc.parse(buf.c_str()), va_arg(arg, double));
                    break;
                case 's': 
                    formatString(buf2, desc.parse(buf.c_str()), va_arg(arg, const char *));
                    break;
                default: 
                    buf += *fmt;
                    pattern = true;
                    break;
            }
            if (!pattern) {
                if ((int)buf2.size() < desc.width) {
                    if (desc.isAlignLeft) {
                        buf2 += std::string(desc.width - buf2.size(), desc.fill);
                    }
                    else {
                        buf2 = std::string(desc.width - buf2.size(), desc.fill) + buf2;
                    }
                }
                fwrite(buf2.c_str(), buf2.size(), 1, f);
                buf2.clear();
                buf.clear();
            }
        }
        else {
            if (*fmt == '%') {
                fwrite(buf.c_str(), buf.size(), 1, f);
                pattern = true;
                buf.clear();
            }
            else {
                buf += *fmt;
            }
        }
        ++fmt;
    }

    assert(!pattern);
    fwrite(buf.c_str(), buf.size(), 1, f);
    buf.clear();

    va_end(arg);

    return r;
}

int main()
{
    myprintf(stdout, "%%s test -> my face %s says : %s\n", "scan", "hello world");
    myprintf(stdout, "%%d test -> %d, %-5d, %05d\n", 3, 3, 3);
    myprintf(stdout, "%%f test -> %f, %.4f, %.4f\n", 3.1415926, 3.14, 3.1415926);
    for (int i = 1; i <= 9; ++i) {
        for (int j = 1; j <= i; ++j) {
            myprintf(stdout, "%dx%d=%-2d ", j, i, i * j);
        }
        myprintf(stdout, "\n");
    }
}
