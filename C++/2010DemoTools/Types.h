#pragma once

#include <string>
#include <vector>
#include <fstream>
#include <sstream>

namespace Scan
{
    typedef char                    int8;
    typedef short                   int16;
    typedef int                     int32;
    typedef long long               int64;
    typedef unsigned char           uint8;
    typedef unsigned short          uint16;
    typedef unsigned int            uint32;
    typedef unsigned long long      uint64;

    typedef wchar_t                 wchar;

    typedef std::string             String;
    typedef std::wstring            WString;
    typedef std::vector<String>     StringVector;
    typedef std::vector<WString>    WStringVector;
    typedef std::ifstream           IFStream;
    typedef std::ofstream           OFStream;
    typedef std::wifstream          WIFStream;
    typedef std::wofstream          WOFStream;
    typedef std::stringstream       StringStream;
    typedef std::wstringstream      WStringStream;
    typedef std::istringstream      IStringStream;
    typedef std::wistringstream     WIStringStream;
    typedef std::ostringstream      OStringStream;
    typedef std::wostringstream     WOStringStream;
    typedef std::istream            IStream;
    typedef std::ostream            OStream;
    typedef std::wostream           WOStream;
    typedef std::istream            IStream;
    typedef std::wistream           WIStream;
}   