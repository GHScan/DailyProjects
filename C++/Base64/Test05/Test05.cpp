// Test05.cpp : 定义控制台应用程序的入口点。
//
#include "stdafx.h"

#include <cassert>

#include <string>
#include <vector>

typedef unsigned char       byte;

const byte BASER64_CODE_TABLE[] =
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
const byte BASER64_RCODE_TABLE[] = 
"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x3e\x00\x00\x00\x3f\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\x00\x00\x00\x00\x00\x00\x00\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x00\x00\x00\x00\x00\x00\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x00\x00\x00\x00\x00";

class Base64Encoder
{
public:
    void pushText(const void* text, size_t len, bool end)
    {
        m_leftText.insert(m_leftText.end(), (const byte*)text, (const byte*)text + len);

        size_t div3 = m_leftText.size() / 3;
        size_t mod3 = m_leftText.size() % 3;

        m_code.reserve(m_code.size() + div3 * 4 + 4);

        m_code.resize(m_code.size() + div3 * 4);
        const byte *srcPtr = &m_leftText[0];
        byte *destPtr = (byte*)m_code.c_str() + m_code.size() - div3 * 4;
        for (size_t i = 0; i < div3; ++i)
        {
            destPtr[0] = BASER64_CODE_TABLE[srcPtr[0] >> 2];
            destPtr[1] = BASER64_CODE_TABLE[((srcPtr[0] << 4) & 0x3f) | (srcPtr[1] >> 4)];
            destPtr[2] = BASER64_CODE_TABLE[((srcPtr[1] << 2) & 0x3f) | (srcPtr[2] >> 6)];
            destPtr[3] = BASER64_CODE_TABLE[srcPtr[2] & 0x3f];

            srcPtr += 3;
            destPtr += 4;
        }

        if (!end) 
        {
            m_leftText.assign(srcPtr, srcPtr + mod3);
            return;
        }
        m_leftText.clear();

        if (mod3 == 0) return;

        m_code.resize(m_code.size() + 4);
        destPtr = (byte*)m_code.c_str() + m_code.size() - 4;
        if (mod3 == 1)
        {
            destPtr[0] = BASER64_CODE_TABLE[srcPtr[0] >> 2];
            destPtr[1] = BASER64_CODE_TABLE[((srcPtr[0] << 4) & 0x3f) | (0 >> 4)];
            destPtr[2] = '=';
            destPtr[3] = '=';
        }
        else if (mod3 == 2)
        {
            destPtr[0] = BASER64_CODE_TABLE[srcPtr[0] >> 2];
            destPtr[1] = BASER64_CODE_TABLE[((srcPtr[0] << 4) & 0x3f) | (srcPtr[1] >> 4)];
            destPtr[2] = BASER64_CODE_TABLE[((srcPtr[1] << 2) & 0x3f) | (0 >> 6)];
            destPtr[3] = '=';
        }
    }

    const std::string& getCode() const
    {
        return m_code;
    }

    static size_t calcCodeLength(const void *text, size_t len)
    {
        return (len + 2) / 3;
    }

private:
    std::string         m_code;
    std::vector<byte>   m_leftText;
};

class Base64Decoder
{
public:
    void pushCode(const std::string& code, bool end)
    {
        m_leftCode += code;

        size_t div4 = m_leftCode.size() / 4;
        size_t mod4 = m_leftCode.size() % 4;

        if (div4 >= 1)
        {   
            size_t newTextLen = calcTextLength(m_leftCode.c_str(), div4 * 4);
            assert(newTextLen == div4 * 3 || end);

            m_text.resize(m_text.size() + newTextLen);
            byte *destPtr = &m_text[0] + m_text.size() - newTextLen;
            const byte*srcPtr = (const byte*)m_leftCode.c_str();

            size_t div4WithNoPadding = div4 - (newTextLen < div4 * 3 ? 1 : 0);
            for (size_t i = 0; i < div4WithNoPadding; ++i)
            {
                destPtr[0] = (BASER64_RCODE_TABLE[srcPtr[0]] << 2) | (BASER64_RCODE_TABLE[srcPtr[1]] >> 4);
                destPtr[1] = (BASER64_RCODE_TABLE[srcPtr[1]] << 4) | (BASER64_RCODE_TABLE[srcPtr[2]] >> 2);
                destPtr[2] = (BASER64_RCODE_TABLE[srcPtr[2]] << 6) | (BASER64_RCODE_TABLE[srcPtr[3]]);

                srcPtr += 4;
                destPtr += 3;
            }

            if (newTextLen < div4 * 3)
            {
                if (newTextLen < div4 * 3 - 1)
                {
                    assert(newTextLen == div4 * 3 - 2);
                    destPtr[0] = (BASER64_RCODE_TABLE[srcPtr[0]] << 2) | (BASER64_RCODE_TABLE[srcPtr[1]] >> 4);
                }
                else
                {
                    destPtr[0] = (BASER64_RCODE_TABLE[srcPtr[0]] << 2) | (BASER64_RCODE_TABLE[srcPtr[1]] >> 4);
                    destPtr[1] = (BASER64_RCODE_TABLE[srcPtr[1]] << 4) | (BASER64_RCODE_TABLE[srcPtr[2]] >> 2);
                }
            }
        }

        if (end)
        {
            assert(mod4 == 0);
        }
        else
        {
            m_leftCode = m_leftCode.c_str() + div4 * 4;
        }
    }

    const std::vector<byte>& getText() const
    {
        return m_text;
    }

    static size_t calcTextLength(const char* code, size_t len)
    {
        assert(len % 4 == 0);
        size_t padding = 0;
        if (len >= 1 && code[len - 1] == '=') 
        {
            ++padding;
            if (len >= 2 && code[len - 2] == '=') ++padding;
        }
        return (len / 4) * 3 - padding;
    }

private:
    std::vector<byte>   m_text;
    std::string         m_leftCode;
};

#include <boost/progress.hpp>

int main()
{
    const char *strs[] =
    {
        "a",
        "ab",
        "abc",
        "abcd",
        "abcde",
        "abcdef",
    };

    for (size_t i = 0; i < _countof(strs); ++i)
    {
        const char *str = strs[i];

        if (i % 2 == 0)
        {
            Base64Encoder encoder;
            for (size_t i = 0; i < strlen(str); ++i)
                encoder.pushText(&str[i], 1, i == strlen(str) - 1);
            cout << encoder.getCode() << endl;

            Base64Decoder decoder;
            for (size_t i = 0; i < encoder.getCode().size(); ++i)
                decoder.pushCode(std::string(1, encoder.getCode()[i]), i == encoder.getCode().size() - 1);
            assert(memcmp((const char*)&decoder.getText()[0], strs[i], strlen(strs[i])) == 0);
        }
        else
        {
            Base64Encoder encoder;
            encoder.pushText(str, strlen(str), true);
            cout << encoder.getCode() << endl;

            Base64Decoder decoder;
            decoder.pushCode(encoder.getCode(), true);
            assert(memcmp((const char*)&decoder.getText()[0], strs[i], strlen(strs[i])) == 0);
        }
    }

    {
        std::string s;
        for (int i = 0; i < (1 << 20) ; ++i)
            s += "abc_def";

        boost::progress_timer _t;
        Base64Encoder encoder;
        encoder.pushText(s.c_str(), s.size(), true);
        Base64Decoder decoder;
        decoder.pushCode(encoder.getCode(), true);
    }
}