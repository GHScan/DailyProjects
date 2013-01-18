// Test05.cpp : 定义控制台应用程序的入口点。
//

#include "stdafx.h"

#include <cassert>
#include <cmath>

#include <exception>
#include <fstream>
#include <string>

typedef unsigned char byte;

namespace Rle
{
    inline byte makeChunkHead_Same(size_t count) { assert(count > 0 && count <= 128); return byte(0x80 | (count - 1)); }
    inline byte makeChunkHead_Diff(size_t count) { assert(count > 0 && count <= 128); return byte(0x00 | (count - 1)); }
    inline bool isSameChunk(byte head) { return (head >> 7) != 0; }
    inline size_t getChunkLength(byte head) { return (head & 0x7f) + 1; }

    inline void encodeChunk(void* dest, size_t destLen, const void *src, size_t srcLen, size_t *destInc, size_t *srcInc)
    {
        assert(src != NULL && srcLen > 0 && destInc != NULL && srcInc != NULL);

        // 最多只处理128
        srcLen = srcLen > 128 ? 128 : srcLen;

        byte* destPtr = (byte*)dest;
        const byte *srcPtr = (const byte*)src;

        // 相等为+, 不等为-
        int count = 0;
        for (size_t i = 1; i < srcLen; ++i)
        {
            if (srcPtr[i] == srcPtr[i - 1])
            {
                if (count < 0) break;
                ++count;
            }
            else 
            {
                if (count > 0) break;
                --count;
            }
        }

        if (count < 0) --count;
        else ++count;
        assert(count != 0);

        *srcInc = abs(count);
        *destInc = count > 0 ? 2 : -count + 1;

        if (dest == NULL || destLen == 0) return;
        assert(destLen >= *destInc);

        if (count > 0) 
        {
            destPtr[0] = makeChunkHead_Same(count);
            destPtr[1] = srcPtr[0];
        }
        else
        {
            count = -count;
            destPtr[0] = makeChunkHead_Diff(count);
            for (int i = 0; i < count; ++i)
            {
                destPtr[1 + i] = srcPtr[i];
            }
        }
    }

    inline void decodeChunk(void* dest, size_t destLen, const void *src, size_t srcLen, size_t *destInc, size_t *srcInc)
    {
        assert(src != NULL && srcLen > 0 && destInc != NULL && srcInc != NULL);

        const byte* srcPtr = (const byte*)src;
        byte* destPtr = (byte*)dest;

        size_t count = getChunkLength(srcPtr[0]);
        if (isSameChunk(srcPtr[0]))
        {
            *srcInc = 2;
            *destInc = count;
            if (dest == NULL || destLen == 0) return;
            assert(destLen >= *destInc);
            assert(srcLen >= *srcInc);

            for (size_t i = 0; i < count; ++i)
            {
                destPtr[i] = srcPtr[1];
            }
        }
        else
        {
            *srcInc = 1 + count;
            *destInc = count;
            if (dest == NULL || destLen == 0) return;
            assert(destLen >= *destInc);
            assert(srcLen >= *srcInc);

            for (size_t i = 0; i < count; ++i)
            {
                destPtr[i] = srcPtr[i + 1];
            }
        }
    }
}

void encodeFile(const char *destFile, const char *srcFile)
{
    std::ofstream fo(destFile, std::ios::binary);
    std::ifstream fi(srcFile, std::ios::binary);   

    char buff[1 << 12] = {0};
    char destBuff[129] = {0};
    while (fi)
    {
        fi.read(buff, sizeof(buff));
        size_t size = fi.gcount();

        size_t offset = 0;
        while (offset < size)
        {
            size_t destInc = 0, srcInc = 0;
            Rle::encodeChunk(destBuff, sizeof(destBuff), buff + offset, size - offset, &destInc, &srcInc);
            offset += srcInc;
            fo.write(destBuff, destInc);
        }
    }
}

void decodeFile(const char *destFile, const char *srcFile)
{
    std::ofstream fo(destFile, std::ios::binary);
    std::ifstream fi(srcFile, std::ios::binary);   

    char buff[1 << 12] = {0};
    char destBuff[128] = {0};
    size_t leftBytes = 0;
    while (fi)
    {
        fi.read(buff + leftBytes, sizeof(buff) - leftBytes);
        size_t size = fi.gcount();
        size += leftBytes;
        leftBytes = 0;

        size_t offset = 0;
        while (offset < size)
        {
            size_t destInc = 0, srcInc = 0;
            Rle::decodeChunk(NULL, 0, buff + offset, size - offset, &destInc, &srcInc);
            if (srcInc > size - offset) break;
            Rle::decodeChunk(destBuff, sizeof(destBuff), buff + offset, size - offset, &destInc, &srcInc);
            assert(srcInc > 0 && destInc > 0);

            offset += srcInc;
            fo.write(destBuff, destInc);
        }

        assert(offset <= size);

        if (offset < size)
        {
            leftBytes = size - offset;
            memmove(buff, buff + offset, leftBytes);
        }
    }

    if (leftBytes > 0) throw std::runtime_error("要解码的文件不完整!");
}

void pauseConsole()
{
    cin.get();
}

int main(int argc, char *argv[])
{
    atexit(pauseConsole);

    if (argc < 2) {cout << "输入要编码的文件!" << endl; return 0;}

    try
    {
        std::string srcFile = argv[1];
        std::string bakFile = srcFile;
        bakFile.replace(bakFile.rfind('.') + 1, -1, "bak");
        std::string restoreFile = srcFile;
        restoreFile.replace(restoreFile .rfind('.') + 1, -1, "rst");

        encodeFile(bakFile.c_str(), srcFile.c_str());
        decodeFile(restoreFile.c_str(), bakFile.c_str());

        {
            size_t oldSize = 0, newSize = 0;
            {
                std::ifstream fi;
                fi.open(srcFile.c_str(), std::ios::binary);
                if (fi) { fi.seekg(-1, std::ios_base::end); oldSize = fi.tellg(); }
                fi.close(); fi.clear();

                fi.open(bakFile.c_str(), std::ios::binary);
                if (fi) { fi.seekg(-1, std::ios_base::end); newSize = fi.tellg(); }
                fi.close(); fi.clear();
            }
            if (oldSize > 0 && newSize > 0)
            {
                cout << "压缩比 : " << (newSize * 100.0f) / oldSize << endl;
            }
        }
    }
    catch(std::exception& e) { cout << "异常! :" << e.what() << endl; }
}