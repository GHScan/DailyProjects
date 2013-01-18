// vim: fileencoding=gbk
#include "pch.h"

#include <cstdio>
#include <cassert>

#include "Serialize.h"

#pragma warning(disable : 4996) // 'sprintf' was declared deprecated

static char g_buf[1024];
static char g_buf2[1024];
static std::string g_nestedWriterPrefix;

StreamBlockWriter::StreamBlockWriter(const char *blockName, std::ostream& so):
    m_so(so)
{
    assert(blockName != NULL);
    sprintf(g_buf, "%s%s {\n", g_nestedWriterPrefix.c_str(), blockName); m_so << g_buf;
    g_nestedWriterPrefix.push_back('\t');
}
StreamBlockWriter::~StreamBlockWriter()
{
    g_nestedWriterPrefix.resize(g_nestedWriterPrefix.size() - 1);
    sprintf(g_buf, "%s}\n", g_nestedWriterPrefix.c_str()); m_so << g_buf;
}
void StreamBlockWriter::write(const char *fieldName, int val)
{
    write(fieldName, &val, &val + 1);
}
void StreamBlockWriter::write(const char *fieldName, bool val)
{
    write(fieldName, (int)val);
}
void StreamBlockWriter::write(const char *fieldName, const int *begin, const int *end)
{
    assert(fieldName != NULL && begin <= end);
    char *p = g_buf;
    p += sprintf(p, "%s%s: ", g_nestedWriterPrefix.c_str(), fieldName);
    while (begin < end) {
        p += sprintf(p, "%d ", *begin++);
    }
    *p++ = '\n'; *p++ = 0;
    m_so << g_buf;
}
void StreamBlockWriter::write(const char *fieldName, float val)
{
    write(fieldName, &val, &val + 1);
}
void StreamBlockWriter::write(const char *fieldName, const float *begin, const float *end)
{
    assert(fieldName != NULL && begin <= end);
    char *p = g_buf;
    p += sprintf(p, "%s%s: ", g_nestedWriterPrefix.c_str(), fieldName);
    while (begin < end) {
        p += sprintf(p, "%f ", *begin++);
    }
    *p++ = '\n'; *p++ = 0;
    m_so << g_buf;
}
void StreamBlockWriter::write(const char *fieldName, const char * val)
{
    assert(fieldName != NULL && val != NULL);
    sprintf(g_buf, "%s%s: %s\n", g_nestedWriterPrefix.c_str(), fieldName, val);
    m_so << g_buf;
}

StreamBlockReader::StreamBlockReader(const char *blockName, std::istream& si):
    m_si(si)
{
    m_si.getline(g_buf, sizeof(g_buf));
    char *p = g_buf;
    while (isspace(*p)) ++p;
    sprintf(g_buf2, "%s {", blockName);
    assert(strcmp(p, g_buf2) == 0);
}
StreamBlockReader::~StreamBlockReader()
{
    m_si.getline(g_buf, sizeof(g_buf));
    char *p = g_buf;
    while (isspace(*p)) ++p;
    assert(strcmp(p, "}") == 0);
}
bool StreamBlockReader::read(const char *fieldName, int* val)
{
    return read(fieldName, val, val + 1);
}
bool StreamBlockReader::read(const char *fieldName, bool* val)
{
    int i = 0;
    if (!read(fieldName, &i, &i + 1)) return false;
    *val = i != 0;
    return true;
}
bool StreamBlockReader::read(const char *fieldName, int *begin,  int *end)
{
    m_si.getline(g_buf, sizeof(g_buf));
    char *p = g_buf;
    while (isspace(*p)) ++p;
    int off = sprintf(g_buf2, "%s: ", fieldName);
    if (strncmp(p, g_buf2, off)) return false;
    p += off;
    while (begin < end) {
        if (*p == 0) return false;
        if (sscanf(p, "%d", begin) == 0) return false;
        while (isspace(*p)) ++p;
        while (*p && !isspace(*p)) ++p;
        ++begin;
    }
    return true;
}
bool StreamBlockReader::read(const char *fieldName, float* val)
{
    return read(fieldName, val, val + 1);
}
bool StreamBlockReader::read(const char *fieldName, float *begin,  float *end)
{
    m_si.getline(g_buf, sizeof(g_buf));
    char *p = g_buf;
    while (isspace(*p)) ++p;
    int off = sprintf(g_buf2, "%s: ", fieldName);
    if (strncmp(p, g_buf2, off)) return false;
    p += off;
    while (begin < end) {
        if (*p == 0) return false;
        if (sscanf(p, "%f", begin) == 0) return false;
        while (isspace(*p)) ++p;
        while (*p && !isspace(*p)) ++p;
        ++begin;
    }
    return true;
}
bool StreamBlockReader::read(const char *fieldName, char * buf, int len)
{
    m_si.getline(g_buf, sizeof(g_buf));
    char *p = g_buf;
    while (isspace(*p)) ++p;
    int off = sprintf(g_buf2, "%s: ", fieldName);
    if (strncmp(p, g_buf2, off)) return false;
    p += off;
    int l = (int)strlen(p);
    if (l > len) return false;
    strcpy(buf, p);
    return true;
}
bool StreamBlockReader::read(const char *fieldName, std::string& s)
{
    char buf[128] = "";
    if (!read(fieldName, buf, sizeof(buf))) return false;
    s = buf;
    return true;
}
