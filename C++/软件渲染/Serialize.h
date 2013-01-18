// vim: fileencoding=gbk

#ifndef SERIALIZE_H
#define SERIALIZE_H

#include <string>
#include <iostream>

class StreamBlockWriter
{
public:
    StreamBlockWriter(const char *blockName, std::ostream& so);
    ~StreamBlockWriter();
    void write(const char *fieldName, int val);
    void write(const char *fieldName, const int *begin, const int *end);
    void write(const char *fieldName, float val);
    void write(const char *fieldName, const float *begin, const float *end);
    void write(const char *fieldName, const char * val);
private:
    std::ostream& m_so;
};

class StreamBlockReader
{
public:
    StreamBlockReader(const char *blockName, std::istream& si);
    ~StreamBlockReader();
    bool read(const char *fieldName, int* val);
    bool read(const char *fieldName, int *begin,  int *end);
    bool read(const char *fieldName, float* val);
    bool read(const char *fieldName, float *begin,  float *end);
    bool read(const char *fieldName, char * buf, int len);
    bool read(const char *fieldName, std::string& s);
private:
    std::istream& m_si;
    bool m_blockOpened;
};

#endif // #ifndef SERIALIZE_H
