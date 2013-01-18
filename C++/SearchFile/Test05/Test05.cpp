// Test05.cpp : �������̨Ӧ�ó������ڵ㡣
//

#include "stdafx.h"

#include <ctype.h>

#include <string>

#include <boost/regex.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string.hpp>

#include "util.h"

std::string g_searchPath;
std::string g_pattern;
bool g_recursiveSearch = false;
bool g_printRelatPath = false;

static bool parseCommand(int argc, char *argv[])
{
    if  (argc < 2) 
    {
        puts(
                "sf path [/s][/p ģʽ��][/r]\n"
                "/s : �ݹ��������ļ��С�\n"
                "/p : ƥ���ļ�����ģʽ����posix��չ��������ʽ��\n"
                "/r : ������·����\n"
            );
        return false;
    }

    for (int i = 1; i < argc; ++i)
    {
        char *arg = argv[i];
        verify(arg != NULL && arg[0] != 0);
        if (arg[0] != '/')
        {
            // �ļ���·��
            verify(g_searchPath.empty() && "�������ļ����Ѿ�ָ����");
            verify(boost::filesystem::is_directory(arg) && "ֻ�������ļ��У�");
            g_searchPath = arg;
        }
        else 
        {
            char c = tolower(arg[1]);
            switch (arg[1])
            {
                case 's':
                    g_recursiveSearch = true;
                    break;
                case 'p':
                    {
                        verify(i + 1 < argc && "/p ��������ָ��ģʽ����");
                        arg = argv[++i];
                        verify(arg != NULL && arg[0] != 0);
                        verify(arg[0] != '/');
                        g_pattern = arg;
                    }
                    break;
                case 'r':
                    g_printRelatPath = true;
                    break;
                default:
                    printf("��Ч�Ĳ��� : \\%c\n", arg[1]);
                    break;
            }
        }
    }
    verify(!g_searchPath.empty() && "����ָ��Ҫ�������ļ��У�"); 

    return true;
}

void work()
{
    boost::regex reg(g_pattern);

    if (g_recursiveSearch)
    {
        boost::filesystem::recursive_directory_iterator begin(g_searchPath), end;
        while (begin != end)
        {
            if (boost::filesystem::is_regular_file(begin->status())) 
            {
                std::string s = begin->string();
                boost::replace_all(s, "/", "\\");
                if (g_printRelatPath) s = s.substr(g_searchPath.size());
                if (g_pattern.empty() || boost::regex_search(s, reg))
                {
                    puts(s.c_str());
                }
            }
            ++begin;
        }
    }
    else
    {
        boost::filesystem::directory_iterator begin(g_searchPath), end;
        while (begin != end)
        {
            if (boost::filesystem::is_regular_file(begin->status())) 
            {
                std::string s = begin->string();
                boost::replace_all(s, "/", "\\");
                if (g_printRelatPath) s = s.substr(g_searchPath.size());
                if (g_pattern.empty() || boost::regex_search(s, reg))
                {
                    puts(s.c_str());
                }
            }
            ++begin;
        }
    }    
}

int main(int argc, char *argv[])
{
    try
    {
        if (parseCommand(argc, argv))
        {
            work();
        }
    }
    catch(std::exception& e)
    {
        printf("exception : %s\n", e.what());
    }

    return 0;
}
