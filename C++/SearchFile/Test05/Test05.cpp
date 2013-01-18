// Test05.cpp : 定义控制台应用程序的入口点。
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
                "sf path [/s][/p 模式串][/r]\n"
                "/s : 递归搜索子文件夹。\n"
                "/p : 匹配文件名的模式。是posix扩展的正则表达式。\n"
                "/r : 输出相对路径。\n"
            );
        return false;
    }

    for (int i = 1; i < argc; ++i)
    {
        char *arg = argv[i];
        verify(arg != NULL && arg[0] != 0);
        if (arg[0] != '/')
        {
            // 文件夹路径
            verify(g_searchPath.empty() && "搜索的文件夹已经指定！");
            verify(boost::filesystem::is_directory(arg) && "只能搜索文件夹！");
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
                        verify(i + 1 < argc && "/p 参数必须指定模式串！");
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
                    printf("无效的参数 : \\%c\n", arg[1]);
                    break;
            }
        }
    }
    verify(!g_searchPath.empty() && "必须指定要搜索的文件夹！"); 

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
