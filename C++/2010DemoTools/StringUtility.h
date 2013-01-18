#pragma once

#include <boost/algorithm/string.hpp>

#include "Types.h"

namespace Scan
{
    namespace StringUtil
    {
        /**
            @brief UTF8字符串
        */
        struct UTF8String
        { 
            String s; 
        };

        /**
            @brief 将UTF8字符串转化为ANSI字符串
        */
        String convertToString(const UTF8String& src);

        /**
            @brief 将Unicode字符串转化为ANSI字符串
        */
        String convertToString(const WString& src);

        /**
            @brief 将UTF8字符串转化为Unicode字符串
        */
        WString convertToWString(const UTF8String& src);

        /**
            @brief 将ANSI字符串转化为Unicode字符串
        */
        WString convertToWString(const String& src);

        /**
            @brief 将ANSI字符串转化为UTF8字符串
        */
        UTF8String convertToUTF8String(const String& src);

        /**
            @brief 将Unicode字符串转化为UTF8字符串
        */
        UTF8String convertToUTF8String(const WString& src);

        /**
            @brief 返回一个字符串大写的拷贝
        */
        inline String toUpperCopy(const String& s)   
        {
            return boost::algorithm::to_upper_copy(s);
        }

        /**
            @brief 将一个字符串转化为大写
        */
        inline void toUpper(String& s)
        {
            boost::algorithm::to_upper(s);
        }

        /**
            @brief 返回一个小写的拷贝
        */
        inline String toLowerCopy(const String& s)
        {
            return boost::algorithm::to_lower_copy(s);
        }

        /**
            @brief 将字符串转化为小写
        */
        inline void toLower(String &s)
        {
            boost::algorithm::to_lower(s);
        }

        /**
            @brief 返回去掉了左边空白的拷贝串
        */
        inline String trimLeftCopy(const String& s)
        {
            return boost::algorithm::trim_left_copy(s);
        }

        /**
            @brief 去掉字符串左边的空白
        */
        inline void trimLeft(String& s)
        {
            boost::algorithm::trim_left(s);
        }

        /**
            @brief 返回去掉了右边空白的拷贝串
        */
        inline String trimRightCopy(const String& s)
        {
            return boost::algorithm::trim_right_copy(s);
        }

        /**
            @brief 去掉右边的空白
        */
        inline void trimRight(String& s)
        {
            boost::algorithm::trim_right(s);
        }

        /**
            @brief 返回去掉左右空白的拷贝串
        */
        inline String trimCopy(const String& s)
        {
            return boost::algorithm::trim_copy(s);
        }

        /**
            @brief 去掉串的左右空白
        */
        inline void trim(String& s)
        {
            boost::algorithm::trim(s);
        }

        /**
            @brief 是否有一个前缀
            @param s 要测试的字符串
            @param prefix 前缀串
            @param isCaseSensitive 是否大小写敏感
        */
        inline bool isStartWith(const String& s, const String& prefix, bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::starts_with(s, prefix) : 
            boost::algorithm::istarts_with(s, prefix);
        }

        /**
            @brief 是否有一个后缀
        */
        inline bool isEndWith(const String& s, const String& postfix, bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::ends_with(s, postfix) :
            boost::algorithm::iends_with(s, postfix);
        }

        /**
            @brief 是否包含一个子串
        */
        inline bool isContain(const String& s, const String& sub, bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::contains(s, sub) :
            boost::algorithm::icontains(s, sub);
        }

        /**
            @brief 比较两个串
        */
        inline bool isEqual(const String& s, const String& test, bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::equals(s, test) :
            boost::algorithm::iequals(s, test);    
        }

        /**
            @brief 返回一个已经替换了一个第一次出现的子串的拷贝串
            @param oldSub 要替换的部分
            @param newSub 替换成的部分
            @return 替换过的拷贝串
        */
        inline String replaceFirstCopy(
            const String& src, 
            const String& oldSub,
            const String& newSub, 
            bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::replace_first_copy(src, oldSub, newSub) : 
            boost::algorithm::ireplace_first_copy(src, oldSub, newSub);
        }

        /**
            @brief 替换字符串中的一个第一次出现的字串
        */
        inline void replaceFirst(
            String &src, 
            const String& oldSub,
            const String& newSub, 
            bool isCaseSensitive = true)
        {
            isCaseSensitive ? 
            boost::algorithm::replace_first(src, oldSub, newSub) : 
            boost::algorithm::ireplace_first(src, oldSub, newSub);
        }

        /**
            @brief 返回一个删除了一个第一次出现的字串的拷贝串
        */
        inline String eraseFirstCopy(
            const String& src, 
            const String& sub,
            bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::erase_first_copy(src, sub) : 
            boost::algorithm::ierase_first_copy(src, sub);
        }

        /**
            @brief 删除字符串中一个字串第一次出现的部分
        */
        inline void eraseFirst(
            String &src, 
            const String& sub,
            bool isCaseSensitive = true)
        {
            isCaseSensitive ? 
            boost::algorithm::erase_first(src, sub) : 
            boost::algorithm::ierase_first(src, sub);
        }

        /**
            @brief 返回替换了字符串中某字串最后一次出现的部分的拷贝串
        */
        inline String replaceLastCopy(
            const String& src, 
            const String& oldSub,
            const String& newSub, 
            bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::replace_last_copy(src, oldSub, newSub) : 
            boost::algorithm::ireplace_last_copy(src, oldSub, newSub);
        }

        /**
            @brief 替换字符串中某字串的最后一次出现
        */
        inline void replaceLast(
            String &src, 
            const String& oldSub,
            const String& newSub, 
            bool isCaseSensitive = true)
        {
            isCaseSensitive ? 
            boost::algorithm::replace_last(src, oldSub, newSub) : 
            boost::algorithm::ireplace_last(src, oldSub, newSub);
        }

        /**
            @brief 返回字符串删除某字串最后一次出现的拷贝串
        */
        inline String eraseLastCopy(
            const String& src, 
            const String& sub,
            bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::erase_last_copy(src, sub) : 
            boost::algorithm::ierase_last_copy(src, sub);
        }

        /**
            @brief 删除字符串中某字串的最后一次出现
        */
        inline void eraseLast(
            String &src, 
            const String& sub,
            bool isCaseSensitive = true)
        {
            isCaseSensitive ? 
            boost::algorithm::erase_last(src, sub) : 
            boost::algorithm::ierase_last(src, sub);
        }

        /**
            @brief 返回一个替换了字符串中所有某字串后的拷贝
        */
        inline String replaceAllCopy(
            const String& src, 
            const String& oldSub,
            const String& newSub, 
            bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::replace_all_copy(src, oldSub, newSub) : 
            boost::algorithm::ireplace_all_copy(src, oldSub, newSub);
        }

        /**
           @brief 替换字符串中所有某字串
        */
        inline void replaceAll(
            String &src, 
            const String& oldSub,
            const String& newSub, 
            bool isCaseSensitive = true)
        {
            isCaseSensitive ? 
            boost::algorithm::replace_all(src, oldSub, newSub) : 
            boost::algorithm::ireplace_all(src, oldSub, newSub);
        }

        /**
            @brief 返回删除了字符串中所有某字串后的拷贝
        */
        inline String eraseAllCopy(
            const String& src, 
            const String& sub,
            bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::erase_all_copy(src, sub) : 
            boost::algorithm::ierase_all_copy(src, sub);
        }

        /**
            @brief 删除字符串中所有某字串
        */
        inline void eraseAll(
            String &src, 
            const String& sub,
            bool isCaseSensitive = true)
        {
            isCaseSensitive ? 
            boost::algorithm::erase_all(src, sub) : 
            boost::algorithm::ierase_all(src, sub);
        }

        /**
            @brief 比较两个字符串的大小
        */
        inline bool isLess(
            const String& first,
            const String& second,
            bool isCaseSensitive = true)
        {
            return 
                isCaseSensitive ? 
                boost::algorithm::lexicographical_compare(first, second) :
                boost::algorithm::ilexicographical_compare(first, second);
        }

        /**
            @brief 返回字符串分割的拷贝
        */
        StringVector splitCopy(const String& s, const String& separator = " ,\t\n");

        /**
            @brief 分割字符串
        */
        void split(StringVector& v, const String& s, const String& separator = " ,\t\n");
    }
}