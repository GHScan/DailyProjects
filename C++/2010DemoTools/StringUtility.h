#pragma once

#include <boost/algorithm/string.hpp>

#include "Types.h"

namespace Scan
{
    namespace StringUtil
    {
        /**
            @brief UTF8�ַ���
        */
        struct UTF8String
        { 
            String s; 
        };

        /**
            @brief ��UTF8�ַ���ת��ΪANSI�ַ���
        */
        String convertToString(const UTF8String& src);

        /**
            @brief ��Unicode�ַ���ת��ΪANSI�ַ���
        */
        String convertToString(const WString& src);

        /**
            @brief ��UTF8�ַ���ת��ΪUnicode�ַ���
        */
        WString convertToWString(const UTF8String& src);

        /**
            @brief ��ANSI�ַ���ת��ΪUnicode�ַ���
        */
        WString convertToWString(const String& src);

        /**
            @brief ��ANSI�ַ���ת��ΪUTF8�ַ���
        */
        UTF8String convertToUTF8String(const String& src);

        /**
            @brief ��Unicode�ַ���ת��ΪUTF8�ַ���
        */
        UTF8String convertToUTF8String(const WString& src);

        /**
            @brief ����һ���ַ�����д�Ŀ���
        */
        inline String toUpperCopy(const String& s)   
        {
            return boost::algorithm::to_upper_copy(s);
        }

        /**
            @brief ��һ���ַ���ת��Ϊ��д
        */
        inline void toUpper(String& s)
        {
            boost::algorithm::to_upper(s);
        }

        /**
            @brief ����һ��Сд�Ŀ���
        */
        inline String toLowerCopy(const String& s)
        {
            return boost::algorithm::to_lower_copy(s);
        }

        /**
            @brief ���ַ���ת��ΪСд
        */
        inline void toLower(String &s)
        {
            boost::algorithm::to_lower(s);
        }

        /**
            @brief ����ȥ������߿հ׵Ŀ�����
        */
        inline String trimLeftCopy(const String& s)
        {
            return boost::algorithm::trim_left_copy(s);
        }

        /**
            @brief ȥ���ַ�����ߵĿհ�
        */
        inline void trimLeft(String& s)
        {
            boost::algorithm::trim_left(s);
        }

        /**
            @brief ����ȥ�����ұ߿հ׵Ŀ�����
        */
        inline String trimRightCopy(const String& s)
        {
            return boost::algorithm::trim_right_copy(s);
        }

        /**
            @brief ȥ���ұߵĿհ�
        */
        inline void trimRight(String& s)
        {
            boost::algorithm::trim_right(s);
        }

        /**
            @brief ����ȥ�����ҿհ׵Ŀ�����
        */
        inline String trimCopy(const String& s)
        {
            return boost::algorithm::trim_copy(s);
        }

        /**
            @brief ȥ���������ҿհ�
        */
        inline void trim(String& s)
        {
            boost::algorithm::trim(s);
        }

        /**
            @brief �Ƿ���һ��ǰ׺
            @param s Ҫ���Ե��ַ���
            @param prefix ǰ׺��
            @param isCaseSensitive �Ƿ��Сд����
        */
        inline bool isStartWith(const String& s, const String& prefix, bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::starts_with(s, prefix) : 
            boost::algorithm::istarts_with(s, prefix);
        }

        /**
            @brief �Ƿ���һ����׺
        */
        inline bool isEndWith(const String& s, const String& postfix, bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::ends_with(s, postfix) :
            boost::algorithm::iends_with(s, postfix);
        }

        /**
            @brief �Ƿ����һ���Ӵ�
        */
        inline bool isContain(const String& s, const String& sub, bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::contains(s, sub) :
            boost::algorithm::icontains(s, sub);
        }

        /**
            @brief �Ƚ�������
        */
        inline bool isEqual(const String& s, const String& test, bool isCaseSensitive = true)
        {
            return isCaseSensitive ? 
            boost::algorithm::equals(s, test) :
            boost::algorithm::iequals(s, test);    
        }

        /**
            @brief ����һ���Ѿ��滻��һ����һ�γ��ֵ��Ӵ��Ŀ�����
            @param oldSub Ҫ�滻�Ĳ���
            @param newSub �滻�ɵĲ���
            @return �滻���Ŀ�����
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
            @brief �滻�ַ����е�һ����һ�γ��ֵ��ִ�
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
            @brief ����һ��ɾ����һ����һ�γ��ֵ��ִ��Ŀ�����
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
            @brief ɾ���ַ�����һ���ִ���һ�γ��ֵĲ���
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
            @brief �����滻���ַ�����ĳ�ִ����һ�γ��ֵĲ��ֵĿ�����
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
            @brief �滻�ַ�����ĳ�ִ������һ�γ���
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
            @brief �����ַ���ɾ��ĳ�ִ����һ�γ��ֵĿ�����
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
            @brief ɾ���ַ�����ĳ�ִ������һ�γ���
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
            @brief ����һ���滻���ַ���������ĳ�ִ���Ŀ���
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
           @brief �滻�ַ���������ĳ�ִ�
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
            @brief ����ɾ�����ַ���������ĳ�ִ���Ŀ���
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
            @brief ɾ���ַ���������ĳ�ִ�
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
            @brief �Ƚ������ַ����Ĵ�С
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
            @brief �����ַ����ָ�Ŀ���
        */
        StringVector splitCopy(const String& s, const String& separator = " ,\t\n");

        /**
            @brief �ָ��ַ���
        */
        void split(StringVector& v, const String& s, const String& separator = " ,\t\n");
    }
}