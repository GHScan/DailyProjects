#include "stdafx.h"

#include <windows.h>

#include "StringUtility.h"
#include "CompileEnvironment.h"

#include "MemoryCheck.h"

namespace Scan
{
    namespace StringCode
    {
        /**
            @brief  ö�� CodePage
            ��ʾ���õĶ��ֽڱ�������.
        */
        enum CodePage
        {
            ACP     = CP_ACP, /**< ANSI ���� */
            UTF8    = CP_UTF8, /**< UTF-8 ���� */
            UTF7    = CP_UTF7, /**< UTF-7 ���� */ 
            MACCP   = CP_MACCP, /**< ƻ��ר�ñ��� */
        };

        /**
            @brief ��Unicodeת��Ϊ���ֽ��ַ���
            @param  pSrc    ��Ҫת����Unicode�ַ���
            @param  pDest   ת����Ķ��ֽڴ�
            @param  cp      ���ֽڵı�������
        */
        void wchars2chars(const WString* pSrc, String* pDest, CodePage cp)
        {
            int iRequireSize = 
                WideCharToMultiByte(
                cp, 
                0, 
                pSrc->c_str(), 
                static_cast<int>(pSrc->size()), 
                NULL,
                0,
                NULL,
                NULL);
            pDest->resize(iRequireSize);

            WideCharToMultiByte(
                cp, 
                0, 
                pSrc->c_str(), 
                static_cast<int>(pSrc->size()), 
                const_cast<char*>(pDest->c_str()),
                static_cast<int>(pDest->size()),
                NULL,
                NULL);
        }

        /**
            @brief �����ֽ�ת��ΪUnicode�ַ���
            @param  pSrc    ��Ҫת���Ķ��ֽ��ַ���
            @param  pDest   ת�����Unicode��
            @param  cp      ���ֽڵı�������
        */
        void chars2wchars(const String* pSrc, WString* pDest, CodePage cp)
        {
            int iRequireSize = 
                MultiByteToWideChar(
                cp, 
                0,
                pSrc->c_str(),
                static_cast<int>(pSrc->size()),
                NULL,
                0);
            pDest->resize(iRequireSize);

            MultiByteToWideChar(
                cp,
                0,
                pSrc->c_str(),
                static_cast<int>(pSrc->size()),
                const_cast<wchar*>(pDest->c_str()),
                static_cast<int>(pDest->size()));
        }
    }

    namespace StringUtil
    {
        String convertToString(const UTF8String& src)
        {
            WString wszTmp = convertToWString(src);
            String szTmp = convertToString(wszTmp);

            return szTmp;
        }

        String convertToString(const WString& src)
        {
            String szTmp;

            StringCode::wchars2chars(&src, &szTmp, StringCode::ACP);

            return szTmp;
        }

        WString convertToWString(const UTF8String& src)
        {
            WString wszTmp;

            StringCode::chars2wchars(
                &src.s,
                &wszTmp,
                StringCode::UTF8);

            return wszTmp;
        }

        WString convertToWString(const String& src)
        {
            WString wszTmp;

            StringCode::chars2wchars(
                &src,
                &wszTmp,
                StringCode::ACP);

            return wszTmp;
        }       

        UTF8String convertToUTF8String(const String& src)
        {
            WString wszTmp = convertToWString(src);
            UTF8String szTmp = convertToUTF8String(wszTmp);

            return szTmp;
        }

        UTF8String convertToUTF8String(const WString& src)
        {
            UTF8String szTmp;

            StringCode::wchars2chars(
                &src, 
                &szTmp.s,
                StringCode::UTF8);

            return szTmp;
        }

        void split(StringVector& v, const String& s, const String& separator)
        {
            String sTmp(s);
            char *p = NULL;
            const char *sep = separator.c_str();
            char *tokenHelper = NULL;

            p = strtok_s(
                const_cast<char*>(sTmp.c_str()), 
                sep, 
                &tokenHelper);
            
            while (p != NULL)
            {
                v.push_back(p);
                
                p = strtok_s(
                    NULL,
                    sep, 
                    &tokenHelper);
            }
        }

        StringVector splitCopy(const String& s, const String& separator)
        {
            StringVector v;
            split(v, s, separator);
            return v;
        }
    }
}