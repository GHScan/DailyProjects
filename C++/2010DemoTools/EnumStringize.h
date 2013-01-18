#pragma once

#include <cassert>

#include <map>

#include "Singleton.h"
#include "Types.h"
#include "Preprocessor.h"

namespace Scan
{
    /**
        @brief ��ö��ת�����ַ���
        @remarks ��ö��ֵ������SCAN_REGISTER_ENUMSTRINGע���
    */
    template<typename EnumType>
    inline const String& enumToString(EnumType e)
    {
        return EnumStringTable<EnumType>::getSingletonPtr()->toString(e);
    }

    /**
        @brief �������ַ�������һ��ö��
        @remarks ��ö��ֵ������SCAN_REGISTER_ENUMSTRINGע���
    */
    template<typename EnumType>
    inline bool enumFromString(EnumType &e, const String& s)
    {
        return EnumStringTable<EnumType>::getSingletonPtr()->fromString(e, s);
    }

    /**
        @brief ע��һ��ö��
    */
#define SCAN_REGISTER_ENUMSTRING(e) \
    namespace EnumStringRegisterSpace \
    { \
        static Scan::EnumStringRegister SCAN_PP_CAT(g_register, __LINE__)(e, SCAN_PP_TO_STRING(e)); \
    }

    /**
        @brief ö���ַ���ӳ���
    */
    template<typename EnumType>
    class EnumStringTable:
        public Singleton<EnumStringTable<EnumType>>
    {
    public:
        /**
            @brief ��ö��ת��Ϊ�ַ���
        */
        const String& toString(EnumType e) const
        {
            assert(m_enumStrings.count(e) != 0);
            return m_enumStrings.find(e)->second;
        }

        /**
            @brief ���ַ����ָ�ö��
        */
        bool fromString(EnumType &e, const String& s) const
        {
            StringEnumMap_CIter iter = m_stringEnums.find(s);
            if (iter != m_stringEnums.end())
            {
                e = EnumType(iter->second);
                return true;
            }
            return false;
        }   

    private:
        friend struct ClassAllocator<EnumStringTable<EnumType>>;
        EnumStringTable(){}
        ~EnumStringTable(){}

    private:
        /**
            @brief ��ֹ����
        */
        EnumStringTable(const EnumStringTable&);
        EnumStringTable& operator = (const EnumStringTable&);

    private:
        friend class EnumStringRegister;
        void addItem(EnumType e, const String& s)
        {
            assert(m_enumStrings.count(e) == 0);
            m_enumStrings[e] = s;
            m_stringEnums[s] = e;
        }

    private:
        typedef std::map<int, String>           EnumStringMap;
        typedef std::map<String, int>           StringEnumMap;
        typedef StringEnumMap::const_iterator   StringEnumMap_CIter;

    private:
        EnumStringMap   m_enumStrings;
        StringEnumMap   m_stringEnums;
    };

    /**
        @brief ö���ַ���ע����
    */
    class EnumStringRegister
    {
    public:
        template<typename EnumType>
        EnumStringRegister(EnumType t, const String& s)
        {
            EnumStringTable<EnumType>::getSingletonPtr(true)->addItem(t, s);
        }
    };
};