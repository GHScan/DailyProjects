#pragma once

namespace Scan
{
    namespace Serialize
    {
        /**
            @brief ��-ֵ��
            ��Ϊ��Ҫֵ������, ���Բ���std::pair; ͬʱҲ��Ϊ����ȷ�����û���
            std::pair<String, int*>����ֵ�����
        */
        template<typename ValueType>
        struct NVPair
        {
            NVPair(const char *_name, ValueType& _obj):
            name(_name), obj(_obj){}
            const char *name;
            ValueType  &obj;
        };

        /**
            @brief ����һ����-ֵ��
        */
        template<typename ValueType>
        inline NVPair<ValueType> makePair(const char* name, ValueType& v)
        {
            return NVPair<ValueType>(name, v);
        }

        /**
            @brief ����һ������-ֵ��
            ������xml��Ҫ�����ĸ�ʽ
        */
#define SCAN_SERIALIZE_NVP(name, obj)   Scan::Serialize::makePair(name, obj)
#define SCAN_SERIALIZE_NVP_AUTO(obj)    Scan::Serialize::makePair(SCAN_PP_NV(obj))
    }
}