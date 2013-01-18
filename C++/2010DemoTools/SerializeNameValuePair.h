#pragma once

namespace Scan
{
    namespace Serialize
    {
        /**
            @brief 名-值对
            因为需要值的引用, 所以不用std::pair; 同时也是为了正确处理用户把
            std::pair<String, int*>当做值的情况
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
            @brief 创建一个名-值对
        */
        template<typename ValueType>
        inline NVPair<ValueType> makePair(const char* name, ValueType& v)
        {
            return NVPair<ValueType>(name, v);
        }

        /**
            @brief 创建一个名字-值对
            尤其是xml需要这样的格式
        */
#define SCAN_SERIALIZE_NVP(name, obj)   Scan::Serialize::makePair(name, obj)
#define SCAN_SERIALIZE_NVP_AUTO(obj)    Scan::Serialize::makePair(SCAN_PP_NV(obj))
    }
}