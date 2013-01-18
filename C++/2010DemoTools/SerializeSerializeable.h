#pragma once

#include <boost/shared_ptr.hpp>

#include "Types.h"

namespace Scan
{
    namespace Serialize
    {
        struct ISerializeFile;

        /**
            @brief ���϶����л��ӿ�
        */
        struct ISerializeable
        {
        public:
            /**
                @brief ��İ汾
                �����Ѿ���������, �����汾�Ķ�����Ĵ��л�������, ��Ҫ�޸�����汾��
            */
            virtual uint16 getClassVersion() const 
            { 
                return 1; 
            }

            /**
                @brief ���л�����
                ����������д��л��򵥶��󡢸��϶���������
            */
            virtual void onSerialzie(ISerializeFile&) = 0;

            virtual ~ISerializeable() = 0 {}
        };    
        typedef boost::shared_ptr<ISerializeable>   SerializeablePtr;
    }
}