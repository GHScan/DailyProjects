#pragma once

#include <boost/shared_ptr.hpp>

#include "Types.h"

namespace Scan
{
    namespace Serialize
    {
        struct ISerializeFile;

        /**
            @brief 复合对象串行化接口
        */
        struct ISerializeable
        {
        public:
            /**
                @brief 类的版本
                对于已经发布的类, 后续版本改动过类的串行化方法后, 需要修改这个版本号
            */
            virtual uint16 getClassVersion() const 
            { 
                return 1; 
            }

            /**
                @brief 串行化方法
                在这个函数中串行化简单对象、复合对象、容器等
            */
            virtual void onSerialzie(ISerializeFile&) = 0;

            virtual ~ISerializeable() = 0 {}
        };    
        typedef boost::shared_ptr<ISerializeable>   SerializeablePtr;
    }
}