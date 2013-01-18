#pragma once

#include <utility>

#include <boost/type_traits.hpp>

#include "Preprocessor.h"

namespace Scan
{
    namespace Serialize
    {
        struct ISerializeFile;

#define _SCAN_COMPOSITE_BEGIN(compositeT, n) \
        template<SCAN_PP_REPEAT_TYPENAME_TYPE_COMMA(n)> \
        inline void serializeContentHook(ISerializeFile &f, compositeT<SCAN_PP_REPEAT_TYPE_COMMA(n)> &p) \
        {

#define _SCAN_COMPOSITE_ELEMENT(n, elem)  \
        f & SCAN_SERIALIZE_NVP(SCAN_PP_TO_STRING(elem), const_cast<boost::remove_cv<SCAN_PP_CAT(Type, n)>::type&>(p.elem));
            
#define _SCAN_COMPOSITE_END()        }

        
        /**
            @brief  µœ÷std::pair
        */
        _SCAN_COMPOSITE_BEGIN(std::pair, 2)
        _SCAN_COMPOSITE_ELEMENT(0, first)
        _SCAN_COMPOSITE_ELEMENT(1, second)
        _SCAN_COMPOSITE_END()

#undef _SCAN_COMPOSITE_END
#undef _SCAN_COMPOSITE_ELEMENT
#undef _SCAN_COMPOSITE_BEGIN
    }
}