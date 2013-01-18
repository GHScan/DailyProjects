#pragma once

#include <cassert>

#include <algorithm>

#include <boost/foreach.hpp>
#include <boost/mpl/if.hpp>
#include <boost/type_traits.hpp>
// #include <boost/typeof/typeof.hpp>

#include "Preprocessor.h"
#include "MetaProgram.h"
#include "DataStructure.h"
#include "Types.h"

#undef min
#undef max

namespace Scan
{
    /**
            @brief boost typeof
        */
//#define SCAN_AUTO           BOOST_AUTO
//#define SCAN_TYPEOF         BOOST_TYPEOF
//#define SCAN_REGISTER_TYPE  BOOST_TYPEOF_REGISTER_TYPE

        /**
            @brief foreach

            ��ʽ SCAN_FOR_EACH(r, c)
            ��ʽ SCAN_FOR_EACH_C(r, c)
        */
//#define _SCAN_FOREACH_VALUE_TYPE(c)   Scan::MPL::ContainerTraits<SCAN_TYPEOF(c), true>::ValueType      
//#define _SCAN_FOREACH_VALUE_TYPE_C(c) const Scan::MPL::ContainerTraits<SCAN_TYPEOF(c), true>::ValueType      
//#define _SCAN_FOREACH_VAR(i)  SCAN_CONN(i, __LINE__)     
//#define _SCAN_FOREACH(r, c, vType)  \
//    if (int __index = 0); else\
//    if (int _SCAN_FOREACH_VAR(counter) = 0); else\
//    for (Scan::DataStructure::Iterator<vType(c)> _SCAN_FOREACH_VAR(iter)(c); \
//    ++_SCAN_FOREACH_VAR(counter) == 1 && _SCAN_FOREACH_VAR(iter).isExistMore(); \
//    ++_SCAN_FOREACH_VAR(counter), ++__index)\
//    for (vType(c) &r = _SCAN_FOREACH_VAR(iter).getNext(); _SCAN_FOREACH_VAR(counter)--; )      
//#define SCAN_FOREACH(r, c) _SCAN_FOREACH(r, c, _SCAN_FOREACH_VALUE_TYPE)
//#define SCAN_FOREACH_C(r, c) _SCAN_FOREACH(r, c, _SCAN_FOREACH_VALUE_TYPE_C)

        /**
            @brief ����һ������������
            ��ʽ SCAN_TYPE_FOREACH(type r, c)
        */
#define SCAN_FOREACH   BOOST_FOREACH

        /**
            @brief ��������λ1�ĸ���
            @remarks ����TӦ����һ������
        */
        template<typename T>
        inline uint8 getBit1Count(T i)
        {
            SCAN_STATIC_ASSERT(boost::is_integral<T>::value);

            uint8 ret = 0;
            uint32 bitCnt = sizeof(T) * 8;
            while (i != 0)
            {
                ++ret;
                i = (i - 1) & i;
            }
            return ret;
        }

        /**
            @brief ����С���ֵ֮�����Բ�ֵ
            @param dis ���ֵռ�ı���
        */
        template<typename T>
        inline T lerp(T const &minV, T const &maxV, double dis)
        {
            assert(minV <= maxV);
            assert(dis <= 1 && dis >= 0);
            return minV + 
                (boost::mpl::if_<boost::is_pointer<T>, uint64, T>::type)(
                (maxV - minV) * dis);
        }

        /**
            @brief ������ֵ
        */
        template<typename T>
        inline T mid(T const &minV, T const &maxV)
        {
            assert(minV <= maxV);
            return lerp(minV, maxV, 0.5);
        }

        /**
            @brief ����һ��������С���ֵ�������λ��
            @param v Ҫ�������
        */
        template<typename T>
        inline float toUnit(T const &minV, T const &maxV, T const &v)
        {
            assert(minV <= maxV);
            if (v < minV)
            {
                return 0;
            }
            if (v > maxV)
            {
                return 1;
            }
            return float((v - minV) * 1.0 / (maxV - minV));
        }

        /** 
            @brief ��һ������λ����С���ֵ��Χ��
        */
        template<typename T>
        inline T& clamp(T& v, T const minV, T const maxV)
        {
            assert(minV <= maxV);
            v = v < minV ? minV : v;
            v = v > maxV ? maxV : v;
            return v;
        }

        /**
            @brief ������λ��[0, 1]֮��
        */
        inline float saturate(float v)
        {
            return clamp(v, 0.0f, 1.0f);
        }

        /**
            @brief ���������������
            ע�͵ĸ�ʽ��: ѭ������, �ڴ�����, ʱ��
        */
        namespace RandomGernatorTypes
        {
            // 2^31 - 2, sizeof(int32_t), 40 
            typedef boost::minstd_rand      RGT_minstd; 
            // 2^48 - 1, sizeof(uint64_t), 80 
            typedef boost::rand48           RGT_rand48;
            // 2^61, 2 * sizeof(int32_t), 20 
            typedef boost::ecuyer1988       RGT_ecuyer1988;
            // ?, 1368 * sizeof(uint32_t), 60 
            typedef boost::kreutzer1986     RGT_kreutzer1986;
            // 2^31 - 1, sizeof(int32_t), 3 
            typedef boost::hellekalek1995   RGT_hellekalek1995; // good!
            // 2^11213 - 1, 352 * sizeof(uint32_t), 100 
            typedef boost::mt11213b         RGT_mt11213b;   // good!
            // 2^19937 - 1, 625 * sizeof(uint32_t), 100 
            typedef boost::mt19937          RGT_mt19937;    // good! boost�Ƽ�
            // 2^32000, 607 * sizeof(double), 150 
            typedef boost::lagged_fibonacci607  RGT_lagged_fibonacci607;
            // 2^67000, 1279 * sizeof(double), 150 
            typedef boost::lagged_fibonacci1279 RGT_lagged_fibonacci1279;
            // 2^120000, 2281 * sizeof(double), 150 
            typedef boost::lagged_fibonacci2281 RGT_lagged_fibonacci2281;
            // 2^170000, 3217 * sizeof(double), 150 
            typedef boost::lagged_fibonacci3217  RGT_lagged_fibonacci3217;
            // 2^230000, 4423 * sizeof(double), 150 
            typedef boost::lagged_fibonacci4423 RGT_lagged_fibonacci4423;
            // 2^510000, 9689 * sizeof(double), 150 
            typedef boost::lagged_fibonacci9689 RGT_lagged_fibonacci9689;
            // 2^1050000, 19937 * sizeof(double), 150 
            typedef boost::lagged_fibonacci19937 RGT_lagged_fibonacci19937;
            // 2^1200000, 23209 * sizeof(double), 140 
            typedef boost::lagged_fibonacci23209 RGT_lagged_fibonacci23209;
            // 2^2300000, 44497 * sizeof(double), 60 
            typedef boost::lagged_fibonacci44497 RGT_lagged_fibonacci44497;
        }

        /**
            @brief �漴������������
        */
        template<typename RandomGeneratorType = RandomGernatorTypes::RGT_mt19937>
        class Random
        {
        public:
            /**
                @brief ��ʼ���漴��������
                @param seed �������
            */
            Random(uint32 seed = static_cast<uint32>(std::time(0))):
              m_engine(seed)
              {
              }

              /**
                @brief ���÷�����
              */
              void reset(uint32 seed = static_cast<uint32>(std::time(0)))
              {
                  m_engine.seed(seed);
              }

              /**
                @brief ���[0, 1]֮��������
              */
              float getFloat_01()
              {
                  return getFloat_01<float>();
              }

              /**
                @brief ���[0, 1]֮��ָ���������͵������
              */
              template<typename FloatType>
              FloatType getFloat_01()
              {
                  static boost::uniform_01<RandomGeneratorType, FloatType> distr(m_engine);
                  return distr();
              }

              /**
                @brief ���[min, max]֮��������
              */
              float getFloat(float min, float max)
              {
                  return getFloat<float>(min, max);
              }

              /**
                @brief ���[min, max]֮��ָ���������͵������
              */
              template<typename FloatType>
              FloatType getFloat(FloatType min, FloatType max)
              {
                  return boost::uniform_real<FloatType>(min, max)(m_engine);
              }

              /**
                @brief ���[min, max]��̬��Χ�������
                ԭ����˵Ч�ʱȶ�̬��Χ�Ը�
              */
              template<int min, int max>
              float getFloat()
              {
                  return getFloat<float, min, max>();
              }

              /**
                @brief ���[min, max]��̬��Χָ���������͵������
              */
              template<typename FloatType, int min, int max>
              FloatType getFloat()
              {
                  static boost::uniform_real<FloatType> distr(min, max);
                  return distr(m_engine);
              }

              /**
                @brief ���С��Χ[min, max]�����������
              */
              int getInt_small(int min, int max)
              {
                  return getInt_small<int>(min, max);
              }       

              /**
                @brief ���С��Χ[min, max]ָ���������͵������
              */
              template<typename IntType>
              IntType getInt_small(IntType min, IntType max)
              {
                  return boost::uniform_smallint<IntType>(min, max)(m_engine);
              }

              /**
                @brief ���С��̬��Χ[min, max]�����������
              */
              template<int min, int max>
              int getInt_small()
              {
                  return getInt_small<int, min, max>();
              }       

              /**
                @brief ���С��̬��Χ[min, max]ָ���������͵������
              */
              template<typename IntType, int min, int max>
              IntType getInt_small()
              {
                  static  boost::uniform_smallint<IntType> distr(min, max);
                  return distr(m_engine);
              }

              /**
                @brief ���[min, max]��Χ�������漴��
              */
              int getInt(int min, int max)
              {
                  return getInt<int>(min, max);
              }       

              /**
                @brief ���[min, max]��ָ���������͵������
              */
              template<typename IntType>
              IntType getInt(IntType min, IntType max)
              {
                  return boost::uniform_int<IntType>(min, max)(m_engine);
              }

              /**
                @brief ��þ�̬��Χ[min, max]�е����������
              */
              template<int min, int max>
              int getInt()
              {
                  return getInt<int, min, max>();
              }       

              /**
                @brief ��þ�̬��Χ[min, max]��ָ���������͵������
              */
              template<typename IntType, int min, int max>
              IntType getInt()
              {
                  static  boost::uniform_int<IntType> distr(min, max);
                  return distr(m_engine);
              }

        private:
            RandomGeneratorType  m_engine;
        };

        typedef Random<> RandomObject;

        /**
            @brief ȫ�������������
        */
        inline RandomObject& getGlobalRandomObject()
        {
            static RandomObject ls_random;
            return ls_random;
        }

        /**
            @brief ȫ�������������
        */
        inline RandomObject* getGlobalRandomObjectPtr() 
        { 
            return &getGlobalRandomObject(); 
        }

        /**
            @brief ��ָ������õ�<�ȽϷ�
        */
        template<typename T>
        struct DerefPtrLess:
            public std::binary_function<const T*, const T*, bool>
        {
            bool operator () (const T* l, const T *r) const
            {
                return *l < *r;
            }
        };
}