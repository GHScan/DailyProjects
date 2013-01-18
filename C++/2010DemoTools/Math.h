#pragma once

#include <cmath>
#include <cassert>

#include <limits>

#include "Types.h"

namespace Scan
{
    namespace Math
    {
        /**
            @brief ��ά����
        */
        struct Vector3
        {
            float x, y, z;

            static const Vector3 UNIT_X; /**< X�� */
            static const Vector3 UNIT_Y; /**< Y�� */
            static const Vector3 UNIT_Z; /**< Z�� */
            static const Vector3 ZERO; /**< �� */

            Vector3(){}

            explicit Vector3(float f):
            x(f), y(f), z(f){}

            Vector3(float _x, float _y, float _z):
            x(_x), y(_y), z(_z){}

            Vector3(float *p):
            x(p[0]), y(p[1]), z(p[2]){}

            ~Vector3() {}

            float* getPtr()
            {
                return &x;
            }

            const float* getPtr() const
            {
                return &x;
            }

            float operator [] (uint32 index) const
            {
                assert(index < 3 && index >= 0);
                return getPtr()[index];
            }

            float& operator [] (uint32 index)
            {
                assert(index < 3 && index >= 0);
                return getPtr()[index];
            }

            /**
                @brief ������������
            */
            float getLength() const
            {   
                return std::sqrt(getLengthSquared());
            }

            /**
                @brief �����������ȵ�ƽ��
            */
            float getLengthSquared() const
            {
                return x * x + y * y + z * z;
            }

            /**
                @brief ���㵽��һ����ľ���
            */
            float distance(const Vector3& o) const
            {
                return (Vector3(*this) -= o).getLength();
            }

            /**
                @brief ���
            */
            float dotProduct(const Vector3& o) const
            {
                return x * o.x  + y * o.y + z * o.z;
            }

            /**
                @brief ���
            */
            Vector3 crossProduct(const Vector3& o) const
            {
                return Vector3(
                    y * o.z - o.y * z,
                    z * o.x - o.z * x,
                    x * o.y - o.x * y);
            }

            /**
                @brief ��׼��
            */
            float normalise()
            {
                static float EPSILON = std::numeric_limits<float>::epsilon();
                float len = getLength();
                if (len > EPSILON)
                {
                    x /= len, y /= len, z /= len;
                }
                return len;
            }

            bool operator == (const Vector3& o) const
            {
                return x == o.x && y == o.y && z == o.z;
            }

            bool operator != (const Vector3& o) const
            {
                return !(*this == o);
            }

            Vector3& operator += (const Vector3& o)
            {
                x += o.x; y += o.y; z += o.z;
                return *this;
            }

            Vector3& operator -= (const Vector3& o)
            {
                x -= o.x; y -= o.y; z -= o.z;
                return *this;
            }

            Vector3& operator *= (const Vector3& o)
            {
                x *= o.x; y *= o.y; z *= o.z;
                return *this;
            }

            Vector3& operator /= (const Vector3& o)
            {
                x /= o.x, y /= o.y, z /= o.z;
                return *this;
            }
        };

        inline Vector3 operator + (const Vector3& lhs, const Vector3& rhs)
        {
            return Vector3(lhs) += rhs;
        }

        inline Vector3 operator - (const Vector3& lhs, const Vector3& rhs)
        {
            return Vector3(lhs) -= rhs;
        }

        inline Vector3 operator * (const Vector3& lhs, const Vector3& rhs)
        {
            return Vector3(lhs) *= rhs;
        }

        inline Vector3 operator / (const Vector3& lhs, const Vector3& rhs)
        {
            return Vector3(lhs) /= rhs;
        }

        inline OStream& operator << (OStream& os, const Vector3& v3)
        {
            os << "<Vector3 x=\"" << v3.x << "\" y=\"" << v3.y << "\" z=\"" << v3.z << "\" />";
            return os;
        }
        inline IStream& operator >> (IStream& is, Vector3& v3)
        {
            is.ignore(12); is >> v3.x;
            is.ignore(5); is >> v3.y;
            is.ignore(5); is >> v3.z;
            is.ignore(4);
            return is;
        }

        /**
            @brief ��ά����
        */
        struct Matrix33
        {   
            static const Matrix33 IDENTITY; /**< ��λ���� */

            Matrix33(){}

            Matrix33(const Vector3& _x, const Vector3& _y, const Vector3& _z):
            x(_x), y(_y), z(_z){}

            Vector3 x, y, z;
        };
    }
}