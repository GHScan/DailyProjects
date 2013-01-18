#pragma once

#if defined(DEBUG) || defined(_DEBUG)

/**
    @brief �ṩֵ
    �����ڶԱ����������͵������жϵ�
*/
#define IS_DEBUG     1

/**
    @see IS_DEBUG
*/
#define IS_RELEASE   0

/**
    @brief �ṩ���������ַ���
    �����������ַ����Ӷ���һ��Ӧ��
*/
#define COMPILE_CONFIG_STRING     "debug"

#else

/**
    @see IS_DEBUG
*/
#define IS_DEBUG     0

/**
    @see IS_DEBUG
*/
#define IS_RELEASE   1

/**
    @see COMPILE_CONFIG_STRING
*/
#define COMPILE_CONFIG_STRING     "release"

#endif

/**
    @brief ��֧��__FUNCTION__���ƽ̨��, SCAN_FUNCTION�ܹ����ص�ǰ����
*/
#if defined(__FUNCTION__)
#define SCAN_FUNCTION __FUNCTION__
#else
#define SCAN_FUNCTION ""
#endif