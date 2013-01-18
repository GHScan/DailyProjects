#pragma once

#if defined(DEBUG) || defined(_DEBUG)

/**
    @brief 提供值
    可用于对编译配置类型的条件判断等
*/
#define IS_DEBUG     1

/**
    @see IS_DEBUG
*/
#define IS_RELEASE   0

/**
    @brief 提供编译配置字符串
    可用于连接字符串从而进一步应用
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
    @brief 在支持__FUNCTION__宏的平台上, SCAN_FUNCTION能够返回当前函数
*/
#if defined(__FUNCTION__)
#define SCAN_FUNCTION __FUNCTION__
#else
#define SCAN_FUNCTION ""
#endif