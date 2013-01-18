#pragma once

#include "Types.h"

namespace Scan
{
    /**
        @brief 获得当前应用程序的完整路径名
    */
    String getAppFullName();

    /**
        @brief 获得当前应用程序的名称
    */
    String getAppName();

    /**
        @brief 分割一个完整文件路径
        @param driver 驱动器
        @param dir 文件夹
        @param title 文件名
        @param ext 后缀名
    */
    void splitFileFullName(
        const String& fullName,
        String& driver,
        String& dir,
        String& title,
        String& ext);

    /**
        @brief 文件是否存在
    */
    bool isFileExist(const String& name);
    bool deleteFile(const String& name);
    bool copyFile(const String& newName, const String& oldName);

    /**
        @brief 创建一个文件夹
        @param recursive 是否递归创建文件夹
    */
    bool createDirectory(const String& name, bool recursive = true);
    bool deleteDirectory(const String& name);
}
