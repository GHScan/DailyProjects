#pragma once

#include "Types.h"

namespace Scan
{
    /**
        @brief ��õ�ǰӦ�ó��������·����
    */
    String getAppFullName();

    /**
        @brief ��õ�ǰӦ�ó��������
    */
    String getAppName();

    /**
        @brief �ָ�һ�������ļ�·��
        @param driver ������
        @param dir �ļ���
        @param title �ļ���
        @param ext ��׺��
    */
    void splitFileFullName(
        const String& fullName,
        String& driver,
        String& dir,
        String& title,
        String& ext);

    /**
        @brief �ļ��Ƿ����
    */
    bool isFileExist(const String& name);
    bool deleteFile(const String& name);
    bool copyFile(const String& newName, const String& oldName);

    /**
        @brief ����һ���ļ���
        @param recursive �Ƿ�ݹ鴴���ļ���
    */
    bool createDirectory(const String& name, bool recursive = true);
    bool deleteDirectory(const String& name);
}
