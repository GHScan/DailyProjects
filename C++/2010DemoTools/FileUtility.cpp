#include "StdAfx.h"

#include <io.h>

#include <windows.h>
#include <commdlg.h>
#include <direct.h>

#include "FileUtility.h"
#include "StringUtility.h"

#include "MemoryCheck.h"

namespace Scan
{
    String getAppFullName()
    {
        char buf[MAX_PATH] = "";
        GetModuleFileName(NULL, buf, sizeof(buf));
        return buf;
    }

    String getAppName()
    {
        char buf[MAX_PATH] = "";
        GetFileTitle(getAppFullName().c_str(), buf, sizeof(buf));
        if (char* p = strrchr(buf, '.'))
        {
            *p = 0;
        }
        return buf;
    }

    void splitFileFullName(const String& fullName,
        String& driver,
        String& dir,
        String& title,
        String& ext)
    {
        char sDriver[8] = "";
        char sDir[MAX_PATH] = "";
        char sTitle[MAX_PATH] = "";
        char sExt[16] = "";

        _splitpath_s(
            fullName.c_str(), 
            sDriver,
            sDir,
            sTitle,
            sExt);

        driver = sDriver;
        dir = sDir;
        title = sTitle;
        ext = sExt;
    }

    bool isFileExist(const String& name)
    {
        return _access(name.c_str(), 0) == 0;
    }

    bool deleteFile(const String& name)
    {
        return DeleteFile(name.c_str()) == TRUE;
    }

    bool deleteDirectory(const String& name)
    {
        return RemoveDirectory(name.c_str()) == TRUE;
    }

    bool copyFile(const String& newName, const String& oldName)
    {
        return CopyFile(oldName.c_str(), newName.c_str(), FALSE) == TRUE;
    }

    bool createDirectory(const String& name, bool recursive/* = true*/)
    {
        if (!recursive)
        {
            _mkdir(name.c_str());
            return true;
        }

        String driver, path, file, ext;
        splitFileFullName(name, driver, path, file, ext);
        StringVector v;
        StringUtil::split(v, path, "/\\");
        String validDir = driver;
        StringVector::iterator iter = v.begin();
        while (iter != v.end())
        {
            validDir += validDir.empty() ? "" : "/";
            validDir += *iter;
            _mkdir(validDir.c_str());
            ++iter;
        }
        validDir += validDir.empty() ? "" : "/" ;
        validDir += file;
        _mkdir(validDir.c_str());

        return true;
    }
}