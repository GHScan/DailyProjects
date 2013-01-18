// stdafx.h : 标准系统包含文件的包含文件，
// 或是经常使用但不常更改的
// 特定于项目的包含文件
//

#pragma once

#include <stdio.h>
#include <tchar.h>
#include <assert.h>

#include <iostream>
using std::cin;
using std::cout;
using std::endl;

// TODO: 在此处引用程序需要的其他头文件

#include <irrlicht.h>
using namespace irr;

#define WIN32_LEAN_AND_MEAN		// 从 Windows 头中排除极少使用的资料
#include <windows.h>
#undef min
#undef max