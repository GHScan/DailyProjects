@echo off
REM 把这个.bat放在test.lua文件夹下

REM 设置当前路径
set base_dir=%~dp0 
%base_dir:~0,2% 
cd %base_dir% 

REM 执行
lua test.lua %1

REM 暂停
pause >> nul