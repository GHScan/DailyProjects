@echo off
REM �����.bat����VCTempCleaner.lua�ļ�����

REM ���õ�ǰ·��
set base_dir=%~dp0 
%base_dir:~0,2% 
cd %base_dir% 

REM ִ��
lua VCTempCleaner.lua %1

REM ��ͣ
pause >> nul