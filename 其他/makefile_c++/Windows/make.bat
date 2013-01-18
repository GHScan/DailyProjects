@echo off
set PATH=F:\MinGW\bin;%PATH%
call "%VS80COMNTOOLS%vsvars32.bat" >nul
rem call "%VS90COMNTOOLS%vsvars32.bat" > nul
rem call "%VS100COMNTOOLS%vsvars32.bat" > nul
make.exe %*
