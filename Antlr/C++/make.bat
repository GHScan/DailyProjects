@echo off
set PATH=E:\MinGW\bin;%PATH%
rem call "%VS80COMNTOOLS%vsvars32.bat" >nul
rem call "%VS90COMNTOOLS%vsvars32.bat" > nul
call "%VS110COMNTOOLS%vsvars32.bat" > nul
make.exe %*
