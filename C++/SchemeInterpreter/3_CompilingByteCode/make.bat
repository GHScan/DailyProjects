@echo off
set PATH=F:\MinGW\bin;%PATH%
rem call "%VS90COMNTOOLS%vsvars32.bat" > nul
call "%VS120COMNTOOLS%vsvars32.bat" > nul
make.exe %*
