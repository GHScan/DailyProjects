@echo off
rem call "%VS90COMNTOOLS%vsvars32.bat" > nul
call "%VS110COMNTOOLS%vsvars32.bat" > nul
make.exe %*
