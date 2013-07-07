@echo off

call "%VS110COMNTOOLS%vsvars32.bat" > nul

set srcfile=%1

main.exe %*
E:\Library\nasm\nasm.exe -f win32 %srcfile%.asm -o %srcfile%.obj
cp %srcfile%.asm _%srcfile%.asm

echo extern "C" void main(); int cmain(){main(); return 0;} > cmain.cpp
cl.exe /nologo /W3 /DWIN32 /D_CONSOLE /EHsc /fp:precise /GR /Zc:wchar_t /Zc:forScope /Gd /TP /DNDEBUG /O2 /GL /MD /Zi /c cmain.cpp 

set libs=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib
link.exe /NOLOGO /DEBUG /MACHINE:X86 /MANIFEST /INCREMENTAL:NO /OPT:REF /OPT:ICF /LTCG /entry:cmain cmain.obj %srcfile%.obj /OUT:%srcfile%.exe %libs%

%srcfile%.exe

rm %srcfile%.*
rm cmain.*
