row->col means implement col with row:

[-]                 yield           coroutine           call/cc         async/await         exception           amb
yield               -               ugly                X               ugly                X                   X
coroutine           good            -                   X               good                X                   X
call/cc             perfect         good                -               perfect
cps-transform
cps-interpreter
