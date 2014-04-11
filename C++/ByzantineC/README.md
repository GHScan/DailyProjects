ByzantineC
==========

A general purpose c programming framework

NOTES:

This project has faild...
Because of the lack of type information in C...
All the difficults i struggled with including:
    @ Generic programming: Lack of alignment information, so ADT could not to have a high efficency memory layout as c++ templates, of cause you can use macros, but it is another maintenance nightmare.
    @ Polymorphism: You have to do simulation as c++ object model, it is hard to use and maintain
    @ Exception: There's not a mechanism like RAII or GC, so after you simulate the exception mechanism through setjmp/longjmp you have to face the problem about resource leaks
    @ Naming hell: Because of the lack of namespace support, you have to name your variable as library.module.class.method, it's to long to use. Hard to write, hard to read
    @ Memory allocator: Still lack of alignemnt information for special type
    @ Inline function: All you have are macros, but they are dangerous because of the side effect of expression

The finaly reason make me give up is:
    @ Memory allocator is too hard to write because of the lack of alignment information
    @ Interface and implemention class have become too complex, it's disgusting
