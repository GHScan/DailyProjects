
#include <stdio.h>

#define function(ret, funcName, ...) struct __args_##funcName{__VA_ARGS__}; ret funcName(struct __args_##funcName args)
#define call(funcName, ...) func((struct __args_##funcName){__VA_ARGS__})

function(void, func, const char *name; int age;)
{
    printf("name:%s\n age:%d\n", args.name, args.age);
}

int main()
{
    call(func, "aa123", 10);
    call(func, .age = 5, .name = "abc456");
    call(func, .name = "def789");
    call(func, .age = 11);
}
