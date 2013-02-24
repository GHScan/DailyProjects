#include "buddy.h"

#include <stdio.h>
#include <string.h>

int main()
{
    struct buddy *mem = buddy_new(10);
    char cmd[32] = "";
    int arg0 = 0;
    while (scanf("%s %d", cmd, &arg0)) {
        printf("command : %s %d\n", cmd, arg0);
        if (strcmp(cmd, "alloc") == 0) {
            printf("-> %d\n", buddy_alloc(mem, arg0));
            buddy_dump(mem);
        }
        else if (strcmp(cmd, "free") == 0) {
            buddy_free(mem, arg0);
            buddy_dump(mem);
        }
        else if (strcmp(cmd, "size") == 0) {
            printf("->%d\n", buddy_size(mem, arg0));
        }
        else if (strcmp(cmd, "dump") == 0) {
            buddy_dump(mem);
        }
        else {}
    }
    buddy_delete(mem);
}
