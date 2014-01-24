#include "pch.h" 

#include <string.h>

#include <sys/mman.h>
#include <fcntl.h>
#include <unistd.h>

static void test_allocExecutable() {
    printf("************** %s\n", __func__);

    int size = 1 << 12;
    char *p = (char*)mmap(NULL, size, PROT_EXEC | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
    {
        unsigned char code[] = {
            0xb8, 0x11, 0, 0, 0, // mov eax, 0x11
            0xc3,                // ret
        };
        memcpy(p, code, sizeof(code));
    }
    printf("ret = %d\n", ((int(*)())p)());
    munmap(p, size);
}
static void test_fileMapping() {
    printf("************** %s\n", __func__);
    int fd = open(__FILE__, O_RDONLY);
    int len = lseek(fd, 0, SEEK_END) + 1;
    char *p = (char*)mmap(NULL, len, PROT_READ, MAP_FILE | MAP_PRIVATE, fd, 0);
    puts(p);
    munmap(p, len);
    close(fd);
}

int main(int argc, char *argv[]) {
    test_allocExecutable();
    test_fileMapping();
}
