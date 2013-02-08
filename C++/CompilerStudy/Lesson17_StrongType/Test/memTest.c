
void strcpy(char *d, char *s)
{
    *d = *s;
    while (*++d = *++s);
}
void strcat(char *d, char *s)
{
    assert(*d);
    while (*++d);
    strcpy(d, s);
}
int strcmp(char *s, char *d)
{
    if (s[0] != d[0]) return s[0] - d[0];
    while (*++s == *++d && *s);
    return *s - *d;
}

int main()
{
    char *buf = (char*)malloc(128);

    strcpy(buf, "id1=%d, id2=%d, ");
    printf(buf, 1, 2);
    printf("\n");

    strcat(buf, "id3=%d\n");
    printf(buf, 3, 4, 5);

    free(buf);

    printf("strcmp: %d, %d, %d",
            strcmp("", "abcd"),
            strcmp("abcd", "abcd"),
            strcmp("adcd", "abcd"));
}
