#include "pch.h" 

#include <readline/readline.h>
#include <readline/history.h>

static bool mygetline(string &line, const string& prompt) {
    printf("%s", prompt.c_str());
    return getline(cin, line), cin;
}
static bool myreadline(string &line, const string& prompt) {
    char *p = readline(prompt.c_str());
    if (p == NULL) return false;
    line = p;
    free(p);
    if (!line.empty()) add_history(line.c_str());
    return true;
}

int main(int argc, char *argv[]) {
    if (argc == 1) {
        printf("%s readline/getline\n", argv[0]);
        return 0;
    }

    if (argv[1] == string("readline")) {
        puts("@readline:");
        for (string line; myreadline(line, ">>"); ) {
            printf("your input:%s\n", line.c_str());
        }
    } else {
        puts("@getline:");
        for (string line; mygetline(line, ">>"); ) {
            printf("your input:%s\n", line.c_str());
        }
    }
}
