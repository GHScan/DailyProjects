#include "pch.h"

#include <assert.h>

#include <unistd.h>

static string cmdRunAndRetrieve(const char *_cmd, const char *input) {
    string cmd = _cmd;
    vector<char*> args;
    {
        char *saveptr;
        for (char *p = (char*)cmd.c_str(); ; p = NULL) {
            char *token = strtok_r(p, " ", &saveptr);
            if (token) args.push_back(token);
            else break;
        }
        args.push_back(NULL);
    }
    if (args.size() < 2) return "";
            
    int readFds[2], writeFds[2];
    pipe(readFds);
    pipe(writeFds);

    if (fork() == 0) {
        dup2(writeFds[0], STDIN_FILENO);
        dup2(readFds[1], STDOUT_FILENO);
        close(writeFds[1]);
        close(readFds[0]);
        execvp(args[0], &args[0]);
        assert(0);
    }

    close(readFds[1]);
    close(writeFds[0]);

    int inputlen = strlen(input);
    if (inputlen > 0) {
        write(writeFds[1], input, inputlen);
    }
    close(writeFds[1]);

    string ret;
    char buf[512];
    for (int n = 0; (n = read(readFds[0], buf, sizeof(buf) - 1)) > 0; ) {
        buf[n] = 0;
        ret += buf;
    }
    close(readFds[0]);

    return ret;
}

int main() {
    cout << cmdRunAndRetrieve("ls -l", "") << endl;
    puts("******************************");
    cout << cmdRunAndRetrieve("grep -o -P \\d+", "abc\ndef\n123\nabc456\n") << endl;
}
