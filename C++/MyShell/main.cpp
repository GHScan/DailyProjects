#include "pch.h" 

#include <memory>
#include <map>

//==============================
struct AST_IORedirection {
    enum Type {
        Stdin, Stdout, StdErr, File,
    };
    Type type;
    string fileName;
};
struct AST_Process {
    virtual ~AST_Process(){}
};
typedef shared_ptr<AST_Process> AST_ProcessPtr;
struct AST_SimpleProcess: public AST_Process {
    string path;
    vector<string> args;
    AST_IORedirection redirections[3];
};
struct AST_Pipeline: public AST_Process {
    AST_ProcessPtr upstream;
    AST_ProcessPtr downstream;
};
struct AST_Subshell: public AST_Process {
    string cmd;
};
struct AST_Job {
    AST_ProcessPtr process;
};
typedef shared_ptr<AST_Job> AST_JobPtr;
//==============================
class CommandParser {
public:
    CommandParser(const char *cmd);
    vector<AST_JobPtr>& getASTJobs();
private:
    vector<AST_JobPtr> m_astJobs;
};
//==============================
struct Job {
    int jobID;
    int gid;
    vector<int> pids;
};
//==============================
class ASTJobInterpreter {
public:
    ASTJobInterpreter(const AST_JobPtr &astJob);
    Job* getJob();
private:
    Job m_job;
};
//==============================
class Shell {
public:
    void addJob(Job *job);
    void delJob(int jobID);
    void joinJob(int jobID);
    int getJobIDByGID(int pid);

private:
    void _handleSIGINT();
    void _handleSIGTSTP();
private:
    string m_cwd;
    map<string, string> m_env;
};
//==============================
static bool isSignalFound_SIGINT = false;
static bool isSignalFound_SIGTSTP = false;

static void handler_SIGINT() {
}
static void handler_SIGTSTP() {
}

static void installSignalHandlers() {
}
//==============================
int main() {
    return 0;
}
