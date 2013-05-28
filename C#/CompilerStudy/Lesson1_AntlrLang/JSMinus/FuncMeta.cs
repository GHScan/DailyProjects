public class FuncMeta {
    public FuncMeta(string name, int argCount, StmtNode stmt) {
        Name = name; ArgCount = argCount; Stmt = stmt;
    }
    public string Name { get; private set; }
    public int ArgCount { get; private set; }
    public StmtNode Stmt { get; private set; }
}
