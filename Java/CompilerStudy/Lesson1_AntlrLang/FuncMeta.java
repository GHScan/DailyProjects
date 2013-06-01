class FuncMeta {
    public FuncMeta(String _name, int _argCount, StmtNode _stmt) {
        name = _name; argCount = _argCount; stmt = _stmt;
    }
    public String name;
    public int argCount;
    public StmtNode stmt;
}
