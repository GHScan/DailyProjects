# What things we will do?
1. Two turn of parsing
    + Turn 1: For struct, add new named type to type system, and create private
    symbol table for each struct; For global variables, create new entry of global
    symbol table; For functions, add entry to global symbol table, and build the
    abstract syntax tree(While building the AST, we will not do the type
    checking and symbol table access.)
    + Trun 2: For each function, emit the byte code base on the AST, and perform
    the semantic analysis (type checking and nested symbol table access) at the same
    time
2. Runtime for strong type system
3. Type system 
    + Buildin type "int", "char", "char * "
    + Struct
    + Pointer
    + Array
    + Implicit type conversion
    + Explicit type conversion
4. Grammar
    + Operator &
    + Operator *
    + Operator ./->
    + Array[exp]
    + Buildin function sizeof
    + Switch. The case expression will be int only
    + Initialize while defination(e.g : "int i = 0, j = 1;")
5. Symbol table.
    + Symbol description including type, index, offset
    + Each struct has a table, and global envrionment also do.
    + Symbol table chain, used inside function.
    
# What things we will do in the next version (+1)? 
1. Type system
    + Operator overload.(We must consider the alias of function name.)
2. Grammar
    + Switch: The case expression can be literal.
3. Byte code (Tripple-address-code)
4. Emit the byte code in a turn of parsing. So we will not see the AST
