# What things we will do?
1. Abstract syntax tree is used for execute. (Won't emit the byte code)
2. Runtime for strong type system
3. Type system 
    + Buildin type "int", "char", "char * "
    + Pointer
    + Array
    + Struct
    + Implicit type conversion
    + Explicit type conversion
4. Grammar
    + Operator &
    + Operator *
    + Operator ./->
    + Buildin function sizeof
    + Switch. The case expression will be int only
5. Symbol table.
    + Symbol description including type, index, offset
    + Each struct has a table, and global envrionment also do.
    + Symbol table chain, used inside function.
    
# What things we will do in the next version (+1)? 
1. Type system
    + Operator overload.(We must consider the alias of function name.)(Versoin +1)
2. Byte code
3. Grammar
    + Switch. The case expression can be literal.

# What things we will do in the next version (+2)? 
1. Byte code (Tripple-address-code)
2. Emit the byte code in a turn of parsing. So we will not see the AST
