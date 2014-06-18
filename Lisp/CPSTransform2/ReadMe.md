### The default behavior

1. builtin procedure including set! and define should be direct style. e.g, `+ - * / = cons car append drop`
2. user defined procedure will be transform to continuation passing style
3. a procedure which accept a high-order procedure as argument should not be builtin, it should aways be transform to  CPS. e.g, `map filter foldl`
4. while a builtin procedure `f` is treated as an argument (first class function), it will be replace with the CPS version which called `f&`
5. the expressions inside `begin` form will be optimized: only the last expression and side-effect expression will exist at last

### Other alternatives

+ set the variable `pure-builtins` and `side-effect-builtins` to empty, then implement all the builtin procedure as CPS, so the whole program including script and interpreter become CPS 
    + this choosen make the interpreter looks more easier and stronger, but the program will also be slower, and the output of CPS transfrom will be harder to review
    + racket can not execute the output of CPS transform because racket consider the builtin procedure as DS
