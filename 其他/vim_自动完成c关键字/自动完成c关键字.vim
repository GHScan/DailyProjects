inoremap <F2> <esc>:call AutoCompleteCToken()<cr>i
function! IsSingleToken(line, token)
    return a:line =~ "^\\s*" . a:token . "\\s*$"
endfunction
function! AutoCompleteCToken()
    let line = getline(".")
    if IsSingleToken(line, "if") || IsSingleToken(line, "while")
        exe "norm A ()\n{\n\n}\<esc>3k$i"
    elseif IsSingleToken(line, "for")
        exe "norm A (;;)\n{\n\n}\<esc>3k^f(a"
    elseif IsSingleToken(line, "do")
        exe "norm o{\n} while(0);\<esc>O"
    elseif IsSingleToken(line, "else")
        exe "norm o{\n}\<esc>O" 
    endif
endfunction
