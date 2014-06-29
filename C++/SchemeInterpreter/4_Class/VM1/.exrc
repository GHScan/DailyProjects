if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
nmap gx <Plug>NetrwBrowseX
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
nnoremap <silent> <F8> :!./main
nnoremap <silent> <F6> :cn
nnoremap <silent> <F5> :cp
nnoremap <silent> <F4> :ts =expand("<cword>")
nnoremap <silent> <F3> :grep -R "\<=expand("<cword>")\>" *
nnoremap <silent> <F2> :nohlsearch
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set background=dark
set backspace=indent,eol,start
set cindent
set completeopt=menu,longest,preview
set expandtab
set fileencodings=ucs-bom,utf-8,cp936,gb18030,big5,euc-jp,euc-kr,latin1
set fileformats=unix,mac,dos
set formatoptions=tcqMm
set guifont=Monaco\ 10
set guioptions=
set helplang=en
set history=500
set hlsearch
set ignorecase
set incsearch
set printoptions=paper:a4
set ruler
set runtimepath=~/.vim,~/.vim/bundle/vim-racket,/var/lib/vim/addons,/usr/share/vim/vimfiles,/usr/share/vim/vim74,/usr/share/vim/vimfiles/after,/var/lib/vim/addons/after,~/.vim/after
set shiftwidth=4
set showcmd
set showmatch
set smartcase
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set tabstop=4
set textwidth=400
" vim: set ft=vim :
