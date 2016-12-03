" Vim syntax file
" Language: rats
" Maintainer: Zebulun Arendsee
" Latest Revision: 2016-12-02
" -----------------------------------------------------------------------------
" INSTALLATION
" Run the following in your UNIX terminal
" $ mkdir -p ~/.vim/syntax/
" $ mkdir -p ~/.vim/ftdetect/
" $ cp rats.vim ~/.vim/syntax/
" $ echo 'au BufRead,BufNewFile *.rats set filetype=rats' > ~/.vim/ftdetect/rats.vim

if exists("b:current_syntax")
  finish
endif

" setting operators
syn match operator '->'
syn match operator '::'
syn match operator ':'
syn match operator '-->'
syn match operator '?'
syn match operator '<'
syn match operator '>'

" define constants
" TODO this currently highlights numbers inside strings, e.g. r5g or 5r or r5
" syn match constant '\([a-zA-Z_]\)\@!-\?[1-9]\d*\h\@!'
syn match constant '\h\@<!\(\d*\.\d\+\|\d\+\)\h\@!'
syn match constant 'NIL'
syn region constant start="'" end="'"
syn region constant start='"' end='"'

" define todo highlighting
syn keyword tag contained TODO NOTE

" define comments
syn match comment '\/\/.*$' contains=tag
syn region comment start='\/\*' end='\*\/' contains=tag

let b:current_syntax = "rats"
hi def link operator    Operator    
hi def link constant    Constant
hi def link comment     Comment
hi def link todo        Todo
