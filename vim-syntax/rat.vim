" Vim syntax file
" Language: rat
" Maintainer: Zebulun Arendsee
" Latest Revision: 2016-12-02
" -----------------------------------------------------------------------------
" INSTALLATION
" Run the following in your UNIX terminal
" $ mkdir -p ~/.vim/syntax/
" $ mkdir -p ~/.vim/ftdetect/
" $ cp rat.vim ~/.vim/syntax/
" $ echo 'au BufRead,BufNewFile *.rat set filetype=rat' > ~/.vim/ftdetect/rat.vim

if exists("b:current_syntax")
  finish
endif

" syn keyword keyword     set save run
" syn keyword function    clear import rand
" syn keyword repeat      foreach in
" syn keyword conditional if else
" syn keyword type        int string float node graph link

syn match section '@import'
syn match section '@export'
syn match section '@path'
syn match section '@compose'
syn match section '@alias'
syn match section '@arg'
syn match section '@check'
syn match section '@effect'
syn match section '@cache'
syn match section '@pack'
syn match section '@open'
syn match section '@fail'
syn match section '@pass'
syn match section '@loop'

syn keyword function id null call true false
syn keyword function memcache datcache nocache

syn match break '^---\+$'
syn match break ';'

" setting operators
syn match operator '\.'
syn match operator '->'
syn match operator '::'
syn match operator ':'
syn match operator '='
syn match operator '-->'
syn match operator '?'
syn match operator '<'
syn match operator '>'

" define constants
" TODO this currently highlights numbers inside strings, e.g. r5g or 5r or r5
" syn match number '\([a-zA-Z_]\)\@!-\?[1-9]\d*\h\@!'
syn match number '\h\@<!\(\d*\.\d\+\|\d\+\)\h\@!'
syn keyword constant NIL
syn keyword constant __all__
syn keyword constant __map__ __val__ __eff__ __cache__ __fail__ __pass__ 

" keywords
syn keyword keyword as with split on merge using

" labels
syn match varlabel ':[a-zA-Z0-9._]\+'

syn region string start="'" end="'"
syn region string start='"' end='"'

" define todo highlighting
syn keyword tag contained TODO NOTE

" define comments
" syn match comment '\/\/.*$' contains=tag
" syn region comment start='\/\*' end='\*\/' contains=tag
syn match comment '#.*'

let b:current_syntax = "rat"

hi def link break    Underlined
hi def link varlabel Special
hi def link section  Label
hi def link operator Operator
hi def link constant Constant
hi def link number   Number
hi def link string   String 
hi def link comment  Comment
hi def link todo     Todo
