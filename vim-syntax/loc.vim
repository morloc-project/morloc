" Vim syntax file
" Language: morloc
" Maintainer: Zebulun Arendsee
" -----------------------------------------------------------------------------
" INSTALLATION
" Run the following in your UNIX terminal
" $ mkdir -p ~/.vim/syntax/
" $ mkdir -p ~/.vim/ftdetect/
" $ cp loc.vim ~/.vim/syntax/
" $ echo 'au BufRead,BufNewFile *.loc set filetype=loc' > ~/.vim/ftdetect/loc.vim



" =============================================================================
"                             P R E A M B L E                                  
" -----------------------------------------------------------------------------
if exists("b:current_syntax")
  finish
endif

let b:current_syntax = ''
unlet b:current_syntax
syn include @R syntax/r.vim

let b:current_syntax = ''
unlet b:current_syntax
syn include @Python syntax/python.vim

let b:current_syntax = ''
unlet b:current_syntax
syn include @Perl syntax/perl.vim

let b:current_syntax = ''
unlet b:current_syntax
syn include @Shell syntax/sh.vim

let b:current_syntax = "loc"



" =============================================================================
"                             K E Y W O R D S                                  
" -----------------------------------------------------------------------------
syn keyword reserved type
syn keyword reserved around
syn keyword reserved before
syn keyword reserved after
syn keyword reserved do
syn keyword reserved set
syn keyword reserved from
syn keyword reserved import
syn keyword reserved export
syn keyword reserved as
syn keyword reserved include
syn keyword reserved alias
syn keyword reserved where
syn keyword reserved data
syn keyword reserved typedef
" -----------------------------------------------------------------------------
hi def link reserved Keyword



" =============================================================================
"                           P R I M A T I V E S                                
" -----------------------------------------------------------------------------
syn region s_string start=/'/ end=/'/
syn region s_string start=/"/ end=/"/
syn match s_num  '\h\@<!-\?\(\d*\.\d\+\|\d\+\)\h\@!'
" -----------------------------------------------------------------------------
hi def link s_num      Number
hi def link s_string   String



" =============================================================================
"                            O P E R A T O R S                                 
" -----------------------------------------------------------------------------
syn match operator /\$/ 
syn match operator /\./ 
syn match operator /::/ 
syn match operator /->/ 
syn match operator /=/ 
" -----------------------------------------------------------------------------
hi def link operator Operator



" =============================================================================
"                          M I S C E L L A N I A                               
" -----------------------------------------------------------------------------
syn match s_varlabel ':\w\+'
syn match s_varlabel '<\w\+>'
" -----------------------------------------------------------------------------
hi def link s_varlabel Special



" =============================================================================
"                             C O M M E N T S                                  
" -----------------------------------------------------------------------------
" define todo highlighting
syn keyword s_todo TODO NOTE FIXME XXX contained 
syn match s_tag /\(Author\|Email\|Github\|Bugs\|Website\|Maintainer\|Description\):/ contained 
" define comments
" syn match comment '\/\/.*$' contains=tag
" syn region comment start='\/\*' end='\*\/' contains=tag
syn match s_comment '#.*' contains=s_todo,s_tag contained
" -----------------------------------------------------------------------------
hi def link s_comment  Comment
hi def link r_comment  Comment
hi def link s_todo     Todo
hi def link s_tag      SpecialComment
