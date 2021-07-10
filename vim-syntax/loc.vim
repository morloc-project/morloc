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

let b:current_syntax = "loc"



" =============================================================================
"                             K E Y W O R D S                                  
" -----------------------------------------------------------------------------


syn keyword reserved where
syn keyword reserved module
syn keyword reserved import
syn keyword reserved from
syn keyword reserved as
syn keyword reserved source
syn keyword reserved export
syn keyword reserved True
syn keyword reserved False
syn keyword reserved and
syn keyword reserved or
syn keyword reserved xor
syn keyword reserved nand
syn keyword reserved not
syn keyword reserved type
syn keyword reserved object
syn keyword reserved table
syn keyword reserved record
syn keyword reserved system

" -----------------------------------------------------------------------------
hi def link reserved Keyword



" =============================================================================
"                           P R I M A T I V E S                                
" -----------------------------------------------------------------------------
syn region s_string start=/"/ end=/"/
syn region s_execute start=/`/ end=/`/
syn region s_execute start=/\[[a-zA-Z0-9]*|/ end=/|\]/
syn match s_num '\([a-zA-Z_]\)\@<!\<[0-9]\+\>\([a-zA-Z_]\)\@!'
syn match s_dbl '\([a-zA-Z_]\)\@<!\<[0-9]\+\.[0-9]\+\>\([a-zA-Z_]\)\@!'

" syn match s_num '\v(\h)@<!-?(\d*\.\d+|\d+)(\h)\@!'
"                  -------                ------- 
"                  negative look behind   negative look ahead
" -----------------------------------------------------------------------------
hi def link s_num      Number
hi def link s_dbl      Number
hi def link s_string   String
hi def link s_execute  String

" =============================================================================
"                            O P E R A T O R S                                 
" -----------------------------------------------------------------------------
syn match operator /=/
syn match operator /::/
syn match operator /:/
syn match operator /,/
syn match operator /(/
syn match operator /)/
syn match operator /{/
syn match operator /}/
syn match operator /->/
syn match operator /=>/

" operators allowed in constraints
syn match operator />/
syn match operator /</
syn match operator />=/
syn match operator /<=/
syn match operator /+/
syn match operator /-/
syn match operator /\//
syn match operator /\/\// " integer division
syn match operator /%/ " modulus
syn match operator /^/ " exponentiation

syn match operator /;/
syn match operator /@/

" -----------------------------------------------------------------------------
hi def link operator Operator



" =============================================================================
"                          M I S C E L L A N I A                               
" -----------------------------------------------------------------------------
syn match s_varlabel '\w\+:'
syn match s_varlabel '<\w\+>'
" -----------------------------------------------------------------------------
hi def link s_varlabel Special



" =============================================================================
"                             C O M M E N T S                                  
" -----------------------------------------------------------------------------
" define todo highlighting
syn match s_todo /\(TODO\|NOTE\|FIXME\):/ contained 
syn keyword s_todo XXX contained
syn match s_tag /\(Author\|Email\|Github\|Bugs\|Website\|Maintainer\|Description\):/ contained 

" define comments
" syn match comment '\/\/.*$' contains=tag
" syn region comment start='\/\*' end='\*\/' contains=tag
syn match s_comment '--.*' contains=s_todo,s_tag
syn region s_comment start="{-" end="-}" contains=s_todo,s_tag

" =============================================================================
"                               E R R O R S                                    
" -----------------------------------------------------------------------------
syn match s_error '^#'

" -----------------------------------------------------------------------------
hi def link s_comment  Comment
hi def link s_todo     Todo
hi def link s_tag      SpecialComment
hi def link s_error    Error
