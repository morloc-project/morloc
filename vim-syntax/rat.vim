" Vim syntax file
" Language: rat
" Maintainer: Zebulun Arendsee
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


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Global syntax - shared between all sections

" illegal operators
syn match s_error '@[^ \n\t]*' contained
syn match s_error '\(\\\|==\|:::\|<-\|[^ @A-Za-z0-9_(=.,?)\[\]<:>;-]\)[^ \n]*' contained
syn match s_error ' \+$' contained

" All legal patterns should be represented in the block below. They are all
" highlighted as errors, unless the patterns are overreidden by the section
" specific lexical rules. 
syn keyword DEFAULT_ERROR id null call true false          contained
syn keyword DEFAULT_ERROR memcache datcache nocache        contained
syn match   DEFAULT_ERROR '\.'                             contained
syn match   DEFAULT_ERROR '->'                             contained
syn match   DEFAULT_ERROR '::'                             contained
syn match   DEFAULT_ERROR '='                              contained
syn match   DEFAULT_ERROR '-->'                            contained
syn match   DEFAULT_ERROR '?'                              contained
syn match   DEFAULT_ERROR ','                              contained
syn match   DEFAULT_ERROR '\h\@<!\(\d*\.\d\+\|\d\+\)\h\@!' contained
syn keyword DEFAULT_ERROR NIL                              contained
syn keyword DEFAULT_ERROR with split on merge using        contained
syn keyword DEFAULT_ERROR as                               contained
syn match   DEFAULT_ERROR ':[a-zA-Z0-9.]\+'                contained
syn region  DEFAULT_ERROR start="'" end="'"                contained
syn region  DEFAULT_ERROR start='"' end='"'                contained

" define todo highlighting
syn keyword s_todo TODO NOTE FIXME XXX contained 
syn match s_tag /\(Author\|Email\|Github\|Bugs\|Website\|Maintainer\|Description\):/ contained 

" define comments
" syn match comment '\/\/.*$' contains=tag
" syn region comment start='\/\*' end='\*\/' contains=tag
syn match s_comment '#.*' contains=s_todo,s_tag contained

syn match s_break '^---\+$' contained
syn match s_break ';' contained
syn keyword s_constant __all__ contained

" section headers
syn match s_section '@import'  contained
syn match s_section '@export'  contained
syn match s_section '@type'    contained
syn match s_section '@path'    contained
syn match s_section '@compose' contained
syn match s_section '@alias'   contained
syn match s_section '@arg'     contained
syn match s_section '@check'   contained
syn match s_section '@effect'  contained
syn match s_section '@cache'   contained
syn match s_section '@pack'    contained
syn match s_section '@open'    contained
syn match s_section '@fail'    contained
syn match s_section '@pass'    contained
syn match s_section '@loop'    contained

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Section specific syntax

" general default functions
syn keyword s_simple_function id null call true false contained

" default caching functions
syn keyword s_cache_function memcache datcache nocache contained

" setting operators
syn match s_compose '\.'  contained
syn match s_rarror  '->'  contained
syn match s_couple  '::'  contained
syn match s_equal   '='   contained
syn match s_depend  '-->' contained
syn match s_switch  '?'   contained
syn match s_sep     ','   contained

" define constants
syn match s_number '\h\@<!\(\d*\.\d\+\|\d\+\)\h\@!' contained
syn keyword s_nil NIL contained

" keywords
syn keyword s_loop_keyword with split on merge using contained
syn keyword s_export_keyword as contained

" labels
syn match s_varlabel ':[a-zA-Z0-9.]\+' contained

" strings
syn region s_string start="'" end="'" contained
syn region s_string start='"' end='"' contained

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Set a highlighting paradigm for each section

syn cluster c_global    contains=s_error,s_comment,s_break,s_constant,s_section,DEFAULT_ERROR
syn cluster c_operators contains=s_compose,s_rarror,s_couple,s_equal,s_depend,s_switch
syn cluster c_function  contains=s_simple_function,s_couple,s_varlabel
syn cluster c_hasarg    contains=s_equal,s_string,s_number,s_sep,s_varlabel

syn region r_header start=/\%^/ end=/@\@=/ contains=s_comment

syn region r_import  start=/@import/  end=/@\@=/ contains=@c_global,s_string
syn region r_export  start=/@export/  end=/@\@=/ contains=@c_global,s_varlabel,s_export_keyword
syn region r_type    start=/@type/    end=/@\@=/ contains=@c_global,s_nil,s_rarror,s_couple
syn region r_path    start=/@path/    end=/@\@=/ contains=@c_global,s_simple_function,s_sep,s_depend,s_switch,s_couple,s_varlabel
syn region r_compose start=/@compose/ end=/@\@=/ contains=@c_global,s_simple_function,s_compose,s_equal
syn region r_alias   start=/@alias/   end=/@\@=/ contains=@c_global,s_simple_function,s_equal,s_number
syn region r_arg     start=/@arg/     end=/@\@=/ contains=@c_global,@c_hasarg,s_couple
syn region r_check   start=/@check/   end=/@\@=/ contains=@c_global,@c_function
syn region r_effect  start=/@effect/  end=/@\@=/ contains=@c_global,@c_function
syn region r_pack    start=/@pack/    end=/@\@=/ contains=@c_global,@c_function
syn region r_open    start=/@open/    end=/@\@=/ contains=@c_global,@c_function
syn region r_fail    start=/@fail/    end=/@\@=/ contains=@c_global,@c_function
syn region r_pass    start=/@pass/    end=/@\@=/ contains=@c_global,@c_function
syn region r_cache   start=/@cache/   end=/@\@=/ contains=@c_global,s_cache_function,s_couple,s_varlabel
syn region r_loop    start=/@loop/    end=/@\@=/ contains=@c_global,@c_hasarg,s_loop_keyword



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Assign colors

let b:current_syntax = "rat"

hi def link s_simple_function Function
hi def link s_cache_function  Function

hi def link s_error    Error
hi def link s_break    Underlined
hi def link s_varlabel Special
hi def link s_section  Label

hi def link s_loop_keyword   Keyword
hi def link s_export_keyword Keyword

hi def link s_compose Operator
hi def link s_rarror  Operator
hi def link s_couple  Operator
hi def link s_equal   Operator
hi def link s_depend  Operator
hi def link s_switch  Operator
hi def link s_sep     Operator

hi def link s_constant Constant
hi def link s_nil      Constant

hi def link s_number   Number
hi def link s_string   String
hi def link s_comment  Comment
hi def link s_todo     Todo
hi def link s_tag      SpecialComment

hi def link DEFAULT_ERROR Error
