" Vim syntax file
" Language:	hm

if exists("b:current_syntax")
    finish
endif

syntax keyword hmLet                let in
syntax match   hmLet                "="
highlight link hmLet Keyword

syntax match   hmFunction           "λ"
syntax match   hmFunction           "\."
highlight link hmFunction Function

syntax keyword hmBoolean            true false
highlight link hmBoolean Number

syntax match   hmNumber             "\<\d*\>"
highlight link hmNumber Number

syntax match   hmString             "\".*\""
highlight link hmString String

syntax match   hmType               "->"
syntax match   hmType               "\<[A-Z][a-zA-Z]*"
syntax match   hmType               "\'[a-z]*"
syntax match   hmType               "_"
highlight link hmType Type

let b:current_syntax = "hm"

