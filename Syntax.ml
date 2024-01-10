type var  = string
type fname = string

type expr = Num of int | Bool of bool |
            Var of var |
            App of fname * expr list |
            If  of expr * expr * expr |
            Let of var * expr * expr

type tl = Expr of expr
        | Bind of var * expr
        | FunD of fname * var list * expr


                    
