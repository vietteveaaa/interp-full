open Tokenize
open Syntax

type sexpr = SNum of int
           | SName of string
           | SComp of sexpr list

exception ParseError

let keywords = ["def"; "let"; "if"; "true"; "false"]

let unname e =
  match e with
  | SName x ->
     if List.mem x keywords
     then raise ParseError
     else x
  | _ -> raise ParseError

let rec parseS se =
  match se with
  | SNum n -> Num n
  | SName "true"  -> Bool true
  | SName "false" -> Bool false
  | SName x -> if List.mem x keywords
               then raise ParseError
               else Var x
  | SComp [SName "if"; e; et; ef] ->
     If (parseS e, parseS et, parseS ef)
  | SComp (SName "if" :: _) -> raise ParseError
  | SComp [SName "let"; SComp [x; e]; e'] ->
     Let (unname x, parseS e, parseS e')
  | SComp (SName f :: es) ->
     if List.mem f keywords
     then raise ParseError
     else App (f, List.map parseS es)
  | _ -> raise ParseError

let parseD e =
  match e with
  | SComp [SName "let"; SName x; e] ->
     if List.mem x keywords
     then raise ParseError
     else Bind (x, parseS e)
  | SComp [SName "def"; SComp (SName f :: xs); e] ->
     if List.mem f keywords
     then raise ParseError
     else FunD (f, List.map unname xs, parseS e)
  | e -> Expr (parseS e)

let rec readWith cur rem ts =
  match ts with
  | [] -> (cur, rem)
  | Name s :: ts -> readWith (SName s :: cur) rem ts
  | Num  n :: ts -> readWith (SNum  n :: cur) rem ts
  | LParen :: ts -> readWith [] (cur :: rem) ts
  | RParen :: ts ->
     begin match rem with
     | [] -> raise ParseError
     | next :: rem -> readWith (SComp (List.rev cur) :: next) rem ts
     end

let read ts =
  match readWith [] [] ts with
  | [r], [] -> r
  | _, _ -> raise ParseError

let keywords = ["def"; "let"; "if"]
