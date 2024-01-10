open Tokenize
open Parse
open Interp

let getTL () =
  let rec aux cur rem =
    match read_line () |> tokenise |>
            readWith cur rem with
    | [se], [] -> parseD se
    | cur, rem -> aux cur rem
  in aux [] []

let startEnv =
  List.map (fun n -> (n, Builtin))
    ["add"; "sub"; "mul"; "div"; "mod"; "eq"; "lt"; "gt"]

let string_of_typ t =
  match t with
  | TNum -> "int"
  | TBool -> "bool"

let string_of_val v =
  match v with
  | VNum n -> string_of_int n
  | VBool true -> "true"
  | VBool false -> "false"

let rec go env fenv =
  print_string "> ";
  try match getTL () with
      | Expr e -> eval env fenv e |>
                    string_of_val |>
                    print_string;
                  print_newline ();
                  go env fenv
      | Bind (x, e) ->
         let v = eval env fenv e
         in go ((x, v) :: env) fenv
      | FunD (f, xs, e) ->
         go env ((f, UDef (f, fenv, env, xs, e)) :: fenv)
  with
  | ParseError ->
     print_string "Parse Error!\n";
     go env fenv
  | TypeError (t, v) ->
     print_string
       ("Type error: expected a value of type " ^
          string_of_typ t ^
            " but received " ^
              string_of_val v ^ "\n");
     go env fenv

let repl () =
  go [] startEnv


