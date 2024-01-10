type tok = LParen | RParen | Name of string | Num of int

let rec splitWhen p s =
  match Seq.uncons s with
  | None -> Seq.empty, Seq.empty
  | Some (c, s) ->
     if p c
     then let (cs, rem) = splitWhen p s
          in Seq.cons c cs, rem
     else Seq.empty, Seq.cons c s

let between lo hi c = lo <= c && c <= hi

let digit = between '0' '9'
let lower = between 'a' 'z'
let upper = between 'A' 'Z'
let alpha c = lower c || upper c
let alnum c = alpha c || digit c

let white c = List.mem c [' '; '\n'; '\r'; '\t']

let getName s =
  let (cs, rem) = splitWhen alnum s
  in Name (String.of_seq cs), rem

let mkNum =
  let rec aux n cs =
    match Seq.uncons cs with
    | None -> n
    | Some (c, cs) ->
       aux (10 * n + Char.code c - Char.code '0') cs
  in aux 0

let getNum neg s =
  let (cs, rem) = splitWhen digit s in
  let n = mkNum cs in
  Num (if neg then -n else n), rem

exception WrongChar of char

let tokenise =
  let rec aux cs =
    match Seq.uncons cs with
    | None -> []
    | Some (c, cs) ->
       if c = '-'
       then let (t, cs) = getNum true cs
            in t :: aux cs
       else if digit c
       then let (t, cs) = getNum false (Seq.cons c cs)
            in t :: aux cs
       else if alpha c
       then let (t, cs) = getName (Seq.cons c cs)
            in t :: aux cs
       else if c = '('
       then LParen :: aux cs
       else if c = ')'
       then RParen :: aux cs
       else if white c
       then aux cs
       else raise (WrongChar c)
  in fun s -> aux (String.to_seq s)

let gcd =
  "(def (gcd m n)
     (if (eq n 0)
         m
         (gcd n (mod m n))))"
