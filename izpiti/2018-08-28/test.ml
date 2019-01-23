let rec razlika_kvadratov a b = (a+b) * (a+b) - (a*a+b*b)

let rec ponovi_seznam n sez =
  if n = 0 then []
  else sez @ ponovi_seznam (n-1) sez

type 'a tree = 
  | Empty
  | Node of 'a tree * 'a * 'a tree

let leaf x = Node (Empty, x, Empty)

let test_tree = Node (Node (leaf 3, 12, Node (leaf 14, 13, leaf 6)), 11, Node (leaf 2, 8, leaf 10))

let rec padajoca v = function
  | Empty -> []
  | Node (lt, x, rt) when x > v -> []
  | Node (lt, x, rt) ->
    let left = padajoca x lt in
    let right = padajoca x rt in
    if List.length left > List.length right then
      left @ [x]
    else right @ [x]

let rec narascajoca v = function
| Empty -> []
| Node (lt, x, rt) when x < v -> []
| Node (lt, x, rt) ->
  let left = narascajoca x lt in
  let right = narascajoca x rt in
  if List.length left > List.length right then
    x :: left
  else x :: right

let rec monotona_pot = function
  | Empty -> []
  | Node (lt, x, rt) -> 
    (*Recursive search for paths*)
    let pure_left = monotona_pot lt in
    let pure_right = monotona_pot rt in
    let left_to_right = (padajoca x lt) @ [x] @ (narascajoca x rt) in
    let right_to_left = (padajoca x rt) @ [x] @ (narascajoca x lt) in
    let options = [pure_left; pure_right; left_to_right; right_to_left] in
    (*Choose the longest one*)
    List.fold_left (fun x y -> if List.length x > List.length y then x else y) pure_left options


(*TRETJA NALOGA*)

type 'a veriga = 
  | Filter of ('a -> bool) * 'a list * 'a veriga
  | Ostalo of 'a list

 let test = 
  Filter ((fun x -> x < 0), [], 
  Filter ((fun x -> x < 10), [],
  Ostalo []))

let rec vstavi x veriga = 
  match veriga with
  | Ostalo (elementi) -> Ostalo (x :: elementi)
  | Filter (f, elementi, filtri) -> 
    if f x then
      Filter (f, x :: elementi, filtri)
    else
      Filter (f, elementi, vstavi x filtri)

let rec poisci x = function
  | Ostalo elementi -> List.mem x elementi
  | Filter (f, elementi, filtri) -> 
    if f x then List.mem x elementi else poisci x filtri

let rec izprazni = function
  | Ostalo elementi -> (Ostalo [], elementi)
  | Filter (f, elementi, filtri) -> 
    let prazni_filtri, pobrani_filtri = izprazni filtri in
    let vsi_elementi = elementi @ pobrani_filtri in
    (Filter (f, [], prazni_filtri), vsi_elementi)

let dodaj_filter f veriga = 
  let veriga' = Filter (f, [], filter) in
  let prazna_veriga, elementi = izprazni veriga' in
  List.fold_left (fun v x -> vstavi x v) prazna_veriga elementi