(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 DODATNE VAJE 
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a drevo = 
  | Prazno
  | Veja of 'a drevo * 'a * 'a drevo

let test_tree = 
  let levo_drevo = Veja (Veja (Prazno, 0, Prazno), 2, Prazno) in
  let desno_drevo = Veja (Veja (Prazno, 6, Prazno), 7, Veja (Prazno, 11, Prazno)) in
  Veja (levo_drevo, 5, desno_drevo)

(*----------------------------------------------------------------------------*]
 Funkcija [bst_of_list] iz seznama naredi dvojiško iskalno drevo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # [11; 6; 7; 0; 2; 5] |> bst_of_list |> is_bst;;
 - : bool = true
[*----------------------------------------------------------------------------*)

let rec vstavi x = function
  | Prazno -> Veja (Prazno, x, Prazno)
  | Veja (_, y, _) as drevo when x = y -> drevo
  | Veja (l, y, d) when x < y -> Veja (vstavi x l, y, d)
  | Veja (l, y, d) -> Veja (l, y, vstavi x d)

(* elegantna rešitev*)
let bst_of_list sez = List.fold_right vstavi sez Prazno 

let bst_od_seznama sez = 
  let rec aux acc = function
  | [] -> acc
  | x :: xs -> 
  let vstavljeno = vstavi x acc in
  aux vstavljeno xs
  in
  aux Prazno sez

(*----------------------------------------------------------------------------*]
 Funkcija [tree_sort] uredi seznam s pomočjo pretvorbe v bst in nato nazaj
 v seznam.

 Opomba: Prosim ne uporabljajte te funkcije v praksi.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # tree_sort ["a"; "c"; "f"; "b"; "e"; "d"];;
 - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
[*----------------------------------------------------------------------------*)

let rec tree_sort list = 
  let bst = bst_of_list list in
  let rec aux bst = 
  match bst with
  | Prazno -> []
  | Veja (l, x, d) -> (aux l) @ [x] @ (aux d)
  in
  aux bst

(*----------------------------------------------------------------------------*]
 Funkcija [follow directions tree] tipa [direction list -> 'a tree -> 'a option]
 sprejme seznam navodil za premikanje po drevesu in vrne vozlišče do katerega 
 vodi podana pot. Ker navodila morda ne vodijo do nobenega vozlišča v drevesu
 vrne rezultat kot [option] tip. Ne pozabite definirati tipa [directions].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # follow [Right; Left] test_tree;;
 - : int option = Some 6
 # follow [Right; Left; Right; Right] test_tree;;
 - : int option = None
[*----------------------------------------------------------------------------*)

type direction = Left | Right

let rec follow (directions : direction list) = function
  | Prazno -> None
  | Veja (l, y, d) -> (
    match directions with
    | [] -> Some y
    | x :: xs -> if x = Left then follow xs l else follow xs d 
  )

(*----------------------------------------------------------------------------*]
 Funkcija [prune directions tree] poišče vozlišče v drevesu glede na navodila,
 ter izbriše poddrevo, ki se začne v izbranem vozlišču.

 Opozorilo: Pri uporabi [Some Node(l, x, r)] se OCaml pritoži, saj to razume 
 kot [(Some Node)(l, x, r)], zato pravilno postavite potrebne oklepaje.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # prune [Right] test_tree;;
 - : int tree option =
 Some (Node (Node (Node (Empty, 0, Empty), 2, Empty), 5, Empty))
[*----------------------------------------------------------------------------*)

let rec prune (directions : direction list) drevo = 
  match directions, drevo with
  | [], _ -> Some Prazno
  | sez, Prazno -> None
  | x :: xs, Veja (l, y, d) -> (
    match x with
    | Left -> (
    let poddrevo = prune xs l in
    match poddrevo with
    | None -> None
    | Some p -> Some (Veja (p, y, d))
    )
    | Right -> (
     let poddrevo = prune xs d in
     match poddrevo with
     | None -> None
     | Some p -> Some (Veja (l, y, p)) 
    )
  )

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 PHANTOM TREES

 Druga možnost pri brisanju podatkov je, da spremenimo tip s katerim
 predstavljamo drevo. Definirate nov tip fantomskega drevesa, ki poleg podatka,
 levega in desnega poddrevesa hrani še dodatno informacijo o stanju [state], ki
 je bodisi [Exists] če je vozlišče še prisotno in pa [Ghost] če je vozlišče v
 drevesu izbrisano in ga upoštevamo le še kot delitveno vozlišče. Še vedno
 predpostavljamo, da imajo drevesa obliko BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type state = Exists | Ghost

type 'a phantom = 
  | Empty
  | Node of 'a phantom * 'a * 'a phantom * state

(*----------------------------------------------------------------------------*]
 Funkcija [phantomize] tipa ['a tree -> 'a phantom_tree] navadnemu drevesu
 priredi ekvivalentno fantomsko drevo.
 Funkcija [kill x ptree] izbriše element [x] v fantomskem drevesu tako, da 
 njegovo stanje nastavi na [Ghost].
 Predpostavite lahko, da v drevesu ni ponovitev elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # phantomize test_tree;;
 - : int phantom_tree =
 P_Node (P_Node (P_Node (P_Empty, 0, P_Empty, Exists), 2, P_Empty, Exists), 5,
 P_Node (P_Node (P_Empty, 6, P_Empty, Exists), 7,
 P_Node (P_Empty, 11, P_Empty, Exists), Exists),
 Exists)

 # bst_of_list [3; 4; 2] |> phantomize |> kill 3 |> kill 6;;
 - : int phantom_tree =
 P_Node (P_Empty, 2,
 P_Node (P_Node (P_Empty, 3, P_Empty, Ghost), 4, P_Empty, Exists), Exists)
[*----------------------------------------------------------------------------*)

let rec phantomize (drevo : 'a drevo) : 'a phantom = 
  match drevo with
  | Prazno -> Empty
  | Veja (l, x, d) -> Node (phantomize l, x, phantomize d, Exists) 

let rec kill (x : 'a) (drevo : 'a phantom) : 'a phantom = 
  match drevo with
  | Empty -> Empty
  | Node (l, y, d, _) when x = y -> Node (l, y, d, Ghost)
  | Node (l, y, d, stanje) when x < y -> Node (kill x l, y, d, stanje)
  | Node (l, y, d, stanje) -> Node (l, y, kill x d, stanje)

(*----------------------------------------------------------------------------*]
 Funkcija [unphantomize] tipa ['a phantom_tree -> 'a tree] fantomskemu drevesu 
 priredi navadno drevo, ki vsebuje zgolj vozlišča, ki še obstajajo. Vrstni red
 vozlišč v končnem drevesu ni pomemben.

 Namig: Lahko uporabite vmesni prehodom na drugo podatkovno strukturo.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # test_tree |> phantomize |> kill 7 |> kill 0 |> kill 5 |> unphantomize;;
 - : int tree = Node (Node (Node (Empty, 2, Empty), 6, Empty), 11, Empty)
[*----------------------------------------------------------------------------*)

let naslednik bst = 
  let rec aux = function
  | Prazno -> None
  | Veja (Prazno, x, _) -> Some x
  | Veja (l, _, _) -> aux l 
  in
  match bst with
  | Prazno -> None
  | Veja (_, _, d) -> aux d 

let rec izbrisi x = function 
  | Prazno -> Prazno
  | Veja (l, y, d) when x < y -> Veja (izbrisi x l, y, d) 
  | Veja (l, y, d) when x > y -> Veja (l, y, izbrisi x d)
  | Veja (l, y, d) as bst -> (
      let succ = naslednik bst in
      match succ with
      | None -> l 
      | Some s -> 
      let popravi = izbrisi s d in
      Veja (l, s, popravi)
  )

let rec spremeni_v_navadno_drevo (drevo : 'a phantom) : 'a drevo = 
  match drevo with
  | Empty -> Prazno
  | Node (l, x, d, stanje) -> (
    match stanje with
    | Exists -> Veja (spremeni_v_navadno_drevo l, x, spremeni_v_navadno_drevo d)
    | Ghost -> izbrisi x (Veja (spremeni_v_navadno_drevo l, x, spremeni_v_navadno_drevo d))
  )

(*uradna rešitev*)
let rec unphantomize (drevo : 'a phantom) : 'a drevo = 
  let rec aux = function
  | Empty -> []
  | Node (l, x, d, Ghost) -> (aux l) @ (aux d)
  | Node (l, x, d, Exists) -> (aux l) @ [x] @ (aux d)
  in
  drevo |> aux |> bst_of_list
