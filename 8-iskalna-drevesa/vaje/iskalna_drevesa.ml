(* ========== Vaja 4: Iskalna Drevesa  ========== *)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Ocaml omogoča enostavno delo z drevesi. Konstruiramo nov tip dreves, ki so
 bodisi prazna, bodisi pa vsebujejo podatek in imajo dve (morda prazni)
 poddrevesi. Na tej točki ne predpostavljamo ničesar drugega o obliki dreves.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type 'a drevo = 
     | Prazno
     | Veja of 'a drevo * 'a * 'a drevo

let leaf x = Veja (Prazno, x, Prazno)

(*----------------------------------------------------------------------------*]
 Definirajmo si testni primer za preizkušanje funkcij v nadaljevanju. Testni
 primer predstavlja spodaj narisano drevo, pomagamo pa si s pomožno funkcijo
 [leaf], ki iz podatka zgradi list.
          5
         / \
        2   7
       /   / \
      0   6   11
[*----------------------------------------------------------------------------*)

let testno_drevo = 
     let levo_drevo = Veja (Veja (Prazno, 0, Prazno), 2, Prazno) in
     let desno_drevo = Veja (Veja (Prazno, 6, Prazno), 7, Veja (Prazno, 11, Prazno)) in
     Veja (levo_drevo, 5, desno_drevo)

let test_tree = 
     let left_t = Veja (leaf 0, 2, Prazno) in
     let right_t = Veja (leaf 6, 7, leaf 11) in
     Veja(left_t, 5, right_t)

(*----------------------------------------------------------------------------*]
 Funkcija [mirror] vrne prezrcaljeno drevo. Na primeru [test_tree] torej vrne
          5
         / \
        7   2
       / \   \
      11  6   0
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mirror test_tree ;;
 - : int tree =
 Node (Node (Node (Empty, 11, Empty), 7, Node (Empty, 6, Empty)), 5,
 Node (Empty, 2, Node (Empty, 0, Empty)))
[*----------------------------------------------------------------------------*)

let rec prezrcali = function
     | Prazno -> Prazno
     | Veja (l, x, d) -> Veja (prezrcali d, x, prezrcali l)

let rec mirror tree = 
     match tree with
     | Prazno -> Prazno
     | Veja (lt, x, rt) -> Veja (mirror rt, x, mirror lt)

(*----------------------------------------------------------------------------*]
 Funkcija [height] vrne višino oz. globino drevesa, funkcija [size] pa število
 vseh vozlišč drevesa.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # height test_tree;;
 - : int = 3
 # size test_tree;;
 - : int = 6
[*----------------------------------------------------------------------------*)

(*size*)
let rec velikost = function
     | Prazno -> 0
     | Veja (l, x, d) -> 1 + velikost l + velikost d

(*repno rekurzivna size*)
let rec_size drevo = 
     let rec aux acc queue = 
          match queue with
          | [] -> acc
          | t :: ts -> (
               match t with
               | Prazno -> aux acc ts
               | Veja (lt, x, rt) -> aux (acc + 1) (lt :: rt :: ts)
          )
          in
          aux 0 [drevo]

(*globina oz. height*)
let rec globina = function
     | Prazno -> 0
     | Veja (l, x, d) -> (*1 + max (globina l) (globina d)*)
          let levo = globina l in
          let desno = globina d in
          1 + max desno levo

(*----------------------------------------------------------------------------*]
 Funkcija [map_tree f tree] preslika drevo v novo drevo, ki vsebuje podatke
 drevesa [tree] preslikane s funkcijo [f].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # map_tree ((<)3) test_tree;;
 - : bool tree =
 Node (Node (Node (Empty, false, Empty), false, Empty), true,
 Node (Node (Empty, true, Empty), true, Node (Empty, true, Empty)))
[*----------------------------------------------------------------------------*)

let rec map_tree f = function
     | Prazno -> Prazno
     | Veja (lt, x, rt) -> Veja (map_tree f lt, f x, map_tree f rt)

(*----------------------------------------------------------------------------*]
 Funkcija [list_of_tree] pretvori drevo v seznam. Vrstni red podatkov v seznamu
 naj bo takšen, da v primeru binarnega iskalnega drevesa vrne urejen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # list_of_tree test_tree;;
 - : int list = [0; 2; 5; 6; 7; 11]
[*----------------------------------------------------------------------------*)

let rec list_of_tree = function
     | Prazno -> []
     | Veja (lt, x, rt) -> 
          let levo_d = list_of_tree lt in
          let desno_d = list_of_tree rt in
          levo_d @ [x] @ desno_d 

(*----------------------------------------------------------------------------*]
 Funkcija [is_bst] preveri ali je drevo binarno iskalno drevo (Binary Search 
 Tree, na kratko BST). Predpostavite, da v drevesu ni ponovitev elementov, 
 torej drevo npr. ni oblike Node( leaf 1, 1, leaf 2)). Prazno drevo je BST.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # is_bst test_tree;;
 - : bool = true
 # test_tree |> mirror |> is_bst;;
 - : bool = false
[*----------------------------------------------------------------------------*)

(*lahko samo zgornjo funkcijo izkoristim*)
let rec is_bst = function
     | Prazno -> true
     | Veja (lt, x, rt) as drevo -> 
     let seznam = list_of_tree drevo in
     let rec je_urejen = function
          | [] -> true
          | x :: [] -> true
          | x :: y :: xs -> if x < y then je_urejen (y :: xs) else false
     in
     je_urejen seznam

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 V nadaljevanju predpostavljamo, da imajo dvojiška drevesa strukturo BST.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert] v iskalno drevo pravilno vstavi dani element. Funkcija 
 [member] preveri ali je dani element v iskalnem drevesu.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 2 (leaf 4);;
 - : int tree = Node (Node (Empty, 2, Empty), 4, Empty)
 # member 3 test_tree;;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec insert x = function
     | Prazno -> Veja (Prazno, x, Prazno)
     | Veja (l, y, d) -> 
     if x = y then Veja (l, y, d)
     else if x > y then Veja(l, y, insert x d)
     else Veja(insert x l, y, d) 
     
let rec member x = function
     | Prazno -> false
     | Veja (l, y, d) ->
     if x = y then true
     else if x < y then member x l
     else member x d
     
(*----------------------------------------------------------------------------*]
 Funkcija [member2] ne privzame, da je drevo bst.
 
 Opomba: Premislte kolikšna je časovna zahtevnost funkcije [member] in kolikšna
 funkcije [member2] na drevesu z n vozlišči, ki ima globino log(n). 
[*----------------------------------------------------------------------------*)

let rec member2 x = function
     | Prazno -> false
     | Veja (l, y, d) -> if x = y then true else member2 x l || member2 x d

(*----------------------------------------------------------------------------*]
 Funkcija [succ] vrne naslednjika korena danega drevesa, če obstaja. Za drevo
 oblike [bst = Node(l, x, r)] vrne najmanjši element drevesa [bst], ki je večji
 od korena [x].
 Funkcija [pred] simetrično vrne največji element drevesa, ki je manjši od
 korena, če obstaja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # succ test_tree;;
 - : int option = Some 6
 # pred (Node(Empty, 5, leaf 7));;
 - : int option = None
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

let predhodnik bst = 
     let rec aux = function
     | Prazno -> None
     | Veja (_, x, Prazno) -> Some x
     | Veja (_, _, d) -> aux d 
     in
     match bst with
     | Prazno -> None
     | Veja (l, _, _) -> aux l 

(*----------------------------------------------------------------------------*]
 Na predavanjih ste omenili dva načina brisanja elementov iz drevesa. Prvi 
 uporablja [succ], drugi pa [pred]. Funkcija [delete x bst] iz drevesa [bst] 
 izbriše element [x], če ta v drevesu obstaja. Za vajo lahko implementirate
 oba načina brisanja elementov.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # (*<< Za [delete] definiran s funkcijo [succ]. >>*)
 # delete 7 test_tree;;
 - : int tree =
 Node (Node (Node (Empty, 0, Empty), 2, Empty), 5,
 Node (Node (Empty, 6, Empty), 11, Empty))
[*----------------------------------------------------------------------------*)

(*pobrisse z naseldnikom*)
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

(*pobrise z predhodnikom*)
let rec delete x = function
     | Prazno -> Prazno
     | Veja (l, y, d) when y < x -> Veja (l, y, delete x d)
     | Veja (l, y, d) when y > x -> Veja (delete x l, y, d)
     | Veja (l, y, d) as bst -> (
          let pred = predhodnik bst in
          match pred with
          | None -> d 
          | Some p -> 
          let popravi = delete p l in
          Veja (popravi, p, d)
     )

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 SLOVARJI

 S pomočjo BST lahko (zadovoljivo) učinkovito definiramo slovarje. V praksi se
 slovarje definira s pomočjo hash tabel, ki so še učinkovitejše. V nadaljevanju
 pa predpostavimo, da so naši slovarji [dict] binarna iskalna drevesa, ki v
 vsakem vozlišču hranijo tako ključ kot tudi pripadajočo vrednost, in imajo BST
 strukturo glede na ključe. Ker slovar potrebuje parameter za tip ključa in tip
 vrednosti, ga parametriziramo kot [('key, 'value) dict].
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

type ('key, 'value) dict = ('key * 'value) drevo

(*----------------------------------------------------------------------------*]
 Napišite testni primer [test_dict]:
      "b":1
      /    \
  "a":0  "d":2
         /
     "c":-2
[*----------------------------------------------------------------------------*)

let test_dict = Veja ( Veja (Prazno, ("a", 0), Prazno), ("b", 1), Veja (Veja (Prazno, ("c", -2), Prazno), ("d", 2), Prazno))

(*----------------------------------------------------------------------------*]
 Funkcija [dict_get key dict] v slovarju poišče vrednost z ključem [key]. Ker
 slovar vrednosti morda ne vsebuje, vrne [option] tip.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_get "banana" test_dict;;
 - : 'a option = None
 # dict_get "c" test_dict;;
 - : int option = Some (-2)
[*----------------------------------------------------------------------------*)

let rec dict_get key = function
     | Prazno -> None
     | Veja (l, (kljuc, vrednost), d) -> 
     if kljuc = key then Some vrednost
     else if kljuc < key then dict_get key d 
     else dict_get key l
     
(*----------------------------------------------------------------------------*]
 Funkcija [print_dict] sprejme slovar s ključi tipa [string] in vrednostmi tipa
 [int] in v pravilnem vrstnem redu izpiše vrstice "ključ : vrednost" za vsa
 vozlišča slovarja.
 Namig: Uporabite funkciji [print_string] in [print_int]. Nize združujemo z
 operatorjem [^]. V tipu funkcije si oglejte, kako uporaba teh funkcij določi
 parametra za tip ključev in vrednosti v primerjavi s tipom [dict_get].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # print_dict test_dict;;
 a : 1
 b : 1
 c : -2
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec print_dict = function
     | Prazno -> ()
     | Veja (l, (k, v), d) -> (
          print_dict l;
          print_string (k ^ ":"); print_int v; print_newline ();
          print_dict d
     )

(*----------------------------------------------------------------------------*]
 Funkcija [dict_insert key value dict] v slovar [dict] pod ključ [key] vstavi
 vrednost [value]. Če za nek ključ vrednost že obstaja, jo zamenja.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # dict_insert "1" 14 test_dict |> print_dict;;
 1 : 14
 a : 1
 b : 1
 c : -2
 d : 2
 - : unit = ()
 # dict_insert "c" 14 test_dict |> print_dict;;
 a : 1
 b : 1
 c : 14
 d : 2
 - : unit = ()
[*----------------------------------------------------------------------------*)

let rec dict_insert key value = function
     | Prazno -> Veja (Prazno, (key, value), Prazno)
     | Veja (l, (k, v), d) when key = k -> Veja (l, (k, value), d)
     | Veja (l, (k, v), d) when key < k -> Veja (dict_insert key value l, (k, v), d)
     | Veja (l, (k, v), d) -> Veja (l, (k, v), dict_insert key value d)
