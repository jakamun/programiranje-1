(*Naloga1*)
(* ( a ) *)

let rec uporabi x = function
  | [] -> x
  | f :: xs -> uporabi (f x) xs

(* ( b ) *)

let rec vecaj x = function
  | [] -> x
  | f :: xs -> 
  let vmesna_vrednost = f x in
  if vmesna_vrednost > x then vecaj vmesna_vrednost xs
  else vecaj x xs

(* ( c ) *)

let rec maksimalno x = function
  | [] -> x
  | f :: xs -> 
  let uporabim = maksimalno (f x) xs in
  let neuporabim = maksimalno x xs in
  max uporabim neuporabim

(* Naloga2 *)

let 

(* Naloga3 *)

type 'a drevo = 
  | List of 'a
  | Veja of 'a drevo * 'a drevo

let test_tree = Veja (Veja (List 3, Veja (List 1, List 4)), Veja (List 1, List 5))

(* ( a ) *)

let rec sez_of_tree = function
  | List x -> [x]
  | Veja (lt, rt) -> 
  let levi_seznam = sez_of_tree lt in
  let desni_seznam = sez_of_tree rt in
  levi_seznam @ desni_seznam

(* ( b ) *)

let rec maksimum = function
  | List x -> x
  | Veja (lt, rt) -> max (maksimum lt) (maksimum rt)

let rec vsi_maksimalni drevo = 
  let maks = maksimum drevo in
  let rec aux = function
    | List x -> List maks
    | Veja (lt, rt) -> Veja (aux lt, aux rt)
    in
    aux drevo

(* ( c ) *)

(*ne dela dobro*)
let rec zamenjaj drevo seznam = 
  match drevo, seznam with
  | List a, [] -> failwith "Napaka"
  | List a, x :: xs -> List x
  | Veja (lt, rt), [] -> Veja (lt, rt)
  | Veja (lt, rt), xs -> Veja (zamenjaj lt xs, zamenjaj rt xs) 
