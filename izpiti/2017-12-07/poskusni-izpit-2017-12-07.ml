(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame dve celi števili ter vrne njuno vsoto.
   Primer: /sestej 2 3 = 5/ *)
let sestej n m = n + m

(* 1.2) Definirajte funkcijo, ki svojemu argumentu prišteje 3.
   Primer: /pristej_tri 10 = 13/ *)
let pristej_tri n = n + 3 

(* 1.3) Definirajte funkcijo, ki vsem elementom seznama prišteje 5.
   Primer: /vsem_pristej_pet [1; 2] = [6; 7]/ *)

let vsem_pristej_pet list =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> aux ((x+5) :: acc) xs
    in
    aux [] list

(* 1.4) Definirajte funkcijo, ki vrne zadnjo komponento nabora s tremi elementi.
   Primer: /tretji (1, "horse", [None]) = [None]/ *)
let rec tretji = function
  | (_,_,x) -> x

(* 1.5) Definirajte funkcijo, ki vzame dve funkciji ter vrne njun kompozitum.
   Primer: /kompozitum succ string_of_int 5 = "6"/ *)
let kompozitum f g x = g (f x)


(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)

(*z praznim seznamom se ustavi; prav tako ta zapis ne dopušča praznega drevesa vsaj koren mora biti*)
type 'a drevo = 
  | Gozd of 'a * 'a drevo list 

let primer = Gozd (2, [Gozd (3, []); Gozd (42, [Gozd (23, [Gozd (5, [Gozd (33, [])])])]); Gozd (7, [Gozd (4, []); Gozd (9, []); Gozd (-99, [])])])

(* 2.2) Napišite funkcijo, ki vrne koren danega rožnega drevesa. *)

let koren = function
  | Gozd (x, list) -> x

(* 2.3) Napišite funkcijo, ki preveri, ali drevo celih števil vsebuje kakšno negativno število. *)

let kaksno_negativno = function
    | Gozd (k, xs) when k < 0 -> true
    | Gozd (_, xs) -> 
    let rec aux = function
    | [] -> false
    | Gozd (k, x) :: xs when k < 0 -> true
    | Gozd (k, []) :: [] -> false
    | Gozd (k, x) :: [] -> aux x
    | Gozd (k, x) :: xs -> aux (x @ xs)
    in
    aux xs

(* 2.4) Sestavite funkcijo, ki sprejme naravno število ter sestavi (poljubno)
   drevo, ki ima toliko otrok.
   Namig: napišite pomožno funkcijo, ki ustvari poljuben seznam dane dolžine. *)

let seznam_dreves n = 
  let rec aux acc n = 
    if n=0 then acc else aux (Gozd (n, []) :: acc) (n-1)
    in
    aux [] n

let drevo_z_veliko_otroci n = 
  let xs = seznam_dreves n
  in
  Gozd (n, xs)

(* 2.5) Sestavite funkcijo, ki izračuna število vseh vozlišč v drevesu.
   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)

let velikost = function
  | Gozd (_, xs) -> 
  let rec aux acc = function
  | [] -> acc
  | Gozd (_, x) :: xs -> aux (acc + 1) (x @ xs)
  in
  aux 1 xs 
