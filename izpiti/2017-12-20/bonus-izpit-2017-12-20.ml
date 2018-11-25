(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame par in zamenja komponenti para.
   Primer: /obrni (2, 4) = (4, 2)/ *)
 let obrni (x, y) = (y, x)

(* 1.2) Definirajte funkcijo, ki vzame par p in vrednost x in zamenja drugo
   komponento para p z x.
   Primer: /zamenjaj_drugo (2, 4) 7 = (2, 7)/ *)
 let zamenjaj_drugo (x, y) z = (x, z)

(* 1.3) Definirajte funkcijo, ki vzame seznam parov in izračuna nov seznam parov,
   ki imajo drugo komponento zamenjano z 42.
   Primer: /vsem_zamenjaj_drugo_z_42 [(12, 1); (2, 4)] = [(12, 42); (2, 42)]/ *)
 let vsem_zamenjaj_drugo_z_42 sez =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> aux (zamenjaj_drugo x 42 :: acc) xs
    in
    aux [] sez

(* 1.4) Definirajte funkcijo, ki varno vrne glavo seznama v primeru, ko seznam ni prazen.
   Uporabite tip option.
   Primer: /glava [1; 2; 3] = Some 1/ *)
 let glava = function
  | [] -> None
  | x :: xs -> Some x

(* 1.5) Definirajte funkcijo, vzame funkcijo (f: 'a -> 'b), neko vrednost (x : 'a) in
   celo število n. Funkcija naj vrne vrednost, ki jo dobimo če f n-krat uporabimo na x,
   torej f (f ... (f x)...).
   Primer: /uporabi_veckrat succ 0 420 = 420/ *)
 let uporabi_veckrat f x n = 
  let rec aux acc n = 
    if n = 0 then acc else aux (f acc) (n-1)
    in
    aux x n

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)
type 'a drevo = Gozd of 'a * 'a drevo list

(* 2.2) Definirajte naslednja rožna drevesa:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

 let t = Gozd (1, [])
 let t' = Gozd (2, [t; t])
 let t'' = Gozd (3, [Gozd (-1, []); t';Gozd (0, [])])

(* 2.3) Definirajte funkcijo, ki vrne gozd rožnega drevesa. *)
let vrni_gozd = function
    | Gozd (k, sez) -> sez

(* 2.4) Definirajte funkcijo, ki izpiše vse vrednosti v rožnem drevesu celih števil.
   Števila naj bodo v ločenih vrsticah. Uporabite (print_int : int -> unit) in
   (print_newline : unit -> unit). *)
let izpisi_vrednosti = function
    | Gozd (k, []) -> print_newline (print_int k)
    | Gozd (k, xs) -> 
    let rec aux acc = function
    | [] -> print_newline (print_int acc)
    | Gozd (k, []) :: [] -> print_newline (print_int acc); aux k []
    | Gozd (k, x) :: xs -> print_newline (print_int acc); aux k (x @ xs)
    in
    aux k xs

(* 2.5) Definirajte funkcijo, ki izračuna globino rožnega drevesa, t.j. dolžino
   najdaljše poti od korena do lista. *)

let rec max_list = function
    | [] -> 0
    | x :: [] -> x
    | x :: xs -> max x (max_list xs)

let rec  globina = function
    | Gozd (k, []) -> 1
    | Gozd (_, xs) -> 1 + max_list (List.map globina xs)

(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)
let globoko_drevo n = 
  let rec aux acc = function
  | n when n = 0 -> acc
  | n -> aux (Gozd (n, [acc])) (n-1)
  in
  aux t (n-1) 

(* 2.7) Definirajte funkcijo, ki sprejme funkcijo (f : 'b -> 'a -> 'b) in začetno vrednost (acc : 'b)
   in funkcijo f zloži [fold] preko drevesa (t : 'a drevo). Vrstni red pri tem ni pomemben.

   Za primer t' želimo vrednost f (f (f acc 1) 2) 2)  (lahko tudi f (f (f acc 2) 1) 2))
   Primer: /zlozi (fun acc x -> x + acc) 0 t'' = 6/

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)

let rec zlozi f acc = function
  | Gozd (k, xs) -> 
  let rec aux acc l = function
  | [] -> f acc l
  | Gozd (k, x) :: xs -> aux (f acc l) (k) (x @ xs)
  in
  aux acc k xs
