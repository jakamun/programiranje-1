(*dodatne vaje iz dreves*)

type 'a drevo = 
  | Prazno
  | Veja of 'a drevo * 'a * 'a drevo

let prazno = Prazno

let character_drevo = Veja (Veja (Veja (Prazno, "d", Prazno), "b", Veja (Prazno, "e", Prazno)), "a", Veja (Prazno, "c", Veja (Prazno, "f", Veja (Prazno, "g", Prazno))))

let integer_drevo = 
  let levo = Veja (Veja (Prazno, 0, Prazno), 2, Prazno) in
  let desno = Veja (Veja (Prazno, 6, Prazno), 7, Veja (Prazno, 11, Prazno))
  in
  Veja (levo, 5, desno)

(*brezvezn primer*)
let pozagaj (drevo : 'a drevo) : 'a drevo = Prazno


