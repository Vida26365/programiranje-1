(*----------------------------------------------------------------------------*
 # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste napisali svoj simulator Turingovih strojev. Zaradi
 preprostosti bomo za abecedo vzeli kar znake tipa `char`, za prazni znak bomo
 izbrali presledek `' '`, stanja pa bomo predstavili z nizi. Za možne premike
 zafiksiramo tip `direction`:
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

(*----------------------------------------------------------------------------*
 ## Implementacija trakov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Tape`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip v obe smeri neomejenih trakov in glavo na danem mestu;
 - `make`, ki naredi nov trak z znaki iz niza ter glavo na prvem znaku;
 - `read`, ki vrne znak pod glavo;
 - `write`, ki pod glavo zapiše dani znak;
 - `move`, ki glavo premakne v dano smer;
 - `print`, ki izpiše vsebino traku (brez presledkov na začetku in koncu) ter
 pod njim z `^` označi mesto glave.

 Zadnji dve funkciji naj vrneta nov trak, obstoječega pa naj pustita
 nespremenjenega.

 Ker je tip `t` abstrakten, si lahko privoščite poljubno implementacijo, zato
 poskrbite tako za učinkovitost kot za preglednost kode.
[*----------------------------------------------------------------------------*)

module type TAPE = sig
  type t

  val make : string -> t
  val move : direction -> t -> t
  val read : t -> char
  val write : char -> t -> t
  val print : t -> unit
end

module Tape : TAPE = struct
  type t = { p : char list; g : char; z : char list }

  let make str = 
    match (String.to_seq str |> List.of_seq) with
    | [] -> { p = []; g = ' '; z = [] }
    | glava::tail -> { p = []; g = glava; z = tail }
  let move (s : direction) (t : t) = 
    match s with
    | Left -> 
      (match t.p with
      | [] -> { p = []; g = ' '; z = t.g::t.z }
      | x::xs -> if t.g = ' ' && t.z = [] then {p = xs; g = x; z = []} else {p = xs; g = x; z = t.g::t.z})
    | Right -> 
      match t.z with
      | [] -> {p = t.g::t.p; g = ' '; z = []}
      | x::xs -> if t.g = ' ' && t.p = [] then {p = []; g = x; z = xs} else {p = t.g::t.p; g = x; z = xs}
  let read t = t.g
  let write c t = { t with g = c }
  let print t = 
    print_string (List.to_seq (List.rev t.p) |> String.of_seq);
    print_char t.g;
    print_string (List.to_seq t.z |> String.of_seq);
    print_newline ();
    print_string (String.make (List.length t.p) ' ');
    print_char '^';
    print_newline ()
end

let primer_trak = Tape.(
  make "A    B  CDE"
  |> move Left
  |> move Left
  |> move Right
  |> move Right
  |> move Right
  |> move Right
  |> move Right
  |> move Right
  |> move Right
  |> write '!'
  |> print
)
(*
AB!DE
  ^
*)
(* val primer_trak : unit = () *)

(*----------------------------------------------------------------------------*
 ## Implementacija Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Machine`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip Turingovih strojev;
 - `make`, ki naredi nov stroj z danim začetnim stanjem in seznamom preostalih
 stanj ter prazno prehodno funkcijo;
 - `initial`, ki vrne začetno stanje stroja;
 - `add_transition`, ki prehodno funkcijo razširi s prehodom $(q, a) \mapsto
 (q', a', d)$;
 - `step`, ki za dano stanje in trak izvede en korak stroja, če je to mogoče.

 Zadnji dve funkciji naj vrneta spremenjene vrednosti, obstoječe argumente pa
 naj pustita nespremenjene. Prav tako pri zadnjih dveh funkcijah lahko
 predpostavite, da ju bomo klicali le na poprej podanih stanjih.

 Tudi tu je tip `t` abstrakten, zato poskrbite za učinkovitost in preglednost
 kode.
[*----------------------------------------------------------------------------*)
module type MACHINE = sig
  type t
  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
  val copy : t -> t
end


module StChH = Hashtbl.Make (
  struct
    type t = state * char
    let equal (s, c) (st, ch) = (String.equal s st) && (Char.equal c ch)
    let hash = Hashtbl.hash
  end
)


module Machine : MACHINE = struct
  type t = {
    initial : state;
    states : state list;
    transitions : (state * char * direction) StChH.t
  }
  let make st stlst = {
    initial = st;
    states = stlst;
    transitions =  StChH.create (List.length stlst)

  }
  let initial rc = rc.initial
  let add_transition st ch s c d rc = 
    StChH.replace rc.transitions (st, ch) (s, c, d);
    rc

  let step rc st tp =
    match StChH.find_opt rc.transitions (st, Tape.read tp) with
    | None -> None
    | Some (s, c, d) -> Some (s, Tape.move d (Tape.write c tp))

  let copy m = { m with transitions = StChH.copy m.transitions }

end

(*----------------------------------------------------------------------------*
 Primer stroja "Binary Increment" na <http://turingmachine.io> lahko
 implementiramo kot:
[*----------------------------------------------------------------------------*)

let binary_increment =
  Machine.(
    make "right" [ "carry"; "done" ]
    |> add_transition "right" '1' "right" '1' Right
    |> add_transition "right" '0' "right" '0' Right
    |> add_transition "right" ' ' "carry" ' ' Left
    |> add_transition "carry" '1' "carry" '0' Left
    |> add_transition "carry" '0' "done" '1' Left
    |> add_transition "carry" ' ' "done" '1' Left
  )

(* val binary_increment : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 Zapišite funkciji `slow_run` in `speed_run` tipa `Machine.t -> str -> unit`, ki
 simulirata Turingov stroj na traku, na katerem je na začetku zapisan dani niz.
 Prva naj izpiše trakove in stanja pri vseh vmesnih korakih, druga pa naj izpiše
 le končni trak. Slednjo bomo uporabljali tudi pri meritvi učinkovitosti
 izvajanja.
[*----------------------------------------------------------------------------*)

let slow_run m niz =
  let tp = Tape.make niz in
  let st = Machine.initial m in
  let rec pom stt tp =
    match Machine.step m stt tp with
    | None -> Tape.print tp
    | Some (st, tap) -> 
      Tape.print tp;
      print_endline stt;
      pom st tap
    in
  pom st tp
    
    (* let stt, tap = Machine.step m (Machine.initial m) tp |> Option.get in *)
  

(* let primer_slow_run =
  slow_run binary_increment "1011" *)
(*
1011
^
right
1011
  ^
right
1011
  ^
right
1011
    ^
right
1011
    ^
right
1011
    ^
carry
1010
  ^
carry
1000
  ^
carry
1100
^
done
*)
(* val primer_slow_run : unit = () *)

let speed_run m niz = 
  let tp = Tape.make niz in
  let st = Machine.initial m in
  let rec pom stt tp =
    match Machine.step m stt tp with
    | None -> Tape.print tp
    | Some (st, tap) -> pom st tap
    in
  pom st tp

(* let primer_speed_run =
  speed_run binary_increment "1011" *)
(*
1100
^
*)
(* val primer_speed_run : unit = () *)

(*----------------------------------------------------------------------------*
 ## Krajši zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ko definiramo Turingov stroj, prehode običajno združujemo najprej po stanjih,
 nato pa še po znakih. Prav tako pri dosti prehodih samo premikamo glavo, trak
 in stanje pa pustimo pri miru. Zapišite funkcije:

 - `for_state`
 - `for_character`
 - `for_characters`
 - `move`
 - `switch_and_move`
 - `write_and_move`
 - `write_switch_and_move`

 s katerimi bi lahko zgornji primer na krajše zapisali kot spodaj.
 Implementacijo in tipe ugotovite sami.
[*----------------------------------------------------------------------------*)
let for_state (stt : state) (sez : (state -> Machine.t -> Machine.t) list list) (m: Machine.t) : Machine.t = 
  let funkcije = List.flatten sez in
  let rec po_funkcijah m = 
    function
    | [] -> m
    | f::fuss -> po_funkcijah (f stt m) fuss
  in
  po_funkcijah m funkcije

let for_character (ch : char) (f : char -> state -> Machine.t -> Machine.t) : (state -> Machine.t -> Machine.t) list= [f ch]
let for_characters (str : string) (f : char -> state -> Machine.t -> Machine.t) (* gor se kliče še state -> char -> machine *) = (*TODO*)
  String.to_seq str
  |> List.of_seq
  |> List.map f
  

let move (d : direction)(ch : char) (stt: state) : (Machine.t -> Machine.t)  = Machine.add_transition stt ch stt ch d (*še machine.t*)
let switch_and_move (stt : state) (d : direction)(ch : char) (st : state) : (Machine.t -> Machine.t) = Machine.add_transition st ch stt ch d
let write_and_move (ch : char) (d : direction) (chr : char) (stt: state) : (Machine.t -> Machine.t) = Machine.add_transition stt chr stt ch d
let write_switch_and_move (ch : char) (stt : state) (d : direction) (chr : char) (st: state) : (Machine.t -> Machine.t) = Machine.add_transition st chr stt ch d


let binary_increment' =
  Machine.make "right" ["carry"; "done"]
  |> for_state "right" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "carry" Left
  ]
  |> for_state "carry" [
    for_character '1' @@ switch_and_move "carry" Left;
    for_characters "0 " @@ write_switch_and_move '1' "done" Left
  ]
(* val binary_increment' : Machine.t = <abstr> *)

let primer_binary_increment' = speed_run binary_increment' "1011"

(*----------------------------------------------------------------------------*
 ## Primeri Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste sestavljali stroje, ki bodo iz začetnega niza na traku na
 različne načine izračunali nov niz. Pri tem lahko predpostavite, da je začetni
 niz sestavljen iz ničel in enic, preostanek traku pa je prazen. Na koncu
 izvajanja naj bo glava na začetku novega niza, z izjemo tega niza pa naj bo
 trak prazen. Ni pa treba, da se izračunani niz začne na istem mestu na traku,
 kot se je začel prvotni niz.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Obračanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki začetni niz obrne na glavo.
[*----------------------------------------------------------------------------*)
let reverse = 
  Machine.make "prva_izvidnica" []
  |> for_state "prva_izvidnica" [
    for_character '!' @@ move Right;
    for_characters "01" @@ switch_and_move "izvidnica" Right;
  ]
  |> for_state "izvidnica" [
    for_characters "01" @@ switch_and_move "postavljanje_tracnic" Left;
    for_character '!' @@ move Right;
    for_character ' ' @@ switch_and_move "aaaa_indijanci,_pospravi_stvari_in_tracnice!" Left
  ]
  |> for_state "postavljanje_tracnic" [
    for_character '1' @@ write_switch_and_move '!' "odlaganje_vrednih_kamnin" Left;    
    for_character '0' @@ write_switch_and_move '!' "odlaganje_nicvrednih_kamnin" Left
  ]
  |> for_state "odlaganje_vrednih_kamnin" [
    for_characters "01!" @@ move Left;
    for_character ' ' @@ write_switch_and_move '1' "nazaj_do_tracnic" Right
  ]
  |> for_state "odlaganje_nicvrednih_kamnin" [
    for_characters "01!" @@ move Left;
    for_character ' ' @@ write_switch_and_move '0' "nazaj_do_tracnic" Right
  ]
  |> for_state "nazaj_do_tracnic" [
    for_characters "01" @@ move Right;
    for_character '!' @@ switch_and_move "prva_izvidnica" Right
  ]
  |> for_state "aaaa_indijanci,_pospravi_stvari_in_tracnice!" [
    for_character '1' @@ write_switch_and_move ' ' "pospravi_tracnice_in_odnesi_vredne_kamnine" Left;
    for_character '0' @@ write_switch_and_move ' ' "pospravi_tracnice_in_odnesi_nicvredne_kamnine" Left;
  ]
  |> for_state "pospravi_tracnice_in_odnesi_vredne_kamnine" [
    for_characters "01" @@ move Left;
    for_character '!' @@ write_and_move ' ' Left;
    for_character ' ' @@ write_switch_and_move '1' "fjuhhh,_ubezali_smo_jim" Right;
  ]
  |> for_state "pospravi_tracnice_in_odnesi_nicvredne_kamnine" [
    for_characters "01" @@ move Left;
    for_character '!' @@ write_and_move ' ' Left;
    for_character ' ' @@ write_switch_and_move '0' "fjuhhh,_ubezali_smo_jim" Right;
  ]
  |> for_state "fjuhhh,_ubezali_smo_jim" [
    for_characters "01 " @@ switch_and_move "mirno smo na zacetku" Left
  ]

let primer_reverse = speed_run reverse "0000111001"
(* 
1001110000          
^
*)
(* val primer_reverse : unit = () *)

(*----------------------------------------------------------------------------*
 ### Podvajanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki podvoji začetni niz.
[*----------------------------------------------------------------------------*)

let duplicate = 
  Machine.make "a prvi beri" []
  |> for_state "a prvi beri" [
    for_character '0' @@ write_switch_and_move ' ' "b prvi beri 0" Right;
    for_character '1' @@ write_switch_and_move ' ' "c prvi beri 1" Right;
  ]
  |> for_state "b prvi beri 0" [
    for_character '0' @@ write_switch_and_move ' ' "d prvi pojdi pisi 0 0" Right;
    for_character '1' @@ write_switch_and_move ' ' "e prvi pojdi pisi 0 1" Right;
  ]
  |> for_state "c prvi beri 1" [
    for_character '0' @@ write_switch_and_move ' ' "f prvi pojdi pisi 1 0" Right;
    for_character '1' @@ write_switch_and_move ' ' "g prvi pojdi pisi 1 1" Right;
  ]
  |> for_state "d prvi pojdi pisi 0 0" [
    for_characters "01" @@ move Right; 
    for_character ' ' @@ write_switch_and_move '!' "h pisi 00 00" Right
  ]
  |> for_state "e prvi pojdi pisi 0 1" [
    for_characters "01" @@ move Right; 
    for_character ' ' @@ write_switch_and_move '!' "i pisi 00 11" Right
  ]
  |> for_state "f prvi pojdi pisi 1 0" [
    for_characters "01" @@ move Right; 
    for_character ' ' @@ write_switch_and_move '!' "j pisi 11 00" Right
  ]
  |> for_state "g prvi pojdi pisi 1 1" [
    for_characters "01" @@ move Right; 
    for_character ' ' @@ write_switch_and_move '!' "k pisi 11 11" Right
  ]
  |> for_state "h pisi 00 00" [
    for_characters "01!" @@ move Right;
    for_character ' ' @@ write_switch_and_move '0' "l pisi 0 00" Right
  ]
  |> for_state "i pisi 00 11" [
    for_characters "01!" @@ move Right;
    for_character ' ' @@ write_switch_and_move '0' "m pisi 0 11" Right
  ]
  |> for_state "j pisi 11 00" [
    for_characters "01!" @@ move Right;
    for_character ' ' @@ write_switch_and_move '1' "n pisi 1 00" Right
  ]
  |> for_state "k pisi 11 11" [
    for_characters "01!" @@ move Right;
    for_character ' ' @@ write_switch_and_move '1' "o pisi 1 11" Right
  ]
  |> for_state "l pisi 0 00" [
    for_character ' ' @@ write_switch_and_move '0' "p pisi 00" Right
  ]
  |> for_state "m pisi 0 11" [
    for_character ' ' @@ write_switch_and_move '0' "r pisi 11" Right
  ]
  |> for_state "n pisi 1 00" [
    for_character ' ' @@ write_switch_and_move '1' "p pisi 00" Right
  ]
  |> for_state "o pisi 1 11" [
    for_character ' ' @@ write_switch_and_move '1' "r pisi 11" Right
  ]
  |> for_state "p pisi 00" [
    for_character ' ' @@ write_switch_and_move '0' "s pisi 0" Right
  ]
  |> for_state "r pisi 11" [
    for_character ' ' @@ write_switch_and_move '1' "q pisi 1" Right
  ]
  |> for_state "s pisi 0" [
    for_character ' ' @@ write_switch_and_move '0' "t pojdi beri" Left
  ]
  |> for_state "q pisi 1" [
    for_character ' ' @@ write_switch_and_move '1' "t pojdi beri" Left
  ]
  |> for_state "t pojdi beri" [
    for_characters "01!" @@ move Left;
    for_character ' ' @@ switch_and_move "u beri" Right
  ]
  |> for_state "u beri" [
    for_character '0' @@ write_switch_and_move ' ' "v beri 0" Right;
    for_character '1' @@ write_switch_and_move ' ' "z beri 1" Right;
    for_character '!' @@ write_switch_and_move ' ' "x koncano na glavi" Right
  ]
  |> for_state "v beri 0" [
    for_character '0' @@ write_switch_and_move ' ' "h pisi 00 00" Right;
    for_character '1' @@ write_switch_and_move ' ' "i pisi 00 11" Right;
    for_character '!' @@ switch_and_move "pisi 00" Right
  ]
  |> for_state "z beri 1" [
    for_character '0' @@ write_switch_and_move ' ' "j pisi 11 00" Right;
    for_character '1' @@ write_switch_and_move ' ' "k pisi 11 11" Right;
    for_character '!' @@ switch_and_move "pisi 11" Right
  ]

let primer_duplicate = 
  speed_run duplicate "010011"
(* 
001100001111       
^
*)
(* val primer_duplicate : unit = () *)

(*----------------------------------------------------------------------------*
 ### Eniški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano
 v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.
[*----------------------------------------------------------------------------*)

let to_unary = 
  Machine.make "ali mogoce kdo slucajno rabi turingov stroj da mu izpise prazen niz" []
  |> for_state "ali mogoce kdo slucajno rabi turingov stroj da mu izpise prazen niz" [
    for_character '0' @@ write_switch_and_move ' ' "Resno? Resno?" Right;
    for_character '1' @@ switch_and_move "potsavi #" Right
  ] 
  |> for_state "potsavi #" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ write_switch_and_move '#' "prevzem posiljke" Left
  ]
  |> for_state "prevzem posiljke" [
    for_character '1' @@ write_switch_and_move '0' "preverim ce moram jutri spet prevzeti paket" Left;
    for_character '0' @@ write_and_move '1'  Left;
    for_character ' ' @@ switch_and_move "O ne, drzava se je odlocila privatizirati postne storitve in jih je prodala podjetju Definitivnonamjemarzaljudiinneprofit org., ki je ugotovilo da je v temu prevec zakotnem kraju premalo ljudi da bi se splacalo tu nditi storitve, zato so zaprli poslovalnico in nehali nuditi soritve na dom zato kot prebivalec tega kraja sedaj ne morem dobiti posiljke!" Right
  ]
  |> for_state "preverim ce moram jutri spet prevzeti paket" [
    for_characters "01" @@ switch_and_move  "odlozi posiljko doma" Right;
    for_character ' ' @@ switch_and_move "koncno n rabim vec prevzemati teh nadleznih paketov" Right
  ]
  |> for_state "koncno n rabim vec prevzemati teh nadleznih paketov" [
    for_character '0' @@ write_switch_and_move ' ' "odlozi posiljko doma" Right;
    for_character '1' @@ switch_and_move "odlozi posiljko doma" Right;
  ]
  |> for_state "odlozi posiljko doma" [
    for_characters "01#" @@ move Right;
    for_character ' ' @@ write_switch_and_move '1' "grem prevzet posiljko" Left
  ]
  |> for_state "grem prevzet posiljko" [
    for_characters "01" @@ move Left;
    for_character '#' @@ switch_and_move "prevzem posiljke" Left
  ]
  |> for_state "O ne, drzava se je odlocila privatizirati postne storitve in jih je prodala podjetju Definitivnonamjemarzaljudiinneprofit org., ki je ugotovilo da je v temu prevec zakotnem kraju premalo ljudi da bi se splacalo imeti odprto poslovalnico, zato so jo zaprli in nehali nuditi soritve na dom, sedaj kot prebivalec tega kraja ne morem dobiti posiljke!" [
    for_character '#' @@ write_switch_and_move ' ' "- zgodba po resničnih dogodkih (Ok, zelo prilagojenih dogodkih)" Right
  ]

let primer_to_unary = speed_run to_unary "1010"
(* 
1111111111
^
*)
(* val primer_to_unary : unit = () *)

(*----------------------------------------------------------------------------*
 ### Dvojiški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
 sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v
 dvojiškem zapisu.
[*----------------------------------------------------------------------------*)

let to_binary = 
  Machine.make "z1?" []
  |> for_state "z1?" [
    for_character '1' @@ write_switch_and_move '1' "z?" Right
  ]
  |> for_state "z?" [
    for_character '1' @@ write_switch_and_move '?' "b11" Right;
  ]
  |> for_state "b11" [
    for_character ' ' @@ move Right;
    for_character '1' @@ write_switch_and_move ' ' "b1" Right
  ]
  |> for_state "b1" [
    for_character '1' @@ write_switch_and_move ' ' "sodo?" Right;
    for_character ' ' @@ switch_and_move "liho" Left
  ]
  |> for_state "sodo?" [
    for_character '1' @@ switch_and_move "goinc" Left;
    for_character ' ' @@ switch_and_move "sodo" Left
  ]
  |> for_state "goinc" [
    for_characters " 1" @@ move Left;
    for_character '?' @@ switch_and_move "inc" Left
  ]
  |> for_state "inc" [
    for_characters " 0" @@ write_switch_and_move '1' "gob" Right;
    for_character '1' @@ write_and_move '0' Left
  ]
  |> for_state "gob" [
    for_characters "01" @@ move Right;
    for_character '?' @@ switch_and_move "b11" Right
  ]
  |> for_state "liho" [
    for_character ' ' @@ move Left;
    for_character '?' @@ write_switch_and_move '1' "finito" Left
  ]
  |> for_state "sodo" [
    for_character ' ' @@ move Left;
    for_character '?' @@ write_switch_and_move '0' "finc" Left
  ]
  |> for_state "finc" [
    for_characters " 0" @@ write_switch_and_move '1' "finito" Left;
    for_character '1' @@ write_and_move '0' Left
  ]
  |> for_state "finito"  [
    for_characters "01" @@ move Left;
    for_character ' ' @@ switch_and_move ":)" Right
  ]
let primer_to_binary = speed_run to_binary (String.make 172868378 '1')
(* 
101010                                           
^
*)
(* val primer_to_binary : unit = () *)