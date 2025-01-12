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
end

module Machine : MACHINE = struct
  type t = {
    initial : state;
    states : state list;
    transitions : (state * (char * (state * char * direction)) list) list
  }
  let make st stlst = {
    initial = st;
    states = stlst;
    transitions = st::stlst |> List.map (fun x -> (x, []))
  }
  let initial rc = rc.initial
  let add_transition st ch s c d rc = 
    let rec po_transitions =
      function
      | [] -> { rc with transitions = (st, [(ch, (s, c, d))])::rc.transitions}
      | (stt, lst)::xs when stt = st -> { rc with transitions = (stt, (ch, (s, c, d))::lst)::xs }
      | _::xs -> po_transitions xs
    in
    po_transitions rc.transitions

  let step rc st tp =
    let rec po_transitions = 
      function
      | [] -> None
      | (stt, lst)::xs when stt = st -> 
        (let rec po_znakih = 
          function
          | [] -> None
          | (ch, (s, c, d))::xs when ch = Tape.read tp -> Some (s, Tape.move d (Tape.write c tp))
          | _::xs -> po_znakih xs
        in
        po_znakih lst)
      | _::xs -> po_transitions xs
    in
    po_transitions rc.transitions
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
(* let woops (c : char) (st : state) (m : Machine.t) = 
  print_string "char: ";
  print_char c;
  print_newline ();
  print_string "state";
  print_string st; *)

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
  Machine.make "zacetek" ["most"; "mist"; "raziskovalec"; "nabiralec"; "unicevalko"; "unicevalki"; "brodnok"; "bridnik"; "glasnok"; "glasnik"; "postar"; "done"; "Thanatos"; "zadnja_ladja"; "zadnja_kocija"; "woops_mal_prevec"]
  |> for_state "zacetek" [
    for_character '0' @@ switch_and_move "most" Left;
    for_character '1' @@ switch_and_move "mist" Left;
    for_character ' ' @@ switch_and_move "done" Left
  ]
  |> for_state "most" [
    for_characters " 01" @@ switch_and_move "glasnok" Left
  ]
  |> for_state "mist" [
    for_characters " 01" @@ switch_and_move "glasnik" Left
  ]
  |>for_state "raziskovalec" [
    for_characters "01" @@ switch_and_move "nabiralec" Right;
    for_character ' ' @@ move Right
  ]
  |> for_state "nabiralec" [
    for_character '0' @@ switch_and_move "unicevalko" Left;
    for_character '1' @@ switch_and_move "unicevalki" Left;
    for_character ' ' @@ switch_and_move "Thanatos" Left
  ]
  |> for_state "unicevalko" [
    for_characters "01 " @@ write_switch_and_move ' ' "brodnok" Left
  ]
  |> for_state "unicevalki" [
    for_characters "01 " @@ write_switch_and_move ' ' "bridnik" Left
  ]
  |> for_state "brodnok" [
    for_character ' ' @@ move Left;
    for_characters "01" @@ switch_and_move "glasnok" Left
  ]
  |> for_state "bridnik" [
    for_character ' ' @@ move Left;
    for_characters "01" @@ switch_and_move "glasnik" Left
  ]
  |> for_state "glasnok" [
    for_character ' ' @@ write_switch_and_move '0' "postar" Right;
    for_characters "01" @@ move Left
  ]
  |> for_state "glasnik" [
    for_character ' ' @@ write_switch_and_move '1' "postar" Right;
    for_characters "01" @@ move Left
  ]
  |> for_state "postar" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "raziskovalec" Right
  ]
  |> for_state "Thanatos" [
    for_characters " 01" @@ write_switch_and_move ' ' "zadnja_ladja" Left
  ]
  |> for_state "zadnja_ladja" [
    for_character ' ' @@ move Left;
    for_characters "01" @@ switch_and_move "zadnja_kocija" Left
  ]
  |> for_state "zadnja_kocija" [
    for_characters "01" @@ move Left;
    for_character ' ' @@ switch_and_move "woops_mal_prevec" Right
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
  Machine.make "ukbvs" []
  |> for_state "ukbvs" [
    for_character '0' @@ write_switch_and_move ' ' "omzu" Right;
    for_character '1' @@ write_switch_and_move ' ' "imzu" Right
  ]
  |> for_state "omzu" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ write_switch_and_move '!' "vrtio" Right
  ]
  |> for_state "imzu" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ write_switch_and_move '!' "vrtii" Right
  ]
  |> for_state "vrtio" [
    for_characters "01!" @@ move Right;
    for_character ' ' @@ write_switch_and_move '0' "vrtso" Right
  ]
  |> for_state "vrtii" [
    for_characters "01!" @@ move Right;
    for_character ' ' @@ write_switch_and_move '1' "vrtsi" Right
  ]
  |> for_state "vrtso" [
    for_character ' ' @@ write_switch_and_move '0' "sodsbundm" Left
  ]
  |> for_state "vrtsi" [
    for_character ' ' @@ write_switch_and_move '1' "sodsbundm" Left
  ]
  |> for_state "sodsbundm" [
    for_characters "01!" @@ move Left;
    for_character ' ' @@ switch_and_move "vsit" Right
  ]
  |> for_state "vsit" [
    for_character '0' @@ write_switch_and_move ' ' "vrtio" Right;
    for_character '1' @@ write_switch_and_move ' ' "vrtii" Right;
    for_character '!' @@ write_switch_and_move ' ' "tanatos" Right
  ]

let primer_duplicate = speed_run duplicate "010011"
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
  Machine.make "zacetek" ["postavi!"; "postavi"; "nazaj"; "beri"; "podvoji"; "oznaci"; "zapisi"; "izbrisi?"; "podvoji_postavi"; "oznaci_postavi"; "zapisi_postavi"; "izbrisi?_postavi"]
  |> for_state "zacetek" [
    for_character '1' @@ write_switch_and_move ' ' "postavi!" Right;
    for_character '0' @@ write_switch_and_move ' ' "done" Right
  ]
  |> for_state "postavi!" [
    for_characters "01!" @@ move Right;
    for_character ' ' @@ write_switch_and_move '!' "postavi" Right
  ]
  |> for_state "postavi" [
    for_characters "01!" @@ move Right;
    for_character ' ' @@ write_switch_and_move '1' "nazaj" Left
  ]
  |> for_state "nazaj" [
    for_characters "01!" @@ move Left;
    for_character ' ' @@ switch_and_move "beri" Right
  ]
  |> for_state "beri" [
    for_character '1' @@ write_switch_and_move ' ' "podvoji_postavi" Right;
    for_character '0' @@ write_switch_and_move ' ' "podvoji" Right;
    for_character '!' @@ write_switch_and_move ' ' "done" Right
  ]
  |> for_state "podvoji" [
    for_characters "01!" @@ move Right;
    for_character ' ' @@ switch_and_move "oznaci" Left
  ]
  |> for_state "oznaci" [
    for_character '1' @@ write_switch_and_move '?' "zapisi" Right;
    for_character '!' @@ switch_and_move "nazaj" Left
  ]
  |> for_state "zapisi" [
    for_character '1' @@ move Right;
    for_character ' ' @@ write_switch_and_move '1' "izbrisi?" Left
  ]
  |> for_state "izbrisi?" [
    for_character '1' @@ move Left;
    for_character '?' @@ write_switch_and_move '1' "oznaci" Left
  ]
  |> for_state "podvoji_postavi" [
    for_characters "01!" @@ move Right;
    for_character ' ' @@ switch_and_move "oznaci_postavi" Left
  ]
  |> for_state "oznaci_postavi" [
    for_character '1' @@ write_switch_and_move '?' "zapisi_postavi" Right;
    for_character '!' @@ switch_and_move "postavi" Left
  ]
  |> for_state "zapisi_postavi" [
    for_character '1' @@ move Right;
    for_character ' ' @@ write_switch_and_move '1' "izbrisi?_postavi" Left
  ]
  |> for_state "izbrisi?_postavi" [
    for_character '1' @@ move Left;
    for_character '?' @@ write_switch_and_move '1' "oznaci_postavi" Left
  ]

let primer_to_unary = slow_run to_unary "1010"
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
  Machine.make "klicajaj" []
  |> for_state "klicajaj" [
    for_characters "1" @@ write_switch_and_move '!' "desnanje" Right
  ]
  |> for_state "desnanje" [
    for_characters "01!" @@ move Right;
    for_character ' ' @@ switch_and_move "zbrisi_enko" Left
  ]
  |> for_state "zbrisi_enko" [
    for_character '1' @@ write_switch_and_move ' ' "goinc" Left;
    for_character '!' @@ write_switch_and_move ' ' "zadnji_inc" Left
  ]
  |> for_state "goinc" [
    for_character '1' @@ move Left;
    for_character '!' @@ switch_and_move "inc" Left
  ]
  |> for_state "inc" [
    for_characters " 0" @@ write_switch_and_move '1' "desnanje" Right;
    for_character '1' @@ write_and_move '0' Left
  ]
  |> for_state "zadnji_inc" [
    for_characters " 0" @@ write_switch_and_move '1' "na_zacetek" Left;
    for_character '1' @@ write_and_move '0' Left
  ]
  |> for_state "na_zacetek" [
    for_characters "10" @@ move Left;
    for_character ' ' @@ switch_and_move "done" Right
  ]

let primer_to_binary = speed_run to_binary (String.make 42 '1')
(* 
101010                                           
^
*)
(* val primer_to_binary : unit = () *)
