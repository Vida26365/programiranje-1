(* OCaml *)
let sez = [1; 2; 3; 4]
let sez2 = 0::sez
(* sez2 kaže na glavo 0, ki kaže naprej na sez. sez2 je narejen v O(1) času *)

let nov_sez = List.rev sez
(* Ustavri nov list, ki ima elemente v obratnem vrstnem redu *)