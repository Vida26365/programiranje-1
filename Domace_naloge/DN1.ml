let rec stevke b n = 
  match n with
  | 0 -> []
  | _ ->  stevke b (n/b)@[n mod b]


let rec take n sez = 
  match n with
  | 0-> []
  | _ -> 
    match sez with
    | [] -> []
    | head:: tail -> [head] @ (take (n-1) tail)


let rec drop_while f sez = 
  match sez with
  |[] -> []
  | prvi::tail ->
    match f prvi with
    |true -> drop_while f (List.tl sez)
    |false -> sez


let filter_mapi f sez = 
  let rec pom f sez i = 
    match sez with
    | [] -> []
    | prvi::tail ->[f i prvi] @ pom f tail (i+1) in
  List.filter_map (fun x -> x) (pom f sez 0)


(* IZOMORFIZMI_______________________________________________________________________________ *)
let phi1 (a,b) = (b,a)  
let psi1 = phi1

let phi3 (a, (b,c))= ((a, b), c) 
let phi3 ((a, b), c) = (a, (b,c))

let phi7 f = (fun x -> fst (f x), fun x -> snd (f x))
let psi7 (f, g) = fun x -> (f x, g x)

(* POLINOMI__________________________________________________________________________________ *)

type polinom = int list

let rec pocisti (sez: polinom) : polinom = 
  match (List.rev sez) with
  |[] -> []
  |prvi::tail ->
    match prvi with
    | 0-> pocisti tail
    | _ -> sez

let rec make n a =
  match n with
  | 0 -> []
  | _ -> a @ (make (n-1) a)

let ( +++ ) (p:polinom) (q:polinom) =
  let rec dodaj h =
    let manj = min (List.length p) (List.length q) in
    let vec = max (List.length p) (List.length q) in
    match (List.length h) with
    | x when x = manj -> h @ (make (vec -manj) [0])
    | _ -> h 
  in
  let v1 = dodaj p in
  let v2 = dodaj q in
  pocisti (List.map2 (fun x y -> x+y) v1 v2)


let rec ( *** ) p q =
  match p with
  |[] -> []
  |prvi::tail -> pocisti (List.map (fun x -> prvi*x) q) +++ ([0] @ (tail *** q))

let rec vrednost (sez:polinom) a = 
  match sez with
  | [] -> 0
  | prvi::tail -> prvi + a * vrednost tail a

let odvod (sez: polinom) =
  let rec seznamaj sez i =
    match sez with
    | [] -> []
    |x when (List.length x) = i -> [i]
    | prvi::tail -> [i] @ (seznamaj tail (i+1)) in 
  match sez with
  | [] -> []
  | prvi::tail -> pocisti (List.map2 ( * ) tail (seznamaj sez 1))




let rec izpis (sez:polinom) =

  let rec seznamaj sez i =
    match sez with
    | [] -> []
    (* |x when (List.length x) +2 = i -> [] *)
      (* ["x^"^ (string_of_int i)] *)
    | prvi::tail -> 
      match i with
      | 0 -> [""] @ (seznamaj tail (i+1))
      | 1 -> ["x"] @ (seznamaj tail (i+1))
      | _ -> ["x^" ^ (string_of_int i)] @ (seznamaj tail (i+1))
    in

  let funkcioniraj a xi =
    match a with
    |0 -> ""
    | 1 -> 
      (match xi with
      | "" ->  " + 1"
      | _ -> " + "  ^  xi)
    | -1 ->
      (match xi with
      | "" ->  " - 1"
      | _ -> " - "  ^  xi)
    |x when x < 0 -> " - "  ^ (string_of_int (abs a))^ xi
    |x when x > 0 -> " + "  ^ (string_of_int (abs a))^ xi
    |_ -> ""
    
  in

  let funkcija_prvih a = 
    let xi = (List.hd (List.rev (seznamaj sez 0))) in
    match a with
    |0 -> ""
    | 1 -> xi
    | -1 -> "-"  ^  xi
    |x when x < 0 -> "-"  ^ (string_of_int a)^ xi
    |x when x > 0 ->(string_of_int a)^ xi 
    |_ -> ""
  in

  let skoraj_koncni_list = List.rev (List.map2 funkcioniraj sez (seznamaj sez 0))
  in
  match skoraj_koncni_list with
  |[] -> ""
  |_::tail ->
    let prvi::_ = List.rev sez in
    (funkcija_prvih prvi) ^ (String.concat "" tail)

  


