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
  |prvi::tail -> (List.map (fun x -> prvi*x) q) +++ ([0] @ (tail *** q))

