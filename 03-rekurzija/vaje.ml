(*----------------------------------------------------------------------------*
 # Rekurzija
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite spodaj opisane funkcije, ne da bi uporabljali funkcije iz standardne
 knjižnice. Kjer to kažejo primeri, napišite tudi repno rekurzivne različice z
 imenom `ime_funkcije_tlrec`.
[*----------------------------------------------------------------------------*)

let rec reverse sez = 
  let rec pom sez1 sez2 =
    match sez1 with
    | [] -> []
    | prvi::tail -> pom tail (prvi::sez2) in
  pom sez []

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let rec repeat x n =
  match n with
  | 0 -> []
  | _ -> repeat x (n-1)

(*----------------------------------------------------------------------------*
 ## Funkcija `repeat`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `repeat x n` vrne seznam `n` ponovitev vrednosti `x`. Za neprimerne
  vrednosti `n` funkcija vrne prazen seznam.
[*----------------------------------------------------------------------------*)

let range n =
  let rec range_ n sez = 
    match n with
    | x when x<0 -> sez
    | _ -> range_ (n-1) (n::sez) in
  range_ n []

(*----------------------------------------------------------------------------*
 ## Funkcija `range`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `range` naj sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente naj funkcija vrne prazen
 seznam. Funkcija naj bo repno rekurzivna. Pri tem ne smete uporabiti vgrajene
 funkcije `List.init`.
[*----------------------------------------------------------------------------*)

let rec map sez f= 
  match sez with
  | [] -> []
  | prvi::tail -> [f prvi] @ (map tail f)

(*----------------------------------------------------------------------------*
 ## Funkcija `map`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `map f list` naj sprejme seznam `list` oblike `x0; x1; x2; ...` in
 funkcijo `f` ter vrne seznam preslikanih vrednosti, torej `f x0; f x1; f x2;
 ...`. Pri tem ne smete uporabiti vgrajene funkcije `List.map`.
[*----------------------------------------------------------------------------*)

let reverse sez = 
  let rec reverse_ sez acc =
    match sez with
    | [] -> acc
    | prvi::tail -> reverse_ tail (prvi::acc) in
  reverse_ sez []

(*----------------------------------------------------------------------------*
 Napišite še funkcijo `map_tlrec`, ki naj bo repno rekurzivna različica funkcije
 `map`.
[*----------------------------------------------------------------------------*)

let map_tlrec f sez = 
  let rec map_tlrec_ f sez acc =
    match sez with
    | [] -> acc
    | prvi::tail -> map_tlrec_ f tail (acc @ [f prvi]) 
  in
  map_tlrec_ f sez []

(*----------------------------------------------------------------------------*
 ## Funkcija `mapi`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `mapi` naj bo ekvivalentna Python kodi:

 ```python
 def mapi(f, lst):
     mapi_lst = []
     index = 0
     for x in lst:
         mapi_lst.append(f(index, x))
         index += 1
     return mapi_lst
 ```

 Pri tem ne smete uporabiti vgrajene funkcije `List.mapi`.
[*----------------------------------------------------------------------------*)

let rec mapi sez = 
  let rec pom sez i = 
    match sez with
    | [] -> []
    | prvi::tail -> (prvi, i) :: (pom tail (i+1)) in
  pom sez 0

(*----------------------------------------------------------------------------*
 ## Funkcija `zip`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `zip` naj sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine, naj vrne napako.
 Pri tem ne smete uporabiti vgrajene funkcije `List.combine`.
[*----------------------------------------------------------------------------*)
(* let rec zip seza sezb = 
  match seza with
  | s when (List.length s) != (List.length sezb) -> raise (Failure ("Different lengths of input lists."))
  | [] -> []
  | prvi::tail ->
    let first::rep = sezb in
    (prvi, first):: (zip tail rep) *)

let rec zip seza sezb = 
  match seza, sezb with
  | [],[] -> []
  | prvi::tail, first::rep -> (prvi, first)::(zip tail rep)
  | _,_ -> raise (Failure "Different lengths of input lists.")

(*----------------------------------------------------------------------------*
 ## Funkcija `unzip`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `unzip` je inverz funkcije `zip`, torej sprejme seznam parov
  `(x0, y0); (x1, y1); ...` in vrne par seznamov (`x0; x1; ...`, `y0; y1; ...`).
  Pri tem ne smete uporabiti vgrajene funkcije `List.split`.
[*----------------------------------------------------------------------------*)

let rec unzip sez = 
  let rec poma sez = 
    match sez with
    | [] -> []
    | (a,b)::tail -> a::(poma tail)
  
  in
  let rec pomb sez = 
    match sez with
    | [] -> []
    | (a,b)::tail -> b::(pomb tail)
  in
  ((poma sez), (pomb sez))
(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip_tlrec sez =
  let rec poma sez acc = 
    match sez with
    | [] -> acc
    | (a,b)::tail -> poma tail (acc @ [a])
  in
  let rec pomb sez acc = 
    match sez with
    | [] -> acc
    | (a,b)::tail -> pomb tail (acc @ [b])
  in
  (poma sez [], pomb sez [])

let rec unzip_tlrec sez = 
  let rec pom sez seza sezb =
    match sez with
    | [] -> (seza, sezb)
    | (a,b)::tail -> pom tail (seza@[a]) (sezb@[b])
  in
  pom sez [] []

(*----------------------------------------------------------------------------*
 ## Zanke
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `loop condition f x` naj se izvede kot python koda:

 ```python
 def loop(condition, f, x):
     while condition(x):
         x = f(x)
     return x
 ```
[*----------------------------------------------------------------------------*)

let rec loop _ _ _ = ()

let primer_loop = loop (fun x -> x < 10) ((+) 4) 4
(* val primer_loop : int = 12 *)

(*----------------------------------------------------------------------------*
 ## Funkcija `fold_left`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `fold_left_no_acc f list` naj sprejme seznam `x0; x1; ...; xn` in
 funkcijo dveh argumentov `f` in vrne vrednost izračuna `f(... (f (f x0 x1) x2)
 ... xn)`. V primeru seznama z manj kot dvema elementoma naj vrne napako.
[*----------------------------------------------------------------------------*)

let rec loop con f x = 
  match x with
  | n when not (con n) -> x
  | _ -> loop con f (f x)

(*----------------------------------------------------------------------------*
 ## Funkcija `apply_sequence`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `apply_sequence f x n` naj vrne seznam zaporednih uporab funkcije `f`
 na vrednosti `x` do vključno `n`-te uporabe, torej `[x; f x; f (f x); ...; fⁿ
 x]`. Funkcija naj bo repno rekurzivna.
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f sez = 
  match sez with
  | [prvi;drugi] -> f prvi drugi
  | prvi::drugi::tail -> fold_left_no_acc f ((f prvi drugi)::tail)
  | _ -> failwith "kashwbkj"

(*----------------------------------------------------------------------------*
 ## Funkcija `filter`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `filter f list` vrne seznam elementov `list`, pri katerih funkcija `f`
  vrne vrednost `true`.
  Pri tem ne smete uporabiti vgrajene funkcije `List.filter`.
[*----------------------------------------------------------------------------*)

let rec apply_sequence f x n =
  let rec pom f x n acc =
    match n with
    | z when (z < 0) -> []
    | 0 -> acc
    | _ -> pom f (f x) (n-1) (x::acc)
  in
  reverse (pom f x n [])

(*----------------------------------------------------------------------------*
 ## Funkcija `exists`
[*----------------------------------------------------------------------------*)

let rec filter f sez =
  match sez with
  | [] -> []
  | prvi::tail when f prvi -> prvi:: (filter tail)
  | _ -> filter tail
(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 Pri tem ne smete uporabiti vgrajene funkcije [List.find] ali podobnih.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let rec exists f sez = 
  match sez with
  | [] -> false
  | prvi::tail when f prvi -> true
  | prvi_tail -> exists f tail

(*----------------------------------------------------------------------------*
 ## Funkcija `first`
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Funkcija `first f default list` vrne prvi element seznama, za katerega
  funkcija `f` vrne `true`. Če takšnega elementa ni, vrne `default`.
  Funkcija je repno rekurzivna.
  Pri tem ne smete uporabiti vgrajene funkcije `List.find` ali podobnih.
[*----------------------------------------------------------------------------*)

let rec first _ _ _ = ()

let primer_first_1 = first ((<) 3) 0 [1; 1; 2; 3; 5; 8]
(* val primer_first_1 : int = 5 *)

let primer_first_2 = first ((<) 8) 0 [1; 1; 2; 3; 5; 8]
(* val primer_first_2 : int = 0 *)
