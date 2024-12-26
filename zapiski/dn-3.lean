set_option autoImplicit false

/------------------------------------------------------------------------------
 ## Naravna števila

 Definirajte funkcijo, ki _rekurzivno_ (torej naivno in ne direktno s formulo,
 ki jo boste morali dokazati) sešteje prvih `n` naravnih števil, ter
 dokažite, da zanjo velja znana enakost (najprej v obliki, ki ne zahteva
 deljenja, nato pa še v običajni obliki).
------------------------------------------------------------------------------/

def vsota_prvih  : Nat -> Nat :=
fun n =>
  match n with
  | Nat.zero => Nat.zero
  | Nat.succ k => n + vsota_prvih k

theorem gauss : (n : Nat) → 2 * vsota_prvih n = n * (n + 1) := by
  intro n
  induction n with
  | zero =>
    rw [vsota_prvih]
  | succ k ih =>
    simp [vsota_prvih]
    rw [Nat.mul_add]
    rw [ih]

    calc 2 * (k + 1) + k * (k + 1) =
      (2 + k) * (k + 1) := by simp [Nat.add_mul]
    _ =  (k + 1) * (2 + k) := by simp [Nat.mul_comm]
    _ =  (k + 1) * (k + 2) := by simp [Nat.add_comm]


theorem cisto_pravi_gauss : (n : Nat) → vsota_prvih n = (n * (n + 1)) / 2 := by
  intro n
  calc vsota_prvih n = ( 2 * vsota_prvih n) / 2 := by simp
  _ = (n * (n + 1)) / 2 := by simp [gauss]

/------------------------------------------------------------------------------
 ## Vektorji

 Definirajmo vektorje podobno kot na predavanjih, le da namesto svojih naravnih
 števil uporabimo vgrajena. Da se tipi ujamejo, funkcijo stikanja napišemo s
 pomočjo taktik.

 Napišite funkcijo `obrni`, ki vrne na glavo obrnjen vektor, ter funkciji
 `glava` in `rep`, ki varno vrneta glavo in rep _nepraznega_ seznama.
------------------------------------------------------------------------------/

inductive Vektor : Type → Nat → Type where
  | prazen : {A : Type} → Vektor A 0
  | sestavljen : {A : Type} → {n : Nat} → A → Vektor A n → Vektor A (n + 1)
deriving Repr

def stakni : {A : Type} → {m n : Nat} → Vektor A m → Vektor A n → Vektor A (m + n) :=
  fun xs ys => match xs with
  | .prazen => by rw [Nat.add_comm]; exact ys
  | .sestavljen x xs' => by rw [Nat.add_right_comm]; exact Vektor.sestavljen x (stakni xs' ys)

def obrni : {A : Type} → {n : Nat} → Vektor A n → Vektor A n :=
  fun vec =>
  match vec with
  | .prazen => Vektor.prazen
  | .sestavljen prvi tail => stakni (obrni tail) (Vektor.sestavljen prvi Vektor.prazen)


def glava : {A : Type} → {n : Nat} → Vektor A n → Option A :=
  fun vec => match vec with
  | .prazen => Option.none
  | .sestavljen x _ => Option.some x

def rep : {A : Type} → {n : Nat} → Vektor A n →  Option (Vektor A (n - 1)):=
  fun vec => match vec with
  | .prazen => Option.none
  | .sestavljen _ xs => Option.some xs

/------------------------------------------------------------------------------
 ## Predikatni račun

 Dokažite spodnje tri trditve. Zadnja je _paradoks pivca_, ki pravi:
   "V vsaki neprazni gostilni obstaja gost, za katerega velja,
   da če pije on, pijejo vsi v gostilni."
 Za dokaz potrebujete klasično logiko, torej nekaj iz modula `Classical`.
------------------------------------------------------------------------------/

theorem forall_implies : {A : Type} → {P Q : A → Prop} →
  (∀ x, (P x → Q x)) → (∀ x, P x) → (∀ x, Q x) := by
  intro x P Q vpvq vp x
  apply vpvq
  apply vp

theorem forall_implies' : {A : Type} → {P : Prop} → {Q : A → Prop} →
  (∀ x, (P → Q x)) ↔ (P → ∀ x, Q x) := by
  intro x P Q
  constructor
  intro vpq p2 x2
  apply vpq
  assumption
  intro pvq x p
  apply pvq
  assumption


theorem paradoks_pivca :
  {G : Type} → {P : G → Prop} →
  (g : G) →  -- (g : G) pove, da je v gostilni vsaj en gost
  ∃ (p : G), (P p → ∀ (x : G), P x) := by

  intro G P g
  apply Classical.byCases
  . intro wtf
    exists g

  . intro g_pije_ne_pijejo_vsi
    have ne_pijejo_vsi : ¬ ∀ (x : G), P x := by
      intro piejo_vsi
      apply g_pije_ne_pijejo_vsi
      intro _
      exact piejo_vsi
    have obstaja_da_ne_pije : ∃(x: G), ¬ P x := by
      apply Classical.not_forall.mp
      exact ne_pijejo_vsi
    have ⟨ p, nPp ⟩ := obstaja_da_ne_pije
    exists p
    intro Pp
    apply Classical.byContradiction
    intro _
    exact nPp Pp



/------------------------------------------------------------------------------
 ## Dvojiška drevesa

 Podan naj bo tip dvojiških dreves skupaj s funkcijama za zrcaljenje in izračun
 višine ter dvema funkcijama, ki obe od leve proti desni naštejeta elemente
 drevesa. Pri tem prva deluje naivno in ima časovno zahtevnost O(n log n), druga
 pa je malo bolj zapletena in deluje v času O(n). Dokažite spodnje enakosti, pri
 čemer lahko do pomožne funkcije `aux` dostopate kot `elementi'.aux`
-------------------------------------------------------------------------------/

inductive Drevo : Type → Type where
  | prazno : {A : Type} → Drevo A
  | sestavljeno : {A : Type} → Drevo A → A → Drevo A → Drevo A

def zrcali : {A : Type} → Drevo A → Drevo A :=
  fun t => match t with
  | .prazno => .prazno
  | .sestavljeno l x d => .sestavljeno (zrcali d) x (zrcali l)

def visina : {A : Type} → Drevo A → Nat :=
  fun t => match t with
  | .prazno => 0
  | .sestavljeno l _ d => 1 + max (visina l) (visina d)

def elementi : {A : Type} → Drevo A → List A :=
  fun t => match t with
  | .prazno => []
  | .sestavljeno l x d => elementi l ++ x :: elementi d

def elementi' : {A : Type} → Drevo A → List A :=
  let rec aux : {A : Type} → Drevo A → List A → List A :=
    fun t acc => match t with
    | .prazno => acc
    | .sestavljeno l x d => aux l (x :: aux d acc)
  fun t => aux t []


theorem zrcali_zrcali :
  {A : Type} → (t : Drevo A) →
  zrcali (zrcali t) = t := by
  intro A t
  induction t with
  | prazno => simp [zrcali]
  | sestavljeno l x r ihl ihr=>
    simp [zrcali]
    constructor
    rw [ihl]
    rw [ihr]

theorem visina_zrcali :
  {A : Type} → (t : Drevo A) →
  visina (zrcali t) = visina t := by
  intro A t
  induction t with
  | prazno => simp [visina]
  | sestavljeno l x r il ir =>
    simp [visina]
    simp [il, ir]
    simp [Nat.max_comm]

theorem lema : {A : Type} -> {t : Drevo A} -> {sez : List A} ->
  elementi'.aux t sez = elementi t ++ sez := by
  intros A t
  induction t with
  | prazno =>
    simp [elementi'.aux]
    simp [elementi]
  | sestavljeno  l x r ihl ihr =>
    simp [elementi]
    simp [elementi'.aux]
    intro sez
    rw [ihl]
    rw [ihr]

theorem elementi_elementi' :
  {A : Type} → (t : Drevo A) →
  elementi t = elementi' t := by
  intros A t
  simp [elementi']
  induction t with
  | prazno =>
    simp [elementi, elementi'.aux]
  | sestavljeno l a r ihl ihr =>
    rw [elementi]
    rw [ihl]
    rw [elementi'.aux]
    rw [<-ihr]
    simp [lema]
