-- Izomorfizmi

theorem eq1 {A B : Prop} : (A ∧ B) ↔ (B ∧ A) :=
  by
    apply Iff.intro
    . intro ainb
      constructor
      exact ainb.right
      exact ainb.left

    . intro bina
      constructor
      exact bina.right
      exact bina.left


theorem eq2 {A B : Prop} : (A ∨ B) ↔ (B ∨ A) :=
  by
    constructor
    . intro avb
      cases avb with
      | inl  a =>
        apply Or.inr --iz a v b ali a
        assumption
      | inr b =>
        apply Or.inl
        exact b
    . intro bva
      cases bva with
      | inl b =>
        apply Or.inr
        exact b
      | inr a =>
        apply Or.inl
        exact a



    -- have xx := avb (Or.inl a)
theorem eq3 {A B C : Prop} : (A ∧ (B ∧ C)) ↔ (B ∧ (A ∧ C)) :=
  by
    apply Iff.intro
    intro abc
    have a := abc.left
    have b := abc.right.left
    have c := abc.right.right
    apply And.intro
    exact b
    apply And.intro
    assumption
    assumption

    intro bac
    apply And.intro
    exact bac.right.left
    apply And.intro
    exact bac.left
    exact bac.right.right




    -- sorry

theorem eq4 {A B C : Prop} : (A ∨ (B ∨ C)) ↔ (B ∨ (A ∨ C)) :=
 sorry

theorem eq5 {A B C : Prop} : A ∧ (B ∨ C) ↔ (A ∧ B) ∨ (A ∧ C) :=
  sorry

theorem eq6 {A B C : Prop} : (B ∨ C) → A ↔ (B → A) ∧ (C → A) :=
  by
    apply Iff.intro
    intro h
    constructor

    intro b
    apply h
    left
    exact b

    intro c
    have xx := h (Or.inr c)

    exact xx

    intro h bvc
    cases bvc
    case inl b =>
      exact h.left b
    case inr c =>
      exact h.right c



theorem eq7 {A B C : Prop} : C → (A ∧ B) ↔ (C → A) ∧ (C → B) :=
  sorry
