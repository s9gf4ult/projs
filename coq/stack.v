Require Import Bool Arith List Cpdt.CpdtTactics.
Set Implicit Arguments.
Set Asymmetric Patterns.

Inductive binop : Set := Plus | Times.

Inductive exp : Set :=
| Const: nat -> exp
| Binop: binop -> exp -> exp -> exp.

Definition binopDenote (b: binop) : nat -> nat -> nat :=
  match b with
  | Plus => plus
  | Times => mult
  end.
