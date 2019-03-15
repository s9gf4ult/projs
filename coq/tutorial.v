
Theorem my_first_proof : (forall A : Prop, A -> A).
Proof.
  auto.
Qed.


Theorem absurd2 : forall A C : Prop, A -> ~ A -> C.
Proof.
intros A C.
intros a nota.
pose(false := nota a).
case false.
Qed.

Require Import Bool.

Definition Is_true (b: bool) : Prop := 
  match b with 
    | true => True 
    | false => False
  end.

Lemma true_Is_true : Is_true true.
Proof.
simpl.
exact I.
Qed.

Print true_Is_true.

Lemma false_not_Is_true: ~ (Is_true false).
Proof.
simpl.
unfold not.
intros f.
exact f.
Qed.

Print false_not_Is_true.

Lemma eqb_a_a : (forall a : bool, Is_true (eqb a a)).
Proof.
intros a.
case a.
simpl.
exact I.
simpl.
exact I.
Qed.

Print eqb_a_a.

Print or.

Lemma or_commutes : (forall A B : Prop, A \/ B -> B \/ A).
Proof.
intros A B.
intros ab.
case ab.
intros a.
pose (x := or_intror a : B \/ A).
exact x.
intros b.
refine (or_introl _).
exact b.
Qed.

Print or_commutes.

Print and.

Lemma and_commutes : (forall A B : Prop, A /\ B -> B /\ A).
Proof.
intros A B.
intros ab.
case ab.
intros a b.
exact (conj b a).
Qed.

Lemma and_true_true: (forall A : Prop, A -> A /\ A).
Proof.
intros A.
intros a.
exact (conj a a).
Qed.

Print "X <-> Y".

Theorem orb_is_or : (forall a b, Is_true (orb a b) <-> Is_true a \/ Is_true b).
Proof.
intros a b.
unfold iff.
refine (conj _ _).
intros ab.
case a, b.
simpl.
refine (or_introl _).
exact I.
refine (or_introl _).
exact I.
refine (or_intror _).
exact I.
simpl in ab.
refine (or_introl _).
exact ab.
intros ab.
case a, b.
simpl.
exact I.
simpl.
exact I.
simpl.
exact I.
case ab.
intros tf.
simpl.
exact tf.
intros tf.
simpl.
exact tf.
Qed.

Print orb_is_or.

