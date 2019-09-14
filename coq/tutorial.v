
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

Theorem negb_is_not : (forall a, Is_true (negb a) <-> (~(Is_true a))).
Proof.
intros a.
unfold iff.
refine (conj _ _).
case a.
simpl.
intros f t.
exact f.
simpl.
intros t f.
exact f.
case a.
simpl.
intros tf.
exact (tf I).
simpl.
intros nf.
exact I.
Qed.

Print negb_is_not.

Definition basic_andb 
  := (fun a => Is_true (andb a true)).

Lemma basic_andb_ex : (ex basic_andb).
Proof.
refine (ex_intro _ true _).
unfold basic_andb.
simpl.
exact I.
Qed.

Print ex.

Theorem thm_forall_exists : (forall b, (exists a, Is_true(eqb a b))).
Proof.
intros b.
case b.
refine (ex_intro _ true _).
simpl.
exact I.
refine (ex_intro _ false _).
exact I.
Qed.

Theorem forall_exists : (forall P : Set->Prop, (forall x, ~(P x)) -> ~(exists x, P x)).
Proof.
intros P.
intros forallxNotP.
unfold not.
intros exPx.
destruct exPx as [x Px].
pose (notP := forallxNotP x).
unfold not in notP.
exact (notP Px).
Qed.

Print forall_exists.

Theorem exists_forall : (forall P : Set->Prop, ~(exists x, P x) -> (forall x, ~(P x))).
Proof.
intros P.
intros notExP.
intros x.
unfold not.
intros Px.
unfold not in notExP.
exact (notExP (ex_intro P x Px)).
Qed.

Print ex_intro.

Lemma no_orb : not (exists a : bool, Is_true (andb false a)).
Proof.
unfold not.
intros notb.
destruct notb as [a P].
destruct a.
exact P.
exact P.
Qed.

Theorem thm_eq_trans__again : (forall x y z: Set, x = y -> y = z -> x = z).
Proof.
intros x y z.
intros xy yz.
rewrite xy.
rewrite <- yz.
exact (eq_refl y).
Qed.

Print thm_eq_trans__again.

Print eq_ind.

Theorem neq_nega: (forall a, a <> (negb a)).
Proof.
intros a.
case a.
simpl.
unfold not.
intros tf.
discriminate tf.
unfold not.
simpl.
intros ft.
discriminate ft.
Qed.

Print neq_nega.

Print Nat.add.

Lemma eqNats : (S (S O)) + (S O) = (S (S (S O))).
Proof.
simpl.
exact (eq_refl _).
Qed.

Print eqNats.

Print nat_ind.

Theorem plus_n_O : (forall n, n + O = n).
  Proof.
  intros n.
  elim n.
    simpl.
    exact (eq_refl _).
    intros n0.
    intros np.
    simpl.
    rewrite np.
    exact (eq_refl _).
Qed.

Print plus_n_O.

Lemma plus_sym: (forall n m, n + m = m + n).
Proof.
intros n m.
elim n.
elim m.
exact (eq_refl _).
intros n0.
intros n00n.
rewrite (plus_n_O (S n0)).
simpl.
exact (eq_refl _).
intros n0.
intros n0m.
simpl.
rewrite n0m.
elim m.
simpl.
exact (eq_refl _).
intros n1.
intros sn1n0.
simpl.
rewrite sn1n0.
exact (eq_refl _).
Qed.

Print plus_sym.

Require Import List.

Print list.
Print list_ind.

Theorem cons_adds_one_to_length :
   (forall A:Type,
   (forall (x : A) (lst : list A),
   length (x :: lst) = (S (length lst)))).
Proof.
intros A x.
intros lst.
simpl.
reflexivity.
Qed.

Print hd.

Print option.

Definition hd_never_fail (A : Type) (lst : list A) (safety_proof : lst <> nil)
  : A
:=
  (match lst as b return (lst = b -> A) with
    | nil => (fun foo : lst = nil =>
                   match (safety_proof foo) return A with
                   end
                )
    | x :: _ => (fun foo : lst = x :: _ =>
                   x
                )
  end) eq_refl.

Theorem correctness_of_hd_never_fail :
   (forall A:Type,
   (forall (x : A) (rest : list A),
   (exists safety_proof : ((x :: rest) <> nil),
      (hd_never_fail A (x :: rest) safety_proof) = x))).
Proof.
intros A x.
intros rest.
assert (witness : ((x :: rest) <> nil)).
  unfold not.
  intros req.
  discriminate req.
refine (ex_intro _ witness _).
simpl.
reflexivity.
Qed.

Print eq_ind.

Print correctness_of_hd_never_fail.

Definition tl_never_fail (A : Type) (lst : list A) (safety_proof : lst <> nil) 
  : list A := 
  (match lst as l return (lst = l -> list A) with 
    | nil => (fun a : lst = nil => 
      match (safety_proof a) return (list A) with 
      end)
    | _ :: x => (fun (a : lst = _ :: x) => x)
  end) eq_refl.

Lemma tl_never_fail_proof : forall (A : Prop) (x : A) (rest : list A)
  , (exists proof : (x :: rest) <> nil, tl_never_fail A (x :: rest) proof = rest).
Proof.
intros A x rest.
assert (wit : x :: rest <> nil).
  unfold not.
  intros lie.
  discriminate lie.
refine (ex_intro _ wit _).
simpl.
reflexivity.
Qed.

Fixpoint app (A : Type) (a : list A) (b : list A) : list A := 
  match a with 
    | nil => b
    | x :: xs => x :: (app A xs b)
  end.
  
Theorem app_nil_l : (forall A:Type, (forall l:list A, nil ++ l = l)).
Proof.
intros A l.
simpl.
reflexivity.
Qed.

Theorem app_nil_r : (forall A:Type, (forall l:list A, l ++ nil = l)).
Proof.
intros A l.
elim l.
reflexivity.
intros a aa aanil.
simpl.
rewrite aanil.
reflexivity.
Qed.

Theorem app_comm_cons : forall A (x y:list A) (a:A), a :: (x ++ y) = (a :: x) ++ y.
Proof.
intros A x y a.
simpl.
reflexivity.
Qed.

Theorem app_assoc : forall A (l m n:list A), l ++ m ++ n = (l ++ m) ++ n.
Proof.
intros A l m n.
elim l.
simpl.
reflexivity.
intros a aa.
intros pre.
simpl.
rewrite pre.
reflexivity.
Qed.

Theorem app_cons_not_nil : forall A (x y:list A) (a:A), nil <> x ++ a :: y.
Proof.
intros A x y a.
elim x.
unfold not.
simpl.
intros lie.
discriminate lie.
intros b bs tru.
unfold not.
intros lie.
simpl in lie.
discriminate lie.
Qed.

Fixpoint beq_nat (n m : nat) : bool :=
  match n with
  | O => match m with
         | O => true
         | S m' => false
         end
  | S n' => match m with
            | O => false
            | S m' => beq_nat n' m'
            end
end.

