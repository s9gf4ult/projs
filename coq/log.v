
Lemma notnotTrue: forall P, ~~P -> P.
Proof.
  intros.
  unfold not in H.
  (* Stuck *)
Abort.

Lemma trueNotNot: forall (P: Prop), P -> ~~P.
Proof.
  intros.
  unfold not.
  intros.
  auto.
Qed.

Lemma revimpl: forall (A B : Prop), (A -> B) -> (~B -> ~A).
Proof.
  intros.
  unfold not.
  intros.
  apply H in H1.
  apply H0 in H1.
  assumption.
Qed.

Lemma revimplNot: forall (A B : Prop), (~A -> ~B) -> (B -> A).
Proof.
  unfold not. intros.
  (* Stuck *)
Abort.
