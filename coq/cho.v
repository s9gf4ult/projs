(* Мы можем либо болеть либо не болеть, третьего не дано *)
Inductive Illness :=
| Ill
| Healthy
.

(* Функция, которая возвращает true если всё норм, и false если не норм. Если мы
здоровы, то всё норм *)
Definition isNorm (i : Illness) : bool :=
  match i with
  | Ill => false
  | Healthy => true
  end.

(* Из предыщущих определений уже можно сказать, что если всё норм, то мы должны
быть здоровы. *)
Lemma normIsHealthy : forall i : Illness, isNorm i = true -> i = Healthy.
Proof.
  intros.
  destruct i.
  - discriminate.
  - reflexivity.
Qed.

(* Температура может быть высокой или нормальной *)
Inductive Temp :=
| HighTemp
| NormTemp
.

(* Ограничение, что если мы здоровы, то температура должна быть нормальной. Если
больны, то температура может быть любой *)
Definition tempRestriction (i : Illness) (t : Temp) := i = Healthy -> t = NormTemp.

Lemma highTempNotOk : forall (i : Illness) (t : Temp)
    (* Итак, для любой температуры и болезненности *)
  , tempRestriction i t
    (* Если соблюдается ограничение на температуру *)
    -> t = HighTemp
    (* А также температура высокая *)
    -> isNorm i = false.
    (* То это не норм *)
Proof.
  intros i t restr T.
  destruct i. {
    reflexivity.
  } {
    rewrite T in restr.
    unfold tempRestriction in restr.
    discriminate restr.
    - reflexivity.
  }
Qed.
