theory Hello
  imports Main
begin

fun append :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where 
"append Nil a = a" | 
"append (Cons a as) b = Cons a (append as b)"

fun revert :: "'a list \<Rightarrow> 'a list" where
"revert Nil = Nil" | 
"revert (Cons a b) = append (revert b) (Cons a Nil)"

lemma rev_assoc [simp]: "append (append a b) c = append a (append b c)"
  apply (induction a)
  apply (auto)
  done

lemma append_nil [simp]: "append a Nil = a"
  apply(induction a)
  apply(auto)
  done

lemma revapp [simp]: "revert (append a b) = append (revert a) (revert b)"
  apply(induction a)
  apply(auto)
  done

lemma rev_rev [simp]: "revert(revert a) = a"
  apply(induction a)
  apply(auto)
  done




end 

