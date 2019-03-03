theory Lists
  imports Main 
begin 

datatype 'a list = Nil | Cons 'a "'a list"

fun app:: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
"app Nil a = a" |
"app (Cons a as) b = Cons a (app as b)"

value "app (Cons a (Cons b Nil)) (Cons c Nil)"

fun rev :: "'a list \<Rightarrow> 'a list" where
"rev Nil = Nil" | 
"rev (Cons a as) = app (rev as) (Cons a Nil)"

value "rev (Cons a (Cons b Nil))"

fun rem :: "'a \<Rightarrow> 'a list \<Rightarrow> 'a list" where
"rem a Nil = Nil" | 
"rem a (Cons b bs) = (if a = b then rem a bs else Cons b (rem a bs))"

value "rem (1 :: nat) (Cons 2 (Cons 1 (Cons 1 (Cons 2 Nil))))"

lemma rems: "size (rem a as) \<le> Suc (size as)"
  apply(induction as)
  apply(auto)
  done

fun nub :: "'a list \<Rightarrow> 'a list" where
"nub Nil = Nil" | 
"nub (Cons a as) = Cons a (nub (rem a as))"

lemma app_nil[simp]: "app a Nil = a"
  apply(induction a)
  apply(auto)
  done

lemma app_assoc[simp]: "app a (app b c) = app (app a b) c"
  apply(induction a)
  apply(auto)
  done

lemma rev_app[simp]: "rev (app a b) = app (rev b) (rev a)"
  apply(induction a)
  apply(auto)
  done

lemma rev_rev[simp]: "rev (rev a) = a"
  apply(induction a)
  apply(auto)
  done

end