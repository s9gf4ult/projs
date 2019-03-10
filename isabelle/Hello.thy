theory Hello
  imports Main
begin

datatype nat = Z | S nat 

fun add :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
"add Z n = n" |
"add (S n) m = S (add n m)"

fun mul :: "nat \<Rightarrow> nat \<Rightarrow> nat" where 
"mul Z a = Z" | 
"mul (S a) b = add (mul a b) (add b b)"

lemma add0[simp]: "add n Z = n"
  apply(induction n)
  apply(auto)
  done

lemma add_assoc: "add a (add b c) = add (add a b) c"
  apply(induction a)
  apply(auto)
  done

lemma add_s[simp]: "add a (S b) = S (add a b)"
  apply(induction a)
  apply(auto)
  done

lemma add_commutative: "add a b = add b a"
  apply(induction a)
  apply(auto)
  done

lemma mul_sz[simp]: "mul (S a) Z = mul a Z"
  apply(induction a)
  apply(auto)
  done

lemma mul_z[simp]: "mul a Z = Z" 
  apply(induction a)
  done 


end 

