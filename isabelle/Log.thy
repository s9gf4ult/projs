theory Log 
  imports Main
begin 

lemma "\<forall> xs \<in> A. \<forall> ys \<in> A. xs \<and> ys \<Longrightarrow> \<not> (\<exists> n. n = False \<and> n \<in> A)" 
  by blast

lemma
"\<forall> A T. (\<forall> x y. T x y \<or> T y x \<Longrightarrow> 
 \<forall> x y. ((A x y \<and> A y x) \<rightarrow> (x = y)) \<Longrightarrow>
 \<forall> x y. T x y \<rightarrow> A x y \<Longrightarrow>
 A x y \<rightarrow> T x y)"

end