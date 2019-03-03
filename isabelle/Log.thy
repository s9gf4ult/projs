theory Log 
  imports Main
begin 

lemma "\<forall> xs \<in> A. \<forall> ys \<in> A. xs \<and> ys \<Longrightarrow> \<not> (\<exists> n. n = False \<and> n \<in> A)" 
  by blast

end