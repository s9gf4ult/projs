
Inductive binum :=
| Bottom : binum
| Top : binum
| Between (b t : binum) : binum
.

Inductive binum_le : binum -> binum -> Prop
| le_eq (a : binum) : binum_le a a
| le_neq
