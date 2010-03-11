#!/usr/bin/perl

$aa = "SELECT * FROM setable434 t1 WHERE
EXISTS(SELECT * FROM setable434 t2 WHERE
t1.idmu <> t2.idmu and t1.rowno = t2.rowno and t1.sdate = t2.sdate and t1.edate = t2.edate and ";
$beg = 4;
$end = 6;
for ($i = $beg; $i <= $end; $i++) {
    if ($i < $end) {
        $aa .= "t1.field_${i} = t2.field_${i} and ";
    } else {
        $aa .= "t1.field_${i} = t2.field_${i} ";
    }
}
$aa .= "and ( ";
for ($i = $beg; $i <= $end; $i++) {
    if ($i < $end) {
        $aa .= "(t2.field_${i} is not null and t2.field_${i} <> 0) or ";
    } else {
        $aa .= "(t2.field_${i} is not null and t2.field_${i} <> 0) ";
    }
}
$aa .= ") ";
    


$aa .= ")";

print "$aa\n";
