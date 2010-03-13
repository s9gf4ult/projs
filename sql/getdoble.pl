#!/usr/bin/perl

$aa = "SELECT * FROM setable434 t1 WHERE
EXISTS(SELECT * FROM setable434 t2 WHERE
t1.idmu <> t2.idmu and t1.rowno = t2.rowno and t1.sdate = t2.sdate and t1.edate = t2.edate and ";
$beg = 4;
$end = 6;
for ($i = $beg; $i <= $end; $i++) {
    $aa .= "t1.field_${i} = t2.field_${i} ";
    if ($i < $end) {
        $aa .= "and ";
    }
}
$aa .= "and ( ";
for ($i = $beg; $i <= $end; $i++) {
    $aa .= "(t2.field_${i} is not null and t2.field_${i} <> 0) ";
    if ($i < $end) {
        $aa .= "or ";
    }
}
$aa .= ") ";
    


$aa .= ")";

print "$aa\n";
