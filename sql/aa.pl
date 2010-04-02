$aa = "insert
into setable196
select t1.refguid, t1.idmu, t1.rowno, t1.sdate, t1.edate, ";

$beg = 1;
$end = 30;
for ($i = $beg; $i <= $end; $i++) {
    $aa .= "t1.field_${i} ";
    if ($i < $end) {
        $aa .= ", ";
    }
}
$aa .= "from stb196_tmp t1";
print $aa;
