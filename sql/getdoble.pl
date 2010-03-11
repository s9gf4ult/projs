#!/usr/bin/perl

$aa = "SELECT * FROM setable434 WHERE \n";
for ($i = 1; $i<=15; $i++) {
    if ($i != 15) {
        $aa = $aa . "field_" . $i . " is not null and field_" . $i . " <> 0 and ";
    } else {
        $aa = $aa . "field_" . $i . " is not null and field_" . $i . " <> 0\n";
    }
}
$aa = $aa . "GROUP BY ";
for ($i = 1; $i<=15; $i++) {
    if ( $i != 15) {
        $aa = $aa . "field_" . $i . ", ";
    } else {
        $aa = $aa . "field_" . $i;
    }
}
$aa .= ", idmu, rowno";
print "$aa\n";
