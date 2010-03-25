$pwf="powercfg";

print "${pwf} /DELETE 11\n";
print "${pwf} /CREATE 11\n";

foreach $tm("timeout-ac", "timeout-dc") {
    foreach $wht("monitor", "disk", "standby", "hibernate") {
        print "${pwf} /CHANGE 11 /${wht}-${tm} 0\n"
    }
}
print "${pwf} /SETACTIVE 11\n";
print "${pwf} /H OFF\n";
