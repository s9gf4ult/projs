use DBI;

my $bdh = DBI->connect('dbi:Oracle:orcl', "se", "s2e3");
my $sqh = $dbh->prepare("select distinct idmu from setable428");
$sqh->execute();
while (@aa = $sqh->fetch_array) {
    print @aa;
}
