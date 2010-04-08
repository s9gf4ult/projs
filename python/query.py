aa="select "
for nmb in range(1,10):
    aa += "SUM(field_%(nmb)d) as field_%(nmb)d, "%{'nmb':nmb}
aa += "SUM(field_%(nmb)d) as field_%(nmb)d from setable428 where sdate >= ? and edate <= ? and idmu = ? and rowno = ? group by idmu"%{'nmb':nmb}
print aa
и
