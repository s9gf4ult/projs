import cx_Oracle

conn = cx_Oracle.connect("se/s2e3@192.168.160.202/sexp")
if not conn:
    quit(1)
    
curs = conn.cursor()
query = "select "
fmt = ""
for whatt in range(1,10):
    query += "field_%d, "%whatt
    fmt += "%7s"%"%7.7d"
fmt += "%7s"%"%7.7d"
query += "field_10 "
query += "from setable428"
curs.execute(query)
print fmt
if not curs:
    conn.close()
    quit(2)

for recordata in curs.fetchall():
    print fmt%recordata
    
conn.close()
