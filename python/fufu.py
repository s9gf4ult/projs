import sqlite3
import uuid

def make_tables(con):
    con.execute("pragma foreign_keys = on")
    con.execute("create table touch (id integer primary key not null, parent integer, val text)")
    con.execute("create table me (id integer primary key not null, node integer not null, left integer not null, right integer not null, foreign key (node) references touch(id) on delete cascade, unique(node), unique(left), unique(right))")
    con.execute("create index left_me on me(left)")
    con.execute("create index right_me on me(right)")
    con.execute("create index node_me on me(node)")

def insert_randoms(con, depth, parent, length):
    if depth > 0:
        for x in xrange(0, length):
            xid = con.execute("insert into touch(parent, val) values (?, ?)", (parent, uuid.uuid1().get_hex())).lastrowid
            insert_randoms(con, depth - 1, xid, length)

def remake_hierarhy(con):
    con.execute("delete from me")
    (count,) = con.execute("select count(*) from touch").fetchone()
    (root,) = con.execute("select count(*) from touch where parent is null").fetchone()
    leng = count * 100
    insert_me_in_range(con, 0, leng, None)

def insert_me_in_range(con, left, right, parent):
    (count,) = parent != None and con.execute("select count(*) from touch where parent = ?", (parent,)).fetchone() or con.execute("select count(*) from touch where parent is null").fetchone()
    if count == 0:
        return
    leng = right - left - 2
    onelen = (int(leng) / count) - 1
    cur = parent != None and con.execute("select id from touch where parent = ?", (parent,)) or con.execute("select id from touch where parent is null")
    for (x, (d,)) in map(lambda a, b: (a, b), xrange(0, count),  cur):
        lf = left + (x * onelen) + 1
        rg = lf + onelen - 1
        con.execute("insert into me(node, left, right) values (?, ?, ?)", (d, lf, rg))
        insert_me_in_range(con, lf, rg, d)
    

if __name__ == "__main__":
    con = sqlite3.connect("fufu.sqlite")
    make_tables(con)
    con.execute("begin transaction")
    try:
        insert_randoms(con, 3, None, 100)
        remake_hierarhy(con)
    except:
        con.rollback()
    else:
        con.commit()
    con.close()
