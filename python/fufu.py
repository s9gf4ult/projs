import sqlite3
import uuid

def make_tables(con):
    con.execute("pragma foreign_keys = on")
    con.execute("create table touch (id integer primary key not null, parent integer, val text)")
    con.execute("create table me (id integer primary key not null, node integer not null, left integer not null, right integer not null, foreign key (node) references touch(id), unique(node), unique(left), unique(right))")

def insert_randoms(con, depth, parent, length):
    if depth > 0:
        for x in xrange(0, length):
            xid = con.execute("insert into touch(parent, val) values (?, ?)", (parent, uuid.uuid1().get_hex())).lastrowid
            insert_randoms(con, depth - 1, xid, length)

def remake_hierarhy(con):
    
