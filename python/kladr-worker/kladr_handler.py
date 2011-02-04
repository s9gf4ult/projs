#!/bin/env python

# -*- coding: utf-8 -*-


try:
    import sqlite3
except:
    print("can not load sqlite3")

class kladr_handler:
    def __init__(self, filename):
        self.database = sqlite3.connect(filename)

    def check_database():
        self.database.execute("pragma foreign_keys = on")
        for table in ["kladr", "street", "socrbase", "region"]:
            if not self.database.execute("select * from sqlite_master where type = 'table' and name = ?", (table,)).fetchone():
                raise Exception("has no kladr table")
        if not self._check_objects():
            self._create_objects()

    def _check_objects(self):
        return (self.database.execute("select * from sqlite_master where type = 'table' and name = 'kladr_objects'").fetchone()) and (self.database.execute("select * from sqlite_master where type = 'table' and name = 'kladr_types'").fetchone())
            
    def _create_objects(self):
        self.database.executescript("""
        drop table kladr_objects;
        drop table kladr_types;
        create table kladr_types (id integer primary key not null, name varchar not null, scname varchar not null, unique(name), unique(scname));
        create table kladr_objects (
        id integer primary key not null,
        parent_id integer,
        name varchar not null,
        kladr_type integer not null,
        code varchar not null,
        region_code integer not null,
        district_code integer not null,
        city_code integer not null,
        place_code integer not null,
        street_code integer,
        actuality_code integer not null,
        ocatd varchar,
        indexd varchar,
        unique(code),
        unique(city_code, place_code, street_code, actuality_code),
        foreign key (parent_id) references kladr_objects(id) on delete cascade,
        foreign key (kladr_type) references kladr_type(id) on delete cascade);
        insert into kladr_type (name, scname) select distinct socrname, scname from socrbase;
        insert into kladr_objects (name, kladr_type, code, region_code, district_code, city_code, place_code, actuality_code, ocatd, indexd)
        select k.name, t.id, k.code,
        cast(substr(k.code, 1, 2) as integer),
        cast(substr(k.code, 3, 3) as integer),
        cast(substr(k.code, 6, 3) as integer),
        cast(substr(k.code, 9, 3) as integer),
        cast(substr(k.code, 12, 2) as integer),
        k.ocatd, k.index
        from kladr k inner join kladr_types t on k.socr = t.scname;
        insert into kladr_objects (name, kladr_type, code, region_code, district_code, city_code, place_code, actuality_code, ocatd, indexd) select k.name, t.id, k.code,
        cast(substr(k.code, 1, 2) as integer),
        cast(substr(k.code, 3, 3) as integer),
        cast(substr(k.code, 6, 3) as integer),
        cast(substr(k.code, 9, 3) as integer),
        cast(substr(k.code, 12, 2) as integer),
        k.ocatd, k.index
        from region k inner join kladr_types t on k.socr = t.scname;
        insert into kladr_objects (name, kladr_type, code, region_code, district_code, city_code, place_code, street_code, actuality_code, ocatd, indexd) select k.name, t.id, k.code,
        cast(substr(k.code, 1, 2) as integer),
        cast(substr(k.code, 3, 3) as integer),
        cast(substr(k.code, 6, 3) as integer),
        cast(substr(k.code, 9, 3) as integer),
        cast(substr(k.code, 12, 4) as integer),
        cast(substr(k.code, 16, 2) as integer),
        k.ocatd, k.index from
        street k inner join kladr_types t on k.socr = t.scname;
        """)
        
        

    def get_parent_list():
        pass

    def get_child_list():
        pass
        
