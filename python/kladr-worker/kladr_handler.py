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
        indexd varchar);
        
        

    def get_parent_list():
        pass

    def get_child_list():
        pass
        
