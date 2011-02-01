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
        if not 
        
        

    def get_parent_list():
        pass

    def get_child_list():
        pass
        
