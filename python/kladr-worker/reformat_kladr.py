#!/bin/env python
# -*- coding: utf-8 -*-

import sqlite3

class kladr(object):
    """
    """
    def __init__(self, filename):
        """
        Arguments:
        - `filename`:
        """
        self._filename = filename
        try:
            self.db = sqlite3.connect(filename)
            self.db.execute("pragma foreign_keys=on")
        except:
            print("Указаный файл скорее всего косячный")

        


        
if __name__ == '__main__':
    import sys
    print(sys.argv[0])
