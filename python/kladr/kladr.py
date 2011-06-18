#!/bin/env python
# -*- coding: utf-8 -*-
## kladr ##

import gtk
import sqlite3
import multiprocessing

class kladr_draw(object):
    """
    """
    def __init__(self, filename):
        """
        
        Arguments:
        - `filename`:
        """
        self._filename = filename
        
