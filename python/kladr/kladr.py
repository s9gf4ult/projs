#!/bin/env python
# -*- coding: utf-8 -*-
## kladr ##

import gtk
import gtk.gdk
import gobject
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

    def root_builder_thread(self, ):
        """
        """
        for (pid, typename, name) in self._connection.execute("select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id where exists(select h.* from kladr_hierarchy h where h.parent = k.id) and not exists(select hh.* from kladr_hierarchy hh where hh.child = k.id) order by t.name, k.name"):
            par = self._view.get_model().append(None, (pid, typename, name))
            for (cid, ctype, cname) in self._connection.execute("select k.id, t.name, k.name from kladr_objects k inner join short_names t on k.short_id = t.id inner join kladr_hierarchy h on h.child = k.id where h.parent = ? order by t.name, k.name", [pid]):
                self._view.get_model().append(par, (cid, ctype, cname))
                

        
    def run(self, ):
        """
        """
        self._connection = sqlite3.connect(self._filename)
        self._connection.execute("pragma foreign_keys=on")
        self._window = gtk.Window()
        self._window.connect("delete-event", self.on_delete)
        sc = gtk.ScrolledWindow()
        self._view = gtk.TreeView()
        sc.add(self._view)
        self._window.add(sc)
        self._model = gtk.TreeStore(gobject.TYPE_INT, gobject.TYPE_STRING, gobject.TYPE_STRING)
        col1 = gtk.TreeViewColumn(u'Type', gtk.CellRendererText(), text = 1)
        col2 = gtk.TreeViewColumn(u'Name', gtk.CellRendererText(), text = 2)
        self._view.append_column(col1)
        self._view.append_column(col2)
        self._view.set_model(self._model)
        self._view.connect("row-expanded", self.on_row_expanded)
        self._root_builder = multiprocessing.Process(target = self.root_builder_thread)
        self._child_builder = multiprocessing.Process(target = self.child_builder_thread)
        self.run_building_root()
        self._window.show_all()
        pass

    def run_building_root(self, ):
        """
        """
        self._root_builder.start()

    def child_builder_thread(self, ):
        """
        """
        pass


    def on_row_expanded(self, tw, iter, path):
        """
        
        Arguments:
        - `tw`:
        - `iter`:
        - `path`:
        """
        pass


    def on_delete(self, window, event):
        """
        
        Arguments:
        - `window`:
        - `event`:
        """
        gtk.main_quit()
        return False



if __name__ == '__main__':
    import sys
    
    if len(sys.argv) == 2:
        k = kladr_draw(sys.argv[1])
        k.run()
        gtk.gdk.threads_init()
        gtk.main()
        
        
