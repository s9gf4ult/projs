#!/usr/bin/env python

# -*- encoding:utf-8 -*-

try:
    import gtk
    import gtk.glade
    import gobject
except:
    print("error in loading gtk !!!")
    exit(1)

class just_do_it:
    def __init__(self):
        self.glade = gtk.glade.XML('gui.glade')
        self.main_window = self.glade.get_widget('window')
        self.main_window.connect('destroy', gtk.main_quit)
        self.delete_child = self.glade.get_widget('pbutton_delete_parent')
        self.delete_child.connect('clicked', self.on_child_delete)
        self.delete_parent = self.glade.get_widget('pbutton_delete_child')
        self.delete_parent.connect('clicked', self.on_parent_delete, self)
        self.commit = self.glade.get_widget('pbutton_commit')
        self.commit.connect('clicked', self.on_commit)
        self.rollback = self.glade.get_widget('pbutton_rollback')
        self.rollback.connect('clicked', self.on_rollback)
        self.open = self.glade.get_widget('pbutton_open')
        self.open.connect('clicked', self.on_open)
        self.parent_view = self.glade.get_widget('treeview_parent')
        self.child_view = self.glade.get_widget('treeview_child')
        self.parent_list = gtk.ListStore(gobject.TYPE_STRING)
        self.parent_view.insert_column(gtk.TreeViewColumn(u'hee', gtk.CellRendererText()), -1)
        self.parent_view.set_model(self.parent_list)
        

    def on_open(button, something):
        pass

    def on_rollback(button, something):
        pass

    def on_parent_delete(button,sf, self):
        self.parent_list.append(('hehe',))

    def on_commit(button, something):
        pass

    def on_child_delete(button, something):
        pass
        
    def show(self):
        self.main_window.show_all()

d = just_do_it()
d.show()
gtk.main()
