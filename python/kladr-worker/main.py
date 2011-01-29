#!/usr/bin/env python

# -*- coding:utf-8 -*-

try:
    import gtk
    import gtk.glade
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
        # self.delete_parent = self.glade.get_widget('pbutton_delete_child')
        # self.commit = self.glade.get_widget('pbutton_commit')
        # self.rollback = self.glade.get_widget('pbutton_rollback')

    def on_child_delete(button, something):
        msg = gtk.MessageDialog()
        msg.props.text = "hello computer !!"
        msg.show()
        
    def show(self):
        self.main_window.show()


d = just_do_it()
d.show()
gtk.main()
