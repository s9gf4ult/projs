#!/usr/bin/env python

# -*- coding: utf-8 -*-

try:
    import gtk
    import gtk.glade
    import gobject
except:
    print("error in loading gtk !!!")
    exit(1)

try:
    from kladr_handler import kladr_handler
except:
    print("can not load kladr_handler")
    exit(1)

class just_do_it:
    def __init__(self):
        self.glade = gtk.glade.XML('gui.glade')
        self.main_window = self.glade.get_widget('window')
        self.main_window.connect('destroy', gtk.main_quit)
        self.delete_child = self.glade.get_widget('pbutton_delete_parent')
        self.delete_child.connect('clicked', self.on_child_delete)
        self.delete_parent = self.glade.get_widget('pbutton_delete_child')
        self.delete_parent.connect('clicked', self.on_parent_delete)
        self.commit = self.glade.get_widget('pbutton_commit')
        self.commit.connect('clicked', self.on_commit)
        self.rollback = self.glade.get_widget('pbutton_rollback')
        self.rollback.connect('clicked', self.on_rollback)
        self.open = self.glade.get_widget('pbutton_open')
        self.open.connect('clicked', self.on_open)
        self.parent_view = self.glade.get_widget('treeview_parent')
        self.child_view = self.glade.get_widget('treeview_child')
        

    def on_open(self, widget, data=None):
        dialog = gtk.FileChooserDialog(title="Open File", action=gtk.FILE_CHOOSER_ACTION_OPEN, parent=self.main_window,
                                       buttons=(gtk.STOCK_CANCEL,gtk.RESPONSE_CANCEL,gtk.STOCK_OPEN,gtk.RESPONSE_OK))
        response=dialog.run()
        if response == gtk.RESPONSE_OK:
            filename=dialog.get_filename()
            dialog.destroy()
            self.handler = kladr_handler(filename)
            try:
                self.check_database()
            except:
                mdialog = gtk.MessageDialog(parent=self.main_window, flags=gtk.DIALOG_MODAL, buttons=gtk.BUTTONS_OK, type=gtk.MESSAGE_ERROR)
                mdialog.props.text="this file is not sqlite3 databse or does not contain Kladr"
                mdialog.run()
                mdialog.destroy()
                return

            self.parent_list = self.handler.get_parent_list()
            self.child_list = self.handler.get_child_list()
            self.parent_view.set_model(self.parent_list)
            self.child_view.set_model(self.child_list)
                
        dialog.destroy()
            
            
        
    def on_rollback(self, widget, data=None):
        pass

    def on_parent_delete(self, widget, data=None):
        self.parent_list.append(['jajaj'])

    def on_commit(self, widget, data=None):
        pass

    def on_child_delete(self, widget, data=None):
        (x,iterator) = self.parent_view.get_selection().get_selected()
        self.parent_list.remove(iterator)
        
    def show(self):
        self.main_window.show_all()

d = just_do_it()
d.show()
gtk.main()
