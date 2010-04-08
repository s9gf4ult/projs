import gtk
import time
import pygtk

class mWin(gtk.Window):
    def __init__(self, parent = "none"):
        if parent == "none":
            gtk.Window.__init__(self)
        else:
            gtk.Window.__init__(self,parent)
        bt1 = gtk.Button("OK")
        bt2 = gtk.Button("Connect")
        hb1 = gtk.HBox()
        hb1.add(bt1)
        hb1.add(bt2)
        self.add(hb1)
        self.connect("delete-event",gtk.main_quit)
        self.show_all()

aa = mWin()
gtk.main()

        

    
