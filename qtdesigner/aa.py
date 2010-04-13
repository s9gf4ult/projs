# -*- coding:UTF-8

import unt
from PyQt4 import QtCore, QtGui
import sys
import cx_Oracle


class MWin(QtGui.QMainWindow, unt.Ui_MainWindow):
    def __init__(self):
        QtGui.QMainWindow.__init__(self)
        unt.Ui_MainWindow.setupUi(self,self)
        self.listWidget.setVisible(0)
        QtCore.QObject.connect(self.connectButton, QtCore.SIGNAL("clicked()"), self.connectButtonPush)
        QtCore.QObject.connect(self, QtCore.SIGNAL("destroyed()"), self.onClose)
    def connectButtonPush(self):
        if sys.platform == "win32":
            self.dbconn = cx_Oracle.connect("se/s2e3@sexp")
        else:
            self.dbconn = cx_Oracle.connect("se/s2e3@192.168.160.202/sexp")
        if not self.dbconn:
            print "database connect erro"
            return
        self.connectButton.setVisible(0)
        self.listWidget.setVisible(1)
        


    def onClose(self):
        self.dbconn.close()
        

app = QtGui.QApplication(sys.argv)
win = MWin()
win.show()
app.exec_()
