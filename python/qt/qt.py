import sys
from PyQt4 import QtGui, QtCore
class closebutton(QtGui.QPushButton):
    def __init__(self, parent):
        QtGui.QPushButton.__init__(self,parent)
    def mousePressEvent(self,event):
        if self.text == "pushed":
            self.setText("pushed Again")
        else:
            self.setText("pushed")
    def mouseDoubleClickEvent(self,event):
        self.setText("double clicked")
        

class SigSlot(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.setWindowTitle('signal & slot')

        lcd = QtGui.QLCDNumber(self)
        slider = QtGui.QSlider(QtCore.Qt.Horizontal, self)
        presser = closebutton(self)
        presser.resize(100,30)

        vbox = QtGui.QVBoxLayout()
        vbox.addWidget(lcd)
        vbox.addWidget(slider)
        vbox.addWidget(presser)

        self.setLayout(vbox)
        self.connect(slider,  QtCore.SIGNAL('valueChanged(int)'), lcd, QtCore.SLOT('display(int)') )
        self.connect(presser, QtCore.SIGNAL('mousePressEvent'), QtCore.SLOT('onAction(action)'))

        self.resize(250, 150)
    def onAction(self,action):
        lcd.set
        


app = QtGui.QApplication(sys.argv)
qb = SigSlot()
qb.show()
sys.exit(app.exec_())
