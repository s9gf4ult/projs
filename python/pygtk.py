#!/usr/bin/env python
#coding: utf-8

import os, re
from socket import *
class Srvstat:
    def __init__(self, host='127.0.0.1', port=52001):
        self.socket = socket(AF_INET, SOCK_STREAM)
        self.socket.bind((host, port))
        self.socket.listen(5)
    def process(self):
        while 1:
            csocket, caddress = self.socket.accept()
            csocket.send('Connection established. Ready for request.\n')
            while 1:
                request = csocket.recv(64)
                if re.match('get\s+srvstat', request, re.IGNORECASE):
                    csocket.send(os.popen('/usr/bin/vmstat 1 10').read())
                    csocket.send(os.popen('/usr/bin/netstat -ta').read())
                elif re.match('quit', request, re.IGNORECASE):
                    break
                else:
                    csocket.send('Unknown command.\n')
                    csocket.close()
                
                    # if __name__ == '__main__':
                    #     serv = Srvstat()
                    #     serv.process()

hello = Srvstat()
hello.process()
