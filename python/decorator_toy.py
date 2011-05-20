#!/bin/env python
# -*- coding: utf-8 -*-
## decorator_toy ##


class packing_decorator(object):
    """
    Attributes:
    
    """
    def __init__(self, complression_level):
        """
        
        Arguments:
        - `complression_level`:
        """
        self._complression_level = complression_level
        
    def __call__(self, obj):
        """decorates an object and makes it's methods send and receive packing and unpacking
        
        Arguments:
        - `object`:
        """
        if hasattr(obj, "send"):
            self._old_send = obj.send
            setattr(obj, "send", self.send_packed)

        if hasattr(obj, "receive"):
            self._old_receive = obj.receive
            setattr(obj, "receive", self.receive_packed)
        return obj

    def send_packed(self, *args, **kargs):
        """pack the data and send it by method of an object
        Arguments:
        - `*args`:
        - `**kargs`:
        """
        data = args[0]
        print("packing the data {0} with compression level {1}".format(data, self._complression_level))
        self._old_send(*args, **kargs)

    def receive_packed(self, *args, **kargs):
        """receive the data then unpack it and return
        Arguments:
        - `*args`:
        - `**kargs`:
        """
        data = self._old_receive(*args, **kargs)
        print("unpack the data {0}".format(data))
        return data

class file_stream(object):
    """
    Attributes:
    
    """
    def send(self, data):
        """send data to file
        
        Arguments:
        - `data`:
        """
        print("data {0} sent to file".format(data))

    def receive(self, ):
        """return received data
        """
        print("receiving the data")
        return "data"


if __name__ == '__main__':
    a = file_stream()
    a.send("1010")
    x = a.receive()
    
    
    b = packing_decorator(100)(file_stream())
    b.send("<<<<data>>>>")
    b.receive()
