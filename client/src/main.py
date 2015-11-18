# -*- coding:utf-8 -*-
from __future__ import absolute_import
from __future__ import unicode_literals
from __future__ import print_function
import socket
from contextlib import closing
import time


def main():
    host = 'localhost'
    port = 5555
    bufsize = 4096

    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    with closing(sock) as s:
        s.connect((host, port))
        start_at = time.time()
        s.send(b'Hello world~~~~~~~')
        s.send(b'Hello world~~~~~~~')
        s.send(b'Hello world~~~~~~~')
        print(s.recv(bufsize))
        end_at = time.time()
        print(end_at-start_at)


if __name__ == '__main__':
    main()
