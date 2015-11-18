# -*- coding:utf-8 -*-
from __future__ import absolute_import
from __future__ import unicode_literals
import socket
import time

from locust import TaskSet, task, Locust
from locust.events import request_success

HOST = 'localhost'
PORT = 5555


class TCPTaskSet(TaskSet):
    bufsize = 5000

    def on_start(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((HOST, PORT))

    @task
    def echo(self):
        start_at = time.time()
        self.sock.send(b'Hello world!!')
        res = self.sock.recv(self.bufsize)
        end_at = time.time()
        request_success.fire(
            request_type='TCP Recv',
            name='test/echo',
            response_time=int((end_at - start_at) * 1000000),
            response_length=len(res),
        )


class TCPUser(Locust):
    task_set = TCPTaskSet
    min_wait = 1000
    max_wait = 1000
