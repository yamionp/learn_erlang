# -*- coding:utf-8 -*-
from __future__ import absolute_import
from __future__ import unicode_literals

import time

import paho.mqtt.client as mqtt
from locust import TaskSet, task, Locust
from locust.events import request_success

HOST = 'localhost'
PORT = 5555


def on_receive_publish(c, u, m):
    # print('#{}[{}]({}): {}'.format(m.mid, m.topic, m.qos, m.payload))
    print m.payload, time.time(), time.time() - float(m.payload)
    elapped = time.time() - float(m.payload)
    request_success.fire(
        request_type='TCP Recv',
        name='test/echo',
        response_time=int(elapped * 1000000),
        response_length=len(m.payload),
    )


class MQTTTaskSet(TaskSet):
    bufsize = 5000

    def on_start(self):
        # connecting to mqtt broker

        self.topic = b'sample/abc'

        self.mqtt = mqtt.Client(protocol=mqtt.MQTTv311)
        self.mqtt.on_message = on_receive_publish
        self.mqtt.connect(HOST, port=PORT, keepalive=5)
        self.mqtt.subscribe(self.topic)
        self.mqtt.loop_start()

    @task
    def pub(self):
        start_at = time.time()
        self.mqtt.publish(self.topic, str(start_at), qos=0)


class MQTTUser(Locust):
    task_set = MQTTTaskSet
    min_wait = 1000
    max_wait = 2000
