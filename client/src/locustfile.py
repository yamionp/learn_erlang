# -*- coding:utf-8 -*-
from __future__ import absolute_import
from __future__ import unicode_literals

import time

import gevent
import paho.mqtt.client as mqtt
from locust import TaskSet, task, Locust
from locust.events import request_success

HOST = 'localhost'
PORT = 5555


def on_receive_publish(c, u, m):
    elapsed = time.time() - float(m.payload)
    request_success.fire(
        request_type='Recv Publish',
        name=m.topic,
        response_time=int((elapsed if elapsed >= 0 else 0) * 1000),
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

        def _recv():
            while not self.mqtt.loop():
                pass

        gevent.spawn(_recv)

    @task
    def pub(self):
        start_at = time.time()
        self.mqtt.publish(self.topic, str(start_at), qos=0)


class MQTTUser(Locust):
    task_set = MQTTTaskSet
    min_wait = 100
    max_wait = 100
