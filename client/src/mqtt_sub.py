# -*- coding:utf-8 -*-
from __future__ import absolute_import
from __future__ import print_function
from __future__ import unicode_literals

from time import sleep

import paho.mqtt.client as mqtt

host = '127.0.0.1'
port = 5555
topics = [(b'sample/abc', 0), (b'a/b', 1), (b'c/d', 2)]

CHARS = b'abcdefghijklmnopqrstuvwxyz'


def _event(func_name):
    return lambda *args, **kwargs: print(func_name, args, kwargs)


if __name__ == '__main__':
    # Publisherと同様に v3.1.1を利用
    client = mqtt.Client(protocol=mqtt.MQTTv311)

    client.on_connect = _event("on_connect")
    client.on_log = _event("on_log")
    client.on_message = lambda c, u, m: print('#{}[{}]({}): {}'.format(m.mid, m.topic, m.qos, m.payload))

    # run
    client.connect(host, port=port, keepalive=60)
    client.subscribe(topics[0][0])

    # 待ち受け状態にする
    while not client.loop():
        sleep(0.1)
