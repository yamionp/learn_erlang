# -*- coding:utf-8 -*-
from __future__ import absolute_import
from __future__ import print_function
from __future__ import unicode_literals

from time import sleep

import paho.mqtt.client as mqtt

host = '127.0.0.1'
port = 5555
topics = [(b'sample/abc', 0), (b'a/b', 1), (b'c/d', 2)]

CHARS = 'abcdefghijklmnopqrstuvwxyz'


def _event(func_name):
    return lambda *args, **kwargs: print(func_name, args, kwargs)


def main():
    # インスタンス作成時に protocol v3.1.1 を指定します
    client = mqtt.Client(protocol=mqtt.MQTTv31)

    client.on_connect = _event("on_connect")
    client.on_log = _event("on_log")

    # 接続
    client.will_set(topics[0][0], b'will payload test', qos=1)
    client.connect(host, port=port, keepalive=60)

    # publish
    client.publish(topics[0][0], 'aaalksjfas;lfkjas;flkasj', qos=0)
    client.publish(topics[0][0], 'aaalksjfas;lfkjas;flkasj', qos=1)

    # 待ち受け状態にする
    for i in xrange(5):
        client.loop()

    client.disconnect()


if __name__ == '__main__':
    main()
