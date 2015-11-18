# -*- coding:utf-8 -*-
from __future__ import absolute_import
from __future__ import print_function
from __future__ import unicode_literals

import random
from time import sleep

import paho.mqtt.client as mqtt

host = '127.0.0.1'
port = 5555
topics = [(b'sample/abc', 0), (b'a/b', 1), (b'c/d', 2)]

CHARS = 'abcdefghijklmnopqrstuvwxyz'


def on_connect(client, userdata, flags, respons_code):
    print('status {0}'.format(respons_code))


def on_message(client, userdata, msg):
    print('on_message', msg.topic + ' ' + str(msg.payload))


def main():
    # インスタンス作成時に protocol v3.1.1 を指定します
    client = mqtt.Client(protocol=mqtt.MQTTv31)
    client.on_connect = on_connect
    client.on_message = on_message

    client.on_disconnect = lambda *args, **kwargs: print("on_disconnect", args, kwargs)
    client.on_publish = lambda *args, **kwargs: print("on_publish", args, kwargs)
    client.on_subscribe = lambda *args, **kwargs: print("on_subscribe", args, kwargs)
    client.on_unsubscribe = lambda *args, **kwargs: print("on_unsubscribe", args, kwargs)
    client.on_log = lambda *args, **kwargs: print("on_log", args, kwargs)

    def recv(wait=0.5):
        sleep(wait)
        client.loop()

    client.connect(host, port=port, keepalive=60)
    recv()

    print('sub')
    client.subscribe([(topic, qos) for topic, qos in topics])
    recv()
    print('unsub')
    client.unsubscribe([topic for topic, qos in topics[1:3]])
    recv()

    for i in range(3):
        print('pub', i)
        client.publish(topics[i][0], '{}:'.format(i) + ''.join([random.choice(CHARS) for _ in xrange((i + 1) * 32)]))
        recv()

    client.disconnect()
    print('end')


if __name__ == '__main__':
    main()
