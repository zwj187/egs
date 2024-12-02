# coding=utf-8
# pip install websockets
# pip install asyncio

import asyncio
import websockets
# import json

"""
@doc websocket测试用例
"""
__author__ = "zwj187@gmail.com"

# server configuration
__server__ = "ws://127.0.0.1"
__port__ = 9999
# __msg__ = json.loads('''{"data":{"code":10000}}''')
__msg__ = '''{"data":{"code":10000}}'''




async def client():
    uri = "%s:%d/" % (__server__, __port__)
    print("connect to %s" % uri)
    async with websockets.connect(uri) as websocket:
        await websocket.send(__msg__)
        response = await websocket.recv()
        print("recv data:", response)

if __name__ == "__main__":
    asyncio.get_event_loop().run_until_complete(client())
