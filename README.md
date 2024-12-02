egs
=====

An OTP application， A Game Server.

Introduction
-------
egs是一个游戏服务器，用于处理游戏逻辑。  

1.基于rebar3来管理项目  
2.使用erlang开发  
3.使用redis+mysql作为数据。  

Build
-----

    $ rebar3 compile


Run
-----
    $ sh scripts/run.sh dev # 测试环境
    $ sh scripts/run.sh prod # 生产环境

Protocol
-------
    使用json格式
    协议号要大于10000，否则会被拦截
    请求格式：
    {
      "data":{
        "code":10000,            -- 协议号
        "协议字段1": "value1",    -- 协议字段1
        "协议字段2": "value2"     -- 协议字段2
      }
    }

    返回格式：
    {
        "err_code": 0,          -- 错误码
        "err_msg": "错误信息",
        "data":{}               -- 返回数据
    }

Test
----
    $ pip install websockets
    $ pip install aysncio
    $ python scripts/client.py
