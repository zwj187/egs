egs
=====

An OTP application， A Simple Game Server.

Introduction
-------

    EGS-简单游戏服务，仅仅简单演示，尚未增加自动化、ORM等功能，服务功能仅限于HASH方式，待优化！


Build
-----

    $ rebar3 compile


Run
-----
    $ sh scripts/run.sh dev # 测试环境
    $ sh scripts/run.sh prod # 生产环境-在_build目录输出ebin文件。

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
        "err_code": 0,           -- 错误码
        "err_msg": "错误信息",  -- 错误信息
        "data":{}               -- 返回数据
    }

Test
----
    $ pip install websockets
    $ pip install aysncio
    $ python scripts/client.py
