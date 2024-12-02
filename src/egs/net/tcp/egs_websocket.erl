%%%-------------------------------------------------------------------
%%% @author zwj187
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% websocket消息处理
%%% @end
%%%-------------------------------------------------------------------
-module(egs_websocket).
-include("egs.hrl").

%% API
-export([
  check_websocket/1,
  hand_shake/1,
  build_frame/1,
  unmask_data/2
]).

%%
-define(OP_CONT, 0).        %% 连续消息
-define(OP_TEXT, 1).        %% 文本消息
-define(OP_BIN, 2).         %% 二进制消息
-define(OP_CLOSE, 8).       %% ws关闭
-define(OP_PING, 9).        %% 心跳ping
-define(OP_PONG, 10).       %% 心跳pong
-define(OP_MSG_TYPE, ?OP_TEXT). %% TODO 消息发送类型 text binary raw


%% 第一个包，检测是否为websocket
check_websocket(<<"GET /", _/binary>> = Data) when is_bitstring(Data) ->
  binary:match(Data, <<"websocket">>) /= nomatch;
check_websocket(_) ->
  ?FALSE.


%% 握手
hand_shake(Bin) ->
  HeaderList = binary:split(Bin, <<"\r\n">>, [global]),
  HeaderTupleList = [list_to_tuple(binary:split(Header, <<": ">>)) || Header <- HeaderList],
  erlang:put(header, HeaderTupleList),
  SecWebSocketKey = proplists:get_value(<<"Sec-WebSocket-Key">>, HeaderTupleList),
  Sha = crypto:hash(sha, [SecWebSocketKey, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
  Base64 = base64:encode(Sha),
  [
    <<"HTTP/1.1 101 Switching Protocols\r\n">>,
    <<"Upgrade: websocket\r\n">>,
    <<"Connection: Upgrade\r\n">>,
    <<"Sec-WebSocket-Accept: ">>, Base64, <<"\r\n">>,
    <<"\r\n">>
  ].


%% 重新包装数据
build_frame(Bin) when is_binary(Bin) ->
  DataLength = iolist_size(Bin),
  build_frame(DataLength, Bin);
build_frame(Content) ->
  Bin = iolist_to_binary(Content),
  DataLength = iolist_size(Bin),
  build_frame(DataLength, Bin).

%% 默认使用二进制发送
build_frame(DataLength, Bin) when DataLength =< 125 ->
  <<1:1, 0:3, ?OP_MSG_TYPE:4, 0:1, DataLength:7, Bin/binary>>;
build_frame(DataLength, Bin) when DataLength >= 125, DataLength =< 65535 ->
  <<1:1, 0:3, ?OP_MSG_TYPE:4, 0:1, 126:7, DataLength:16, Bin/binary>>;
build_frame(DataLength, Bin) when DataLength > 65535 ->
  <<1:1, 0:3, ?OP_MSG_TYPE:4, 0:1, 127:7, DataLength:64, Bin/binary>>.


%% 处理数据(解析数据)
unmask_data(<<Fin:1, 0:3, Opcode:4, 1:1, DataLen:7, MaskKey:4/binary, Data/binary>>, PackBin) when DataLen < 126 andalso DataLen =< size(Data) ->
  unmask_data1(Fin, Opcode, DataLen, MaskKey, Data, PackBin);

unmask_data(<<Fin:1, 0:3, Opcode:4, 1:1, 126:7, DataLen:16, MaskKey:4/binary, Data/binary>>, PackBin) when DataLen =< size(Data) ->
  unmask_data1(Fin, Opcode, DataLen, MaskKey, Data, PackBin);

unmask_data(<<Fin:1, 0:3, Opcode:4, 1:1, 127:7, 0:1, DataLen:63, MaskKey:4/binary, Data/binary>>, PackBin) when DataLen =< size(Data) ->
  unmask_data1(Fin, Opcode, DataLen, MaskKey, Data, PackBin);

% Error, the MSB of extended payload length must be 0
unmask_data(<<_Fin:1, 0:3, _Opcode:4, _:1, 127:7, 1:1, _Len:63, _/binary>>, PackBin) ->
  ?LOG_ERROR("the MSB of extended payload length must be 0.~n"),
  {ok, PackBin, <<>>};

% Error, Client to server message must be masked
unmask_data(<<_Fin:1, 0:3, _Opcode:4, 0:1, _Len:7, _Data/binary>>, PackBin) ->
  ?LOG_ERROR("Client to server message must be masked.~n"),
  {ok, PackBin, <<>>};

unmask_data(<<>>, PackBin) ->
  {ok, PackBin, <<>>};

unmask_data(Data, PackBin) ->
  {ok, PackBin, Data}.

%% 二进制数据解析
unmask_data1(1, ?OP_BIN, Len, MaskKey, Data, PackBin) ->
  %%
  <<Data1:Len/binary, Rest/binary>> = Data,
  Bin = do_unmask(MaskKey, Data1),
  unmask_data(Rest, <<PackBin/binary,Bin/binary>>);

unmask_data1(1, ?OP_TEXT, Len, MaskKey, Data, PackBin) ->
  %%
  <<Data1:Len/binary, Rest/binary>> = Data,
  Bin = do_unmask(MaskKey, Data1),
  unmask_data(Rest, <<PackBin/binary,Bin/binary>>);

%% miss match
unmask_data1(_Fin, _Opcode, Len, _MaskKey, Data, PackBin) ->
  %%?ERROR("Data miss match : ~p",[{_Fin, _Opcode, Len, _MaskKey, Data}]),
  <<_:Len/binary, Rest/binary>> = Data,
  unmask_data(Rest, PackBin).


%% 解压数据
do_unmask(Key, <<_:512, _Rest/binary>> = Data) ->
  K = binary:copy(Key, 16),
  <<LongKey:512>> = K,
  <<ShortKey:32>> = Key,
  do_unmask(ShortKey, LongKey, Data, <<>>);
do_unmask(Key, Data) ->
  <<ShortKey:32>> = Key,
  do_unmask(ShortKey, none, Data, <<>>).

do_unmask(Key, LongKey, Data, Acc) ->
  case Data of
    <<A:512, Rest/binary>> ->
      C = A bxor LongKey,
      do_unmask(Key, LongKey, Rest, <<Acc/binary, C:512>>);
    <<A:32, Rest/binary>> ->
      C = A bxor Key,
      do_unmask(Key, LongKey, Rest, <<Acc/binary, C:32>>);
    <<A:24>> ->
      <<B:24, _:8>> = <<Key:32>>,
      C = A bxor B,
      <<Acc/binary, C:24>>;
    <<A:16>> ->
      <<B:16, _:16>> = <<Key:32>>,
      C = A bxor B,
      <<Acc/binary, C:16>>;
    <<A:8>> ->
      <<B:8, _:24>> = <<Key:32>>,
      C = A bxor B,
      <<Acc/binary, C:8>>;
    <<>> ->
      Acc
  end.