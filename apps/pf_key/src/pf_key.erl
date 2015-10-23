%%% @doc Interface module for {@link //pf_key}.
%%% Handles encoding and decoding of `PF_KEY' messages, as well as starting
%%% and using `PF_KEY' port processes.
-module(pf_key).
-export([encode_header/1, decode_header/1, encode_ext/1, decode_ext/1,
         sizeof/1]).
-ignore_xref([encode_header/1, decode_header/1, encode_ext/1, decode_ext/1,
              sizeof/1]).

-include_lib("pf_key/include/pf_key.hrl").

%% @doc Encode a PF_KEY header `Msg' to binary.
-spec encode_header(Msg :: tuple()) -> binary().
encode_header(Msg) when is_record(Msg, sadb_msg) ->
    <<(Msg#sadb_msg.version):8/native-unsigned,
      (Msg#sadb_msg.type):8/native-unsigned,
      (Msg#sadb_msg.errno):8/native-unsigned,
      (Msg#sadb_msg.satype):8/native-unsigned,
      (Msg#sadb_msg.len):16/native-unsigned,
      (Msg#sadb_msg.reserved):16/native-unsigned,
      (Msg#sadb_msg.seq):32/native-unsigned,
      (Msg#sadb_msg.pid):32/native-unsigned>>.

%% @doc Decode a PF_KEY extension from the start of `Bin`; ignore the rest.
-spec decode_header(Bin :: binary()) -> tuple().
decode_header(<<Version:8/native-unsigned,
                Type:8/native-unsigned,
                Errno:8/native-unsigned,
                SAType:8/native-unsigned,
                Len:16/native-unsigned,
                Reserved:16/native-unsigned,
                Seq:32/native-unsigned,
                Pid:32/native-unsigned,
                _Rest/binary>>) ->
    {sadb_msg, Version, Type, Errno, SAType, Len, Reserved, Seq, Pid}.

%% @doc Encode a PF_KEY extension `Msg` to binary.
-spec encode_ext(Msg :: tuple()) -> binary().
encode_ext(Msg) when is_record(Msg, sadb_sa) ->
    <<(Msg#sadb_sa.len):16/native-unsigned,
      (Msg#sadb_sa.exttype):16/native-unsigned,
      (Msg#sadb_sa.spi):32/native-unsigned,
      (Msg#sadb_sa.replay):8/native-unsigned,
      (Msg#sadb_sa.state):8/native-unsigned,
      (Msg#sadb_sa.auth):8/native-unsigned,
      (Msg#sadb_sa.encrypt):8/native-unsigned,
      (Msg#sadb_sa.flags):32/native-unsigned>>;
encode_ext(Msg) when is_record(Msg, sadb_lifetime) ->
    <<(Msg#sadb_lifetime.len):16/native-unsigned,
      (Msg#sadb_lifetime.exttype):16/native-unsigned,
      (Msg#sadb_lifetime.allocations):32/native-unsigned,
      (Msg#sadb_lifetime.bytes):64/native-unsigned,
      (Msg#sadb_lifetime.addtime):64/native-unsigned,
      (Msg#sadb_lifetime.usetime):64/native-unsigned>>;
encode_ext(Msg) when is_record(Msg, sadb_address) ->
    <<(Msg#sadb_address.len):16/native-unsigned,
      (Msg#sadb_address.exttype):16/native-unsigned,
      (Msg#sadb_address.proto):8/native-unsigned,
      (Msg#sadb_address.prefixlen):8/native-unsigned,
      (Msg#sadb_address.reserved):16/native-unsigned>>;
encode_ext(Msg) when is_record(Msg, sadb_key) ->
    <<(Msg#sadb_key.len):16/native-unsigned,
      (Msg#sadb_key.exttype):16/native-unsigned,
      (Msg#sadb_key.bits):16/native-unsigned,
      (Msg#sadb_key.reserved):16/native-unsigned>>.

%% @doc Decode a PF_KEY extension from the start of `Bin`; ignore the rest.
-spec decode_ext(Bin :: binary()) -> tuple().
decode_ext(<<Len:16/native-unsigned,
             ExtType:16/native-unsigned,
             _/binary>> = Bin) when ExtType == ?SADB_EXT_SA ->
    <<Len:16/native-unsigned,
      ExtType:16/native-unsigned,
      SPI:32/native-unsigned,
      Replay:8/native-unsigned,
      State:8/native-unsigned,
      Auth:8/native-unsigned,
      Encrypt:8/native-unsigned,
      Flags:32/native-unsigned,
      _Tail/binary>> = Bin,
    {sadb_sa, Len, ExtType, SPI, Replay, State, Auth, Encrypt, Flags};
decode_ext(<<Len:16/native-unsigned,
             ExtType:16/native-unsigned,
             _/binary>> = Bin) when ExtType == ?SADB_EXT_LIFETIME_CURRENT ;
                                    ExtType == ?SADB_EXT_LIFETIME_HARD ;
                                    ExtType == ?SADB_EXT_LIFETIME_SOFT ->
    <<Len:16/native-unsigned,
      ExtType:16/native-unsigned,
      Allocations:32/native-unsigned,
      Bytes:64/native-unsigned,
      AddTime:64/native-unsigned,
      UseTime:64/native-unsigned,
      _Tail/binary>> = Bin,
    {sadb_lifetime, Len, ExtType, Allocations, Bytes, AddTime, UseTime};
decode_ext(<<Len:16/native-unsigned,
             ExtType:16/native-unsigned,
             _/binary>> = Bin) when ExtType == ?SADB_EXT_ADDRESS_SRC ;
                                    ExtType == ?SADB_EXT_ADDRESS_DST ;
                                    ExtType == ?SADB_EXT_ADDRESS_PROXY ->
    <<Len:16/native-unsigned,
      ExtType:16/native-unsigned,
      Proto:8/native-unsigned,
      PrefixLen:8/native-unsigned,
      Reserved:16/native-unsigned,
      _Tail/binary>> = Bin,
    {sadb_address, Len, ExtType, Proto, PrefixLen, Reserved};
decode_ext(<<Len:16/native-unsigned,
             ExtType:16/native-unsigned,
             _/binary>> = Bin) when ExtType == ?SADB_EXT_KEY_AUTH ;
                                    ExtType == ?SADB_EXT_KEY_ENCRYPT ->
    <<Len:16/native-unsigned,
      ExtType:16/native-unsigned,
      Bits:16/native-unsigned,
      Reserved:16/native-unsigned,
      _Tail/binary>> = Bin,
    {sadb_key, Len, ExtType, Bits, Reserved}.

%% @doc Size in bytes of PF_KEY record `Type` (in binary form).
-spec sizeof(Type :: atom()) -> non_neg_integer().
sizeof(sadb_msg)      -> 16;
sizeof(sadb_ext)      -> 4;
sizeof(sadb_sa)       -> 16;
sizeof(sadb_lifetime) -> 32;
sizeof(sadb_address)  -> 8;
sizeof(sadb_key)      -> 8.
