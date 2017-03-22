%%%----------------------------------------------------------------------
%%% File    : bxe_util.erl
%%% Author  : Zachary Dean <contact@zadean.com>
%%% Purpose : Utility stuff for the BaseX client
%%% Created : 22 Mar 2017 by Zachary Dean <contact@zadean.com>
%%%----------------------------------------------------------------------

-module(bxe_util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([
         utf8_to_base64/1,
         base64_to_utf8/1,
         utf8_to_hex/1,
         hex_to_utf8/1,
         nvl/2
         ]).

utf8_to_base64(Data) when is_binary(Data) ->
   base64:encode(Data);
utf8_to_base64(Data) when is_list(Data) ->
   base64:encode_to_string(Data).

base64_to_utf8(Base64) when is_binary(Base64) ->
   base64:decode(Base64);
base64_to_utf8(Base64) when is_list(Base64) ->
   base64:decode_to_string(Base64).

utf8_to_hex(String) when is_binary(String) ->
   utf8_to_hex_l(String);
utf8_to_hex(String) when is_list(String) ->
   list_to_binary(utf8_to_hex_l(String)).

hex_to_utf8(Hex) when is_binary(Hex) ->
   hex_to_utf8_l(Hex);
hex_to_utf8(Hex) when is_list(Hex) ->
   list_to_binary(hex_to_utf8_l(Hex)).


%% ==========================================================
%%                Internal functions
%% ==========================================================

utf8_to_hex_l(<<H,T/binary>>) ->
   [integer_to_list(H, 16)|utf8_to_hex_l(T)];
utf8_to_hex_l(<<>>) -> [].

hex_to_utf8_l(<<H:2/bytes,T/binary>>) ->
   [binary_to_integer(H, 16)|hex_to_utf8_l(T)];
hex_to_utf8_l(<<>>) -> [].

nvl([], Val2) -> Val2;
nvl(Val1, _Val2) -> Val1.
