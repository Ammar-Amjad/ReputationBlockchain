%%%-------------------------------------------------------------------
%%% @author Ammar
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. Sep 2022 5:21 PM
%%%-------------------------------------------------------------------
-module(pow).
-author("Ammar").  
%% API
-export([check/2, hasherloop/2, find_pow/2, divide/2]).

check(CoinNeeded, Orignal_str) ->
  ComparisonStr = "0000000000000000000000000000000000000000000000000000000000000000",
  New_string = string:left(ComparisonStr, CoinNeeded - 1) ++ "1" ++ string:sub_string(ComparisonStr, CoinNeeded + 1),
  Hash = io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256,Orignal_str))]),
  {Hash < New_string, Hash}.

hasherloop(CoinNeeded, Orignal_String) ->
  {Check, Hash} = check(CoinNeeded, Orignal_String),
  case Check of
    true  ->
      {ok, {Orignal_String, Hash}};
    false ->
      hasherloop(CoinNeeded, Hash)
  end.

find_pow(CoinNeeded, Status) ->
  erlang:system_flag(scheduler_wall_time, true),
  String = "aamjad;" ++ binary_to_list(base64:encode(crypto:strong_rand_bytes(12))), 
  case hasherloop(CoinNeeded, String) of
    {ok, {Original_str, Hash}} -> 
      {_, CPU_Time} = erlang:statistics(runtime),
      {_, Run_Time} = erlang:statistics(wall_clock),
      io:format("Total CPU Time: ~p sec\n", [CPU_Time/1000]),
      io:format("Total Run Time: ~p sec\n", [Run_Time/1000]),
      io:format("Total Cores Used: ~p \n", [divide(CPU_Time, Run_Time)]),
      {Original_str ++ "\n" ++ "Hash: " ++ Hash ++ "\n", Run_Time/1000, self()};
    _Other ->
      io:fwrite("Error finding PoW\n")
    end.

divide(Numerator, 0) ->
    0;
divide(Numerator, Denominator) when Denominator =/= 0 ->
    Numerator / Denominator.