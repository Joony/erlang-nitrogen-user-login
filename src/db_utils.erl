%% Copyright 2009 Joony (jonathan.mcallister@gmail.com)
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% db_utils.erl
%%
%% This module provides basic functions for interacting with the database.
%%

-module(db_utils).
-include("wf.inc").
-include("config.inc").
-export([init/0, start/0, stop/0, write/1, do/1, get_all_rows/1]).

-include_lib("stdlib/include/qlc.hrl").

%%% initialize the database and tables. Only run once!
init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    db_users:init(),
    mnesia:stop().

%%% start the database
start() ->
    crypto:start(),
    mnesia:start().

%%% stop the database
stop() ->
    crypto:stop(),
    mnesia:stop().

write(Row) ->
    F = fun() ->
		mnesia:write(Row)
    end,
    mnesia:transaction(F).

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    case mnesia:transaction (F) of
	{atomic, Val} ->
	    Val;
	{aborted, Reason} ->
	    io:format("Query: ~w aborted.~nReason: ~w~n", [Q, Reason]),
	    aborted
    end.

get_all_rows(Table) ->
     mnesia:transaction(fun() -> qlc:eval(qlc:q([X || X <- mnesia:table(Table)])) end).

