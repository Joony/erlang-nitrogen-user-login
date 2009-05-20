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
%% string_utils.erl
%%
%% This module provides convenience functions for dealing with strings.
%% It's pretty useless at the moment and only provides one function,
%% which doesn't even return a random string :(
%%

-module(string_utils).
-export([generate_random_string/1, md5_hex/1]).

generate_random_string(Length) ->
    lists:map(fun (_) -> random:uniform(90)+$\s+1 end, lists:seq(1,Length)).

md5_hex(S) ->
    [hex(N) || <<N:4>> <= erlang:md5(S)].

hex(N) when N < 10 ->
        $0+N;
hex(N) when N >= 10, N < 16 ->
    $a+(N-10).
