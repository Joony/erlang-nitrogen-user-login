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
%% email_utils.erl
%%
%% This module provides basic functions for interacting with the whichever email application you choose.
%%

-module(email_utils).
-include("wf.inc").
-include("config.inc").
-export([send/3, send_email_verification_code/2, test/0]).

send(To, Subject, Body) ->
    esmtp:send(esmtp_mime:msg("<" ++ To ++ ">","<jonathan.mcallister@gmail.com>", Subject, Body)).

test() ->
    EmailAddress = "jonathan.mcallister@gmail.com",
    Code = string_utils:generate_random_string(32),
    CodeDigest = string_utils:md5_hex(Code),
    email_utils:send_email_verification_code(EmailAddress, CodeDigest).

send_email_verification_code(To, VerificationCode) ->
    %%io:format("Code: ~w~n", [VerificationCode]),
    esmtp:send(esmtp_mime:msg("<" ++ To ++ ">","<jonathan.mcallister@gmail.com>", "User Login in Erlang Example - Action required - Please verify your email address", "To verify your email address, click the link below.<br /><br /><a href=\"http://127.0.0.1:8000/web/users/verify/email/?code=" ++ VerificationCode ++ "\">Confirm you email address now.</a>" )).

