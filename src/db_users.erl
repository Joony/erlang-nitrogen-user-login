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
%% db_users.erl
%%
%% This module provides all the functions for dealing with users.
%%

-module(db_users).
-include("wf.inc").
-include("config.inc").
-export([init/0, add_user/3, validate_user/2, delete_user/1, is_username_used/1, is_email_used/1, get_email_address/1, verify_email/2, update_verification_code/2, delete_verification_code/1]).

-include_lib("stdlib/include/qlc.hrl").

%%% initialize the database and tables
init() ->
    mnesia:start(),
    mnesia:delete_table(verification_codes),
    mnesia:create_table(verification_codes, [{attributes, record_info (fields, verification_codes)}, {disc_copies, [node()]}]),
    mnesia:delete_table(users),
    mnesia:create_table(users, [{attributes, record_info (fields, users)}, {disc_copies, [node()]}]),
    mnesia:stop().

%%% add the user to the users database
add_user(Username, EmailAddress, Password) ->
    <<PasswordDigest:160>> = crypto:sha(Password),
    Code = string_utils:generate_random_string(32),
    <<CodeDigest:128>> = crypto:md5(Code),
    io:format("CodeDigest: ~w~n", [CodeDigest]),
    UsersRow = #users { username=Username, email_address=EmailAddress, password=PasswordDigest, date_joined=erlang:universaltime() },
    VerificationCodeRow = #verification_codes { email_address=EmailAddress, verification_code=Code },
    F = fun() ->
		mnesia:write(UsersRow),
		mnesia:write(VerificationCodeRow)
	end,
    case mnesia:transaction(F) of
	{atomic, Val} ->
	    case validate_user(Username, Password) of
		{valid, _ID} ->
		    email_utils:send_email_verification_code(EmailAddress, CodeDigest),
		    ok;
		{aborted, Reason} ->
		    io:format("Failed to login after registration. Reason: ~s~n", [Reason]),
		    {aborted, Reason}
	    end;
	{aborted, Reason} ->
	    io:format ("Added user failed!~nReason: ~s~n", [Reason]),
	    {aborted, Reason}
    end.

validate_user(Username, Password) ->
    <<PasswordDigest:160>> = crypto:sha(Password),
    case db_utils:do (qlc:q ([X#users.username ||  X <- mnesia:table(users), check(X#users.username, X#users.email_address, Username), X#users.password == PasswordDigest])) of
	fail ->
	    {aborted, "Not valid"};
        Results ->
            if length (Results) == 1 ->
		    %% update the last logged in time
		    case update_last_logged_in(Username) of
			{atomic, ok} ->
			    {valid, hd(Results)};
			{aborted, Val} ->
			    io:format("Error: Unable to update last_logged_in for user ~s~n", [Username]),
			    {aborted, "Not valid"}
		    end;
		true ->
		    {aborted, "Not valid"}
	    end
    end.

update_last_logged_in(Username) ->
    F = fun() ->
		[E] = mnesia:read(users, Username, write),
		Update = E#users{last_logged_in=erlang:universaltime()},
		mnesia:write(Update)
	end,
    mnesia:transaction(F).

delete_verification_code(EmailAddress) ->
    F = fun() ->
		mnesia:delete(verification_codes, EmailAddress, write)
	end,
    mnesia:transaction(F).

delete_user(Username) ->
    F = fun() ->
		mnesia:delete(users, Username, write)
	end,
    mnesia:transaction(F).

%% not implemented yet, but will be used for the forgot password process if the user has already requested a code.
update_verification_code(EmailAddress) ->
    update_verification_code(EmailAddress, "").

update_verification_code(EmailAddress, Code) ->
    F = fun() ->
		[E] = mnesia:read(verification_codes, EmailAddress, write),
		Update = E#verification_codes{verification_code=Code},
		mnesia:write(Update)
	end,
    mnesia:transaction(F).

%% result seems backwards compared to the name
is_username_used(Username) ->
    case db_utils:do(qlc:q ([X#users.username || X <- mnesia:table(users), string:equal(X#users.username, Username)])) of
	aborted ->
	    false;
	Results ->
	    if
		length(Results) == 1 ->
		    false;
		true ->
		    true
	    end
    end.

%%% result seems backwards compared to the name
is_email_used(EmailAddress) ->					
    case db_utils:do(qlc:q ([X#users.email_address || X <- mnesia:table(users), string:equal(X#users.email_address, EmailAddress)])) of
	aborted ->
	    false;
	Results ->
	    if
		length(Results) == 1 ->
		    false;
		true ->
		    true
	    end
    end.

check(Username, EmailAddress, Input) ->
    if 
	Username == Input ; EmailAddress == Input ->
	    true;
	true ->
	    false
    end.

%%% Used to get the email address for Gravatar
get_email_address(Username) ->
    db_utils:do(qlc:q([X#users.email_address || X <- mnesia:table(users), X#users.username =:= Username])).


verify_email(EmailAddress, Code) ->
    F = fun() ->
		Result = qlc:e(qlc:q([X#verification_codes.email_address || X <- mnesia:table(verification_codes), X#verification_codes.email_address =:= EmailAddress, X#verification_codes.verification_code =:= Code])),
		io:format("Result: ~s, ~w~n", [Result, length(Result)]),
		if
                    length(Result) == 1 ->
			io:format("Found one result~n"),
			mnesia:delete(verification_codes, EmailAddress, write),
			[User] = qlc:e(qlc:q([X || X <- mnesia:table(users), X#users.email_address =:= EmailAddress])),
			io:format("User: ~w~n", [User]),
			UserUpdate = User#users{verified=true},
			mnesia:write(UserUpdate);
		    true ->
			{aborted, "Error: problem with the validation code"}
		end
	end,
    case mnesia:transaction (F) of
        {atomic, Val} ->    
            Val;
	{aborted, Reason} ->
            io:format ("Query: ~w aborted.~nReason: ~w~n", [F, Reason]),
            aborted    
    end.
