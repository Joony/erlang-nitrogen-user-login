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
-export([init/0, add_user/3, validate_user/2, delete_user/1, is_username_used/1, is_email_used/1, get_email_address/1, verify_email/1, invalidate_email/1, delete_email_verification_code/1, new_email_verification_code/1]).

-include_lib("stdlib/include/qlc.hrl").

%%% initialize the database and tables
init() ->
    mnesia:start(),
    mnesia:delete_table(users),
    mnesia:create_table(users, [{attributes, record_info(fields, users)}, {disc_copies, [node()]}]),
    mnesia:delete_table(chronology_users),
    mnesia:create_table(chronology_users, [{attributes, record_info(fields, chronology_users)}, {disc_copies, [node()]}]),
    mnesia:delete_table(verification_levels),
    mnesia:create_table(verification_levels, [{attributes, record_info(fields, verification_levels)}, {disc_copies, [node()]}]),
    mnesia:delete_table(verification_codes_email),
    mnesia:create_table(verification_codes_email, [{attributes, record_info (fields, verification_codes_email)}, {disc_copies, [node()]}]),
    mnesia:stop().

%%% add the user to the users database
add_user(Username, EmailAddress, Password) ->
    <<PasswordDigest:160>> = crypto:sha(Password),
    VerificationCode = string_utils:md5_hex(string_utils:generate_random_string(32)),
    CurrentTime = erlang:universaltime(),
    UsersRow = #users { username=Username, email_address=EmailAddress, password=PasswordDigest },
    ChronologyUsersRow = #chronology_users { username=Username, date_joined=CurrentTime, last_logged_in=CurrentTime },
    VerificationLevelsRow = #verification_levels { username=Username },
    VerificationCodesEmailRow = #verification_codes_email { username=Username, verification_code=VerificationCode },
    F = fun() ->
		mnesia:write(UsersRow),
		mnesia:write(ChronologyUsersRow),
		mnesia:write(VerificationLevelsRow),
		mnesia:write(VerificationCodesEmailRow)
	end,
    case mnesia:transaction(F) of
	{atomic, _} ->
	    case validate_user(Username, Password) of
		{valid, _ID} ->
		    email_utils:send_email_verification_code(EmailAddress, VerificationCode),
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
			{aborted, _} ->
			    io:format("Error: Unable to update last_logged_in for user ~s~n", [Username]),
			    {aborted, "Not valid"}
		    end;
		true ->
		    {aborted, "Not valid"}
	    end
    end.

update_last_logged_in(Username) ->
    F = fun() ->
		[E] = mnesia:read(chronology_users, Username, write),
		Update = E#chronology_users{last_logged_in=erlang:universaltime()},
		mnesia:write(Update)
	end,
    mnesia:transaction(F).

delete_user(Username) ->
    F = fun() ->
		mnesia:delete(users, Username, write),
		mnesia:delete(verification_codes_email, Username, write),
		mnesia:delete(chronology_users, Username, write),
		mnesia:delete(verification_levels, Username, write)
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


verify_email(Code) ->
    FGetUsername = fun() ->
		mnesia:match_object({verification_codes_email, '_', Code})
	end,
    case mnesia:transaction(FGetUsername) of
	{atomic, [VerificationCodesEmailRow]} ->
	    %{atomic, [VerificationCodesEmailRow]} = mnesia:transaction(FGetUsername),
	    Username = VerificationCodesEmailRow#verification_codes_email.username,
	    FUpdateVerifiedEmail = fun() ->
					   [VerificationLevels] = mnesia:read(verification_levels, Username, write),
					   VerificationLevelsUpdate = VerificationLevels#verification_levels{verified_email=true},
					   mnesia:write(VerificationLevelsUpdate)
				   end,
	    mnesia:transaction(FUpdateVerifiedEmail),
	    delete_email_verification_code(Username);
	_ ->
	    %io:format("Invalid verification code~n"),
	    {aborted, "invalid verification code"}
    end.

delete_email_verification_code(Username) ->
    F = fun() ->
		mnesia:delete({verification_codes_email, Username})
	end,
    mnesia:transaction(F).

invalidate_email(Username) ->
    FUpdateVerifiedEmail = fun() ->
		[VerificationLevels] = mnesia:read(verification_levels, Username, write),
		VerificationLevelsUpdate = VerificationLevels#verification_levels{verified_email=false},
		mnesia:write(VerificationLevelsUpdate)
	end,
    mnesia:transaction(FUpdateVerifiedEmail).

new_email_verification_code(Username) ->
    VerificationCode = string_utils:md5_hex(string_utils:generate_random_string(32)),
    VerificationCodesEmailRow = #verification_codes_email { username=Username, verification_code=VerificationCode },
    F = fun() ->
		mnesia:write(VerificationCodesEmailRow)
	end,
    mnesia:transaction(F).
