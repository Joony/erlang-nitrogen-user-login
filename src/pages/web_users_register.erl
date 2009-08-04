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
%% web_users_register.erl
%%
%% The registration page where users can sign up.  After registration,
%% users will automatically be logged in.
%%

-module(web_users_register).
-include_lib("nitrogen/include/wf.inc").
-include_lib("nitrogen_elements/include/nitrogen_elements.hrl").
%-include_lib("nitrogen_elements/src/element_recaptcha/elements.hrl").
-compile(export_all).

main() ->
    #template { file="./wwwroot/template.html"}.
 																	   

title() ->
	"web_users_register".

body() ->
    Body = [
	    #label { text="Register" },
	    #br {},
	    #label { text="Username:" },
	    #br {},
	    #textbox { id=username },
	    #br {},
	    #label { text="Email Address:" },
	    #br {},
	    #textbox { id=email_address },
	    #br {},
	    #label { text="Password:" },
	    #br {},
	    #password { id=password },
	    #br {},
	    #label { text="Confirm Password:" },
	    #br {},
	    #password { id=password2 },
	    #br {},
	    #recaptcha { id=recaptcha, public_key="AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" },
	    #br {},
	    #button { id=submit, text="Register", postback=register },
	    #flash { id=flash },
	    #panel { id=test }
	   ],
    wf:wire(submit, username, #validate { attach_to=username, validators=[#is_required { text="Required." }]}),
    wf:wire(submit, username, #validate { attach_to=username, validators=[#custom { text="Username already registered.", function=(fun(X, Y) -> is_username_used(X, Y) end) }] }),
    wf:wire(submit, username, #validate { attach_to=username, validators=[#custom { text="Error: Only letters, numbers, and underscores are allowed in usernames.", function=(fun(X, Y) -> check_username(X, Y) end) }] }),
    wf:wire(submit, username, #validate { attach_to=username, validators=[#min_length { text="Error: Username has to be at least three characters.", length=3 }] }),
    wf:wire(submit, email_address, #validate { attach_to=email_address, validators=[#is_required { text="Required." }] }),
    wf:wire(submit, email_address, #validate { attach_to=email_address, validators=[#is_email { text="Required: Ensure that you have entered your email address correctly." }] }),
    wf:wire(submit, email_address, #validate { attach_to=email_address, validators=[#custom { text="Email already registered.", function=(fun(X, Y) -> is_email_used(X, Y) end) }] }),
    wf:wire(submit, password, #validate { attach_to=password2, validators=[#confirm_password { text="Error: Passwords do not match.", password=password2 }] }),
    wf:wire(submit, password, #validate { attach_to=password, validators=[#min_length { text="Error: Password must be at least six characters.", length=6 }] }),
    wf:render(Body).


event(register) ->
    %io:format("response: ~s~n", [hd(wf:q(recaptcha_response_field))]),
    %io:format("challenge: ~s~n", [hd(wf:q(recaptcha_challenge_field))]),
    %io:format("ip: ~p~n", [inet_parse:ntoa(ip())]),
    %io:format("answer: ~p~n", [util_recaptcha:recaptcha_check_answer(inet_parse:ntoa(ip()), hd(wf:q(recaptcha_challenge_field)), hd(wf:q(recaptcha_response_field)))]),
    case util_recaptcha:check_answer(inet_parse:ntoa(util_ip:ip()), hd(wf:q(recaptcha_challenge_field)), hd(wf:q(recaptcha_response_field))) of
	{"true", "success"} ->
	    case db_users:add_user(hd(wf:q(username)), hd(wf:q(email_address)), hd(wf:q(password))) of
		ok ->
		    io:format("New user: ~s has signed up~n", [wf:q(username)]),
		    wf:user(hd(wf:q(username))),
		    wf:redirect("dashboard");
		aborted ->
		    wf:flash("Error: Registration failed, please try again.")
	    end;
	{"false", _} ->
	    %{_,{_,_,Error}} = util_recaptcha:get_error(ErrorCode),
	    %io:format("Error: ~p~n", [Error]),
	    %wf:update(recaptcha, #panel { id=recaptcha, body="<script type='text/javascript'>" ++ Error ++ "</script>" }),
	    wf:flash("Error: The CAPTCHA answer was incorrect, please try again.")
    end;
event(_) -> ok.

is_username_used(_, _) ->
    db_users:is_username_used(hd(wf:q(username))).

is_email_used(_, _) ->
    db_users:is_email_used(hd(wf:q(email_address))).

check_username(_, _) ->
    case regexp:first_match(hd(wf:q(username)), "[^A-z0-9.]") of % not a letter, not a number, or not a period
	nomatch ->
	    true;
	_ ->
	    false
    end.

