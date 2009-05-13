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
%% web_users_login.erl
%%
%% The login screen.
%%

-module (web_users_login).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() -> 
	#template { file="./wwwroot/template.html"}.

title() ->
	"web_users_login".

body() ->

    Body = [
	    #label { text="Login:" },
	    #textbox { id=username, postback=login, next=password },
	    #br {},
	    #label { text="Password:" },
	    #password { id=password, postback=login, next=submit },
	    #br {},
	    #button { id=submit, text="Login", postback=login }
    ],
    wf:wire(submit, username, #validate { validators = [ #is_required { text="Required" }]}),
    wf:render(Body).

event(login) ->
    case db_users:validate_user(hd(wf:q(username)), hd(wf:q(password))) of
	{ valid, _ID } ->
	    io:format("User: ~s has logged in~n", [wf:q(username)]),
	    wf:flash("Correct"),
	    wf:user(hd(wf:q(username))),
	    wf:redirect("dashboard");
	_ ->
	    wf:flash("Incorrect")
    end;

event(_) -> ok.
