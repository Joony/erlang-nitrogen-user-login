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
%% web_users_dashboard.erl
%%
%% A users homepage.  Users will be directed here after logging in.
%%

-module (web_users_dashboard).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() ->
    Username = wf:user(),
    case Username of
	undefined ->
	    wf:redirect("login");
	_ ->
	    ok
    end,
    #template { file="./wwwroot/template.html"}.

title() ->
	"web_users_dashboard".

body() ->
    Username = wf:user(),
    Body = [
	    #label { text="web_users_dashboard body." },
	    #gravatar { email=db_users:get_email_address(Username), size="60" },
	    #br {},
	    #link { text="Logout", postback=logout },
	    #br {}
    ],
    wf:render(Body).

event(logout) ->
    wf:clear_user(),
    wf:redirect ("login");
event(_) ->
    ok.
