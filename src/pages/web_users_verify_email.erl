-module (web_users_verify_email).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() ->
    [Code] = wf:q("key"),
    case db_users:verify_email(Code) of
	{atomic,ok} ->
	    io:format("Everything looks ok~n");
	{aborted, "invalid verification code"} ->
	    io:format("Invalid verification code~n");
	_ ->
	    io:format("Something went wrong~n")
    end,

#template { file="./wwwroot/template.html"}.
    

title() ->
	"web_users_verify_email".

body() ->
	#label{text="web_users_verify_email body."}.
	
event(_) -> ok.
