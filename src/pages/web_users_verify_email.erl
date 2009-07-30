-module (web_users_verify_email).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() ->
    [Code] = wf:q("code"),
    case db_users:verify_email(Code) of
	{atomic,ok} ->
	    Verified = true;
	    %io:format("Everything looks ok~n");
	{aborted, "invalid verification code"} ->
	    Verified = false,
	    wf:redirect("/web/errors/verification/email");
	    %io:format("Invalid verification code~n");
	_ ->
	    Verified = false,
	    wf:redirect("/web/errors/unexpected")
	    %io:format("Something went wrong~n")
    end,
    #template { file="./wwwroot/verification.html", bindings=[{'Verified', Verified}] }.
    

title() ->
	"web_users_verify_email".

body(Verified) ->
    io:format("Verified = ~s~n", [Verified]),
    case Verified of
	true ->
	    #label{text="Your email address has been successfully verified"};
	_ ->
	    #label{text="There was a problem verifying your email address"}
    end.
	
event(_) -> ok.
