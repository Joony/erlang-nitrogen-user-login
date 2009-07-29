-module (web_users_verify_email).
-include_lib ("nitrogen/include/wf.inc").
-compile(export_all).

main() ->
    [Code] = wf:q("key"),
    case db_users:verify_email(Code) of
	{atomic,ok} ->
	    Validated = true,
	    %io:format("Everything looks ok~n");
	{aborted, "invalid verification code"} ->
	    Validated = false,
	    wf:redirect("/web/errors/verification/email"),
	    %io:format("Invalid verification code~n");
	_ ->
	    Validated = false,
	    wf:redirect("/web/errors/unexpected"),
	    %io:format("Something went wrong~n")
    end,
    #template { file="./wwwroot/validation.html", bindings=[{'Validated', Validated}] }.
    

title() ->
	"web_users_verify_email".

body(Validated) ->
    io:format("Validated = ~s~n", [Validated]),
    case Validated of
	true ->
	    #label{text="Your email address has been successfully validated"};
	_ ->
	    #label{text="There was a problem validating your email address"}
    end.
	
event(_) -> ok.
