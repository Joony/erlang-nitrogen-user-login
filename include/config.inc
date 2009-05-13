-ifndef(config_inc).
-define(config_inc, ok).


-record(users, { username, email_address, password, date_joined, last_logged_in, verified=false }).
-record(verification_codes, { email_address, verification_code }).
 
-endif.