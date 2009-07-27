-ifndef(config_inc).
-define(config_inc, ok).


-record(users, { username, email_address, password }).
-record(chronology_users, { username, date_joined, last_logged_in }).
-record(verification_levels, { username, verified_email=false }).
-record(verification_codes_email, { verification_code, username }).
 
-endif.