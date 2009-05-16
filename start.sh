#!/bin/sh
cd `dirname $0`

echo Starting Nitrogen.
erl \
	-name nitrogen@localhost \
	-pa ./ebin -pa ./include \
	-s make all \
	-eval "application:start(user_login)" \
	-eval "db_utils:start()"
