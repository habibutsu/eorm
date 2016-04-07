.PHONY: test

REBAR=./rebar3

compile:
	${REBAR} compile

test:
	${REBAR} ct
	${REBAR} cover

test_suite:
	${REBAR} ct --suite=../eorm_${suite}_SUITE
	${REBAR} cover

clean:
	${REBAR} clean
