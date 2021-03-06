REBAR=`which rebar || echo -n ./rebar`

all: deps compile
deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

dialyzer: compile
	dialyzer src/*.erl

test: compile
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean

debug: compile
	erl -pa ebin/ -s irckerl_app start

# eof
