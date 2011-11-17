.SUFFIXES: .erl .beam .yrl
#.PHONY: all main subdirs clean run-shell

# find all .erl files in ./src/, and compile them to the same structure in ./ebin/
ERL_SRC := $(shell find src -name '*.erl')
ERL_OBJ := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})
ERL_SRC_NEW := $(patsubst src/%.erl,ebin/%.erl,${ERL_SRC})
SRC_SUBDIRS := $(shell find src -type d)
OBJ_SUBDIRS := $(patsubst src%,ebin%,${SRC_SUBDIRS})

ERL=erl
ERLC=erlc
FLAGS=-W9 +verbose +report_errors +report_warnings +trace +debug_info

all: compile ebin/irckerl.app
compile: ${OBJ_SUBDIRS} ${ERL_OBJ}
compile-debug: ${ERL_SRC_NEW}


${OBJ_SUBDIRS}:
	mkdir $@

ebin/%.app: src/%.app.src
	cp $< $@

ebin/%.beam: src/%.erl
	${ERLC} ${FLAGS} -o $(dir $@) $<

ebin/%.erl: src/%.erl
	cp $< $@

debug: compile compile-debug
	${ERL} -pa ebin/ -s irckerl_ctrl start

clean:
	rm -rf ebin/
	rm -f erl_crash.dump

# eof
