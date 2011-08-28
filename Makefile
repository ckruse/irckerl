.SUFFIXES: .erl .beam .yrl
#.PHONY: all main subdirs clean run-shell

# find all .erl files in ./src/, and compile them to the same structure in ./ebin/
ERL_SRC := $(shell find src -name '*.erl')
ERL_OBJ := $(patsubst src/%.erl,ebin/%.beam,${ERL_SRC})
SRC_SUBDIRS := $(shell find src -type d)
OBJ_SUBDIRS := $(patsubst src%,ebin%,${SRC_SUBDIRS})


all: compile ebin/irckerl.app
compile: ${OBJ_SUBDIRS} ${ERL_OBJ}

${OBJ_SUBDIRS}:
	mkdir $@

ebin/%.app: src/%.app.src
	cp $< $@

ebin/%.beam: src/%.erl
	erlc +debug_info -o $(dir $@) $<

debug: compile
	erl -pa ebin/ -s irckerl_ctrl start

clean:
	rm -rf ebin/
	rm -f erl_crash.dump

# eof
