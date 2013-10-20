NAME=chess

all: compile 

compile:
	./rebar compile

clean:
	./rebar clean
	rm -rf rel/${NAME}

start: compile 
	exec erl -pa $(PWD)/ebin $(PWD)/deps/*/ebin -s ${NAME}
