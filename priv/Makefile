SHELL = /bin/sh

all:
	cd spawner && make
	mkdir -p ./bin
	cp spawner/spawner ./bin/spawner
	cd eai_clients && make
	mkdir -p ./bin
	cd eai_clients && cp echo sender receiver ticker ../bin/

clean:
	cd eai_clients && make clean
	-(cd bin && rm -f echo sender receiver ticker)
