SHELL=/bin/sh

CC=gcc

CC_OPTS=-m32 -Wall -DDEBUG -g

ERL_INC=-I/opt/erlang/r14b02/lib/erlang/lib/erl_interface-3.7.3/include
WMQ_INC=-I../inc
WMQ_LIB=-L../lib
WMQ_l=-lmqic
WMQ=$(WMQ_INC) $(WMQ_LIB) -Wl,-rpath=../lib -Wl,-rpath=/usr/lib $(WMQ_l)

CSRC=ini.h common.h ini.c common.c

all: echo sender receiver ticker

clean:
	rm -f echo sender receiver

echo: $(CSRC) echo_main.c
	$(CC) $(CC_OPTS) -o echo ini.c common.c echo_main.c $(ERL_INC) $(WMQ)

sender: $(CSRC) sender_main.c
	$(CC) $(CC_OPTS) -o sender ini.c common.c sender_main.c $(ERL_INC) $(WMQ)

receiver: $(CSRC) receiver_main.c
	$(CC) $(CC_OPTS) -o receiver ini.c common.c receiver_main.c $(ERL_INC) $(WMQ)

ticker: $(CSRC) ticker_main.c
	$(CC) $(CC_OPTS) -o ticker ini.c common.c ticker_main.c $(ERL_INC) $(WMQ)
