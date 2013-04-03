SHELL=/bin/sh

CC=gcc

CC_OPTS=-m32 -Wall 

all: spawner

clean:
	rm -f spawner

spawner: main.c
	$(CC) $(CC_OPTS) -o spawner main.c
