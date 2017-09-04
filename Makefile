APP=fifo_zlogin
include config.mk
REBAR = $(shell pwd)/rebar3

.PHONY: deps rel quick-test tree 

uname_S := $(shell sh -c 'uname -s 2>/dev/null || echo not')
uname_V6 := $(shell sh -c 'uname -v 2>/dev/null | cut -c-6 || echo not')


ifeq ($(uname_S),FreeBSD)
	PLATFORM = freebsd
endif
ifeq ($(uname_V6),joyent)
	PLATFORM = smartos
endif

include fifo.mk

apps/fifo_zlogin/priv/runpty: utils/runpty.c
	${CC} utils/runpty.c -o apps/fifo_zlogin/priv/runpty

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > fifo_zlogin.version

version_header: version
	@echo "-define(VERSION, <<\"$(shell cat fifo_zlogin.version)\">>)." > apps/fifo_zlogin/src/fifo_zlogin_version.hrl
