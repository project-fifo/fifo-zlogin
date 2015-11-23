REBAR = $(shell pwd)/rebar3

.PHONY: deps rel package quick-test tree

all: apps/fifo_zlogin/priv/runpty version compile

include fifo.mk

apps/fifo_zlogin/priv/runpty: utils/runpty.c
	gcc utils/runpty.c -o apps/fifo_zlogin/priv/runpty

version:
	echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > fifo_zlogin.version

version_header: version
	cp fifo_zlogin.version rel/files/fifo_zlogin.version
	echo "-define(VERSION, <<\"$(shell cat fifo_zlogin.version)\">>)." > apps/fifo_zlogin/src/fifo_zlogin_version.hrl

package: rel
	make -C rel/pkg package

rel: all
	-rm -r ./rel/fifo_zlogin/share
	$(REBAR) as prod release
