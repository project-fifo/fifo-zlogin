REBAR = $(shell pwd)/rebar3

.PHONY: deps rel package quick-test tree dist

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
	gcc utils/runpty.c -o apps/fifo_zlogin/priv/runpty

version:
	@echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > fifo_zlogin.version

version_header: version
	@echo "-define(VERSION, <<\"$(shell cat fifo_zlogin.version)\">>)." > apps/fifo_zlogin/src/fifo_zlogin_version.hrl

dist: ${PLATFORM} ;

package: rel
	make -C rel/pkg package

freebsd: update ${PLATFORM}/rel
	gmake -C rel/pkgng package

freebsd/rel: apps/fifo_zlogin/priv/runpty version_header
	$(REBAR) as prod release

smartos: update ${PLATFORM}/rel
	make -C rel/bootstrap bootstrap

smartos/rel: apps/fifo_zlogin/priv/runpty version_header compile
	-rm -r ./rel/fifo_zlogin/share
	$(REBAR) as prod release


clean:
	$(REBAR) clean
	make -C rel/bootstrap clean
	make -C rel/pkgng clean