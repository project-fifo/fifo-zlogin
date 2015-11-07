REBAR = $(shell pwd)/rebar3

.PHONY: deps rel package quick-test tree

quick-test: cp-hooks
	-$(REBAR) compile 
	$(REBAR) eunit

all: cp-hooks deps compile

cp-hooks:
	#cp hooks/* .git/hooks

apps/fifo_zlogin/priv/runpty: utils/runpty.c
	gcc utils/runpty.c -o apps/fifo_zlogin/priv/runpty

version:
	echo "$(shell git symbolic-ref HEAD 2> /dev/null | cut -b 12-)-$(shell git log --pretty=format:'%h, %ad' -1)" > fifo_zlogin.version

version_header: version
	cp fifo_zlogin.version rel/files/fifo_zlogin.version
	echo "-define(VERSION, <<\"$(shell cat fifo_zlogin.version)\">>)." > apps/fifo_zlogin/src/fifo_zlogin_version.hrl

package: rel
	make -C rel/pkg package

compile: apps/fifo_zlogin/priv/runpty version_header
	$(REBAR) compile

deps:
	$(REBAR) update

clean:
	$(REBAR) clean
	make -C rel/pkg clean

test: all
	$(REBAR) xref
	$(REBAR) eunit

rel: all
	-rm -r ./rel/fifo_zlogin/share
	$(REBAR) as prod release

###
### Docs
###
docs:
	$(REBAR) doc

##
## Developer targets
##

stage : rel
	$(foreach dep,$(wildcard deps/* wildcard apps/*), rm -rf rel/fifo_zlogin/lib/$(shell basename $(dep))-* && ln -sf $(abspath $(dep)) rel/fifo_zlogin/lib;)

##
## Dialyzer
##
APPS = kernel stdlib sasl erts ssl tools os_mon runtime_tools crypto inets \
	xmerl webtool snmp public_key mnesia eunit syntax_tools compiler
COMBO_PLT = $(HOME)/.fifo_zlogin_combo_dialyzer_plt

check_plt: compile
	dialyzer --check_plt --plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

build_plt: compile
	dialyzer --build_plt --output_plt $(COMBO_PLT) --apps $(APPS) \
		deps/*/ebin apps/*/ebin

dialyzer: compile
	@echo
	@echo Use "'make check_plt'" to check PLT prior to using this target.
	@echo Use "'make build_plt'" to build PLT prior to using this target.
	@echo
	@sleep 1
	dialyzer -Wno_return --plt $(COMBO_PLT) deps/*/ebin apps/*/ebin | grep -v -f dialyzer.mittigate

cleanplt:
	@echo
	@echo "Are you sure?  It takes about 1/2 hour to re-build."
	@echo Deleting $(COMBO_PLT) in 5 seconds.
	@echo
	sleep 5
	rm $(COMBO_PLT)

tree:
	rebar3 tree | grep -v '=' | sed 's/ (.*//' > tree

tree-diff: tree
	git diff test -- tree
