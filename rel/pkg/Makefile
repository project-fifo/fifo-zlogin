VERSION=0.8.0
COMPONENT=fifo_zlogin
SUFFIX:=$(shell echo $(SUFFIX))
BASENAME:=$(COMPONENT)-$(VERSION)$(SUFFIX)
.PHONY: package clean

clean:
	rm *.gz

package:
	cp install.sh $(BASENAME)
	cp -r ../../_build/prod/rel/$(COMPONENT) $(COMPONENT)
	tar -cf $(BASENAME).tar $(COMPONENT)
	rm -rf $(COMPONENT)
	uuencode $(BASENAME).tar $(BASENAME).tar >> $(BASENAME)
	[ -f $(BASENAME).gz ] && rm $(BASENAME).gz || true
	rm  $(BASENAME).tar
	gzip -9 $(BASENAME)
