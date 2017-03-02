VERSION=0.9.1
COMPONENT=fifo_zlogin
SUFFIX:=$(shell echo $(SUFFIX))
BASENAME:=$(COMPONENT)-$(VERSION)$(SUFFIX)
.PHONY: package clean

clean:
	rm *.gz

package:
	cp install.sh $(BASENAME)
	cp -r ../../_build/prod/rel/$(COMPONENT) $(COMPONENT)
	cp /opt/local/lib/libncurses.so.6 $(COMPONENT)/share/lib/libncurses.so.6
	cp /opt/local/lib/libcrypto.so.1.0.0 $(COMPONENT)/share/lib/libcrypto.so.1.0.0
	tar -cf $(BASENAME).tar $(COMPONENT)
	rm -rf $(COMPONENT)
	uuencode $(BASENAME).tar $(BASENAME).tar >> $(BASENAME)
	[ -f $(BASENAME).gz ] && rm $(BASENAME).gz || true
	rm  $(BASENAME).tar
	gzip -9 $(BASENAME)
