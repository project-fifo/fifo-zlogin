TARGET_DIR ?=/opt/local
FILE ?=$(COMPONENT)-$(VERSION)$(SUFFIX)
BLOCK_SIZE ?=65536
STAGE_DIR ?=deploy
TMP_DIR ?=tmp
PKG_CATEGORY ?=fifo
PKG_HOMEPAGE ?=https://project-fifo.net

.PHONY: package_list dep_list clean-pkg

package_list:
	-rm plist || true
	(cd $(STAGE_DIR); find * -type f | sort) >> plist

dep_list:
	( echo 'deps:  { '; \
		for dep in ${DEPS}; do \
		pkg query --glob "	\"%n\" : { \"origin\" : \"%o\", \"version\" : \"%v\" }," "$$dep"; \
		done ; \
		echo '}' ) > deplist

clean-pkg:
	-rm -r tmp build-info packlist

tmp/$(FILE).tgz: dep_list package_list 
	-rm -r tmp
	mkdir tmp
	pkg_create -i install.sh -k deinstall.sh -D displayfile -B build-info -c comment -d description -f packlist -I $(TARGET_DIR) -p $(STAGE_DIR) -U tmp/$(FILE).tgz
