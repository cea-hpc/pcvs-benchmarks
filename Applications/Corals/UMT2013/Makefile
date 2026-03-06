VERSION = 2.0c

include make.defs
include distfiles.txt

ifeq ($(STATIC_LINK),on)
SUBDIRS = CMG_CLEAN cmg2Kull Teton
else
#SUBDIRS = CMG_CLEAN cmg2Kull Teton python
SUBDIRS = CMG_CLEAN cmg2Kull Teton 
endif

.PHONY: subdirs $(SUBDIRS) clean

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

# specify compile order dependency
cmg2Kull: CMG_CLEAN
Teton : cmg2Kull
python : Teton

clean:
	@for i in ${SUBDIRS} ; do (cd $$i; $(MAKE) clean); done

veryclean:
	@for i in ${SUBDIRS} ; do (cd $$i; $(MAKE) veryclean); done

DISTDIR = cmg-umt-$(VERSION)
dist: $(DISTFILES)
	rm -rf $(DISTDIR) 
	rm -f $(DISTDIR).tar.gz
	mkdir $(DISTDIR)
	list='$(DISTFILES)'; for f in $$list; do \
		cp --parents $$f $(DISTDIR); \
	done
	-find $(DISTDIR) -type d -exec chmod 777 {} \;
	-find $(DISTDIR) -type f -exec chmod a+wr {} \;
	tar cf $(DISTDIR).tar $(DISTDIR)
	gzip $(DISTDIR).tar
	rm -rf $(DISTDIR)
