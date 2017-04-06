SUBDIRS= CMI Field Region

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

Region: CMI
Field: CMI

clean:
	@for i in ${SUBDIRS} ; do (cd $$i; $(MAKE) clean); done

veryclean:
	@for i in ${SUBDIRS} ; do (cd $$i; $(MAKE) veryclean); done
