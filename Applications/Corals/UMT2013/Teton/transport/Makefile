SUBDIRS= Teton TetonInterface

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

clean:
	@for i in ${SUBDIRS} ; do (cd $$i; $(MAKE) clean); done

veryclean:
	@for i in ${SUBDIRS} ; do (cd $$i; $(MAKE) veryclean); done
