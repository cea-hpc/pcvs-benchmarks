TARGETS= test libt.so 

all : $(TARGETS)

CC=$(TS_CC) 

.PHONY: $(TARGETS)

libt.so : b.c
	$(CC) -shared -fpic -L. $^ -o $@


test : a.c libt.so
	$(CC) -g a.c -lt -L. -o test $(PLUG)


clean:	
	rm -fr $(TARGETS)
