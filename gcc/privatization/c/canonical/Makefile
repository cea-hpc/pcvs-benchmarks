TARGETS= test testO3

all : $(TARGETS)

CC=$(TS_CC) 

test : a.c b.c
	$(CC) -g -rdynamic -O0 $^ -o $@ 

testO3 : a.c b.c
	$(CC) -g -O3 $^ -o $@

.PHONY: $(TARGETS)
