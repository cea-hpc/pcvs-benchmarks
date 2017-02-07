TARGETS= test testO3 

all : $(TARGETS)

CC=$(TS_CC)

	
test : an.c bn.c
	$(CC) -g $^ -o $@ 

testO3 : an.c bn.c
	$(CC) -g $^  -o $@ 


clean:	
	rm -fr $(TARGETS)
