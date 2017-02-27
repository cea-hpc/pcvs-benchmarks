#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <sys/types.h>
#include <unistd.h>

constexpr int f(int a, int b) { return a / (b * b );};

const int max = f(100, 4);
 
int tab[max];

int main(int argc, char *argv[])
{
	srand(time(NULL) * getpid());
	int i = rand();
	int j = f(i, i);
	return 0;
}
