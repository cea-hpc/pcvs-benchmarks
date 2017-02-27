#include <iostream>

class A
{
	public:
		A(size_t val) {printf("CONSTRUCT %p\n", (void*)this);}
		~A() {printf("DESTRUCT %p\n", (void*)this);}
};
thread_local int a;
thread_local A obj(20);

int main(int argc, char *argv[])
{
	printf("obj = %p\n", (void*)(&obj));
	printf("a = %p\n", (int*)(&a));
	return 0;
}
