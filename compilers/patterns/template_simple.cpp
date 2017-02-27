//#include <mpi.h>
#include <cstdlib>
#include <iostream>

template<typename T>
class Foo
{
	T currentVal;
	static T nbVal;
	public:
	static const T min = 1;
	Foo(T val) : currentVal(val) {};
};

template<typename A, size_t s> 
struct myStruct {
	Foo<A> a[s];
};

class Dummy
{
	public:
	static myStruct<int, Foo<int>::min> sub_obj;
	Dummy() {}
};

Dummy obj;
int main(int argc, char *argv[])
{
	fprintf(stderr,"d = %p\n", (void*)&(obj));
	return 0;
}
