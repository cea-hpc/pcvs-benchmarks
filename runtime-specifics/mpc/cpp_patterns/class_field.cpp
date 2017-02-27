#include<cassert>

class Foo
{
	public:
		int val;
		static int attr1 ; //privatized
		static int * attr2 ; // privatized
		static const int attr3 ; // NOT PRIVATIZED
		static const int * attr4 ; //privatized
		static int * const attr5 ; // NOT PRIVATIZED

		Foo(int val = 0) : val(val){};
};

int main(int argc, char *argv[])
{
	Foo obj;	
	return 0;
}
