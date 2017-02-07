#include <stdio.h>
#include <mpc.h>

class A
{
	public:
	static const char a;
	static constexpr char * const b = "My chain is contained by b";
};

const char A::a = 'a';

class B
{
	public:
	static constexpr char a = A::a;
	static constexpr char * b = A::b;
};

int main(int argc, char *argv[])
{
	printf("%c\n", A::a);
	printf("%s\n", A::b);

	return 0;
}
