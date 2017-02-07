#include <iostream>
class A
{
	private:
		static const int MAX;

	public:
		A(){};
		int max(){return MAX;}
};

const int A::MAX = 10;
int main(int argc, char *argv[])
{

	A obj;
	std::cout << "i = " << obj.max() << std::endl;
	return 0;
}
