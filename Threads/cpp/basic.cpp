#include <thread>
#include <iostream>
#include <sstream>

void foo()
{
	std::cout << "Hello from thread" << std::endl;
}

int main(int argc, char *argv[])
{
	std::thread t(foo);

	std::ostringstream strdata;

	strdata << t.get_id();
	std::string str = strdata.str();

	std::cout << str << std::endl;

	if(str.length() == 0)
	{
		return 1;
	}

	t.join();

	return 0;
}
