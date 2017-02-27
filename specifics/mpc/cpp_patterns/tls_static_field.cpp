#include <iostream>
#include <vector>
using namespace std;

#define ASSERT(u, v) do{if(!u){std::cerr << v << std::endl; abort();  }}while(0)
#define REMARK(...) do{printf(__VA_ARGS__);}while(0)
namespace tbb
{
template<typename T>
class atomic
{
	T val;
	public:
	bool operator==(const atomic& obj) const { return val == obj.val;}
	atomic(T val) : val(val) {}
};
}


namespace TestConstExprInitializationOfGlobalObjectsHelper{
    //according to ISO C++11 [basic.start.init], static data fields of class template have unordered
    //initialization unless it is an explicit specialization
    template<typename T>
    struct tester;

    #define TESTER_SPECIALIZATION(T,ct_value)                            \
    template<>                                                           \
    struct tester<T> {                                                   \
        thread_local static tbb::atomic<T> static_atomic;                             \
    };                                                                   \
	thread_local tbb::atomic<T> tester<T>::static_atomic(ct_value);                   \
                                                                         \

	TESTER_SPECIALIZATION(unsigned long,8UL)
}

int main(int argc, char *argv[])
{
	
	return 0;
}
