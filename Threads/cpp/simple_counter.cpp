#include <iostream>
#include <thread>
#include <mutex>
#include <vector>

class Counter
{
public:
	Counter()
	{
		this->counter = 0;
	}

	void incr()
	{
		this->m.lock();
		this->counter++;
		this->m.unlock();
	}

	int get()
	{
		int ret = 0;

		this->m.lock();
		ret = this->counter;
		this->m.unlock();

		return ret;
	}

private:
	int        counter = 0;
	std::mutex m;
};

static class Counter cnt;

void thread_main()
{
	std::cout << "Hello from thread" << std::endl;

	cnt.incr();
}

int main()
{
	for(int i = 0; i < 64; i++)
	{
		std::vector <std::thread *> ths;

		for(int j = 0; j < 32; j++)
		{
			std::thread *th = new std::thread(thread_main);
			ths.push_back(th);
		}

		for(int j = 0; j < 32; j++)
		{
			std::thread *th = ths.back();
			ths.pop_back();
			std::cout << "Thread id: " << th->get_id() << std::endl;
			th->join();
			delete th;
		}
	}


	if(cnt.get() != (64 * 32) )
	{
		return 1;
	}

	return 0;
}
