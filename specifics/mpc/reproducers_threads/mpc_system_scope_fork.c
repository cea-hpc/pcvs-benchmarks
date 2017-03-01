#include <mpc.h>



void *foo( void *b)
{
}




int main(int argc, const char *argv[])
{
	int pid = 1;

	sctk_thread_attr_t attr;
	sctk_thread_attr_init (&attr);
	sctk_thread_attr_setscope (&attr, SCTK_THREAD_SCOPE_SYSTEM);

	sctk_thread_t th;

	int i = 0;

	for (i = 0; i < 10; i++)
	{
		sctk_user_thread_create( &th, &attr, foo, NULL);
	}



	pid = fork();

	if( pid == 0 )
	{
		for (i = 0; i < 10; i++)
		{
			sctk_user_thread_create( &th, &attr, foo, NULL);
		}

	}
	else
	{
		wait(NULL);
	}    

	return 0;
}
