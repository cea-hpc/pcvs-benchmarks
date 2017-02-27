#include <pthread.h>
#include <stdio.h>


class My_Mutex {
protected:
  pthread_mutex_t lock;

public:
    My_Mutex()
    {
      fprintf(stderr,"In Constructor\n");
      pthread_mutex_init(&lock,NULL);
      fprintf(stderr,"In Constructor DONE\n");
    }

    // Destructeur
     ~My_Mutex() {
      fprintf(stderr,"In Destructor\n");
      pthread_mutex_destroy(&lock);
      fprintf(stderr,"In Destructor DONE\n");
    }
  
  int Lock();
  int UnLock();
};

int My_Mutex::Lock(){
  return pthread_mutex_lock(&lock);
}

int My_Mutex::UnLock(){
  return pthread_mutex_unlock(&lock);
}

static My_Mutex Mutex_glob;

int main(int argc, char**argv){
  fprintf(stderr,"In Main\n");
  {
    My_Mutex *Mutex_loc;
    Mutex_loc = new My_Mutex();
    
    Mutex_loc->Lock();
    Mutex_glob.Lock();
    
    Mutex_glob.UnLock();
    Mutex_loc->UnLock();

    delete(Mutex_loc);
  }
  fprintf(stderr,"In Main DONE\n");
  return 0;
}
