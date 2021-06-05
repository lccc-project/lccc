#include <threads.h>

int start(void*){return 0;}

int main(){
    thrd_t thread;
    thrd_create(&thread,start,NULL);
}