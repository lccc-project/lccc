#include <pthread.h>

void* start(void* v){
    return v;
}

int main(){
    pthread_t pthread;
    pthread_create(&pthread,NULL,start,NULL);
}