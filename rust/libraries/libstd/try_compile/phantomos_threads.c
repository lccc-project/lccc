#include <phantomos/thread.h>

void call(ThreadHandle* hdl,void* ts,void* fs){
    ExitThread(1);
}

int main(){
    struct ThreadStartContext ctx = {call,0,0};
    struct ThreadHandle* h;
    ThreadStart(&ctx,&h);
    return ThreadJoin(h);
}