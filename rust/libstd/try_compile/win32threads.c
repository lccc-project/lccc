#include <processthreadsapi.h>

DWORD WINAPI start(LPVOID param){}


int main(){
    CreateThread(NULL,0,start,NULL,0,NULL);
}