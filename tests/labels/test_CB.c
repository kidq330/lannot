 /* run.config
   STDOPT: +"-lannot=CB" 
 */


#include <stdbool.h>

int maintest(int a, unsigned int b, short c, int d, bool e, long long f){
    if(a < b && c == 42 && !d) return 1;
    if(e <= 42) return 2;
    if(f + 12 != 26) return 3;
	return 0;
}
