/* run.config
   STDOPT: +"-lannot=context"
 */

int maintest(int c){
	int a = 1;
	int b = 2;
	
	while(c){
		b = a + c;
		a = 2;
	}
	
   return a + b + c;
}
