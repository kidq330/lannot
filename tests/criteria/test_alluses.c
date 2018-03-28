/* run.config
   EXECNOW: LOG @PTEST_NAME@_labels.c LOG @PTEST_NAME@_labels.hyperlabels LOG @PTEST_NAME@_labels.labels LOG @PTEST_NAME@_output.log @frama-c@ -lannot=alluses -lannot-debug 1 @PTEST_DIR@/@PTEST_NAME@.c -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */

int f(){
	return 0;
}

int main(int c){
	int a = 1;
	int b = 2;
	
	if (a){
		if(b){
			b = 3;
		}
		else{
			a = f();
		}
	}
	else{
		a = b;
		if (b){
			a = 3;
			b = b;	
		}
		else{
			b = a;		
		}
	}
	
	switch(c){
   
     case 1 :
     	return a + b;
     case 12 : 
     	return c;
     default :
     	return ( c ? 0 : c);
   
   }
   return 0;
}
