/* run.config
   EXECNOW: LOG @PTEST_NAME@_labels.c LOG @PTEST_NAME@_labels.hyperlabels LOG @PTEST_NAME@_labels.labels LOG @PTEST_NAME@_output.log @frama-c@ -lannot=FCC @PTEST_DIR@/@PTEST_NAME@.c -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */

extern void printf();
// return index of the last element
// in X that equals y.
// if y is not in X, return -1.
int findLast (int *tab, int n, int y) {
	for (int i = n-1; i>=0; i--){
		if (tab[i] == y)
			return i;
	}
	return -1;
}

int main(){
	int tab[10];
	int x = 4;
	for(int i = 0; i < 10; i++)
		tab[i] = i+1;
	int i = findLast (tab,10,x);
	printf("%d à la case %d, -1 si notfound",x,i);
	return 0;

}

/*int f(){
	return 0;
}

int main(int c){
	int a = 1;
	int b = a;
	
	if (a){
		a = f();
		a = 3;
		b = a;
	}
	else
		a = c + b;
	
	return b;
}


int main(int c){
   int a = 1;
   int b = a;
   
   switch(c){
   
     case 1 :
     	return a + b;
     case 12 : 
     	return c;
     default :
     	return ( c ? 0 : c);
   
   }

}*/
