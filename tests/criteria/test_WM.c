/* run.config
   EXECNOW: LOG @PTEST_NAME@_labels.c @frama-c@ -lannot=WM -lannot-mutators=-COR @PTEST_DIR@/@PTEST_NAME@.c -lannot-debug 2 -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */


int main(int a, int b){
	return a + b;
}
