/* run.config
   EXECNOW: LOG @PTEST_NAME@_labels.c LOG @PTEST_NAME@_labels.labels LOG @PTEST_NAME@_output.log @frama-c@ -lannot=ASSERT @PTEST_DIR@/@PTEST_NAME@.c -lannot-debug 1 -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */


int main(int a, int b, int c){
	//@ assert a & b;
	
	if (c)
		return 0;
	return 1;
}
