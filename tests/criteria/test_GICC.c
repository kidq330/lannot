/* run.config
   EXECNOW: LOG @PTEST_NAME@_labels.c @frama-c@ -lannot=GICC @PTEST_DIR@/@PTEST_NAME@.c -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */


int main(int a, int b, int c){
	if (a && b || c)
		return 0;
	return 1;
}
