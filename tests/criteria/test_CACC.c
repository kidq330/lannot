/* run.config
   LOG: @PTEST_NAME@_labels.c 
   LOG: @PTEST_NAME@_labels.labels
   LOG: @PTEST_NAME@_labels.hyperlabels
   LOG: @PTEST_NAME@_output.log
   EXECNOW: @frama-c@ -lannot=CACC @PTEST_FILE@ -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
   OPT:
 */


int maintest(int a, int b, int c){
	if (a == b)
		a = a - b;

	if (a && b || c)
		return 0;
	return 1;
}
