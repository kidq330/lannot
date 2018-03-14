/* run.config
   EXECNOW: LOG @PTEST_NAME@_labels.c LOG @PTEST_NAME@_labels.hyperlabels LOG @PTEST_NAME@_labels.labels LOG @PTEST_NAME@_output.log @frama-c@ -lannot=FCC @PTEST_DIR@/@PTEST_NAME@.c -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
 */


int f(){
	return 0;
}

void main(){
	f();
}
