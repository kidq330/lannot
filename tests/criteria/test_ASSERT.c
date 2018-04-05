/* run.config
   EXECNOW: LOG @PTEST_NAME@_labels.c LOG @PTEST_NAME@_labels.labels LOG @PTEST_NAME@_output.log @frama-c@ -lannot=ASSERT -lannot-debug 1 @PTEST_DIR@/@PTEST_NAME@.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
   EXECNOW: mv @PTEST_DIR@/@PTEST_NAME@_labels.c @PTEST_DIR@/result/@PTEST_NAME@_labels.c
   EXECNOW: mv @PTEST_DIR@/@PTEST_NAME@_labels.labels @PTEST_DIR@/result/@PTEST_NAME@_labels.labels
 */

#include "limits.h"

int main(int a, int b){
	if(a && b){
		//@ assert !a || !b;
		return a+1;
	 }
	else
		//@ assert !a || !b; 
		return b;
}
