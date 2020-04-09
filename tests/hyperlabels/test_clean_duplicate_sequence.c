/* run.config
   LOG: @PTEST_NAME@_labels.c
   LOG: @PTEST_NAME@_labels.labels
   LOG: @PTEST_NAME@_labels.hyperlabels
   LOG: @PTEST_NAME@_output.log
   EXECNOW: @frama-c@ -lannot=ADC -lannot-no-clean-duplicate @PTEST_FILE@ -lannot-o @PTEST_DIR@/result/@PTEST_NAME@_labels.c > @PTEST_DIR@/result/@PTEST_NAME@_output.log
   OPT:
 */


int main(int x)
{
    int a = x - x;
    int b = (a - a) * a;

    return b * b + a * a;
}
