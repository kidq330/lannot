/* Generated by Frama-C LTest */

#include "errno.h"
#include "math.h"
#include "stdarg.h"
#include "stddef.h"
#include "stdio.h"

#ifndef pc_label
#define pc_label(...) do{}while(0)
#endif
#ifndef pc_label_bindings
#define pc_label_bindings(...) do{}while(0)
#endif
#ifndef pc_label_sequence
#define pc_label_sequence(...) do{}while(0)
#endif
#ifndef pc_label_sequence_condition
#define pc_label_sequence_condition(...) do{}while(0)
#endif

int armstrong(int low, int high)
 {
   int __retres;
   int i;
   int temp1;
   int temp2;
   int remainder_0;
   int __SEQ_STATUS_low_1 = 1;
   int __SEQ_STATUS_high_2 = 1;
   int __SEQ_STATUS_i_3 = 0;
   int __SEQ_STATUS_i_4 = 0;
   int __SEQ_STATUS_i_5 = 0;
   int __SEQ_STATUS_i_6 = 0;
   int __SEQ_STATUS_temp1_7 = 0;
   int __SEQ_STATUS_temp1_8 = 0;
   int __SEQ_STATUS_temp1_9 = 0;
   int __SEQ_STATUS_temp1_10 = 0;
   int __SEQ_STATUS_n_11 = 0;
   int __SEQ_STATUS_n_12 = 0;
   int __SEQ_STATUS_n_13 = 0;
   int __SEQ_STATUS_result_14 = 0;
   int __SEQ_STATUS_result_15 = 0;
   int __SEQ_STATUS_result_16 = 0;
   int __SEQ_STATUS_temp2_17 = 0;
   int __SEQ_STATUS_temp2_18 = 0;
   int __SEQ_STATUS_temp2_19 = 0;
   int __SEQ_STATUS_temp2_20 = 0;
   int __SEQ_STATUS_remainder_0_21 = 0;
   int __SEQ_STATUS_n_22 = 0;
   int __SEQ_STATUS_n_23 = 0;
   int __SEQ_STATUS_n_24 = 0;
   int __SEQ_STATUS_result_25 = 0;
   int __SEQ_STATUS_result_26 = 0;
   int __SEQ_STATUS_result_27 = 0;
   int __SEQ_STATUS_i_28 = 0;
   int __SEQ_STATUS_i_29 = 0;
   int n = 0;
   __SEQ_STATUS_n_11 = 1;
   __SEQ_STATUS_n_22 = 1;
   int result = 0;
   __SEQ_STATUS_result_14 = 1;
   __SEQ_STATUS_result_25 = 1;
   pc_label(__SEQ_STATUS_low_1 == 1,1,"ADC");
   pc_label(__SEQ_STATUS_high_2 == 1,2,"ADC");
   printf("Armstrong numbers between %d an %d are: ",low,high);
   i = low + 1;
   __SEQ_STATUS_i_3 = 1;
   __SEQ_STATUS_i_5 = 1;
   __SEQ_STATUS_i_28 = 1;
   while (1) {
     pc_label(__SEQ_STATUS_i_3 == 1,3,"ADC");
     pc_label(__SEQ_STATUS_i_4 == 1,4,"ADC");
     if (! (i < high)) break;
     pc_label(__SEQ_STATUS_i_5 == 1,5,"ADC");
     pc_label(__SEQ_STATUS_i_6 == 1,6,"ADC");
     __SEQ_STATUS_temp2_17 = 0;
     __SEQ_STATUS_temp2_18 = 0;
     __SEQ_STATUS_temp2_19 = 0;
     __SEQ_STATUS_temp2_20 = 0;
     temp2 = i;
     __SEQ_STATUS_temp2_17 = 1;
     __SEQ_STATUS_temp2_19 = 1;
     __SEQ_STATUS_temp1_7 = 0;
     __SEQ_STATUS_temp1_8 = 0;
     __SEQ_STATUS_temp1_9 = 0;
     __SEQ_STATUS_temp1_10 = 0;
     temp1 = i;
     __SEQ_STATUS_temp1_7 = 1;
     __SEQ_STATUS_temp1_9 = 1;
     loop: ;
     pc_label(__SEQ_STATUS_temp1_7 == 1,7,"ADC");
     pc_label(__SEQ_STATUS_temp1_8 == 1,8,"ADC");
     if (temp1 == 0) goto skip;
     pc_label(__SEQ_STATUS_temp1_9 == 1,9,"ADC");
     pc_label(__SEQ_STATUS_temp1_10 == 1,10,"ADC");
     __SEQ_STATUS_temp1_7 = 0;
     __SEQ_STATUS_temp1_8 = 0;
     __SEQ_STATUS_temp1_9 = 0;
     __SEQ_STATUS_temp1_10 = 0;
     temp1 /= 10;
     __SEQ_STATUS_temp1_8 = 1;
     __SEQ_STATUS_temp1_10 = 1;
     pc_label(__SEQ_STATUS_n_11 == 1,11,"ADC");
     pc_label(__SEQ_STATUS_n_12 == 1,12,"ADC");
     pc_label(__SEQ_STATUS_n_13 == 1,13,"ADC");
     __SEQ_STATUS_n_11 = 0;
     __SEQ_STATUS_n_12 = 0;
     __SEQ_STATUS_n_13 = 0;
     __SEQ_STATUS_n_22 = 0;
     __SEQ_STATUS_n_23 = 0;
     __SEQ_STATUS_n_24 = 0;
     n ++;
     __SEQ_STATUS_n_12 = 1;
     __SEQ_STATUS_n_23 = 1;
     goto loop;
     skip:
     while (1) {
       pc_label(__SEQ_STATUS_temp2_17 == 1,17,"ADC");
       pc_label(__SEQ_STATUS_temp2_18 == 1,18,"ADC");
       if (! (temp2 != 0)) break;
       {
         double tmp;
         pc_label(__SEQ_STATUS_temp2_19 == 1,19,"ADC");
         pc_label(__SEQ_STATUS_temp2_20 == 1,20,"ADC");
         __SEQ_STATUS_remainder_0_21 = 0;
         remainder_0 = temp2 % 10;
         __SEQ_STATUS_remainder_0_21 = 1;
         pc_label(__SEQ_STATUS_remainder_0_21 == 1,21,"ADC");
         pc_label(__SEQ_STATUS_n_22 == 1,22,"ADC");
         pc_label(__SEQ_STATUS_n_23 == 1,23,"ADC");
         pc_label(__SEQ_STATUS_n_24 == 1,24,"ADC");
         tmp = pow((double)remainder_0,(double)n);
         pc_label(__SEQ_STATUS_result_25 == 1,25,"ADC");
         pc_label(__SEQ_STATUS_result_26 == 1,26,"ADC");
         pc_label(__SEQ_STATUS_result_27 == 1,27,"ADC");
         __SEQ_STATUS_result_14 = 0;
         __SEQ_STATUS_result_15 = 0;
         __SEQ_STATUS_result_16 = 0;
         __SEQ_STATUS_result_25 = 0;
         __SEQ_STATUS_result_26 = 0;
         __SEQ_STATUS_result_27 = 0;
         result = (int)((double)result + tmp);
         __SEQ_STATUS_result_15 = 1;
         __SEQ_STATUS_result_26 = 1;
         __SEQ_STATUS_temp2_17 = 0;
         __SEQ_STATUS_temp2_18 = 0;
         __SEQ_STATUS_temp2_19 = 0;
         __SEQ_STATUS_temp2_20 = 0;
         temp2 /= 10;
         __SEQ_STATUS_temp2_18 = 1;
         __SEQ_STATUS_temp2_20 = 1;
       }
     }
     pc_label(__SEQ_STATUS_result_14 == 1,14,"ADC");
     pc_label(__SEQ_STATUS_result_15 == 1,15,"ADC");
     pc_label(__SEQ_STATUS_result_16 == 1,16,"ADC");
     if (result == i) {
       pc_label(__SEQ_STATUS_i_28 == 1,28,"ADC");
       pc_label(__SEQ_STATUS_i_29 == 1,29,"ADC");
       printf("%d ",i);
     }
     __SEQ_STATUS_n_11 = 0;
     __SEQ_STATUS_n_12 = 0;
     __SEQ_STATUS_n_13 = 0;
     __SEQ_STATUS_n_22 = 0;
     __SEQ_STATUS_n_23 = 0;
     __SEQ_STATUS_n_24 = 0;
     n = 0;
     __SEQ_STATUS_n_13 = 1;
     __SEQ_STATUS_n_24 = 1;
     __SEQ_STATUS_result_14 = 0;
     __SEQ_STATUS_result_15 = 0;
     __SEQ_STATUS_result_16 = 0;
     __SEQ_STATUS_result_25 = 0;
     __SEQ_STATUS_result_26 = 0;
     __SEQ_STATUS_result_27 = 0;
     result = 0;
     __SEQ_STATUS_result_16 = 1;
     __SEQ_STATUS_result_27 = 1;
     __SEQ_STATUS_i_3 = 0;
     __SEQ_STATUS_i_4 = 0;
     __SEQ_STATUS_i_5 = 0;
     __SEQ_STATUS_i_6 = 0;
     __SEQ_STATUS_i_28 = 0;
     __SEQ_STATUS_i_29 = 0;
     i ++;
     __SEQ_STATUS_i_4 = 1;
     __SEQ_STATUS_i_6 = 1;
     __SEQ_STATUS_i_29 = 1;
   }
   __retres = 0;
   return __retres;
 }


