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
   int __SEQ_STATUS_1 = 1;
   int __SEQ_STATUS_2 = 1;
   int __SEQ_STATUS_3 = 0;
   int __SEQ_STATUS_4 = 0;
   int __SEQ_STATUS_5 = 0;
   int __SEQ_STATUS_6 = 0;
   int __SEQ_STATUS_7 = 0;
   int __SEQ_STATUS_8 = 0;
   int __SEQ_STATUS_9 = 0;
   int __SEQ_STATUS_10 = 0;
   int __SEQ_STATUS_11 = 0;
   int __SEQ_STATUS_12 = 0;
   int __SEQ_STATUS_13 = 0;
   int __SEQ_STATUS_14 = 0;
   int __SEQ_STATUS_15 = 0;
   int __SEQ_STATUS_16 = 0;
   int __SEQ_STATUS_17 = 0;
   int __SEQ_STATUS_18 = 0;
   int __SEQ_STATUS_19 = 0;
   int __SEQ_STATUS_20 = 0;
   int __SEQ_STATUS_21 = 0;
   int __SEQ_STATUS_22 = 0;
   int __SEQ_STATUS_23 = 0;
   int __SEQ_STATUS_24 = 0;
   int __SEQ_STATUS_25 = 0;
   int __SEQ_STATUS_26 = 0;
   int __SEQ_STATUS_27 = 0;
   int __SEQ_STATUS_28 = 0;
   int __SEQ_STATUS_29 = 0;
   int n = 0;
   __SEQ_STATUS_11 = 1;
   __SEQ_STATUS_19 = 1;
   int result = 0;
   __SEQ_STATUS_22 = 1;
   __SEQ_STATUS_25 = 1;
   pc_label(__SEQ_STATUS_1 == 1,1,"ADC");
   pc_label(__SEQ_STATUS_2 == 1,2,"ADC");
   printf("Armstrong numbers between %d an %d are: ",low,high);
   __SEQ_STATUS_3 = 0;
   __SEQ_STATUS_4 = 0;
   __SEQ_STATUS_5 = 0;
   __SEQ_STATUS_6 = 0;
   __SEQ_STATUS_28 = 0;
   __SEQ_STATUS_29 = 0;
   i = low + 1;
   __SEQ_STATUS_3 = 1;
   __SEQ_STATUS_5 = 1;
   __SEQ_STATUS_28 = 1;
   while (1) {
     pc_label(__SEQ_STATUS_3 == 1,3,"ADC");
     pc_label(__SEQ_STATUS_4 == 1,4,"ADC");
     if (! (i < high)) break;
     pc_label(__SEQ_STATUS_5 == 1,5,"ADC");
     pc_label(__SEQ_STATUS_6 == 1,6,"ADC");
     __SEQ_STATUS_14 = 0;
     __SEQ_STATUS_15 = 0;
     __SEQ_STATUS_16 = 0;
     __SEQ_STATUS_17 = 0;
     temp2 = i;
     __SEQ_STATUS_14 = 1;
     __SEQ_STATUS_16 = 1;
     __SEQ_STATUS_7 = 0;
     __SEQ_STATUS_8 = 0;
     __SEQ_STATUS_9 = 0;
     __SEQ_STATUS_10 = 0;
     temp1 = i;
     __SEQ_STATUS_7 = 1;
     __SEQ_STATUS_9 = 1;
     loop: ;
     pc_label(__SEQ_STATUS_7 == 1,7,"ADC");
     pc_label(__SEQ_STATUS_8 == 1,8,"ADC");
     if (temp1 == 0) goto skip;
     pc_label(__SEQ_STATUS_9 == 1,9,"ADC");
     pc_label(__SEQ_STATUS_10 == 1,10,"ADC");
     __SEQ_STATUS_7 = 0;
     __SEQ_STATUS_8 = 0;
     __SEQ_STATUS_9 = 0;
     __SEQ_STATUS_10 = 0;
     temp1 /= 10;
     __SEQ_STATUS_8 = 1;
     __SEQ_STATUS_10 = 1;
     pc_label(__SEQ_STATUS_11 == 1,11,"ADC");
     pc_label(__SEQ_STATUS_12 == 1,12,"ADC");
     pc_label(__SEQ_STATUS_13 == 1,13,"ADC");
     __SEQ_STATUS_11 = 0;
     __SEQ_STATUS_12 = 0;
     __SEQ_STATUS_13 = 0;
     __SEQ_STATUS_19 = 0;
     __SEQ_STATUS_20 = 0;
     __SEQ_STATUS_21 = 0;
     n ++;
     __SEQ_STATUS_12 = 1;
     __SEQ_STATUS_20 = 1;
     goto loop;
     skip:
     while (1) {
       pc_label(__SEQ_STATUS_14 == 1,14,"ADC");
       pc_label(__SEQ_STATUS_15 == 1,15,"ADC");
       if (! (temp2 != 0)) break;
       {
         double tmp;
         pc_label(__SEQ_STATUS_16 == 1,16,"ADC");
         pc_label(__SEQ_STATUS_17 == 1,17,"ADC");
         __SEQ_STATUS_18 = 0;
         remainder_0 = temp2 % 10;
         __SEQ_STATUS_18 = 1;
         pc_label(__SEQ_STATUS_18 == 1,18,"ADC");
         pc_label(__SEQ_STATUS_19 == 1,19,"ADC");
         pc_label(__SEQ_STATUS_20 == 1,20,"ADC");
         pc_label(__SEQ_STATUS_21 == 1,21,"ADC");
         tmp = pow((double)remainder_0,(double)n);
         pc_label(__SEQ_STATUS_22 == 1,22,"ADC");
         pc_label(__SEQ_STATUS_23 == 1,23,"ADC");
         pc_label(__SEQ_STATUS_24 == 1,24,"ADC");
         __SEQ_STATUS_22 = 0;
         __SEQ_STATUS_23 = 0;
         __SEQ_STATUS_24 = 0;
         __SEQ_STATUS_25 = 0;
         __SEQ_STATUS_26 = 0;
         __SEQ_STATUS_27 = 0;
         result = (int)((double)result + tmp);
         __SEQ_STATUS_23 = 1;
         __SEQ_STATUS_26 = 1;
         __SEQ_STATUS_14 = 0;
         __SEQ_STATUS_15 = 0;
         __SEQ_STATUS_16 = 0;
         __SEQ_STATUS_17 = 0;
         temp2 /= 10;
         __SEQ_STATUS_15 = 1;
         __SEQ_STATUS_17 = 1;
       }
     }
     pc_label(__SEQ_STATUS_25 == 1,25,"ADC");
     pc_label(__SEQ_STATUS_26 == 1,26,"ADC");
     pc_label(__SEQ_STATUS_27 == 1,27,"ADC");
     if (result == i) {
       pc_label(__SEQ_STATUS_28 == 1,28,"ADC");
       pc_label(__SEQ_STATUS_29 == 1,29,"ADC");
       printf("%d ",i);
     }
     __SEQ_STATUS_11 = 0;
     __SEQ_STATUS_12 = 0;
     __SEQ_STATUS_13 = 0;
     __SEQ_STATUS_19 = 0;
     __SEQ_STATUS_20 = 0;
     __SEQ_STATUS_21 = 0;
     n = 0;
     __SEQ_STATUS_13 = 1;
     __SEQ_STATUS_21 = 1;
     __SEQ_STATUS_22 = 0;
     __SEQ_STATUS_23 = 0;
     __SEQ_STATUS_24 = 0;
     __SEQ_STATUS_25 = 0;
     __SEQ_STATUS_26 = 0;
     __SEQ_STATUS_27 = 0;
     result = 0;
     __SEQ_STATUS_24 = 1;
     __SEQ_STATUS_27 = 1;
     __SEQ_STATUS_3 = 0;
     __SEQ_STATUS_4 = 0;
     __SEQ_STATUS_5 = 0;
     __SEQ_STATUS_6 = 0;
     __SEQ_STATUS_28 = 0;
     __SEQ_STATUS_29 = 0;
     i ++;
     __SEQ_STATUS_4 = 1;
     __SEQ_STATUS_6 = 1;
     __SEQ_STATUS_29 = 1;
   }
   __retres = 0;
   return __retres;
 }


