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

int armstrong(int low, int high)
 {
   int __retres;
   int i;
   int temp1;
   int temp2;
   int remainder_0;
   int __SEQ_STATUS_low_1 = low == (-2147483647-1);
   int __SEQ_STATUS_low_2 = low == 2147483647;
   int __SEQ_STATUS_high_3 = high == (-2147483647-1);
   int __SEQ_STATUS_high_4 = high == 2147483647;
   int __SEQ_STATUS_i_5 = 0;
   int __SEQ_STATUS_i_6 = 0;
   int __SEQ_STATUS_i_7 = 0;
   int __SEQ_STATUS_i_8 = 0;
   int __SEQ_STATUS_i_9 = 0;
   int __SEQ_STATUS_i_10 = 0;
   int __SEQ_STATUS_i_11 = 0;
   int __SEQ_STATUS_i_12 = 0;
   int __SEQ_STATUS_temp1_13 = 0;
   int __SEQ_STATUS_temp1_14 = 0;
   int __SEQ_STATUS_temp1_15 = 0;
   int __SEQ_STATUS_temp1_16 = 0;
   int __SEQ_STATUS_temp1_17 = 0;
   int __SEQ_STATUS_temp1_18 = 0;
   int __SEQ_STATUS_temp1_19 = 0;
   int __SEQ_STATUS_temp1_20 = 0;
   int __SEQ_STATUS_n_21 = 0;
   int __SEQ_STATUS_n_22 = 0;
   int __SEQ_STATUS_temp2_23 = 0;
   int __SEQ_STATUS_temp2_24 = 0;
   int __SEQ_STATUS_temp2_25 = 0;
   int __SEQ_STATUS_temp2_26 = 0;
   int __SEQ_STATUS_temp2_27 = 0;
   int __SEQ_STATUS_temp2_28 = 0;
   int __SEQ_STATUS_temp2_29 = 0;
   int __SEQ_STATUS_temp2_30 = 0;
   int __SEQ_STATUS_remainder_0_31 = 0;
   int __SEQ_STATUS_remainder_0_32 = 0;
   int __SEQ_STATUS_n_33 = 0;
   int __SEQ_STATUS_n_34 = 0;
   int __SEQ_STATUS_result_35 = 0;
   int __SEQ_STATUS_result_36 = 0;
   int __SEQ_STATUS_result_37 = 0;
   int __SEQ_STATUS_result_38 = 0;
   int __SEQ_STATUS_i_39 = 0;
   int __SEQ_STATUS_i_40 = 0;
   int __SEQ_STATUS_i_41 = 0;
   int __SEQ_STATUS_i_42 = 0;
   int n = 0;
   int result = 0;
   pc_label(__SEQ_STATUS_low_1 == 1,1,"BADC");
   pc_label(__SEQ_STATUS_low_2 == 1,2,"BADC");
   pc_label(__SEQ_STATUS_high_3 == 1,3,"BADC");
   pc_label(__SEQ_STATUS_high_4 == 1,4,"BADC");
   printf("Armstrong numbers between %d an %d are: ",low,high);
   i = low + 1;
   __SEQ_STATUS_i_5 = i == (-2147483647-1);
   __SEQ_STATUS_i_6 = i == 2147483647;
   __SEQ_STATUS_i_9 = i == (-2147483647-1);
   __SEQ_STATUS_i_10 = i == 2147483647;
   __SEQ_STATUS_i_39 = i == (-2147483647-1);
   __SEQ_STATUS_i_40 = i == 2147483647;
   while (1) {
     pc_label(__SEQ_STATUS_i_5 == 1,5,"BADC");
     pc_label(__SEQ_STATUS_i_6 == 1,6,"BADC");
     pc_label(__SEQ_STATUS_i_7 == 1,7,"BADC");
     pc_label(__SEQ_STATUS_i_8 == 1,8,"BADC");
     if (! (i < high)) break;
     pc_label(__SEQ_STATUS_i_9 == 1,9,"BADC");
     pc_label(__SEQ_STATUS_i_10 == 1,10,"BADC");
     pc_label(__SEQ_STATUS_i_11 == 1,11,"BADC");
     pc_label(__SEQ_STATUS_i_12 == 1,12,"BADC");
     __SEQ_STATUS_temp2_23 = 0;
     __SEQ_STATUS_temp2_24 = 0;
     __SEQ_STATUS_temp2_25 = 0;
     __SEQ_STATUS_temp2_26 = 0;
     __SEQ_STATUS_temp2_27 = 0;
     __SEQ_STATUS_temp2_28 = 0;
     __SEQ_STATUS_temp2_29 = 0;
     __SEQ_STATUS_temp2_30 = 0;
     temp2 = i;
     __SEQ_STATUS_temp2_23 = temp2 == (-2147483647-1);
     __SEQ_STATUS_temp2_24 = temp2 == 2147483647;
     __SEQ_STATUS_temp2_27 = temp2 == (-2147483647-1);
     __SEQ_STATUS_temp2_28 = temp2 == 2147483647;
     __SEQ_STATUS_temp1_13 = 0;
     __SEQ_STATUS_temp1_14 = 0;
     __SEQ_STATUS_temp1_15 = 0;
     __SEQ_STATUS_temp1_16 = 0;
     __SEQ_STATUS_temp1_17 = 0;
     __SEQ_STATUS_temp1_18 = 0;
     __SEQ_STATUS_temp1_19 = 0;
     __SEQ_STATUS_temp1_20 = 0;
     temp1 = i;
     __SEQ_STATUS_temp1_13 = temp1 == (-2147483647-1);
     __SEQ_STATUS_temp1_14 = temp1 == 2147483647;
     __SEQ_STATUS_temp1_17 = temp1 == (-2147483647-1);
     __SEQ_STATUS_temp1_18 = temp1 == 2147483647;
     loop: ;
     pc_label(__SEQ_STATUS_temp1_13 == 1,13,"BADC");
     pc_label(__SEQ_STATUS_temp1_14 == 1,14,"BADC");
     pc_label(__SEQ_STATUS_temp1_15 == 1,15,"BADC");
     pc_label(__SEQ_STATUS_temp1_16 == 1,16,"BADC");
     if (temp1 == 0) goto skip;
     pc_label(__SEQ_STATUS_temp1_17 == 1,17,"BADC");
     pc_label(__SEQ_STATUS_temp1_18 == 1,18,"BADC");
     pc_label(__SEQ_STATUS_temp1_19 == 1,19,"BADC");
     pc_label(__SEQ_STATUS_temp1_20 == 1,20,"BADC");
     __SEQ_STATUS_temp1_13 = 0;
     __SEQ_STATUS_temp1_14 = 0;
     __SEQ_STATUS_temp1_15 = 0;
     __SEQ_STATUS_temp1_16 = 0;
     __SEQ_STATUS_temp1_17 = 0;
     __SEQ_STATUS_temp1_18 = 0;
     __SEQ_STATUS_temp1_19 = 0;
     __SEQ_STATUS_temp1_20 = 0;
     temp1 /= 10;
     __SEQ_STATUS_temp1_15 = temp1 == (-2147483647-1);
     __SEQ_STATUS_temp1_16 = temp1 == 2147483647;
     __SEQ_STATUS_temp1_19 = temp1 == (-2147483647-1);
     __SEQ_STATUS_temp1_20 = temp1 == 2147483647;
     pc_label(__SEQ_STATUS_n_21 == 1,21,"BADC");
     pc_label(__SEQ_STATUS_n_22 == 1,22,"BADC");
     __SEQ_STATUS_n_21 = 0;
     __SEQ_STATUS_n_22 = 0;
     __SEQ_STATUS_n_33 = 0;
     __SEQ_STATUS_n_34 = 0;
     n ++;
     __SEQ_STATUS_n_21 = n == (-2147483647-1);
     __SEQ_STATUS_n_22 = n == 2147483647;
     __SEQ_STATUS_n_33 = n == (-2147483647-1);
     __SEQ_STATUS_n_34 = n == 2147483647;
     goto loop;
     skip:
     while (1) {
       pc_label(__SEQ_STATUS_temp2_23 == 1,23,"BADC");
       pc_label(__SEQ_STATUS_temp2_24 == 1,24,"BADC");
       pc_label(__SEQ_STATUS_temp2_25 == 1,25,"BADC");
       pc_label(__SEQ_STATUS_temp2_26 == 1,26,"BADC");
       if (! (temp2 != 0)) break;
       {
         double tmp;
         pc_label(__SEQ_STATUS_temp2_27 == 1,27,"BADC");
         pc_label(__SEQ_STATUS_temp2_28 == 1,28,"BADC");
         pc_label(__SEQ_STATUS_temp2_29 == 1,29,"BADC");
         pc_label(__SEQ_STATUS_temp2_30 == 1,30,"BADC");
         __SEQ_STATUS_remainder_0_31 = 0;
         __SEQ_STATUS_remainder_0_32 = 0;
         remainder_0 = temp2 % 10;
         __SEQ_STATUS_remainder_0_31 = remainder_0 == (-2147483647-1);
         __SEQ_STATUS_remainder_0_32 = remainder_0 == 2147483647;
         pc_label(__SEQ_STATUS_remainder_0_31 == 1,31,"BADC");
         pc_label(__SEQ_STATUS_remainder_0_32 == 1,32,"BADC");
         pc_label(__SEQ_STATUS_n_33 == 1,33,"BADC");
         pc_label(__SEQ_STATUS_n_34 == 1,34,"BADC");
         tmp = pow((double)remainder_0,(double)n);
         pc_label(__SEQ_STATUS_result_35 == 1,35,"BADC");
         pc_label(__SEQ_STATUS_result_36 == 1,36,"BADC");
         __SEQ_STATUS_result_35 = 0;
         __SEQ_STATUS_result_36 = 0;
         __SEQ_STATUS_result_37 = 0;
         __SEQ_STATUS_result_38 = 0;
         result = (int)((double)result + tmp);
         __SEQ_STATUS_result_35 = result == (-2147483647-1);
         __SEQ_STATUS_result_36 = result == 2147483647;
         __SEQ_STATUS_result_37 = result == (-2147483647-1);
         __SEQ_STATUS_result_38 = result == 2147483647;
         __SEQ_STATUS_temp2_23 = 0;
         __SEQ_STATUS_temp2_24 = 0;
         __SEQ_STATUS_temp2_25 = 0;
         __SEQ_STATUS_temp2_26 = 0;
         __SEQ_STATUS_temp2_27 = 0;
         __SEQ_STATUS_temp2_28 = 0;
         __SEQ_STATUS_temp2_29 = 0;
         __SEQ_STATUS_temp2_30 = 0;
         temp2 /= 10;
         __SEQ_STATUS_temp2_25 = temp2 == (-2147483647-1);
         __SEQ_STATUS_temp2_26 = temp2 == 2147483647;
         __SEQ_STATUS_temp2_29 = temp2 == (-2147483647-1);
         __SEQ_STATUS_temp2_30 = temp2 == 2147483647;
       }
     }
     pc_label(__SEQ_STATUS_result_37 == 1,37,"BADC");
     pc_label(__SEQ_STATUS_result_38 == 1,38,"BADC");
     if (result == i) {
       pc_label(__SEQ_STATUS_i_39 == 1,39,"BADC");
       pc_label(__SEQ_STATUS_i_40 == 1,40,"BADC");
       pc_label(__SEQ_STATUS_i_41 == 1,41,"BADC");
       pc_label(__SEQ_STATUS_i_42 == 1,42,"BADC");
       printf("%d ",i);
     }
     __SEQ_STATUS_n_21 = 0;
     __SEQ_STATUS_n_22 = 0;
     __SEQ_STATUS_n_33 = 0;
     __SEQ_STATUS_n_34 = 0;
     n = 0;
     __SEQ_STATUS_result_35 = 0;
     __SEQ_STATUS_result_36 = 0;
     __SEQ_STATUS_result_37 = 0;
     __SEQ_STATUS_result_38 = 0;
     result = 0;
     __SEQ_STATUS_i_5 = 0;
     __SEQ_STATUS_i_6 = 0;
     __SEQ_STATUS_i_7 = 0;
     __SEQ_STATUS_i_8 = 0;
     __SEQ_STATUS_i_9 = 0;
     __SEQ_STATUS_i_10 = 0;
     __SEQ_STATUS_i_11 = 0;
     __SEQ_STATUS_i_12 = 0;
     __SEQ_STATUS_i_39 = 0;
     __SEQ_STATUS_i_40 = 0;
     __SEQ_STATUS_i_41 = 0;
     __SEQ_STATUS_i_42 = 0;
     i ++;
     __SEQ_STATUS_i_7 = i == (-2147483647-1);
     __SEQ_STATUS_i_8 = i == 2147483647;
     __SEQ_STATUS_i_11 = i == (-2147483647-1);
     __SEQ_STATUS_i_12 = i == 2147483647;
     __SEQ_STATUS_i_41 = i == (-2147483647-1);
     __SEQ_STATUS_i_42 = i == 2147483647;
   }
   __retres = 0;
   return __retres;
 }


