/* Generated by Frama-C LTest */

#include "errno.h"
#include "stdarg.h"
#include "stddef.h"
#include "stdio.h"

#ifndef pc_label
#define pc_label(...) do{}while(0)
#endif
#ifndef pc_label_bindings
#define pc_label_bindings(...) do{}while(0)
#endif

int main(char *line)
 {
   int __retres;
   int i;
   int j;
   int __SEQ_STATUS_i_1 = 0;
   int __SEQ_STATUS_i_2 = 0;
   int __SEQ_STATUS_i_3 = 0;
   int __SEQ_STATUS_i_4 = 0;
   int __SEQ_STATUS_i_5 = 0;
   int __SEQ_STATUS_i_6 = 0;
   int __SEQ_STATUS_j_7 = 0;
   int __SEQ_STATUS_j_8 = 0;
   int __SEQ_STATUS_j_9 = 0;
   int __SEQ_STATUS_j_10 = 0;
   int __SEQ_STATUS_j_11 = 0;
   int __SEQ_STATUS_j_12 = 0;
   int __SEQ_STATUS_j_13 = 0;
   int __SEQ_STATUS_j_14 = 0;
   i = 0;
   while (1) {
     pc_label(__SEQ_STATUS_i_1 == 1,1,"BDUC");
     pc_label(__SEQ_STATUS_i_2 == 1,2,"BDUC");
     if (! ((int)*(line + i) != '\000')) break;
     while (1) {
       pc_label(__SEQ_STATUS_i_3 == 1,3,"BDUC");
       pc_label(__SEQ_STATUS_i_4 == 1,4,"BDUC");
       if (! (! (((int)*(line + i) >= 'a' && (int)*(line + i) <= 'z' || 
                  (int)*(line + i) >= 'A' && (int)*(line + i) <= 'Z') || 
                 (int)*(line + i) == '\000'))) break;
       pc_label(__SEQ_STATUS_i_5 == 1,5,"BDUC");
       pc_label(__SEQ_STATUS_i_6 == 1,6,"BDUC");
       __SEQ_STATUS_j_7 = 0;
       __SEQ_STATUS_j_8 = 0;
       __SEQ_STATUS_j_9 = 0;
       __SEQ_STATUS_j_10 = 0;
       __SEQ_STATUS_j_11 = 0;
       __SEQ_STATUS_j_12 = 0;
       __SEQ_STATUS_j_13 = 0;
       __SEQ_STATUS_j_14 = 0;
       j = i;
       __SEQ_STATUS_j_7 = j == (-2147483647-1);
       __SEQ_STATUS_j_8 = j == 2147483647;
       __SEQ_STATUS_j_11 = j == (-2147483647-1);
       __SEQ_STATUS_j_12 = j == 2147483647;
       while (1) {
         pc_label(__SEQ_STATUS_j_7 == 1,7,"BDUC");
         pc_label(__SEQ_STATUS_j_8 == 1,8,"BDUC");
         pc_label(__SEQ_STATUS_j_9 == 1,9,"BDUC");
         pc_label(__SEQ_STATUS_j_10 == 1,10,"BDUC");
         if (! ((int)*(line + j) != '\000')) break;
         pc_label(__SEQ_STATUS_j_11 == 1,11,"BDUC");
         pc_label(__SEQ_STATUS_j_12 == 1,12,"BDUC");
         pc_label(__SEQ_STATUS_j_13 == 1,13,"BDUC");
         pc_label(__SEQ_STATUS_j_14 == 1,14,"BDUC");
         *(line + j) = *(line + (j + 1));
         __SEQ_STATUS_j_7 = 0;
         __SEQ_STATUS_j_8 = 0;
         __SEQ_STATUS_j_9 = 0;
         __SEQ_STATUS_j_10 = 0;
         __SEQ_STATUS_j_11 = 0;
         __SEQ_STATUS_j_12 = 0;
         __SEQ_STATUS_j_13 = 0;
         __SEQ_STATUS_j_14 = 0;
         j ++;
         __SEQ_STATUS_j_9 = j == (-2147483647-1);
         __SEQ_STATUS_j_10 = j == 2147483647;
         __SEQ_STATUS_j_13 = j == (-2147483647-1);
         __SEQ_STATUS_j_14 = j == 2147483647;
       }
       *(line + j) = (char)'\000';
     }
     __SEQ_STATUS_i_1 = 0;
     __SEQ_STATUS_i_2 = 0;
     __SEQ_STATUS_i_3 = 0;
     __SEQ_STATUS_i_4 = 0;
     __SEQ_STATUS_i_5 = 0;
     __SEQ_STATUS_i_6 = 0;
     i ++;
     __SEQ_STATUS_i_1 = i == (-2147483647-1);
     __SEQ_STATUS_i_2 = i == 2147483647;
     __SEQ_STATUS_i_3 = i == (-2147483647-1);
     __SEQ_STATUS_i_4 = i == 2147483647;
     __SEQ_STATUS_i_5 = i == (-2147483647-1);
     __SEQ_STATUS_i_6 = i == 2147483647;
   }
   printf("Output String: ");
   puts((char const *)line);
   __retres = 0;
   return __retres;
 }


