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

int checkPrimeNumber(int n);

int main(int n1, int n2)
{
  int __retres;
  int i;
  int flag;
  int __SEQ_STATUS_n1_1 = 1;
  int __SEQ_TMP_n1_1 = n1;
  int __SEQ_STATUS_n1_2 = 1;
  int __SEQ_STATUS_n2_3 = 1;
  int __SEQ_TMP_n2_3 = n2;
  int __SEQ_STATUS_n2_4 = 1;
  int __SEQ_STATUS_i_5 = 0;
  int __SEQ_TMP_i_5 = 0;
  int __SEQ_STATUS_i_6 = 0;
  int __SEQ_STATUS_i_7 = 0;
  int __SEQ_TMP_i_7 = 0;
  int __SEQ_STATUS_i_8 = 0;
  int __SEQ_STATUS_i_9 = 0;
  int __SEQ_STATUS_i_10 = 0;
  int __SEQ_STATUS_i_11 = 0;
  int __SEQ_STATUS_i_12 = 0;
  int __SEQ_STATUS_flag_13 = 0;
  int __SEQ_TMP_flag_13 = 0;
  int __SEQ_STATUS_flag_14 = 0;
  int __SEQ_STATUS_i_15 = 0;
  int __SEQ_STATUS_i_16 = 0;
  int __SEQ_STATUS_i_17 = 0;
  int __SEQ_STATUS_i_18 = 0;
  pc_label(__SEQ_STATUS_n1_1 == 1 && __SEQ_TMP_n1_1 == (-2147483647-1),1,
           "BAUC");
  pc_label(__SEQ_STATUS_n1_2 == 1 && __SEQ_TMP_n1_1 == 2147483647,2,"BAUC");
  pc_label(__SEQ_STATUS_n2_3 == 1 && __SEQ_TMP_n2_3 == (-2147483647-1),3,
           "BAUC");
  pc_label(__SEQ_STATUS_n2_4 == 1 && __SEQ_TMP_n2_3 == 2147483647,4,"BAUC");
  printf("Prime numbers between %d and %d are: ",n1,n2);
  i = n1 + 1;
  __SEQ_STATUS_i_5 = 1;
  __SEQ_TMP_i_5 = i;
  __SEQ_STATUS_i_6 = 1;
  __SEQ_STATUS_i_9 = 1;
  __SEQ_STATUS_i_10 = 1;
  __SEQ_STATUS_i_15 = 1;
  __SEQ_STATUS_i_16 = 1;
  while (1) {
    pc_label(__SEQ_STATUS_i_5 == 1 && __SEQ_TMP_i_5 == (-2147483647-1),5,
             "BAUC");
    pc_label(__SEQ_STATUS_i_6 == 1 && __SEQ_TMP_i_5 == 2147483647,6,"BAUC");
    pc_label(__SEQ_STATUS_i_7 == 1 && __SEQ_TMP_i_7 == (-2147483647-1),7,
             "BAUC");
    pc_label(__SEQ_STATUS_i_8 == 1 && __SEQ_TMP_i_7 == 2147483647,8,"BAUC");
    if (! (i < n2)) break;
    pc_label(__SEQ_STATUS_i_9 == 1 && __SEQ_TMP_i_5 == (-2147483647-1),9,
             "BAUC");
    pc_label(__SEQ_STATUS_i_10 == 1 && __SEQ_TMP_i_5 == 2147483647,10,"BAUC");
    pc_label(__SEQ_STATUS_i_11 == 1 && __SEQ_TMP_i_7 == (-2147483647-1),11,
             "BAUC");
    pc_label(__SEQ_STATUS_i_12 == 1 && __SEQ_TMP_i_7 == 2147483647,12,"BAUC");
    __SEQ_STATUS_flag_13 = 0;
    __SEQ_STATUS_flag_14 = 0;
    flag = checkPrimeNumber(i);
    __SEQ_STATUS_flag_13 = 1;
    __SEQ_TMP_flag_13 = flag;
    __SEQ_STATUS_flag_14 = 1;
    pc_label(__SEQ_STATUS_flag_13 == 1 && __SEQ_TMP_flag_13 == (-2147483647-1),
             13,"BAUC");
    pc_label(__SEQ_STATUS_flag_14 == 1 && __SEQ_TMP_flag_13 == 2147483647,14,
             "BAUC");
    if (flag == 1) {
      pc_label(__SEQ_STATUS_i_15 == 1 && __SEQ_TMP_i_5 == (-2147483647-1),15,
               "BAUC");
      pc_label(__SEQ_STATUS_i_16 == 1 && __SEQ_TMP_i_5 == 2147483647,16,
               "BAUC");
      pc_label(__SEQ_STATUS_i_17 == 1 && __SEQ_TMP_i_7 == (-2147483647-1),17,
               "BAUC");
      pc_label(__SEQ_STATUS_i_18 == 1 && __SEQ_TMP_i_7 == 2147483647,18,
               "BAUC");
      printf("%d ",i);
    }
    __SEQ_STATUS_i_5 = 0;
    __SEQ_STATUS_i_6 = 0;
    __SEQ_STATUS_i_7 = 0;
    __SEQ_STATUS_i_8 = 0;
    __SEQ_STATUS_i_9 = 0;
    __SEQ_STATUS_i_10 = 0;
    __SEQ_STATUS_i_11 = 0;
    __SEQ_STATUS_i_12 = 0;
    __SEQ_STATUS_i_15 = 0;
    __SEQ_STATUS_i_16 = 0;
    __SEQ_STATUS_i_17 = 0;
    __SEQ_STATUS_i_18 = 0;
    i ++;
    __SEQ_STATUS_i_7 = 1;
    __SEQ_TMP_i_7 = i;
    __SEQ_STATUS_i_8 = 1;
    __SEQ_STATUS_i_11 = 1;
    __SEQ_STATUS_i_12 = 1;
    __SEQ_STATUS_i_17 = 1;
    __SEQ_STATUS_i_18 = 1;
  }
  __retres = 0;
  return __retres;
}

int checkPrimeNumber(int n)
{
  int j;
  int __SEQ_STATUS_j_19 = 0;
  int __SEQ_TMP_j_19 = 0;
  int __SEQ_STATUS_j_20 = 0;
  int __SEQ_STATUS_n_21 = 1;
  int __SEQ_TMP_n_21 = n;
  int __SEQ_STATUS_n_22 = 1;
  int __SEQ_STATUS_n_23 = 1;
  int __SEQ_STATUS_n_24 = 1;
  int __SEQ_STATUS_j_25 = 0;
  int __SEQ_STATUS_j_26 = 0;
  int __SEQ_STATUS_j_27 = 0;
  int __SEQ_STATUS_j_28 = 0;
  int flag = 1;
  j = 2;
  while (1) {
    pc_label(__SEQ_STATUS_j_19 == 1 && __SEQ_TMP_j_19 == (-2147483647-1),19,
             "BAUC");
    pc_label(__SEQ_STATUS_j_20 == 1 && __SEQ_TMP_j_19 == 2147483647,20,
             "BAUC");
    pc_label(__SEQ_STATUS_n_21 == 1 && __SEQ_TMP_n_21 == (-2147483647-1),21,
             "BAUC");
    pc_label(__SEQ_STATUS_n_22 == 1 && __SEQ_TMP_n_21 == 2147483647,22,
             "BAUC");
    if (! (j <= n / 2)) break;
    pc_label(__SEQ_STATUS_n_23 == 1 && __SEQ_TMP_n_21 == (-2147483647-1),23,
             "BAUC");
    pc_label(__SEQ_STATUS_n_24 == 1 && __SEQ_TMP_n_21 == 2147483647,24,
             "BAUC");
    pc_label(__SEQ_STATUS_j_25 == 1 && __SEQ_TMP_j_19 == (-2147483647-1),25,
             "BAUC");
    pc_label(__SEQ_STATUS_j_26 == 1 && __SEQ_TMP_j_19 == 2147483647,26,
             "BAUC");
    if (n % j == 0) {
      flag = 0;
      break;
    }
    pc_label(__SEQ_STATUS_j_27 == 1 && __SEQ_TMP_j_19 == (-2147483647-1),27,
             "BAUC");
    pc_label(__SEQ_STATUS_j_28 == 1 && __SEQ_TMP_j_19 == 2147483647,28,
             "BAUC");
    __SEQ_STATUS_j_19 = 0;
    __SEQ_STATUS_j_20 = 0;
    __SEQ_STATUS_j_25 = 0;
    __SEQ_STATUS_j_26 = 0;
    __SEQ_STATUS_j_27 = 0;
    __SEQ_STATUS_j_28 = 0;
    j ++;
    __SEQ_STATUS_j_19 = 1;
    __SEQ_TMP_j_19 = j;
    __SEQ_STATUS_j_20 = 1;
    __SEQ_STATUS_j_25 = 1;
    __SEQ_STATUS_j_26 = 1;
    __SEQ_STATUS_j_27 = 1;
    __SEQ_STATUS_j_28 = 1;
  }
  return flag;
}


