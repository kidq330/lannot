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
  int __SEQ_STATUS_n2_2 = 1;
  int __SEQ_STATUS_i_3 = 0;
  int __SEQ_STATUS_i_4 = 0;
  int __SEQ_STATUS_i_5 = 0;
  int __SEQ_STATUS_i_6 = 0;
  int __SEQ_STATUS_flag_7 = 0;
  int __SEQ_STATUS_i_8 = 0;
  int __SEQ_STATUS_i_9 = 0;
  pc_label(__SEQ_STATUS_n1_1 == 1,1,"AUC");
  pc_label(__SEQ_STATUS_n2_2 == 1,2,"AUC");
  printf("Prime numbers between %d and %d are: ",n1,n2);
  i = n1 + 1;
  __SEQ_STATUS_i_3 = 1;
  __SEQ_STATUS_i_5 = 1;
  __SEQ_STATUS_i_8 = 1;
  while (1) {
    pc_label(__SEQ_STATUS_i_3 == 1,3,"AUC");
    pc_label(__SEQ_STATUS_i_4 == 1,4,"AUC");
    if (! (i < n2)) break;
    pc_label(__SEQ_STATUS_i_5 == 1,5,"AUC");
    pc_label(__SEQ_STATUS_i_6 == 1,6,"AUC");
    __SEQ_STATUS_flag_7 = 0;
    flag = checkPrimeNumber(i);
    __SEQ_STATUS_flag_7 = 1;
    pc_label(__SEQ_STATUS_flag_7 == 1,7,"AUC");
    if (flag == 1) {
      pc_label(__SEQ_STATUS_i_8 == 1,8,"AUC");
      pc_label(__SEQ_STATUS_i_9 == 1,9,"AUC");
      printf("%d ",i);
    }
    __SEQ_STATUS_i_3 = 0;
    __SEQ_STATUS_i_4 = 0;
    __SEQ_STATUS_i_5 = 0;
    __SEQ_STATUS_i_6 = 0;
    __SEQ_STATUS_i_8 = 0;
    __SEQ_STATUS_i_9 = 0;
    i ++;
    __SEQ_STATUS_i_4 = 1;
    __SEQ_STATUS_i_6 = 1;
    __SEQ_STATUS_i_9 = 1;
  }
  __retres = 0;
  return __retres;
}

int checkPrimeNumber(int n)
{
  int j;
  int __SEQ_STATUS_flag_10 = 0;
  int __SEQ_STATUS_flag_11 = 0;
  int __SEQ_STATUS_j_12 = 0;
  int __SEQ_STATUS_j_13 = 0;
  int __SEQ_STATUS_n_14 = 1;
  int __SEQ_STATUS_j_15 = 0;
  int __SEQ_STATUS_j_16 = 0;
  int __SEQ_STATUS_n_17 = 1;
  int __SEQ_STATUS_j_18 = 0;
  int __SEQ_STATUS_j_19 = 0;
  int flag = 1;
  __SEQ_STATUS_flag_10 = 1;
  j = 2;
  __SEQ_STATUS_j_12 = 1;
  __SEQ_STATUS_j_15 = 1;
  __SEQ_STATUS_j_18 = 1;
  while (1) {
    pc_label(__SEQ_STATUS_j_12 == 1,12,"AUC");
    pc_label(__SEQ_STATUS_j_13 == 1,13,"AUC");
    pc_label(__SEQ_STATUS_n_14 == 1,14,"AUC");
    if (! (j <= n / 2)) break;
    pc_label(__SEQ_STATUS_n_17 == 1,17,"AUC");
    pc_label(__SEQ_STATUS_j_18 == 1,18,"AUC");
    pc_label(__SEQ_STATUS_j_19 == 1,19,"AUC");
    if (n % j == 0) {
      __SEQ_STATUS_flag_10 = 0;
      flag = 0;
      __SEQ_STATUS_flag_11 = 1;
      break;
    }
    pc_label(__SEQ_STATUS_j_15 == 1,15,"AUC");
    pc_label(__SEQ_STATUS_j_16 == 1,16,"AUC");
    __SEQ_STATUS_j_12 = 0;
    __SEQ_STATUS_j_13 = 0;
    __SEQ_STATUS_j_15 = 0;
    __SEQ_STATUS_j_16 = 0;
    __SEQ_STATUS_j_18 = 0;
    __SEQ_STATUS_j_19 = 0;
    j ++;
    __SEQ_STATUS_j_13 = 1;
    __SEQ_STATUS_j_16 = 1;
    __SEQ_STATUS_j_19 = 1;
  }
  pc_label(__SEQ_STATUS_flag_10 == 1,10,"AUC");
  pc_label(__SEQ_STATUS_flag_11 == 1,11,"AUC");
  return flag;
}


