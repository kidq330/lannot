/* Generated by Frama-C LTest */


#ifndef pc_label
#define pc_label(...) do{}while(0)
#endif
#ifndef pc_label_bindings
#define pc_label_bindings(...) do{}while(0)
#endif

int main(int x)
 {
   int __retres;
   int __SEQ_STATUS_x_1 = 1;
   int __SEQ_STATUS_a_2 = 0;
   int __SEQ_STATUS_b_3 = 0;
   pc_label(__SEQ_STATUS_x_1 == 1,1,"AUC");
   int a = x - x;
   __SEQ_STATUS_a_2 = 1;
   pc_label(__SEQ_STATUS_a_2 == 1,2,"AUC");
   int b = (a - a) * a;
   __SEQ_STATUS_b_3 = 1;
   pc_label(__SEQ_STATUS_b_3 == 1,3,"AUC");
   __retres = b * b + a * a;
   return __retres;
 }


