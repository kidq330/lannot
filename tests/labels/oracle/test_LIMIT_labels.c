/* Generated by Frama-C LTest */


#ifndef pc_label
#define pc_label(...) do{}while(0)
#endif
#ifndef pc_label_bindings
#define pc_label_bindings(...) do{}while(0)
#endif

int maintest(int a, int b)
 {
   int __retres;
   pc_label(a < b && ((a - b) + 1 <= 0 && - ((a - b) + 1) <= 0),1,"LIMIT");
   if (a < b) ;
   pc_label(a <= b && (a - b <= 0 && - (a - b) <= 0),2,"LIMIT");
   if (a <= b) ;
   pc_label(a >= b && (a - b <= 0 && - (a - b) <= 0),3,"LIMIT");
   if (a >= b) ;
   pc_label(a > b && ((a - b) - 1 <= 0 && - ((a - b) - 1) <= 0),4,"LIMIT");
   if (a > b) ;
   __retres = 0;
   return __retres;
 }


