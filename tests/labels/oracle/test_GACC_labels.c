/* Generated by Frama-C LTest */


#ifndef pc_label
#define pc_label(...) do{}while(0)
#endif
#ifndef pc_label_bindings
#define pc_label_bindings(...) do{}while(0)
#endif

int maintest(int a, int b, int c)
 {
   int __retres;
   pc_label(a && (! (b || c) && c || (b || c) && ! c),1,"GACC");
   pc_label(! a && (! (b || c) && c || (b || c) && ! c),2,"GACC");
   pc_label(b && (! (a || c) && c || (a || c) && ! c),3,"GACC");
   pc_label(! b && (! (a || c) && c || (a || c) && ! c),4,"GACC");
   pc_label(c && ! (a && b),5,"GACC");
   pc_label(! c && ! (a && b),6,"GACC");
   if (a && b || c) {
     __retres = 0;
     goto return_label;
   }
   __retres = 1;
   return_label: return __retres;
 }


