/* Generated by Frama-C LTest */


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

int maintest(int c)
 {
   int __retres;
   pc_label_sequence(1,1UL,1,3,"53",0);
   pc_label_sequence(1,2UL,1,3,"53",0);
   pc_label_sequence(1,3UL,1,4,"53",0);
   pc_label_sequence(1,4UL,1,4,"53",0);
   pc_label_sequence(1,5UL,1,4,"53",0);
   pc_label_sequence(1,6UL,1,4,"53",0);
   pc_label_sequence_condition(0,"54");
   int a = 1;
   pc_label_sequence(1,1UL,2,3,"54",0);
   pc_label_sequence(1,3UL,2,4,"54",0);
   pc_label_sequence(1,5UL,2,4,"54",0);
   pc_label_sequence_condition(0,"55");
   int b = 2;
   pc_label_sequence(1,3UL,3,4,"55",0);
   pc_label_sequence(1,4UL,2,4,"55",0);
   while (1) {
     if (! c) break;
     pc_label_sequence(1,1UL,3,3,"N/A",0);
     pc_label_sequence(1,2UL,3,3,"N/A",0);
     pc_label_sequence_condition(0,"55");
     b = a + c;
     pc_label_sequence(1,5UL,3,4,"55",0);
     pc_label_sequence(1,6UL,2,4,"55",0);
     pc_label_sequence_condition(0,"54");
     a = 2;
     pc_label_sequence(1,2UL,2,3,"54",0);
     pc_label_sequence(1,4UL,3,4,"54",0);
     pc_label_sequence(1,6UL,3,4,"54",0);
   }
   pc_label_sequence(1,3UL,4,4,"N/A",0);
   pc_label_sequence(1,4UL,4,4,"N/A",0);
   pc_label_sequence(1,5UL,4,4,"N/A",0);
   pc_label_sequence(1,6UL,4,4,"N/A",0);
   __retres = (a + b) + c;
   return __retres;
 }


