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

int maintest(int a, int b, int c)
 {
   int __retres;
   pc_label((a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) && (
            (a && b) || c),1,"GICC");
   pc_label((a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) && ! (
            (a && b) || c),2,"GICC");
   pc_label((! a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) && (
            (a && b) || c),3,"GICC");
   pc_label((! a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) && ! (
            (a && b) || c),4,"GICC");
   pc_label((b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) && (
            (a && b) || c),5,"GICC");
   pc_label((b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) && ! (
            (a && b) || c),6,"GICC");
   pc_label((! b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) && (
            (a && b) || c),7,"GICC");
   pc_label((! b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) && ! (
            (a && b) || c),8,"GICC");
   pc_label(c && ((a && b) || c),9,"GICC");
   pc_label(c && ! ((a && b) || c),10,"GICC");
   pc_label(! c && ((a && b) || c),11,"GICC");
   pc_label(! c && ! ((a && b) || c),12,"GICC");
   if ((a && b) || c) {
     __retres = 0;
     goto return_label;
   }
   __retres = 1;
   return_label: return __retres;
 }


