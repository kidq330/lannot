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
   pc_label((a < b || a / b != 42) != (a < b && a / b != 42),1,"WM COR");
   pc_label((a <= b) != (a < b),2,"WM ROR");
   pc_label((a > b) != (a < b),3,"WM ROR");
   pc_label((a >= b) != (a < b),4,"WM ROR");
   pc_label(a < 0,5,"WM ABS");
   pc_label(b < 0,6,"WM ABS");
   pc_label((a / b == 42) != (a / b != 42),7,"WM ROR");
   pc_label(a * b != a / b,8,"WM AOR");
   pc_label(a + b != a / b,9,"WM AOR");
   pc_label(a - b != a / b,10,"WM AOR");
   pc_label(a < 0,11,"WM ABS");
   pc_label(b < 0,12,"WM ABS");
   if (a < b && a / b != 42) {
     pc_label(a < 0,13,"WM ABS");
     pc_label(b < 0,14,"WM ABS");
     __retres = a % b;
     goto return_label;
   }
   else {
     pc_label((a + b) / 4 != (a + b) * 4,15,"WM AOR");
     pc_label((a + b) + 4 != (a + b) * 4,16,"WM AOR");
     pc_label((a + b) - 4 != (a + b) * 4,17,"WM AOR");
     pc_label(a * b != a + b,18,"WM AOR");
     pc_label(a / b != a + b,19,"WM AOR");
     pc_label(a - b != a + b,20,"WM AOR");
     pc_label(a < 0,21,"WM ABS");
     pc_label(b < 0,22,"WM ABS");
     __retres = (a + b) * 4;
     goto return_label;
   }
   return_label: {
                   pc_label(__retres < 0,23,"WM ABS");
                   return __retres;
                 }
 }


