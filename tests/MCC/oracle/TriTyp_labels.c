/* Generated by Frama-C LTest */
#ifndef pc_label
#define pc_label(...) do{}while(0)
#endif
#ifndef pc_label_bindings
#define pc_label_bindings(...) do{}while(0)
#endif

int triang(int Side1, int Side2, int Side3)
 {
   int __retres;
   int triOut;
   pc_label(Side1 <= 0 && (Side2 <= 0 && Side3 <= 0),1,"MCC");
   pc_label(Side1 <= 0 && (Side2 <= 0 && ! (Side3 <= 0)),2,"MCC");
   pc_label(Side1 <= 0 && (! (Side2 <= 0) && Side3 <= 0),3,"MCC");
   pc_label(Side1 <= 0 && (! (Side2 <= 0) && ! (Side3 <= 0)),4,"MCC");
   pc_label(! (Side1 <= 0) && (Side2 <= 0 && Side3 <= 0),5,"MCC");
   pc_label(! (Side1 <= 0) && (Side2 <= 0 && ! (Side3 <= 0)),6,"MCC");
   pc_label(! (Side1 <= 0) && (! (Side2 <= 0) && Side3 <= 0),7,"MCC");
   pc_label(! (Side1 <= 0) && (! (Side2 <= 0) && ! (Side3 <= 0)),8,"MCC");
   if ((Side1 <= 0 || Side2 <= 0) || Side3 <= 0) {
     triOut = 4;
     __retres = triOut;
     goto return_label;
   }
   triOut = 0;
   pc_label(Side1 == Side2,9,"MCC");
   pc_label(! (Side1 == Side2),10,"MCC");
   if (Side1 == Side2) triOut ++;
   pc_label(Side1 == Side3,11,"MCC");
   pc_label(! (Side1 == Side3),12,"MCC");
   if (Side1 == Side3) triOut += 2;
   pc_label(Side2 == Side3,13,"MCC");
   pc_label(! (Side2 == Side3),14,"MCC");
   if (Side2 == Side3) triOut += 3;
   pc_label(triOut == 0,15,"MCC");
   pc_label(! (triOut == 0),16,"MCC");
   if (triOut == 0) {
     pc_label(Side1 + Side2 <= Side3 && (Side2 + Side3 <= Side1 && Side1 + Side3 <= Side2),
              17,"MCC");
     pc_label(Side1 + Side2 <= Side3 && (Side2 + Side3 <= Side1 && ! (
                                         Side1 + Side3 <= Side2)),18,"MCC");
     pc_label(Side1 + Side2 <= Side3 && (! (Side2 + Side3 <= Side1) && 
                                         Side1 + Side3 <= Side2),19,"MCC");
     pc_label(Side1 + Side2 <= Side3 && (! (Side2 + Side3 <= Side1) && ! (
                                         Side1 + Side3 <= Side2)),20,"MCC");
     pc_label(! (Side1 + Side2 <= Side3) && (Side2 + Side3 <= Side1 && 
                                             Side1 + Side3 <= Side2),21,
              "MCC");
     pc_label(! (Side1 + Side2 <= Side3) && (Side2 + Side3 <= Side1 && ! (
                                             Side1 + Side3 <= Side2)),22,
              "MCC");
     pc_label(! (Side1 + Side2 <= Side3) && (! (Side2 + Side3 <= Side1) && 
                                             Side1 + Side3 <= Side2),23,
              "MCC");
     pc_label(! (Side1 + Side2 <= Side3) && (! (Side2 + Side3 <= Side1) && ! (
                                             Side1 + Side3 <= Side2)),24,
              "MCC");
     if ((Side1 + Side2 <= Side3 || Side2 + Side3 <= Side1) || Side1 + Side3 <= Side2) 
       triOut = 4;
     else triOut = 1;
     __retres = triOut;
     goto return_label;
   }
   pc_label(triOut > 3,25,"MCC");
   pc_label(! (triOut > 3),26,"MCC");
   if (triOut > 3) triOut = 3;
   else {
     pc_label(triOut == 1 && Side1 + Side2 > Side3,27,"MCC");
     pc_label(triOut == 1 && ! (Side1 + Side2 > Side3),28,"MCC");
     pc_label(! (triOut == 1) && Side1 + Side2 > Side3,29,"MCC");
     pc_label(! (triOut == 1) && ! (Side1 + Side2 > Side3),30,"MCC");
     if (triOut == 1 && Side1 + Side2 > Side3) triOut = 2;
     else {
       pc_label(triOut == 2 && Side1 + Side3 > Side2,31,"MCC");
       pc_label(triOut == 2 && ! (Side1 + Side3 > Side2),32,"MCC");
       pc_label(! (triOut == 2) && Side1 + Side3 > Side2,33,"MCC");
       pc_label(! (triOut == 2) && ! (Side1 + Side3 > Side2),34,"MCC");
       if (triOut == 2 && Side1 + Side3 > Side2) triOut = 2;
       else {
         pc_label(triOut == 3 && Side2 + Side3 > Side1,35,"MCC");
         pc_label(triOut == 3 && ! (Side2 + Side3 > Side1),36,"MCC");
         pc_label(! (triOut == 3) && Side2 + Side3 > Side1,37,"MCC");
         pc_label(! (triOut == 3) && ! (Side2 + Side3 > Side1),38,"MCC");
         if (triOut == 3 && Side2 + Side3 > Side1) triOut = 2;
         else triOut = 4;
       }
     }
   }
   __retres = triOut;
   return_label: return __retres;
 }


