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

int f(void)
 {
   int __retres;
   __retres = 0;
   return __retres;
 }

int maintest(int c)
{
  int __retres;
  pc_label_sequence(1,1544UL,1,2,"51",0);
  pc_label_sequence(1,1488UL,1,2,"51",0);
  pc_label_sequence(1,1433UL,1,2,"51",0);
  pc_label_sequence(1,1379UL,1,2,"51",0);
  pc_label_sequence_condition(0,"52");
  int a = 1;
  pc_label_sequence(1,1543UL,1,2,"52",0);
  pc_label_sequence(1,1487UL,1,2,"52",0);
  pc_label_sequence(1,1432UL,1,2,"52",0);
  pc_label_sequence_condition(0,"53");
  int b = 2;
  pc_label_sequence(1,1599UL,1,2,"53",0);
  pc_label_sequence(1,1542UL,1,2,"53",0);
  pc_label_sequence(1,1486UL,1,2,"53",0);
  test: ;
  pc_label_sequence(1,1432UL,2,2,"52",0);
  if (a) {
    pc_label_sequence(1,1486UL,2,2,"53",0);
    if (b) {
      pc_label_sequence_condition(0,"53");
      b = 3;
      pc_label_sequence(1,1716UL,1,2,"53",0);
      pc_label_sequence(1,1657UL,1,2,"53",0);
    }
    else {
      test2: pc_label_sequence_condition(0,"52");
             a = f();
      pc_label_sequence(1,1658UL,1,2,"52",0);
      pc_label_sequence(1,1600UL,1,2,"52",0);
    }
  }
  else {
    pc_label_sequence(1,1600UL,2,2,"52",0);
    pc_label_sequence(1,1487UL,2,2,"52",0);
    pc_label_sequence_condition(0,"54");
    int i = a;
    pc_label_sequence(1,1598UL,1,2,"54",0);
    pc_label_sequence(1,1541UL,1,2,"54",0);
    while (1) {
      pc_label_sequence(1,1657UL,2,2,"53",0);
      pc_label_sequence(1,1542UL,2,2,"53",0);
      pc_label_sequence(1,1771UL,2,2,"54",0);
      pc_label_sequence(1,1541UL,2,2,"54",0);
      if (! (i < b)) break;
      f();
      pc_label_sequence(1,1832UL,2,2,"54",0);
      pc_label_sequence(1,1598UL,2,2,"54",0);
      pc_label_sequence_condition(0,"54");
      i ++;
      pc_label_sequence(1,1832UL,1,2,"54",0);
      pc_label_sequence(1,1771UL,1,2,"54",0);
    }
  }
  test4: ;
  pc_label_sequence(1,1379UL,2,2,"51",0);
  switch (c) {
    int tmp;
    test5: case 1: case 14: ;
    pc_label_sequence(1,1716UL,2,2,"53",0);
    pc_label_sequence(1,1599UL,2,2,"53",0);
    pc_label_sequence(1,1658UL,2,2,"52",0);
    pc_label_sequence(1,1543UL,2,2,"52",0);
    __retres = a + b;
    goto return_label;
    case 12: case 13: ;
    pc_label_sequence(1,1433UL,2,2,"51",0);
    __retres = c;
    goto return_label;
    default: ;
    pc_label_sequence(1,1488UL,2,2,"51",0);
    if (c) tmp = 0;
    else {
      pc_label_sequence(1,1544UL,2,2,"51",0);
      tmp = c;
    }
    __retres = tmp;
    goto return_label;
  }
  __retres = 0;
  return_label: return __retres;
}


