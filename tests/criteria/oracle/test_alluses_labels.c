/* Generated by Frama-C LTest */
#ifndef pc_label
#define pc_label(...) do{}while(0)
#endif
#ifndef pc_label_bindings
#define pc_label_bindings(...) do{}while(0)
#endif

int f(void)
 {
   int __retres;
   __retres = 0;
   return __retres;
 }

int main(int c)
{
  int __retres;
  set_pc_label_sequence(1,2297,1,2,"48",0);
  set_pc_label_sequence(1,1904,1,2,"48",0);
  set_pc_label_sequence(1,1604,1,2,"48",0);
  set_pc_label_sequence(1,1382,1,2,"48",0);
  pc_label_sequence_condition(0,"49");
  int a = 1;
  set_pc_label_sequence(1,1966,1,2,"49",0);
  set_pc_label_sequence(1,1661,1,2,"49",0);
  set_pc_label_sequence(1,1435,1,2,"49",0);
  pc_label_sequence_condition(0,"50");
  int b = 2;
  set_pc_label_sequence(1,2952,1,2,"50",0);
  set_pc_label_sequence(1,2434,1,2,"50",0);
  set_pc_label_sequence(1,2029,1,2,"50",0);
  set_pc_label_sequence(1,1719,1,2,"50",0);
  set_pc_label_sequence(1,1489,1,2,"50",0);
  use_pc_label_sequence(1,1435,2,2,"49",0);
  if (a) {
    use_pc_label_sequence(1,1489,2,2,"50",0);
    if (b) {
      pc_label_sequence_condition(0,"50");
      b = 3;
      set_pc_label_sequence(1,3519,1,2,"50",0);
      set_pc_label_sequence(1,2875,1,2,"50",0);
      set_pc_label_sequence(1,2364,1,2,"50",0);
      set_pc_label_sequence(1,1965,1,2,"50",0);
    }
    else {
      pc_label_sequence_condition(0,"49");
      a = f();
      set_pc_label_sequence(1,2296,1,2,"49",0);
      set_pc_label_sequence(1,1903,1,2,"49",0);
    }
  }
  else {
    use_pc_label_sequence(1,1965,2,2,"50",0);
    use_pc_label_sequence(1,1719,2,2,"50",0);
    pc_label_sequence_condition(0,"49");
    a = b;
    set_pc_label_sequence(1,2725,1,2,"49",0);
    set_pc_label_sequence(1,2228,1,2,"49",0);
    use_pc_label_sequence(1,2364,2,2,"50",0);
    use_pc_label_sequence(1,2029,2,2,"50",0);
    if (b) {
      pc_label_sequence_condition(0,"49");
      a = 3;
      set_pc_label_sequence(1,3271,1,2,"49",0);
      set_pc_label_sequence(1,2651,1,2,"49",0);
      use_pc_label_sequence(1,3435,2,2,"50",0);
      use_pc_label_sequence(1,2875,2,2,"50",0);
      use_pc_label_sequence(1,2434,2,2,"50",0);
      pc_label_sequence_condition(0,"50");
      b = b;
      set_pc_label_sequence(1,4227,1,2,"50",0);
      set_pc_label_sequence(1,3435,1,2,"50",0);
    }
    else {
      use_pc_label_sequence(1,2651,2,2,"49",0);
      use_pc_label_sequence(1,2228,2,2,"49",0);
      use_pc_label_sequence(1,1903,2,2,"49",0);
      use_pc_label_sequence(1,1661,2,2,"49",0);
      pc_label_sequence_condition(0,"50");
      b = a;
      set_pc_label_sequence(1,5100,1,2,"50",0);
    }
  }
  use_pc_label_sequence(1,1382,2,2,"48",0);
  switch (c) {
    int tmp;
    case 1: ;
    use_pc_label_sequence(1,5100,2,2,"50",0);
    use_pc_label_sequence(1,4227,2,2,"50",0);
    use_pc_label_sequence(1,3519,2,2,"50",0);
    use_pc_label_sequence(1,2952,2,2,"50",0);
    use_pc_label_sequence(1,3271,2,2,"49",0);
    use_pc_label_sequence(1,2725,2,2,"49",0);
    use_pc_label_sequence(1,2296,2,2,"49",0);
    use_pc_label_sequence(1,1966,2,2,"49",0);
    __retres = a + b;
    goto return_label;
    case 12: ;
    use_pc_label_sequence(1,1604,2,2,"48",0);
    __retres = c;
    goto return_label;
    default: ;
    use_pc_label_sequence(1,1904,2,2,"48",0);
    if (c) tmp = 0;
    else {
      use_pc_label_sequence(1,2297,2,2,"48",0);
      tmp = c;
    }
    __retres = tmp;
    goto return_label;
  }
  __retres = 0;
  return_label: return __retres;
}


