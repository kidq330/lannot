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

_Bool bool_test(_Bool a)
 {
   _Bool __retres;
   pc_label((int)a == 0,1,"IOB");
   pc_label((int)a == 255,2,"IOB");
   __retres = (_Bool)((int)a + (int)a != 0);
   pc_label((int)__retres == 0,3,"IOB");
   pc_label((int)__retres == 255,4,"IOB");
   return __retres;
 }

char char_test(char a)
{
  char __retres;
  pc_label((int)a == -128,5,"IOB");
  pc_label((int)a == 127,6,"IOB");
  __retres = (char)((int)a + (int)a);
  pc_label((int)__retres == -128,7,"IOB");
  pc_label((int)__retres == 127,8,"IOB");
  return __retres;
}

signed char signed_char_test(signed char a)
{
  signed char __retres;
  pc_label((int)a == -128,9,"IOB");
  pc_label((int)a == 127,10,"IOB");
  __retres = (signed char)((int)a + (int)a);
  pc_label((int)__retres == -128,11,"IOB");
  pc_label((int)__retres == 127,12,"IOB");
  return __retres;
}

unsigned char unsigned_char_test(unsigned char a)
{
  unsigned char __retres;
  pc_label((int)a == 0,13,"IOB");
  pc_label((int)a == 255,14,"IOB");
  __retres = (unsigned char)((int)a + (int)a);
  pc_label((int)__retres == 0,15,"IOB");
  pc_label((int)__retres == 255,16,"IOB");
  return __retres;
}

int int_test(int a)
{
  int __retres;
  pc_label(a == (-2147483647-1),17,"IOB");
  pc_label(a == 2147483647,18,"IOB");
  __retres = a + a;
  pc_label(__retres == (-2147483647-1),19,"IOB");
  pc_label(__retres == 2147483647,20,"IOB");
  return __retres;
}

unsigned int unsigned_int_test(unsigned int a)
{
  unsigned int __retres;
  pc_label(a == 0U,21,"IOB");
  pc_label(a == 4294967295U,22,"IOB");
  __retres = a + a;
  pc_label(__retres == 0U,23,"IOB");
  pc_label(__retres == 4294967295U,24,"IOB");
  return __retres;
}

short short_test(short a)
{
  short __retres;
  pc_label((int)a == -32768,25,"IOB");
  pc_label((int)a == 32767,26,"IOB");
  __retres = (short)((int)a + (int)a);
  pc_label((int)__retres == -32768,27,"IOB");
  pc_label((int)__retres == 32767,28,"IOB");
  return __retres;
}

unsigned short unsigned_short_test(unsigned short a)
{
  unsigned short __retres;
  pc_label((int)a == 0,29,"IOB");
  pc_label((int)a == 65535,30,"IOB");
  __retres = (unsigned short)((int)a + (int)a);
  pc_label((int)__retres == 0,31,"IOB");
  pc_label((int)__retres == 65535,32,"IOB");
  return __retres;
}

long long_test(long a)
{
  long __retres;
  pc_label(a == (-9223372036854775807-1),33,"IOB");
  pc_label(a == 9223372036854775807L,34,"IOB");
  __retres = a + a;
  pc_label(__retres == (-9223372036854775807-1),35,"IOB");
  pc_label(__retres == 9223372036854775807L,36,"IOB");
  return __retres;
}

unsigned long unsigned_long_test(unsigned long a)
{
  unsigned long __retres;
  pc_label(a == 0UL,37,"IOB");
  pc_label(a == 18446744073709551615UL,38,"IOB");
  __retres = a + a;
  pc_label(__retres == 0UL,39,"IOB");
  pc_label(__retres == 18446744073709551615UL,40,"IOB");
  return __retres;
}

long long long_long_test(long long a)
{
  long long __retres;
  pc_label(a == (-9223372036854775807-1),41,"IOB");
  pc_label(a == 9223372036854775807LL,42,"IOB");
  __retres = a + a;
  pc_label(__retres == (-9223372036854775807-1),43,"IOB");
  pc_label(__retres == 9223372036854775807LL,44,"IOB");
  return __retres;
}

unsigned long long unsigned_long_long_test(unsigned long long a)
{
  unsigned long long __retres;
  pc_label(a == 0ULL,45,"IOB");
  pc_label(a == 18446744073709551615ULL,46,"IOB");
  __retres = a + a;
  pc_label(__retres == 0ULL,47,"IOB");
  pc_label(__retres == 18446744073709551615ULL,48,"IOB");
  return __retres;
}


