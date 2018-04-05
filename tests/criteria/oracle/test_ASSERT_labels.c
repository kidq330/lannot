#include "/usr/local/share/frama-c/stady/externals.h"

int main_precond(int a, int b) {
  return 1;
  
}
int main(int a, int b)
{
  int old_a;
  int old_b;
  old_a = a;
  old_b = b;
  {
    int __retres;
    if (a && b) {
      {
        {
          mpz_t __sd_Z_to_Z_0;
          mpz_t __sd_Z_cst_0;
          int __sd_eq_0;
          int __sd_or_0;
          __gmpz_init_set_si(__sd_Z_to_Z_0,a);
          __gmpz_init_set_str(__sd_Z_cst_0,"0",10);
          __sd_eq_0 = __gmpz_cmp(__sd_Z_to_Z_0,__sd_Z_cst_0);
          __sd_or_0 = __sd_eq_0 == 0;
          if (! __sd_or_0) {
            mpz_t __sd_Z_to_Z_1;
            mpz_t __sd_Z_cst_1;
            int __sd_eq_1;
            __gmpz_init_set_si(__sd_Z_to_Z_1,b);
            __gmpz_init_set_str(__sd_Z_cst_1,"0",10);
            __sd_eq_1 = __gmpz_cmp(__sd_Z_to_Z_1,__sd_Z_cst_1);
            __sd_or_0 = __sd_eq_1 == 0;
            __gmpz_clear(__sd_Z_to_Z_1);
            __gmpz_clear(__sd_Z_cst_1);
          }
          pc_label(! __sd_or_0,0,"ASSERT");
          __gmpz_clear(__sd_Z_to_Z_0);
          __gmpz_clear(__sd_Z_cst_0);
          
        }
        ;
        __retres = a + 1;
        goto return_label;
      }
    }
    else {
      {
        {
          mpz_t __sd_Z_to_Z_2;
          mpz_t __sd_Z_cst_2;
          int __sd_eq_2;
          int __sd_or_1;
          __gmpz_init_set_si(__sd_Z_to_Z_2,a);
          __gmpz_init_set_str(__sd_Z_cst_2,"0",10);
          __sd_eq_2 = __gmpz_cmp(__sd_Z_to_Z_2,__sd_Z_cst_2);
          __sd_or_1 = __sd_eq_2 == 0;
          if (! __sd_or_1) {
            mpz_t __sd_Z_to_Z_3;
            mpz_t __sd_Z_cst_3;
            int __sd_eq_3;
            __gmpz_init_set_si(__sd_Z_to_Z_3,b);
            __gmpz_init_set_str(__sd_Z_cst_3,"0",10);
            __sd_eq_3 = __gmpz_cmp(__sd_Z_to_Z_3,__sd_Z_cst_3);
            __sd_or_1 = __sd_eq_3 == 0;
            __gmpz_clear(__sd_Z_to_Z_3);
            __gmpz_clear(__sd_Z_cst_3);
          }
          pc_label(! __sd_or_1,1,"ASSERT");
          __gmpz_clear(__sd_Z_to_Z_2);
          __gmpz_clear(__sd_Z_cst_2);
          
        }
        ;
        __retres = b;
        goto return_label;
      }
    }
    
    return_label: return __retres;
  }
}
