int main (int a, int b, int c) {
  int z =  a && b || c;
  
  if (z && !a) {

  }

  int n = ! (a && z) + 2*5 - (a || c);
  return n;
}
