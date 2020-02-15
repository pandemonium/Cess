#include <stdio.h>

int fib (int x) {
  return fib(x + 1) + fib(x + 2);
}

int main()
{
  int i;
  int j = 1;

  if (1.0) printf("hi");

  for (i = 0; i < 5; i = i + 1) {
    printf ("i: %d; j: %d\n", i, j);
    j = j + 1;
  }

  printf ("after - i: %d, j: %d\n", i, j);
  
  return 0;

}