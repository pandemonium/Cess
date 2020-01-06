#include <stdio.h>

int main()
{
  int i;
  int j = 1;

  for (i = 0; i < 5; i = i + 1) {
    printf ("i: %d; j: %d\n", i, j);
    j = j + 1;
  }

  printf ("after - i: %d, j: %d\n", i, j);

  return 0;
}