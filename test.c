#include <stdio.h>

int g = 10;

int foo(int j)
{
  return 1;
}

void v(int j)
{
  printf("hi, mom");
}

int main()
{
  int c = 1, d = 5;

  for (c = 0, d = c; c < 10; c = c + 1) 
    printf ("c = %d, d = %d", c, d);

  return 0;
}