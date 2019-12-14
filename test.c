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
  int c, d = { foo(1) };

  for (foo(c) < 10; printf("Hi, mom"); c = c + 1 ) printf("hi");

  return 0;
}