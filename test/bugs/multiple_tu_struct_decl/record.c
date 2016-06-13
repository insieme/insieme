#include <stdlib.h>

struct muha {
  int a;
};

struct muha* init() {
  struct muha* g = (struct muha*)malloc(sizeof(struct muha));
  g->a = 42;
  return g;
}

