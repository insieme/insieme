#include <stdio.h>
#include <stdlib.h>

struct Village;
struct Patient;
struct Hosp;

struct Patient {
   int id;
   int seed;
   int time;
   int time_left;
   int hosps_visited;
   struct Village *home_village;
   struct Patient *back;
   struct Patient *forward;
};
struct Hosp {
   int personnel;
   int free_personnel;
   struct Patient *waiting;
   struct Patient *assess;
   struct Patient *inside;
   struct Patient *realloc;
//   struct _irt_lock  realloc_lock;
};
struct Village {
   int id;
   struct Village *back;
   struct Village *next;
   struct Village *forward;
   struct Patient *population;
   struct Hosp hosp;
   int level;
   int  seed;
};

int main(int argc, char* argv[]) {

	struct Village *v = malloc(sizeof(struct Village));
	v->population = malloc(sizeof(struct Patient));

	//v->hosp;
	struct Patient* p;
	v->population->back = p;

	free(v->population);
	free(v);
}
