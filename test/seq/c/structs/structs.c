

//typedef int bool;
#define bool int

#define true 1
#define false 0

#include <string.h>

typedef struct pStruct {
	char name[30];
	unsigned age;
} Person;

Person getPerson() {
	Person res;
	strcpy(res.name, "John Doe");
	res.age = 101;
	return res;
}

bool isTeenager(Person person) {
	return person.age >= 10 && person.age < 20;
}

int main(int argc, char* argv[]) {
	Person mrX = getPerson();
	return isTeenager(mrX);
}
