#include <stdio.h>
#include <stdlib.h>

typedef struct {
	int size;
	int data[];
} list;

typedef list* List;

List createList(int size) {
	List res = (List)malloc(sizeof(list) + sizeof(int) * size);
	res->size = size;
	return res;
}

List createSequence(int start, int end) {
	List res = createList(end - start + 1);
	for (int i=0; i<res->size; i++) {
		res->data[i] = start + i;
	}
	return res;
}

void deleteList(List list) {
	free(list);
}

int equal(List a, List b) {
	int res = a->size == b->size;
	for (int i=0; res && i<a->size; i++) {
		res = res && a->data[i] == b->data[i];
	}
	return res;
}

void printList(List list) {
	for (int i=0; i<list->size; i++) {
		printf("%d ", list->data[i]);
	}
	printf("\n");
}

int main() {

	List a = createSequence(3,7);
	printList(a);
	
	List b = createSequence(3,9);
	printList(b);

	printf("Equal: %s\n", (equal(a,b))?"Yes":"No");

	b->size = 5;
	printList(b);
	printf("Equal: %s\n", (equal(a,b))?"Yes":"No");


	deleteList(a);
	deleteList(b);

	return EXIT_SUCCESS;
}


