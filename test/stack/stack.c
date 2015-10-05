
#define NULL 0

#include <stdlib.h>

extern int printf(const char *, ...);

// define a simple linked list containing integers
typedef struct elem {
	int value;
	struct elem* next;
} item;

typedef struct {
	item* top;
} stack;

typedef stack* Stack;


Stack newStack() {
	Stack res = malloc(sizeof(stack));
	res->top = NULL;
	return res;
}

void dumpStack(Stack stack) {

	// dump elements
	while (stack->top) {
		item* tmp = stack->top;
		stack->top = tmp->next;
		free(tmp);
	}

	// dump stack
	free(stack);
}

void push(Stack stack, int value) {
	item* element = malloc(sizeof(item));
	element->value = value;
	element->next = stack->top;
	stack->top = element;
}

int pop(Stack stack) {
	if (!stack->top) {
		return -1;
	}
	item* tmp = stack->top;
	int res = tmp->value;
	stack->top = tmp->next;
	free(tmp);
	return res;
}

int main() {

	int i=0;

	Stack stack;
		
	stack = newStack();
	
	for (i=0; i<10; i++) {
		printf("Pushing %d\n", i);
		push(stack, i);
	}
	for (i=0; i<5; i++) {
		printf("Poping  %d\n", pop(stack));
	}

	dumpStack(stack);
	
	return 0;
}
