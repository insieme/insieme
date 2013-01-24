
#include <stdio.h>
#include <error.h>

int main() {


	// use some externally defined error value
	error_message_count = 0;
	printf("%d\n", error_message_count);

	// update counter
	error_message_count++;
	printf("%d\n", error_message_count);

}
