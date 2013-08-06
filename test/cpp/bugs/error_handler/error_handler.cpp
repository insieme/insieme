#include <iostream>
#include <string>

#include <sstream>

#include <stdlib.h>
#include <getopt.h>
#include <unistd.h>

/* Error handler */
#define ERROR_HANDLER(x, y) __check_error((x), (y), __FILE__, __LINE__)

/** 
 * @brief Force exit if condition is 0
 *        MACRO -> ERROR_HANDLER(x, y)
 * 
 * @param condition      Condition to be checked
 * @param errorString    Collection of error strings
 * @param file           File to be checked
 * @param line           Line number
 */

inline void __check_error(bool condition, const std::string errorString, const char *file, const int line) {
	if (!condition) {
		std::cerr << "ERROR: "
		<< errorString << std::endl
		<< "in " << file << " line: " << line << std::endl;
		exit(1);
	}
}


bool flag(){
	return true;
}

char cad[] = "this is string";

int main(int argc, char **argv){

		ERROR_HANDLER(flag(), "Invalid argument '" + std::string(cad) + "'");

		std::cout << "error right" << std::endl;

	return 0;
}

