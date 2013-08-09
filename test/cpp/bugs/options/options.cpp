#include <stdlib.h>

#include <iostream>
#include <string>
#include <sstream>
#include <getopt.h>

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


/* Options long names */
static struct option longopts[] = {
	{ "rows",            required_argument,      NULL,              'r' },
	{ "prep-time",       no_argument,            NULL,     			true},
	{ NULL,              0,                      NULL,               0  }
};



/**********************************/
/* @name option                   */
/* @brief Process user parameters */
/* @param ac argc                 */
/*        av argv                 */
/**********************************/
void option(int ac, char **av) {
	if (ac == 1) std::cout << av[0] << ": Execute with default parameter(s)..\n(--help for program usage)\n\n";
	int opt;
	while ((opt = getopt_long(ac, av, "r:", longopts, NULL)) != -1) {
		switch (opt) {
		case '?' :
			ERROR_HANDLER(0, "Invalid option '" + std::string(av[optind-1]) + "'");
			break;

		case ':' :
			ERROR_HANDLER(0, "Missing argument of option '" + std::string(av[optind-1]) + "'");
			break;

		case 'r':
			{
			std::istringstream iss(optarg);
			int a = -1;
			iss >> a;
			ERROR_HANDLER((!iss.fail()), "Invalid argument '" + std::string(optarg) + "'");
			}
			break;

		case 0 :
			break;

		default :
			ERROR_HANDLER(0, "Error: parsing arguments");
		}
	}
}



int main(int argc, char **argv){

	option(argc, argv);

	return 0;
}

