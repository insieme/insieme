#include <string>
#include <iostream>
#include <boost/regex.hpp>


static const boost::regex numberFilter  ("([0-9]+)(\\.[0-9]+)?([ufl]*)");
static const boost::regex precissionFilter  ("([0-9]|([0-9]*\\.[0-9]+))([ufl]*)");
static const boost::regex zerosFilter  ("\\.[0]+");
static const boost::regex zeroValueFilter  ("([0]+)(\\.[0]+)?([ufl]*)");

int main (){

	std::cout << "NotANumber " << boost::regex_match ("NotANumber", numberFilter,boost::match_default) << std::endl;
	std::cout << "1000 " << boost::regex_match ("1000", numberFilter) << std::endl;

	return 0;
}
