#include <string>
#include <iostream>
#include <boost/regex.hpp>


static const boost::regex numberFilter  ("([0-9]+)(\\.[0-9]+)?([ufl]*)");
static const boost::regex precissionFilter  ("([0-9]|([0-9]*\\.[0-9]+))([ufl]*)");
static const boost::regex zerosFilter  ("\\.[0]+");
static const boost::regex zeroValueFilter  ("([0]+)(\\.[0]+)?([ufl]*)");

int main (){

	boost::regex_match ("asfdaf", numberFilter);

	return 0;
}
