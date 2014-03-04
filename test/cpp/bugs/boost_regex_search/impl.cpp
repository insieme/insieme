#include "header.h"
#include <string>
#include <iostream>
#include <boost/regex.hpp>

typedef boost::regex RX;

void S::f() {
	RX rx(".*XXX.*");
	std::string str("stringXXXstring");
	boost::smatch m; //PROBLEM

	while(true) {
		if(boost::regex_search(str, m, rx)) {
			std::cout << "true" << std::endl;
		}
		break;
	}
}
