#include <iostream>
#include "boost/exception/all.hpp"

struct exception : virtual std::exception, virtual boost::exception {};
typedef ::boost::error_info<struct _Msg, std::string> ErrMsg;
typedef ::boost::error_info<struct _No, int> ErrNo;

int main () {
	try {
		throw exception() << ErrMsg("error_msg");
	} catch(exception& e) {
		if(std::string const* msg = boost::get_error_info<ErrMsg>(e))
			std::cout << "Exception catched -- errorMsg: " << *msg << std::endl;
		
		std::cout << boost::diagnostic_information(e) << std::endl;
	}

	/*
	//FIXME: why does it fail if used without ref?
	try {
		throw exception() << ErrMsg("error_msg");
	} catch(exception e) {
		std::cout << "Exception catched -- errorMsg: " << *boost::get_error_info<ErrMsg>(e) << std::endl;
		std::cout << boost::diagnostic_information(e) << std::endl;
	}
	*/
	
	try {
		throw exception() << ErrNo(1);
	} catch(exception& e) {
		if(int const* no = boost::get_error_info<ErrNo>(e))
			std::cout << "Exception catched -- error No: " << *no << std::endl;
		
		std::cout << boost::diagnostic_information(e) << std::endl;
	}
	
	try {
		throw exception() << ErrNo(1);
	} catch(...) {
		std::cout << "Exception catched" << std::endl;
	}

	try {
		throw exception() << ErrMsg("string");
	} catch(...) {
		std::cout << "Exception catched" << std::endl;
	}
}
