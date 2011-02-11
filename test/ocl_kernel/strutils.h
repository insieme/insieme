#pragma once

#include <string>
#include <cstdarg>

std::string format(const char* formatString, ...) {
	va_list arglist;
	va_start(arglist, formatString);
	const unsigned BUFFER_SIZE = 2048;
	char buffer[BUFFER_SIZE];
	vsnprintf(buffer, BUFFER_SIZE, formatString, arglist);
	va_end(arglist);
	return std::string(buffer);
}
