/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 */

#pragma once

#include <stdio.h>
#include <algorithm>
#include <functional>
#include <string>
#include <cstring>
#include <cctype>
#include <sstream>
#include <iterator>

#include <boost/iostreams/concepts.hpp>
#include <boost/iostreams/operations.hpp>
#include <boost/iostreams/filtering_stream.hpp>

#include "functional_utils.h"

using std::string;

template <typename T>
string toString(const T& value) {
	std::stringstream res;
	res << value;
	return res.str();
}

namespace detail {

	// a template functor to convert any object into something printable
	template <typename T>
	struct to_printable {
		string operator()(const T& obj) const {
			return toString(obj);
		}
	};

	template <> struct to_printable<bool>       : public id<bool> {};

	template <> struct to_printable<char>       : public id<char> {};
	template <> struct to_printable<short>      : public id<short> {};
	template <> struct to_printable<int>        : public id<int> {};
	template <> struct to_printable<long>       : public id<long> {};
	template <> struct to_printable<long long>  : public id<long long> {};

	template <> struct to_printable<unsigned char>       : public id<unsigned char> {};
	template <> struct to_printable<unsigned short>      : public id<unsigned short> {};
	template <> struct to_printable<unsigned int>        : public id<unsigned int> {};
	template <> struct to_printable<unsigned long>       : public id<unsigned long> {};
	template <> struct to_printable<unsigned long long>  : public id<unsigned long long> {};

	template <> struct to_printable<float>  : public id<float> {};
	template <> struct to_printable<double> : public id<double> {};

	template <> struct to_printable<string> : public id<string> {};

	template <typename T>
	struct to_printable<T&> : public to_printable<T> {};
	template <typename T>
	struct to_printable<const T> : public to_printable<T> {};
	template <typename T>
	struct to_printable<const T&> : public to_printable<T> {};


	// a template functor to convert something printable into a C-primitive
	template<typename T>
	struct to_c_primitive : public id<T> {};

	template<>
	struct to_c_primitive<string> {
		const char* operator()(const string& str) {
			return str.c_str();
		}
	};

	template <typename T>
	struct to_c_primitive<T&> : public to_c_primitive<T> {};
	template <typename T>
	struct to_c_primitive<const T> : public to_c_primitive<T> {};
	template <typename T>
	struct to_c_primitive<const T&> : public to_c_primitive<T> {};


	// a wrapper for the to_c_primitive functor
	template<typename T>
	auto to_c_prim(const T& t) -> decltype(to_c_primitive<T>()(t)) {
		return to_c_primitive<T>()(t);
	}

}

template <typename... Args>
string format(const char* formatString, const Args&... args) {
	string retval;
	const int BUFFER_SIZE_FIXED = 2048;
	int BUFFER_SIZE = BUFFER_SIZE_FIXED;
	char buffer[BUFFER_SIZE_FIXED];

	#pragma GCC diagnostic push
	#pragma GCC diagnostic ignored "-Wformat-security"

	int written = snprintf(buffer, BUFFER_SIZE, formatString, detail::to_c_prim(detail::to_printable<decltype(args)>()(args))...);
	if(written >= BUFFER_SIZE || written < 0) { // deal with both GNU and c99 error reporting
		BUFFER_SIZE = written > 0 ? written + 1 : 1024 * 1024 * 8;
		char* heap_buffer = new char[BUFFER_SIZE];
		snprintf(heap_buffer, BUFFER_SIZE, formatString, detail::to_c_prim(detail::to_printable<decltype(args)>()(args))...);
		retval = string(heap_buffer);
		delete[] heap_buffer;
	} else {
		retval = string(buffer);
	}

	#pragma GCC diagnostic pop

	return retval;
}

/**
 * A utility method to split a string along its white spaces.
 *
 * @param str the string to be splitted
 * @return the vector of substrings
 */
std::vector<string> split(const string& str);

/**
 * A utility method to find the common prefix of two strings.
 *
 * @return a new string containing the prefix
 */
string commonPrefix(string a, string b);

/**
 * Tests whether the given string contains the given sub-string.
 *
 * @param str the string searching in
 * @param substr the string searching for
 */
bool containsSubString(const string& str, const string& substr);

/**
* Tests whether the given string does not contain the given sub-string.
*
* @param str the string searching in
* @param substr the string searching for
*/
bool notContainsSubString(const string& str, const string& substr);

/**
* Tests whether the given string contains exactly N times the given sub-string.
*
* @param str the string searching in
* @param substr the string searching for
* @param n the times
*/
bool containsNTimesSubString(const string& str, const string& substr, const int n);

/**
 * Converts a string from camelcase to underscore style to. Mainly used to convert names in INSIEME programming style to IR style
 *
 * @param input the input string to be converted to underscore
 */
string camelcaseToUnderscore(const string input);


namespace insieme {
namespace utils {

	/**
	 * Removes C (/ *.. * /) and C++ (//..) style comments from the input string
	 */
	string removeCppStyleComments(const string& in);

	/**
	 * Removes lines starting with # from the input string
	 */
	string removePragmas(const string& in);

}
}

/**
 * This functor can be used to print elements to an output stream.
 */
template <typename Extractor>
struct print : public std::binary_function<std::ostream&, const typename Extractor::argument_type&, std::ostream&> {
	Extractor extractor;
	std::ostream& operator()(std::ostream& out, const typename Extractor::argument_type& cur) const {
		return out << extractor(cur);
	}
};

template <typename Iterator, typename Printer>
struct Joinable {
	const string separator;
	const Iterator begin;
	const Iterator end;
	const Printer& printer;

	Joinable(const string& separator, const Iterator& begin, const Iterator& end, const Printer& printer)
	    : separator(separator), begin(begin), end(end), printer(printer){};
};


template <typename Iterator, typename Printer>
std::ostream& operator<<(std::ostream& out, const Joinable<Iterator, Printer>& joinable) {
	if(joinable.begin != joinable.end) {
		auto iter = joinable.begin;
		joinable.printer(out, *iter);
		++iter;
		for(; iter != joinable.end; ++iter) {
			out << joinable.separator;
			joinable.printer(out, *iter);
		}
	}
	return out;
}

/**
 * Joins the values in the collection to the stream separated by a supplied separator.
 **/
template <typename Container, typename Printer = print<typename Container::element_type>>
Joinable<typename Container::const_iterator, Printer> join(const string& separator, const Container& container, const Printer& printer = Printer()) {
	return Joinable<typename Container::const_iterator, Printer>(separator, container.begin(), container.end(), printer);
}

template <typename Container>
Joinable<typename Container::const_iterator, print<id<const typename Container::value_type&>>> join(const string& separator, const Container& container) {
	return Joinable<typename Container::const_iterator, print<id<const typename Container::value_type&>>>(separator, container.begin(), container.end(),
	                                                                                                      print<id<const typename Container::value_type&>>());
}

template <typename Iterator, typename Printer>
Joinable<Iterator, Printer> join(const string& separator, const Iterator& begin, const Iterator& end, const Printer& printer) {
	return Joinable<Iterator, Printer>(separator, begin, end, printer);
}

template <typename Iterator>
Joinable<Iterator, print<id<const typename std::iterator_traits<Iterator>::value_type&>>> join(const string& separator, const Iterator& begin,
                                                                                               const Iterator& end) {
	return Joinable<Iterator, print<id<const typename std::iterator_traits<Iterator>::value_type&>>>(
	    separator, begin, end, print<id<const typename std::iterator_traits<Iterator>::value_type&>>());
}

template <typename Iterator, typename Printer>
Joinable<Iterator, Printer> join(const string& separator, const std::pair<Iterator, Iterator>& range, const Printer& printer) {
	return Joinable<Iterator, Printer>(separator, range.first, range.second, printer);
}

template <typename Iterator>
Joinable<Iterator, print<id<const typename std::iterator_traits<Iterator>::value_type&>>> join(const string& separator,
                                                                                               const std::pair<Iterator, Iterator>& range) {
	return Joinable<Iterator, print<id<const typename std::iterator_traits<Iterator>::value_type&>>>(
	    separator, range.first, range.second, print<id<const typename std::iterator_traits<Iterator>::value_type&>>());
}

template <typename SeparatorFunc, typename Container, typename Printer>
void functionalJoin(SeparatorFunc seperatorFunc, Container container, Printer printer) {
	if(container.size() > 0) {
		auto iter = container.cbegin();
		printer(*iter);
		++iter;
		std::for_each(iter, container.cend(), [&](const typename Container::value_type& cur) {
			seperatorFunc();
			printer(cur);
		});
	}
}

template <typename Element, typename Printer>
struct Multiplier {
	const Element& element;
	const unsigned times;
	const Printer& printer;
	const string& separator;

	Multiplier(const Element& element, unsigned times, const Printer& printer, const string& separator)
	    : element(element), times(times), printer(printer), separator(separator){};
};

template <typename Element, typename Printer>
std::ostream& operator<<(std::ostream& out, const Multiplier<Element, Printer>& multiplier) {
	if(multiplier.times > 0) { multiplier.printer(out, multiplier.element); }
	for(unsigned i = 1; i < multiplier.times; i++) {
		out << multiplier.separator;
		multiplier.printer(out, multiplier.element);
	}

	return out;
}

template <typename Element, typename Printer = print<id<Element>>>
Multiplier<Element, Printer> times(const Element& element, unsigned times, const string& separator = "", const Printer& printer = Printer()) {
	return Multiplier<Element, Printer>(element, times, printer, separator);
}


namespace {

	struct escape_character_filter {
		typedef char char_type;
		typedef boost::iostreams::output_filter_tag category;

		template <typename Sink>
		bool put(Sink& snk, char c) {
			switch(c) {
			case '\a': return boost::iostreams::put(snk, '\\') && boost::iostreams::put(snk, 'a');
			case '\b': return boost::iostreams::put(snk, '\\') && boost::iostreams::put(snk, 'b');
			case '\f': return boost::iostreams::put(snk, '\\') && boost::iostreams::put(snk, 'f');
			case '\n': return boost::iostreams::put(snk, '\\') && boost::iostreams::put(snk, 'n');
			case '\r': return boost::iostreams::put(snk, '\\') && boost::iostreams::put(snk, 'r');
			case '\t': return boost::iostreams::put(snk, '\\') && boost::iostreams::put(snk, 't');
			case '\v': return boost::iostreams::put(snk, '\\') && boost::iostreams::put(snk, 'v');
			case '\'': return boost::iostreams::put(snk, '\\') && boost::iostreams::put(snk, '\'');
			case '\\': return boost::iostreams::put(snk, '\\') && boost::iostreams::put(snk, '\\');
			case '"': return boost::iostreams::put(snk, '\\') && boost::iostreams::put(snk, '"');
			}
			return boost::iostreams::put(snk, c);
		}
	};

	template <typename T>
	class CharacterEscaper {
		T& sink;
		boost::iostreams::filtering_ostream filter;

	  public:
		CharacterEscaper(T& sink) : sink(sink) {
			filter.push(escape_character_filter());
			filter.push(sink);
			filter.set_auto_close(true);
		}

		CharacterEscaper(const CharacterEscaper& other) : sink(other.sink) {
			filter.push(escape_character_filter());
			filter.push(sink);
			filter.set_auto_close(true);
		}

		template <typename E>
		CharacterEscaper& operator<<(E& element) {
			filter << element;
			return *this;
		}
	};
}

template <typename T>
CharacterEscaper<T> escape(T& stream) {
	return CharacterEscaper<T>(stream);
}


// Indent handling

/**
 * A utility class helping to manager the indent of hierarchically structured code.
 */
struct Indent {
	/**
	 * The number of blanks represented by this indent
	 */
	unsigned indent;

	/**
	 * The step size the indent should be increased.
	 */
	unsigned stepSize;

	/**
	 * Creates a new indent instance based on the given number of
	 * indent-blanks and the given step size.
	 */
	Indent(unsigned indent = 0, unsigned stepSize = 4) : indent(indent), stepSize(stepSize) {}

	/**
	 * Increases the indent by the given number of steps.
	 */
	Indent operator+(int steps) const {
		return Indent(indent + steps * stepSize);
	}

	/**
	 * Decreases the indent by the given number of steps.
	 */
	Indent operator-(int steps) const {
		return Indent(indent - steps * stepSize);
	}

	/**
	 * Increases the indent by one unit (prefix).
	 */
	Indent& operator++() {
		indent += stepSize;
		return *this;
	}

	/**
	 * Increases the indent by one unit (postfix).
	 */
	Indent operator++(int) {
		Indent res = *this;
		++*this;
		return res;
	}
};

namespace std {

	inline std::ostream& operator<<(std::ostream& out, const Indent& indent) {
		return out << times(" ", indent.indent);
	}
}
