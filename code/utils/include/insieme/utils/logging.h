/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
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
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#pragma once

#include <iostream>

#include <boost/iostreams/stream.hpp>

#include <boost/date_time/posix_time/posix_time.hpp>

#include <boost/any.hpp>

#include <mutex>
#include <thread>

#include <stdexcept>

#ifdef __GNUC__
#include <signal.h>
#include <execinfo.h>
#endif

namespace insieme {
namespace utils {
namespace log {

#define LOG_DEFAULT DEBUG

namespace io = boost::iostreams;

/**
 * Temporary object used to wrap the log stream. This object is responsible to
 * collect logs and flush the stream once the object is deallocated.
 *
 * A lock is used to mantain exclusivity of logs, in case of multi-threaded application
 * the logger guarantees mutual exclusion between threads using the stream.
 */
struct Writer {
	std::ostream& logStream;
	std::recursive_mutex& mutex;

	Writer(std::ostream& out, std::recursive_mutex& mutex) : logStream( out ), mutex( mutex ) {
		mutex.lock();
	}

	// Creates a copy from an existing writer. When a logger is requested
	// a temporary object is created. Also a new lock is created.
	Writer(const Writer& other) : logStream( other.logStream ), mutex( other.mutex ) {
		mutex.lock();
	}

	~Writer() {
		// Append a new line at the end of this log sequence
		logStream << std::endl;
		logStream.flush();
		mutex.unlock();
	}
};

enum Level { DEBUG, INFO, WARNING, ERROR, FATAL };

// Because the file name cannot be use as a template parameters
// we keep a list of objects representing the context of the logger
typedef std::vector<boost::any> Ctx;

enum TimeFormat { FULL, TIME, DATE };

// Prints the current time
template <TimeFormat format>
struct TimeSpec;

template <>
struct TimeSpec<FULL> {
	static void format(std::ostream& out, const Ctx& ctx) {
		out << boost::posix_time::second_clock::local_time();
	}
};

template <>
struct TimeSpec<TIME> {
	static void format(std::ostream& out, const Ctx& ctx) {
		out << boost::posix_time::second_clock::local_time().time_of_day();
	}
};

template <>
struct TimeSpec<DATE> {
	static void format(std::ostream& out, const Ctx& ctx) {
		out << boost::posix_time::second_clock::local_time().date();
	}
};

struct LoggingLevelNotDefined: public std::runtime_error {
	LoggingLevelNotDefined(const std::string& message): std::runtime_error(message){}
};

/**
 * Prints the level at which the log was taken.
 */
template <const Level L=DEBUG>
struct LevelSpec {

	static std::string loggingLevelToStr(const Level& level) {
		switch(level) {
		case DEBUG:		return "DEBUG";
		case INFO:		return "INFO ";
		case WARNING:	return "WARN ";
		case ERROR:		return "ERROR";
		case FATAL:		return "FATAL";
		default:
		assert(false); return "UNKNOWN";
		}
	}
	
	static Level loggingLevelFromStr(const std::string& level) {
		if(level.empty())		return LOG_DEFAULT;
		if(level == "DEBUG")	return DEBUG;
		if(level == "INFO")		return INFO;
		if(level == "WARNING")	return WARNING;
		if(level == "ERROR")	return ERROR;
		if(level == "FATAL")	return FATAL;
		std::ostringstream os;
		os << "Logging level '" << level << 
			"' not valid. Available logging levels are: 'DEBUG', 'INFO', 'WARNING', 'ERROR', 'FATAL'" 
		   << std::endl;
		throw LoggingLevelNotDefined(os.str());
	}

	static void format(std::ostream& out, const Ctx& ctx) {
		out << loggingLevelToStr(L);
	}
};

/**
 * Prints out the name of the file which is issuing the log.
 * Because it's not possible to transfer string literals via
 * template parameters, the value is read from the ctx object
 */
template <unsigned Pos>
struct FileNameSpec {
	static void format(std::ostream& out, const Ctx& ctx) {
		// Cut out the entire path and prints the file name
		std::string file_name(boost::any_cast<const char *>(ctx[Pos]));
		size_t pos = file_name.find_last_of('/');
		if(pos == std::string::npos) {
			out << file_name;
		} else {
			out << file_name.substr(pos+1);
		}
	}
};

/**
 * Report the line number which is passed as template parameter
 */
template <unsigned Line>
struct LineSpec {
	static void format(std::ostream& out, const Ctx& ctx) { out << Line; }
};

/**
 * The formater takes several specifiers, in any order and compose them
 * printing the values to the log stream, a specifier could be one of the
 * previous classes or a formatter itself. The values are separated by a
 * separator character.
 */
template <char Separator, class ...Formats>
class Formatter;

/**
 * This specialization of the Formatter deals with the situation
 * where only 1 element is on the specifier list.
 */
template <char Separator, class Spec>
struct Formatter<Separator, Spec> {

	static void format(std::ostream& out, const Ctx& ctx) {
		Spec::format(out, ctx);
	}
};

/**
 * This specialization of the Formatter deals with the general
 * case, it use template recursion to produce the list of
 * specifiers in the given order.
 */
template <char Separator, class Head, class ...Tail>
struct Formatter<Separator, Head, Tail...> {

	static void format(std::ostream& out, const Ctx& ctx) {
		Head::format(out, ctx);
		out << Separator;
		Formatter<Separator, Tail...>::format(out, ctx);
	}
};

inline void handler(int sig) {

	int j, nptrs;
#define SIZE 100
	void *buffer[100];
    char **strings;

	nptrs = backtrace(buffer, SIZE);
    fprintf(stderr, "backtrace() returned %d addresses\n", nptrs);

	/* The call backtrace_symbols_fd(buffer, nptrs, STDOUT_FILENO)
       would produce similar output to the following: */

	strings = backtrace_symbols(buffer, nptrs);
    if (strings == NULL) {
        perror("backtrace_symbols");
        exit(EXIT_FAILURE);
    }

	for (j = 0; j < nptrs; j++)
		fprintf(stderr, "%s\n", strings[j]);

	free(strings);
	exit(1);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							LOGGER
//-----------------------------------------------------------
// Logger is the main logging class, the active logger is a
// singleton and once the output stream, level and verbosity
// is initialized, cannot be changed.
//
//
class Logger {
	// null logger is used to dump logs which do not match the
	// current level.
	io::stream<io::null_sink> 	m_null_logger;
	// mutex which is shared among all the instances of writer,
	// guarantee that different threads writing into the logger
	// do not overlap
	std::recursive_mutex		mutex;

	// reference to the output stream
	std::ostream&				out;
	Level 						m_level;
	unsigned short				m_verbosity;

	Logger(std::ostream& out, const Level& level, unsigned short verbosity) :
		m_null_logger(io::null_sink()),
		out(out),
		m_level(level),
		m_verbosity(verbosity)
	{ 
#ifdef __GNUC__
		/* Register an handler for segmentation faults */
 		// signal(SIGSEGV, handler); 
#endif
	}

public:


	/**
	 * Returns a reference to the current logger. The first time the method is
	 * called, the logger is initialized with the values provided as arguments.
	 *
	 * Sequent calls to this method with different input parameters has no
	 * effect on the underlying logger, the same logger is always returned.
	 */
	static Logger& get(std::ostream& out = std::cout, const Level& level = INFO, unsigned short verbosity = 0) {
		static Logger logger(out, level, verbosity);
		return logger;
	}

	/**
	 * Updates the global logging level to the given value.
	 */
	static void setLevel(Level level, short verbosity = 0) {
		get().m_level = level; get().m_verbosity = verbosity;
	}

	// Level getters/setters
	const Level& level() const { return m_level; }
	const unsigned short& verbosity() const { return m_verbosity; }

	/**
	 * Returns a writer to the active stream.
	 */
	template <class Formatter>
	Writer getActiveStream(const Ctx& ctx) {
		Writer currStream( out, mutex );
		Formatter::format(currStream.logStream, ctx);
		return currStream;
	}

	/**
	 * Returns the stream which match the level @level.
	 */
	template <class Formatter>
	Writer getStream(const Level& level, const Ctx& ctx) {
		if(level >= m_level) {
			Writer currStream(out, mutex);
			Formatter::format(currStream.logStream, ctx);
			return currStream;
		}
		return Writer( m_null_logger, mutex );
	}

};


} // End log namespace
} // End utils namespace
} // End insieme namespace

using namespace insieme::utils::log;

// Build the context used by the formatter
#define MAKE_CONTEXT			Ctx({ boost::any((const char*)__FILE__) })

// Creates the object used to format the output of the logger.
#define MAKE_FORMAT(LEVEL) 		Formatter<' ', LevelSpec<LEVEL>, Formatter<':', FileNameSpec<0>, LineSpec<__LINE__>>>

#define LOG( LEVEL ) 			if(Logger::get().level() > LEVEL) ; \
								else (Logger::get().getActiveStream<MAKE_FORMAT(LEVEL)>( MAKE_CONTEXT ).logStream << "] ")

#define VLOG(VerbLevel)			if(VerbLevel > Logger::get().verbosity()) ; \
							 	else (Logger::get().getActiveStream<MAKE_FORMAT(DEBUG)>( MAKE_CONTEXT ).logStream << "] ")

#define VLOG_IS_ON(VerbLevel)	(VerbLevel <= Logger::get().verbosity())

#define LOG_STREAM(LEVEL) 		(Logger::get().getStream<MAKE_FORMAT(LEVEL)>(LEVEL, MAKE_CONTEXT).logStream)

