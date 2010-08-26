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

#include <ostream>

#include <exception>
#include <stdexcept>

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/iostreams/categories.hpp>
#include <boost/iostreams/concepts.hpp>
#include <boost/iostreams/stream.hpp>

#include <boost/thread/mutex.hpp>
#include <boost/thread/recursive_mutex.hpp>

#include <boost/shared_ptr.hpp>

namespace insieme {
namespace utils {
namespace log {

/*! \brief An interface for stream writer.
 *
 *  Two stream writers are needed for the logger. The normal one that writes charatters
 *  in the output stream (WriterImpl), and a fake one that trashes all the characters
 * 	(NullWriter).
 */
struct Writer: public boost::iostreams::sink {
	virtual std::streamsize write(const char* s, std::streamsize n) = 0;
	virtual ~Writer(){}
};

/*! \brief NullWriter trashes all the character.
 *
 *  NullWriter is used when the current log level is higher than the requested log level.
 *  For example when the debug() method of the Logger class is called and the current
 *  log level is set to Info.
 */
struct NullWriter: public Writer {
	NullWriter() { }
	std::streamsize write(const char* s, std::streamsize n) { return 0; }
};

/*! \brief WriterImpl sends the log message to the underlying output stream.
 *
 *  The message is sent to the underlying output stream associated to the logger instance.
 *  A mutex is used in order to have multi-threading safe library.
 */
class WriterImpl: public Writer {
	std::ostream& out;
	boost::recursive_mutex::scoped_lock* lock;
public:
	WriterImpl(std::ostream& out, boost::recursive_mutex& m): out(out) {
		lock = new boost::recursive_mutex::scoped_lock(m); /* acquire the lock */
	}

	std::streamsize write(const char* s, std::streamsize n) {
		out.write(s, n); /* sends characters to the underlying output stream */
		return n;
	}

	~WriterImpl() {
		out.flush();
		delete lock; /* release the lock */
	}
};

typedef boost::reference_wrapper<Writer> writer_ref;

/*! \brief The Wrapper is a temporary object that keeps lines consistency.
 *
 *  Wrapper allows the use of the '<<' operator and assures lines consistency in a
 *  multi-threading environment.
 */
struct Wrapper: public boost::iostreams::stream<writer_ref> {
	boost::shared_ptr<Writer> writer;

	Wrapper(Writer *w): writer(boost::shared_ptr<Writer>(w)) { open( boost::ref(*w) ); }
	Wrapper(Wrapper const& other): writer(other.writer) { open(boost::ref( *(other.writer.get()) )); }
};

/*! \brief The logging levels.
 *
 *  Four logging levels are allowed.
 */
enum Level{ Debug, Info, Warn, Error, Fatal };

struct LoggingLevelNotDefined: public std::runtime_error {
	LoggingLevelNotDefined(const std::string& message): std::runtime_error(message){}
};

Level getLoggingLevelFromStr(std::string level){
	if(level.empty())		return Info; // default loggin level
	if(level == "Debug")	return Debug;
	if(level == "Info")		return Info;
	if(level == "Warn")		return Warn;
	if(level == "Error")	return Error;
	if(level == "Fatal")	return Fatal;
	std::ostringstream os;
	os << "Logging level '" << level << "' not valid. Available logging levels are: 'Debug', 'Info', 'Warning', 'Error', 'Fatal'" << std::endl;
	throw LoggingLevelNotDefined(os.str());
}

std::string getLoggingLevelFromEnum(Level l) {
	switch(l) {
		case Debug:	return "Debug";
		case Info: return "Info";
		case Warn: return "Warn";
		case Error: return "Error";
		case Fatal: return "Fatal";
	}
	return std::string();
}

class Logger {
public:

	static Logger& get(std::ostream &o = std::cout, std::ostream &e = std::cerr, Level level = Debug) {
		static Logger theLogger(o, e, level);
		return theLogger;
	}

	Level level() const { return mLevel; }

	template <Level L>
	Wrapper writer() {
		if(mLevel <= L)
			return create_wrapper(getLoggingLevelFromEnum(L), mOut);
		return *null_wrapper;
	}

private:
	std::ostream 			&mOut, &mErr;
	Level 					mLevel;
	boost::recursive_mutex	mMux;

	Wrapper*				null_wrapper;

	Logger(std::ostream& out, std::ostream& err, Level level): mOut(out), mErr(err), mLevel(level), null_wrapper( new Wrapper(new NullWriter) ) {  }

	Wrapper create_wrapper(std::string level, std::ostream& out) {
		Wrapper w( new WriterImpl(out, mMux) );
		boost::posix_time::ptime now = boost::posix_time::second_clock::local_time();
		w << level << " " <<
			// boost::this_thread::get_id() <<
			" ["<< now << "] ";
		return w;
	}
};

#define DEBUG(text) \
	do { if( insieme::utils::log::Logger::get().level() == insieme::utils::log::Debug) \
		insieme::utils::log::Logger::get().writer<insieme::utils::log::Debug>() << text << std::endl; } while(false);

#define INFO(text) \
	do { if( insieme::utils::log::Logger::get().level() <= insieme::utils::log::Info) \
		insieme::utils::log::Logger::get().writer<insieme::utils::log::Info>() << text << std::endl; } while(false);

#define WARN(text) \
	do { if( insieme::utils::log::Logger::get().level() <= insieme::utils::log::Warn) \
		insieme::utils::log::Logger::get().writer<insieme::utils::log::Warn>() << text << std::endl; } while(false);

#define ERROR(text) \
	do { if( insieme::utils::log::Logger::get().level() <= insieme::utils::log::Error) \
		insieme::utils::log::Logger::get().writer<insieme::utils::log::Error>() << text << std::endl; } while(false);

#define FATAL(text) \
	do { if( insieme::utils::log::Logger::get().level() <= insieme::utils::log::Fatal) \
		insieme::utils::log::Logger::get().writer<insieme::utils::log::Fatal>() << text << std::endl; assert(false && "Fatal error occurred.") } while(false);

} // End log namespace
} // End utils namespace
} // End insieme namespace
