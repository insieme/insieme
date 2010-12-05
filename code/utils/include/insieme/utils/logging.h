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
#include <boost/filesystem.hpp>

#include "insieme/utils/cmd_line_utils.h"

namespace insieme {
namespace utils {
namespace log {

namespace io = boost::iostreams;

struct Writer {
	std::ostream& logStream;

	Writer(std::ostream& out) : logStream( out ) {}

	~Writer() {
		logStream << std::endl;
		logStream.flush();
	}
};

enum Level { DEBUG, INFO, WARNING, ERROR, FATAL };

class Logger {

	io::stream<io::null_sink> 	m_null_logger;
	Writer 						m_logger;
	Writer						m_empty_logger;
	Level 						m_level;
	unsigned short				m_verbosity;

	Logger(std::ostream& out, const Level& level, unsigned short verbosity) :
		m_null_logger(io::null_sink()), m_logger(out), m_empty_logger(m_null_logger), m_level(level), m_verbosity(verbosity) { }

public:

	static Logger& get(std::ostream& out=std::cout, const Level& level=DEBUG, unsigned short verbosity=CommandLineOptions::Verbosity) {
		static Logger logger(out, level, verbosity);
		return logger;
	}

	// Level getters/setters
	const Level& level() const { return m_level; }
	Level& level() { return m_level; }

	const unsigned short& verbosity() const { return m_verbosity; }
	unsigned short& verbosity() { return m_verbosity; }

	Writer getActiveStream() { return m_logger; }

	Writer getStream(const Level& level) {
		if(level <= m_level)
			return m_logger;
		return m_empty_logger;
	}

};

} // End log namespace
} // End utils namespace
} // End insieme namespace

#define LOG_PREFIX(Level) ": [" << boost::filesystem::path(__FILE__).leaf() << ":" << __LINE__ << "] "

#define LOG(Level) 	if(insieme::utils::log::Logger::get().level() > Level) ; \
					else (insieme::utils::log::Logger::get().getActiveStream().logStream << #Level << LOG_PREFIX(Level))

#define VLOG(VerbosityLevel) if(VerbosityLevel > insieme::utils::log::Logger::get().verbosity()) ; \
							 else (insieme::utils::log::Logger::get().getActiveStream().logStream << "VLOG:" << \
									 VerbosityLevel << LOG_PREFIX(insieme::utils::log::Logger::get().level()))

#define VLOG_IS_ON(VerbosityLevel)	(VerbosityLevel <= insieme::utils::log::Logger::get().verbosity())

#define LOG_STREAM(Level) (insieme::utils::log::Logger::get().getStream(Level).logStream)

