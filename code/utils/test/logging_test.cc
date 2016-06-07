/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <gtest/gtest.h>

#include "insieme/utils/logging.h"

// redirect stdout to stderr so gtest finds it
class StdoutRedirect {
	std::stringstream buffer;
	std::streambuf* old;

  public:
	StdoutRedirect() : old(std::cout.rdbuf(buffer.rdbuf())) {}

	~StdoutRedirect() {
		std::cout.rdbuf(old);
		std::cerr << buffer.str();
	}
};

TEST(Logging, EnvVariableLevel) {
	ASSERT_DEATH({
		StdoutRedirect redirect;
		SETENV_WRAPPER(LOG_LEVEL_ENV, "DEBUG", 1);
		insieme::utils::logger_details::reloadConfiguration();
		LOG(DEBUG) << "DEBUG log test";
		LOG(INFO) << "INFO log test";
		LOG(WARNING) << "WARNING log test";
		LOG(ERROR) << "ERROR log test";
		LOG(FATAL) << "FATAL log test";
	} exit(1);, "DEBUG.*INFO.*WARNING.*ERROR.*FATAL");

	ASSERT_DEATH({
		StdoutRedirect redirect;
		SETENV_WRAPPER(LOG_LEVEL_ENV, "INFO", 1);
		insieme::utils::logger_details::reloadConfiguration();
		LOG(DEBUG) << "DEBUG log test";
		LOG(INFO) << "INFO log test";
		LOG(WARNING) << "WARNING log test";
		LOG(ERROR) << "ERROR log test";
		LOG(FATAL) << "FATAL log test";
	} exit(1);, "INFO.*WARNING.*ERROR.*FATAL");


	ASSERT_DEATH({
		StdoutRedirect redirect;
		SETENV_WRAPPER(LOG_LEVEL_ENV, "WARNING", 1);
		insieme::utils::logger_details::reloadConfiguration();
		LOG(DEBUG) << "DEBUG log test";
		LOG(INFO) << "INFO log test";
		LOG(WARNING) << "WARNING log test";
		LOG(ERROR) << "ERROR log test";
		LOG(FATAL) << "FATAL log test";
	} exit(1);, "WARNING.*ERROR.*FATAL");


	ASSERT_DEATH({
		StdoutRedirect redirect;
		SETENV_WRAPPER(LOG_LEVEL_ENV, "ERROR", 1);
		insieme::utils::logger_details::reloadConfiguration();
		LOG(DEBUG) << "DEBUG log test";
		LOG(INFO) << "INFO log test";
		LOG(WARNING) << "WARNING log test";
		LOG(ERROR) << "ERROR log test";
		LOG(FATAL) << "FATAL log test";
	} exit(1);, "ERROR.*FATAL");


	ASSERT_DEATH({
		StdoutRedirect redirect;
		SETENV_WRAPPER(LOG_LEVEL_ENV, "FATAL", 1);
		insieme::utils::logger_details::reloadConfiguration();
		LOG(DEBUG) << "DEBUG log test";
		LOG(INFO) << "INFO log test";
		LOG(WARNING) << "WARNING log test";
		LOG(ERROR) << "ERROR log test";
		LOG(FATAL) << "FATAL log test";
	} exit(1);, "FATAL");
}

// single character, since otherwise regexs get cumbersome
void Alpha() {
	LOG(ERROR) << "$\n";
}
void Beta() {
	LOG(ERROR) << "%\n";
}

TEST(Logging, EnvVariableFilter) {
	ASSERT_DEATH({
		StdoutRedirect redirect;
		SETENV_WRAPPER(LOG_LEVEL_ENV, "DEBUG", 1);
		SETENV_WRAPPER(LOG_FILTER_ENV, ".*Alpha.*", 1);
		Alpha();
		Beta();
	} exit(1);, "^[^%]*$[^%]*$");

	ASSERT_DEATH({
		StdoutRedirect redirect;
		SETENV_WRAPPER(LOG_LEVEL_ENV, "DEBUG", 1);
		SETENV_WRAPPER(LOG_FILTER_ENV, ".*Beta.*", 1);
		Alpha();
		Beta();
	} exit(1);, "^[^$]*%[^$]*$");
}
