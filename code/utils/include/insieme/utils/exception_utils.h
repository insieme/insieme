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

#include <exception>
#include <sstream>

namespace insieme {
namespace utils {

	typedef std::shared_ptr<const std::exception> ExceptionPtr;

	/**
	 * This class represent an exception which is able to preserve its exception stack.
	 * In this way it is possible to trace the stack of exception till the cause.
	 */
	struct TraceableException : std::exception {
		template <class ExTy = const std::exception>
		TraceableException(const std::string& msg, const std::string& ex_type, const std::string& file_name = "", int line_no = -1,
		                   const ExTy* sub_except = NULL) throw()
		    : msg(msg), ex_type(ex_type), file_name(file_name), line_no(line_no),
		      sub_except(sub_except ? std::make_shared<const ExTy>(*sub_except) : std::shared_ptr<const ExTy>()) {
			update_err_msg();
		}

		void setMessage(const std::string& msg) {
			if(this->msg != msg) {
				this->msg = msg;
				update_err_msg();
			}
		}

		virtual const char* what() const throw() {
			return err_msg.c_str();
		}

		virtual ~TraceableException() throw() {}

	  private:
		void update_err_msg() {
			std::ostringstream ss;
			ss << "Exception of type '" << ex_type << "' raised at location(" << file_name << ", " << line_no << ")";
			if(!msg.empty()) { ss << ":\n" << msg; }
			if(sub_except) { ss << "\nCause:\n" << sub_except->what(); }
			err_msg = ss.str();
		}

		std::string msg;
		std::string ex_type;
		std::string file_name;
		int line_no;
		std::string err_msg;

		// Thsi is a pointer to the eventual sub exception which cause this exception to be retrown
		const ExceptionPtr sub_except;
	};

	#define THROW_EXCEPTION(Type, msg, args...) throw Type(msg, #Type, __FILE__, __LINE__, static_cast<const std::exception*>(NULL), args)

	#define RETHROW_EXCEPTION(Type, sub_ex, msg, args...) throw Type(msg, #Type, __FILE__, __LINE__, &sub_ex, args)

} // end utils namespace
} // end insieme namespace
