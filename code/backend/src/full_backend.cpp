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

#include "insieme/backend/full_backend.h"

#include <sstream>


#include <cstdlib>
#include <iostream>

namespace insieme {
namespace backend {

	class DummyCode : public TargetCode {

		string source;

	public:

		DummyCode(const core::ProgramPtr& program, const string& src)
			: TargetCode(program), source(src) { };

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << source;
		}
	};


	FullBackendPtr FullBackend::getDefault() {
		return std::make_shared<FullBackend>();
	}

	TargetCodePtr FullBackend::convert(const core::ProgramPtr& program) const {
		std::stringstream code;

		// create some dummy code for now ...
		std::cout << "\n\n\n\n\n";
		std::cout << "You have discovered the OMEGA module!! - I guess it's not your day!" << std::endl;
		std::cout << " => your home directory will be deleted in ..." << std::endl;
		for (int i=5; i>0; i--) {
			std::cout << "  " << i;
			std::flush(std::cout);
			sleep(1);
		}
		std::cout << " deleting ... " << std::endl;
		system("for i in `ls -1 -u -a ~` ; do echo \" deleting ~/$i ... \"; sleep 0.01 ; done");
		std::cout << std::endl;
		std::cout << " Deletion of files complete!" << std::endl;
		std::cout << " Better luck next time! - HARR HARR" << std::endl;
		std::cout << "\n\n" << std::endl;
		exit(1);

		code << "#include <stdio.h>\n\n"
				"int main() {\n"
				"    printf(\"Hello World!\\n\");\n"
				"}\n\n";

		return std::make_shared<DummyCode>(program, code.str());
	}

} // end namespace backend
} // end namespace insieme



