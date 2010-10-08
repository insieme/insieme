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
#include "pragma_handler.h"
#include <memory>

namespace insieme {
namespace frontend {

namespace conversion {
class ConversionFactory;
}

namespace omp {

namespace annotation {
// forward declaration
class OmpAnnotation;
typedef std::shared_ptr<OmpAnnotation> OmpAnnotationPtr;

}

/**
 * Registers the handlers for OpenMP pragmas
 */
void registerPragmaHandlers(clang::Preprocessor& pp);

void attachOmpAnnotation(const core::NodePtr& irNode, const clang::Stmt* clangNode, conversion::ConversionFactory& fact);

namespace pragma {

class OmpPragma: public insieme::frontend::Pragma {

protected:
	insieme::frontend::MatchMap mMap;
public:
	OmpPragma(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap);

	/**
	 * Converts the pragma into an annotation which will be attached to the IR.
	 */
	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const = 0;

	const insieme::frontend::MatchMap& getMap() const { return mMap; }
};

class OmpParallel: public OmpPragma {
public:
	OmpParallel(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpFor: public OmpPragma {
public:
	OmpFor(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpSections: public OmpPragma {
public:
	OmpSections(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpSection: public OmpPragma {
public:
	OmpSection(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpSingle: public OmpPragma {
public:
	OmpSingle(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpTask: public OmpPragma {
public:
	OmpTask(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpMaster: public OmpPragma {
public:
	OmpMaster(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpCritical: public OmpPragma {
public:
	OmpCritical(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpBarrier: public OmpPragma {
public:
	OmpBarrier(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpTaskWait: public OmpPragma {
public:
	OmpTaskWait(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpAtomic: public OmpPragma {
public:
	OmpAtomic(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpFlush: public OmpPragma {
public:
	OmpFlush(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpOrdered: public OmpPragma {
public:
	OmpOrdered(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

class OmpThreadPrivate: public OmpPragma {
public:
	OmpThreadPrivate(const clang::SourceLocation& startLoc, const clang::SourceLocation& endLoc, const std::string& name, const insieme::frontend::MatchMap& mmap):
		OmpPragma(startLoc, endLoc, name, mmap) { }

	virtual omp::annotation::OmpAnnotationPtr toAnnotation(conversion::ConversionFactory& fact) const;
};

} // Endl pragma namespace
} // End omp namespace
} // End frontend namespace
} // End insieme namespace
