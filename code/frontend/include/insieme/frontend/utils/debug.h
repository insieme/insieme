

#ifndef NDEBUG

#include "insieme/core/checks/full_check.h"

#define DEBUG_CHECK(expr)\
	core::checks::MessageList&& errors = core::checks::check(expr); \
	if(!errors.empty()){\
		std::cout << " ======================= " << std::endl; \
		dumpPretty(expr); \
		std::cout << errors << std::endl;\
		std::cout << " ======================= " << std::endl; \
		abort();\
	}

#define ASSERT_EQ_TYPES(exprA, exprB)\
	if((*exprA != *exprB)){\
		std::cout << " === TYPES MISSMATCH [" << __FILE__ << ":" << __LINE__ << "] ===" << std::endl; \
		dumpPretty(exprA); \
		std::cout << " vs " << std::endl; \
		dumpPretty(exprB); \
		std::cout << " ======================= " << std::endl; \
		exit(-1);\
	}


#define PRINTLOCATION(expr)\
	std::cout << utils::location(expr->getLocStart(), expr->getASTContext().getSourceManager()) << std::endl;


////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
//
#else

#define DEBUG_CHECK(expr)\
	if(0){ }

#define ASSERT_EQ_TYPES(exprA, exprB)\
	if(0){ }

#define PRINTLOCATION(expr)\
	if(0){ }



#endif
