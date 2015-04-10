/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#pragma GCC diagnostic ignored "-Wuninitialized"
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#include <clang/AST/DeclGroup.h>

#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>
#include <clang/AST/DeclTemplate.h>
#include <clang/AST/ASTContext.h>
#include <llvm/Support/Casting.h>

#include <clang/Basic/SourceLocation.h>
#include <clang/Basic/SourceManager.h>
#pragma GCC diagnostic pop

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <sstream>
#include <iostream>

namespace insieme {
namespace frontend {
namespace utils {

using namespace llvm;

#define REMOVE_SYMBOLS(str) \
{ \
		boost::replace_all(str, "<", "_"); \
		boost::replace_all(str, ">", "_"); \
		boost::replace_all(str, "::","_"); \
		boost::replace_all(str, ":","_"); \
		boost::replace_all(str, " ", "_"); \
		boost::replace_all(str, "(", "_"); \
		boost::replace_all(str, ")", "_"); \
		boost::replace_all(str, ",", "_"); \
		boost::replace_all(str, "*", "_"); \
		boost::replace_all(str, "&", "_"); \
		boost::replace_all(str, ".", "_"); \
		boost::replace_all(str, "+", "_"); \
		boost::replace_all(str, "/", "_"); \
		boost::replace_all(str, "-", "_"); \
}

namespace {
	std::string createNameForAnnon (const std::string& prefix, const clang::Decl* decl, const clang::SourceManager& sm){
		std::stringstream ss;
		ss << prefix;

		// canonicalize filename in case we refer to it from different relative locations
		std::string filename = sm.getFilename(decl->getLocStart()).str();
		boost::filesystem::path path(filename);
		path = boost::filesystem::canonical(path);

		ss << path.string();   //.getHashValue();
		ss << sm.getExpansionLineNumber (decl->getLocStart());
		ss << sm.getExpansionColumnNumber(decl->getLocStart());

		std::string name =  ss.str();
		REMOVE_SYMBOLS(name);
		boost::replace_all(name, ".", "_");  // names have full path, remove symbols
		boost::replace_all(name, "/", "_");
        boost::replace_all(name, "-", "_");

		return name;
	}
}

std::string removeSymbols(std::string& s) {
	REMOVE_SYMBOLS(s);
	boost::replace_all(s, "&", "_");
	return s;
}

/* we build a complete name for the class,
 * qualified name does not have the specific types of the spetialization
 */
std::string getNameForRecord(const clang::NamedDecl* decl, const clang::QualType& type, const clang::SourceManager& sm){

	if(decl->getNameAsString().empty()){
		return createNameForAnnon("_anonRecord", decl, sm);
	}
	std::string fullName = decl->getQualifiedNameAsString();

	if (llvm::isa<clang::ClassTemplateSpecializationDecl>(decl) &&  !llvm::isa<clang::TypedefNameDecl>(decl)) {

		std::string name = decl->getNameAsString();
		std::string typeName = type->getCanonicalTypeInternal ().getAsString();

		//fullname has the namespaces and owners, just scope
		//type name has the class name and typing
		//
		//     namespace::owner::myClass                 <= qualname
		//                class  myClass<int, type>      <= typename
		//                       myClass                 just the name, the key to happines
		//      ---------------------------------
		//     namespace::owner::myClass<int, type>      <= final name

		unsigned pos = typeName.find(name);
		boost::replace_last(fullName, name, std::string(typeName.begin()+pos, typeName.end()));
	}

	REMOVE_SYMBOLS(fullName);
	return fullName;
}


std::string buildNameForFunction(const clang::FunctionDecl* funcDecl){

	std::string name = funcDecl->getQualifiedNameAsString();
	if(const clang::CXXMethodDecl* method = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl)) {
		if (method->isVirtual()) {
			name = funcDecl->getNameAsString();
		} else if (method->getParent()) {
	            if(method->getParent()->isLambda()) {
        	        name = createNameForAnnon("lambda", method->getParent(), funcDecl->getASTContext().getSourceManager());
	            }
		} else if(method->isOverloadedOperator()) {
		//	name = funcDecl->getNameAsString();
		}
	}

	//Normally we use the function name of the member function.
    //This can be dangerous when using templated member functions.
    //In this case we have to add the return type information to
    //the name, because it is not allowed to have overloaded functions
    //that only differ by the return type.
    //FIXME: Call to external functions might not work. What should we do with templated operators?!
    //TODO clenaup early exit
    /*
    if(funcDecl->isTemplateInstantiation() && !funcDecl->isOverloadedOperator()) {
		std::string functionname = funcDecl->getNameAsString();
        std::string returnType = funcDecl->getResultType().getAsString();
        functionname.append(returnType);

		if (llvm::isa<clang::CXXMethodDecl>(funcDecl) && llvm::cast<clang::CXXMethodDecl>(funcDecl)->isConst())
			functionname.append("_c");

        utils::removeSymbols(functionname);

        return functionname;
	}
	*/

    //if we have non type template specialization args,
    //we have to modify the name (e.g. template <bool VAR>)
    if(funcDecl->getTemplateSpecializationArgs()) {
        for(unsigned int i=0; i<funcDecl->getTemplateSpecializationArgs()->size(); i++) {
            if(funcDecl->getTemplateSpecializationArgs()->get(i).getKind() == clang::TemplateArgument::ArgKind::Integral) {
                name.append(funcDecl->getTemplateSpecializationArgs()->get(i).getAsIntegral().toString(10));
            }
        }
    }

    //we need to replace right and left shift operators with a dummy
    //to avoid wrong renaming and double occurence when both operators
    //have been overloaded
	boost::algorithm::replace_last(name, "operator<<","dummyss");
	boost::algorithm::replace_last(name, "operator>>","dummygg");
	//if no shift operators found lets check for less, greater
	//and less equals, greater equals (handled in one case)
	boost::algorithm::replace_last(name, "operator<","sdummy");
	boost::algorithm::replace_last(name, "operator>","gdummy");
	// also other*symbols
	boost::algorithm::replace_last(name, "operator()","PARENdummy");
	boost::algorithm::replace_last(name, "operator/=","DIVEQdummy");
	boost::algorithm::replace_last(name, "operator-=","MINEQdummy");
	boost::algorithm::replace_last(name, "operator+=","PLUEQdummy");
	boost::algorithm::replace_last(name, "operator*=","TIMEQdummy");
	boost::algorithm::replace_last(name, "operator==","EQEQdummy");

	boost::algorithm::replace_last(name, "operator*","ASTdummy");
	boost::algorithm::replace_last(name, "operator,","COMdummy");
	boost::algorithm::replace_last(name, "operator+","PLUSdummy");
	boost::algorithm::replace_last(name, "operator-","MINUSdummy");
	boost::algorithm::replace_last(name, "operator/","DIVIDEdummy");
	boost::algorithm::replace_last(name, "operator=","EQUALSdummy");


	//if(!funcDecl->isOverloadedOperator())
	{
		// beware of spetialized functions, the type does not show off
		// check if we have template spec args otherwise seg faults may occur
		if (funcDecl->isFunctionTemplateSpecialization () && funcDecl->getTemplateSpecializationArgs()){
			for(unsigned i =0; i<funcDecl->getTemplateSpecializationArgs()->size(); ++i){
				//only add something if we have a type
				auto arg = funcDecl->getTemplateSpecializationArgs()->get(i);
				switch(arg.getKind()) {
					case clang::TemplateArgument::Expression: {
						name.append ("_"+arg.getAsExpr()->getType().getAsString());
						break;
					}
					case clang::TemplateArgument::Type: {
					    //check if the type is a lambda, this one needs special handling
					    std::string typeName;
					    if(arg.getAsType().getTypePtr()->getAsCXXRecordDecl() && arg.getAsType().getTypePtr()->getAsCXXRecordDecl()->isLambda()) {
                            typeName = createNameForAnnon("lambda", arg.getAsType().getTypePtr()->getAsCXXRecordDecl(), funcDecl->getASTContext().getSourceManager());
					    } else {
                            typeName = arg.getAsType().getAsString();
					    }
						name.append ("_"+typeName);
						break;
					}
					case clang::TemplateArgument::Null: {
						name.append ("_null");
						break;
					}
					case clang::TemplateArgument::Declaration: {
						name.append ("_"+arg.getAsDecl()->getType().getAsString());
						break;
					}
					case clang::TemplateArgument::NullPtr: {
						name.append ("_nullptr");
						break;
					}
					case clang::TemplateArgument::Integral:  {
						name.append ("_"+arg.getAsIntegral().toString(10));
						break;
					}
					case clang::TemplateArgument::Template: {
						name.append ("_"+arg.getAsTemplate().getAsTemplateDecl()->getTemplatedDecl()->getNameAsString());
						break;
					}
					case clang::TemplateArgument::TemplateExpansion: {
						//I don't know what to do here
						break;
					}
					case clang::TemplateArgument::Pack: {
						for(clang::TemplateArgument::pack_iterator it = arg.pack_begin(), end = arg.pack_end();it!=end;it++) {
							const clang::QualType& argType = (*it).getAsType();
							name.append ("_" + argType.getAsString());
						}
						break;
					}
				}
			}
		}

		if(funcDecl->isTemplateInstantiation()) {
			std::string returnType = funcDecl->getResultType().getAsString();
			name.append(returnType);
		}
	}

	if (llvm::isa<clang::CXXMethodDecl>(funcDecl) && llvm::cast<clang::CXXMethodDecl>(funcDecl)->isConst()) {
		name.append("_c");
	}

	REMOVE_SYMBOLS(name);

    //check for dummyss or dummygg and replace it with the original name
	boost::algorithm::replace_last(name, "dummyss", "operator<<");
	boost::algorithm::replace_last(name, "dummygg", "operator>>");
	//if nothing was found check for the other ones
	boost::algorithm::replace_last(name, "sdummy", "operator<");
	boost::algorithm::replace_last(name, "gdummy", "operator>");
	// and the asterisc symbol
	boost::algorithm::replace_last(name, "ASTdummy","operator*");
	boost::algorithm::replace_last(name, "COMdummy","operator,");
	boost::algorithm::replace_last(name, "MINUSdummy","operator-");
	boost::algorithm::replace_last(name, "PARENdummy", "operator()");

	boost::algorithm::replace_last(name, "PLUSdummy", "operator+");
	boost::algorithm::replace_last(name, "DIVIDEdummy", "operator/");
	boost::algorithm::replace_last(name, "EQUALSdummy", "operator=");

	boost::algorithm::replace_last(name, "DIVEQdummy", "operator/=");
	boost::algorithm::replace_last(name, "MINEQdummy", "operator-=");
	boost::algorithm::replace_last(name, "PLUEQdummy", "operator+=");
	boost::algorithm::replace_last(name, "TIMEQdummy", "operator*=");
	boost::algorithm::replace_last(name, "EQEQdummy", "operator==");


	// all done
	return name;
}

std::string buildNameForVariable(const clang::VarDecl* varDecl){
	std::string name = varDecl->getQualifiedNameAsString();
	REMOVE_SYMBOLS(name);
	return name;

}

std::string buildNameForGlobal(const clang::VarDecl* varDecl, const clang::SourceManager& sm){
	std::stringstream ss;
	ss << "_global";

	ss << sm.getFilename(varDecl->getLocStart()).str();   //.getHashValue();
	ss << sm.getExpansionLineNumber (varDecl->getLocStart());
	ss << sm.getExpansionColumnNumber(varDecl->getLocStart());
	ss << "_";
	ss << sm.getExpansionColumnNumber(varDecl->getLocEnd());

	std::string name =  ss.str();
	REMOVE_SYMBOLS(name);
	boost::replace_all(name, ".", "_");  // names have full path, remove symbols
	boost::replace_all(name, "/", "_");
    boost::replace_all(name, "-", "_");

	return name;
}


std::string buildNameForEnum(const clang::EnumDecl* enumDecl, const clang::SourceManager& sm) {
	std::stringstream ss;
	ss << "_enum";

    // A canonical path is needed to avoid different names for
    // differently specified paths ( a/b/enum.h vs a/c/../b/enum.h)
    char path[PATH_MAX];
    realpath(sm.getFilename(enumDecl->getLocStart()).str().c_str(), path);
    ss << path;

	ss << sm.getExpansionLineNumber (enumDecl->getLocStart());
	ss << sm.getExpansionColumnNumber(enumDecl->getLocStart());
	ss << "_";
	ss << sm.getExpansionColumnNumber(enumDecl->getLocEnd());

	std::string name =  ss.str();
	REMOVE_SYMBOLS(name);
	boost::replace_all(name, ".", "_");  // names have full path, remove symbols
	boost::replace_all(name, "/", "_");
    boost::replace_all(name, "-", "_");

	return name;

}

std::string buildNameForEnumConstant(const clang::EnumConstantDecl* ecd, const clang::SourceManager& sm){
	std::stringstream ss;
	ss << "_enumCtnt";

    // A canonical path is needed to avoid different names for
    // differently specified paths ( a/b/enum.h vs a/c/../b/enum.h)
    char path[PATH_MAX];
    realpath(sm.getFilename(ecd->getLocStart()).str().c_str(), path);
    ss << path;

	ss << sm.getExpansionLineNumber (ecd->getLocStart());
	ss << sm.getExpansionColumnNumber(ecd->getLocStart());
	ss << "_";
	ss << sm.getExpansionColumnNumber(ecd->getLocEnd());

	std::string name =  ss.str();
	REMOVE_SYMBOLS(name);
	boost::replace_all(name, ".", "_");  // names have full path, remove symbols
	boost::replace_all(name, "/", "_");
    boost::replace_all(name, "-", "_");
    return name;
}

} // End utils namespace
} // End frontend namespace
} // End insieme namespace
