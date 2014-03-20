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

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#pragma GCC diagnostic ignored "-Wuninitialized"
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#include <clang/AST/DeclGroup.h>

#include <clang/AST/Decl.h>
#include <clang/AST/Expr.h>
#include <clang/AST/DeclTemplate.h>
#include <llvm/Support/Casting.h>

#include <clang/Basic/SourceLocation.h>
#include <clang/Basic/SourceManager.h>
#pragma GCC diagnostic pop

#include <boost/algorithm/string.hpp>
#include <sstream>

namespace insieme {
namespace frontend {
namespace utils {

using namespace llvm;

#define REMOVE_SYMBOLS(str) \
{ \
		boost::replace_all(str, "<", "_"); \
		boost::replace_all(str, ">", "_"); \
		boost::replace_all(str, "::","_"); \
		boost::replace_all(str, " ", "_"); \
		boost::replace_all(str, "(", "_"); \
		boost::replace_all(str, ")", "_"); \
		boost::replace_all(str, ",", "_"); \
		boost::replace_all(str, "*", "_"); \
}

std::string removeSymbols(std::string& s) {
    REMOVE_SYMBOLS(s);
    boost::replace_all(s, "&", "_");
    return s;
}

/* we build a complete name for the class,
 * qualified name does not have the specific types of the spetialization
 */
std::string getNameForRecord(const clang::NamedDecl* decl, const clang::QualType& type, bool isDefinedInSystemHeader=false){

	// beware of aliasing types, if we find a typedef, better to use the inner type
	if (llvm::dyn_cast<clang::TypedefNameDecl>(decl)){
		if (const clang::RecordType* recTy = llvm::dyn_cast<clang::RecordType>(type->getCanonicalTypeInternal().getTypePtr())){
			// unleast is anonymous.. then there is no way to use anywhere else without the typedef name
			if (!recTy->getDecl()->getNameAsString().empty()){
				return getNameForRecord(recTy->getDecl(), type->getCanonicalTypeInternal(), isDefinedInSystemHeader);
			}
		}
	}

    // hold on. if it is a cxxrecorddecl, the decl has no default name
    // and we know the type, we return the name of the qualtype
    if(const clang::RecordDecl* rec = llvm::dyn_cast<clang::RecordDecl>(decl)) {
        if(decl->getNameAsString().empty() && !rec->isAnonymousStructOrUnion())
        if(!isDefinedInSystemHeader) {
            //sometimes we have a cpp lambda, don't touch this things.
            //If it is no cpp lambda or not even a cxxrec use the normal behaviour
            if(const clang::CXXRecordDecl* cxxrec = llvm::dyn_cast<clang::CXXRecordDecl>(decl)) {
                if(!cxxrec->isLambda()) {
                    //filter out some special cases
                    //FIXME: find a smarter way to check for non
                    //anonymous structs or union that have an anonymous type
                    if( type.getUnqualifiedType().getAsString().find("<anonymous at") == std::string::npos )
                        return type.getUnqualifiedType().getAsString();
                }
            } else {
                    //FIXME: find a smarter way to check for non
                    //anonymous structs or union that have an anonymous type
                    if( type.getUnqualifiedType().getAsString().find("<anonymous at") == std::string::npos )
                        return type.getUnqualifiedType().getAsString();
            }
        }
    }

	if(decl->getNameAsString().empty()){
		// empty name, build an annonymous name for this fella
		std::stringstream ss;
		ss << "_anon";
		ss << (unsigned long long) decl;
		return ss.str();
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


std::string buildNameForFunction (const clang::FunctionDecl* funcDecl){

	std::string name = funcDecl->getQualifiedNameAsString();
	if(const clang::CXXMethodDecl* method = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl))
		if (method->isVirtual())
			name = funcDecl->getNameAsString();

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
	boost::algorithm::replace_last(name, "operator*","ASTdummy");
	boost::algorithm::replace_last(name, "operator,","COMdummy");
	boost::algorithm::replace_last(name, "operator()","PARENdummy");

	// beware of spetialized functions, the type does not show off
	// check if we have template spec args otherwise seg faults may occur
	if (funcDecl->isFunctionTemplateSpecialization () && funcDecl->getTemplateSpecializationArgs()){
		for(unsigned i =0; i<funcDecl->getTemplateSpecializationArgs()->size(); ++i){
            //only add something if we have a type
            switch(funcDecl->getTemplateSpecializationArgs()->get(i).getKind()) {
                case clang::TemplateArgument::Expression: {
                    name.append ("_" +
                        funcDecl->getTemplateSpecializationArgs()->get(i).getAsExpr()->getType().getAsString());
                    break;
                }
                case clang::TemplateArgument::Type: {
                    name.append ("_" + funcDecl->getTemplateSpecializationArgs()->get(i).getAsType().getAsString());
                    break;
                }
                case clang::TemplateArgument::Null: {
                    name.append ("_null");
                    break;
                }
                case clang::TemplateArgument::Declaration: {
                    name.append ("_" +
                        funcDecl->getTemplateSpecializationArgs()->get(i).getAsDecl()->getType().getAsString());
                        break;
                }
                case clang::TemplateArgument::NullPtr: {
                    name.append ("_nullptr");
                    break;
                }
                case clang::TemplateArgument::Integral:  {
                    name.append ("_" + funcDecl->getTemplateSpecializationArgs()->get(i).getAsIntegral().toString(10));
                    break;
                }
                case clang::TemplateArgument::Template: {
                    name.append ("_" +
                        funcDecl->getTemplateSpecializationArgs()->get(i).getAsTemplate().getAsTemplateDecl()->getTemplatedDecl()->getNameAsString());
                }
                case clang::TemplateArgument::TemplateExpansion: {
                    //I don't know what to do here
                    break;
                }
                case clang::TemplateArgument::Pack: {

					for(clang::TemplateArgument::pack_iterator it = funcDecl->getTemplateSpecializationArgs()->get(i).pack_begin(), end = funcDecl->getTemplateSpecializationArgs()->get(i).pack_end();it!=end;it++) {
						const clang::QualType& argType = (*it).getAsType();
                    name.append ("_" +
                    	argType.getAsString());
                        //(*it).getAsExpr()->getType().getAsString());
					}
                    break;
                }
            }
		}
	}

	REMOVE_SYMBOLS(name);

	if (llvm::isa<clang::CXXMethodDecl>(funcDecl) && llvm::cast<clang::CXXMethodDecl>(funcDecl)->isConst())
		name.append("_c");

    //check for dummyss or dummygg and replace it with the original name
	boost::algorithm::replace_last(name, "dummyss", "operator<<");
	boost::algorithm::replace_last(name, "dummygg", "operator>>");
	//if nothing was found check for the other ones
	boost::algorithm::replace_last(name, "sdummy", "operator<");
	boost::algorithm::replace_last(name, "gdummy", "operator>");
	// and the asterisc symbol
	boost::algorithm::replace_last(name, "ASTdummy","operator*");
	boost::algorithm::replace_last(name, "COMdummy","operator-");
	boost::algorithm::replace_last(name, "PARENdummy", "operator()");

	// all done
	return name;
}

std::string buildNameForVariable  (const clang::VarDecl* varDecl){
	std::string name = varDecl->getQualifiedNameAsString();
	REMOVE_SYMBOLS(name);
	return name;

}

std::string buildNameForGlobal (const clang::VarDecl* varDecl, const clang::SourceManager& sm){
	std::stringstream ss;
	ss << "_global";

	ss << sm.getFilename(varDecl->getLocStart()).str();   //.getHashValue();
	ss << sm.getExpansionLineNumber (varDecl->getLocStart());
	ss << sm.getExpansionColumnNumber(varDecl->getLocStart());

	std::string name =  ss.str();
	REMOVE_SYMBOLS(name);
	boost::replace_all(name, ".", "_");  // names have full path, remove symbols
	boost::replace_all(name, "/", "_");
    boost::replace_all(name, "-", "_");

	return name;
}


std::string buildNameForEnum (const clang::EnumDecl* enumDecl, const clang::SourceManager& sm) {
    std::string name = enumDecl->getQualifiedNameAsString();
    //std::string name = type->getDecl()->getNameAsString();
	REMOVE_SYMBOLS(name);
    if(name.empty() || name == "_anonymous_") {   // clang 3.4 might return _annonymous_ instad of empty
		std::stringstream ss;
		ss << "_anonEnum";

		ss << sm.getFilename(enumDecl->getLocStart()).str();   //.getHashValue();
		ss << sm.getExpansionLineNumber (enumDecl->getLocStart());
		ss << sm.getExpansionColumnNumber(enumDecl->getLocStart());

		name =  ss.str();
		REMOVE_SYMBOLS(name);
		boost::replace_all(name, ".", "_");  // names have full path, remove symbols
		boost::replace_all(name, "/", "_");
        boost::replace_all(name, "-", "_");
    }
    return name;
}

std::string buildNameForEnumConstant(const clang::EnumConstantDecl* ecd) {
    std::string name = "__insieme_enum_constant_" + ecd->getQualifiedNameAsString();
	REMOVE_SYMBOLS(name);
    assert(!name.empty() && "what kind of enumconstant has no name?");
    return name;
}

} // End utils namespace
} // End frontend namespace
} // End insieme namespace
