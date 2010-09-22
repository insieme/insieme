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

#include "program.h"
#include "ast_visitor.h"
#include "lang_basic.h"
#include "types.h"
#include <xercesc/dom/DOM.hpp>

using namespace insieme::core;

class XmlVisitor : public ASTVisitor<void> {

public:
	std::ostream& xmlStream;

public:
	XmlVisitor(std::ostream& stream = std::cout) : xmlStream (stream) {}

	void visitGenericType(const GenericTypePtr& cur) {
		xmlStream << "<genType id = \"" << (std::size_t)&*cur << "\">\n";

		xmlStream << "\t<familyName> " << (cur->getFamilyName()).getName() << " </familyName>\n";

		// baseType
		if (const TypePtr base = cur->getBaseType()){
			xmlStream << "\t<baseType>\n";
			xmlStream << "\t\t<typePtr ref = \"" << (std::size_t)&*base << "\">\n";

			// all the edge annotations

			xmlStream << "\t\t</typePtr>\n";
			xmlStream << "\t</baseType>\n";
		}

		// typeParams
		xmlStream << "\t<typeParams>\n";
		const vector< TypePtr > param = cur->getTypeParameter();
		for(vector < TypePtr >::const_iterator iter = param.begin(); iter != param.end(); ++iter){
			xmlStream << "\t\t<typePtr ref = \"" << (std::size_t)&*(*iter) << "\">\n";
			// all the edge annotations
			xmlStream << "\t\t</typePtr>\n";
		}
		xmlStream << "\t</typeParams>\n";

		// intTypeParams
		xmlStream << "\t<intTypeParams>\n";
		const vector< IntTypeParam > intParam = cur->getIntTypeParameter();
		for(vector < IntTypeParam>::const_iterator iter = intParam.begin(); iter != intParam.end(); ++iter){
			xmlStream << "\t\t<intTypeParam type = \"";
			switch (iter->getType()) {
				case IntTypeParam::VARIABLE:
					xmlStream << "variable\" value = \"" << iter->getSymbol()<< "\">\n";
					break;
				case IntTypeParam::CONCRETE:
					xmlStream << "concrete\" value = \"" << iter->getValue()<< "\">\n";
					break;
				case IntTypeParam::INFINITE:
					xmlStream << "infinite\">\n";
					break;
				default:
					xmlStream << "Invalid parameter\">\n";
					break;
			}
		}
		xmlStream << "\t</intTypeParams>\n";
		xmlStream << "</genType>\n";
	}

	void visitExpression(const ExpressionPtr& cur) {

	}

	void visitArrayType(const ArrayTypePtr& cur) {
	}

	void visitRefType(const RefTypePtr& cur) {

	}
};
