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

#include <iostream>
#include <memory>
#include "insieme/transform/parameter.h"

namespace insieme {
namespace transform {
namespace parameter {

	const Value emptyValue = combineValues();

	const TupleParameterPtr no_parameters = tuple();


	bool Parameter::isAtomic() const {
		return dynamic_cast<const AtomicParameter*>(this);
	}


	bool TupleParameter::isValid(const Value& value) const {
		// test whether it is a list of values
		if (!boost::apply_visitor(is_type_of<vector<Value>>(), value)) {
			return false;
		}

		// test type of values within list
		const vector<Value>& values = boost::get<vector<Value>>(value);
		const vector<ParameterPtr>& params = getComponents();
		if (values.size() != params.size()) {
			return false;
		}

		auto paired = make_paired_range(values, params);
		return all(paired.first, paired.second, [](const std::pair<Value, ParameterPtr>& cur) {
			return cur.second->isValid(cur.first);
		});
	}

	bool ListParameter::isValid(const Value& value) const {
		// test whether it is a list of values
		if (!boost::apply_visitor(is_type_of<vector<Value>>(), value)) {
			return false;
		}

		// test type of values within list
		const vector<Value>& values = boost::get<vector<Value>>(value);
		const ParameterPtr& elementType = getElementType();

		return all(values, [&](const Value& cur) {
			return elementType->isValid(cur);
		});
	}

	namespace {

		void printFormated(std::ostream& out, const ParameterPtr& ptr, unsigned indent) {
			const int column = 20;

			string desc = ptr->getDescription();
			if (!desc.empty()) {
				desc = " ... " + desc;
			}

			if (AtomicParameterPtr atomic = std::dynamic_pointer_cast<AtomicParameter>(ptr)) {
				const string& name = atomic->getTypeName();
				out << times(" ", indent) << name << times(" ", column - indent - name.length()) << desc << "\n";
				return;
			}

			if (TupleParameterPtr tuple = std::dynamic_pointer_cast<TupleParameter>(ptr)) {
				out << times(" ", indent) << "( \n";
				for_each(ptr->getComponents(), [&](const ParameterPtr& cur) {
					printFormated(out, cur, indent + 4);
				});
				out << times(" ", indent) << ") " << times(" ", column - indent - 2) << desc << "\n";
				return;
			}

			if (ListParameterPtr list = std::dynamic_pointer_cast<ListParameter>(ptr)) {
				out << times(" ", indent) << "[ \n";
				for_each(ptr->getComponents(), [&](const ParameterPtr& cur) {
					printFormated(out, cur, indent + 4);
				});
				out << times(" ", indent) << "]* " << times(" ", column - indent - 3) << desc << "\n";
				return;
			}


			std::cout << "ERROR: unexpeted parameter type: " << *ptr << "\n";
			assert(false && "Unexpected Parameter type encountered!");
		}

	}


	std::ostream& InfoPrinter::printTo(std::ostream& out) const {
		out << "Parameters: \n";
		printFormated(out, parameter, 4);
		out << "\n";
		return out;
	}

} // end namespace parameter
} // end namespace transform
} // end namespace insieme
