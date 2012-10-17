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

#include "insieme/utils/printable.h"

#include "insieme/core/ir_address.h"
#include "insieme/core/datapath/datapath.h"

namespace insieme {
namespace analysis {
namespace inductive {

	/**
	 * A class used to reference a memory location in memory. At the
	 * moment the addressing is based on a pair of a expression address
	 * (the address of the expression constructing the memory location)
	 * and a data path referencing the data element relatively within
	 * the allocated data structure.
	 *
	 * TODO: this description may be generically parameterized to allow
	 * more specific descriptions (potentially) exceeding the capabilities
	 * of an address.
	 *
	 * The following would be possible:
	 *  	Pointer  			 => Context Insensitive Analysis results
	 *		Addresses 			   => Context Sensitive Analysis results
	 *		Addresses + Iteration Counter => Instance Sensitive Analysis
	 */
	class MemoryLocation : public utils::Printable {

		/**
		 * The address of the expression creating the memory location.
		 */
		core::ExpressionAddress constructor;

		/**
		 * The relative path from the root object created by the constructor
		 * to the referenced memory location.
		 */
		core::datapath::DataPath dataPath;

	public:

		/**
		 * Creates a new memory location referencing the root of the object
		 * created using the given constructor expression.
		 *
		 * @param constructor the expression used to allocate the new memory location; it has
		 * 			to be a call to the ref.var or ref.new literal!
		 */
		MemoryLocation(const core::ExpressionAddress& constructor);

		/**
		 * Creates a new memory location referencing the specified sub-element of the
		 * object created using the given constructor expression.
		 *
		 * @param constructor the expression used to allocate the new memory location; it has
		 * 			to be a call to the ref.var or ref.new literal!
		 * @param dataPath the path to the data element referenced within the addressed object
		 */
		MemoryLocation(const core::ExpressionAddress& constructor, const core::datapath::DataPath& dataPath);


		MemoryLocation member(const core::ExpressionPtr& member) const;
		MemoryLocation member(const string& name) const;
		MemoryLocation element(const core::ExpressionPtr& element) const;
		MemoryLocation element(unsigned index) const;
		MemoryLocation component(const core::LiteralPtr& component) const;
		MemoryLocation component(unsigned index) const;


	protected:

		/**
		 * Prints instances of this object to the output stream in a user-friendly format.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

	};



} // end namespace inductive
} // end namespace analysis
} // end namespace insieme
