/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once

#include <iostream>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/dump/dump.h"
#include "insieme/core/dump/annotations.h"

#include "insieme/utils/printable.h"


namespace insieme {
namespace core {
namespace dump {

namespace binary {

	/**
	* The magic number stored at the head of all encodings.
	*/
	const uint64_t MAGIC_NUMBER = 0x494e5350495245; // HEX version of INSPIRE

	/**
	 * Writes a binary encoding of the given IR node into the given output stream.
	 *
	 * @param out the stream to be writing to
	 * @param ir the code fragment to be written
	 * @param converterRegister the register of annotation converter to be used
	 */
	void dumpIR(std::ostream& out, const NodePtr& ir, const AnnotationConverterRegister& converterRegister = AnnotationConverterRegister::getDefault());

	/**
	 * Writes a binary encoding of a given IR address into the given output stream.
	 *
	 * @param out the stream to be writing to
	 * @param address the address to be written
	 * @param converterRegister the register of annotation converter to be used
	 */
	void dumpAddress(std::ostream& out, const NodeAddress& address,
	                 const AnnotationConverterRegister& converterRegister = AnnotationConverterRegister::getDefault());

	/**
	 * Writes a binary encoding of a given list of IR address into the given output stream.
	 *
	 * @param out the stream to be writing to
	 * @param addresses the addresses to be written
	 * @param converterRegister the register of annotation converter to be used
	 */
	void dumpAddresses(std::ostream& out, const vector<NodeAddress>& addresses,
	                   const AnnotationConverterRegister& converterRegister = AnnotationConverterRegister::getDefault());

	/**
	 * Restores an IR code fragment from the given input stream. For constructing
	 * the resulting nodes, the given manager will be used. In case the stream contains
	 * an illegal encoding, an InvalidEncodingException will be thrown.
	 *
	 * @param in the stream to be reading from
	 * @param manager the node manager to be used for creating nodes
	 * @param converterRegister the register of annotation converter to be used
	 * @return the resolved node
	 */
	NodePtr loadIR(std::istream& in, NodeManager& manager, const AnnotationConverterRegister& converterRegister = AnnotationConverterRegister::getDefault());

	/**
	 * Restores a node address and the associated IR constructs from the given input
	 * stream. For constructing the resulting nodes, the given manager will be used.
	 * In case the stream contains an illegal encoding, an InvalidEncodingException
	 * will be thrown.
	 *
	 * @param in the stream to be reading from
	 * @param manager the node manager to be used for creating nodes
	 * @param converterRegister the register of annotation converter to be used
	 * @return the resolved address
	 */
	NodeAddress loadAddress(std::istream& in, NodeManager& manager,
	                        const AnnotationConverterRegister& converterRegister = AnnotationConverterRegister::getDefault());

	/**
	 * Restores a list of node address and the associated IR constructs from the given
	 * input stream. For constructing the resulting nodes, the given manager will be used.
	 * In case the stream contains an illegal encoding, an InvalidEncodingException
	 * will be thrown.
	 *
	 * @param in the stream to be reading from
	 * @param manager the node manager to be used for creating nodes
	 * @param converterRegister the register of annotation converter to be used
	 * @return the resolved addresses
	 */
	vector<NodeAddress> loadAddresses(std::istream& in, NodeManager& manager,
	                                  const AnnotationConverterRegister& converterRegister = AnnotationConverterRegister::getDefault());

	/**
	 * A wrapper to be streamed into an output stream when aiming on dumping some
	 * code.
	 */
	class BinaryDump : public utils::Printable {
		/**
		 * The addresses to be dumped.
		 */
		const vector<NodeAddress> addresses;

		/**
		 * A converter register to be used for converting annotations.
		 */
		const AnnotationConverterRegister& converterRegister;

	  public:
		/**
		 * Creates a new instance dumping the given node.
		 *
		 * @param ir the node to be dumped.
		 */
		BinaryDump(const NodePtr& ir, const AnnotationConverterRegister& converterRegister = AnnotationConverterRegister::getDefault())
		    : addresses(toVector(NodeAddress(ir))), converterRegister(converterRegister) {}

		/**
		 * Creates a new instance dumping the given node address.
		 *
		 * @param address the address to be dumped.
		 */
		BinaryDump(const NodeAddress& address, const AnnotationConverterRegister& converterRegister = AnnotationConverterRegister::getDefault())
		    : addresses(toVector(address)), converterRegister(converterRegister) {}

		/**
		 * Creates a new instance dumping the given list of node addresses.
		 *
		 * @param addresses the addresses to be dumped.
		 */
		BinaryDump(const vector<NodeAddress>& addresses, const AnnotationConverterRegister& converterRegister = AnnotationConverterRegister::getDefault())
		    : addresses(addresses), converterRegister(converterRegister) {}

		/**
		 * Bridges the gap to the actual binary dump function.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			dumpAddresses(out, addresses, converterRegister);
			return out;
		}
	};

} // end namespace binary

} // end namespace dump
} // end namespace core
} // end namespace insieme
