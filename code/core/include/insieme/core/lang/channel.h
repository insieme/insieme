/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/lang/extension.h"

namespace insieme {
namespace core {
namespace lang {


	// --------------------- Extension ----------------------------

	/**
	 * An extension covering the abstract array type and all its
	 * associated operators.
	 */
	class ChannelExtension : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		ChannelExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:

		/**
		 * A generic channel type 'template'.
		 */
		LANG_EXT_TYPE(GenChannel, "channel<'a,'s>")

	  /**
	   * An abstract operator for the creation of a channel utilizing a buffer of elements.
	   * The type and size have to be fixed.
	   */
	  LANG_EXT_LITERAL(ChannelCreate, "channel_create", "(type<'a>, type<'s>) -> channel<'a,'s>")

		/**
		 * An abstract operator for releasing a channel. After releasing it, it can not be
		 * utilized any further.
		 */
		LANG_EXT_LITERAL(ChannelRelease, "channel_release", "(channel<'a,'s>) -> unit")

		/**
		 * An abstract operator sending a data element to a channel. The operation blocks
		 * if the buffer of the targeted channel is full. Accesses to channels are implicitly
		 * synchronized among threads.
		 */
		LANG_EXT_LITERAL(ChannelSend, "channel_send", "(channel<'a,'s>,'a) -> unit")

		/**
		 * An abstract operator receiving a data element form a channel. The operation blocks
		 * if the buffer of the targeted channel is empty. Accesses to channels are implicitly
		 * synchronized among threads.
		 */
		LANG_EXT_LITERAL(ChannelRecv, "channel_recv", "(channel<'a,'s>) -> 'a")

		/**
		 * An abstract operation testing whether the buffer associated to the given channel
		 * is full. This operation is non-blocking.
		 */
		LANG_EXT_LITERAL(ChannelFull, "channel_full", "(channel<'a,'s>) -> bool")

		/**
		 * An abstract operation testing whether the buffer associated to the given channel
		 * is empty. This operation is non-blocking.
		 */
		LANG_EXT_LITERAL(ChannelEmpty, "channel_empty", "(channel<'a,'s>) -> bool")

	};


	// --------------------- Utilities ----------------------------

	/**
	 * A type wrapper to handle reference types in a more user
	 * friendly way then its raw encoding.
	 */
	class ChannelType {

		/**
		 * The type of elements to be communicated through the channel's buffer.
		 */
		TypePtr elementType;

		/**
		 * The size of the channel's buffer.
		 */
		ExpressionPtr size;

		/**
		 * Creates a new instances of this class based on the given element type and size.
		 */
		ChannelType(const TypePtr& elementType, const ExpressionPtr& size) : elementType(elementType), size(size) {}

	  public:

		/**
		 * 'Parses' the given node and and tries to decode it as a channel type or expression
		 * of a channel type.
		 */
		ChannelType(const NodePtr& node);

		ChannelType(const ChannelType&) = default;
		ChannelType(ChannelType&&) = default;

		ChannelType& operator=(const ChannelType&) = default;
		ChannelType& operator=(ChannelType&&) = default;


		// --- utilities ---

		static GenericTypePtr create(const TypePtr& elementType, const ExpressionPtr& size);

		operator GenericTypePtr() const;

		// --- observers and mutators ---

		const TypePtr& getElementType() const {
			return elementType;
		}

		void setElementType(const TypePtr& type) {
			assert_true(type) << "Type must not be null!";
			elementType = type;
		}

		const ExpressionPtr& getSize() const {
			return size;
		}

		void setSize(const LiteralPtr& size) {
			this->size = size;
		}

		void setSize(const VariablePtr& size) {
			this->size = size;
		}

		bool isConstSize() const {
			return size.isa<LiteralPtr>();
		}

		bool isVariableSize() const {
			return !isConstSize();
		}
	};

	// --------------------- utilities -------------------

	bool isChannel(const NodePtr& node);

	bool isFixedSizedChannelType(const NodePtr& node);

	bool isVariableSizedChannelType(const NodePtr& node);

} // end namespace lang
} // end namespace core
} // end namespace insieme
