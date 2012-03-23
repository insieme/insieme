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

#include "insieme/core/dump/binary_dump.h"

#include <inttypes.h>
#include <limits>
#include <map>
#include <vector>

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_builder.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace core {
namespace dump {

	namespace binary {

		// The binary format of the IR dump:
		//		<MAGIC_NUMBER><NUM_NODES><NODE>+
		//
		// where the magic number is used to identify data streams,
		// the number of nodes is recording the number of contained nodes
		// and the list of nodes is containing the actual encoding of the
		// nodes.
		// Each node encoding starts with a type_t type (= NodeType). In case
		// it is a value node, it is followed by the encoding of its value.
		// for strings, the encoding consists of the string length followed
		// by a character array (not null-terminated).
		// All other nodes are encoded using the type, the number of children
		// and the list of indices of children.
		// Within the binary file, every node of the DAG is only stored once
		// and referenced via its index.


		namespace {

			/**
			 * The magic number stored at the head of all encodings.
			 */
			const uint64_t MAGIC_NUMBER = 0x494e5350495245; // HEX version of INSPIRE

			// some type definitions
			typedef uint16_t length_t;
			typedef uint16_t type_t;
			typedef uint16_t index_t;

			// some convenience utilities
			template<typename T>
			void write(std::ostream& out, T value) {
				out.write((char*)&value, sizeof(T));
			}

			template<typename T>
			T read(std::istream& in) {
				T value = 0;
				in.read((char*)&value, sizeof(T));
				return value;
			}


			// -- writer --

			/**
			 * A static visitor used to store node values within a stream.
			 */
			struct ValueDumper : public boost::static_visitor<void> {

				std::ostream& out;
				ValueDumper(std::ostream& out) : out(out) {}

				void operator()(bool value) const {
					write<uint8_t>(out, (value)?1:0);
				}

				void operator()(char value) const {
					write<uint8_t>(out, value);
				}

				void operator()(int value) const {
					write<int32_t>(out, value);
				}

				void operator()(unsigned value) const {
					write<uint32_t>(out, value);
				}

				void operator()(const string& value) const {
					assert(false && "Should not be handled this way!");
				}
			};

			/**
			 * The binary dumper is converting the DAG representing the
			 * code fragment to be dumped into a list such that the root
			 * node is stored at position 1 (index 0). The list
			 * is then written into the output stream using a binary format.
			 */
			class BinaryDumper {

				/**
				 * The list of all nodes to be dumped.
				 */
				vector<NodePtr> nodeList;

				/**
				 * An reverse index, assigning every node its index
				 * within the node list.
				 */
				utils::map::PointerMap<NodePtr, index_t> index;

			public:

				/**
				 * Dumps the given ir code fragment to the given output stream.
				 */
				void dump(std::ostream& out, const NodePtr& ir) {

					// create node list and index
					createIndex(ir);

					// write magic number
					write(out, MAGIC_NUMBER);

					// write number of nodes
					write<index_t>(out, nodeList.size());

					// dump nodes
					for_each(nodeList, [&](const NodePtr& cur) {
						dumpNode(out, cur);
					});
				}

			private:

				/**
				 * Dumps a single node into the output stream.
				 */
				void dumpNode(std::ostream& out, const NodePtr& node) {

					// write type (for all nodes this is the same)
					type_t type = node->getNodeType();
					write(out, type);

					// check whether it is a string value
					if (type == NT_StringValue) {
						const string& str = static_pointer_cast<StringValuePtr>(node)->getValue();

						// write the string content
						write<length_t>(out, str.length());

						// write string (not including \0)
						out.write(str.c_str(), str.length());
						return;
					}

					// check whether it is a value => special handling
					if (node->isValue()) {
						// dump content
						boost::apply_visitor(ValueDumper(out), node->getNodeValue());
						return;
					}

					// all other nodes are handled the same way

					// handle standard node
					const NodeList& children = node->getChildList();

					// write length of child list
					write<length_t>(out, children.size());

					// write child list
					for_each(children, [&](const core::NodePtr& cur) {
						auto pos = index.find(cur);
						assert(pos != index.end() && "Index not correctly established!");
						write<index_t>(out, pos->second);
					});
				}

				/**
				 * Creates the node index by enlisting all nodes within the IR DAG
				 * within the node list and the reverse lookup table (index).
				 */
				void createIndex(const NodePtr& ir) {
					// index all nodes
					visitDepthFirstOnce(ir, [&](const NodePtr& cur) {
						// check whether index has been assigned before
						auto pos = index.find(cur);
						if (pos != index.end()) {
							return;
						}
						index[cur] = (index_t)nodeList.size();
						nodeList.push_back(cur);
					});

					// check whether index limit is sufficient
					assert(nodeList.size() < std::numeric_limits<index_t>::max()
							&& "Number of nodes is exceeding index limit!");
				}
			};

			/**
			 * The binary loader is restoring an IR structure from a binary input stream.
			 */
			class BinaryLoader {

				/**
				 * The builder used to construct nodes.
				 */
				IRBuilder builder;

				/**
				 * A list of node skeletons extracted while passing through
				 * the the file.
				 */
				vector<pair<NodeType, vector<index_t>>> nodes;

				/**
				 * The index of all resolved nodes.
				 */
				vector<NodePtr> index;

			public:

				BinaryLoader(NodeManager& manager) : builder(manager) {}

				NodePtr load(std::istream& in) {

					// check magic number
					if (read<uint64_t>(in) != MAGIC_NUMBER) {
						throw InvalidEncodingException("Encoding error: wrong magic number!");
					}

					// load index
					loadIndex(in);

					// restore nodes
					return resolve(0); // root node has always index 0
				}

			private:

				void loadIndex(std::istream& in) {

					// load number of nodes
					index_t numNodes;
					in.read((char*)&numNodes, sizeof(numNodes));

					// allocate sufficient space
					nodes.resize(numNodes);
					index.resize(numNodes);

					// resolve nodes
					for(index_t i=0; i<numNodes; i++) {
						loadNode(i, in);
					}
				}

				void loadNode(index_t pos, std::istream& in) {

					// load node type
					type_t type;
					in.read((char*)&type, sizeof(type));

					if (type == NT_StringValue) {
						// load string
						length_t length = read<length_t>(in);

						// load string
						char data[length+1];
						in.read(data, length);
						data[length] = '\0';

						// register string value
						index[pos] = builder.stringValue(string(data));
						return;
					}


					// handle value node
					if (type == NT_BoolValue || type == NT_CharValue
							|| type == NT_IntValue || type == NT_UIntValue) {

						ValuePtr value;
						if (type == NT_BoolValue) {
							value = builder.boolValue(bool(read<uint8_t>(in) != 0));
						} else if (type == NT_CharValue) {
							value = builder.charValue(char(read<uint8_t>(in)));
						} else if (type == NT_UIntValue) {
							value = builder.uintValue(unsigned(read<uint32_t>(in)));
						} else if (type == NT_IntValue) {
							value = builder.intValue(int(read<int32_t>(in)));
						} else {
							assert(false && "Inconsistent code!");
						}

						// register value
						index[pos] = value;
						return;
					}

					// restore all other nodes
					length_t length = read<length_t>(in);

					vector<index_t> children;
					for(length_t i=0; i<length; i++) {
						children.push_back(read<index_t>(in));
					}

					// register node value
					nodes[pos] = std::make_pair(NodeType(type), children);
				}

				NodePtr resolve(index_t pos) {

					// check whether node has been resolved before
					if (index[pos]) {
						return index[pos];
					}

					// resolve child list
					NodeList children = ::transform(nodes[pos].second, fun(*this, &BinaryLoader::resolve));
					NodePtr res = builder.get(nodes[pos].first, children);

					// remember newly resolved node
					index[pos] = res;
					return res;
				}
			};

			void dumpPathIndices(std::ostream& out, const Path& path) {
				// dump in order
				if (path.getLength() > 2) {
					dumpPathIndices(out, path.getPathToParent());
				}
				write<index_t>(out, path.getIndex());
			}

			void dumpPath(std::ostream& out, const Path& path) {
				// start with the length
				write<length_t>(out, path.getLength() - 1);

				// dump path
				dumpPathIndices(out, path);
			}

		}

		void dumpIR(std::ostream& out, const NodePtr& ir) {
			BinaryDumper().dump(out, ir);
		}

		void dumpAddress(std::ostream& out, const NodeAddress& address) {
			dumpAddresses(out, toVector(address));
		}

		void dumpAddresses(std::ostream& out, const vector<NodeAddress>& addresses) {
			assert(!addresses.empty() && "Cannot dump empty list of addresses!");

			// just dump full IR tree ...
			dumpIR(out, addresses[0].getRootNode());

			// .. followed by the address paths
			for_each(addresses, [&](const NodeAddress& cur) {
				dumpPath(out, cur.getPath());
			});
		}


		NodePtr loadIR(std::istream& in, core::NodeManager& manager) {
			return BinaryLoader(manager).load(in);
		}

		NodeAddress loadAddress(std::istream& in, NodeManager& manager) {
			vector<NodeAddress> list = loadAddresses(in, manager);
			assert(!list.empty() && "Resolved address list must not be empty!");
			return list[0];
		}

		vector<NodeAddress> loadAddresses(std::istream& in, NodeManager& manager) {
			// first, load tree
			NodePtr root = loadIR(in, manager);

			// check whether there are any addresses.
			if (in.eof()) {
				in.clear();
				return toVector(NodeAddress(root));
			}
			// load paths
			vector<NodeAddress> res;
			while(!in.eof()) {
				length_t length = read<length_t>(in);

				if(length == 0) {
					if(res.size() > 0)
						return res;
					else
						return toVector(NodeAddress(root));
				}

				if (!in.eof()) {
					NodeAddress cur(root);
					for(length_t i=0; i<length; i++) {
						cur = cur.getAddressOfChild(read<index_t>(in));
					}
					res.push_back(cur);
				}
			}
			in.clear();
			return res;
		}

	} // end namespace binary

} // end namespace dump
} // end namespace core
} // end namespace insieme
