#include "insieme/core/transform/materialize.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation_utils.h"

namespace insieme {
namespace core {
namespace transform {


	TypePtr materialize(const TypePtr& type) {

		// check for null pointer
		if (!type) return type;

		// do not materialize references
		//if (lang::isReference(type)) {
		if (lang::isCppReference(type)||lang::isCppRValueReference(type)) {
			return type;
		}

		// wrap type into parameter
		IRBuilder builder(type->getNodeManager());
		return builder.refType(type);
	}


	LambdaIngredients materialize(const LambdaIngredients& in) {

		LambdaIngredients out;

		// shortcut for empty parameter list
		if (in.params.empty()) {
			out.body = in.body;
			return out;
		}

		// materialize parameters and body
		NodeManager& mgr = in.body->getNodeManager();
		IRBuilder builder(mgr);

		// replace all variables in the body by their materialized version
		insieme::utils::map::PointerMap<VariablePtr,ExpressionPtr> substitute;
		for(const auto& cur : in.params) {

			// get materialized type
			auto paramType = materialize(cur->getType());

			// create the new parameters with an extra ref
			auto newParam = builder.variable(paramType, cur->getID());

			// migrate annotations
			utils::migrateAnnotations(cur, newParam);

			// record changes
			out.params.push_back(newParam);
			substitute[cur] = builder.deref(newParam);
		}
		out.body = transform::replaceVarsGen(mgr, in.body, substitute);

		// done
		return out;
	}

} // end namespace transform
} // end namespace core
} // end namespace insieme
