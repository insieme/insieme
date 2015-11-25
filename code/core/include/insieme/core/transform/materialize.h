#pragma once

#include "insieme/core/ir.h"

namespace insieme {
namespace core {
namespace transform {

	TypePtr materialize(const TypePtr&);

	struct LambdaIngredients {
		VariableList params;
		StatementPtr body;
	};

	LambdaIngredients materialize(const LambdaIngredients&);

} // end namespace transform
} // end namespace core
} // end namespace insieme
