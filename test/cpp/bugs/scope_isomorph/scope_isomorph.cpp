
//
//  main: frontend/include/insieme/frontend/tu/ir_translation_unit.h:140: void
//  insieme::frontend::tu::IRTranslationUnit::addType(const GenericTypePtr&, const TypePtr&): Assertion `types.find(symbol) == types.end()' failed.
//
//
//	the second type is like the first one, the translation unit does not like to add it again to the set of types
//


int main (){

	{
struct obj{
	int memb;
};
		typedef struct obj obja;
		obja a;
	}

	{
struct obj{
	int memb;
};
		typedef struct obj obja;
		obja a;
	}


	return 0;
}
