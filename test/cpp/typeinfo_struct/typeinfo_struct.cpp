// comparing type_info objects
#include <iostream>   // std::cout
#include <typeinfo>   // operator typeid

struct Base {};
struct Derived : Base {};
struct Poly_Base {virtual void Member(){}};
struct Poly_Derived: Poly_Base {};

typedef int my_int_type;

const std::type_info& getType(){
	return typeid(void);
}

int main() {
  std::cout << std::boolalpha;

  // fundamental types:
  std::cout << "int vs my_int_type: ";
  std::cout << ( typeid(int) == typeid(my_int_type) ) << '\n';

  // class types:
  std::cout << "Base vs Derived: ";
  std::cout << ( typeid(Base)==typeid(Derived) ) << '\n';

  // non-polymorphic object:
  Base* pbase = new Derived;

  std::cout << "Base vs *pbase: ";
  std::cout << ( typeid(Base)==typeid(*pbase) ) << '\n';

  // polymorphic object:
  Poly_Base* ppolybase = new Poly_Derived;

  std::cout << "Poly_Base vs *ppolybase: ";
  std::cout << ( typeid(Poly_Base)==typeid(*ppolybase) ) << '\n';

  // simple case
  int a;
  float b;
  std::cout << ( typeid(a)==typeid(b) ) << '\n';  

	// type literals
  { 
  	typeid(void);
  	std::cout << ( typeid(int)!=typeid(long) ) << std::endl;  
  	std::cout << ( typeid(void) == getType() );  
  }

  return 0;
}
