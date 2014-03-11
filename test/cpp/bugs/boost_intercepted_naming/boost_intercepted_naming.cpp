#include <iostream>
#include <string>
#include <boost/shared_ptr.hpp>

/** 
 * TEST1: 
 * our test class derives from an class
 */
class Test0 {
	class TestObj : std::string {
		std::string val2;
	public:
		TestObj() : val2("0") { std::cout << "Create object (implicit calls)" << std::endl; }
		TestObj(std::string val) : std::string(), val2(val) { std::cout << "Create object (explicit calls)" << std::endl; };
		void print() {
			std::cout << "Name: " << val2 << std::endl;
		}		
	};

	public:
		void test() {
			Test0::TestObj t1("TestObj:test");
			Test0::TestObj t2;
			t1.print();
			t2.print();
		}	
};

/** 
 * TEST1: 
 * our test class derives from an intercepted class that takes template arguments.
 * Therefore we have a class Test that derives from boost::shared_ptr<int>
 * Check if this is translated in the correct way, and if the ctor calls to base
 * classes are done. Implicit calls and explicit calls.
 */
class Test1 {
	class TestObj : boost::shared_ptr<int > {
	private:
		std::string name;
	public:
		TestObj() : name("TestObj:fu") { std::cout << "Create object (implicit calls)" << std::endl; }
		TestObj(std::string _name) : boost::shared_ptr<int >(), name(_name) { std::cout << "Create object (explicit calls)" << std::endl; };
		void print() {
			std::cout << "Name: " << name << std::endl;
		}
	};

	public:
		void test() {
			Test1::TestObj t1("TestObj:test");
			Test1::TestObj t2;
			t1.print();
			t2.print();
		}

};

/** 
 * TEST2: 
 * our test class derives from an intercepted class that takes template arguments.
 * Therefore we have a class Test that derives from boost::shared_ptr<own type>
 * Check if this is translated in the correct way, and if the ctor calls to base
 * classes are done. Implicit calls and explicit calls.
 */
namespace Inner {
class XUT {

};
};

class Test2 {

	class TestObj : boost::shared_ptr<Inner::XUT> {
	private:
		std::string name;
	public:
		TestObj() : name("TestObj:fu") { std::cout << "Create object (implicit calls)" << std::endl; }
		TestObj(std::string _name) : boost::shared_ptr<Inner::XUT >(), name(_name) { std::cout << "Create object (explicit calls)" << std::endl; };
		void print() {
			std::cout << "Name: " << name << std::endl;
		}
	};

	public:
		void test() {
			Test2::TestObj t1("TestObj:test");
			Test2::TestObj t2;
			t1.print();
			t2.print();
		}

};

/** 
 * TEST3: 
 * our test class derives from an intercepted class that takes template arguments.
 * Therefore we have a class Test that derives from boost::shared_ptr<own type<boost::shared_ptr<int>>>
 * Check if this is translated in the correct way, and if the ctor calls to base
 * classes are done. Implicit calls and explicit calls.
 */
enum Z {
 A=0
};

namespace Inner {
	template <typename T>
	class XT {
		T element;
	public:
		void getSize() {
			std::cout << "Inner::XT<typename T> sizeof:" << sizeof(T) << std::endl;
		}
	};
};

class Test3 {

	class TestObj : boost::shared_ptr<Inner::XT<boost::shared_ptr<int> > > {
	private:
		std::string name;
		Z z;
		int intz;
	public:
		TestObj() : intz(0), z(A), name("TestObj:fu") { std::cout << "Create object (implicit calls)" << std::endl; }
		TestObj(std::string _name) : boost::shared_ptr<Inner::XT<boost::shared_ptr<int> > >(), name(_name) { std::cout << "Create object (explicit calls)" << std::endl; };
		void print() {
			std::cout << "Name: " << name << std::endl;
			this->get()->getSize();
		}
	};

	public:
		void test() {
			Test3::TestObj t1("TestObj:test");
			Test3::TestObj t2;
			t1.print();
			t2.print();
		}

};




int main() {
	Test0 test0;
	test0.test();
	Test1 test1;
	test1.test();
	Test2 test2;
	test2.test();
	Test3 test3;
	test3.test();
	return 0;	
}
