#include <iostream>

class AbstractShape {
	protected:
		AbstractShape();
	public:
		virtual void fill() const = 0;
		virtual void draw() const = 0;
		virtual ~AbstractShape() { std::cout << "AbstractShape::~AbstractShape()"; }
};

class Shape : public AbstractShape {
	public:
		virtual void fill() const { }
    	virtual void draw() const { std::cout << "Shape::draw()"; }
    	virtual ~Shape() { std::cout << "Shape::~Shape()"; }
};

class Circle: public Shape {
	public:
		void fill() const { }
		void draw() const { std::cout << "Circle::draw()"; }
		~Circle() { std::cout << "Circle::~Circle()"; }
};

class Circle2: public AbstractShape {
	public:
		void fill() const { }
		void draw() const { std::cout << "Circle2::draw()"; }
		~Circle2() { std::cout << "Circle2::~Circle2()"; }
};

class A
{
public:
  int x;
};


class B : public A
{
public:
  int x;
  B()
  {
    x = 0;
    A::x = 1;
  }
};


int main() {

	B b;

	Shape s;
	s.draw();		// Shape::draw()
	Circle c1;
	c1.draw();		// Circle::draw()
	Shape& r1 = c1;
	r1.draw();    	// Circle::draw()
	Shape* p1 = &c1;
	p1->draw();   	// Circle::draw()

	AbstractShape* pAS;	//only a pointer
	pAS = &c1;
	pAS->draw();	// Circle::draw()

	AbstractShape& rAS = c1;
	rAS.draw();		// Circle::draw()

	p1 = new Circle;
	p1->draw();  	// Circle::draw()

	delete p1;      // Circle::~Circle()

	Circle2 c2;
	AbstractShape& r2 = c2;
	r2.draw();      		// Circle2::draw()

	AbstractShape* p2 = new Circle();
	p2->draw();				// Circle2::draw()

	delete p2;		// Circle2::~Circle2()

	return 0;
}
