#include <stdio.h>

class Shape {
	public:
		void fill() const { printf("Shape::fill()\n"); }
    	void draw() const { printf("Shape::draw()\n"); }
    	~Shape() { printf("Shape::~Shape()\n"); }
};

class Circle: public Shape {
	public:
		void fill() const { printf("Circle::fill()\n"); }
		void draw() const { printf("Circle::draw()\n"); }
		~Circle() { printf("Circle::~Circle()\n"); }
};

class Circle2: public Shape {
	public:
		void fill() const { printf("Circle2::fill()\n"); }
		void draw() const { printf("Circle2::draw()\n"); }
		~Circle2() { printf("Circle2::~Circle2()\n"); }
};

class A
{
public:
	A() : x(1) {}
	int x;
};


class B : public A
{
	public:
	int x;
	B() : x(10) {}
};


int main() {

	B b;
	printf("B::x %d\n", b.x);
	printf("B::A::x %d\n", b.A::x);

	Shape s;
	Circle c1;
	Circle2 c2;

	s.draw();		// Shape::draw()
	c1.draw();		// Circle::draw()
	c2.draw();		// Circle2::draw()

//	automatically added operator= has no name -> problem with CAnnotation
//	Shape& r1 = c1;
//	r1.draw();    	// Circle::draw()
//
//	r1 = c2;
//	r1.draw();    	// Circle::draw()

	Shape *p1 = &c1;
	p1->draw();   	// Circle::draw()
	p1 = &c1;
	p1->draw();   	// Circle::draw()

	return 0;
}
