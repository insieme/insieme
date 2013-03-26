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


int main() {

	Shape s;
	Circle c1;
	//Circle2 c2;

	s.draw();		// Shape::draw()
	c1.draw();		// Circle::draw()
	//c2.draw();		// Circle2::draw()

	Shape *p1 = &c1;
	p1->draw();   	// Circle::draw()
	//p1 = &c1;
	//p1->draw();   	// Circle::draw()

	return 0;
}
