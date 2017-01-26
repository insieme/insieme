#include <stdio.h>

class Shape {
	public:
		void fill() const { printf("Shape::fill()\n"); }
    	void draw() const { printf("Shape::draw() const\n"); }
    	void draw() { printf("Shape::draw()\n"); }
    	~Shape() { printf("Shape::~Shape()\n"); }
};

class Circle: public Shape {
	public:
		void fill() const { printf("Circle::fill()\n"); }
		void draw() const { printf("Circle::draw() const\n"); }
		void draw() { printf("Circle::draw()\n"); }
		~Circle() { printf("Circle::~Circle()\n"); }
};

class Circle2: public Shape {
	public:
		void fill() const { printf("Circle2::fill()\n"); }
		void draw() const { printf("Circle2::draw() const\n"); }
		~Circle2() { printf("Circle2::~Circle2()\n"); }
};


int main() {


	Shape s;
	s.draw();			// Shape::draw()

	Circle c1;
	c1.draw();			// Circle::draw()

	Shape *p1 = &c1;
	p1->draw();			// Shape::draw()

	/**
	 * WARNING CONST is lost in INSPIRE!!!
	 * const Shape *cp1 = &c1;
	 * cp1->draw();			// Shape::draw() const
	 * */


	Shape& rs = c1;
	rs.draw();			// Shape::draw()

	const Shape& crs = c1;
	crs.draw();			// Shape::draw() const

	return 0;
}
