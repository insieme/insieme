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


	{
		Shape* ps1 = new Shape;
		ps1->draw();			// Shape::draw()

		Circle* pc1 = new Circle;
		pc1->draw();			// Circle::draw()

		Shape* ps2 = new Circle; 
		Shape* ps3 = ps2;
		ps2->draw();			// Shape::draw()

		/**
		* WARNING CONST is lost in INSPIRE!!!
		* const Shape *cp1 = new Circle();
		* cp1->draw();			// Shape::draw() const
		* */

		Shape& rs = *pc1;
		rs.draw();			// Shape::draw()

		const Shape& crs = *pc1;
		crs.draw();			// Shape::draw() const

		delete ps1;
		delete pc1;
		delete ps2;
	}

	{
		Shape* ps1 = new Shape[1];
		ps1[0].draw();			// Shape::draw()

		Circle* pc1 = new Circle[1];
		pc1->draw();			// Circle::draw()

		Shape* ps2 = new Circle[1]; 
		Shape* ps3 = ps2;
		ps2[0].draw();			// Shape::draw()

		/**
		* WARNING CONST is lost in INSPIRE!!!
		* const Shape *cp1 = new Circle();
		* cp1->draw();			// Shape::draw() const
		* */

		Shape& rs = pc1[0];
		rs.draw();			// Shape::draw()
	
		const Shape& crs = pc1[0];
		crs.draw();			// Shape::draw() const

		delete[] ps1;
		delete[] pc1;
		delete[] ps2;	
	}
	return 0;
}
