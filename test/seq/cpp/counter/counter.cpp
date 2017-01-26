
#include <stdio.h>

struct Counter {
	int val;
	Counter(int val = 0) { this->val = val; }
	void inc(int diff = 1) { this->val += diff; }
	void dec(int diff = 1) { this->val -= diff; }
	void reset(int val = 0) { this->val = val; }
	int get() const { return this->val; }  
};

int main() {
	
	Counter c;
	printf("c=%d\n", c.get());
	c.inc();
	printf("c=%d\n", c.get());
	c.inc(2);
	printf("c=%d\n", c.get());
	c.dec();
	printf("c=%d\n", c.get());
	c.dec(2);
	printf("c=%d\n", c.get());
	c.reset(5);
	printf("c=%d\n", c.get());

	return 0;
}

