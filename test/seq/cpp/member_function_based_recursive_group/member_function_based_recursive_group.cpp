
// This test case has been extracted from the AllScale API.

struct Intervals {

	template<typename Fun>
	void forEach(const Fun& fun) const {
	}

};


struct LargeArray {

	Intervals ranges;

	void op() {

        // by capturing this, the three types
        //      - Intervals
        //      - LargeArray
        //      - the Lambda type
        // become a recursive group, which would not be the case
        // if you would neither capture the this, nor if you would
        // only consider the fields of those three types.
        //
        // the compiler had a bug leading to invalid semantic check
        // results for this input

		ranges.forEach([this](int i){
		});

	}

};

int main() {

    LargeArray a;
    a.op();

	return 0;
}
