//#include <iostream>
#include <CGAL/Uncertain.h>

int main() {
	bool X = false;
	bool Y = true;

	__extension__ ({
		CGAL::Uncertain<bool> CGAL_TMP = (X);
		CGAL::certainly_not(CGAL_TMP) ? CGAL::make_uncertain(X) : CGAL_TMP & CGAL::make_uncertain((Y));
	});

	CGAL::Uncertain<bool> x =
	({
		CGAL::Uncertain<bool> CGAL_TMP = (X);
		CGAL::certainly_not(X) ? CGAL::make_uncertain(X) : CGAL::make_uncertain((Y));
	});

	CGAL::Uncertain<bool> y =
	({
		CGAL::certainly_not(X) ? CGAL::make_uncertain(X) : CGAL::make_uncertain((Y));
	});

	CGAL::Uncertain<bool> z = ({ CGAL::make_uncertain(X); });


	//CGAL::make_uncertain(false);
	//({ CGAL::make_uncertain(false); });
	({ CGAL::Uncertain<bool> CGAL_TMP; 
		CGAL_TMP; 
	});
	
	({ CGAL::Uncertain<bool> CGAL_TMP = (true); 
		CGAL_TMP;
	});

	CGAL_AND(X,Y);

	return 0;
}
