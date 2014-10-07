// Author (2010-2011): Bernhard Kornberger <office@geom.at>, Geom e.U. (http://geom.at)
// Project leader: Martin Schifko <Martin.Schifko@ecs.steyr.com>, Engineering Center Steyr
//
// Written for Engineering Center Steyr GmbH & Co KG, A 4300 St. Valentin, Austria. This 
// software comes without any warranty, without even the implied warranty of merchantability 
// or fitness for a particular purpose.

#include <iostream>

using namespace std;

struct Obj{
	
	static string A;
	static string B;
	
	static const string colorArray[2];
//	static const string colorNames[2];

};


// Color definitions
string Obj::A("string A");
string Obj::B("string B");

const string Obj::colorArray[2]={
	A,
	B,
};

//const string Obj::colorNames[2]={
//	"LIGHTBLUE",
//	"DARKBLUE",
//};

int main(){
	//for (int i=0;  i < 2; ++i){
		//std::cout << Obj::colorNames[i] << std::endl;
		std::cout << Obj::colorArray[0] << std::endl;
	//}

	return 0;
}
