#include <iostream>

constexpr int getFive(){
	return 5;
}

int main (){

	int array[getFive() + 5];

	for (int i =0; i < 10; i++)
		array[i] =i;

	for (int i =0; i < 10; i++)
		std::cout << " elem: " << i << std::endl;


	constexpr double earth_gravitational_acceleration = 9.8;
	constexpr double moon_gravitational_acceleration = earth_gravitational_acceleration / 6.0;

	std::cout << " moon: " << moon_gravitational_acceleration << std::endl;

	return 0;
}
