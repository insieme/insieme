// async example
 #include <iostream>       // std::cout
 #include <future>         // std::async, std::future

// a non-optimized way of checking for prime numbers:
bool is_prime (int x) {
    std::cout << "Calculating. Please, wait...\n";
    for (int i=2; i<x; ++i) 
        if (x%i==0) 
            return false;
    return true;
}

void nothing () { 
    std::cout << "Nothing function...\n";
    return;
}

int main () {
    //fun call with/without args
    int i=313222313;
    std::future<bool> fut = std::async (is_prime,i);
    std::cout << "Checking whether 313222313 is prime.\n";
    bool ret = fut.get();      // waits for is_prime to return
    if (ret) 
        std::cout << "It is prime!\n";
    else 
        std::cout << "It is not prime.\n";


    std::future<bool> fut2 = std::async (is_prime,2313);
    std::cout << "Checking whether 2313 is prime.\n";
    bool ret2 = fut2.get();      // waits for is_prime to return
    if (ret2) 
        std::cout << "It is prime!\n";
    else 
        std::cout << "It is not prime.\n";


    // test unit return type
    std::future<void> unit_fut = std::async(nothing);
    unit_fut.get();

    // lambda calls
    auto fut3 = std::async ([](int i) { return i*i; }, 10);
    std::cout << "int direct Square 10: " << fut3.get() << "\n";
    auto fut4 = std::async ([](int i) { return i*i; }, i);
    std::cout << "int arg Square 313222313: " << fut4.get() << "\n";

    auto fut7 = std::async ([](const int& i) { return i*i; }, 10);
    std::cout << "const int ref direct Square 10: " << fut7.get() << "\n";
    auto fut8 = std::async ([](const int& i) { return i*i; }, i);
    std::cout << "const int ref arg Square 313222313: " << fut8.get() << "\n";
    
    //lambda call with capturing
    auto fut9 = std::async ([&](int j) { int k=i; i=1; return j*k; }, 10);
    std::cout << "int direct 10*313222313: " << fut9.get() << "\n";
    auto fut10 = std::async ([&](int j) { return j*i; }, i);
    std::cout << "int arg 10*313222313: " << fut10.get() << "\n";

//    auto fut12 = std::async ([&](int& j) { return j*i; }, i);
//    std::cout << "int ref arg 10*313222313: " << fut12.get() << "\n";

    auto fut13 = std::async ([&](const int& j) { return j*i; }, 10);
    std::cout << "const int ref direct 10*313222313: " << fut13.get() << "\n";
    auto fut14 = std::async ([&](const int& j) { return j*i; }, i);
    std::cout << "const int ref arg 10*313222313: " << fut14.get() << "\n";

    return 0;
}
