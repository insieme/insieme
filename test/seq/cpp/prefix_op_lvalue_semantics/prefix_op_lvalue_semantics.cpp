
// tests that ++ and -- prefix ops are correctly translated using the C++ semantics (where they return lvalues)

#include <stdio.h>

int main() {

    int arr[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    int idx = 0;

    printf("%d\n", arr[++idx]);

    ++--++--++idx;

    printf("%d\n", arr[idx]);

    ++idx = 5;
    
    printf("%d\n", arr[idx]);

    ----idx;
    ++++idx;
    
    printf("%d\n", arr[idx]);
    
    printf("%d\n", ++++idx > 5);
}
