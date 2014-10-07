#include <stdio.h>
#include <stdint.h>

// these initializations must be generated
static const uint8_t kCat3[] = { 173, 148, 140, 0 };
static const uint8_t kCat4[] = { 176, 155, 140, 135, 0 };
static const uint8_t kCat5[] = { 180, 157, 141, 134, 130, 0 };
static const uint8_t kCat6[] =
  { 254, 254, 243, 230, 196, 177, 153, 140, 133, 130, 129, 0 };
static const uint8_t* const kCat3456[] = { kCat3, kCat4, kCat5, kCat6 };

int main() {
        printf("%d \n", *kCat3456[1]);

        return 0;
}
