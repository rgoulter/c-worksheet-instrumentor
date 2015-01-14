// moap.c, mother-of-all programs.
// The idea is to be a sampler of features in CS1010,
// so as to demonstrate what can/should be visualised,
// and give a concrete example of how this may be difficult.
#include <stdio.h>
#include <string.h>

int factRec(int n) {
    if (n < 2) {
        return 1;
    } else {
        return n * factRec(n - 1);
    }
}

struct MyStruct {
    int x;
    float y;
};
typedef struct MyStruct MyStruct_t;

int main(int argc, char** argv) {
    // Various variables.
    int x = 5;
    float f = 3.14;
    int *intPtr = &x;
    char *charPtr = "Hello world";
    char charArr[20] = "And some more";
    int intArr[4] = {1, 9, 2, 8};

    MyStruct_t ms = { .x = 3, .y = 1.23 };

    // Let's do some assignment / expressions.
    x = 10;
    *intPtr = 5;
    charArr[0] = 'a';
    // charPtr[6] = 'W'; // SegFault, duh.

    x = factRec(3); // func. call

    {
        int x = 2; // re-use same name in different scope
        float someNumber = 2.4;
        f += someNumber + x;
    }

    ms.x = 7;

    MyStruct_t anotherStruct = { 9, 2.1 };
    ms = anotherStruct;
    ms.y = 4.3;

    // Write to STDOUT
    printf("Size: %d\n", sizeof(size_t));

    // Selection Statements
    if (x < 15) {
        f = 3.4;
    } else {
        x = 2;
    }

    switch (x) {
        case 0:
            printf("case 0\n");
            break;
        case 10:
            printf("case 10\n");
            break;
    }

    // Repetition Statements
    for (x = 0; x < 5; x++) {
        int z = x * 10;
        printf("nyah %d\n", z);
    }

    // e.g. String manipulations in C
    strcat(charArr, "words");
    printf("%s\n", charArr);
}
