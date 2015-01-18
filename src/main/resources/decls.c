// What are some examples of declarations to deal with?
// How many of these can we deal with for now?
#include <stdio.h>

int myFunc(int x) {
    return x * x;
}

int someFunc(int, int);
int someFunc(x, y) {
    return x + y;
}

struct MyStruct {
    int x;
    float y;
};
typedef struct MyStruct MyStruct_t;

typedef int (*intFunc)(int);

int main(int argc, char **argv) {
    int n = 2;
    int x;
    int y, z[4] = {1, 2, 3, 4}, w[n][n];
    char *str = "hello";

    // structs
    struct { int a; int b; } struct1;
    struct MyStruct struct2;
    MyStruct_t struct3;

    // func ptrs
    int (*func1)(int);
    intFunc func2;
    // what about ptrs-to-func, arrays of func, func returning funcPtr..?

    // const ptrs
    int * const p = &x; // val is const
    const int * q = &x; // ptr is const
    const int * const r = &x;
}
