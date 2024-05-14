# C Worksheet Instrumentor

[![Build status](https://ci.appveyor.com/api/projects/status/440fhl6e7jgh0v1w/branch/master?svg=true)](https://ci.appveyor.com/project/rgoulter/c-worksheet-instrumentor/branch/master)

This project takes up the tradition of a "worksheet", in the same vein as
projects like Swift's Playgrounds or Scala Worksheets: add comments beside code to 'visualise' the
results from running the program. e.g.:

```c
int x;
x = 5 * 3;      //> x = 15
```

This project will "`worksheetify`" a given C source file. Particularly, source
files for single-process, single-threaded C99 programs, which may take input
from STDIN, and may output values to STDOUT.

### Passing Input to Worksheet

The Worksheet can take input to give to STDIN by use of comment annotations,
e.g., to feed in `5 2 3`:

```c
//IN:
// 5 2 3
```

## Worksheet Examples

#### Gracefully Catch Segfaults

```c
int main(int argc, char* argv) {
  int x = 5;                                      //> x is int
  int *px;                                        //> px is pointer to int
  px = &x;                                        //> px = 0x7ffc365efd5c = 5
  *px = 3;                                        //> (*px) = 3
  x;                                              //> 3
  px = 0;                                         //> px = (nil)
  *px = 5;                                        //> SEGFAULT
  x;
  printf("done.\n");
}
```

#### Show Value of Expression statements:

```c
int main(int argc, char* argv) {
  10 % -3;                                        //> 1
  -10 % 3;                                        //> -1
  (union { int i; float f; }) { .i = 3 }.f;       //> 4.2039e-45
}
```

#### Provide Content for STDIN:

```c
#include <stdio.h>

//IN:
// 3
// 5 6 4
int main(int argc, char* argv) {
  int n, sum = 0;                                 //> sum is int
  scanf("%d", &n);                                //> 1
  printf("compute sum of %d numbers:\n", n);      //> compute sum of 3 numbers:
  for (int i = 0; i < n; i++) {
    int val;                                      //> val is int
    int _ = scanf("%d", &val);                    //> _ is int
    sum += val;                                   //> sum = 5
                                                  //| sum = 11  [iteration:1]
                                                  //| sum = 15  [iteration:2]
  }
  printf("sum = %d\n", sum);                      //> sum = 15
}
```

#### Show Values Across Multiple Iterations

For blocks which have many iterations, it's possible to 'filter' the
worksheet to one iteration of the block:

```c
#include <stdio.h>
int main(int argc, char* argv) {
  int sum = 0;                                    //> sum is int
  for (int i = 0; i < 5; i++) {
    // worksheet filter iteration == 2
    for (int j = 0; j < 3; j++) {
      printf("i is %d, j is %d\n", i, j);         //> i is 2, j is 0	[iteration:6]
                                                  //| i is 2, j is 1	[iteration:7]
                                                  //| i is 2, j is 2	[iteration:8]
      sum += i * j;                               //> sum = 3	[iteration:6]
                                                  //| sum = 5	[iteration:7]
                                                  //| sum = 9	[iteration:8]
    }
  }
  sum;                                            //> 30
}
```

## How to Get the Program

#### GitHub Release Artifact

Try a [release](https://github.com/rgoulter/c-worksheet-instrumentor/releases).

#### Build with Gradle

To build with gradle

```
gradle build
```

## Runtime Requirements

A C compiler is required. (And on Windows, assumes a `gcc.exe` is on the
`PATH`, e.g. using Mingw-w64 using Msys2).

This tool also requires a Java Runtime Environment, and either `JAVA_HOME`
is set, or `java` is on the `PATH`.

## Running the Program

Scripts provided are `c-worksheetify` and `c-worksheetify-server`. (And
`c-worksheetify.bat`, `c-worksheetify-server.bat`, respectively, for Windows).  

The former can be run e.g. `$ /path/to/c-worksheetify /path/to/some.c`, to
produce instrumented output (like the Examples below).  

The 'server' script is provided because the standalone program was taking too
long to load. It is intended for use by text-editor plugins.

## Plugins for Editors

* **Vim**:
  [rgoulter/c-worksheet.vim](https://github.com/rgoulter/c-worksheet.vim)

## Implementation

This project is implemented in Scala, building on top of ANTLR4
parser-generator to create a C parser. Various AST listeners/visitors then
construct information about the C program, and insert relevant statements into
an augmented C program. This instrumented C source is then compiled & run,
and information is received and gathered by the `Worksheetify` class.

The key ideas in this project being to understand the `CType` of a declaration
or expression; and to be able to construct a series of statements which would
output a value for this `CType`.  
So, an expression statement `E;` becomes something like `{ typeof(E) tmp = E;
output(E); }`.

## Known Limitations

While this project aims to work for C99 programs which run in a single process
on a single thread, there are some limitations/restrictions on the programs
which can be `worksheetified`:

* No macros: Most/all macro usage is unsupported for the input C programs.
    (As such, varargs are also unsupported, as these rely on macros). This is
    because the grammar of an unprocessed file is different. e.g.:

    ```c
    #define LECD(D,S) S D
    LECD(x, int); // becomes int x
    ```

* Headers: This project can understand typedefs and symbols of included
    headers, but not the tags of structs/enums. Of course, no visualisation
    is provided for the header.

* Arrays: Also, "arrays" from `malloc`/`calloc`/etc. can't be visualised in all
    cases. This is mostly because C's arrays don't have an `array.length`.
