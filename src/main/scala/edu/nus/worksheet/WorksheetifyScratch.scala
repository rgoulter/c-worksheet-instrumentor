package edu.nus.worksheet

import edu.nus.worksheet.instrumentor.*;

object WorksheetifyScratch {

  def main(args: Array[String]): Unit = {
    val inputProgram2 = """#include <stdio.h>
int main(int argc, char** argv) { // Line 02
  typedef char *T;
  T x;
  {  // Line 05
    typedef int T;
    T x;
    x = 5;
  }  // Line 09
  x = "hello";
}""";
    val inputProgram = """#include <stdio.h>
int main(int argc, char* argv) { // Line 02
    struct S;
    struct S;
    typedef struct S S;
    S *ptrToS;
    struct S { int x; };
    S myS = { 3 };
    ptrToS = &myS;

    size_t x;
    x = 5;
    printf("%d\n", x);

    struct S2 { int x; };
    struct S2 s2 = { 3 };
    struct S2 *p2;
    p2 = &s2;
}""";
    val inputLines = inputProgram.linesIterator.toList;

    try {
      val wsOutput = Worksheetify.worksheetifyForInput(inputProgram);
      val wsOutputStr = wsOutput.generateWorksheetOutput(); // block until done.

      println(wsOutputStr);
    } catch {
      case ex: WorksheetifyException => {
        println(ex.dumpString());
      }
      case e: Throwable => throw e;
    }
  }
}
