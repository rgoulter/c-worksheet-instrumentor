package edu.nus.worksheet

object WorksheetifyScratch {

  def main(args : Array[String]) : Unit = {
    val inputProgram2 = """#include <stdio.h>
int main(int argc, char* argv) { // Line 02
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

    struct S2 { int x; };
    struct S2 s2 = { 3 };
    struct S2 *p2;
    p2 = &s2;
}""";
    val inputLines = inputProgram.lines.toList;

    val wsOutput = new WorksheetOutput();
    Worksheetify.processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines); // block until done.
    println(wsOutputStr);
  }
}