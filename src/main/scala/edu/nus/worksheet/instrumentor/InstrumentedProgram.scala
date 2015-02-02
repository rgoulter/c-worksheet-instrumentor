package edu.nus.worksheet.instrumentor

import scala.io._
import scala.sys.process._
import scala.concurrent.{Channel, Promise}

class InstrumentedProgram(inputProgram : String) {
  
  // Some folder to compile to.
  var programFolder = "/tmp";
  
  // gcc on path
  var cc = "gcc";
  
  var programName = "a.out";
  
  def programPath() : String = s"$programFolder/$programName";

  def compile() = {
    // "-xc -" so we can use STDIN
    val compileCommand = s"$cc -xc -o $programPath -"; 

    println("Compiling..");
    println("$ " + compileCommand);

    def handleIn(output: java.io.OutputStream) {
      // Write our input string to the process' STDIN
      val writer = new java.io.PrintWriter(output);
      writer.write(inputProgram);
      writer.close();
    }
    
    def handleOut(input: java.io.InputStream) {
      val ccOut = Source.fromInputStream(input).mkString;
      if (ccOut.length > 0)
        println("Compiler STDOUT:" + ccOut);
    }
    
    def handleErr(input: java.io.InputStream) {
      val ccErr = Source.fromInputStream(input).mkString;
      if (ccErr.length > 0)
        println("Compiler STDERR:" + ccErr);
    }

    val processIO = new ProcessIO(handleIn, handleOut, handleErr);
    val compileResult = compileCommand.run(processIO).exitValue();
    
    if (compileResult != 0) {
      println("ERROR COMPILING CODE!");
    }
  }
  
  def run() = {
    println("Running...");
    println("$ " + programPath);
    programPath!;
  }
}

object InstrumentedProgram {
  def main(args : Array[String]) : Unit = {
    val inputFileIS = getClass().getClassLoader().getResourceAsStream("hello.c");
    val inputString = Source.fromInputStream(inputFileIS).mkString;
    
    val p = new InstrumentedProgram(inputString);
    p.compile();
    p.run();
  }
}