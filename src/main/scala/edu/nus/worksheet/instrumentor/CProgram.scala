package edu.nus.worksheet.instrumentor

import scala.io._
import scala.sys.process._
import java.util.regex.Pattern
import scala.concurrent.{Channel, Promise}
import scala.collection.mutable.LinkedList
import scala.collection.mutable.ListBuffer


abstract class Diagnostic(source : String, line : Int, column : Int, message : String);

object Diagnostic {
  val Warning = "([^:]+):(\\d+):(\\d+): warning: (.*)".r
  val Error = "([^:]+):(\\d+):(\\d+): error: (.*)".r
}

case class WarningMessage(source : String, line : Int, col : Int, message : String)
extends Diagnostic(source, line, col, message);

case class ErrorMessage(source : String, line : Int, col : Int, message : String)
extends Diagnostic(source, line, col, message);


class CProgram(var inputProgram : String) {
  // Some folder to compile to.
  var programFolder = "/tmp";
  
  // gcc on path
  var cc = "gcc";
  
  var programName = "a.out";
  
  def programPath() : String = s"$programFolder/$programName";

  def compile() : (Seq[WarningMessage], Seq[ErrorMessage]) = {
    // "-xc -" so we can use STDIN
    val useC99Standard = "-std=c99";
    val outputPath = s"-o $programPath"
    val readFromStdIn = "-xc -";
    val suppressDiagnosticCaret = "-fno-diagnostics-show-caret";
    val suppressDiagnosticFlag = "-fno-diagnostics-show-option";
    
    val compileCommand = Seq(cc,
                             useC99Standard,
                             outputPath,
                             suppressDiagnosticCaret,
                             suppressDiagnosticFlag,
                             readFromStdIn).mkString(" "); 

    println("Compiling..");
    println("$ " + compileCommand);
    
    val diagnosticsChannel = new Channel[(Seq[WarningMessage], Seq[ErrorMessage])]();

    def handleIn(output: java.io.OutputStream) {
      // Write our input string to the process' STDIN
      val writer = new java.io.PrintWriter(output);
      // writer.write(inputProgram);
      writer.write(inputProgram);
      writer.close();
    }
    
    def handleOut(input: java.io.InputStream) {
      val ccOut = Source.fromInputStream(input).mkString;
      if (ccOut.length > 0)
        println("Compiler STDOUT:" + ccOut);
    }
    
    def handleErr(input: java.io.InputStream) {
      // Process lines which are of form:
      // <source>:<line>:<char>: (error|warning): <message>
      val warnings = new ListBuffer[WarningMessage]();
      val errors = new ListBuffer[ErrorMessage]();
      
      for(err <- Source.fromInputStream(input).getLines()) {
        err match {
          case Diagnostic.Warning(src,line,col,msg) => {
            warnings += WarningMessage(src, line.toInt, col.toInt, msg);
            println(s"$line:$col Warning: $msg");
          }
          case Diagnostic.Error(src,line,col,msg) => {
            errors += ErrorMessage(src, line.toInt, col.toInt, msg);
            println(s"$line:$col Error: $msg");
          }
          case s => {
            println("no match:" + s)
          };
        }
      }
      
      diagnosticsChannel.write((warnings, errors));
    }

    val processIO = new ProcessIO(handleIn, handleOut, handleErr);
    val compileResult = Process(compileCommand).run(processIO).exitValue();
    
    return diagnosticsChannel.read;
  }
  
  def process() : ProcessBuilder = Process(programPath);
}

object CProgram {
  def main(args : Array[String]) : Unit = {
    val inputFileIS = getClass().getClassLoader().getResourceAsStream("hello.c");
    val inputString = Source.fromInputStream(inputFileIS).mkString;
    
    val p = new CProgram(inputString);
    p.compile();
    p.process()!;
  }
}