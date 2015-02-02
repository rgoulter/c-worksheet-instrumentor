package edu.nus.worksheet

import scala.io._
import java.io._
import scala.sys.process.{ Process, ProcessIO }
import scala.collection.mutable
import scala.concurrent.{Channel, Promise, promise}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.regex.Pattern
import edu.nus.worksheet.instrumentor._

object Worksheetify {

  def processWorksheet(srcLines : List[String]) : (Map[Int, Promise[List[String]]], Map[Int, Promise[List[String]]]) = {
    // var currentLine : Int = -1
    // var errorHasOccurred = false
    // val inputChannel = new Channel[Option[String]]
    val outputPerLine = mutable.Map[Int, mutable.MutableList[String]]()
    // val errPerLine = mutable.Map[Int, mutable.MutableList[String]]()
    
    // Construct Map[Int, Promise[List[String]]], a Promise for each line number.
    val stdoutResultPromises = srcLines.zipWithIndex.map({ case (line,lineNum) => (lineNum, promise[List[String]]) }).toMap
    val stderrResultPromises = srcLines.zipWithIndex.map({ case (line,lineNum) => (lineNum, promise[List[String]]) }).toMap

    def fulfillPromise(outputLines : Option[mutable.MutableList[String]], p : Promise[List[String]]) =
      outputLines match {
        case Some(output) => {
          // If there is some List[String], it won't be empty.
          p success output.toList
        }
        case None => {
          // Here we use Future success/failure mechanism for
          // "has output" / "no output", so that the Client can
          // simply only consider promise.future success, and not
          // deal with Option types.
          p failure new Throwable("No output received")
        }
      }
    
    def handleIn(output: java.io.OutputStream) {
      // For now, the augmented programs don't accept STDIN.
      // This is not a good limitation.
      output.close();
    }
    
    def handleOut(input: java.io.InputStream) {
      /*
       * SCHEMA:
       * LINE <number> ::= instrumented program is currently at the given line.
       */
      val LineNum = "LINE (\\d+)".r

      val lines = Source.fromInputStream(input).getLines();
      var currentLine = 0; // lines of source start from 1.
      
      for (line <- lines) {
        var output : mutable.MutableList[String] =
          outputPerLine getOrElseUpdate(currentLine, mutable.MutableList[String]())

        line match {
          case LineNum(d) => currentLine = d.toInt;
          case s => {
            println(currentLine + ":" + line);
            output += line;
          }
        }
      }
    }
    
    def handleErr(input: java.io.InputStream) {
      val ccErr = Source.fromInputStream(input).mkString;
      if (ccErr.length > 0)
        println("Instrumented Program STDERR:" + ccErr);
    }

    println("Instrumenting...");
    val instrumentedProgram = Instrumentor.instrument(srcLines.mkString("\n"));
    println(instrumentedProgram);
    
    // Output to /tmp/instrument.c
    val writeInstrumentedOutput = new BufferedWriter(new FileWriter("/tmp/instrumented.c"));
    writeInstrumentedOutput.write(instrumentedProgram);
    writeInstrumentedOutput.close();

    val prog = new CProgram(instrumentedProgram);
    prog.compile();
    
    println("Running...");
    println("$ " + prog.programPath());

    val processIO = new ProcessIO(handleIn, handleOut, handleErr);
    val proc = prog.process().run(processIO);
    
    // The idea of sending stdoutResultPromises : Map[Line# -> Promise[List[String]]]
    // was to send output once we were done with a LOC.
    // With loops, etc., we can no longer guarantee all the output has been sent
    // until program termination. Sigh.
    proc.exitValue();
    
    for ((lineNum, prom) <- stdoutResultPromises.seq) {
      fulfillPromise(outputPerLine.get(lineNum), prom);
    }

    // We don't even *USE* the following, right?
    // Of course, can simplify this redundant(?) structure.
    // Until then, fail all.
    for ((lineNum, prom) <- stderrResultPromises.seq) {
      fulfillPromise(None, prom);
    }
    
    // Tuple of (promised stdout, promised stderr)
    (stdoutResultPromises, stderrResultPromises)
  }



  // Synchronously construct a String output from
  // the asynchronous output built by Worksheetify.
  def generateWorksheetOutput(src : Seq[String],
                              output : Map[Int, Promise[List[String]]],
                              err : Map[Int, Promise[List[String]]] = Map()) : String = {
    // Take each line of input, and `combine" it will the List of its output
    val colForWS = 50
    val res = new StringBuilder
    
    for ((line, lineNum) <- src.zipWithIndex) {
      // Ensures output always starts on a new line.
      if (res.lastIndexOf("\n") != res.length - 1) {
        res.append("\n")
      }
      if (line.length == 0) {
        res.append("\n")
      }
      
      res.append(line)
      
      // For STDOUT, STDERR, append the List[String] of output which
      // corresponds to a lineNumber, to the current result.
      def handleOutput(outWithPromise : Map[Int, Promise[List[String]]]) =
        outWithPromise get (lineNum + 1) match { // line numbers start from 1.
          case Some(p) => {
            val blockChannel = new Channel[String]
            p.future onSuccess {
              case outputForLine => {
                // Guarantee that handleOutput always starts at colForWS column
                val lastLineLength = res.length - res.lastIndexOf("\n")
                if (lastLineLength <= colForWS + 1) {
                  res.append(" " * (colForWS - lastLineLength + 1))
                } else {
                  // If the src line is too long,
                  // then the Worksheet output will need to be added
                  // to the next line of the output.
                  res.append('\n')
                  res.append(" " * colForWS)
                }
                
                //val outputForLine = output.getOrElse(lineNum, List())
                val outputForLineIter = outputForLine.iterator
                
                if (outputForLineIter.hasNext) {
                  // The first line of the output.
                  res.append("//> ")     // the " //> "  
                  res.append(outputForLineIter.next()) // assume each array is non-empty

                  // Subsequent lines of output.
                  val prefix = "//| "
                  while (outputForLineIter.hasNext) {
                    res.append('\n')
                    res.append(" " * colForWS)
                    res.append(prefix) // the " //| "  
                    res.append(outputForLineIter.next())
                  }
                }
                
                blockChannel write "dummy"
              }
            }
            p.future onFailure {
              case _ => blockChannel write "failure"
            }
            
            blockChannel.read
          }
          
          // No Promisem made for the given lineNum.
          case None => ()
        }
      
      handleOutput(output)
      
      // TODO: Different Symbols for indicating *ERROR* worksheet output.
      // handleOutput(err) // Errors for Instrument-based interpreter not handled like this.
    }
    
    return res.toString()
  }
  
  
  
  def main(args : Array[String]) : Unit = {
    val inputProgram = """#include <stdio.h>

int main(int argc, char* argv) {
  printf("Line1\nLine 2\n");
  printf("Another line\n");
}""";
    val inputLines = inputProgram.lines.toList;
    
    val (output, err) = processWorksheet(inputLines);
    val wsOutput = generateWorksheetOutput(inputLines, output, err);
    println(wsOutput);
  }
}