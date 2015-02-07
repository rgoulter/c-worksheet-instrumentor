package edu.nus.worksheet;

import scala.collection.mutable;
import scala.collection.mutable.MutableList;
import scala.concurrent.{Promise, Future, Channel, ExecutionContext};
import ExecutionContext.Implicits.global;

class WorksheetOutput(colForWS : Int = 50, prefixes : Seq[String] = Seq("//> ", "//| ")) {
  val outputPerLine = mutable.Map[Int, MutableList[String]]();
  val allOutputReceived = Promise[Boolean]();

  // General output from program. e.g. printf
  def addLineOfOutput(lineNum : Int, line : String) {
    outputPerLine.getOrElseUpdate(lineNum, MutableList()) += line;
  }

  // Corresponds to "WORKSHEET "
  def addWorksheetOutput(lineNum : Int, line : String) = addLineOfOutput(lineNum, line);

  // Corresponds to compiler errors
  def addErrorMessage(lineNum : Int, line : String) = addLineOfOutput(lineNum, line);

  def addWarningMessage(lineNum : Int, line : String) = addLineOfOutput(lineNum, line);
  
  // Assumes that it stars from wsCol anyway, so doesn't prepend with any padding.
  def generateWorksheetOutputForLine(outputForLine : Seq[String]) : String = {
    val res = new StringBuilder();

    val outputForLineIter = outputForLine.iterator;
          
    if (outputForLineIter.hasNext) {
      // The first line of the output.
      res.append(prefixes(0)); // the " //> "
      res.append(outputForLineIter.next());

      // Subsequent lines of output.
      while (outputForLineIter.hasNext) {
        res.append('\n' + (" " * colForWS));
        res.append(prefixes(1)); // the " //| "  
        res.append(outputForLineIter.next());
      }
    }
    
    return res.toString();
  }
  
  // Blocks until the promise allOutputReceived is done.
  def generateWorksheetOutput(src : Seq[String]) : String = {
    val result = new Channel[String]();
    
    allOutputReceived.future.onComplete {
      case _ => result.write(generateWorksheetOutputNow(src));
    }
    
    return result.read;
  }

  // Assumes that allOutputRecieved has completed.
  private[WorksheetOutput] def generateWorksheetOutputNow(src : Seq[String]) : String = {
    assert(allOutputReceived.isCompleted);
    
    // Take each line of input, and `combine" it will the List of its output
    val res = new StringBuilder;
    
    for ((line, lineNum) <- src.zipWithIndex) {
      // Ensures output always starts on a new line.
      if (res.lastIndexOf("\n") != res.length - 1) {
        res.append('\n');
      }
      
      res.append(line);
      
      // For outputPerLine append the List[String] of output which
      // corresponds to a lineNumber, to the current result.

      // zip's indices start from 0; worksheet outputs as first line starts at 1
      outputPerLine get (lineNum + 1) match {
        case Some(outputForLine) => {
          // Guarantee that handleOutput always starts at colForWS column
          val lastLineLength = res.length - res.lastIndexOf("\n");
          val outputPadding = 
            if (lastLineLength <= colForWS + 1) {
              " " * (colForWS - lastLineLength + 1);
            } else {
              // If the src line is too long,
              // then the Worksheet output will need to be added
              // to the next line of the output.
              '\n' + (" " * colForWS);
            }
                
          //val outputForLine = output.getOrElse(lineNum, List())
          val wsOutput = generateWorksheetOutputForLine(outputForLine);
          if (!wsOutput.isEmpty()) {
            res.append(outputPadding);
            res.append(wsOutput);
          }
        }
        case None => res.append('\n');
      }
    }
    
    return res.toString()
  }
  
  
  // Worksheetify indicates no more output is coming.
  def close() {
    allOutputReceived.success(true);
  }
}