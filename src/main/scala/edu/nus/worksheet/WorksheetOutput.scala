package edu.nus.worksheet;

import java.util.concurrent.LinkedTransferQueue;

import scala.collection.mutable;
import scala.collection.mutable.ListBuffer;
import scala.concurrent.{Promise, Future, ExecutionContext};
import ExecutionContext.Implicits.global;

trait WorksheetOutputListener {
  def outputReceived(kind: String, lineNum: Int, output: String): Unit;
}

class WorksheetOutput(
    src: Iterable[String],
    colForWS: Int = 50,
    prefixes: Seq[String] = Seq("//> ", "//| "),
    maxOutputPerLine: Int = Worksheetify.OutputLimitDefault
) {
  val outputPerLine = mutable.Map[Int, ListBuffer[String]]();
  val allOutputReceived = Promise[Boolean]();
  private[WorksheetOutput] val receivedOutputListeners =
    ListBuffer[WorksheetOutputListener]();

  def addOutputListener(listener: WorksheetOutputListener): Unit = {
    receivedOutputListeners += listener;
  }

  // General output from program. e.g. printf
  def addLineOfOutput(
      lineNum: Int,
      line: String,
      force: Boolean = false
  ): Unit = {
    val ml = outputPerLine.getOrElseUpdate(lineNum, ListBuffer());

    val message = if ml.length < maxOutputPerLine || force then {
      Some(line);
    } else if ml.length == maxOutputPerLine then {
      Some("... [output truncated]");
    } else {
      None;
    }

    message match {
      case Some(message) => {
        ml += message;

        receivedOutputListeners.foreach { listener =>
          listener.outputReceived("output", lineNum, message);
        };
      }
      case None => ();
    }
  }

  // Corresponds to "WORKSHEET "
  def addWorksheetOutput(lineNum: Int, line: String) =
    addLineOfOutput(lineNum, line);

  // Corresponds to compiler errors
  def addErrorMessage(lineNum: Int, line: String) =
    addLineOfOutput(lineNum, line);

  def addWarningMessage(lineNum: Int, line: String) =
    addLineOfOutput(lineNum, line);

  // Assumes that it stars from wsCol anyway, so doesn't prepend with any padding.
  def generateWorksheetOutputForLine(
      outputForLineIter: Iterator[String]
  ): String = {
    val res = new StringBuilder();

    if outputForLineIter.hasNext then {
      // The first line of the output.
      res.append(prefixes(0)); // the " //> "
      res.append(outputForLineIter.next());

      // Subsequent lines of output.
      while outputForLineIter.hasNext do {
        res.append("\\n" + (" " * colForWS));
        res.append(prefixes(1)); // the " //| "
        res.append(outputForLineIter.next());
      }
    }

    return res.toString();
  }

  // Blocks until the promise allOutputReceived is done.
  def generateWorksheetOutput(): String = {
    val result = new LinkedTransferQueue[String]();

    allOutputReceived.future.onComplete { case _ =>
      result.put(generateWorksheetOutputNow());
    }

    return result.take();
  }

  // Assumes that allOutputRecieved has completed.
  private[WorksheetOutput] def generateWorksheetOutputNow(): String = {
    assert(allOutputReceived.isCompleted);

    // Take each line of input, and `combine" it will the List of its output
    val res = new StringBuilder;

    for (line, lineNum) <- src.zipWithIndex do {
      // Ensures output always starts on a new line.
      if res.lastIndexOf("\n") != res.length - 1 then {
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
            if lastLineLength <= colForWS + 1 then {
              " " * (colForWS - lastLineLength + 1);
            } else {
              // If the src line is too long,
              // then the Worksheet output will need to be added
              // to the next line of the output.
              "\\n" + (" " * colForWS);
            }

          // val outputForLine = output.getOrElse(lineNum, List())
          val wsOutput = generateWorksheetOutputForLine(outputForLine.iterator);
          if !wsOutput.isEmpty() then {
            res.append(outputPadding);
            res.append(wsOutput);
          }
        }
        case None => res.append('\n');
      }
    }

    return res.toString();
  }

  // Worksheetify indicates no more output is coming.
  def close(): Unit = {
    allOutputReceived.success(true);
  }
}
