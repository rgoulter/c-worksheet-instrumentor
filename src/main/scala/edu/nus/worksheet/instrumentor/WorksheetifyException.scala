package edu.nus.worksheet.instrumentor

abstract class WorksheetifyException extends RuntimeException {
  def dumpString() : String;
}

case class ParseException(originalProgram : String, loc : (Int, Int), msg : String)
  extends WorksheetifyException {
  val (line, col) = loc;

  def dumpString() : String = {
    originalProgram.lines.zipWithIndex.map({ case (l, i) =>
      if (i + 1 == line)
        l + "  // " + msg;
      else
        l;
    }).mkString("\n");
  }
}

case class UnableToInstrumentException(originalProgram : String,
                                       instrumentedProgram : String,
                                       errors : Iterable[String])
  extends WorksheetifyException {
  private[UnableToInstrumentException] def srcWithLineNums(src : String) : String = {
    val srcLines = src.lines.toSeq;
    val longestInsLine = srcLines.map(_.length()).max;

    srcLines.zipWithIndex.map({ case (l, i) =>
      l + " " * (longestInsLine - l.length()) + "  // " + i;
    }).mkString("\n");
  }

  // Dump the instrumented source, (w/ line #s added),
  // followed by the errors as comments,
  // followed by original source as comment.
  def dumpString() : String = {
    srcWithLineNums(instrumentedProgram) + "\n\n" +
    errors.map("// " + _).mkString("\n") + "\n\n" +
    srcWithLineNums(originalProgram).lines.map("// " + _).mkString("\n");
  }
}
