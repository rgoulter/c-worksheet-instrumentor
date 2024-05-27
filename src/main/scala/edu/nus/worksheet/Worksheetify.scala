package edu.nus.worksheet

import scala.io.*
import java.io.*
import scala.sys.process.{Process, ProcessIO}
import scala.collection.mutable
import scala.concurrent.{Channel, Promise}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random;
import java.util.regex.Pattern
import edu.nus.worksheet.instrumentor.*

object Worksheetify {
  val MaxIterationsDefault = 10000;
  val OutputLimitDefault = 8;

  def processWorksheet(
      srcLines: Seq[String],
      outputTo: WorksheetOutput,
      cc: String = FindCompiler.findCompilerOnPath(),
      stdinLines: Seq[String] = Seq(),
      maxIterations: Int = MaxIterationsDefault
  ): Unit = {
    // For worksheet directives (in instrumenting code),
    // we generate a random string so that it becomes more difficult
    // for a program to interfere with the instrumentor.
    val nonce = "_" + (Random.alphanumeric.take(5).mkString);

    val blockFilters: mutable.Map[String, Int => Boolean] = mutable.HashMap();

    def handleIn(output: java.io.OutputStream): Unit = {
      val out = new PrintWriter(output);
      stdinLines.foreach(out.println(_));
      out.close();
    }

    def handleOut(input: java.io.InputStream): Unit = {
      // Regexs to match from the STDOUT of the instrumented program.
      val LineNum = LineDirective(nonce).regex();
      val Worksheet = WorksheetDirective(nonce).regex();
      val WorksheetResult = WorksheetDirective(nonce).regex("exprResult");
      val WorksheetTermination = WorksheetDirective(nonce).regex("termination");
      val FunctionEnter = FunctionEnterDirective(nonce).regex();
      val FunctionReturn = FunctionReturnDirective(nonce).regex();

      // wait for all output from instrumented program :(
      val lines = Source.fromInputStream(input).getLines();
      val currentLineStack = mutable.Stack[(Int, String)]();
      val blockIterations = mutable.Map[String, Int]();
      currentLineStack.push((0, "")); // lines of source start from 1.
      val hasStdout = mutable.Set[Int]();

      def currentLine(): Int =
        currentLineStack.top._1
      def currentBlock(): String =
        currentLineStack.top._2;
      def setCurrentLine(line: Int, block: String) =
        currentLineStack(0) = (line, block); // stack begins at 0

      def currentIterationInBlock(blk: String): Int =
        blockIterations.getOrElse(blk, 0);

      // Try to output, if we don't need to filter it out.
      def output(s: String) = {
        val filtersSatisfied = blockFilters.iterator.forall({ el =>
          val (blockName, pred) = el;

          // Consider only filters for the blocks the currentBlock is
          // a descendant of.
          def isAncestorOfCurrentBlock(bn: String): Boolean =
            currentBlock().startsWith(bn);

          if isAncestorOfCurrentBlock(blockName) then
            pred(currentIterationInBlock(blockName));
          else true;
        })

        // Predicate whether to 'output' for the current block/line
        val currentBlockPredicate =
          blockFilters.getOrElse(currentBlock(), { (_: Int) => true; });
        val currentBlockIteration = currentIterationInBlock(currentBlock());

        // println(currentLine() + ":WS " + s);
        if filtersSatisfied then
          if currentBlockIteration > 0 then
            outputTo.addWorksheetOutput(
              currentLine(),
              s + s"\t[iteration:$currentBlockIteration]"
            );
          else outputTo.addWorksheetOutput(currentLine(), s);
      }

      for line <- lines do {
        line match {
          case LineNum(s, d, blockName, blockIteration) => {
            if s.length() > 0 then {
              hasStdout.add(currentLine())
              output(s);
            }
            setCurrentLine(d.toInt, blockName);
            blockIterations.put(currentBlock(), blockIteration.toInt)
            // println(s"LINE: $d in block $blockName iter $blockIteration");
          }
          case Worksheet(_, s) => {
            output(s);
          }
          case WorksheetResult(pre, s) => {
            if pre.length() > 0 then {
              hasStdout.add(currentLine())
              output(pre);
            }

            // Special Case: `printf` returns an int. So `printf("...")` is a function appl'n,
            //  returning an int result. But showing this result is detrimental to worksheet.
            //  (and screws up with the tests). So, only output a result if there's no output to
            //  STDOUT on the same line already.
            val hasNoOutputOnLine = !hasStdout.contains(currentLine());
            if hasNoOutputOnLine then {
              output(s);
            }
          }
          case WorksheetTermination(_, s) => {
            // Output the message, no matter what.
            outputTo.addLineOfOutput(currentLine(), s, true);
          }
          case FunctionEnter() => {
            currentLineStack.push((-1, "function"));
          }
          case FunctionReturn() => {
            currentLineStack.pop();
          }
          case s => {
            hasStdout.add(currentLine())
            output(s);
          }
        }
      }

      // No more output coming from instrumented program,
      // so worksheet has no more output to give.
      outputTo.close();
    }

    def handleErr(input: java.io.InputStream): Unit = {
      val ccErr = Source.fromInputStream(input).mkString;
    }

    // If Program has errors, we can correspond these errors
    // and return *that* as the output.
    // println("Checking...");
    val inputProgramSrc = srcLines.mkString("\n");
    val originalProgram = new CProgram(inputProgramSrc, cc = cc);
    val (inputWarnings, inputErrors) = originalProgram.checkForErrors();

    if !inputErrors.isEmpty then {
      // println("There were errors! Stopping.");

      def messageFor(d: Diagnostic): String =
        s"${d.line}:${d.column}: ${d.message}";

      inputErrors.foreach({ error =>
        outputTo.addErrorMessage(error.line, messageFor(error));
      });
      inputWarnings.foreach({ warning =>
        outputTo.addWarningMessage(warning.line, messageFor(warning));
      });
      outputTo.close();

      return;
    }

    // println("Instrumenting...");
    val instrumentor = Instrumentor.instrumentorFor(inputProgramSrc, nonce);
    val instrumentedProgram = instrumentor.getInstrumentedProgram();
    blockFilters ++= instrumentor.blockFilters;
    // println(instrumentedProgram);

    val wsMacros = Map("WORKSHEET_MAX_ITERATIONS" -> maxIterations.toString)
    val prog =
      new CProgram(instrumentedProgram, cc = cc, macroDefinitions = wsMacros);
    val (instrumentedWarnings, instrumentedErrors) = prog.compile();

    // Check if the instrumented program compiles.
    // It should: Given a C program which compiles,
    //  our instrumenting shouldn't introduce any errors.
    // - If it does not, this is a bug; but should fail with dignity.
    if !instrumentedErrors.isEmpty then {
      val errorMessages = instrumentedErrors.map(_.diagnosticMessage());
      throw new UnableToInstrumentException(
        srcLines.mkString("\n"),
        instrumentedProgram,
        errorMessages
      );
    }

    // println("Running...");
    // println("$ " + prog.programPath());

    val processIO = new ProcessIO(handleIn, handleOut, handleErr);
    val proc = prog.process().run(processIO);
  }

  def worksheetifyForInput(
      inputProgram: String,
      maxOutputPerLine: Int = OutputLimitDefault,
      maxIterations: Int = MaxIterationsDefault,
      cc: String = FindCompiler.findCompilerOnPath()
  ): WorksheetOutput = {
    val inputLines = inputProgram.linesIterator.toSeq;
    val wsOutput =
      new WorksheetOutput(inputLines, maxOutputPerLine = maxOutputPerLine);
    val stdInput = StdinMarkup.extractFromSource(inputProgram);

    // May throw a Worksheetify Exception
    Worksheetify.processWorksheet(
      inputLines,
      wsOutput,
      cc = cc,
      stdinLines = stdInput,
      maxIterations = maxIterations
    );

    return wsOutput;
  }

  private[worksheet] def dumpExceptionToFile(
      ex: WorksheetifyException,
      filename: String = "c_worksheet_failed.c"
  ): Unit = {
    // Something went wrong.. dump to (cwd? tmp?),
    // and add a message that we were unable to instrument the program.

    val dumpFile = new File(filename);
    val pw = new PrintWriter(dumpFile);

    try {
      // Dump the failed instrumentation to some file.
      pw.println(ex.dumpString())
    } finally {
      pw.close();
    }
  }

  def main(args: Array[String]): Unit = {
    if args.length == 0 then {
      println("Expected: java Worksheetify <input>.c");
      return;
    }

    val inputFilename = args(0);
    val inputProgram = Source.fromFile(inputFilename);
    val inputLines = inputProgram.getLines().toSeq;

    try {
      val wsOutput = worksheetifyForInput(inputLines.mkString("\n"));
      val wsOutputStr = wsOutput.generateWorksheetOutput(); // block until done.

      println(wsOutputStr);
    } catch {
      case ex: WorksheetifyException => {
        dumpExceptionToFile(ex);

        // May be helpful to add the message at the line the user is
        // 'focussed at'.
        val lineToAddMessageAt = 0;
        val inputWithMessage = inputLines.zipWithIndex
          .map({ case (l, i) =>
            if i == lineToAddMessageAt then l + s" // Failed to instrument";
            else l;
          })
          .mkString("\n");

        // Output the original program (with message)
        println(inputWithMessage);
      }
      case e: Throwable => throw e;
    }
  }
}
