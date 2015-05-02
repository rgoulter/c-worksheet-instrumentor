package edu.nus.worksheet

import scala.io._
import java.io._
import scala.sys.process.{ Process, ProcessIO }
import scala.collection.mutable
import scala.collection.mutable.MutableList
import scala.concurrent.{Channel, Promise, promise}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random;
import java.util.regex.Pattern
import edu.nus.worksheet.instrumentor._

object Worksheetify {

  def processWorksheet(srcLines : Seq[String],
                       outputTo : WorksheetOutput,
                       cc : String = FindCompiler.findCompilerOnPath(),
                       stdinLines : Seq[String] = Seq()) {
    // For worksheet directives (in instrumenting code),
    // we generate a random string so that it becomes more difficult
    // for a program to interfere with the instrumentor.
    val nonce = "_" + (Random.alphanumeric.take(5).mkString);

    val blockFilters : mutable.Map[String, Int => Boolean] = mutable.HashMap();

    def handleIn(output: java.io.OutputStream) {
      val out = new PrintWriter(output);
      stdinLines.foreach(out.println(_));
      out.close();
    }

    def handleOut(input: java.io.InputStream) {
      // Regexs to match from the STDOUT of the instrumented program.
      val LineNum = LineDirective(nonce).regex();
      val Worksheet = WorksheetDirective(nonce).regex();
      val FunctionEnter = FunctionEnterDirective(nonce).regex();
      val FunctionReturn = FunctionReturnDirective(nonce).regex();

      // wait for all output from instrumented program :(
      val lines = Source.fromInputStream(input).getLines();
      val currentLineStack = mutable.ArrayStack[(Int, String)]();
      val blockIterations = mutable.Map[String, Int]();
      currentLineStack.push((0, "")); // lines of source start from 1.

      def currentLine() : Int =
        currentLineStack.top._1
      def currentBlock() : String =
        currentLineStack.top._2;
      def setCurrentLine(line : Int, block : String) =
        currentLineStack(0) = (line, block); // stack begins at 0

      def currentIterationInBlock(blk : String) : Int =
        blockIterations.getOrElse(blk, 0);

      // Try to output, if we don't need to filter it out.
      def output(s : String) = {
        val filtersSatisfied = blockFilters.iterator.forall({ el =>
          val (blockName, pred) = el;

          // Consider only filters for the blocks the currentBlock is
          // a descendant of.
          def isAncestorOfCurrentBlock(bn : String) : Boolean =
            currentBlock.startsWith(bn);

          if (isAncestorOfCurrentBlock(blockName))
            pred(currentIterationInBlock(blockName));
          else
            true;
        })

        // Predicate whether to 'output' for the current block/line
        val currentBlockPredicate =
          blockFilters.getOrElse(currentBlock(), { _ : Int => true; });
        val currentBlockIteration = currentIterationInBlock(currentBlock);

        // println(currentLine + ":WS " + s);
        if (filtersSatisfied)
          if (currentBlockIteration > 0)
            outputTo.addWorksheetOutput(currentLine, s + s"\t[iteration:$currentBlockIteration]");
          else
            outputTo.addWorksheetOutput(currentLine, s);
      }

      for (line <- lines) {
        line match {
          case LineNum(s, d, blockName, blockIteration) => {
            if (s.length() > 0) {
              output(s);
            }
            setCurrentLine(d.toInt, blockName);
            blockIterations.put(currentBlock, blockIteration.toInt)
            // println(s"LINE: $d in block $blockName iter $blockIteration");
          }
          case Worksheet(s) => {
            output(s);
          }
          case FunctionEnter() => {
            currentLineStack.push((-1, "function"));
          }
          case FunctionReturn() => {
            currentLineStack.pop();
          }
          case s => {
            output(s);
          }
        }
      }

      // No more output coming from instrumented program,
      // so worksheet has no more output to give.
      outputTo.close();
    }

    def handleErr(input: java.io.InputStream) {
      val ccErr = Source.fromInputStream(input).mkString;
    }

    // If Program has errors, we can correspond these errors
    // and return *that* as the output.
    // println("Checking...");
    val inputProgramSrc = srcLines.mkString("\n");
    val originalProgram = new CProgram(inputProgramSrc, cc = cc);
    val (inputWarnings, inputErrors) = originalProgram.checkForErrors();

    if (!inputErrors.isEmpty) {
      // println("There were errors! Stopping.");

      def messageFor(d : Diagnostic) : String =
        s"${d.line}:${d.column}: ${d.message}";

      inputErrors.foreach({ error => outputTo.addErrorMessage(error.line, messageFor(error)); });
      inputWarnings.foreach({ warning => outputTo.addWarningMessage(warning.line, messageFor(warning)); });
      outputTo.close();

      return;
    }

    // println("Instrumenting...");
    val instrumentor = Instrumentor.instrumentorFor(inputProgramSrc, nonce);
    val instrumentedProgram = instrumentor.rewriter.getText();
    blockFilters ++= instrumentor.blockFilters;
    // println(instrumentedProgram);

    val prog = new CProgram(instrumentedProgram, cc = cc);
    val (instrumentedWarnings, instrumentedErrors) = prog.compile();
    assert(instrumentedErrors.isEmpty);

    // println("Running...");
    // println("$ " + prog.programPath());

    val processIO = new ProcessIO(handleIn, handleOut, handleErr);
    val proc = prog.process().run(processIO);
  }



  def main(args : Array[String]) : Unit = {
    if (args.length == 0) {
      println("Expected: java Worksheetify <input>.c");
      return;
    }

    val inputFilename = args(0);
    val inputLines = Source.fromFile(inputFilename).getLines().toSeq;

    val wsOutput = new WorksheetOutput();
    processWorksheet(inputLines, wsOutput);
    val wsOutputStr = wsOutput.generateWorksheetOutput(inputLines);
    println(wsOutputStr);
  }
}