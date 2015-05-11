package edu.nus.worksheet

import java.io.{ PrintWriter, BufferedReader, InputStreamReader, IOException };
import java.net.{ InetAddress, ServerSocket, Socket, SocketException }
import java.util.{ Timer, TimerTask };
import scala.io.Source;
import argonaut._, Argonaut._
import edu.nus.worksheet.instrumentor.WorksheetifyException;

object WorksheetifyServer {
  // Request: JSON
  //       inputtype = "text" | "filepath"
  //           input = C code | path
  //      outputtype = "text" | "json-outputlist" | "json-outputtext"
  //   maxIterations = "number"
  //     outputLimit = "number"
  case class Request(inputType : String,
                     inputValue : String,
                     outputType : String,
                     maxIterations : Option[Int],
                     outputLimit : Option[Int])

  implicit def RequestCodecJson: CodecJson[Request] =
    casecodec5(Request.apply, Request.unapply)("inputtype", "input", "outputtype", "maxiterations", "outputlimit");

  // Generate a response given a (valid) WorksheetOutput object.
  // Assumes worksheet has finished processing.
  private[WorksheetifyServer] def responseFor(outputType : String, wsOutput : WorksheetOutput) : String = {
    outputType match {
      // Output raw text result.
      case "text" =>
        wsOutput.generateWorksheetOutput();

      // Return JSON, with list-of-lists as a result.
      case "json-outputlist" => {
        // Turn from mut.map[Int, mut.List[String]]
        // into List[List[String]]
        val lsOfLs = (1 to wsOutput.outputPerLine.keys.max).map({ i =>
          wsOutput.outputPerLine.getOrElse(i, scala.collection.mutable.MutableList[String]()).toList;
        }).toList;

        Map("result" -> lsOfLs).asJson.nospaces;
      }

      // Return JSON, with raw text result as an entry.
      case "json-outputtext" =>
        Map("result-src" -> wsOutput.generateWorksheetOutput()).asJson.nospaces;

      // Invalid output type.
      case _ =>
        Map("error" -> "invalid output type").asJson.nospaces;
    }
  }

  // Is there a tidier way to achieve the same result, here?
  // Generate a response for when an exception was thrown.
  private[WorksheetifyServer] def responseFor(outputType : String, ex : WorksheetifyException) : String = {
    val lineToAddMessageAt = 0;
    val inputLines = ex.originalProgram.lines.toSeq;

    val inputWithMessage = inputLines.zipWithIndex.map({ case (l, i) =>
      if (i == lineToAddMessageAt)
        l + s" // Failed to instrument";
      else
        l;
    }).mkString("\n");

    outputType match {
      // Output raw text result.
      case "text" =>
        inputWithMessage;

      // Return JSON, with list-of-lists as a result.
      case "json-outputlist" => {
        // May be helpful to add the message at the line the user is
        // 'focussed at'.

        val lsInputWithMessage = inputLines.zipWithIndex.map({ case (l, i) =>
          if (i == lineToAddMessageAt)
            List[String](s" // Failed to instrument");
          else
            List[String]();
        }).toList;

        Map("result" -> lsInputWithMessage).asJson.nospaces;
      }

      // Return JSON, with raw text result as an entry.
      case "json-outputtext" =>
        Map("result-src" -> inputWithMessage).asJson.nospaces;

      // Invalid output type.
      case _ =>
        Map("error" -> "invalid output type").asJson.nospaces;
    }
  }

  private[WorksheetifyServer] def responseForProgram(inputProgram : String,
                                                     outputType : String,
                                                     maxIter : Int,
                                                     outputLimit : Int) : String = {
    try {
      val wsOutput = Worksheetify.worksheetifyForInput(inputProgram,
                                                       maxIterations = maxIter,
                                                       maxOutputPerLine = outputLimit);
      wsOutput.generateWorksheetOutput(); // block until done.

      return responseFor(outputType, wsOutput);
    } catch {
      case ex : WorksheetifyException => {
        Worksheetify.dumpExceptionToFile(ex);

        return responseFor(outputType, ex);
      }
      case e : Throwable => throw e;
    }
  }

  private[WorksheetifyServer] def responseForMaybeRequest(option : Option[Request]) : String = {
    option match {
      case Some(req) => {
        val maxIter     = req.maxIterations.getOrElse(Worksheetify.MaxIterationsDefault);
        val outputLimit = req.maxIterations.getOrElse(Worksheetify.OutputLimitDefault);

        req match {
          // `text` input type means the given input value is
          // the C source to instrument.
          case Request("text", inputProgram, outputType, _, _) =>
            responseForProgram(inputProgram, outputType, maxIter, outputLimit);

          // `text` input type means the given input value is
          // path to the C source file to instrument.
          case Request("filepath", inputFilename, outputType, _, _) => {
            val inputProgram = Source.fromFile(inputFilename).mkString;
            responseForProgram(inputProgram, outputType, maxIter, outputLimit);
          }

          // Other cases are invalid.
          case _ =>
            Map("error" -> "invalid request JSON").asJson.nospaces;
        }
      }
      case None =>
        Map("error" -> "could not parse request").asJson.nospaces;
    }
  }

  private[WorksheetifyServer] def handleClient(socket : Socket) : Unit = {
    try {
      // when we do accept a socket, wait for a JSON request.
      // Read all the data from the input stream.
      val in = socket.getInputStream();
      val out = new PrintWriter(socket.getOutputStream());
      val requestStr = Source.fromInputStream(in).mkString;

      val option : Option[Request] = Parse.decodeOption[Request](requestStr.toString);

      // respond-with, either
      //  - raw text
      //  - json-of: text OR list-of-outputs.

      val response = responseForMaybeRequest(option);
      out.println(response);
      out.flush();

      in.close();
      out.close();

      socket.close();
    } catch {
      case ioEx : IOException =>
        ioEx.printStackTrace();
    }
  }

  def startServer(port : Int) : Unit = {
    try {
      // Start listening for sockets on port..
      val listener = new ServerSocket(port);

      // Start a timer
      var idleTimer : Timer = null;

      def scheduleTimerToExit() : Unit = {
        idleTimer = new Timer();

        // Schedule server to exit after 5 idle minutes.
        idleTimer.schedule(new TimerTask() {
          override def run() : Unit = {
            System.exit(0);
          }
        }, 5 * 60 * 1000);
      }

      scheduleTimerToExit();

      while (true) {
        val socket = listener.accept();

        handleClient(socket);

        // Reset the timer.
        idleTimer.cancel();
        scheduleTimerToExit();
      }

      listener.close();
    } catch {
      case e: IOException =>
        System.err.println(s"Could not listen on port: $port");
        e.printStackTrace();
        System.exit(-1);
    }
  }

  def main(args : Array[String]) : Unit = {
    if (args.length < 1) {
      printf("scala MyServer port");
      return;
    }

    val port: Int = args(0).toInt;

    startServer(port);
  }
}

