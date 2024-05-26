package edu.nus.worksheet.instrumentor.test.integration

import java.net.Socket
import java.net.InetSocketAddress
import java.net.ConnectException
import java.net.SocketTimeoutException
import java.io.*

import java.nio.file.{Files, Path, Paths}
import java.nio.charset.StandardCharsets

import scala.io.Source
import scala.sys.process.*

import argonaut.Argonaut.*
import argonaut.Json

import org.scalatest.*
import flatspec.*

import edu.nus.worksheet.*
import edu.nus.worksheet.instrumentor.*

class WorksheetifyServerIntegrationSpec extends AnyFlatSpec {
  val worksheetifyServerPort = 10010;

  def waitUntilPortFree(
      host: String,
      port: Int,
      maxRetries: Int = 10
  ): Boolean = {
    println("waiting until port free");
    var retries = 0;
    while retries < maxRetries do {
      val socket = new Socket();
      try {
        println("attempting connection");
        socket.connect(new InetSocketAddress(host, port), 200);
        println("successfully connected: port is available");
        // able to connect; server still running
      } catch {
        case _: ConnectException => {
          println("caught ConnectException: port is free");
          return true;
        }
        case _: SocketTimeoutException => {
          println("caught SocketTimeoutException: port is busy");
        }
      } finally {
        socket.close();
      }

      retries += 1;
      println(f"waiting / retrying ${retries}/${maxRetries}");
      Thread.sleep(1000); // Wait for 1 second before retrying
    }
    return false;
  }

  def sendRequestToServer(
      hostname: String,
      port: Int,
      message: String
  ): Option[String] = {
    var clientSocket: Socket = null
    try {
      clientSocket = new Socket(hostname, port)

      println(f"sending: ${message}");

      val outputStream = new DataOutputStream(clientSocket.getOutputStream)
      outputStream.write(message.getBytes("UTF-8"))
      outputStream.flush()
      clientSocket.shutdownOutput()

      val inputStream = new DataInputStream(clientSocket.getInputStream)
      val buffer = new Array[Byte](8)
      var received = ""
      var bytesRead = inputStream.read(buffer)
      while bytesRead > 0 do {
        received += new String(buffer, 0, bytesRead, "UTF-8")
        bytesRead = inputStream.read(buffer)
      }

      println(f"received: ${received}");

      Some(received)
    } catch {
      case _: Exception => None
    } finally {
      if clientSocket != null then {
        clientSocket.close()
      }
    }
  }

  override def withFixture(test: NoArgTest) = {
    val installedBinPath = Paths.get(
      "build/install/c-worksheet-instrumentor/bin/c-worksheetify-server"
    );
    val command: Seq[String] =
      if System.getProperty("os.name").toLowerCase.contains("windows") then
        Seq(
          s"${installedBinPath.toString}.bat",
          worksheetifyServerPort.toString
        )
      else Seq(installedBinPath.toString, worksheetifyServerPort.toString)

    val serverProcess = Process(command).run();

    // Wait for server to be ready
    Thread.sleep(2000);

    try {
      super.withFixture(test);
    } finally {
      println("destroying server process");
      serverProcess.destroy();

      // kludge: the server process isn't destroyed on Windows.
      // println("waiting for server process to finish");
      // val exitVal = serverProcess.exitValue();

      waitUntilPortFree("localhost", worksheetifyServerPort);
    }
  }

  def createTempFileWithContents(contents: String): Path = {
    val tempFile = Files.createTempFile("temp", ".txt")
    Files.write(tempFile, contents.getBytes(StandardCharsets.UTF_8))
    tempFile
  }

  "c-worksheetify-server" should "return worksheet output for inputtype=filepath, outputtype=text" taggedAs (IntegrationTest) in {
    val snapshot = "hello.c";

    val inputProgram = Source.fromResource(f"snapshots/${snapshot}").mkString;

    val expectedOutput =
      Source.fromResource(f"snapshots/${snapshot}.expected").mkString;

    val inputProgramPath = createTempFileWithContents(inputProgram);

    val message = Json(
      "inputtype" := "filepath",
      "input" := inputProgramPath.toString,
      "outputtype" := "text"
    ).nospaces

    val actualOutput =
      sendRequestToServer("localhost", worksheetifyServerPort, message).get;

    assertResult(expectedOutput)(actualOutput.replaceAll("\\r\\n", "\n"));
  }

  it should "return JSON output for inputtype=filepath, outputtype=json-outputlist" taggedAs (IntegrationTest) in {
    val snapshot = "hello.c";

    val inputProgram = Source.fromResource(f"snapshots/${snapshot}").mkString;

    val expectedOutput = """{"result":[[],[],[],["Hello World"]]}
""";

    val inputProgramPath = createTempFileWithContents(inputProgram);

    val message = Json(
      "inputtype" := "filepath",
      "input" := inputProgramPath.toString,
      "outputtype" := "json-outputlist"
    ).nospaces

    val actualOutput =
      sendRequestToServer("localhost", worksheetifyServerPort, message).get;

    assertResult(expectedOutput)(actualOutput.replaceAll("\\r\\n", "\n"));
  }

  it should "return JSON output for inputtype=text, outputtype=json-outputlist" taggedAs (IntegrationTest) in {
    val snapshot = "hello.c";

    val inputProgram = Source.fromResource(f"snapshots/${snapshot}").mkString;

    val expectedOutput = """{"result":[[],[],[],["Hello World"]]}
""";

    val message = Json(
      "inputtype" := "text",
      "input" := inputProgram,
      "outputtype" := "json-outputlist"
    ).nospaces

    val actualOutput =
      sendRequestToServer("localhost", worksheetifyServerPort, message).get;

    assertResult(expectedOutput)(actualOutput.replaceAll("\\r\\n", "\n"));
  }

}
