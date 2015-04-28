package edu.nus.worksheet.instrumentor

import java.io._;
import argonaut._, Argonaut._
import CTypeCodec._;
import StringConstruction._;

 
object HeaderUtils {
  def cachedHeaderFilename(header : String) : String =
    s".worksheet_$header.cached"

  def saveHeaderTypes(header : String, typedefs : Seq[(String, CType)]) {
    val ht : (String, Int, List[(String, CType)]) =
      (header, headerChecksum(header), typedefs.toList);

    val encoded : String = ht.asJson.nospaces;

    val out = new PrintWriter(new File(cachedHeaderFilename(header)));

    try {
      out.println(encoded);
    } finally {
      out.close();
    }
  }

  def loadHeaderTypes(header : String) : Option[Seq[(String, CType)]] = {
    val hc = headerChecksum(header);

    val cachedFile = new File(cachedHeaderFilename(header));

    if (cachedFile.exists()) {
      val source = scala.io.Source.fromFile(cachedFile);
      val lines = try source.getLines mkString "\n" finally source.close()

      // Eclipse gives this an error, though the code runs fine?
      // Why, pickle, why?
      lines.decodeOption[(String, Int, List[(String, CType)])] match {
        case Some((hdr, hdrChecksum, typedefs)) => {
          if (hdrChecksum == hc) {
            Some(typedefs);
          } else {
            None;
          }
        }
        case unknown => None;
      }
    } else {
      None
    }
  }

  def getWithPreprocessedHeader[A](header : String, f : String => A) : Option[A] = {
    val input = s"#include <$header>";
    val prog = new CProgram(input);

    return prog.preprocessed() match {
      case Some(processed) => try {
        Some(f(processed));
      } catch {
        // Some error occured while processing the header file.
        // e.g. some feature our tools don't understand.
        case _ : Throwable => {
          None;
        }
      }
      case None => None;
    }
  }

  def getCTypesOfHeader(header : String) : Seq[CType] = {
    getWithPreprocessedHeader(header, getCTypesOf) match {
      case Some(ctypes) => ctypes;
      case None => Seq();
    }
  }

  def getTypedefsOfHeaderUncached(header : String) : Iterable[(String, CType)] = {
    getWithPreprocessedHeader(header, getTypedefsOf) match {
      case Some(typedefs) => typedefs.toIterable;
      case None => Seq();
    }
  }

  def getTypedefsOfHeader(header : String) : Iterable[(String, CType)] = {
    loadHeaderTypes(header) match {
      case Some(typedefs) => typedefs;
      case None => {
        // Cache either doesn't exist,
        // or is invalid.
        val tds = getTypedefsOfHeaderUncached(header);
        saveHeaderTypes(header, tds.toSeq);
        return tds;
      }
    }
  }

  def getTypedefNamesOfHeader(header : String) : Iterable[String] = {
    getTypedefsOfHeader(header).map(_._1);
  }

  def headerChecksum(header : String) : Int = {
    getWithPreprocessedHeader(header, _.hashCode()) match {
      case Some(hc) => hc;
      case None => 0;
    }
  }

  def addTypedefsOfHeaderToScope(header : String, scope : Scope) {
    val typedefs = getTypedefsOfHeader(header);

    for ((typename, ct) <- typedefs) {
      scope.defineTypedef(typename, ct);
    }
  }

  
  def main(args : Array[String]) : Unit = {
    // String that can be sent down a wire
    println("Get CTypes of Stdio.h");
    getTypedefsOfHeader("stdio.h");
    println("Get CTypes of Stdio.h");
    getTypedefsOfHeader("stdio.h");
    println("Get CTypes of Stdio.h");
    getTypedefsOfHeader("stdio.h");

//    val pt = PrimitiveType("x", "int");
//    val ptrT = new PointerType("p", pt);
//    
//    val res = pt.asJson;
//    val resStr : String = pt.asJson.spaces2;
//
//    println(res.spaces2);
//    
//    val res2 = ptrT.asJson;
//    println(res2.spaces2);
//
//    res2.spaces2.decodeOption[CType] match {
//      case Some(ct) => {
//        println(s"Decoded res2 to $ct");
//      }
//      case None =>
//        println("Couldn't decode res2.");
//    }
  }
}