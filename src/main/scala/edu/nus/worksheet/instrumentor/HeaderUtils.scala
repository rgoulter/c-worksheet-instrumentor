package edu.nus.worksheet.instrumentor;

import java.io.{File, PrintWriter};

import argonaut.{Argonaut, DecodeJson, EncodeJson};
import Argonaut.{
  StringToParseWrap,
  StringToStringWrap,
  ToJsonIdentity,
  jEmptyObject
};

import org.antlr.v4.runtime.tree.ParseTreeWalker;

import CTypeCodec.*;
import StringConstruction.{getCTypesOf, getScopeStuffOf, getTypedefsOf};

object HeaderUtils {
  class HeaderCachePayload(
      val symbols: Iterable[CType],
      val structs: Iterable[StructType],
      val enums: Iterable[EnumType],
      val typedefs: Iterable[(String, CType)]
  );

  implicit def HeaderCachePayloadEncodeJson: EncodeJson[HeaderCachePayload] =
    EncodeJson((payload: HeaderCachePayload) =>
      ("symbols" := payload.symbols.toList) ->:
        ("structs" := payload.structs.toList) ->:
        ("enums" := payload.enums.toList) ->:
        ("typedefs" := payload.typedefs.toList) ->:
        jEmptyObject;
    );

  implicit def HeaderCachePayloadDecodeJson: DecodeJson[HeaderCachePayload] =
    DecodeJson(c =>
      for {
        symbols <- (c --\ "symbols").as[List[CType]];
//      structs <- (c --\ "structs").as[List[StructType]];
//      enums <- (c --\ "enums").as[List[EnumType]];
        typedefs <- (c --\ "typedefs").as[List[(String, CType)]];
      } yield new HeaderCachePayload(symbols, null, null, typedefs);
    );

  def cachedHeaderFilename(header: String): String =
    s".worksheet_$header.cached";

  private[HeaderUtils] def saveHeaderTypes(
      header: String,
      symbols: Iterable[CType],
      structs: Iterable[StructType],
      enums: Iterable[EnumType],
      typedefs: Iterable[(String, CType)]
  ): Unit = {
    val ht: (String, Int, HeaderCachePayload) =
      (
        header,
        headerChecksum(header),
        new HeaderCachePayload(
          symbols.toList,
          structs.toList,
          enums.toList,
          typedefs.toList
        )
      );

    val encoded: String = ht.asJson.nospaces;

    val out = new PrintWriter(new File(cachedHeaderFilename(header)));

    try {
      out.println(encoded);
    } finally {
      out.close();
    }
  }

  private[HeaderUtils] def loadHeaderTypes(
      header: String
  ): Option[HeaderCachePayload] = {
    val hc = headerChecksum(header);

    val cachedFile = new File(cachedHeaderFilename(header));

    if cachedFile.exists() then {
      val source = scala.io.Source.fromFile(cachedFile);
      val lines =
        try source.getLines().mkString("\n")
        finally source.close();

      // Eclipse gives this an error, though the code runs fine?
      // Why, pickle, why?
      lines.decodeOption[(String, Int, HeaderCachePayload)] match {
        case Some((hdr, hdrChecksum, payload)) => {
          if hdrChecksum == hc then {
            Some(payload);
          } else {
            None;
          }
        }
        case unknown => None;
      }
    } else {
      None;
    }
  }

  def getWithPreprocessedHeader[A](
      header: String,
      f: String => A
  ): Option[A] = {
    val input = s"#include <$header>";
    val prog = new CProgram(input);

    return prog.preprocessed() match {
      case Some(processed) =>
        try {
          Some(f(processed));
        } catch {
          // Some error occured while processing the header file.
          // e.g. some feature our tools don't understand.
          case _: Throwable => {
            None;
          }
        }
      case None => None;
    }
  }

  def getCTypesOfHeader(header: String): Seq[CType] = {
    getWithPreprocessedHeader(header, getCTypesOf) match {
      case Some(ctypes) => ctypes;
      case None         => Seq();
    }
  }

  def getTypedefsOfHeaderUncached(header: String): Iterable[(String, CType)] = {
    getWithPreprocessedHeader(header, getTypedefsOf) match {
      case Some(typedefs) => typedefs.iterator.to(Iterable);
      case None           => Seq();
    }
  }

  def getHeaderCache(header: String): HeaderCachePayload = {
    loadHeaderTypes(header) match {
      case Some(typedefs) => typedefs;
      case None => {
        // Cache either doesn't exist,
        // or is invalid.
        val (syms, structs, enums, typedefs) = getScopeStuffOfHeader(header);
        saveHeaderTypes(header, syms, structs, enums, typedefs.toSeq);
        return new HeaderCachePayload(syms, structs, enums, typedefs);
      }
    }
  }

  def getSymbolsOfHeader(header: String): Iterable[CType] =
    getHeaderCache(header).symbols;

  def getTypedefsOfHeader(header: String): Iterable[(String, CType)] =
    getHeaderCache(header).typedefs;

  def getTypedefNamesOfHeader(header: String): Iterable[String] = {
    getTypedefsOfHeader(header).map(_._1);
  }

  def getScopeStuffOfHeader(header: String): (
      Iterable[CType],
      Iterable[StructType],
      Iterable[EnumType],
      Iterable[(String, CType)]
  ) =
    getWithPreprocessedHeader(header, getScopeStuffOf) match {
      case Some(data) => data;
      case None       => (Seq(), Seq(), Seq(), Seq());
    }

  def headerChecksum(header: String): Int = {
    getWithPreprocessedHeader(header, _.hashCode()) match {
      case Some(hc) => hc;
      case None     => 0;
    }
  }

  def addTypedefsOfHeaderToScope(header: String, scope: Scope): Unit = {
    val typedefs = getTypedefsOfHeader(header);

    for (typename, ct) <- typedefs do {
      scope.defineTypedef(typename, ct);
    }
  }

  def addSymbolsOfHeaderToScope(header: String, scope: Scope) =
    getSymbolsOfHeader(header).foreach(scope.defineSymbol);
}
