package edu.nus.worksheet.instrumentor

import argonaut._, Argonaut._

object CTypeCodec {
  implicit def CTypeEncodeJson : EncodeJson[CType] =
    EncodeJson((ct : CType) =>
                 ct match {
                   case pt : PrimitiveType =>
                     PrimitiveTypeEncodeJson(pt);
                   case at : ArrayType =>
                     ArrayTypeEncodeJson(at);
                   case pt : PointerType =>
                     PointerTypeEncodeJson(pt);
                   case st : StructType =>
                     StructTypeEncodeJson(st)
                   case fd : ForwardDeclarationType =>
                     ForwardDeclarationTypeEncodeJson(fd);
                   case _ =>
                     throw new UnsupportedOperationException("Couldn't encode " + ct);
                 });

  implicit def CTypeDecodeJson : DecodeJson[CType] =
    DecodeJson(c => for {
      kind <- (c --\ "kind").as[String]
      result <- kind match {
        case "primitive" =>
          PrimitiveTypeDecodeJson(c);
        case "array" =>
          ArrayTypeDecodeJson(c)
        case "pointer" =>
          PointerTypeDecodeJson(c);
        case "struct" =>
          StructTypeDecodeJson(c);
        case "forward-declaration" =>
          ForwardDeclarationTypeDecodeJson(c);
        case _ =>
          throw new UnsupportedOperationException(s"Couldn't decode '$kind'");
      }
    } yield result);

  implicit def PrimitiveTypeEncodeJson : EncodeJson[PrimitiveType] =
    EncodeJson((ct : PrimitiveType) =>
                 ("id" := ct.id) ->:
                 ("ctype" := ct.ctype) ->:
                 ("kind" := "primitive") ->:
                 jEmptyObject);

  def PrimitiveTypeDecodeJson : DecodeJson[PrimitiveType] =
    DecodeJson(c => for {
      id <- (c --\ "id").as[String]
      ctype <- (c --\ "ctype").as[String]
    } yield PrimitiveType(id, ctype));

  implicit def ArrayTypeEncodeJson : EncodeJson[ArrayType] =
    EncodeJson((ct : ArrayType) =>
                 ("id" := ct.id) ->:
                 ("idx" := ct.index) ->:
                 ("n" := ct.n) ->:
                 ("of" := ct.of) ->:
                 ("kind" := "array") ->:
                 jEmptyObject);

  def ArrayTypeDecodeJson : DecodeJson[ArrayType] =
    DecodeJson(c => for {
      id <- (c --\ "id").as[String]
      index <- (c --\ "index").as[String]
      n <- (c --\ "n").as[String]
      of <- (c --\ "of").as[CType]
    } yield ArrayType(id, index, n, of));

  implicit def PointerTypeEncodeJson : EncodeJson[PointerType] =
    EncodeJson((ct : PointerType) =>
                 ("id" := ct.id) ->:
                 ("of" := ct.of) ->:
                 ("kind" := "pointer") ->:
                 jEmptyObject);

  def PointerTypeDecodeJson : DecodeJson[PointerType] =
    DecodeJson(c => for {
      id <- (c --\ "id").as[String]
      of <- (c --\ "of").as[CType]
    } yield PointerType(id, of));

  implicit def StructTypeEncodeJson : EncodeJson[StructType] =
    EncodeJson((ct : StructType) =>
                 ("id" := ct.id) ->:
                 ("structOrUnion" := ct.structOrUnion) ->:
                 ("tag" := ct.structTag) ->:
                 ("members" := ct.members.toList) ->:
                 ("kind" := "struct") ->:
                 jEmptyObject);

  def StructTypeDecodeJson : DecodeJson[StructType] =
    DecodeJson(c => for {
      id <- (c --\ "id").as[String]
      sOrU <- (c --\ "structOrUnion").as[String]
      tag <- (c --\ "tag").as[String]
      members <- (c --\ "members").as[List[CType]]
    } yield StructType(id, sOrU, tag, members));

  implicit def ForwardDeclarationTypeEncodeJson : EncodeJson[ForwardDeclarationType] =
    EncodeJson((ct : ForwardDeclarationType) =>
                 ("id" := ct.id) ->:
                 ("tag" := ct.tag) ->:
                 ("kind" := "forward-declaration") ->:
                 jEmptyObject);

  def ForwardDeclarationTypeDecodeJson : DecodeJson[ForwardDeclarationType] =
    DecodeJson(c => for {
      id <- (c --\ "id").as[String]
      tag <- (c --\ "tag").as[String]
    } yield ForwardDeclarationType(id, tag));

  // EnumType, FunctionType, VarArgType ... These we can do later.


}