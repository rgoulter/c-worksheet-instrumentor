package edu.nus.worksheet.instrumentor;

import argonaut.{Argonaut, DecodeJson, EncodeJson};
import Argonaut.{StringToStringWrap, jEmptyObject};

object CTypeCodec {
  implicit def CTypeEncodeJson: EncodeJson[CType] =
    EncodeJson((ct: CType) =>
      ct match {
        case pt: PrimitiveType =>
          PrimitiveTypeEncodeJson(pt);
        case at: ArrayType =>
          ArrayTypeEncodeJson(at);
        case pt: PointerType =>
          PointerTypeEncodeJson(pt);
        case st: StructType =>
          StructTypeEncodeJson(st)
        case et: EnumType =>
          EnumTypeEncodeJson(et);
        case ft: FunctionType =>
          FunctionTypeEncodeJson(ft);
        case fd: ForwardDeclarationType =>
          ForwardDeclarationTypeEncodeJson(fd);
        case va: VarArgType =>
          ("kind" := "vararg") ->: jEmptyObject;
        case null =>
          ("kind" := "null") ->: jEmptyObject;
        case _ =>
          throw new UnsupportedOperationException("Couldn't encode " + ct);
      }
    );

  implicit def CTypeDecodeJson: DecodeJson[CType] =
    DecodeJson(c =>
      for {
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
          case "enum" =>
            EnumTypeDecodeJson(c);
          case "function" =>
            FunctionTypeDecodeJson(c);
          case "forward-declaration" =>
            ForwardDeclarationTypeDecodeJson(c);
          case "vararg" =>
            // I don't understand the argonaut types
            DecodeJson(c =>
              for {
                id <- (c --\ "kind").as[String]
              } yield VarArgType()
            )(c);
          case "null" =>
            DecodeJson(c =>
              for {
                id <- (c --\ "kind").as[String]
              } yield null
            )(c);
          case _ =>
            throw new UnsupportedOperationException(s"Couldn't decode '$kind'");
        }
      } yield result
    );

  implicit def PrimitiveTypeEncodeJson: EncodeJson[PrimitiveType] =
    EncodeJson((ct: PrimitiveType) =>
      ("id" := ct.id) ->:
        ("ctype" := ct.ctype) ->:
        ("kind" := "primitive") ->:
        jEmptyObject
    );

  def PrimitiveTypeDecodeJson: DecodeJson[PrimitiveType] =
    DecodeJson(c =>
      for {
        id <- (c --\ "id").as[Option[String]]
        ctype <- (c --\ "ctype").as[String]
      } yield PrimitiveType(id, ctype)
    );

  implicit def ArrayTypeEncodeJson: EncodeJson[ArrayType] =
    EncodeJson((ct: ArrayType) =>
      ("id" := ct.id) ->:
        ("index" := ct.index) ->:
        ("n" := ct.n) ->:
        ("of" := ct.of) ->:
        ("kind" := "array") ->:
        jEmptyObject
    );

  def ArrayTypeDecodeJson: DecodeJson[ArrayType] =
    DecodeJson(c =>
      for {
        id <- (c --\ "id").as[Option[String]]
        index <- (c --\ "index").as[Option[String]]
        n <- (c --\ "n").as[Option[String]]
        of <- (c --\ "of").as[CType]
      } yield ArrayType(id, index, n, of)
    );

  implicit def PointerTypeEncodeJson: EncodeJson[PointerType] =
    EncodeJson((ct: PointerType) =>
      ("id" := ct.id) ->:
        ("of" := ct.of) ->:
        ("kind" := "pointer") ->:
        jEmptyObject
    );

  def PointerTypeDecodeJson: DecodeJson[PointerType] =
    DecodeJson(c =>
      for {
        id <- (c --\ "id").as[Option[String]]
        of <- (c --\ "of").as[CType]
      } yield PointerType(id, of)
    );

  implicit def StructTypeEncodeJson: EncodeJson[StructType] =
    EncodeJson((ct: StructType) =>
      ("id" := ct.id) ->:
        ("structOrUnion" := ct.structOrUnion) ->:
        ("tag" := ct.structTag) ->:
        ("members" := ct.members.toList) ->:
        ("kind" := "struct") ->:
        jEmptyObject
    );

  def StructTypeDecodeJson: DecodeJson[StructType] =
    DecodeJson(c =>
      for {
        id <- (c --\ "id").as[Option[String]]
        sOrU <- (c --\ "structOrUnion").as[String]
        tag <- (c --\ "tag").as[Option[String]]
        members <- (c --\ "members").as[List[CType]]
      } yield StructType(id, sOrU, tag, members)
    );

  implicit def EnumTypeEncodeJson: EncodeJson[EnumType] =
    EncodeJson((ct: EnumType) =>
      ("id" := ct.id) ->:
        ("tag" := ct.enumTag) ->:
        ("constants" := ct.constants.toList) ->:
        ("kind" := "enum") ->:
        jEmptyObject
    );

  def EnumTypeDecodeJson: DecodeJson[EnumType] =
    DecodeJson(c =>
      for {
        id <- (c --\ "id").as[Option[String]]
        tag <- (c --\ "tag").as[Option[String]]
        constants <- (c --\ "constants").as[List[String]]
      } yield EnumType(id, tag, constants)
    );

  implicit def FunctionTypeEncodeJson: EncodeJson[FunctionType] =
    EncodeJson((ct: FunctionType) =>
      ("id" := ct.id) ->:
        ("return" := ct.returnType) ->:
        ("parameters" := ct.parameterTypes.toList) ->:
        ("kind" := "function") ->:
        jEmptyObject
    );

  def FunctionTypeDecodeJson: DecodeJson[FunctionType] =
    DecodeJson(c =>
      for {
        id <- (c --\ "id").as[Option[String]]
        rtn <- (c --\ "return").as[CType]
        params <- (c --\ "parameters").as[List[CType]]
      } yield FunctionType(id, rtn, params)
    );

  implicit def ForwardDeclarationTypeEncodeJson
      : EncodeJson[ForwardDeclarationType] =
    EncodeJson((ct: ForwardDeclarationType) =>
      ("id" := ct.id) ->:
        ("tag" := ct.tag) ->:
        ("kind" := "forward-declaration") ->:
        jEmptyObject
    );

  def ForwardDeclarationTypeDecodeJson: DecodeJson[ForwardDeclarationType] =
    DecodeJson(c =>
      for {
        id <- (c --\ "id").as[Option[String]]
        tag <- (c --\ "tag").as[String]
      } yield ForwardDeclarationType(id, tag)
    );

  // EnumType, FunctionType, VarArgType ... These we can do later.

}
