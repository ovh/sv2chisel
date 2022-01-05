// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import io.circe._
import logger.EasyLogging

// helpers for enum
trait EnumDecoder {
  this : Enumeration =>
  def ctx: String
  def decode: Decoder[this.Value] = Decoder.instance(c => {
    c.as[String] match {
      case Right(s) => 
        try { 
          Right(this.withName(s))
        } catch {
          case _: java.util.NoSuchElementException => 
            Left(DecodingFailure(s"Unknown value $s in $ctx ", c.history))
        }
      case Left(DecodingFailure(s, l)) => Left(DecodingFailure(s"Cannot parse properly $ctx (not a string). $s", l))
    }
  })
}



case class RemoveConcatOptions(
  useChiselCat: Boolean = true
){
  /** Decoder for circe parsing from yaml */
  def decode: Decoder[RemoveConcatOptions] = Decoder.instance(c => {
    val default = RemoveConcatOptions()
    for {
      useChiselCat <- c.getOrElse[Boolean]("useChiselCat")(default.useChiselCat)
    } yield {
      RemoveConcatOptions(useChiselCat)
    }
  })
}

case class TopLevelChiselGenerator(
  name: String,
  withWrapper: Boolean = true
){
  /** Decoder for circe parsing from yaml */
  def decode: Decoder[TopLevelChiselGenerator] = Decoder.instance(c => {
    for {
      name <- c.get[String]("name")
      withWrapper <- c.getOrElse[Boolean]("withWrapper")(true)
    } yield {
      TopLevelChiselGenerator(name, withWrapper)
    }
  })
}

case class ChiselizerOptions(
  toCamelCase: Boolean = false,
  ignoreEnumFieldScalastyle: Boolean = false,
  unpackedEmissionStyle:ChiselizerOptions.UnpackedEmissionStyle.Value = ChiselizerOptions.UnpackedEmissionStyle.default,
  topLevelChiselGenerators: Seq[TopLevelChiselGenerator] = Seq(),
  baseBlackboxRessourcePath: Option[String] = None
) extends EasyLogging {
  /** Decoder for circe parsing from yaml */
  def decode: Decoder[ChiselizerOptions] = Decoder.instance(c => {
    val default = ChiselizerOptions()
    implicit val d1 = ChiselizerOptions.UnpackedEmissionStyle.decode
    implicit val d2 = TopLevelChiselGenerator("").decode
    for {
      unpackedEmissionStyle <- c.getOrElse[ChiselizerOptions.UnpackedEmissionStyle.Value]("unpackedEmissionStyle")(default.unpackedEmissionStyle)
      topLevelChiselGenerators <- c.getOrElse[Seq[TopLevelChiselGenerator]]("topLevelChiselGenerators")(Seq())
      baseBlackboxRessourcePath <- c.getOrElse[String]("baseBlackboxRessourcePath")("")
      toCamelCase <- c.getOrElse[Boolean]("toCamelCase")(default.toCamelCase)
      ignoreEnumFieldScalastyle <- c.getOrElse[Boolean]("ignoreEnumFieldScalastyle")(default.ignoreEnumFieldScalastyle)
    } yield {
      val bbPath = baseBlackboxRessourcePath match {
        case "" => None
        case s if(s.contains("/resources/")) => Some(s)
        case _ => 
          critical(s"Ignoring non-compliant baseBlackboxRessourcePath $baseBlackboxRessourcePath (must contain /resources/)")
          None
          
      }
      ChiselizerOptions(toCamelCase, ignoreEnumFieldScalastyle, unpackedEmissionStyle, topLevelChiselGenerators, bbPath)
    }
  })
}
object ChiselizerOptions {
  object UnpackedEmissionStyle extends Enumeration with EnumDecoder {
    val Reg, Mem, SyncReadMem = Value
    def default = Reg
    def ctx = "unpackedEmissionStyle for Chiselizer"
  }
}


case class LegalizeParamDefaultOptions(
  legalizeMethod: LegalizeParamDefaultOptions.LegalizeMethod.Value = LegalizeParamDefaultOptions.LegalizeMethod.default
){
  /** Decoder for circe parsing from yaml */
  def decode: Decoder[LegalizeParamDefaultOptions] = Decoder.instance(c => {
    val default = LegalizeParamDefaultOptions()
    implicit val decoder = LegalizeParamDefaultOptions.LegalizeMethod.decode
    for {
      legalizeMethod <- c.getOrElse[LegalizeParamDefaultOptions.LegalizeMethod.Value]("legalizeMethod")(default.legalizeMethod)
    } yield {
      LegalizeParamDefaultOptions(legalizeMethod)
    }
  })
}


object LegalizeParamDefaultOptions {
  object LegalizeMethod extends Enumeration with EnumDecoder {
    val comment, overrideOption, moveOrComment, moveOrOverride = Value
    def default = moveOrOverride
    def ctx = "legalizeMethod for LegalizeParamDefaults"
  }
}

case class TranslationOptions(
  legalizeParamDefault: LegalizeParamDefaultOptions = LegalizeParamDefaultOptions(),
  removeConcat: RemoveConcatOptions = RemoveConcatOptions(),
  chiselizer: ChiselizerOptions = ChiselizerOptions()
) {
  /** Decoder for circe parsing from yaml */
  def decode: Decoder[TranslationOptions] = Decoder.instance(c => {
    val default = TranslationOptions()
    implicit val d1 = LegalizeParamDefaultOptions().decode
    implicit val d2 = RemoveConcatOptions().decode
    implicit val d3 = ChiselizerOptions().decode
    for {
      legalizeParamDefault <- c.getOrElse[LegalizeParamDefaultOptions]("LegalizeParamDefaults")(default.legalizeParamDefault)
      removeConcat <- c.getOrElse[RemoveConcatOptions]("RemoveConcats")(default.removeConcat)
      chiselizer <- c.getOrElse[ChiselizerOptions]("Chiselizer")(default.chiselizer)
    } yield {
      TranslationOptions(legalizeParamDefault, removeConcat, chiselizer)
    }
  })
}
