// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import io.circe._

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

case class ChiselizerOptions(
  unpackedEmissionStyle:ChiselizerOptions.UnpackedEmissionStyle.Value = ChiselizerOptions.UnpackedEmissionStyle.default,
  addTopLevelChiselGenerator: Option[String] = None
){
  /** Decoder for circe parsing from yaml */
  def decode: Decoder[ChiselizerOptions] = Decoder.instance(c => {
    val default = ChiselizerOptions()
    implicit val decoder = ChiselizerOptions.UnpackedEmissionStyle.decode
    for {
      unpackedEmissionStyle <- c.getOrElse[ChiselizerOptions.UnpackedEmissionStyle.Value]("unpackedEmissionStyle")(default.unpackedEmissionStyle)
      addTopLevelChiselGenerator <- c.getOrElse[String]("addTopLevelChiselGenerator")("")
    } yield {
      val addTop = addTopLevelChiselGenerator match {
        case "" => None
        case s => Some(s) 
      }
      ChiselizerOptions(unpackedEmissionStyle, addTop)
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
    val comment, overrideOption = Value
    def default = overrideOption
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
