// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import io.circe._

import logger._

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
object LegalizeParamDefaultOptions extends EasyLogging {
  object LegalizeMethod extends Enumeration {
    val comment, overrideOption = Value
    def default = overrideOption
    
    def decode: Decoder[LegalizeMethod.Value] = Decoder.instance(c => {
      c.as[String] match {
        case Right(s) => 
          try { 
            Right(LegalizeMethod.withName(s))
          } catch {
            case _: java.util.NoSuchElementException => 
              Left(DecodingFailure(s"Unknown legalizeMethod ($s) for LegalizeParamDefaults", c.history))
          }
        case Left(DecodingFailure(s, l)) => Left(DecodingFailure(s"Cannot parse properly legalizeMethod for LegalizeParamDefaultOptions (not a string). $s", l))
      }
    })
  }
}

case class TranslationOptions(
  legalizeParamDefault: LegalizeParamDefaultOptions = LegalizeParamDefaultOptions(),
  removeConcat: RemoveConcatOptions = RemoveConcatOptions()
) {
  /** Decoder for circe parsing from yaml */
  def decode: Decoder[TranslationOptions] = Decoder.instance(c => {
    val default = TranslationOptions()
    implicit val d1 = LegalizeParamDefaultOptions().decode
    implicit val d2 = RemoveConcatOptions().decode
    for {
      legalizeParamDefault <- c.getOrElse[LegalizeParamDefaultOptions]("LegalizeParamDefaults")(default.legalizeParamDefault)
      removeConcat <- c.getOrElse[RemoveConcatOptions]("RemoveConcats")(default.removeConcat)
    } yield {
      TranslationOptions(legalizeParamDefault, removeConcat)
    }
  })
}
