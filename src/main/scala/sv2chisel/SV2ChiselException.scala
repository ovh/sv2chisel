// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import scala.util.control.NoStackTrace

/** Exception indicating user error
  *
  * These exceptions indicate a problem due to bad input and thus do not include a stack trace.
  * This can be extended by custom transform writers.
  */
class SV2ChiselUserException(message: String, cause: Throwable = null)
  extends RuntimeException(message, cause) with NoStackTrace

/** Wraps exceptions from CustomTransforms so they can be reported appropriately */
case class CustomTransformException(cause: Throwable) extends Exception("", cause)

/** Exception indicating something went wrong *within* Firrtl itself
  *
  * These exceptions indicate a problem inside the compiler and include a stack trace to help
  * developers debug the issue.
  *
  * This class is private because these are issues within Firrtl itself. Exceptions thrown in custom
  * transforms are treated differently and should thus have their own structure
  */
private[sv2chisel] class SV2ChiselInternalException(message: String, cause: Throwable = null)
  extends Exception(message, cause)
