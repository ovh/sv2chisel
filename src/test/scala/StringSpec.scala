// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class StringSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "String" 
  
  it should "be casted properly" in {
    val result = emit(wrapInPackage("""
      |localparam W = 64;
      |function logic check_str;
      |    // str is 8 bytes / 64 bits
      |    input logic[W-1:0] str; 
      |    // first check against 8 chars second check against only 4 chars (32 bits)
      |    check_str = str[W-1:0] == "OPTIONS " || str[W-1:32] == "GET ";
      |endfunction
      """.stripMargin
    ))
    debug(result)
    
    result should contain ( "import sv2chisel.helpers.vecconvert._" ) // to get .V
    
    // probably the most decent option (asUInt)
    result should contain ( "(str(W-1,0).asUInt === \"OPTIONS \".V.asUInt) || (str(W-1,32).asUInt === \"GET \".V.asUInt)" ) 
  }

}