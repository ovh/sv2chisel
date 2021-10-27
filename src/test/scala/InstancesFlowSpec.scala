// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class InstanceFlowSpec extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  "Instance Concats" should "be properly emitted" in {
    val result = emitInModule(s"""
      |// might contain more than the RAM due to moves
      |localparam DBLW = 2;
      |localparam WWW = 16;
      |wire [DBLW-1:0]   inst_in_u1;
      |wire [WWW-1:0]    inst_in_u2;
      |wire [WWW-1:0]    inst_in_u3;
      |wire              inst_in_b;
      |
      |assign inst_in_u1 = '0;
      |assign inst_in_u2 = '1;
      |assign inst_in_u3 = inst_in_u2;
      |assign inst_in_b = '1;
      |
      |wire [DBLW-1:0]   inst_out_u1;
      |wire [WWW-1:0]    inst_out_u2;
      |wire [WWW-1:0]    inst_out_u3;
      |wire              inst_out_b;
      |mod inst(
      |  .in(inst_in_u3),
      |  .din({inst_in_u1, inst_in_b, inst_in_u2}),
      |  .dout({inst_out_u1, inst_out_b, inst_out_u2}),
      |  .out(inst_out_u3)
      |);
      |wire [DBLW-1:0]   post_out_u1;
      |wire [WWW-1:0]    post_out_u2;
      |wire [WWW-1:0]    post_out_u3;
      |wire              post_out_b;
      |
      |assign post_out_u1 = inst_out_u1;
      |assign post_out_u2 = inst_out_u2;
      |assign post_out_u3 = inst_out_u3;
      |assign post_out_b = inst_out_b;
      """.stripMargin
    )
    debug(result)
    result should contains ("class Test() extends MultiIOModule {")
    
    result should contains ("inst.in := inst_in_u3")
    result should contains ("inst.din := inst_din.asTypeOf(inst.din)") // issue here
    result should contains ("inst_dout := inst.dout.asTypeOf(inst_dout)")
    result should contains ("inst_out_u3 := inst.out")

    // more to add
    
  }

}