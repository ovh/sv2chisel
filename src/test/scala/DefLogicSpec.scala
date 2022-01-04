// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chiselTests

import sv2chiselTests.utils._
import logger._

class DefLogicSpecs extends Sv2ChiselSpec {
  Logger.setLevel(LogLevel.Warn)
  
  behavior of "InferDefLogic"
  
  it should "emit RegInit properly" in {
    val result = emitInModule(s"""
      |localparam WWW = 1;
      |localparam WW = 1;
      |
      |wire clk;
      |wire          change;
      |reg [WW-1:0]  counter = '0;
      |reg [WWW-1:0] current = '1;
      |
      |always @(posedge clk) begin
      |    if (change) begin
      |        if (current < (1<<WWW)-1) begin
      |            current <= current + 1;
      |        end else begin
      |            current <= 0;
      |        end
      |        counter <= 0;            
      |    end else begin
      |        counter <= counter + 1;
      |    end
      |end
      """.stripMargin
    )
    result should containStr ("class Test() extends Module {")

    result should containStr ("val WWW = 1")
    result should containStr ("val change = Wire(Bool())")
    result should containStr ("val counter = RegInit(UInt(WW.W), 0.U)")
    result should containStr ("val current = RegInit(UInt(WWW.W), ((1 << WWW)-1).U)")
    
    // TO DO 
    result should containStr ("when(current < ((1 << WWW)-1).U) {")
    result should containStr ("current := current+1.U")
    
  }
  
  it should "legalize output reg" in {
    val result = emit(s"""
      |module my_module #(
      |    parameter WIDTH = 3
      |)(
      |    input clk,
      |    input rst,
      |    output reg [WIDTH-1:0] counter = '0
      |);
      |    
      |    always @(posedge clk) begin
      |        if (rst) begin
      |            counter <= 0;            
      |        end else begin
      |            counter <= counter + 1;
      |        end
      |    end
      |endmodule

      """.stripMargin
    )
    result should containStr ("class my_module(" )
    result should containStr ("val WIDTH: Int = 3" )
    result should containStr (") extends Module {")
    result should containStr ("val rst = IO(Input(Bool()))") // clock is abstracted but rst is not
    result should containStr ("val counter = IO(Output(UInt(WIDTH.W)))")
    
    result should containStr ("// NOTE: The following statements are auto generated based on existing output reg of the original verilog source")
    result should containStr ("val counter_out_reg = RegInit(UInt(WIDTH.W), 0.U)")
    result should containStr ("counter := counter_out_reg")

    result should containStr ("when(rst) {")
    result should containStr (  "counter_out_reg := 0.U")
    result should containStr ("} .otherwise {")
    result should containStr (  "counter_out_reg := counter_out_reg+1.U")
    result should containStr ("}")

    
  }

}