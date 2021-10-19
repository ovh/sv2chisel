// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

import my_package::*;

module my_module (
  input clock,
  input reset,
  input  [my_width-1:0] a,
  input  [my_width-1:0] b,
  output [my_width:0] c
  );
  
  logic [my_package::my_width:0] r;
  
  always_ff @ (posedge clock) begin
    if (reset) begin
      r <= '0;
    end else begin
      r <= a + b;
    end
  end
  
  assign c = r;
  
endmodule // my_module