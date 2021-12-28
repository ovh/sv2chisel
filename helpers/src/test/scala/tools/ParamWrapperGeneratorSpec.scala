package sv2chiselHelpersTests.tools

import sv2chisel.helpers.tools._
import sv2chiselHelpersTests.utils._

import chisel3._
import chisel3.experimental.IntParam

import org.scalatest._
import flatspec._

class ParamWrapperGeneratorSpec extends AnyFlatSpec with VerilogMatchers {

  behavior of "ParamWrapperGenerator"

  val setTestRunDir = Array(
      "--target-dir",
      "test_run_dir"
  )

  it should s"emit flawlessly a simple module" in {
    class Test(val inW: Int, val outW: Int) extends RawModule {
      val in  = IO(Input(UInt(inW.W)))
      val out = IO(Output(Vec(2, UInt(outW.W))))
      out(0) := in.asTypeOf(out(0))
      out(1) := in.asTypeOf(out(1))
    }
    val instances = Map(
        ParamSet(Seq(("IN_WIDTH", IntParam(5)), ("OUT_WIDTH", IntParam(5))))  -> (() => new Test(5, 5)),
        ParamSet(Seq(("IN_WIDTH", IntParam(10)), ("OUT_WIDTH", IntParam(2)))) -> (() => new Test(10, 2))
    )
    val result = ParamWrapperGenerator.emit(instances, args = setTestRunDir)
    result should containExactly("module Test_0(")
    result should containExactly("module Test_1(")
    result should containExactly(
        "module Test #(",
        "    parameter IN_WIDTH,",
        "    parameter OUT_WIDTH",
        "  )",
        "  (",
        "    input [9:0] in,",
        "    output [4:0] out_0,",
        "    output [4:0] out_1",
        "  );"
    )
    result should containExactly(
        "  generate",
        "    if(IN_WIDTH == 5 && OUT_WIDTH == 5) begin",
        "      Test_0 Test(",
        "        .in(in[4:0]),",
        "        .out_0(out_0),",
        "        .out_1(out_1)",
        "      );"
    )
    result should containExactly(
        "    end else if(IN_WIDTH == 10 && OUT_WIDTH == 2) begin",
        "      Test_1 Test(",
        "        .in(in),",
        "        .out_0(out_0[1:0]),",
        "        .out_1(out_1[1:0])",
        "      );",
        "      assign out_0[4:2] = '0;",
        "      assign out_1[4:2] = '0;"
    )
  }

  it should s"emit flawlessly a simple module with unflatPorts" in {
    class Test(val inW: Int, val outW: Int) extends RawModule {
      val in  = IO(Input(UInt(inW.W)))
      val out = IO(Output(Vec(2, UInt(outW.W))))
      out(0) := in.asTypeOf(out(0))
      out(1) := in.asTypeOf(out(1))
    }
    val instances = Map(
        ParamSet(Seq(("IN_WIDTH", IntParam(5)), ("OUT_WIDTH", IntParam(5))))  -> (() => new Test(5, 5)),
        ParamSet(Seq(("IN_WIDTH", IntParam(10)), ("OUT_WIDTH", IntParam(2)))) -> (() => new Test(10, 2))
    )
    val result = ParamWrapperGenerator.emit(instances, unflatPorts = true, args = setTestRunDir)
    result should containExactly("module Test_0(")
    result should containExactly("module Test_1(")
    result should containExactly(
        "module Test #(",
        "    parameter IN_WIDTH,",
        "    parameter OUT_WIDTH",
        "  )",
        "  (",
        "    input [9:0] in,",
        "    output [1:0] [4:0] out",
        "  );"
    )
    result should containExactly(
        "  generate",
        "    if(IN_WIDTH == 5 && OUT_WIDTH == 5) begin",
        "      Test_0 Test(",
        "        .in(in[4:0]),",
        "        .out_0(out[0]),",
        "        .out_1(out[1])",
        "      );"
    )
    result should containExactly(
        "    end else if(IN_WIDTH == 10 && OUT_WIDTH == 2) begin",
        "      Test_1 Test(",
        "        .in(in),",
        "        .out_0(out[0][1:0]),",
        "        .out_1(out[1][1:0])",
        "      );",
        "      assign out[0][4:2] = '0;",
        "      assign out[1][4:2] = '0;"
    )
  }

  it should s"emit flawlessly a module with vec ports with unflatPorts" in {
    class Test(val inW: Int, val inL: Int, val outW: Int, val outL: Int) extends RawModule {
      val in  = IO(Input(Vec(inL, UInt(inW.W))))
      val out = IO(Output(Vec(outL, UInt(outW.W))))
      out := in.asTypeOf(out)
    }
    val params = Seq(
        Map("IN_W" -> 5, "IN_LEN"  -> 4, "OUT_W" -> 10, "OUT_LEN" -> 2),
        Map("IN_W" -> 20, "IN_LEN" -> 1, "OUT_W" -> 4, "OUT_LEN"  -> 5)
    )

    val instances = Map(params.map(m => {
      val p = ParamSet(m.toSeq.map { case (n, v) => n -> IntParam(v) })
      p -> (() => new Test(m("IN_W"), m("IN_LEN"), m("OUT_W"), m("OUT_LEN")))
    }): _*)
    val result = ParamWrapperGenerator.emit(instances, unflatPorts = true, args = setTestRunDir)
    result should containExactly("module Test_0(")
    result should containExactly("module Test_1(")
    result should containExactly(
        "module Test #(",
        "    parameter IN_W,",
        "    parameter IN_LEN,",
        "    parameter OUT_W,",
        "    parameter OUT_LEN",
        "  )",
        "  (",
        "    input [3:0] [19:0] in,",
        "    output [4:0] [9:0] out",
        "  );"
    )
    result should containExactly("  generate")
    result should containExactly(
        "    if(IN_W == 5 && IN_LEN == 4 && OUT_W == 10 && OUT_LEN == 2) begin",
        "      Test_0 Test(",
        "        .in_0(in[0][4:0]),",
        "        .in_1(in[1][4:0]),",
        "        .in_2(in[2][4:0]),",
        "        .in_3(in[3][4:0]),",
        "        .out_0(out[0]),",
        "        .out_1(out[1])",
        "      );",
        "      assign out[2] = '0;",
        "      assign out[3] = '0;",
        "      assign out[4] = '0;"
    )
    result should containExactly(
        "    end else if(IN_W == 20 && IN_LEN == 1 && OUT_W == 4 && OUT_LEN == 5) begin",
        "      Test_1 Test(",
        "        .in_0(in[0]),",
        "        .out_0(out[0][3:0]),",
        "        .out_1(out[1][3:0]),",
        "        .out_2(out[2][3:0]),",
        "        .out_3(out[3][3:0]),",
        "        .out_4(out[4][3:0])",
        "      );",
        "      assign out[0][9:4] = '0;",
        "      assign out[1][9:4] = '0;",
        "      assign out[2][9:4] = '0;",
        "      assign out[3][9:4] = '0;",
        "      assign out[4][9:4] = '0;"
    )
  }

  class SimpleBundle extends Bundle {
    val data = UInt(5.W)
    val truc = Bool()
  }
  class MyBundle(val size: Int) extends Bundle {
    val b  = Bool()
    val u  = UInt((size * 2).W)
    val v  = Vec(size + 1, Bool())
    val vb = Vec(size, new SimpleBundle)
  }
  class TestBundle(val size: Int) extends RawModule {
    val in  = IO(Input(new MyBundle(size)))
    val out = IO(Output(Vec(2, new MyBundle(size))))
    out(0) := in
    out(1) := in
  }

  it should s"emit flawlessly a module with nested bundle ports with unflatPorts" in {
    val params = Seq(
        Map("SIZE" -> 2),
        Map("SIZE" -> 3)
    )

    val instances = Map(params.map(m => {
      val p = ParamSet(m.toSeq.map { case (n, v) => n -> IntParam(v) })
      p -> (() => new TestBundle(m("SIZE")))
    }): _*)
    val result = ParamWrapperGenerator.emit(instances, unflatPorts = true, args = setTestRunDir)
    result should containExactly("module TestBundle_0(")
    result should containExactly("module TestBundle_1(")
    result should containExactly(
        "module TestBundle #(",
        "    parameter SIZE",
        "  )",
        "  (",
        "    input MyBundle in,",
        "    output [1:0] MyBundle out",
        "  );"
    )
    result should containExactly("  generate")
    result should containExactly(
        "    if(SIZE == 2) begin",
        "      TestBundle_0 TestBundle(",
        "        .in_vb_0_truc(in.vb[0].truc),",
        "        .in_vb_0_data(in.vb[0].data),",
        "        .in_vb_1_truc(in.vb[1].truc),",
        "        .in_vb_1_data(in.vb[1].data),",
        "        .in_v_0(in.v[0]),",
        "        .in_v_1(in.v[1]),",
        "        .in_v_2(in.v[2]),",
        "        .in_u(in.u[3:0]),",
        "        .in_b(in.b),",
        "        .out_0_vb_0_truc(out[0].vb[0].truc),",
        "        .out_0_vb_0_data(out[0].vb[0].data),",
        "        .out_0_vb_1_truc(out[0].vb[1].truc),",
        "        .out_0_vb_1_data(out[0].vb[1].data),",
        "        .out_0_v_0(out[0].v[0]),",
        "        .out_0_v_1(out[0].v[1]),",
        "        .out_0_v_2(out[0].v[2]),",
        "        .out_0_u(out[0].u[3:0]),",
        "        .out_0_b(out[0].b),",
        "        .out_1_vb_0_truc(out[1].vb[0].truc),",
        "        .out_1_vb_0_data(out[1].vb[0].data),",
        "        .out_1_vb_1_truc(out[1].vb[1].truc),",
        "        .out_1_vb_1_data(out[1].vb[1].data),",
        "        .out_1_v_0(out[1].v[0]),",
        "        .out_1_v_1(out[1].v[1]),",
        "        .out_1_v_2(out[1].v[2]),",
        "        .out_1_u(out[1].u[3:0]),",
        "        .out_1_b(out[1].b)",
        "      );",
        "      assign out[0].vb[2] = '0;",
        "      assign out[0].v[3] = '0;",
        "      assign out[0].u[5:4] = '0;",
        "      assign out[1].vb[2] = '0;",
        "      assign out[1].v[3] = '0;",
        "      assign out[1].u[5:4] = '0;"
    )
    result should containExactly(
        "    end else if(SIZE == 3) begin",
        "      TestBundle_1 TestBundle(",
        "        .in_vb_0_truc(in.vb[0].truc),",
        "        .in_vb_0_data(in.vb[0].data),",
        "        .in_vb_1_truc(in.vb[1].truc),",
        "        .in_vb_1_data(in.vb[1].data),",
        "        .in_vb_2_truc(in.vb[2].truc),",
        "        .in_vb_2_data(in.vb[2].data),",
        "        .in_v_0(in.v[0]),",
        "        .in_v_1(in.v[1]),",
        "        .in_v_2(in.v[2]),",
        "        .in_v_3(in.v[3]),",
        "        .in_u(in.u),",
        "        .in_b(in.b),",
        "        .out_0_vb_0_truc(out[0].vb[0].truc),",
        "        .out_0_vb_0_data(out[0].vb[0].data),",
        "        .out_0_vb_1_truc(out[0].vb[1].truc),",
        "        .out_0_vb_1_data(out[0].vb[1].data),",
        "        .out_0_vb_2_truc(out[0].vb[2].truc),",
        "        .out_0_vb_2_data(out[0].vb[2].data),",
        "        .out_0_v_0(out[0].v[0]),",
        "        .out_0_v_1(out[0].v[1]),",
        "        .out_0_v_2(out[0].v[2]),",
        "        .out_0_v_3(out[0].v[3]),",
        "        .out_0_u(out[0].u),",
        "        .out_0_b(out[0].b),",
        "        .out_1_vb_0_truc(out[1].vb[0].truc),",
        "        .out_1_vb_0_data(out[1].vb[0].data),",
        "        .out_1_vb_1_truc(out[1].vb[1].truc),",
        "        .out_1_vb_1_data(out[1].vb[1].data),",
        "        .out_1_vb_2_truc(out[1].vb[2].truc),",
        "        .out_1_vb_2_data(out[1].vb[2].data),",
        "        .out_1_v_0(out[1].v[0]),",
        "        .out_1_v_1(out[1].v[1]),",
        "        .out_1_v_2(out[1].v[2]),",
        "        .out_1_v_3(out[1].v[3]),",
        "        .out_1_u(out[1].u),",
        "        .out_1_b(out[1].b)",
        "      );"
    )
  }

  it should s"emit flawlessly a wrapper with nested bundle ports" in {

    val (result, _) = VerilogPortWrapper.generate(() => new TestBundle(3), Some("test_wrapper"), args = setTestRunDir)

    result should containExactly(
        "module test_wrapper (",
        "    input MyBundle in,",
        "    output [1:0] MyBundle out",
        "  );"
    )

    result should containExactly(
        "  TestBundle inst (",
        "    .in_vb_0_truc(in.vb[0].truc),",
        "    .in_vb_0_data(in.vb[0].data),",
        "    .in_vb_1_truc(in.vb[1].truc),",
        "    .in_vb_1_data(in.vb[1].data),",
        "    .in_vb_2_truc(in.vb[2].truc),",
        "    .in_vb_2_data(in.vb[2].data),",
        "    .in_v_0(in.v[0]),",
        "    .in_v_1(in.v[1]),",
        "    .in_v_2(in.v[2]),",
        "    .in_v_3(in.v[3]),",
        "    .in_u(in.u),",
        "    .in_b(in.b),",
        "    .out_0_vb_0_truc(out[0].vb[0].truc),",
        "    .out_0_vb_0_data(out[0].vb[0].data),",
        "    .out_0_vb_1_truc(out[0].vb[1].truc),",
        "    .out_0_vb_1_data(out[0].vb[1].data),",
        "    .out_0_vb_2_truc(out[0].vb[2].truc),",
        "    .out_0_vb_2_data(out[0].vb[2].data),",
        "    .out_0_v_0(out[0].v[0]),",
        "    .out_0_v_1(out[0].v[1]),",
        "    .out_0_v_2(out[0].v[2]),",
        "    .out_0_v_3(out[0].v[3]),",
        "    .out_0_u(out[0].u),",
        "    .out_0_b(out[0].b),",
        "    .out_1_vb_0_truc(out[1].vb[0].truc),",
        "    .out_1_vb_0_data(out[1].vb[0].data),",
        "    .out_1_vb_1_truc(out[1].vb[1].truc),",
        "    .out_1_vb_1_data(out[1].vb[1].data),",
        "    .out_1_vb_2_truc(out[1].vb[2].truc),",
        "    .out_1_vb_2_data(out[1].vb[2].data),",
        "    .out_1_v_0(out[1].v[0]),",
        "    .out_1_v_1(out[1].v[1]),",
        "    .out_1_v_2(out[1].v[2]),",
        "    .out_1_v_3(out[1].v[3]),",
        "    .out_1_u(out[1].u),",
        "    .out_1_b(out[1].b)",
        "  );",
        "endmodule"
    )
  }

  it should s"emit flawlessly a wrapper with proper vec ports" in {
    class Test extends RawModule {
      val in  = IO(Input(Vec(4, UInt(5.W))))
      val out = IO(Output(Vec(2, UInt(10.W))))
      out := in.asTypeOf(out)
    }

    val (result, _) = VerilogPortWrapper.generate(() => new Test(), Some("test_wrapper"), args = setTestRunDir)
    result should containExactly(
        "module test_wrapper (",
        "    input [3:0] [4:0] in,",
        "    output [1:0] [9:0] out",
        "  );"
    )

    result should containExactly(
        "  Test inst (",
        "    .in_0(in[0]),",
        "    .in_1(in[1]),",
        "    .in_2(in[2]),",
        "    .in_3(in[3]),",
        "    .out_0(out[0]),",
        "    .out_1(out[1])",
        "  );"
    )
  }

  class InnerTest(val w: Int) extends RawModule {
    val in  = IO(Input(UInt(w.W)))
    val out = IO(Output(UInt(w.W)))
    out := in
  }
  class Test(val inW: Int, val outW: Int) extends RawModule {
    val in    = IO(Input(UInt(inW.W)))
    val out   = IO(Output(UInt(outW.W)))
    val inner = Module(new InnerTest(inW))
    inner.in := in
    out := inner.out.asTypeOf(out)
  }

  it should s"emit conflict-free names for nested modules" in {
    val instances = Map(
        ParamSet(Seq(("IN_WIDTH", IntParam(5)), ("OUT_WIDTH", IntParam(5))))  -> (() => new Test(5, 5)),
        ParamSet(Seq(("IN_WIDTH", IntParam(10)), ("OUT_WIDTH", IntParam(2)))) -> (() => new Test(10, 2))
    )

    val result = ParamWrapperGenerator.emit(instances, args = setTestRunDir)
    result should containExactly("module Test_0(")
    result should containExactly("module Test_1(")
    result should containExactly("module Test_0_InnerTest(")
    result should containExactly("module Test_1_InnerTest(")
  }

  it should s"emit conflict-free names nested modules with forcedName" in {
    val instances = Map(
        ParamSet(Seq(("IN_WIDTH", IntParam(5)), ("OUT_WIDTH", IntParam(5))))  -> (() => new Test(5, 5)),
        ParamSet(Seq(("IN_WIDTH", IntParam(10)), ("OUT_WIDTH", IntParam(2)))) -> (() => new Test(10, 2))
    )

    val result = ParamWrapperGenerator.emit(instances, Some("special_name"), args = setTestRunDir)
    result should containExactly("module special_name_Test_0(")
    result should containExactly("module special_name_Test_1(")
    result should containExactly("module special_name_0_InnerTest(")
    result should containExactly("module special_name_1_InnerTest(")
    result should containExactly("module special_name #(")
  }

  it should s"emit flawlessly a nested black box" in {
    class TestBlackBox(_width: Int) extends BlackBox(Map("WIDTH" -> _width)) {
      val io = IO(new Bundle {
        val in  = Input(UInt(_width.W))
        val out = Output(UInt(_width.W))
      })
      override def desiredName = "verilog_ext_module"
    }

    class Test(val inW: Int, val outW: Int) extends RawModule {
      val in    = IO(Input(UInt(inW.W)))
      val out   = IO(Output(UInt(outW.W)))
      val inner = Module(new TestBlackBox(inW))
      inner.io.in := in
      out := inner.io.out.asTypeOf(out)
    }
    val instances = Map(
        ParamSet(Seq(("IN_WIDTH", IntParam(5)), ("OUT_WIDTH", IntParam(5))))  -> (() => new Test(5, 5)),
        ParamSet(Seq(("IN_WIDTH", IntParam(10)), ("OUT_WIDTH", IntParam(2)))) -> (() => new Test(10, 2))
    )

    val result = ParamWrapperGenerator.emit(instances, args = setTestRunDir)
    result should containExactly("module Test_0(")
    result should containExactly("module Test_1(")
    result should contain("verilog_ext_module #(.WIDTH(5)) inner (")
    result should contain("verilog_ext_module #(.WIDTH(10)) inner (")
  }

  it should s"handle forced names for top module" in {
    class Test(val inW: Int, val outW: Int) extends RawModule {
      val in  = IO(Input(UInt(inW.W)))
      val out = IO(Output(UInt(outW.W)))
      out := in.asTypeOf(out)
    }
    val instances = Map(
        ParamSet(Seq(("IN_WIDTH", IntParam(5)), ("OUT_WIDTH", IntParam(5))))  -> (() => new Test(5, 5)),
        ParamSet(Seq(("IN_WIDTH", IntParam(10)), ("OUT_WIDTH", IntParam(2)))) -> (() => new Test(10, 2))
    )
    val result = ParamWrapperGenerator.emit(instances, Some("WeIrdName"), args = setTestRunDir)
    result should containExactly("module WeIrdName_Test_0(")
    result should containExactly("module WeIrdName_Test_1(")
    result should containExactly("module WeIrdName #(", "    parameter IN_WIDTH,", "    parameter OUT_WIDTH")
    result should containExactly("  generate")
  }

  it should s"handle forced preset" in {
    class Test(val inW: Int, val outW: Int) extends Module {
      val in  = IO(Input(UInt(inW.W)))
      val out = IO(Output(UInt(outW.W)))
      val reg = RegInit(0.U.asTypeOf(out))
      reg := in.asTypeOf(out)
      out := reg
    }
    val instances = Map(
        ParamSet(Seq(("IN_WIDTH", IntParam(5)), ("OUT_WIDTH", IntParam(5))))  -> (() => new Test(5, 5)),
        ParamSet(Seq(("IN_WIDTH", IntParam(10)), ("OUT_WIDTH", IntParam(2)))) -> (() => new Test(10, 2))
    )
    val result = ParamWrapperGenerator.emit(instances = instances, forcePreset = true, args = setTestRunDir)
    result should containExactly("module Test_0(")
    result should containExactly("module Test_1(")
    result should containExactly("module Test #(", "    parameter IN_WIDTH,", "    parameter OUT_WIDTH")
    result should containExactly("  generate")
    result shouldNot containExactly("  input        reset,")
    result shouldNot containExactly("module PresetWrapper(")
  }

  it should "raise exception on mismatched ports list" in {
    an[WrapperException] shouldBe thrownBy {
      class Test(val inW: Int, val outW: Int) extends RawModule {
        // this will NOT work due to different number of ports ...
        val in     = IO(Input(Vec(inW, Bool())))
        val out    = IO(Output(Vec(outW, Bool())))
        val outBis = if (outW == 2) Some(IO(Output(Vec(outW, Bool())))) else None
        out := in.asTypeOf(out)
        outBis match {
          case Some(d) => d := in.asTypeOf(d)
          case _       =>
        }

      }
      val instances = Map(
          ParamSet(Seq(("IN_WIDTH", IntParam(5)), ("OUT_WIDTH", IntParam(5))))  -> (() => new Test(5, 5)),
          ParamSet(Seq(("IN_WIDTH", IntParam(10)), ("OUT_WIDTH", IntParam(2)))) -> (() => new Test(10, 2))
      )
      val result = ParamWrapperGenerator.emit(instances, args = setTestRunDir)
      println(result)
    }
  }
}
