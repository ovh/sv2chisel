# sv2chisel: Features & Limitations

#### Contents
1. [Features](#1-features)
    1. [sv2chisel Translator](#11-sv2chisel-translator)
        - [(System)Verilog Subset](#supported-systemVerilog-subset)
        - [Comments Preservation](#comments-preservation)
        - [Blackboxes Integration](#blackboxes-integration)
        - [Semantic & Transformations](#semantic--transformations)
        - [Configuration & Translation Options](#configuration--translation-options)
    2. [sv2chisel-helpers Chisel Library](#12-sv2chisel-helpers-chisel-library)
        - [Miscellaneous Helpers](#miscellaneous-helpers)
        - [Generation Utilities for Chisel Integration: *Chisel As IP*](#generation-utilities-for-chisel-integration-chisel-as-ip)
2. [Limitations](#2-limitations)
    1. [Translation](#21-translation-limitations)
        - [Iterative Approach](#iterative-approach)
        - [Non-synthesizable (System)Verilog constructs](#non-synthesizable-systemVerilog-constructs)
        - [No proper reset inference](#no-proper-reset-inference-todo-infer-resets)
        - [Preprocessor directives are ignored](#preprocessor-directives-are-ignored-todo-integrate-define-and-ifdef-wherever-possible)
        - [Initial begin and assert are ignored](#initial-begin-and-assert-are-ignored-todo-translate-assertions-detect-initializations)
    2. [Scala/Chisel](#22-scalachisel-limitations-impairing-hardware-development-experience)
        - [Large integers (>64 bits)](#large-integers-64-bits)
        - [From untyped bit-vector to UInt vs Vec(Bool) usage](#from-untyped-bit-vector-to-uint-vs-vecbool-usage)
        - [Negedge concept is not supported in Chisel](#negedge-concept-is-not-supported-in-chisel)
        - [Blocking assignment have no straightforward equivalent in Chisel](#blocking-assignment-have-no-straightforward-equivalent-in-chisel)

<!-- 
Table template:

<table td style="width:100%">
<tr><td><center> 

COLUMNA </center></td>
<td><center> 

COLUMNB </center></td></tr>

<tr><td style="width:50%">

```Verilog
localparam
```

</td><td style="width:50%">

```scala
val
```

</td></tr></table>
-->

---

## 1. Features
sv2chisel project is divided in two parts: 
- **sv2chisel** a standalone tool, able to translate (System)Verilog sources into Chisel
- **sv2chisel-helpers** a Chisel/scala library, sometimes required by the translated chisel and more generally providing various utilities to integrate Chisel into HDL projects
   
### 1.1. sv2chisel translator

#### Supported (System)Verilog subset
sv2chisel aims at supporting the complete synthesizable subset of (System)Verilog.
However, the development has been based on concrete code examples and is hence limited to the syntax encountered through this *corpus* used as reference for the iterative development process of sv2chisel.

After multiple iterations and several thousands lines of Verilog successfully translated, here is a *non-exhaustive* list of Verilog constructs currently supported:
- **Base blocks**
  - `module` *including parameters and various io declaration styles*
  - `package`
  - `function` *assuming hardware types as both args (inputs) and return (output)*


- **Statements**
  - `localparam` *both software and hardware*
  - signal declarations: `input|output` `wire` `reg` `logic`, *including packed, unpacked or both*
  - processes: `always_ff` `always @(*)` `always @(posedge <clock>)`
  - module instances `mod #(<...>) inst (<...>)`
  - `assign`
  - non blocking assignments `<=`
  - conditions `if` `else` `case`
  - `generate` with `for` and `if`
  - `typedef` of `enum` and `struct`


- **Expressions:** 
  - theoretically almost everything one can think of and write in Verilog
  - including some barbarian syntaxes such as `'{default: '0}` and replicate patterns `{REP{pattern}}` 
  - *otherwise please fill an issue to report unsupported syntaxes (and their actual meaning!)*


- **Miscellaneous**
  - some native Verilog functions such as `$clog2`
  - literals `2'b01`, `32'd0123456789`, `64'h0123456789abcdef`

#### Comments Preservation
A not so obvious nor anecdotic feature of sv2chisel is to retain comments from the original files, providing a smooth transition from existing source with all their complexity to their Chisel counterpart.

The following table illustrates comment preservation with Verilog and chisel version side by side: 
  
<table td style="width:100%"><tr><td style="width:50%">

```SystemVerilog
/* first intro comment
 *
 * intro comment
 */
package test_p; // this is package test_p 
    /* comment1 prior A
     * comment2 prior A
     * comment3 prior A
     */
    localparam A = 2; // comment after A


    // comment just before B
    localparam B = 2;

    // comment one line before C

    localparam C = 2;
    
    
    
    localparam WWW = 16; /* comment after WWW
     end of package is coming
 */
endpackage  // test_p
/* end of file comment
 * on multiple lines
 */
```

</td><td style="width:50%">

```scala
/* first intro comment
 *
 * intro comment
 */
package object test_p { // this is package test_p
  /* comment1 prior A
     * comment2 prior A
     * comment3 prior A
     */
  val A = 2 // comment after A


  // comment just before B
  val B = 2

  // comment one line before C

  val C = 2



  val WWW = 16 /* comment after WWW
     end of package is coming
 */
} // test_p
/* end of file comment
 * on multiple lines
 */
```

</td></tr></table>

> One might notice some slight misalignment, feel free to open a PR to improve the comment rendering. 
Another nice to have feature would be the translation of comments in front of declarations (`class`, `package`, `def`, `val`) into javadoc-style comments.

#### Blackboxes Integration
Chisel natively provides an interface for blackboxes and sv2chisel leverage [this Chisel API](https://www.chisel-lang.org/chisel3/docs/explanations/blackboxes.html) to provide easy integration of blackboxes within the project.

Why one would need to blackbox some (System)Verilog sources instead of translating them?
- third party IPs *(vendor-dependent blocks, proprietary IPs)*
- very low level or unsupported Verilog constructs *(negedge, pre-processor directives)*

Table below illustrates an example of blackboxing capabilities of sv2chisel.
Note that it automatically tackles some limitations of native BlackBoxes API, in particular regarding IO port types by providing a chisel-friendly `Module` wrapper.

<table td style="width:100%"><tr><td style="width:50%">

```Verilog
module blackbox_example
    #(
        // Number of bits of one word
        parameter WIDTH,
        // Number of taps (= read points)
        parameter TAPS = 1,
        // Init value
        parameter logic [WIDTH-1:0] INIT_VALUE = '0,
        // Use vendor primitive for fpga
        parameter USE_VENDOR_PRIMITIVE = 0
    )
    (
        input clk,
        // Port A
        input  [WIDTH-1:0]           in_data,
        output [TAPS-1:0][WIDTH-1:0] out_data
    );
    
    /* module content with a lots of 
     * ifdefs & vendor primitives 
     */
    
endmodule
```

</td><td style="width:50%">

```scala
class blackbox_example(
    // Number of bits of one word
    val WIDTH: Int, 
    // Number of taps (= read points)
    val TAPS: Int = 1, 
    // Init value
    val INIT_VALUE: UInt = 0.U, 
    // Use vendor primitive for fpga
    val USE_VENDOR_PRIMITIVE: Boolean = false
  ) extends RawModule {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    // Port A
    val in_data = Input(UInt(WIDTH.W))
    val out_data = Output(Vec(TAPS, UInt(WIDTH.W)))
  })
  val inst = Module(new tapped_shift_registerBB(
    WIDTH, 
    TAPS, 
    INIT_VALUE, 
    USE_VENDOR_PRIMITIVE
  ))
  inst.io.clk := io.clk.asTypeOf(inst.io.clk)
  inst.io.in_data := io.in_data
  io.out_data := inst.io.out_data.asTypeOf(io.out_data)
}
class blackbox_exampleBB(
    val WIDTH: Int, 
    val TAPS: Int = 1, 
    val INIT_VALUE: UInt = 0.U, 
    val USE_VENDOR_PRIMITIVE: Boolean = false
  ) extends BlackBox(Map(
        "WIDTH" -> WIDTH, 
        "TAPS" -> TAPS, 
        "INIT_VALUE" -> INIT_VALUE.litValue, 
        "USE_VENDOR_PRIMITIVE" -> USE_VENDOR_PRIMITIVE
  )) with HasBlackBoxResource {
  val io = IO(new Bundle {
    val clk = Input(UInt(1.W))
    val in_data = Input(UInt(WIDTH.W))
    val out_data = Output(UInt((TAPS*WIDTH).W))
  })
  addResource("/hdl/blackbox_example.sv")
}
```

</td></tr></table>


> See the [example config file](https://github.com/ovh/sv2chisel/blob/master/src/main/resources/project/config.yml) for the syntax used to specify which sources shall be treated as blackboxes 
>
> See [Chiselizer.baseBlackboxRessourcePath option](#baseblackboxressourcepath) for additional integration options

#### Semantic & Transformations
The translation of (System)Verilog to Chisel can unfortunately not be reduced to a simple syntactic rewriting. 
Here are the main steps of the translation:
1. **Lexing & Parsing** *(based on the sv2017 ANTLR grammar provided by [Nic30's hdlConvertor](https://github.com/Nic30/hdlConvertor/tree/master/grammars))*
2. **Mapping to custom IR** (Intermediate Representation, *inspired by [FIRRTL](https://github.com/chipsalliance/firrtl/blob/master/src/main/scala/firrtl/ir/IR.scala))*
3. **Iterative transformations** of the IR towards a Chisel-friendly version 
4. **Translation** of the IR into Scala/Chisel
5. **Emission** with re-integration of original comments

The among the iterative transformations are the main ones:
- **Core semantic transforms**
  - logic inference as either `Reg`, `Wire` or an elaboration-dependent mix of both
  - clock propagation
  - project-wide `UInt` vs `Vec[Bool]` inference
  - parameters type inference
  - expressions type inference & casts
- **Syntactic transforms** 
  - function implicit returns fix
  - patterns translation
  - concatenated assignation fix
  - Re-used parameters fix

> For further insights about sv2chisel, have a look to the paper [(System)Verilog to Chisel Translation for Faster Hardware Design](https://hal.archives-ouvertes.fr/hal-02949112/document).

#### Configuration & Translation Options
This section details all currently available translation options, note that these options may only be provided in a yaml config file (see the [example config file](https://github.com/ovh/sv2chisel/blob/master/src/main/resources/project/config.yml) for the actual syntax).

<!-- ##### translationOptions.Chiselizer `Dict`  -->
##### translationOptions.Chiselizer.topLevelChiselGenerators `List[Dict]`
>Probably the most important option to benefit to get started with code integration thanks to *Chisel as IP* generation utilities.

Each entry of the list is a `Dict` which shall contain a field `name` corresponding to the name of one "top" module (ie an entry-point module that you want to generate Verilog from Chisel, you might want to list one, several or all of the modules of the original Verilog project)

<table td style="width:100%">
<tr><td><center> 

(default) **withWrapper: true** </center></td>
<td><center> 

**withWrapper: false** </center></td></tr>
<tr><td style="width:50%;vertical-align: top;">

Use ParamWrapperGenerator or VerilogPortWrapper, see [Generation Utilities for Chisel Integration: *Chisel As IP*](#generation-utilities-for-chisel-integration-chisel-as-ip)


Example of Module without parameters
```scala
object my_moduleGen extends App {
  VerilogPortWrapper.emit(
    () => new my_module(),
    // automatic clock detection & renaming if required
    renameWrapperPorts = Map("clock" -> "clk"), 
    // preset are the only kind of reset mapped 
    // to chisel implicit reset yet
    forcePreset = true, 
    // args forwarding to chisel stack 
    args = args 
  )
}
```

</td><td>
Generate vanilla Chisel Main Generator App

Example of Module without parameters
```scala
object my_moduleGen extends App {
  new ChiselStage().emitVerilog(new my_module())
}
```

</td></tr>
<tr><td style="width:50%;vertical-align: top;">

Example of Module with parameters
```scala
object my_moduleGen extends App {
  val gen = () => new my_module(
    PARAM_A = 0,
    PARAM_B = ??? // must be provided by user
  )
  val params = ParamSet(Seq(
    "PARAM_A" -> IntParam(0),
    "PARAM_B" -> ??? // must be provided by user
  ))
  ParamWrapperGenerator.emit(
    Map(params -> gen),
    renameWrapperPorts = Map("clock" -> "clk"),
    forcePreset = true,
    // beahave as VerilogPortWrapper
    unflatPorts = true,
    args = args
  )
}
```

</td><td style="width:50%;vertical-align: top;">


Example of Module with parameters
```scala
object my_moduleGen extends App {
  new ChiselStage().emitVerilog(new my_module(
    PARAM_A = 0,
    PARAM_B = ???
  ))
}
```

</td></tr></table>


##### translationOptions.Chiselizer.unpackedEmissionStyle `Enum`

```Verilog
reg my_rom_init [0:5] = '{default: '0};
reg my_rom [0:5];
always @(posedege clk) begin
  my_rom[id] <= my_rom_init[id];
end
```

<table td style="width:100%">
<tr><td><center> 

(default) `unpackedEmissionStyle: Mem` </center></td>
<td><center> 

`unpackedEmissionStyle: Reg` </center></td></tr>

<tr><td style="width:50%">


```scala
import sv2chisel.helpers.MemInit
val my_rom_init = MemInit.fill(6, Bool())(BigInt(0))
val my_rom = Mem(6, Bool())
my_rom(id) := my_rom_init(id)
```

</td><td style="width:50%">

```scala
val my_rom_init = RegInit(Vec(6, Bool()), 0.U.asTypeOf(Vec(6, Bool())))
val my_rom = Reg(Vec(6, Bool()))
my_rom(id) := my_rom_init(id)
```

</td></tr></table>


##### translationOptions.Chiselizer.baseBlackboxRessourcePath `String`
Output path for blackboxes files, copied there in their original (System)Verilog version.
When this path is provided, `HasBlackBoxResource` trait is mixed-in the BlackBox definition. 

It gives Chisel the ability to copy the blackboxes resources (Verilog files actually implententing the blackbox) along the main chisel-generated Verilog, as explained in further details in [chisel documentation](https://www.chisel-lang.org/chisel3/docs/explanations/blackboxes.html).
This is not compulsory, only a convenience, as these Verilog dependancies can be provided later down the usual EDA flow (tcl/fdo file lists provided to tools) directly from their original folder.
Indeed the Chisel stack does **not** manipulate the blackbox itself.

> NB: the path must contains /resources/ to be valid from sv2chisel point of view, but from chisel generation it MUST be within the src/(main|test)/resources of the Scala/Chisel project in order to be found by `addResource` method of `HasBlackBoxResource`.


##### translationOptions.Chiselizer.toCamelCase `Boolean`
```Verilog
module my_module #(
  parameter MY_PARAM
)(input my_input, output my_output);
  assign my_output = MY_PARAM ? my_input : '0
endmodule
```

<table td style="width:100%">
<tr><td><center> 

(default) `toCamelCase: false` </center></td>
<td><center> 

`toCamelCase: true` </center></td></tr>

<tr><td style="width:50%">

no identifiers are changed

```scala
class my_module(
  val MY_PARAM : Boolean
) extends Module {
  val my_input = IO(Input(Bool()))
  val my_output = IO(Output(Bool()))
  my_output := Mux(MY_PARAM, my_input, false.B)
}
```

</td><td style="width:50%">

all identifiers are converted to camelCase (assuming initial snake_case)

```scala
class MyModule(
  val myParam : Boolean
) extends Module {
  val myInput = IO(Input(Bool()))
  val myOutput = IO(Output(Bool()))
  myOutput := Mux(myParam, myInput, false.B)
}
```

</td></tr></table>

##### translationOptions.Chiselizer.ignoreEnumFieldScalastyle `Boolean`

```Verilog
typedef enum logic [1:0] {
    STATE_A = 2'd0,
    STATE_B = 2'd1,
    STATE_C = 2'd2
} my_enum;
```


<table td style="width:100%">
<tr><td><center> 

(default) `ignoreEnumFieldScalastyle: false` </center></td>
<td><center> 

`ignoreEnumFieldScalastyle: true` </center></td></tr>

<tr><td style="width:50%">

```scala
object my_enum extends GenericHwEnum {
  val STATE_A = Value
  val STATE_B = Value
  val STATE_C = Value
} 
```

</td><td style="width:50%">

```scala
object my_enum extends GenericHwEnum {
  val STATE_A = Value // scalastyle:ignore
  val STATE_B = Value // scalastyle:ignore
  val STATE_C = Value // scalastyle:ignore
} 
```

</td></tr></table>

---

<!-- ##### translationOptions.LegalizeParamDefaults `Dict`  -->
##### translationOptions.LegalizeParamDefaults.legalizeMethod `Enum`
Choose a strategy to legalize the reused parameters among the following options: 
- [*default*] `moveOrOverride`
- `comment`
- `overrideOption`
- `moveOrComment`

Given the following Verilog excerpt, here are the respective expected outputs:
```Verilog
module my_module #(
  parameter A = 3, 
  parameter B = A + 1
)(/* IOs */);
/* module logic */
endmodule
```
- **overrideOption** (universal but verbose, includes update of existing parameter maps of instance of `my_module` within the project)
```scala
class my_module(
  val A : Int = 3,
  val override_computed_B : Option[Int] = None
) extends Module {
  val B = override_computed_B.getOrElse(A + 1)
  /* IOs */
  /* module logic */
}
```

- **comment** (only work in practice if the parameter `B` is always overwritten during instantiation)
```scala
class my_module(
  val A : Int = 3,
  val B : Int = ??? /* A + 1 */
) extends Module {
  /* IOs */
  /* module logic */
}
```

- **move** (only applicable if the parameter `B` is never explicitly passed --and hence overwritten-- during instantiation, typical use-case: IO width computation)
```scala
class my_module(
  val A : Int = 3 
) extends Module {
  val B = A + 1
  /* IOs */
  /* module logic */
}
```

This last option, `move`, is by far the most elegant and the most accurate to translate the actual intent of HDL developers but unfortunately not universal.
The option is hence declined with either `comment` or `override` fallback options.


---

<!-- ##### translationOptions.RemoveConcats `Dict`  -->
##### translationOptions.RemoveConcats.useChiselCat `Boolean`
```Verilog
input  ia, ib, ic;
output oa;
output [1:0] ob;

assign {oa, ob} = {ia, ib, ic};
```

<table td style="width:100%">
<tr><td><center> 

(default) `useChiselCat: true` </center></td>
<td><center> 

`useChiselCat: false` </center></td></tr>

<tr><td style="width:50%">

Use Chisel Cat utility for RHS concatenation as it cannot unfortunately be used for LHS concatenations.

```scala
import chisel3.util.Cat

val ia, ib, ic = IO(Input(Bool()))
val oa = IO(Output(Bool()))
val ob = IO(Output(UInt(2.W)))

val auto_concat = Wire(new Bundle {
  val oa = Bool()
  val ob = UInt(2.W)
})
auto_concat := Cat(ia, ib, ic).asTypeOf(auto_concat)
oa := auto_concat.oa
ob := auto_concat.ob
```
</td><td style="width:50%">

```scala
val ia, ib, ic = IO(Input(Bool()))
val oa = IO(Output(Bool()))
val ob = IO(Output(UInt(2.W)))

val auto_concat = Wire(new Bundle {
  val oa = Bool()
  val ob = UInt(2.W)
})
val auto_concat_1 = Wire(new Bundle {
  val ia = Bool()
  val ib = Bool()
  val ic = Bool()
})
auto_concat_1.ia := ia
auto_concat_1.ib := ib
auto_concat_1.ic := ic
auto_concat := auto_concat_1.asTypeOf(auto_concat)
oa := auto_concat.oa
ob := auto_concat.ob
```

</td></tr></table>


> Up-to-date options syntax is detailed in the [toy-project config.yml](https://github.com/ovh/sv2chisel/blob/master/src/main/resources/project/config.yml)

### 1.2. sv2chisel-helpers Chisel library

#### Miscellaneous Helpers
sv2chisel-helpers provide a consequent amount of boiler-plate which considerably helps the transition from Verilog to chisel
- Subrange assignment of Vec
- Subrange assignment of Bundle
- MemInit primitive
- Hardware Strings support
- Hardware Enumeration support

All these helpers are automatically imported upon use by the translated Chisel code.
As stated in the [getting started guide](https://github.com/ovh/sv2chisel/#readme), the project in charge of compiling the generated Chisel shall include a proper libraryDependency to sv2chisel-helpers. 

#### Generation Utilities for Chisel Integration: *Chisel As IP*
> Note: An excerpt of [this talk](https://youtu.be/BHeZ0jxtmf4?t=346) given at *Chisel Community Conference 2021* introduces the issues tackled by the generation utilities introduced in this section

##### VerilogPortWrapper
Chisel-generated integration issue: **ports are flattened**
- `io: Vec(N, <type>)` become `io_1: <type>`, ..., `io_N: <type>`
- `io: Record` become `io_fieldA`, ..., `io_fieldN`

Solution: **provide a wrapper mapping expected ports to flattened ports**

```scala
class my_module extends RawModule {
  val in  = IO(Input(Vec(4, UInt(5.W))))
  val out = IO(Output(Vec(2, UInt(10.W))))
  out := in.asTypeOf(out)
}
```

**Native chisel generation:** `new ChiselStage().emitVerilog(new my_module())`
```Verilog
module my_module(
  input  [4:0] in_0,
  input  [4:0] in_1,
  input  [4:0] in_2,
  input  [4:0] in_3,
  output [9:0] out_0,
  output [9:0] out_1
);
  wire [19:0] _T = {in_3,in_2,in_1,in_0}; // @[main.scala 8:21]
  assign out_0 = _T[9:0]; // @[main.scala 8:21]
  assign out_1 = _T[19:10]; // @[main.scala 8:21]
endmodule
```

**VerilogPortWrapper additional generation:** `VerilogPortWrapper.emit(() => new my_module())` (original module is suffixed with `_raw`)
```Verilog
module my_module (
    input [3:0] [4:0] in,
    output [1:0] [9:0] out
  );
  my_module_raw inst (
    .in_0(in[0]),
    .in_1(in[1]),
    .in_2(in[2]),
    .in_3(in[3]),
    .out_0(out[0]),
    .out_1(out[1])
  );
endmodule
```

> `VerilogPortWrapper` also works with `Record` (and `Bundle`), in which case `className` (which can be explicitly overridden in the Chisel definition of the `Record`) is used as type reference.
> To be valid Verilog the wrapper will need the equivalent Verilog implementation.
> As of today this implementation can be provided as an import statement using the `initialStatements` option of `VerilogPortWrapper`: `initialStatements =  Seq("import my_compatibility_package::*;")`
>
> PR are welcome to instead automatically generate this package directly at the top of the wrapper :-)

##### ParamWrapperGenerator 
While `ParamWrapperGenerator` behave as the `VerilogPortWrapper` when `unflatPorts` option is set to true, its main focus mainly consists in generating a big switch between several parameter sets in order to provide an (almost) fully transparent interface as a Verilog-parameterizable module.

```scala
class Test(val inW: Int, val outW: Int) extends RawModule {
  val in  = IO(Input(UInt(inW.W)))
  val out = IO(Output(Vec(2, UInt(outW.W))))
  out(0) := in.asTypeOf(out(0))
  out(1) := in.asTypeOf(out(1))
}
```

```scala
val instances = Map(
    ParamSet(Seq(("IN_WIDTH", IntParam(5)), ("OUT_WIDTH", IntParam(5))))  -> (() => new Test(5, 5)),
    ParamSet(Seq(("IN_WIDTH", IntParam(10)), ("OUT_WIDTH", IntParam(2)))) -> (() => new Test(10, 2))
)
ParamWrapperGenerator.emit(instances, args = setTestRunDir)
```

Generates one large file

```systemVerilog
module Test_0(
  input  [4:0] in,
  output [4:0] out_0,
  output [4:0] out_1
);
  assign out_0 = in;
  assign out_1 = in;
endmodule

module Test_1(
  input  [9:0] in,
  output [1:0] out_0,
  output [1:0] out_1
);
  assign out_0 = in[1:0];
  assign out_1 = in[1:0];
endmodule


module Test #(
    parameter IN_WIDTH,
    parameter OUT_WIDTH
  )
  (
    input [9:0] in,
    output [1:0] [4:0] out
  );
  initial begin
    if (!( (IN_WIDTH == 5 && OUT_WIDTH == 5) ||
        (IN_WIDTH == 10 && OUT_WIDTH == 2) )) begin
      $info("The following values were provided:\n >IN_WIDTH = %d\n >OUT_WIDTH = %d", IN_WIDTH, OUT_WIDTH);
      $error("CRITICAL FAILURE: Unmapped parameter set");
      $fatal(1, "CRITICAL FAILURE: Unmapped parameter set");
    end
  end
  generate
    if(IN_WIDTH == 5 && OUT_WIDTH == 5) begin
      Test_0 Test(
        .in(in[4:0]),
        .out_0(out[0]),
        .out_1(out[1])
      );

    end else if(IN_WIDTH == 10 && OUT_WIDTH == 2) begin
      Test_1 Test(
        .in(in),
        .out_0(out[0][1:0]),
        .out_1(out[1][1:0])
      );
      assign out[0][4:2] = '0;
      assign out[1][4:2] = '0;
    end else begin: unmapped_param_set
      initial begin
        $info("The following values were provided:\n >IN_WIDTH = %d\n >OUT_WIDTH = %d", IN_WIDTH, OUT_WIDTH);
        $info("CRITICAL FAILURE: Unmapped parameter set");
      end
    end
  endgenerate
endmodule
```

##### Chisel as IP

Both `ParamWrapperGenerator` and `VerilogPortWrapper` aim at providing the ability to integrate Chisel code --would it be translated or handwritten-- *as IP* within larger HDL codebases.

We strongly believe that this integration capability with compatible interfaces is key to see an actual deployment of Chisel usage within existing *(legacy)* HDL code-bases.
More generally we hope to see this approach acting as a bridge to bring more hardware engineer aboard Chisel adventure!

---

## 2. Limitations

### 2.1. Translation Limitations
The following limitations are not related to Chisel, but purely to the development of sv2chisel.

#### Iterative Approach
sv2chisel has been developed with an iterative approach based on the translation of existing code-bases, and is hence not fully covering (System)Verilog syntax (yet).
This means it might *not* work out-of-the-box with your own code, some slight adjustment might be required either in input Verilog, output Chisel or both.

#### Non-synthesizable (System)Verilog constructs
Non-synthesizable (System)Verilog constructs --such as classes, interfaces, etc.-- are not supported.
In particular, translating testbenches is **not** the focus of sv2chisel. 
However, thanks to [*Chisel as IP* integration utilities](#generation-utilities-for-chisel-integration-chisel-as-ip), existing testbenches can easily be used to verify the Verilog generated by Chisel.    
 
#### No proper reset inference [TODO: infer resets]
- *asynchronous reset* are **NOT** supported yet, translation attempts might lead to undesirable side effects
- *synchronous reset* are yet treated as standard conditions 
  - `if(rst) my_reg <= <reset_value>;` translated to `when(rst) { my_reg := <reset_value> }` chisel hardware condition 
  - such reset registers are translated as `my_reg := Reg(<type>)` 
  - it results in 100% valid & functional Chisel but quite suboptimal and non-generic
  - proper reset inference should leverage the implicit reset provided by Chisel `Module` with `RegInit(<type>, <reset_value>)`
- *fpga preset* (`reg my_reg = <init_value>;`) are supported
  - translated as implicit synchronous reset (`RegInit(<type>, <reset_value>)`)
  - use the wrapper option `forcePreset = true` to emit them as preset in Chisel generated Verilog 
  - NB: they can also easily be emitted as asynchronous reset by mixing-in the trait `RequireAsyncReset`

#### Preprocessor directives are ignored [TODO: integrate define and ifdef wherever possible]
Critical warning are raised during emission of chisel code but other earlier warnings/error might be related (typically multiple definitions of the same signal).
Here are our current recommendations about translation of preprocessor directives:
- If the preprocessor directives are mostly used in leaf modules of the hierarchy (typically to select vendors primitives), the best quick-fix option will probably to black-box theses leaf modules *(fully supported by both sv2chisel & chisel)*
- If your design does rely heavily on preprocessor directives across its entire hierarchy, a manual translation might be preferable
- To emulate the global scope of preprocessor definition, typical scala idiomatic translation would rely on a global `object`, either with static value members or following a getter/setter pattern and while initialization should occur before chisel elaboration. 

#### Initial begin and assert are ignored [TODO: translate assertions; detect initializations]
- Reported by a warning at Verilog parsing stage
- Mostly non-functional verification statements with no impact on the quality of results (simulation & synthesis)
- Manual translations of initialization statement is required when impacting 


### 2.2. Scala/Chisel limitations impairing hardware development experience
The following limitations are not related to the translation tools but rather due to structural differences between Verilog and Chisel/Scala.
While they might cause temporary inconvenience and prevent a fully automated translation of some Verilog sources, please don't be scared as they are all fixable manually.

#### Large integers (>64 bits)
Scala `Int` are limited to 64 bits and `BigInt` (arbitrary long) are unfortunately not a universal replacement option as Chisel provides a limited support for them.
- this might lead to functional issues, in particular with shift left operations: `1 << 63` returns the lowest negative integer and `1 << 64` wraps to `1`
- `BigInt(1) << N` would behave as expected for any value of `N`, however, not all Chisel primitives accept BigInt, most notably `.W` for signal width definition
As a conclusion, sv2chisel emission relies only on scala `Int` but raises critical warnings on each integer shifting operation, kindly asking for the user to check by himself whether the shift operation is safe in the context.

#### From untyped bit-vector to `UInt` vs `Vec[Bool]` usage 
- `UInt` represent a bit-vector (of fixed width or not) 
  - it provides arithmetic and bitwise operations
  - however, they are not bit-assignable
- `Vec[T]` represent a collection of `T`, with T  seriously slow down the whole stack from generation to simulation
Typical example: IPv6 addresses are 128 bits vector one would like to be treated as such in the generated Verilog *(and not individual bits)*
- however bits operations are often used on IP addresses which leads to a difficult choice:
  - use UInt for performance and accept ugly boiler plate to assign some bits wherever required
  - use Vec for coding comfort but pay a high price in time spent by tools on the design
- as the boiler plate code is quite dependent on the context, sv2chisel has no choice but to use Vec in such cases

#### Negedge concept is not supported in Chisel
- hence not supported by sv2chisel
  - design heavily relying on posedge/negedge are quite low-level and might not benefit from an upgrade to Chisel
  - sub-modules relying on negedge can be easily blackboxed and integrated in translated hierarchy

#### Blocking assignment have no straightforward equivalent in Chisel
- hence not supported by sv2chisel 
- require manual translation
- a typical scala pattern to translate a for loop relying on variable and blocking assignment is a recursive function
  - the idea is to use the successive return values of the function as intermediate signals, just like variable with blocking assignment are unrolled by Verilog elaboration
  - the resulting scala is generally quite elegant and more readable than blocking assignments

 
