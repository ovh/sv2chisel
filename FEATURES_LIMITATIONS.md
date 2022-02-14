## Features and limitations of sv2chisel

Contents
TODO: table of content

### Features



### Known major translation limitations
#### No proper reset inference [TODO: infer resets]
- *asynchronous reset* are **NOT** supported yet, their use might lead to undesirable side effects
- *synchronous reset* are yet treated as standard conditions 
  - `if(rst) my_reg <= <reset_value>;` translated to `when(rst) { my_reg := <reset_value> }` chisel hardware condition 
  - such reset registers are translated as `my_reg := Reg(<type>)` 
  - it results in 100% valid Chisel but quite suboptimal and non-generic
  - proper reset inference should leverage the implicit reset provided by Chisel `Module` with `RegInit(<type>, <reset_value>)`
- *fpga preset* (`reg my_reg = <init_value>;`) are supported
  - translated as implicit synchronous reset (`RegInit(<type>, <reset_value>)`)
  - use the wrapper option `forcePreset = true` to emit them as preset in Chisel generated Verilog 
  - NB: they can also easily be emitted as asynchronous reset by mixing-in the trait `RequireAsyncReset`

#### Preprocessor directives are ignored [TODO: re-integrate wherever possible]
Critical warning during emission of chisel code but can lead to other warnings/error (typically multiple definitions of the same signal).
Current hints about translation of these statements:
- If the preprocessor directives are mostly used in leaf modules of the hierarchy (typically to select vendors primitives), the best quick-fix option will probably to black-box theses leaf modules *(fully supported by both sv2chisel & chisel)*
- If your design does rely heavily on preprocessor directives across its entire hierarchy, a manual translation might be preferable
- To emulate the global scope of preprocessor definition, typical scala idiomatic translation would rely on a global `object`, either with static value members or following a getter/setter pattern and while initialization should occur before chisel elaboration. 

#### Initial begin and assert are ignored [TODO: translate assertions; detect initializations]
- Reported by a warning at Verilog parsing stage
- Mostly non-functional verification statements with no impact on the quality of results (simulation & synthesis)
- Manual translations of initialization statement is required when impacting 

### Scala/Chisel limitations impairing user-experience
#### Scala Int restricted to 64 and limited Chisel support of BigInt
Scala `Int` are limited to 64 bits and BigInt are not a transparent replacement option.
- this might lead to functional issues, in particular with shift left operations: `1 << 63` is the lowest negative integer and `1 << 64` will wrap to `1`
- `BigInt(1) << N` would behave as expected for any value of `N`, however, not all Chisel primitives accepts BigInt, most notably `.W` (width)

#### `UInt` are not bit-assignable while large `Vec` seriously slow down the whole stack from generation to simulation
Typical example: IPv6 addresses are 128 bits vector one would like to be treated as such in the generated verilog *(and not individual bits)*
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
  - the idea is to use the successive return values of the function as intermediate signals, just like variable with blocking assignment are unrolled by verilog elaboration
  - the resulting scala is generally quite elegant and more readable than blocking assignments

#### A good news to smooth a bit the roughness of user-experience
sv2chisel-helpers provide a consequent amount of boiler-plate which considerably helps the transition from verilog to chisel
- VerilogPortWrapper & ParamWrapperGenerator to integrate a Chisel module *as IP* in existing design
- Subrange assignment of Vec
- Subrange assignation of Bundle
- MemInit primitive
- Hardware Strings support
- Hardware Enumeration support *(to be deprecated when natively integrated to Chisel)*

All these helpers are automatically imported upon use by the translated Chisel code.
As stated in the getting started guide, the project in charge of compiling the generated Chisel shall include a proper libraryDependency to sv2chisel-helpers. 
 
