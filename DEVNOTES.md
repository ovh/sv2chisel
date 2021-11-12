# sv2chisel Development Notes

## TODO

### SHORT-TERM Improvements

#### URGENT

- fix ref serialization with path forgotten 
- LegalizeParamDefault
  - add cli option to *NOT* add override_auto_computed_ logic (only comment value as for extModule)
  - propagate override_auto_computed_ to defInstances

- infer UInt for bundle fields (non-critical, harder because usage might be cross descriptions)
- Improve behavior of inlineif: should be emitted as sw as soon as pred is sw 
- fix behavior of emission with path relative to ~ (~ considered as standard name)

- add true regression tests based on actual sv files (internal repo and CI ?) down to 
  - scala compilation
  - chisel elaboration
  - verilog generation
  - verilog testing with prior test-benches
  
- warn about ignored ifdef (silently ignored for now) 

- fix disabled cast from SInt to UInt (or double cast ??) => required by elaboration
- fix systematic .U cast => cause issue with not and minus primops => need case for:
  - negative literal
  - Prim Minus
  - Prim BitNot

- add special case for empty string used as hardware => single char at 0 

- front: catch "file not found" exception & terminate properly

#### CRITICAL
- FIX & USE OR DROP Special Vec of Char Inference for register affected to 
- LESS URGENT : support printf
- infer Boolean instead of UInt for parameters & then legalize software expression or include implicit conversion Int to Bool
- Remove all Utils.throwInternalError in IR & implicits => replace with Option, can raise errors properly on None in visitor/transforms/chiselizer 

#### NEXT-STEPS

##### Re-integrate ifdef (pre-proc macros) within AST 
- && legalize scoped declaration based on known pattern in the repo
- De-scope should be done directly here in this pass (only for port decl & val decl)

##### fix flow reference transform as stated in comment on top of its file

##### Add a transform to to automatically fix wire/reg selection depending on IfGen (not that easy) 

##### Add package hierarchy in emitted file, based on folder hierarchy

##### Create a set of transform/modifications to fix missing instances in project:
- foreach DefInstance referring to an unknown Module :
   - collect all IOs for this module if multiples instances
   - during project emission emit the Inferref Black-box
   - Replace DefInstance by DefBlackboxInstance: emission as 
- InferBlackBoxes transform :
  - PASS#1: collect unknown modules with all their IOs from potentially multiple instances 
  - PASS#2: replace all related DefInstance into "DefBlackboxInstance" by:
     - fixing potentially missing connection (DontCare inputs)
     - switch on a flag isBlackbox (to have differentiated emission)
- then: 
  - support the newly introduced switch in chiselizer
  - add blackbox class emission on project basis
   - inside a blackbox folder at emission root
   - with package blackboxes
   - NOTE: same emission as Module for IOs, only the param changes


*ERRATUM* : the following will not work well with param defaults in verilog let's hence keep the version with isBlackbox in DefInstance

// Emission template with no need for actual distinction between blackbox instances & standard ones : 
```scala
class ModuleName(param: Map[String, Param]) extends BlackBox(param) {
  val io = IO(
    ??? // depends on context
  )
}
```
Just understood why this Map syntax exist => default param in the verilog
if we wanted to go for this template we would  actually need to emit it with multiple apply function based to the various usages observed  => not flexible
```scala
object ModuleName {
  def apply(param1: Int, param2: String, param3: Boolean): ModuleName = {
    val params = Map(
      "param1" -> IntParam(param1),
      "param2" -> StringParam(param2)
      "param3" -> IntParam(param3)
    )
    new ModuleName(params)
  }
}
```
*END OF ERRATUM* 

##### instance management 
- might require project mode earlier than expected ?
- Generate at least dumb wrapper class around a black-box
- shall be an option to keep some module as verilog black-box (with proper resource) (for obvious lack of support of some verilog constructs // external IPs & libs not to be converted ...)
- 2 lists provided :
    - first for entry points (current one)
    - the other for black-boxing (IOs & params parsing only for blackbox wrapper generation)
- Note : when exploration is allowed, black-boxes have to be treated first
- Note : even mapping to IR should be stopped ? (at least when verboseness is low )
- Note : Instances decl knownledge should be leveraged to type DefInstance (Named)Assigns in TypeReference transform
- Alternative: be able to provide a lib of known module declaration (name, params, ports) 

#### IMPORTANT STUDY NOTES: 
- Statement re-ordering is NOT required so far (warning in msim, error in synth, + no example so far)
- De-scope is not required either => actually there are also scopes in verilog 

#### TECH DEBT
1./ downto/upto type vs usage management ... 
    ensure it will work with implicits & casts 

2./ TOWARDS OPEN-SOURCING 
- make implicits import unrelated to fpga-vac-chisel for open-sourcing
- remove all references to existing hash-table files


#### Low-hanging fruits

1./ Add More Test-cases
    - improve/fix TODO left in ComplexParamSpec

2./ Better comment integration => force closing brackets to have proper tokens
3./ Better emission for expression add space around first Doprim with binary op
4./ Review concat conversion to bundle => MixedVecInit(...).asTypeOf could be leveraged instead
 
####  MID-TERM Improvements

0./ infer main clock & reset 
=> decide mapping strategy to avoid whithClockAndReset within Module
=> no explicit reset : add comment on top of file to say the module was designed with presets 
=> single clock => map to clock (default one for Module) 

project mode => hierarchy inference => preset set at top level
single source => add reset.annotate(PresetAnnotation) (not required thanks to fpga-vac-chisel utils)

1./ Project Mode: 
- hierarchic path search (would be perfect for hash-table example)
   - entry point + path to be searched (+ option recursive search + option filetype)
- add the ability to provide a list of chisel modules (not to be found in hierarchy - needs to be imported as scala object > publish fpga-vac-chisel locally and add it as a dependency of sv2chisel in built.sbt > not flexible for open-sourcing project but would do the trick for the current project at OVH  )

2./ Seriously add scaladoc with at least basic description of each transform principles 

3./ Supports always more verilog constructs...
- fpga-vac code base
- pick some interesting open-source projects from the Internet

4./ Generate a default sbt project with all required files (build.sbt, build properties, ...) 

## Concept: source to source translation

### OVH context
- 1 verilog file = 1 module => 1 chisel file = 1 module class
- sv packages = 1 file => 1 chisel file => package object ? full package ?
  - depends on what to be stored inside
  - constants => package object
  - 
- PROJECT TRANSLATION? use folder hierarchy to infer package hierarchy
  - How to handle common-generated package ? => package at root
  - 
- Intermediate STEP: single file translation : Hash-table
  - auto black-box included sources ?
  - generate (empty) wrapper for SV black-boxes ?

### Misc considerations 
- only for HW source files for know
- what about conversion of SV test-benches ?
  - FIRST STEP: ensure proper tooling to preprocess TB
    - get instantiation parameters (potentially multiple modules due to FIFO, ...)
    - generate corresponding verilog source with Chisel
    - launch tests as usual (fdo)
    - containerization ??? => remove after ?
    - automatization ?
      - pre-process all for missing modules ?
      - catch vsim launch errors ?
      - generate big "generate switch" for module with multiple parameterizations called within the design ?
      
  - SECOND STEP: convert to chisel-tester peek/poke scheme ?
    - use posedgePokeWrapper
    - use delayed pokes to optimize perfs by default => emulates 
    - invent a new test paradigm? Fully parallel just like TB in current SV ?
- Need to develop a tool 
  - to lookup for all Chisel HW classes => modules available
  - list all verilog Modules
  - build map between both 
  - Needed for :
    - test mapping => are we in front of a chisel instance or a simple verilog module ?
- Logic => Reg / Wire inference ???
 
    

## REQUIREMENTS
### MUST HAVE
- source to source translation on file basis
- tests linkage (see above)

### ADDITIONAL FEATURES
- project to project translation 
  - take care of folder hierarchy as package hierarchy
- auto black-boxing (additional wrapper with ressource & param mapping)
  - might be harder than expected => type inference 
    - at ovh only integer and string => not that hard
  - param that depends for one another 
    - dirty hack in SV => nothing to be don
    - chisel should retain no default arguments in module class => bad practices
- convert 

## INTERNAL STRUCTURE
- Parser
  - Lexer / Parser with g4 syntax
  - scala management (bit unclear)
- IR / AST
  - Able to represent all chisel HW statements 
  - Only the subset that makes sense in SV
  
- Emitter
  - Convert IR / AST into Chisel source
  - the closer to Chisel is the IR => the easier / more straightforward the Emitter will be
  - Include Emission within IR through toString ???
    - makes it very straightforward 
    - might be quite ugly in the long run ?
    - not scalable BUT we do not want to scale ...
    


## Solving issues
- comments & layout 
  - store tokens interval for all SVNode of the IR
  - push comments & WS to a separate channel
  - re-integrate comments & WS during emission
  - OR re-integrate them as part of the IR (to be extended accordingly) with a transform 

- pre-processor
  - see "Islands in the Stream" from the ANTLRv4 reference http://lms.ui.ac.ir/public/group/90/59/01/15738_ce57.pdf ???
  - use the same method as for comments & layout
    - separate stream
    - re-integrate later ? as IR transform
      - if the ifdef-else-endif pattern enclose a meaning full part of the AST => OK
      - otherwise => fail as UNSUPPORTED
  - note that our goal is to provide equivalence for part of pre-proc commands only
    - most of it will be assumed to be unsupported
    - we are not here to build a sv2017 fully compliant parser
    - our goal is to extract meaningful hardware representation from verilog hdl source file
    - and that is not so obvious as most of verilog spec is awfully not intended to express hardware description ...
    
    
- expressions
  - rewrite very simple PrimOps
    - no propagate type
    - remove dependencies on fixed, interval, contraints, ...
    - DoPrim has no Type
  - do we even need to finely parse expressions ?
    - is there any usual changes between verilog & scala expression ?
    - yes => constant parsing at least    
    
