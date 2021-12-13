# sv2chisel (System)Verilog to Chisel Translator
[![Maintenance](https://img.shields.io/maintenance/yes/2022.svg)]() 
[![License](https://img.shields.io/badge/license-BSD%203--Clause-blue)](https://github.com/ovh/sv2chisel/blob/master/LICENSE)
<!-- [![Build Status](https://travis-ci.org/ovh/sv2chisel.svg)](https://travis-ci.org/ovh/sv2chisel)  -->
**sv2chisel** translates synthesizable (System)Verilog to *low-level* Chisel.

The obtained Chisel is intended to be manually refactored to benefit from the advanced chisel's features such as type and functional parameterization thanks to Scala support for polymorphism and high-order functions. 

Important notice: **sv2chisel** was developed with a test-driven development methodology, and is hence not fully covering (System)Verilog syntax.
This means it might probably *not* work out-of-the-box with your own code, some slight adjustment might be required either in input Verilog, output Chisel or both.

**sv2chisel** emerged as a research effort, if you use it or borrow some concepts in your own work, please cite the [associated paper](https://hal.archives-ouvertes.fr/hal-02949112/document) (bibtex entry below).

# Content
The current repository contains 2 projects:
- **sv2chisel** the (System)Verilog to Chisel translator
- *sv2chisel-helpers* a small chisel library of implicit helpers used to make Chisel emission smoother, most notably for the usual (System)Verilog index and range affectation pattern

Note: *sv2chisel-helpers* is not used directly by **sv2chisel** but by the emitted Chisel code. (Check if emitted files include some `import sv2chisel.helpers`)

# Installation
**sv2chisel** and *sv2chisel-helpers* are currently not available on maven central and hence have to be manually published locally, which only takes a couple of steps detailed below.

## Prerequisite
- Install sbt [official documentation](https://www.scala-sbt.org/1.x/docs/Setup.html)

## Publish locally sv2chisel & sv2chisel-helpers
```bash
git clone https://github.com/ovh/sv2chisel.git
cd sv2chisel
sbt
```
In sbt shell
```sbt
sbt:sv2chisel> publishLocal
sbt:sv2chisel> helpers/publishLocal
```
The `publishLocal` commands make sv2chisel and sv2chisel-helpers libraries available locally to be used in other Scala project.  

## Setup your own Chisel project
If this is your first time using [Chisel](https://www.chisel-lang.org), we highly recommend you to follow either the [online bootcamp](https://mybinder.org/v2/gh/freechipsproject/chisel-bootcamp/master) or the [chisel-tutorials](https://github.com/ucb-bar/chisel-tutorial) first.

To set-up efficiently a new Chisel project, the easiest way is to clone the [chisel-template](https://github.com/freechipsproject/chisel-template).

As final step, you need to include sv2chisel-helpers library in this new project, by adding the following line to your `build.sbt` file:
```sbt
libraryDependencies += "com.ovhcloud" %% "sv2chisel-helpers" % "0.1.0-SNAPSHOT"
```

This new project is intended to be used as output target for the Chisel files converted from your (System)Verilog sources.

## Create a separate sbt project for your own translator app
*This step is optional, if you want to skip it, just hack into the existing `Main.scala` within your local clone of sv2chisel, editing it as described in the next step.*

Create another empty sbt project. [Official documentation](https://docs.scala-lang.org/getting-started/sbt-track/getting-started-with-scala-and-sbt-on-the-command-line.html) 

To be able to use **sv2chisel** within this newly created project, just add the following line to your `build.sbt` file.
```sbt
libraryDependencies += "com.ovhcloud" %% "sv2chisel" % "0.1.0-SNAPSHOT"
```

# Usage
The process follow the following steps:

1. Translation, *either* [simple](#1-translation-simple) *or* [advanced](#1-translation-advanced)
2. [Creation of a Chisel main](#2-creation-of-a-chisel-main)
3. [Correctness Test](#3-test-check-translation-correctness)
4. [Manual upgrade of the 1-1 translation to more idiomatic scala/chisel syntaxes](#4-upgrade-your-low-level-chisel)
---

## 1. Translation (simple)
### A. Create a config file
See https://github.com/ovh/sv2chisel/blob/master/src/main/resources/project/config.yml

### B. Run the generic application
Either from shell
```bash
sbt 'runMain sv2chisel.Main -c config.yml'
```

or directly in sbt to avoid sbt startup time
```bash
sbt:sv2chisel> runMain sv2chisel.Main -c config.yml
```

Running `runMain sv2chisel.Main -help` details available options, in particular control of the level of verbosity. 

## 1. Translation [ADVANCED]

### A. Create your own translator app
Now that **sv2chisel** and *sv2chisel-helpers* libraries are installed on your machine, create a new Scala main app template using sv2chisel API. 
Here is a template with a few comments to be used as a starting point to translate your (System)Verilog file(s) or project(s):
https://github.com/ovh/sv2chisel/blob/master/src/main/scala/sv2chisel/AppExample.scala

This template is to be edited to fit your needs and saved under a proper scala hierarchy such as `<your-project>/src/main/scala/<MyTranslator.scala>`

### B. Translate your code 
1. In your translator project sbt: `runMain MyTranslator`
2. In your chisel-ready project sbt: `compile` should not raise errors, if it does, please raise an issue here

---

## 2. Creation of a Chisel Main
In order to check the translation correctness of the translation, let's now translate it back to Verilog!
Yeah it sounds silly but it's the way it works: Chisel is an hardware construction language, not intended to be provided directly to synthesis and simulation tools but rather to be executed and produce a low-level Verilog, almost down to netlist.

Let's take an example of input verilog
```verilog
module test #(
    param TEST = 1
  )(
    input clock,
    input reset,
    input a,
    output b
  )
  // module body
endmodule
```

that would be translated into Chisel
```scala
package myproject

import chisel3._
import sv2chisel.helpers.vecconvert._ // assuming module body requires it

class test extends MultiIOModule (
    val TEST: Int = 1
  ){
    val a = IO(Input(Bool())
    val b = IO(Output(Bool())
  
  // module body
}
```

We will use this small chisel main app to generate it:

```scala
import myproject._
import chisel3.stage._

object MyTestGenerator extends App { 
  (new ChiselStage()).emitVerilog(new test(10))
}
```
To produce your Verilog, run this app with sbt `runMain MyTestGenerator` if placed in *src/main* or with sbt `test:runMain MyTestGenerator` if placed in src/test.


## 3. Test: Check translation correctness
You can now integrate the chisel-emitted Verilog into your usual simulation and synthesis flow, and check that it is consistent.
Simulation should pass and synthesis produce on-par resource usage results.
If it is not the case, investigate the translation result, be sure you understand the implication of every warning message and feel free to open an issue for help or to raise a discovered bug.

## 4. Upgrade your low-level Chisel
Here we are, here is precisely where the whole fun starts, and where this step-by step guide stops.
Please refer to [Chisel documentation](https://www.chisel-lang.org) for user-guide and example around Chisel's generation powers.

# Citing this work
If you use this work or borrow some concept for your own research, please cite the following [paper](https://hal.archives-ouvertes.fr/hal-02949112/document):
```bibtex
@inproceedings{bruant2020sv2chisel,
  author    = {Jean Bruant and
               Pierre-Henri Horrein and
               Olivier Muller and
               Tristan Grol{\'{e}}at and
               Fr{\'{e}}d{\'{e}}ric P{\'{e}}trot},
  title     = {(System)Verilog to Chisel Translation for Faster Hardware Design},
  booktitle = {Proceedings of the 31th International Workshop on Rapid System Prototyping,
               {RSP} 2020, Virtual Conference, September 24-25, 2020},
  publisher = {{ACM}},
  year      = {2020},
}
```

# Contributing

You've developed a new cool feature? Fixed an annoying bug? We'd be happy
to hear from you!

## Getting Started
If you have successfully published sv2chisel locally, then you are all set to start hacking into the code.

## Documentation 
The in-code documentation remains yet quite sparse.
Here is a quick overview of the code base, within *src/main/*:
1. Lexing & Parsing to IR
    - *antlr4/* ANTLR SystemVerilog Lexer & Parser (generates Java sources within *sv2chisel/target/scala-2.12/src_managed/main/antlr4/sv2chisel/antlr/*)
    - *scala/sv2chisel/Visitor.scala* Visits the AST generated by the parser and map it into our custom Intermediate Representation (IR)
    - *scala/sv2chisel/ir/* describe our IR which is based on [Firrtl](https://github.com/freechipsproject/firrtl/). Main content seats within *IR.scala* while the side files provide many convenient implicit functions operating on the IR.
2. Transforms
    - *scala/sv2chisel/Driver.scala* contains the list of transforms applied to a project
    - *scala/sv2chisel/transforms/* folder contains all the transforms
3. IR to Chisel
    - *scala/sv2chisel/Chiselizer.scala* provide implicit functions to convert the IR into Chisel tokens. Note that some constructs of the IR are expected to be removed prior this step, thanks to previous transforms.
    - *scala/sv2chisel/Emitter.scala* synchronize the chisel token stream with the original token stream in order to re-insert comments and some part of the layout into the final Chisel text to be written to file.   

## Testing
All unit-tests can be found within *src/test* and can be run with sbt `test` for the whole batch or with `testOnly sv2chiselTests.TestName` for a particular one.
Please add new test-cases along with new features or bug fix to highlight the quality of your contribution.

During main development process, functional testing was carried out both with the open-source RISC-V core [PicoRV32](https://github.com/cliffordwolf/picorv32/) and some internal verilog libraries at OVHcloud.

Continuous integration system including such automated functional testing is under investigation.

## Sharing your modifications
Have a look in [CONTRIBUTING.md](https://github.com/ovh/sv2chisel/blob/master/CONTRIBUTING.md) and feel free to submit a pull-request on this repository.
 
 
# Related links
 * Contribute: https://github.com/ovh/sv2chisel/blob/master/CONTRIBUTING.md
 * Report bugs: https://github.com/ovh/sv2chisel/issues
 <!-- * Get latest version: TODO: maven publishing -->
 
# Licenses
## External licenses
- [Firrtl](https://github.com/freechipsproject/firrtl/) borrowed sources: https://github.com/ovh/sv2chisel/blob/master/LICENSE.firrtl
- [Nic30's](https://github.com/Nic30/hdlConvertor) SystemVerilog Parser https://github.com/ovh/sv2chisel/blob/master/LICENSE.Nic30

## sv2chisel license
See https://github.com/ovh/sv2chisel/blob/master/LICENSE

