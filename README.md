# sv2chisel (System)Verilog to Chisel Translator
[![Maintenance](https://img.shields.io/maintenance/yes/2022.svg)]() 
[![License](https://img.shields.io/badge/license-BSD%203--Clause-blue)](https://github.com/ovh/sv2chisel/blob/master/LICENSE)
[![GitHub tag (latest SemVer)](https://img.shields.io/github/v/tag/ovh/sv2chisel.svg?include_prereleases&sort=semver)](https://github.com/ovh/sv2chisel/releases/latest)
<!-- [![Build Status](https://travis-ci.org/ovh/sv2chisel.svg)](https://travis-ci.org/ovh/sv2chisel)  -->
**sv2chisel** translates synthesizable (System)Verilog to *low-level* Chisel.

The resulting Chisel is intended to be manually refactored to benefit from the advanced Chisel's features such as type and functional parameterization thanks to Scala support for polymorphism and high-order functions. 
Several research efforts, such as [this paper](https://hal.archives-ouvertes.fr/hal-03157426/document), demonstrate in practice the relevance of such refactoring.

**sv2chisel** has achieved 1:1 translation of several large codebases with few or no manual modifications.
[Discover all Features & Limitations](https://github.com/ovh/sv2chisel/blob/master/FEATURES_LIMITATIONS.md#sv2chisel-features--limitations) 

**sv2chisel** emerged as a research effort, if you use it or borrow some concepts in your own work, please cite the [associated paper](https://hal.archives-ouvertes.fr/hal-02949112/document) [(bibtex entry below)](#citing-this-work).

### Project Contents
The current repository holds the sv2chisel project which is divided in two parts: 
- **sv2chisel** a standalone tool, able to translate (System)Verilog sources into Chisel
- *sv2chisel-helpers* a Chisel/scala library, sometimes required by the translated Chisel and more generally providing [various utilities to integrate Chisel into HDL projects](https://github.com/ovh/sv2chisel/blob/master/FEATURES_LIMITATIONS.md#generation-utilities-for-chisel-integration-chisel-as-ip)

---

# Getting Started
 
## Get sv2chisel
> Note: the version of sv2chisel x.5.x is aligned on Chisel stack 3.5.x and the versioning intends to follow the same evolution as Chisel stack one on minors

#### Native Binaries
sv2chisel releases provide native standalone binaries for the following platforms:
- [Linux](https://github.com/ovh/sv2chisel/releases/download/v0.5.0/sv2chisel_linux_amd64) *(tested on a regular basis on Ubuntu 20.04)*
- [Darwin](https://github.com/ovh/sv2chisel/releases/download/v0.5.0/sv2chisel_darwin_amd64) *(tested on a regular basis on MacOS with Darwin Kernel Version 17.7.0)*
- [Windows](https://github.com/ovh/sv2chisel/releases/download/v0.5.0/sv2chisel_windows_amd64.exe) *(tested on windows 10 -- git-bash is recommended for colored console printing)*

Just `chmod u+x sv2chisel_<build>` and you are all set for your first translation!

#### Fat JAR
sv2chisel releases also provide a [standalone jar]((https://github.com/ovh/sv2chisel/releases/download/v0.5.0/sv2chisel_jar.tar.gz)) file which only require a jvm installation.

Just `untar -xzf sv2chisel_jar.tar.gz` and you are all set for your first translation! 


#### From code
Finally, sv2chisel code can be directly executed with any working sbt installation by cloning this repository, see [direct usage from sources](#direct-usage-from-sources).

## Setup your own Chisel project
If this is your first time using [Chisel](https://www.chisel-lang.org), we highly recommend you to follow either the [online bootcamp](https://mybinder.org/v2/gh/freechipsproject/chisel-bootcamp/master) or the [chisel-tutorials](https://github.com/ucb-bar/chisel-tutorial) first.

To set-up efficiently a new Chisel project, the easiest way is to clone the [chisel-template](https://github.com/freechipsproject/chisel-template).

As final step, you need to include *sv2chisel-helpers* library in this new project, by adding the following line to your local `build.sbt` file:
```sbt
// sv2chisel was first published in 2021, on new sonatype servers hence requiring non default resolvers
resolvers ++= Seq(
  "New Sonatype Snapshots" at "https://s01.oss.sonatype.org/content/repositories/snapshots/",
  "New Sonatype Releases" at "https://s01.oss.sonatype.org/service/local/repositories/releases/content/",
)
// For simpler usage, sv2chisel minor version is aligned on chisel stack minor version: x.5.x 
libraryDependencies += "com.ovhcloud" %% "sv2chisel-helpers" % "0.5.0"
```

This new project is intended to be used as output target for the Chisel files converted from your (System)Verilog sources.

---

# Usage
Complete 4-steps process, from (System)Verilog descriptions to upgraded Chisel generators:

1. [Translation](#1-translation)
2. [Creation of a Chisel main](#2-creation-of-a-chisel-main)
3. [Correctness Test](#3-test-check-translation-correctness)
4. [Manual upgrade of the 1-1 translation to more idiomatic scala/chisel syntaxes](#4-upgrade-your-low-level-chisel)
---

## 1. Translation
### Option A: Create a config file for your HDL project
See https://github.com/ovh/sv2chisel/blob/master/src/main/resources/project/config.yml

Then simply run sv2chisel with this config file:
```bash
./sv2chisel -c config.yml
```

Additional option (such as verbosity control) can be found with `./sv2chisel -help`

### Option B: Raw CLI Usage
See `./sv2chisel -help` to get started, basic usage:

```bash
./sv2chisel <path/to/my_verilog_file1.sv> ... <path/to/my_verilog_fileN.sv>
```

---

**IMPORTANT TRANSLATION NOTICE**

Please review and fix (or at least acknowledge) any error (fatal/critical) messages reported by **sv2chisel** before proceeding any further, as the generated Chisel code will probably not be usable in such cases.

Do not hesitate to raise an issue [here](https://github.com/ovh/sv2chisel/issues) in case of trouble.

---


## 2. Creation of a Chisel Main 

In order to check the translation correctness of the translation, let's now translate it back to Verilog!
Yeah it sounds silly but it's the way it works: Chisel is an hardware construction language, not intended to be provided directly to synthesis and simulation tools but rather to be executed and produce a low-level Verilog, almost down to netlist.


Getting started with Chisel generation API can be a bit frightening for Scala/Chisel newcomers, fortunately sv2chisel is able to generate that boilerplate for you.
See [details about specifying a top-level in the config file](https://github.com/ovh/sv2chisel/blob/master/FEATURES_LIMITATIONS.md#translationoptionschiselizertoplevelchiselgenerators-listdict) or [Manual setup example below](#manual-chisel-project-setup).

>In a nutshell, just set the `translationOptions.Chiselizer.topLevelChiselGenerators` option in your config file, using the syntax presented in the [example config file](https://github.com/ovh/sv2chisel/blob/master/src/main/resources/project/config.yml)

It will generate an object App such as `object my_moduleGen extends App {/* */}`.
To generate your Verilog, run this app with sbt `runMain my_moduleGen` if placed in *src/main/scala* or with sbt `Test / runMain my_moduleGen` if placed in *src/test/scala*.

---

**Error reporting**

- `compile` step should not raise errors, if it does, please raise an issue [here](https://github.com/ovh/sv2chisel/issues)
- Similarly, Chisel elaboration step (in between `Elaborating...` and `Done elaborating.` message) should not raise errors, if it does, please raise an issue [here](https://github.com/ovh/sv2chisel/issues)
- Finally, FIRRTL compilation step might raise errors, in particular related to missing connections in your design. 
This is due to a strict Chisel/FIRRTL toolchain policy: no declared wire shall be left unassigned and no latches are allowed.
If you do get such errors, you are welcome to fix them yourself either in the verilog or the generated chisel, but do not hesitate reach the very welcoming [Chisel/FIRRTL community](https://www.chisel-lang.org/community.html) for help!

---

## 3. Test: Check translation correctness
You can now integrate the chisel-emitted Verilog into your usual simulation and synthesis flow, and check that it is consistent.
Simulation should pass and synthesis produce on-par resource usage results.
If it is not the case, investigate the translation result, be sure you understand the implication of every warning message and feel free to open an issue for help or to raise a discovered bug.

> Beware that ports are flattened in the resulting verilog, you might hence need a verilog wrapper to integrate the resulting verilog, [*fortunately sv2chisel can bring it to you!*](https://github.com/ovh/sv2chisel/blob/master/FEATURES_LIMITATIONS.md#translationoptionschiselizertoplevelchiselgenerators-listdict)
> - a port `myport: Vec(n+1, <>)` *(verilog [N:0])* becomes n ports from `myport_0: <>` to `myport_n: <>`
> - a port `myport: Bundle` *(verilog struct)* becomes several individual ports named after the fields names such as `myport_myfieldA` ... `myport_myfieldN`


## 4. Upgrade your low-level Chisel
Your translated project is working as expected?
Here is precisely where the whole fun starts, and where this step-by step guide stops.
Please refer to [Chisel documentation](https://www.chisel-lang.org) for various user-guides and examples to truly unleash Chisel's generation powers.

---

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
If you have successfully published sv2chisel locally, then you are all set to start hacking into the code, otherwise see [direct usage from sources](#direct-usage-from-sources).

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

## Release to Maven Central
### Setup
- NB: Based on `sbt-pgp` & `sbt-sonatype` plug-ins
- Add Credentials in `~/.sbt/1.0/sonatype.sbt`
```sbt
credentials += Credentials("Sonatype Nexus Repository Manager",
        "s01.oss.sonatype.org", // created after 2021
        "<sonatypeUserName>",
        "<sonatypePwd>")
```
- Add PGP private key in your/CI keyring


### Release options
1. *SNAPSHOTS*
```
sbt:sv2chisel> publishSigned
sbt:sv2chisel> helpers/publishSigned
```

2. *RELEASE:* Based on sbt-release plugin, just follow the instruction of `sbt 'release'`

 
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

---

# Annexes

---

## Direct Usage From Sources

### Prerequisite
- Install sbt [official documentation](https://www.scala-sbt.org/1.x/docs/Setup.html)

### Publish locally sv2chisel & sv2chisel-helpers
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

### Translation
#### Option A: Run the generic application
Either from shell
```bash
sbt 'runMain sv2chisel.Main -c config.yml'
```

or directly in sbt to avoid sbt startup time
```bash
sbt:sv2chisel> runMain sv2chisel.Main -c config.yml
```

Running `runMain sv2chisel.Main -help` details available options, in particular control of the level of verbosity.

#### Option B: Create your own translator app
##### Setup
Create a new empty scala project. [Official documentation](https://docs.scala-lang.org/getting-started/sbt-track/getting-started-with-scala-and-sbt-on-the-command-line.html) 

To be able to use **sv2chisel** within this newly created project, just add the following line to your `build.sbt` file.
```sbt
libraryDependencies += "com.ovhcloud" %% "sv2chisel" % "0.1.0-SNAPSHOT"
```

Create a new Scala main app template using sv2chisel API, here is a template with a few comments to be used as a starting point to translate your (System)Verilog file(s) or project(s):
https://github.com/ovh/sv2chisel/blob/master/src/main/scala/sv2chisel/AppExample.scala

This template is to be edited to fit your needs and saved under a proper scala hierarchy such as `<your-project>/src/main/scala/<MyTranslator.scala>`

##### Translate your code 
In your translator project sbt: `runMain MyTranslator`

---

## Manual chisel project setup

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
