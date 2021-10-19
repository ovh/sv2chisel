// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

package sv2chisel

import logger._

import io.circe.yaml
import io.circe._

import java.io.{InputStreamReader, FileInputStream}
import org.sellmerfud.optparse._

/** Simple case class holding command-line configuration options */
case class Sv2ChiselCliConfig(
  // Generation options
  logLevel: LogLevel.Value = LogLevel.Info,
  emissionPath: String = "", // priority over config file
  // Main config file => priority over manual setup below
  configFile: String = "", 
  // Manual configuration for simple use-cases
  projectName: String = "",
  basePath: String = "",
  projectFiles: Seq[String] = Seq()
)

/** Simple case class holding configuration of a project */
case class Sv2ChiselProjectConfig(
  name: String = "",
  basePath: String = "",
  emissionPath: String = "",
  files: Seq[String] = Seq()
) {
  /** Decoder for circe parsing from yaml */
  def decode: Decoder[Sv2ChiselProjectConfig] = Decoder.instance(c => {
    val default = Sv2ChiselProjectConfig()
    for {
      name <- c.getOrElse[String]("name")(default.name)
      basePath <- c.getOrElse[String]("basePath")(default.basePath)
      emissionPath <- c.getOrElse[String]("emissionPath")(default.emissionPath)
      files <- c.getOrElse[Seq[String]]("files")(default.files)
    } yield {
      Sv2ChiselProjectConfig(name, basePath, emissionPath, files)
    }
  })
}

/** Main App providing CLI */
object Main extends App with EasyLogging {
  
  // Command-line argument parsing
  val cli = new OptionParser[Sv2ChiselCliConfig] {
    // Add an argument parser to handle LogLevel values
    addArgumentParser[LogLevel.Value] { arg => 
      try { 
        LogLevel(arg)
      } catch {
        case e: Exception => throw new InvalidArgumentException(s" (${e.getMessage})") 
      }
    }
    
    banner = "sv2chisel [Options] sv_files... or sv2chisel [Options] -c config_file"
    separator("")
    separator("Commons Options:")
    reqd[LogLevel.Value]("-l", "--log-level <error|warn|struct|info|debug|trace>", "Logger verbosity level") { 
      (value, cfg) => cfg.copy(logLevel = value)
    }
    reqd[String]("-o", "--emission-path PATH", "Base emission path") { (value, cfg) => cfg.copy(emissionPath = value) }
    separator("")
    separator("Config File (prio over manually specified files):")
    reqd[String]("-c", "--config-file FILE", "Yaml Configuration File") { (value, cfg) => cfg.copy(configFile = value) }
    separator("")
    separator("Manual command-line configuration")
    reqd[String]("-i", "--base-path PATH", "Base path for files") { (value, cfg) => cfg.copy(basePath = value) }
    reqd[String]("-n", "--name NAME", "Project name") { (value, cfg) => cfg.copy(projectName = value) }
    arg[String] { (arg, cfg) => cfg.copy(projectFiles = cfg.projectFiles :+ arg) }
  }
  
  val cfg = try { 
    cli.parse(args, Sv2ChiselCliConfig())
  } catch {
    case e: OptionParserException => {
      // "clean" exit without stack-trace
      fatal(e.getMessage)
      println(cli)
      sys.exit(0)
    }
  }
  
  Logger.setLevel(cfg.logLevel)
  
  // Command line argument validation
  val projects = if(cfg.configFile != ""){
    if(!cfg.projectFiles.isEmpty) {
      val files = cfg.projectFiles.mkString("[",", ","]")
      critical(s"Config file ${cfg.configFile} specified. The following manually specified files are ignored: $files")
    }
    
    // YAML PARSING
    yaml.parser.parse(new InputStreamReader(new FileInputStream(cfg.configFile))) match {
      case Left(failure) => 
        fatal(s"Error while reading YAML: $failure")
        sys.exit(1)
      
      case Right(json) => 
        implicit val decoder = Sv2ChiselProjectConfig().decode
        json.asArray match {
          case Some(a) => a.flatMap(e => e.as[Sv2ChiselProjectConfig] match {
              case Left(failure) => 
                critical(s"Error while reading YAML: $failure")
                critical(s"Ignoring project entry $e")
                None
              case Right(p) => if(cfg.emissionPath != "") Some(p.copy(emissionPath = cfg.emissionPath)) else Some(p)
            })
          case _ =>
            fatal(s"Error while reading YAML: expecting an array of projects. Be sure to start with a dash '-'.")
            sys.exit(1)
        }
    }
    
  } else if(!cfg.projectFiles.isEmpty) {
    Seq(Sv2ChiselProjectConfig(
      name = cfg.projectName,
      basePath = cfg.basePath,
      emissionPath = cfg.emissionPath,
      files = cfg.projectFiles
    ))
  } else {
    fatal("Please specify either one config file with -c option or at least one (system)verilog file.")
    println(cli)
    sys.exit(0)
  }
  
  // Main Loop
  projects.foreach( p => {
    struct(s" ---- Processing project ${p.name} ---- ")
    Driver.emitChisel(Project(p.name, p.basePath, p.files), p.emissionPath)
  })

}