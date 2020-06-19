// Freely inspired by firrtl LoggerOptions.scala
// Originally retrieved from https://github.com/freechipsproject/firrtl/ March 2020
// See LICENSE.firrtl for license details.

package logger

/** Internal options used to control the logging
  *
  * @param globalLogLevel the verbosity of logging (default: [[logger.LogLevel.None]])
  * @param classLogLevels the individual verbosity of logging for specific classes
  * @param logToFile      if true, log to a file
  * @param logClassNames  indicates logging verbosity on a class-by-class basis
  */
class LoggerOptions private [logger] (
  val globalLogLevel: LogLevel.Value              = LogLevel.Warn,
  val classLogLevels: Map[String, LogLevel.Value] = Map.empty,
  val logClassNames:  Boolean                     = false,
  val logFileName:    Option[String]              = None) {

  private [logger] def copy(
    globalLogLevel: LogLevel.Value              = globalLogLevel,
    classLogLevels: Map[String, LogLevel.Value] = classLogLevels,
    logClassNames:  Boolean                     = logClassNames,
    logFileName:    Option[String]              = logFileName): LoggerOptions = {

    new LoggerOptions(
      globalLogLevel = globalLogLevel,
      classLogLevels = classLogLevels,
      logClassNames = logClassNames,
      logFileName = logFileName)

  }

  /** Return the name of the log file, defaults to `a.log` if unspecified */
  def getLogFileName(): Option[String] = logFileName.orElse(Some("a.log"))

}
