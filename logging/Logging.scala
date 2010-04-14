trait Logging {
  val _log = org.slf4j.LoggerFactory.getLogger(getClass)
  def log = _log
  def debug(msg: => String) = if (log.isDebugEnabled) log.debug(msg)
  def error(msg: => String, e:Throwable) = if (log.isErrorEnabled) log.error(msg,e)
}
