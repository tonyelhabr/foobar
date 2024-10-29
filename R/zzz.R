#' @importFrom utils sessionInfo
.onLoad <- function(libname, pkgname) {
  agent_option <- getOption("foobar.agent")
  session_info <- utils::sessionInfo()
  r_version <- session_info$R.version
  agent <- sprintf(
    "R (%s.%s %s %s %s)",
    r_version$major,
    r_version$minor,
    r_version$platform,
    r_version$arch,
    r_version$arch
  )
  options("foobar.agent" = agent)
}
