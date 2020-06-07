exit_with_statuscode <- function(status) {
  quit(status = status, save = 'no', runLast = FALSE)
}

skip_remaining_drone_ci_steps <- function() {
  # https://discourse.drone.io/t/how-to-exit-a-pipeline-early-without-failing/3951
  exit_with_statuscode(78)
}

#' skip remaining drone CI steps if tag is dev tag
#' @export
skip_remaining_if_dev_tag <- function() {
  if (is_dev_tag()) {
    skip_remaining_drone_ci_steps()
  }
  exit_with_statuscode(0)
}
