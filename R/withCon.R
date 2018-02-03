#' Make sure connections are closed
#'
#' This function mimics Python's \code{with} clause, which helps
#' to make sure that connections are closed.
#' Concretely, this function closes connection objects when
#' one of the followings occurred:
#' \itemize{
#' \item{operations are completed}
#' \item{operations throw an exception}
#' }
#'
#' Internally, generic function \code{\link{conclose}} is called for
#' each connection object after operations.
#' Currently, \code{conclose} is defined for
#' \code{\link[base]{connection}} and
#' \code{\link[DBI]{DBIConnection-class}}.
#' The function still works with unsupported connection object,
#' for which \code{conclose} does nothing.
#'
#' @param ... arbitrary number of connection objects;
#'            should be named to refer to them in the operation

#' @param do operations to conduct with the connection objects
#' @return outcome of \code{expr}
#' @export
#' @examples
#' \dontrun{
#' # database
#' library(DBI)
#' withCon(conn = dbConnect(RSQLite::SQLite(), 'temp.db'), do = {
#'   dbWriteTable(conn, 'tbl', mtcars)
#'   dbReadTable(conn, 'tbl')
#' })
#' file.remove('temp.db')
#'
#' # files
#' withCon(f1 = file('foo.txt', 'w'), f2 = file('bar.txt', 'w'), do = {
#'   write('foo', f1)
#'   write('bar', f2)
#' })
#' file.remove('foo.txt', 'bar.txt')
#' }
withCon <- function(..., do = {}) {

  cons <- list(...)
  # if some of the `cons` is unnamed, warn
  # because there is no way to refer to it
  if (length(cons) > 0 && any(is.na(names(cons)) | names(cons)==''))
    warning('connection objects should be named')

  # conduct operations
  # error is kept and thrown after closing objects
  err <- NULL
  ret <- tryCatch(
    eval(substitute(do), envir=cons, enclos=parent.frame()),
    error = function(e) {
      err <<- e
      NULL
    }
  )

  # close objects
  lapply(cons, conclose)

  # if error occurred, raise now
  if (!is.null(err)) {
    stop(sprintf('Exception occurred operations: %s', err$message))
  }

  ret
}



