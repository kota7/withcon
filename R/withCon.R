#' Make sure connection is closed
#'
#' This function mimics Python's \code{with} clause.  Concretely,
#' Connections objects are automatically closed:
#' \itemize{
#' \item{After a series of operations are conducted}
#' \item{Operations throw exceptions}
#' }
#'
#' @param ... arbitrary number of connection objects;
#'            should be named to refer to them in the operation;
#'            currently \code{\link[base]{connection}} and
#'            \code{\link[DBI]{DBIConnection-class}} are supported
#' @param do operations to conduct with the connection objects
#' @return outcome of \code{expr}
#' @export
#' @examples
#' \dontrun{
#' # database
#' library(DBI)
#' withCon(conn=dbConnect(RSQLite::SQLite(), 'temp.db'), do = {
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



