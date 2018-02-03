#' Generic `close` operation for connection objects
#'
#' @param obj connection object
#' @export
#' @examples
#' f <- file('foo.txt', 'w')
#' conclose(f)  # same as `close(f)`
#'
#' g <- DBI::dbConnect(RSQLite::SQLite(), "bar.db")
#' conclose(g)  # same as `DBI::dbDisconnect(g)`
#'
#' h <- 10
#' conclose(h)  # nothing is done
conclose <- function(obj) UseMethod("conclose", obj)

#' @export
#' @rdname conclose
conclose.connection <- function(obj) close(obj)

#' @export
#' @rdname conclose
conclose.DBIConnection <- function(obj) DBI::dbDisconnect(obj)

#' @export
#' @rdname conclose
conclose.default <- function(obj) {
  message('`conclose` method is not defined for class ', class(obj),
          '; nothing is done')
}
