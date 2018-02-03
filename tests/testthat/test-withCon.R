library(testthat)
library(withcon)
library(DBI)
library(RSQLite)

context('withCon closes files')

test_that('withCon closes a file object', {
  fname <- tempfile()
  fobj <- file(fname, 'w')
  expect_true(isOpen(fobj), info='file open')
  withCon(f = fobj, do = {})
  expect_error(isOpen(fobj), info='file closed')
  file.remove(fname)
})

test_that('wichCon read from / write to file', {
  fn1 <- tempfile()
  fn2 <- tempfile()
  write(1:10, fn1)

  withCon(f1=file(fn1, 'r'), f2=file(fn2, 'w'), do = {
    x <- scan(f1, quiet=TRUE)
    write(x^2, f2)
  })
  expect_equal(scan(fn2, quiet=TRUE), (1:10)^2)
  file.remove(fn1, fn2)
})

test_that('withCon closes file object even with error', {
  fname <- tempfile()
  fobj <- file(fname, 'w')
  expect_true(isOpen(fobj), info='file open')
  expect_error(withCon(f = fobj, do = { foo }), info='operation fails')
  expect_error(isOpen(fobj), info='file closed even with error')
  file.remove(fname)
})

test_that('withCon closes multiple files', {
  f1 <- tempfile()
  f2 <- tempfile()
  fobj1 <- file(f1, 'w')
  fobj2 <- file(f2, 'w')
  expect_true(isOpen(fobj1), info='file open')
  expect_true(isOpen(fobj2), info='file open')
  withCon(f = fobj1, g = fobj2, do = {})
  expect_error(isOpen(fobj1), info='file closed')
  expect_error(isOpen(fobj2), info='file closed')
  file.remove(f1, f2)
})


context('withCon closes DBI connections')

test_that('withCon closes DBI connection', {
  con <- dbConnect(SQLite(), ':memory:')
  expect_true(dbIsValid(con), info='file open')
  withCon(conn = con, do = {})
  expect_false(dbIsValid(con), info='file closed')
})

test_that('withCon read from / write to database', {
  db <- tempfile()
  con <- dbConnect(SQLite(), db)
  dbWriteTable(con, 'foo', mtcars)
  dbDisconnect(con)

  withCon(con=dbConnect(SQLite(), db), do = {
    x <- dbReadTable(con, 'foo')
    dbWriteTable(con, 'bar', aggregate(mpg ~ cyl, FUN=mean, data=x))
  })

  res <- {
    con <- dbConnect(SQLite(), db)
    ret <- dbReadTable(con, 'bar')
    dbDisconnect(con)
    ret
  }
  ans <- aggregate(mpg ~ cyl, FUN=mean, data=mtcars)
  expect_equal(res, ans)
  file.remove(db)
})

test_that('withCon closes DBI connections with error', {
  con <- dbConnect(SQLite(), ':memory:')
  expect_true(dbIsValid(con), info='file open')
  expect_error(withCon(conn = con, do = { foo }), info='error operation')
  expect_false(dbIsValid(con), info='file closed even with error')
})

test_that('withCon closes multiple DBI connections', {
  db1 <- tempfile()
  db2 <- tempfile()
  con1 <- dbConnect(SQLite(), db1)
  con2 <- dbConnect(SQLite(), db2)
  expect_true(dbIsValid(con1), info='con1 open')
  expect_true(dbIsValid(con2), info='con2 open')
  withCon(con1=con1, con2=con2, do = { })
  expect_false(dbIsValid(con1), info='con1 close')
  expect_false(dbIsValid(con2), info='con2 close')
  file.remove(db1, db2)
})


context('withCon and variable scope')

test_that('withCon operation finds with variables in parent scope', {
  x <- 10
  withCon(f = file('foo.txt', 'w'), do = {
    write(x*5, f)
  })
  expect_equal(scan('foo.txt', quiet=TRUE), 50,
               info='variable in parent env')
  file.remove('foo.txt')

  x <- 5
  func <- function() {
    y <- 3
    withCon(f = file('foo.txt', 'w'), do = {
      write(x*y, f)
    })
  }
  func()
  expect_equal(scan('foo.txt', quiet=TRUE), 15,
               info='variable in grand parent env')
  file.remove('foo.txt')


  func <- function() {
    x <- 100
    function() x
  }
  withCon(f = file('foo.txt', 'w'), do = {
    x <- 0
    fx <- func()
    write(fx()*5, f)
  })
  expect_equal(scan('foo.txt', quiet=TRUE), 500,
               info='variable inside function scope')
  file.remove('foo.txt')

})


