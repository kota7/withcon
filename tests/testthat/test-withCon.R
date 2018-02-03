library(testthat)
library(withcon)
library(DBI)
library(RSQLite)

context('withCon function with files')

test_that('withCon closes a file object', {
  fname <- tempfile()
  fobj <- file(fname, 'w')
  expect_true(isOpen(fobj), info='file open')
  withCon(f = fobj, do = {})
  expect_error(isOpen(fobj), info='file closed')
  file.remove(fname)
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


context('withCon with DBI connections')

test_that('withCon closes DBI connection', {
  con <- dbConnect(SQLite(), ':memory:')
  expect_true(dbIsValid(con), info='file open')
  withCon(conn = con, do = {})
  expect_false(dbIsValid(con), info='file closed')
})

test_that('withCon closes DBI connections with error', {
  con <- dbConnect(SQLite(), ':memory:')
  expect_true(dbIsValid(con), info='file open')
  expect_error(withCon(conn = con, do = { foo }), info='error operation')
  expect_false(dbIsValid(con), info='file closed even with error')
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


