library(testthat)
library(withcon)
library(DBI)
library(RSQLite)

context('withCon function with files')

test_that('withCon works with file object', {
  fname <- tempfile()
  fobj <- file(fname, 'w')
  expect_true(isOpen(fobj), info='file open')
  withCon(f = fobj, do = {})
  expect_error(isOpen(fobj), info='file closed')
  file.remove(fname)
})

test_that('withCon works with file object even with error', {
  fname <- tempfile()
  fobj <- file(fname, 'w')
  expect_true(isOpen(fobj), info='file open')
  expect_error(withCon(f = fobj, do = { foo }), info='operation fails')
  expect_error(isOpen(fobj), info='file closed even with error')
  file.remove(fname)
})

test_that('withCon works with multiple files', {
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

test_that('withCon works with DBI connections', {
  con <- dbConnect(SQLite(), ':memory:')
  expect_true(dbIsValid(con), info='file open')
  withCon(conn = con, do = {})
  expect_false(dbIsValid(con), info='file closed')
})

test_that('withCon works with DBI connections with error', {
  con <- dbConnect(SQLite(), ':memory:')
  expect_true(dbIsValid(con), info='file open')
  expect_error(withCon(conn = con, do = { foo }), info='error operation')
  expect_false(dbIsValid(con), info='file closed even with error')
})
