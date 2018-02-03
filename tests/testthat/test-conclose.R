library(testthat)
library(withcon)
library(DBI)
library(RSQLite)

context('conclose generic function')

test_that('conclose works with file object', {
  fname <- tempfile()
  f <- file(fname, 'w')
  expect_true(isOpen(f), info='file open')
  conclose(f)
  expect_error(isOpen(f), info='file closed')

  g <- file(fname, 'r')
  expect_true(isOpen(g), info='file open again')
  conclose(g)
  expect_error(isOpen(g), info='file closed again')

  file.remove(fname)
})

test_that('conclose works with DBI connections', {
  con <- dbConnect(SQLite(), ':memory:')
  expect_true(dbIsValid(con), info='db open')
  conclose(con)
  expect_false(dbIsValid(con), info='db closed')
})
