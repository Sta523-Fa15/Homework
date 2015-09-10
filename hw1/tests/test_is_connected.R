library(testthat)

#stopifnot(file.exists("graph.R"))
#source("graph.R")

is_connected = function(g,x,y) TRUE

context("Test is_connected")

test_that("Connected graphs", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))

  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))

  g3 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 3L),
                     weights = c(1, 1)),
            C = list(edges   = c(2L, 3L),
                     weights = c(1, 1)))

  g4 = list(A = list(edges   = c(2L),
                     weights = c(1)),
            B = list(edges   = c(3L),
                     weights = c(1)),
            C = list(edges   = c(4L),
                     weights = c(1)),
            D = list(edges   = c(5L),
                     weights = c(1)),
            E = list(edges   = c(6L),
                     weights = c(1)),
            F = list(edges   = integer(),
                     weights = numeric()))


  expect_true(is_connected(g1,1,1))

  expect_true(is_connected(g2,1,1))
  expect_true(is_connected(g2,1,2))
  expect_true(is_connected(g2,2,1))
  expect_true(is_connected(g2,2,2))

  expect_true(is_connected(g3,1,1))
  expect_true(is_connected(g3,1,2))
  expect_true(is_connected(g3,2,1))
  expect_true(is_connected(g3,2,3))
  expect_true(is_connected(g3,3,2))
  expect_true(is_connected(g3,3,3))


  expect_true(is_connected(g4,1,2))
  expect_true(is_connected(g4,1,3))
  expect_true(is_connected(g4,1,4))
  expect_true(is_connected(g4,1,5))
  expect_true(is_connected(g4,1,6))

  expect_false(is_connected(g4,6,1))
  expect_false(is_connected(g4,6,2))
  expect_false(is_connected(g4,6,3))
  expect_false(is_connected(g4,6,4))
  expect_false(is_connected(g4,6,5))
})


test_that("Unconnected graphs", {
  g1 = list(A = list(edges   = integer(),
                     weights = numeric()))

  g2 = list(A = list(edges   = c(1L),
                     weights = c(1)),
            B = list(edges   = c(2L),
                     weights = c(1)))

  g3 = list(A = list(edges   = c(1L),
                     weights = c(1)),
            B = list(edges   = c(1L, 3L),
                     weights = c(1, 1)),
            C = list(edges   = c(3L),
                     weights = c(1)))


  expect_false(is_connected(g1,1,1))

  expect_true(is_connected(g2,1,1))
  expect_false(is_connected(g2,1,2))
  expect_false(is_connected(g2,2,1))
  expect_true(is_connected(g2,2,2))

  expect_true(is_connected(g3,1,1))
  expect_false(is_connected(g3,1,2))
  expect_true(is_connected(g3,2,1))
  expect_false(is_connected(g3,2,2))
  expect_true(is_connected(g3,2,3))
  expect_false(is_connected(g3,3,2))
  expect_true(is_connected(g3,3,3))
})

test_that("Vertex labels", {
  g1 = list(A = list(edges   = c(1L),
                     weights = c(1)))

  g2 = list(A = list(edges   = c(1L, 2L),
                     weights = c(1, 1)),
            B = list(edges   = c(1L, 2L),
                     weights = c(1, 1)))

  # Good labels

  expect_true(is_connected(g1,1,1))
  expect_true(is_connected(g1,"A","A"))
  expect_true(is_connected(g1,"A",1))
  expect_true(is_connected(g1,1,"A"))

  expect_true(is_connected(g2,1,2))
  expect_true(is_connected(g2,"A","B"))
  expect_true(is_connected(g2,"A",2))
  expect_true(is_connected(g2,1,"B"))

  expect_true(is_connected(g1,1,1L))
  expect_true(is_connected(g1,1L,1))
  expect_true(is_connected(g1,1L,1L))

  # Bad labels

  expect_error(is_connected(g1,1,3))
  expect_error(is_connected(g1,"A",3))
  expect_error(is_connected(g1,1,"C"))
  expect_error(is_connected(g1,"A","C"))

  expect_error(is_connected(g1,1,TRUE))
  expect_error(is_connected(g1,"A",TRUE))

  expect_error(is_connected(g1,1,NaN))
  expect_error(is_connected(g1,"A",NaN))

  expect_error(is_connected(g1,1,NA_real_))
  expect_error(is_connected(g1,"A",NA_real_))
  expect_error(is_connected(g1,1,NA_integer_))
  expect_error(is_connected(g1,"A",NA_integer_))
  expect_error(is_connected(g1,1,NA_character_))
  expect_error(is_connected(g1,"A",NA_character_))
  expect_error(is_connected(g1,1,NA_complex_))
  expect_error(is_connected(g1,"A",NA_complex_))
})


test_that("Bad graphs", {
  bad_g1 = list(list())
  bad_g2 = list(list(edges = 1L))
  bad_g3 = list(list(weights = 1))

  expect_error(is_connected(bad_g1,1,1))
  expect_error(is_connected(bad_g2,1,1))
  expect_error(is_connected(bad_g3,1,1))

  expect_error(is_connected(1,1,1))
  expect_error(is_connected(1L,1,1))
  expect_error(is_connected("1",1,1))
  expect_error(is_connected(TRUE,1,1))
})

